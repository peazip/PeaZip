program fca;

(*************************************************************************
 DESCRIPTION    : Simple test (console mode) program to encrypt/decrypt files
                  with 128 bit AES and HMAC-SHA1 or EAX authentication

 REQUIREMENTS   : TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

 REMARKS        : For version < 0.60 info is from t_ecrypt/fca_enc

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     07.12.03  W.Ehrhardt  Initial version, all paras fixed
 0.20     07.12.03  we          Salt: hash Date, Time, Randseed, TSC
 0.30     08.12.03  we          IOCheck
 0.40     08.12.03  we          using paramstr(1) .. (3)
 0.41     09.12.03  we          header verifier, {$M..}
 0.42     12.04.04  we          Delphi 7
 0.43     07.07.04  we          with USEDLL
 0.50     08.07.04  we          EAX support, new names/file headers
 0.60     09.07.04  W.Ehrhardt  Initial FCA version
 0.61     23.10.04  we          --test replaced by {$ifdef fca_selftest}
 0.70     04.05.05  we          warning if zlib compression detected
 0.71     22.01.06  we          uses new Hash, HMAC units
 0.72     14.06.07  we          Type TAES_EAXContext
 0.73     15.11.08  we          uses BTypes, buf: array of byte
 0.74     23.07.09  we          D12 fixes
 0.75     30.12.12  we          D17 adjustments
 0.75     10.11.17  we          {$M ...} for BIT16 only
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2003-2017 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)



{$i STD.INC}

{$ifdef win32or64}
  {$ifdef APPCONS}
    {$apptype console}
  {$endif}
{$else}
  {$ifdef BIT16}
    {$M $4000,0,655360}
  {$endif}
{$endif}

{$I-,S+}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      CH_INTV, AES_INTV,
    {$else}
      CH_INTF, AES_INTF,
    {$endif}
  {$else}
     Hash, SHA1, AES_EAX,
  {$endif}

  fcrypta, TSC, Dates, BTypes, mem_util;


const
  Version = 'V0.75';


{---------------------------------------------------------------------------}
procedure GetSalt(var salt: TFCASalt);
  {-generate salt for key derivation}
var
  Ctr: TCtrRec;
  JD,  msC: longint;
  sctx: THashContext;
  sdig: TSHA1Digest;
begin
  {Hash Date, Time, Randseed, and (normally) TSC}
  _ReadCounter(Ctr);
  JD := CurrentJulianDay;
  msC := msCount;
  SHA1Init(sctx);
  SHA1Update(sctx, @Ctr, sizeof(Ctr));
  SHA1Update(sctx, @JD, sizeof(JD));
  SHA1Update(sctx, @msC, sizeof(msC));
  SHA1Update(sctx, @randseed, sizeof(randseed));
  SHA1Final(sctx, sdig);
  move(sdig, salt, sizeof(salt));
end;



{---------------------------------------------------------------------------}
procedure Abort(msg: str255);
  {-write msg and halt with error code 1}
begin
  writeln(#7, msg);
  halt(1);
end;


{---------------------------------------------------------------------------}
procedure CheckIO(msg: str255);
  {-Check IO, abort with msg if error}
begin
  if IOResult<>0 then Abort('IO error: '+msg);
end;


{---------------------------------------------------------------------------}
{$ifdef VER80}
const
  BUFSIZE = $A000;            {D1: data segment together with stack}
{$else}
const
  BUFSIZE = $E000;
{$endif}

var
  buf     : array[0..BUFSIZE-1] of byte;  {file IO buffer             }


{---------------------------------------------------------------------------}
procedure Encrypt(UseEAX: boolean; sPW, InName, OutName: str255);
  {-Encrypt file InName to OutName using password sPW}
var
  n      : word;                         {read/write counter         }
  len    : longint;                      {remaining input file length}
  hdr    : TFCAHdr;
  cxh    : TFCA_HMAC_Context;            {file crypt context         }
  cxe    : TAES_EAXContext;
  auth   : TFCA_AuthBlock;
  fi,fo  : file;                         {input and output files     }

begin
  randomize;

  filemode := 0;
  assign(fi,{$ifdef D12Plus} string {$endif}(InName));
  reset(fi,1);
  len := filesize(fi);
  CheckIO('open input file '+InName);

  assign(fo,{$ifdef D12Plus} string {$endif}(OutName));
  rewrite(fo,1);
  CheckIO('open output file '+OutName);

  GetSalt(hdr.salt);

  if UseEAX then begin
    if FCA_EAX_initS(cxe, sPW, hdr)<>0 then Abort('Internal error (FCA_EAX_init)');
  end
  else begin
    if FCA_HMAC_initS(cxh, sPW, hdr)<>0 then Abort('Internal error (FCA_HMAC_init)');
  end;

  write('Encrypting ...');
  blockwrite(fo, hdr, sizeof(hdr));
  CheckIO('write output file');

  while len>0 do begin
    if len>sizeof(buf) then n:=sizeof(buf) else n:=len;
    blockread(fi,buf,n);
    CheckIO('read input file');
    dec(len,n);
    if UseEAX then begin
      if FCA_EAX_encrypt(cxe, buf, n)<>0 then Abort('Internal error (FCA_EAX_encrypt)');
    end
    else begin
      if FCA_HMAC_encrypt(cxh, buf, n)<>0 then Abort('Internal error (FCA_HMAC_encrypt)');
    end;

    blockwrite(fo,buf,n);
    CheckIO('write output file');
    write('.');
  end;
  if UseEAX then begin
    FCA_EAX_final(cxe, auth);
  end
  else begin
    FCA_HMAC_final(cxh, auth);
  end;
  blockwrite(fo,auth,sizeof(auth));
  CheckIO('write output file');
  close(fo);
  CheckIO('close output file');
  close(fi);
  if IOResult<>0 then ;
  writeln(' done');
end;



{---------------------------------------------------------------------------}
procedure Decrypt(sPW, InName, OutName: str255);
  {-Decrypt file InName to OutName using password sPW}
var
  n       : word;                         {read/write counter         }
  len     : longint;                      {remaining input file length}
  hdrk    : TFCAHdr;                      {hdr from key deriv         }
  hdrf    : TFCAHdr;                      {hdr from file deriv        }
  cxh     : TFCA_HMAC_Context;            {HMAC-SHA1 context          }
  cxe     : TAES_EAXContext;              {EAX context                }
  authf   : TFCA_AuthBlock;
  authc   : TFCA_AuthBlock;
  fi,fo   : file;                         {input and output files     }
  UseEAX  : boolean;
begin
  filemode := 0;
  assign(fi,{$ifdef D12Plus} string {$endif}(InName));
  reset(fi,1);
  len := filesize(fi) - sizeof(hdrf) - sizeof(authf);
  CheckIO('open input file '+InName);

  CheckIO('read input file');
  blockread(fi, hdrf, sizeof(hdrf));
  CheckIO('read input file');

  if (hdrf.FCASig<>C_FCA_Sig) or (hdrf.Flags and $F0 <> $A0) then begin
    Abort('Invalid file header');
  end;

  if hdrf.Flags and $02 <> 0 then begin
    writeln(#7'*** Warning: Found zlib compression flag, use t_zlibex to inflate <outfile>');
  end;

  hdrk := hdrf;
  UseEAX := odd(hdrf.Flags);
  if UseEAX then begin
    if FCA_EAX_initS(cxe, sPW, hdrk)<>0 then Abort('Internal error (FCA_EAX_init)');
    writeln('Found: authenticate/verify with AES_EAX ...');
  end
  else begin
    if FCA_HMAC_initS(cxh, sPW, hdrk)<>0 then Abort('Internal error (FCA_HMAC_init)');
    writeln('Found: Authenticate/verify with HMAC-SHA1-128 ...');
  end;
  if hdrf.PW_ver<>hdrk.PW_ver then Abort('Wrong password');


  write('Decrypting ...');
  assign(fo,{$ifdef D12Plus} string {$endif}(OutName));
  rewrite(fo,1);
  CheckIO('open output file '+OutName);
  while len>0 do begin
    if len>sizeof(buf) then n:=sizeof(buf) else n:=len;
    blockread(fi,buf,n);
    CheckIO('read input file');
    dec(len,n);
    if UseEAX then begin
      if FCA_EAX_decrypt(cxe, buf, n)<>0 then Abort('Internal error (FCA_EAX_decrypt)');
    end
    else begin
      if FCA_HMAC_decrypt(cxh, buf, n)<>0 then Abort('Internal error (FCA_HMAC_decrypt)');
    end;
    blockwrite(fo,buf,n);
    CheckIO('write output file');
    write('.');
  end;
  if UseEAX then begin
    FCA_EAX_final(cxe, authc);
  end
  else begin
    FCA_HMAC_final(cxh, authc);
  end;
  close(fo);
  CheckIO('close output file');
  blockread(fi,authf, sizeof(authf));
  CheckIO('read input file');
  if not compmem(@authf, @authc, sizeof(authf)) then begin
    Abort(' authentication failed');
  end
  else writeln(' OK');
end;

var
  i: integer;
  {$ifdef D12Plus}
    ps: string;
  {$else}
    ps: string[10];
  {$endif}
begin
  writeln('FCA ',Version,' - Encrypt/authenticate with AES-128   (C) 2003-2012 W.Ehrhardt');
  {$ifdef fca_selftest}
    Encrypt(true, 'test1234','dates.pas', '##EAX.enc');
    Decrypt('test1234', '##EAX.enc', '##EAX.out');
    Encrypt(false, 'test1234','dates.pas', '##HMAC.enc');
    Decrypt('test1234', '##HMAC.enc', '##HMAC.out');
    halt;
  {$endif}
  ps := paramstr(1);
  for i:=1 to length(ps) do ps[i] := upcase(ps[i]);
  if (paramcount<>4) or ((ps<>'-EAX') and (ps<>'-HMAC') and (ps<>'-D')) then begin
    writeln('Usage: FCA [-hmac | -eax | -d] <password> <infile> <outfile>');
    writeln('       -hmac: encrypt/authenticate with HMAC-SHA1-128');
    writeln('       -eax : encrypt/authenticate with AES_EAX');
    writeln('       -d   : decrypt/verify');
    {$ifdef BIT16}
    writeln('       <password> may not contain white space');
    {$else}
    writeln('       use "<password>" if password contains white space');
    {$endif}
    halt;
  end;
  {$ifdef D12Plus}
    if ps='-D' then Decrypt(str255(paramstr(2)),str255(paramstr(3)),str255(paramstr(4)))
    else Encrypt(ps='-EAX', str255(paramstr(2)),str255(paramstr(3)),str255(paramstr(4)));
  {$else}
    if ps='-D' then Decrypt(paramstr(2),paramstr(3),paramstr(4))
    else Encrypt(ps='-EAX', paramstr(2),paramstr(3),paramstr(4));
  {$endif}
end.

