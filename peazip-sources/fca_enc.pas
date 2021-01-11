program fca_enc;


(*************************************************************************
 DESCRIPTION     :  Simple test (console mode) program to encrypt a file with
                    128 bit AES and HMAC-SHA1 or EAX authentication

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

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
 0.51     22.01.06  we          uses new Hash units
 0.52     14.06.07  we          Type TAES_EAXContext
 0.53     15.11.08  we          uses BTypes, buf: array of byte
 0.64     10.11.17  we          {$M ...} for BIT16 only
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

{$ifdef win32}
  {$ifndef VirtualPascal}
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

  fcrypta, BTypes, TSC, Dates;


const
  Version = 'V0.53';


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
  buf    : array[0..BUFSIZE-1] of byte;  {file IO buffer             }

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
  assign(fi,InName);
  reset(fi,1);
  len := filesize(fi);
  CheckIO('open input file '+InName);

  assign(fo,OutName);
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

var
  UseEAX: boolean;
  i: integer;
  ps: string[10];
begin
  writeln('FCA_ENC ',Version,' - Encrypt/authenticate with AES-128   (C) 2003-2008 W.Ehrhardt');
  if (paramcount=1) and (paramstr(1)='--test') then Encrypt(true, 'test1234','dates.pas', '##.enc')
  else begin
    ps := paramstr(1);
    for i:=1 to length(ps) do ps[i] := upcase(ps[i]);
    if (paramcount<>4) or ((ps<>'-EAX') and (ps<>'-HMAC')) then begin
      writeln('Usage: FCA_ENC [-hmac|-eax] <password> <infile> <outfile>');
      writeln('       -hmac  : authenticate with HMAC-SHA1-128');
      writeln('       -eax   : authenticate with AES_EAX');
      {$ifdef BIT16}
      writeln('       <password> may not contain white space');
      {$else}
      writeln('       use "<password>" if password contains white space');
      {$endif}
      halt;
    end;
    UseEAX := ps='-EAX';
    Encrypt(UseEAX,paramstr(2),paramstr(3),paramstr(4));
  end;
end.

