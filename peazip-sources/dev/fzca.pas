program fzca;


(*************************************************************************
 DESCRIPTION    : Simple test (console mode) program to encrypt/decrypt files
                  with 128 bit AES and HMAC-SHA1 or EAX authentication
                  and optional zlib compression

 REQUIREMENTS   : BP7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

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
 0.70     04.05.05  we          zlib compression: needs BP7+
 0.71     22.01.06  we          uses new Hash, HMAC units
 0.72     14.06.07  we          Type TAES_EAXContext
 0.73     15.11.08  we          uses BTypes, buf: array of byte
 0.74     23.07.09  we          D12 fixes
 0.75     30.12.12  we          D17 adjustments
 0.76     10.11.17  we          {$M ...} for BIT16 only
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


{tbd: -z or levels -0 .. -9}


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

{$ifdef BIT16}
  {$F+}
{$endif}

{$ifdef FPC}
  {$define FPC_ProcVar}
  {$ifdef FPC_DELPHI}
    {$undef FPC_ProcVar}
  {$endif}
  {$ifdef FPC_TP}
    {$undef FPC_ProcVar}
  {$endif}
  {$ifdef FPC_GPC}
    {$undef FPC_ProcVar}
  {$endif}
{$else}
  {$undef FPC_ProcVar}
{$endif}


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
  zlibh, zlibex,
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
procedure Abort(const msg: str255);
  {-write msg and halt with error code 1}
begin
  writeln(#7, msg);
  halt(1);
end;


{---------------------------------------------------------------------------}
procedure CheckIO(const msg: str255);
  {-Check IO, abort with msg if error}
begin
  if IOResult<>0 then Abort('IO error: '+msg);
end;


{---------------------------------------------------------------------------}
procedure IntError(const msg: str255);
begin
  Abort('Internal error: '+msg);
end;


{---------------------------------------------------------------------------}
{$ifdef VER80}
const
  BUFSIZE = $9000; {D1: data segment together with stack}
{$else}
const
  BUFSIZE = $D000;
{$endif}

var
  buf: array[0..BUFSIZE-1] of byte;  {file IO buffer}

const
  clevel=Z_DEFAULT_COMPRESSION;

type
  TFCUser = record
              cxh : TFCA_HMAC_Context;   {HMAC-SHA1 context}
              cxe : TAES_EAXContext;     {EAX context}
              fi  : file;
              fo  : file;
              Rest: longint;             {remaining input file length}
              EAX : boolean;
            end;
  PFCUser = ^TFCUser;


{---------------------------------------------------------------------------}
function EncRead(bufp, userdata: pointer; mlen: word; var done: boolean): longint;
  {-Callback: read from input file}
var
  n: unsigned;
begin
  {read unencrypted input}
  blockread(PFCUser(Userdata)^.fi, bufp^, mlen, n);
  if IOResult<>0 then begin
    EncRead := -1;
    done := true;
  end
  else begin
    EncRead := n;
    done := eof(PFCUser(Userdata)^.fi);
  end;
end;


{---------------------------------------------------------------------------}
function EncWrite(bufp, userdata: pointer; size: word): longint;
  {-Callback: encrypt bufp, write to output file}
var
  n: unsigned;
begin
  with PFCUser(Userdata)^ do begin
    if EAX then begin
      if FCA_EAX_encrypt(cxe, bufp^, size)<>0 then IntError('FCA_EAX_encrypt');
    end
    else begin
      if FCA_HMAC_encrypt(cxh, bufp^, size)<>0 then IntError('FCA_HMAC_encrypt');
    end;
    blockwrite(fo, bufp^, size, n);
    if IOResult<>0 then EncWrite := -1 else EncWrite := n;
  end;
end;


{---------------------------------------------------------------------------}
function DecRead(bufp, userdata: pointer; mlen: word; var done: boolean): longint;
  {-Callback: read from input file and decrypt}
var
  nread: unsigned;
  nmax: word;
begin
  with PFCUser(Userdata)^ do begin
    if Rest>mlen then nmax:=mlen else nmax:=Rest;
    {read encrypted input, use and update rest}
    blockread(fi, bufp^, nmax, nread);
    if IOResult<>0 then begin
      DecRead := -1;
      done := true;
    end
    else begin
      {decrypt}
      DecRead := nread;
      dec(Rest, nread);
      done := (Rest<=0) or eof(fi);
      if EAX then begin
        if FCA_EAX_decrypt(cxe, bufp^, nread)<>0 then IntError('FCA_EAX_decrypt');
      end
      else begin
        if FCA_HMAC_decrypt(cxh, bufp^, nread)<>0 then IntError('FCA_HMAC_decrypt');
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function DecWrite(bufp, userdata: pointer; size: word): longint;
  {-Callback: write to output file}
var
  n: unsigned;
begin
  blockwrite(PFCUser(Userdata)^.fo, bufp^, size, n);
  if IOResult<>0 then DecWrite := -1 else DecWrite := n;
end;


{---------------------------------------------------------------------------}
procedure Encrypt(UseEAX, UseZL: boolean; const sPW, InName, OutName: str255);
  {-Encrypt file InName to OutName using password sPW}
var
  n      : word;                         {read/write counter         }
  res    : longint;
  hdr    : TFCAHdr;
  auth   : TFCA_AuthBlock;
  User   : TFCUser;
begin
  randomize;

  filemode := 0;
  assign(User.fi,{$ifdef D12Plus} string {$endif}(InName));
  reset(User.fi,1);
  User.Rest := filesize(User.fi);
  User.EAX  := UseEAX;
  CheckIO('open input file '+InName);

  assign(User.fo,{$ifdef D12Plus} string {$endif}(OutName));
  rewrite(User.fo,1);
  CheckIO('open output file '+OutName);

  GetSalt(hdr.salt);

  if UseEAX then begin
    if FCA_EAX_initS(User.cxe, sPW, hdr)<>0 then IntError('FCA_EAX_init');
  end
  else begin
    if FCA_HMAC_initS(User.cxh, sPW, hdr)<>0 then IntError('FCA_HMAC_init');
  end;

  if UseZL then hdr.Flags := hdr.Flags or $02;

  write('Encrypting ...');
  blockwrite(User.fo, hdr, sizeof(hdr));
  CheckIO('write output file');

  if UseZL then begin
    {$ifdef FPC_Procvar}
      res := DeflateEx(@EncRead, @EncWrite, clevel, @User);
    {$else}
      res := DeflateEx(EncRead, EncWrite, clevel, @User);
    {$endif}
    write(' DeflateEx result: ',res, ' - ');
  end
  else begin
    {no compression of source}
    while User.Rest>0 do begin
      if User.Rest>sizeof(buf) then n:=sizeof(buf) else n:=User.Rest;
      blockread(User.fi,buf,n);
      CheckIO('read input file');
      dec(User.Rest,n);
      if UseEAX then begin
        if FCA_EAX_encrypt(User.cxe, buf, n)<>0 then IntError('FCA_EAX_encrypt');
      end
      else begin
        if FCA_HMAC_encrypt(User.cxh, buf, n)<>0 then IntError('FCA_HMAC_encrypt');
      end;
      blockwrite(User.fo,buf,n);
      CheckIO('write output file');
      write('.');
    end;
  end;
  if UseEAX then begin
    FCA_EAX_final(User.cxe, auth);
  end
  else begin
    FCA_HMAC_final(User.cxh, auth);
  end;
  blockwrite(User.fo,auth,sizeof(auth));
  CheckIO('write output file');
  close(User.fo);
  CheckIO('close output file');
  close(User.fi);
  if IOResult<>0 then ;
  writeln(' done');
end;


{---------------------------------------------------------------------------}
procedure Decrypt(const sPW, InName, OutName: str255);
  {-Decrypt file InName to OutName using password sPW}
var
  n       : word;                         {read/write counter  }
  hdrk    : TFCAHdr;                      {hdr from key deriv  }
  hdrf    : TFCAHdr;                      {hdr from file deriv }
  authf   : TFCA_AuthBlock;
  authc   : TFCA_AuthBlock;
  User    : TFCUser;
  res     : longint;
  UseZL   : boolean;
begin
  filemode := 0;
  assign(User.fi,{$ifdef D12Plus} string {$endif}(InName));
  reset(User.fi,1);
  User.Rest := filesize(User.fi) - sizeof(hdrf) - sizeof(authf);
  CheckIO('open input file '+InName);

  CheckIO('read input file');
  blockread(User.fi, hdrf, sizeof(hdrf));
  CheckIO('read input file');

  if (hdrf.FCASig<>C_FCA_Sig) or (hdrf.Flags and $F0 <> $A0) then begin
    Abort('Invalid file header');
  end;

  hdrk := hdrf;
  UseZL:= hdrf.Flags and $02 <> 0;
  User.EAX := odd(hdrf.Flags);

  if UseZL then writeln('Found: zlib compression flag ...');

  if User.EAX then begin
    if FCA_EAX_initS(User.cxe, sPW, hdrk)<>0 then IntError('FCA_EAX_init');
    writeln('Found: authenticate/verify with AES_EAX ...');
  end
  else begin
    if FCA_HMAC_initS(User.cxh, sPW, hdrk)<>0 then IntError('FCA_HMAC_init');
    writeln('Found: Authenticate/verify with HMAC-SHA1-128 ...');
  end;
  if hdrf.PW_ver<>hdrk.PW_ver then Abort('Wrong password');

  write('Decrypting ...');
  assign(User.fo,{$ifdef D12Plus} string {$endif}(OutName));
  rewrite(User.fo,1);
  CheckIO('open output file '+OutName);
  if UseZL then begin
    {$ifdef FPC_Procvar}
      res := InflateEx(@DecRead, @DecWrite, @User);
    {$else}
      res := InflateEx(DecRead, DecWrite, @User);
    {$endif}
    write(' InflateEx result: ',res, ' - ');
  end
  else begin
    while User.Rest>0 do begin
      if User.Rest>sizeof(buf) then n:=sizeof(buf) else n:=User.Rest;
      blockread(User.fi,buf,n);
      CheckIO('read input file');
      dec(User.Rest,n);
      if User.EAX then begin
        if FCA_EAX_decrypt(User.cxe, buf, n)<>0 then IntError('FCA_EAX_decrypt');
      end
      else begin
        if FCA_HMAC_decrypt(User.cxh, buf, n)<>0 then IntError('FCA_HMAC_decrypt');
      end;
      blockwrite(User.fo,buf,n);
      CheckIO('write output file');
      write('.');
    end;
  end;
  if User.EAX then begin
    FCA_EAX_final(User.cxe, authc);
  end
  else begin
    FCA_HMAC_final(User.cxh, authc);
  end;
  close(User.fo);
  CheckIO('close output file');
  blockread(User.fi,authf, sizeof(authf));
  CheckIO('read input file');
  if not compmem(@authf, @authc, sizeof(authf)) then begin
    Abort(' authentication failed');
  end
  else writeln(' OK');
end;


{.$define fca_selftest}

var
  i: integer;
  z: boolean;
  {$ifdef D12Plus}
    ps: string;
  {$else}
    ps: string[10];
  {$endif}
begin
  writeln('FZCA ',Version,' - Encrypt/authenticate with AES-128   (C) 2003-2012 W.Ehrhardt');
  {$ifdef fca_selftest}
    writeln('*** FZCA self test ***');
    writeln('+++ Using EAX and zlib');
    Encrypt(true,  true, 'test1234','dates.pas', '##EAX.enc');
    Decrypt('test1234',  '##EAX.enc', '##EAX.out');
    writeln('+++ CTR and HMAC-SHA1');
    Encrypt(false, false,'test1234','dates.pas', '##HMAC.enc');
    Decrypt('test1234',  '##HMAC.enc', '##HMAC.out');
    halt;
  {$endif}
  ps := paramstr(1);
  for i:=1 to length(ps) do ps[i] := upcase(ps[i]);
  if (paramcount<4) or ((ps<>'-EAX') and (ps<>'-HMAC') and (ps<>'-D')) then begin
    writeln('Usage: FZCA [-hmac | -eax | -d] <password> <infile> <outfile> [-z]');
    writeln('       -hmac: encrypt/authenticate with HMAC-SHA1-128');
    writeln('       -eax : encrypt/authenticate with AES_EAX');
    writeln('       -d   : decrypt/verify');
    writeln('       -z   : encrypt with zlib compression enabled');
    {$ifdef BIT16}
    writeln('       <password> may not contain white space');
    {$else}
    writeln('       use "<password>" if password contains white space');
    {$endif}
    halt;
  end;
  {$ifdef D12Plus}
    if ps='-D' then Decrypt(str255(paramstr(2)),str255(paramstr(3)),str255(paramstr(4)))
    else begin
      z := (paramstr(5)='-z') or (paramstr(5)='-Z');
      Encrypt(ps='-EAX',z,str255(paramstr(2)),str255(paramstr(3)),str255(paramstr(4)));
    end;
  {$else}
    if ps='-D' then Decrypt(paramstr(2),paramstr(3),paramstr(4))
    else begin
      z := (paramstr(5)='-z') or (paramstr(5)='-Z');
      Encrypt(ps='-EAX',z,paramstr(2),paramstr(3),paramstr(4));
    end;
  {$endif}
end.

