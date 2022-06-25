program t_fct256;

(*************************************************************************
 DESCRIPTION    : Simple test (console mode) program to encrypt and
                  decrypt files with 256 bit Twofish (authenticate with
                  EAX mode or HMAC-Whirlpool)

 REQUIREMENTS   : TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

 REMARKS        :

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     04.08.06  G.Tani      Initial version based on Wolfgang Ehrhardt's fca 0.71:
                                used cryptographic functions from unit fcrypta_eax256 instead of fcrypta;
                                modified procedure GetSalt;
                                used unit sysutils for diskfree/disksize and date & time functions.
 0.11     04.08.06  W.Ehrhardt  CondDefine use_sysutils, portable code
 0.12     05.08.06  we          Reintroduced HMAC code,
 0.13     05.08.06  we          Replaced SHA1 by Whirlpool
 0.14     05.08.06  we          Adjust buffer sizes for TPW, D1; updated HMAC info messages
 0.15     14.06.07  we          Type TAES_EAXContext
 0.16     15.11.08  we          uses BTypes, buf: array of byte
 0.17     23.07.09  we          D12 fixes
 0.18     30.12.12  we          D17 adjustments
 0.19     28.03.16  we          Twofish version
 0.20     10.11.17  we          {$M ...} for BIT16 only
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2003-2017 Wolfgang Ehrhardt, Giorgio Tani

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

{NOTE: If no HMAC: remove UseEAX, change -eax to -e}

{$i STD.INC}

{$ifdef win32or64}
  {$ifdef APPCONS}
    {$apptype console}
  {$endif}
{$else}
  {$ifdef BIT16}
    {$M $6000,0,655360}
  {$endif}
{$endif}

{$ifdef HAS_INT64}
  {$define use_sysutils}
{$endif}


{$I-,S+}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  Hash, TF_EAX, Whirl512,
  BTypes, fctf256,
  {$ifdef use_sysutils}
    {$ifdef UNIT_SCOPE}
      system.sysutils,
    {$else}
      sysutils,
    {$endif}
  {$else}
    dates,
  {$endif}
  TSC;


const
  Version = 'V0.19';

{$ifdef use_sysutils}
{---------------------------------------------------------------------------}
procedure GetSalt(var salt: TFCF256Salt);
  {-generate salt for key derivation}
var
  Ctr: TCtrRec;
  TS: TTimeStamp;
  sctx: THashContext;
  sdig: TWhirlDigest;
  qword: int64;
begin
  {Hash Date, Time, Randseed, size and free space (in byte) on active disk and (normally) TSC}
  _ReadCounter(Ctr);
  Whirl_Init(sctx);
  TS:=datetimetotimestamp(now);                     {get date and time with millisecond precision}
  Whirl_Update(sctx, @TS.date, sizeof(TS.date));
  Whirl_Update(sctx, @TS.time, sizeof(TS.time));
  qword:=disksize(0);
  Whirl_Update(sctx, @qword, sizeof(qword));        {size of active disk}
  qword:=diskfree(0);
  Whirl_Update(sctx, @qword, sizeof(qword));        {free space on active disk}
  Whirl_Update(sctx, @Ctr, sizeof(Ctr));
  Whirl_Update(sctx, @randseed, sizeof(randseed));
  Whirl_Final(sctx, sdig);
  move(sdig, salt, sizeof(salt));
end;
{$else}
{---------------------------------------------------------------------------}
procedure GetSalt(var salt: TFCF256Salt);
  {-generate salt for key derivation}
var
  Ctr: TCtrRec;
  JD,  msC: longint;
  sctx: THashContext;
  sdig: TWhirlDigest;
begin
  {Hash Date, Time, Randseed, and (normally) TSC}
  _ReadCounter(Ctr);
  JD := CurrentJulianDay;
  msC := msCount;
  Whirl_Init(sctx);
  Whirl_Update(sctx, @Ctr, sizeof(Ctr));
  Whirl_Update(sctx, @JD, sizeof(JD));
  Whirl_Update(sctx, @msC, sizeof(msC));
  Whirl_Update(sctx, @randseed, sizeof(randseed));
  Whirl_Final(sctx, sdig);
  move(sdig, salt, sizeof(salt));
end;
{$endif}


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
{$ifdef WINCRT}
const
  BUFSIZE = $6000;            {TPW,D1: data segment together with stack}
{$else}
const
  BUFSIZE = $C000;
{$endif}

var
  buf    : array[0..BUFSIZE-1] of byte;  {file IO buffer             }

{---------------------------------------------------------------------------}
procedure Encrypt(UseEAX: boolean; sPW, InName, OutName: str255);
  {-Encrypt file InName to OutName using password sPW}
var
  n      : word;                         {read/write counter         }
  len    : longint;                      {remaining input file length}
  hdr    : TFCF256Hdr;
  cxh    : TFCF_HMAC256_Context;         {file crypt context         }
  cxe    : TTF_EAXContext;
  auth   : TFCF256_AuthBlock;
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
    if FCF_EAX256_initS(cxe, sPW, hdr)<>0 then Abort('Internal error (FCF_EAX256_init)');
  end
  else begin
    if FCF_HMAC256_initS(cxh, sPW, hdr)<>0 then Abort('Internal error (FCF_HMAC256_init)');
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
      if FCF_EAX256_encrypt(cxe, buf, n)<>0 then Abort('Internal error (FCF_EAX256_encrypt)');
    end
    else begin
      if FCF_HMAC256_encrypt(cxh, buf, n)<>0 then Abort('Internal error (FCF_HMAC356_encrypt)');
    end;
    blockwrite(fo,buf,n);
    CheckIO('write output file');
    write('.');
  end;
  if UseEAX then begin
    FCF_EAX256_final(cxe, auth);
  end
  else begin
    FCF_HMAC256_final(cxh, auth);
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
  i,len   : longint;                      {remaining input file length}
  hdrk    : TFCF256Hdr;                   {hdr from key deriv         }
  hdrf    : TFCF256Hdr;                   {hdr from file deriv        }
  cxh     : TFCF_HMAC256_Context;         {file crypt context         }
  cxe     : TTF_EAXContext;               {EAX context                }
  authf   : TFCF256_AuthBlock;
  authc   : TFCF256_AuthBlock;
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

  if (hdrf.FCFSig<>C_FCF_Sig) or (hdrf.Flags and $F0 <> $A0) then begin
    Abort('Invalid file header');
  end;

  if hdrf.Flags and $02 <> 0 then begin
    writeln(#7'*** Warning: Found zlib compression flag, use t_zlibex to inflate <outfile>');
  end;

  if hdrf.Flags and $04 <> 0 then begin
    writeln('Found: 256 bit key size');
  end;

  hdrk := hdrf;
  UseEAX := odd(hdrf.Flags);
  if UseEAX then begin
    if FCF_EAX256_initS(cxe, sPW, hdrk)<>0 then Abort('Internal error (FCF_EAX256_init)');
    writeln('Found: authenticate/verify with SP_EAX ...');
  end
  else begin
    if FCF_HMAC256_initS(cxh, sPW, hdrk)<>0 then Abort('Internal error (FCF_HMAC256_init)');
    writeln('Found: Authenticate/verify with HMAC-Whirlpool-256 ...');
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
      if FCF_EAX256_decrypt(cxe, buf, n)<>0 then Abort('Internal error (FCF_EAX256_decrypt)');
    end
    else begin
      if FCF_HMAC256_decrypt(cxh, buf, n)<>0 then Abort('Internal error (FCF_HMAC256_decrypt)');
    end;
    blockwrite(fo,buf,n);
    CheckIO('write output file');
    write('.');
  end;
  if UseEAX then begin
    FCF_EAX256_final(cxe, authc);
  end
  else begin
    FCF_HMAC256_final(cxh, authc);
  end;
  close(fo);
  CheckIO('close output file');
  blockread(fi,authf, sizeof(authf));
  CheckIO('read input file');
  for i:=0 to 15 do begin
    if authf[i]<>authc[i] then begin
      writeln(' Authentication failure!');
      exit;
    end;
  end;
  writeln(' Successful authentication');
end;

var
  i: integer;
  {$ifdef D12Plus}
    ps: string;
  {$else}
    ps: string[10];
  {$endif}
begin
  writeln('T_FCF256 ',Version,' - Encrypt/authenticate with Twofish-256    (C) 2003-2016 WE,GT');
  {$ifdef fca_selftest}
    Encrypt(false, 'test1234','dates.pas', '##HMAC.enc');
    Decrypt('test1234', '##HMAC.enc', '##HMAC.out');
    Encrypt(true, 'test1234','dates.pas', '##EAX.enc');
    Decrypt('test1234', '##EAX.enc', '##EAX.out');
    halt;
  {$endif}
  ps := paramstr(1);
  for i:=1 to length(ps) do ps[i] := upcase(ps[i]);
  if (paramcount<>4) or ((ps<>'-EAX') and (ps<>'-HMAC') and (ps<>'-D')) then begin
    writeln('Usage: T_FCF256 [-hmac | -eax | -d] <password> <infile> <outfile>');
    writeln('       -hmac: encrypt/authenticate with HMAC-Whirlpool-256');
    writeln('       -eax : encrypt/authenticate with Twofish-256 EAX');
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

