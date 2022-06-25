program fca_dec;


(*************************************************************************
 DESCRIPTION     :  Simple test (console mode) program to decrypt a file
                    that was encrypted with fca_enc (using fcrypta)

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     07.12.03  W.Ehrhardt  Initial version, all paras fixed
 0.20     08.12.03  we          IOCheck
 0.30     08.12.03  we          using paramstr(1) .. (3)
 0.31     09.12.03  we          header verifier, {$M..}
 0.32     12.04.04  we          Delphi 7
 0.32     07.07.04  we          with USEDLL
 0.50     08.07.04  we          EAX support, new names/file headers
 0.51     22.01.06  we          uses new Hash units
 0.52     14.06.07  we          Type TAES_EAXContext
 0.53     15.11.08  we          uses BTypes, buf: array of byte
 0.54     10.11.17  we          {$M ...} for BIT16 only
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
    {$M $4000,$1000,655360}
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
    Hash, HMACSHA1, AES_EAX,
  {$endif}
  fcrypta, BTypes, mem_util;


const
  Version = 'V0.53';

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
  assign(fi,InName);
  reset(fi,1);
  len := filesize(fi) - sizeof(hdrf) - sizeof(authf);
  CheckIO('open input file '+InName);

  CheckIO('read input file');
  blockread(fi, hdrf, sizeof(hdrf));
  CheckIO('read input file');

  if (hdrf.FCASig<>C_FCA_Sig) or (hdrf.Flags and $F0 <> $A0) then begin
    Abort('Invalid file header');
  end;

  hdrk := hdrf;
  UseEAX := odd(hdrf.Flags);
  if UseEAX then begin
    if FCA_EAX_initS(cxe, sPW, hdrk)<>0 then Abort('Internal error (FCA_EAX_init)');
    writeln('Authenticate/verify with AES_EAX ...');
  end
  else begin
    if FCA_HMAC_initS(cxh, sPW, hdrk)<>0 then Abort('Internal error (FCA_HMAC_init)');
    writeln('Authenticate/verify with HMAC-SHA1-128 ...');
  end;
  if hdrf.PW_ver<>hdrk.PW_ver then Abort('Wrong password');


  write('Decrypting ...');
  assign(fo,OutName);
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


begin
  writeln('FCA_DEC ',Version,' - Decrypt/verify files from FCA_ENC    (C) 2003-2008 W.Ehrhardt');
  if paramstr(1)='--test' then Decrypt('test1234', '##.enc', '##.out')
  else begin
    if paramcount<>3 then begin
      writeln('Usage: FCA_DEC <password> <infile> <outfile>');
      {$ifdef BIT16}
      writeln('       <password> may not contain white space');
      {$else}
      writeln('       use "<password>" if password contains white space');
      {$endif}
      halt;
    end;
    Decrypt(paramstr(1),paramstr(2),paramstr(3));
  end;
end.

