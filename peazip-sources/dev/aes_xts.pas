unit AES_XTS;

(*************************************************************************

 DESCRIPTION   :  AES XTS mode functions

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REMARKS       :  1. The IV and buf fields of the main contexts are used for
                     temporary buffers. Tweak context IV holds enc(tweak)*a^j.
                  2. Quote from the IEEE Draft: "Attention is called to the
                     possibility that implementation of this standard may
                     require use of subject matter covered by patent rights."
                     Before using this source/mode read the patent section
                     in legal.txt!

 REFERENCES    : [1] IEEE P1619, Draft Standard for Cryptographic Protection
                     of Data on Block-Oriented Storage Devices. Available from
                     http://ieee-p1619.wetpaint.com/page/IEEE+Project+1619+Home


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     23.09.07  we          Initial version like ECB (BP7+, encrypt)
 0.11     24.09.07  we          BP7+ decrypt
 0.12     24.09.07  we          TP5-TP6
 0.13     27.09.07  we          ILen now longint
 0.14     27.09.07  we          Check ILen+ofs if BIT16 and $R+
 0.15     16.11.08  we          Use Ptr2Inc from BTypes
 0.16     27.07.10  we          AES_Err_Invalid_16Bit_Length
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2007-2010 Wolfgang Ehrhardt

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


interface

uses
  BTypes, AES_Type, AES_Base, AES_Encr, AES_Decr;


type
  TAES_XTSContext = packed record
                      main : TAESContext; {Main  context}
                      tweak: TAESContext; {Tweak context}
                    end;


function AES_XTS_Init_Encr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS encrypt context (key expansion), error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

function AES_XTS_Encrypt(ptp, ctp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Encrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}
  {$ifdef DLL} stdcall; {$endif}

function AES_XTS_Init_Decr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS decrypt context (key expansion), error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

function AES_XTS_Decrypt(ctp, ptp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Decrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------}
procedure mul_a(var T: TAESBlock);
  {-Multiply tweak block by the primitive element a from GF(2^128)}
var
  i: integer;
  cin,cout: byte;
const
  masks: array[0..1] of byte = (0,$87);
begin
  cin := 0;
  {Turn off range checking for byte shifts}
  {$ifopt R+} {$define SetRPlus} {$else} {$undef SetRPlus} {$endif}
  {$R-}
  for i:=0 to AESBLKSIZE-1 do begin
    cout := T[i] shr 7;
    T[i] := (T[i] shl 1) or cin;
    cin  := cout;
  end;
  T[0] := T[0] xor masks[cin];
  {$ifdef SetRPlus}
    {$R+}
  {$endif}
end;


{---------------------------------------------------------------------------}
function AES_XTS_Init_Encr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS encrypt context (key expansion), error if invalid key size}
var
  err: integer;
begin
  fillchar(ctx, sizeof(ctx), 0);
  err := AES_Init(K1, KBits, ctx.main);
  if err=0 then err := AES_Init(K2, KBits, ctx.tweak);
  AES_XTS_Init_Encr := err;
end;


{---------------------------------------------------------------------------}
function AES_XTS_Init_Decr({$ifdef CONST}const{$else}var{$endif} K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
  {-Init XTS decrypt context (key expansion), error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}
var
  err: integer;
begin
  fillchar(ctx, sizeof(ctx), 0);
  err := AES_Init_Decr(K1, KBits, ctx.main);
  if err=0 then err := AES_Init(K2, KBits, ctx.tweak);
  AES_XTS_Init_Decr := err;
end;


{---------------------------------------------------------------------------}
function AES_XTS_Encrypt(ptp, ctp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Encrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}
var
  i,n: longint;
  m: word;
begin

  AES_XTS_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.main.Decrypt<>0 then begin
    AES_XTS_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_XTS_Encrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ILen+ofs(ptp^) > $FFFF) or (ILen+ofs(ctp^) > $FFFF) then begin
      AES_XTS_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_XTS_Encrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {encrypt the tweak twk, tweak.IV = enc(twk)}
  AES_Encrypt(ctx.tweak, twk, ctx.tweak.IV);

  with ctx.main do begin
    {process full blocks}
    for i:=1 to n do begin
      AES_XorBlock(PAESBlock(ptp)^, ctx.tweak.IV, buf);
      AES_Encrypt(ctx.main, buf, buf);
      AES_XorBlock(buf, ctx.tweak.IV, PAESBlock(ctp)^);
      mul_a(ctx.tweak.IV);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing, encrypt last full plaintext block}
      AES_XorBlock(PAESBlock(ptp)^, ctx.tweak.IV, buf);
      AES_Encrypt(ctx.main, buf, buf);
      AES_XorBlock(buf, ctx.tweak.IV, buf);
      mul_a(ctx.tweak.IV);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      {pad and encrypt final short block}
      IV := buf;
      move(PAESBlock(ptp)^, IV, m);
      AES_XorBlock(IV, ctx.tweak.IV, IV);
      AES_Encrypt(ctx.main, IV, IV);
      AES_XorBlock(IV, ctx.tweak.IV, PAESBlock(ctp)^);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      move(buf,PAESBlock(ctp)^,m);
    end;
  end;
end;


{---------------------------------------------------------------------------}
function AES_XTS_Decrypt(ctp, ptp: Pointer; ILen: longint;
            {$ifdef CONST}const{$else}var{$endif} twk: TAESBlock; var ctx: TAES_XTSContext): integer;
  {-Decrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}
var
  i,n: longint;
  m: word;
begin

  AES_XTS_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.main.Decrypt=0 then begin
    AES_XTS_Decrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_XTS_Decrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ILen+ofs(ptp^) > $FFFF) or (ILen+ofs(ctp^) > $FFFF) then begin
      AES_XTS_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_XTS_Decrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {encrypt the tweak twk, tweak.IV = enc(twk)}
  AES_Encrypt(ctx.tweak, twk, ctx.tweak.IV);

  with ctx.main do begin
    for i:=1 to n do begin
      AES_XorBlock(PAESBlock(ctp)^, ctx.tweak.IV, buf);
      AES_Decrypt(ctx.main, buf, buf);
      AES_XorBlock(buf, ctx.tweak.IV, PAESBlock(ptp)^);
      mul_a(ctx.tweak.IV);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing, "increment" tweak because}
      {final short plaintext is padded in this block}
      IV := ctx.tweak.IV;
      mul_a(IV);
      {Decrypt last full ciphertext block <-> final short plaintext}
      AES_XorBlock(PAESBlock(ctp)^, IV, buf);
      AES_Decrypt(ctx.main, buf, buf);
      AES_XorBlock(buf, IV, buf);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      {pad and decrypt short CT block to last full PT block}
      IV := buf;
      move(PAESBlock(ctp)^, IV, m);
      AES_XorBlock(IV, ctx.tweak.IV, IV);
      AES_Decrypt(ctx.main, IV, IV);
      AES_XorBlock(IV, ctx.tweak.IV, PAESBlock(ptp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      move(buf,PAESBlock(ptp)^,m);
    end;
  end;
end;


end.
