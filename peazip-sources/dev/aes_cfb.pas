unit AES_CFB;

(*************************************************************************

 DESCRIPTION     :  AES CFB128 functions
                    Because of buffering en/decrypting is associative

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [3] http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
                    [1] http://csrc.nist.gov/fips/fips-197.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.08.03  we          initial version
 0.11     21.09.03  we          functions, error codes
 0.12     27.09.03  we          FPC/go32v2
 0.13     03.10.03  we          3-para encr/decr
 0.14     05.10.03  we          STD.INC, TP5-6
 0.15     01.01.04  we          Handle full blocks first
 0.16     01.01.04  we          Decrypt: bugfix for ctp=ptp
 0.17     12.06.04  we          uses BLKSIZE constant
 0.18     12.06.04  we          check for nil pointers
 0.19     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.20     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.21     09.07.06  we          Checked: D9-D10
 0.22     16.11.08  we          Use Ptr2Inc, pByte from BTypes
 0.23     27.07.10  we          Longint ILen in AES_CFB_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2010 Wolfgang Ehrhardt

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
  BTypes, AES_Type, AES_Base, AES_Encr;

{$ifdef CONST}
function AES_CFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function AES_CFB_Init(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
{$endif}

function AES_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}
  {$ifdef DLL} stdcall; {$endif}

function AES_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function AES_CFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
{$else}
function AES_CFB_Init(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
{$endif}
  {-AES key expansion, error if invalid key size, encrypt IV}
var
  err: integer;
begin
  {-AES key expansion, error if invalid key size}
  err := AES_Init_Encr(Key, KeyBits, ctx);
  AES_CFB_Init := err;
  if err=0 then begin
    {encrypt IV}
    AES_Encrypt(ctx, IV, ctx.IV);
  end;
end;


{---------------------------------------------------------------------------}
function AES_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}
begin
  AES_CFB_Encrypt := 0;

  if ctx.Decrypt<>0 then begin
    AES_CFB_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CFB_Encrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CFB_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=AESBLKSIZE do with ctx do begin
      {Cipher text = plain text xor encr(IV/CT), cf. [3] 6.3}
      AES_XorBlock(PAESBlock(ptp)^, IV, PAESBlock(ctp)^);
      AES_Encrypt(ctx, PAESBlock(ctp)^, IV);
      inc(Ptr2Inc(ptp), AESBLKSIZE);
      inc(Ptr2Inc(ctp), AESBLKSIZE);
      dec(ILen, AESBLKSIZE);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Test buffer empty}
    if bLen>=AESBLKSIZE then begin
      AES_Encrypt(ctx, buf, IV);
      bLen := 0;
    end;
    buf[bLen] := IV[bLen] xor pByte(ptp)^;
    pByte(ctp)^ := buf[bLen];
    inc(bLen);
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;


{---------------------------------------------------------------------------}
function AES_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}
begin
  AES_CFB_Decrypt := 0;

  if ctx.Decrypt<>0 then begin
    AES_CFB_Decrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CFB_Decrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CFB_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=AESBLKSIZE do with ctx do begin
      {plain text = cypher text xor encr(IV/CT), cf. [3] 6.3}
      {must use buf, otherwise overwrite bug if ctp=ptp}
      buf := PAESBlock(ctp)^;
      AES_XorBlock(buf, IV, PAESBlock(ptp)^);
      AES_Encrypt(ctx, buf, IV);
      inc(Ptr2Inc(ptp), AESBLKSIZE);
      inc(Ptr2Inc(ctp), AESBLKSIZE);
      dec(ILen, AESBLKSIZE);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Test buffer empty}
    if bLen>=AESBLKSIZE then begin
      AES_Encrypt(ctx, buf, IV);
      bLen := 0;
    end;
    buf[bLen] := pByte(ctp)^;
    pByte(ptp)^ := buf[bLen] xor IV[bLen];
    inc(bLen);
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;

end.
