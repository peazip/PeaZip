unit AES_CFB8;

(*************************************************************************

 DESCRIPTION     :  AES CFB8 functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [3] http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
                    [1] http://csrc.nist.gov/fips/fips-197.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     25.12.07  W.Ehrhardt  Initial encrypt version
 0.11     25.12.07  we          AES_CFB8_Decrypt
 0.12     16.11.08  we          Use Ptr2Inc, pByte from BTypes
 0.13     27.07.10  we          Longint ILen in AES_CFB8_En/Decrypt
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
  BTypes, AES_Type, AES_Base, AES_Encr;

{$ifdef CONST}
function AES_CFB8_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, store IV}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function AES_CFB8_Init(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, store IV}
{$endif}

function AES_CFB8_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}
  {$ifdef DLL} stdcall; {$endif}

function AES_CFB8_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function AES_CFB8_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
{$else}
function AES_CFB8_Init(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
{$endif}
  {-AES key expansion, error if invalid key size, store IV}
var
  err: integer;
begin
  {-AES key expansion, error if invalid key size}
  err := AES_Init_Encr(Key, KeyBits, ctx);
  AES_CFB8_Init := err;
  if err=0 then ctx.IV := IV;
end;


{---------------------------------------------------------------------------}
function AES_CFB8_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}
begin
  AES_CFB8_Encrypt := 0;

  if ctx.Decrypt<>0 then begin
    AES_CFB8_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CFB8_Encrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CFB8_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  {Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}
  while ILen>0 do with ctx do begin
    AES_Encrypt(ctx, IV, buf);
    {encrypt next byte}
    pByte(ctp)^ := buf[0] xor pByte(ptp)^;
    {shift 8 bits}
    move(IV[1],IV[0],AESBLKSIZE-1);
    IV[AESBLKSIZE-1] := pByte(ctp)^;
    {increment pointers}
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;


{---------------------------------------------------------------------------}
function AES_CFB8_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}
begin
  AES_CFB8_Decrypt := 0;

  if ctx.Decrypt<>0 then begin
    AES_CFB8_Decrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CFB8_Decrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CFB8_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  {Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}
  while ILen>0 do with ctx do begin
    AES_Encrypt(ctx, IV, buf);
    {shift 8 bits}
    move(IV[1],IV[0],AESBLKSIZE-1);
    IV[AESBLKSIZE-1] := pByte(ctp)^;
    {decrypt next byte}
    pByte(ptp)^ := buf[0] xor pByte(ctp)^;
    {increment pointers}
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;

end.
