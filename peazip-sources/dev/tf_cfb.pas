unit TF_CFB;

(*************************************************************************

 DESCRIPTION     :  Twofish CFB128 functions
                    Because of buffering en/decrypting is associative

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  B.Schneier, Applied Cryptography, 2nd ed., ch. 9.6


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     29.05.06  W.Ehrhardt  Initial version analog BF_CFB
 0.11     16.11.08  we          Use Ptr2Inc, pByte from BTypes
 0.12     31.07.10  we          Longint ILen in TF_CFB_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006-2010 Wolfgang Ehrhardt

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
  BTypes, TF_Base;

{$ifdef CONST}

function  TF_CFB_Init(const Key; KeyBits: word; const IV: TTFBlock; var ctx: TTFContext): integer;
  {-TF key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_CFB_Reset(const IV: TTFBlock; var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  TF_CFB_Init(var Key; KeyBits: word; var IV: TTFBlock; var ctx: TTFContext): integer;
  {-TF key expansion, error if invalid key size, encrypt IV}

procedure TF_CFB_Reset(var IV: TTFBlock; var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag, encrypt IV}

{$endif}

function  TF_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}
  {$ifdef DLL} stdcall; {$endif}

function  TF_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function TF_CFB_Init(const Key; KeyBits: word; const IV: TTFBlock; var ctx: TTFContext): integer;
{$else}
function TF_CFB_Init(var Key; KeyBits: word; var IV: TTFBlock; var ctx: TTFContext): integer;
{$endif}
  {-TF key expansion, error if invalid key size, encrypt IV}
var
  err: integer;
begin
  err := TF_Init(Key, KeyBits, ctx);
  TF_CFB_Init := err;
  if err=0 then begin
    {encrypt IV}
    TF_Encrypt(ctx, IV, ctx.IV);
  end;
end;


{---------------------------------------------------------------------------}
procedure TF_CFB_Reset({$ifdef CONST}const {$else} var {$endif} IV: TTFBlock; var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag, encrypt IV}
begin
  TF_Reset(ctx);
  TF_Encrypt(ctx, IV, ctx.IV);
end;


{---------------------------------------------------------------------------}
function TF_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}
begin
  TF_CFB_Encrypt := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      TF_CFB_Encrypt := TF_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      TF_CFB_Encrypt := TF_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=TFBLKSIZE do with ctx do begin
      {Cipher text = plain text xor encr(IV/CT)}
      TF_XorBlock(PTFBlock(ptp)^, IV, PTFBlock(ctp)^);
      TF_Encrypt(ctx, PTFBlock(ctp)^, IV);
      inc(Ptr2Inc(ptp), TFBLKSIZE);
      inc(Ptr2Inc(ctp), TFBLKSIZE);
      dec(ILen, TFBLKSIZE);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Test buffer empty}
    if bLen>=TFBLKSIZE then begin
      TF_Encrypt(ctx, buf, IV);
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
function TF_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}
begin
  TF_CFB_Decrypt := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      TF_CFB_Decrypt := TF_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      TF_CFB_Decrypt := TF_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=TFBLKSIZE do with ctx do begin
      {plain text = cypher text xor encr(IV/CT)}
      {must use buf, otherwise overwrite bug if ctp=ptp}
      buf := PTFBlock(ctp)^;
      TF_XorBlock(buf, IV, PTFBlock(ptp)^);
      TF_Encrypt(ctx, buf, IV);
      inc(Ptr2Inc(ptp), TFBLKSIZE);
      inc(Ptr2Inc(ctp), TFBLKSIZE);
      dec(ILen, TFBLKSIZE);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Test buffer empty}
    if bLen>=TFBLKSIZE then begin
      TF_Encrypt(ctx, buf, IV);
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
