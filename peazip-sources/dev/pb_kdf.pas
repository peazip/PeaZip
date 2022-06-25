unit pb_kdf;

{General Password Based Key Derivation Function 2}


interface

(*************************************************************************

 DESCRIPTION     :  RFC 2898: Password Based Key Derivation Function 2

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://www.faqs.org/rfcs/rfc2898.html
                    http://www.faqs.org/rfcs/rfc3211.html  [includes test vectors]

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     17.01.06  W.Ehrhardt  Initial version based on keyderiv 1.29
 0.11     17.01.06  we          error codes
 0.12     23.01.06  we          new names: unit pb_kdf, functions kdf2/s
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006 Wolfgang Ehrhardt

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



uses
  Hash,HMAC;

type
  TKD_String = string[255];     {easy short type for win32}

const
  kdf_err_nil_pointer = $0001;  {phash descriptor is nil}
  kdf_err_digestlen   = $0002;  {digest length from descriptor is zero}


function kdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}
  {$ifdef DLL} stdcall; {$endif}

function kdf2s(phash: PHashDesc; sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}
  {$ifdef DLL} stdcall; {$endif}


implementation


{helper type}
type
  pByte = ^byte;
  TMSBA = array[0..3] of byte;


{---------------------------------------------------------------------------}
procedure IncMSB(var CTR: TMSBA);
  {-Increment CTR[3]..CTR[0], i.e. 32 Bit MSB counter}
var
  j: integer;
begin
  for j:=3 downto 0 do begin
    inc(CTR[j]);
    if CTR[j]<>0 then exit;
  end;
end;


{---------------------------------------------------------------------------}
function kdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}
var
  i, k, lk, hLen: word;
  j: longint;
  pDK: pByte;   {pointer to DK  }
  ii: TMSBA;    {i in big endian}
  u, ucum: THashDigest;
  ctx: THMAC_Context;
begin
  if phash=nil then begin
    kdf2 := kdf_err_nil_pointer;
    exit;
  end;
  if phash^.HDigestLen=0 then begin
    kdf2 := kdf_err_digestlen;
    exit;
  end;
  kdf2 := 0;
  hLen := phash^.HDigestLen;
  lk := 0;
  pDK := pByte(@DK);
  fillchar(ii, sizeof(ii), 0);
  for i:=1 to 1 + pred(dkLen) div hLen do begin
    IncMSB(ii);
    fillchar(ucum, sizeof(ucum),0);
    for j:=1 to C do begin
      hmac_init(ctx, phash, pPW, pLen);
      if j=1 then begin
        {U_1 = PRF(pPW, salt || ii)}
        hmac_updateXL(ctx, salt, sLen);
        hmac_updateXL(ctx, @ii, 4);
      end
      else begin
        {U_i = PRF(pPW, U_(i-1)}
        hmac_updateXL(ctx, @u, hLen);
      end;
      hmac_final(ctx, u);
      {update cumulative XOR U_i}
      for k:=0 to hLen-1 do ucum[k] := ucum[k] xor u[k];
    end;
    {T_i = F(P,S,C,l) = ucum}
    for k:=0 to hLen-1 do begin
      {concat T_i}
      if lk<dkLen then begin
        pDK^ := ucum[k];
        inc(lk);
        inc(longint(pDK));
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function kdf2s(phash: PHashDesc; sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}
begin
  kdf2s := kdf2(phash, @sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;


end.
