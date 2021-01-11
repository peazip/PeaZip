unit aes_cprf;

{Variable-length key AES CMAC Pseudo-Random Function-128}

{$i STD.INC}

interface

uses
  AES_Type, AES_OMAC;


(*************************************************************************

 DESCRIPTION   : Variable-length key AES CMAC Pseudo-Random Function-128

 REQUIREMENTS  : TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA : ---

 MEMORY USAGE  : ---

 DISPLAY MODE  : ---

 REFERENCES    : [1] RFC 4615: The Advanced Encryption Standard-Cipher-based
                     Message Authentication Code-Pseudo-Random Function-128
                     (AES-CMAC-PRF-128) Algorithm for the Internet Key
                     Exchange Protocol (IKE)


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     28.05.07  W.Ehrhardt  Initial version
 0.11     28.05.07  we          function returns OMAC results
 0.12     16.06.07  we          AES_CPRF128_selftest stdcall
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2007 Wolfgang Ehrhardt

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


function AES_CPRF128({$ifdef CONST} const Key {$else} var Key {$endif}; KeyBytes: word;
                     msg: pointer; msglen: longint; var PRV: TAESBlock): integer;
  {Calculate variable-length key AES CMAC Pseudo-Random Function-128 for msg}
  {returns AES_OMAC error and 128-bit pseudo-random value PRV}
  {$ifdef DLL} stdcall; {$endif}

function AES_CPRF128_selftest: boolean;
  {-Selftest with RFC 4615 test vectors}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
function AES_CPRF128({$ifdef CONST} const Key {$else} var Key {$endif}; KeyBytes: word;
                     msg: pointer; msglen: longint; var PRV: TAESBlock): integer;
  {Calculate variable-length key AES CMAC Pseudo-Random Function-128 for msg}
  {returns AES_OMAC error and 128-bit pseudo-random value PRV}
var
  LK: TAESBlock;    {local 128 bit key}
  ctx: TAESContext;
  err: integer;
const
  ZB: TAESBlock = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
begin
  if KeyBytes=16 then begin
    {If the key, is exactly 128 bits, then we use it as-is (copy to local)}
    move(Key, LK, 16);
    err := 0;
  end
  else begin
    {If key length is not 128 bits, then we derive the local key LK by
    applying the AES-CMAC algorithm using a 128-bit zero as the CMAC key
    and Key as the input message: LK := AES-CMAC(0, Key, KeyBytes)}
    err := AES_OMAC_Init(ZB, 128, ctx);
    if err=0 then err := AES_OMAC_Update(@Key, KeyBytes, ctx);
    if err=0 then AES_OMAC_Final(LK, ctx);
  end;
  {PRV := AES-CMAC(LK, msg, msglen)}
  if err=0 then err := AES_OMAC_Init(LK, 128, ctx);
  if err=0 then err := AES_OMAC_Update(msg, msglen, ctx);
  if err=0 then AES_OMAC_Final(PRV, ctx);
  AES_CPRF128 := err;
end;


{---------------------------------------------------------------------------}
function AES_CPRF128_selftest: boolean;
  {-Selftest with RFC 4615 test vectors}
var
  PRV: TAESBlock;
  i,j: integer;
const
  {Test vectors from RFC section 4, Message is fix}
  msg: array[0..19] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,
                               $0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13);
  {Base key is fix, but test three diffenrent length >16, =16, <16}
  key: array[0..17] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,
                               $0a,$0b,$0c,$0d,$0e,$0f,$ed,$cb);
   KL: array[1..3] of word =(18,16,10);
  {PRF outputs}
  PRA: array[1..3] of TAESBlock = (($84,$a3,$48,$a4,$a4,$5d,$23,$5b,$ab,$ff,$fc,$0d,$2b,$4d,$a0,$9a),
                                   ($98,$0a,$e8,$7b,$5f,$4c,$9c,$52,$14,$f5,$b6,$a8,$45,$5e,$4c,$2d),
                                   ($29,$0d,$9e,$11,$2e,$db,$09,$ee,$14,$1f,$cf,$64,$c0,$b7,$2f,$3d));
begin
  AES_CPRF128_selftest := false;
  for i:=1 to 3 do begin
    if AES_CPRF128(Key, KL[i], @msg, sizeof(msg), PRV)<>0 then exit;
    for j:=0 to 15 do if PRV[j]<>PRA[i][j] then exit;
  end;
  AES_CPRF128_selftest := true;
end;


end.
