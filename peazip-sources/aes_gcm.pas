unit AES_GCM;

(*************************************************************************

 DESCRIPTION     :  AES GCM mode functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D12/D17-D18/D25S, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 WARNING         :  GCM mode (as all CTR modes) demands that the same key / IV pair
                    is never reused for encryption.

 REFERENCES      :  [1] D. McGrew, J. Viega, The Galois/Counter Mode of Operation (GCM)
                        http://www.csrc.nist.gov/groups/ST/toolkit/BCM/documents/proposedmodes/gcm/gcm-revised-spec.pdf
                    [2] B. Gladman, source code archives aes-modes-src-23-07-09.zip and aes-modes-vs2008-07-10-08.zip
                        http://gladman.plushost.co.uk/oldsite/AES/index.php, source code archives
                    [3] M. Dworkin, Recommendation for Block Cipher Modes of Operation: Galois/Counter Mode (GCM) and GMAC
                        http://csrc.nist.gov/publications/nistpubs/800-38D/SP-800-38D.pdf

 REMARKS         :  This implementation uses a GCM version with 4KB table.
                    [A table-less version can be generated if gf_t4k is removed
                     and gf_mul_h simply uses gf_mul(a, ctx.ghash_h).
                     Using a 256 byte table is slower than a table-less version,
                     a 64KB table version is incompatible with 16-bit code.]

                    See [3] for recommendations on IV and tag length


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20.09.10  W.Ehrhardt  Initial version: mul_x
 0.11     20.09.10  we          Make4K_Table
 0.12     20.09.10  we          AES_GCM_Init
 0.13     20.09.10  we          gf_mul_h
 0.14     20.09.10  we          AES_GCM_Reset_IV
 0.15     21.09.10  we          basic gf_mul
 0.16     21.09.10  we          AES_GCM_Final
 0.17     21.09.10  we          AES_GCM_Encrypt
 0.18     21.09.10  we          AES_GCM_Add_AAD
 0.19     21.09.10  we          Fix quirk with byte-wise encryption
 0.20     21.09.10  we          Fix quirk with IV_len=16
 0.21     22.09.10  we          AES_GCM_Decrypt
 0.22     22.09.10  we          $ifdef GCM4KTab, improved gf_mul, mul_x
 0.23     22.09.10  we          $ifdef inline_xorblock in gf_mul
 0.24     23.09.10  we          word IV_Len, more argument checks
 0.25     23.09.10  we          AES_GCM_Enc_Auth, AES_GCM_Dec_Veri
 0.26     24.09.10  we          AES_Err_GCM_Auth_After_Final
 0.27     24.09.10  we          Fix for aad_cnt >= 2^29 or txt_acnt >= 2^29
 0.28     26.09.10  we          64 bit counter for aad and auth. text
 0.29     26.09.10  we          use int64 if available
 0.30     02.07.12  we          64-bit adjustments
 0.31     21.11.12  we          64-bit fixes (use 32-bit not 16-bit code)
 0.32     31.08.15  we          constant time compare in Internal_Dec_Veri
 0.33     08.08.17  we          RB for CPUARM
**************************************************************************)


(*-------------------------------------------------------------------------
 Pascal Implementation  (C) Copyright 2010-2017 Wolfgang Ehrhardt

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

type
  TGCM_Tab4K = array[0..255] of TAESBlock;   {64 KB gf_mul_h table  }

type
  TBit64 = packed array[0..1] of longint;    {64 bit counter        }

type
  TAES_GCMContext = packed record
                      actx    : TAESContext; {Basic AES context     }
                      aad_ghv : TAESBlock;   {ghash value AAD       }
                      txt_ghv : TAESBlock;   {ghash value ciphertext}
                      ghash_h : TAESBlock;   {ghash H value         }
                      gf_t4k  : TGCM_Tab4K;  {gf_mul_h table        }
                      aad_cnt : TBit64;      {processed AAD bytes   }
                      atx_cnt : TBit64;      {authent. text bytes   }
                      y0_val  : longint;     {initial 32-bit ctr val}
                    end;


{$ifdef CONST}
function AES_GCM_Init(const Key; KeyBits: word; var ctx: TAES_GCMContext): integer;
  {-Init context, calculate key-dependent GF(2^128) element H=E(K,0) and mul tables}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function AES_GCM_Init(var Key; KeyBits: word; var ctx: TAES_GCMContext): integer;
  {-Init context, calculate key-dependent GF(2^128) element H=E(K,0) and gf_mul tables}
{$endif}

function AES_GCM_Reset_IV(pIV: pointer; IV_len: word; var ctx: TAES_GCMContext): integer;
  {-Reset: keep key but start new encryption with given IV}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update auth data}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode, update auth data}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Add_AAD(pAAD: pointer; aLen: longint; var ctx: TAES_GCMContext): integer;
  {-Add additional authenticated data (will not be encrypted)}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Final(var tag: TAESBlock; var ctx: TAES_GCMContext): integer;
  {-Compute GCM tag from context}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Enc_Auth(var tag: TAESBlock;                     {Tag record}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ptp: pointer; pLen: longint;        {plaintext: address / length}
                              ctp: pointer;                       {ciphertext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
  {-All-in-one call to encrypt/authenticate}
  {$ifdef DLL} stdcall; {$endif}

function AES_GCM_Dec_Veri(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ctp: pointer; cLen: longint;        {ciphertext: address / length}
                              ptp: pointer;                       {plaintext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}
  {$ifdef DLL} stdcall; {$endif}

{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{internal/testing}
procedure gf_mul_h(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} ctx: TAES_GCMContext);
  {-Multiply a by ctx.ghash_h in GF(2^128}

procedure gf_mul(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} b: TAESBlock);
  {-multiply two GF(2**128) field elements, a := a*b}


implementation

(* This implementation is based on Brian Gladman's source codes [2] which are

---------------------------------------------------------------------------
 Copyright (c) 1998-2008, Brian Gladman, Worcester, UK. All rights reserved.

 LICENSE TERMS

 The redistribution and use of this software (with or without changes)
 is allowed without the payment of fees or royalties provided that:

  1. source code distributions include the above copyright notice, this
     list of conditions and the following disclaimer;

  2. binary distributions include the above copyright notice, this list
     of conditions and the following disclaimer in their documentation;

  3. the name of the copyright holder is not used to endorse products
     built using this software without specific written permission.

 DISCLAIMER

 This software is provided 'as is' with no explicit or implied warranties
 in respect of its properties, including, but not limited to, correctness
 and/or fitness for purpose.
---------------------------------------------------------------------------
*)


const
  CTR_POS  = 12;
  BLK_MASK = AESBLKSIZE-1;

const
   gft_le: array[0..255] of word = ( {Table of 'carries' in mulx8 operation}
             $0000,$c201,$8403,$4602,$0807,$ca06,$8c04,$4e05,$100e,$d20f,$940d,$560c,$1809,$da08,$9c0a,$5e0b,
             $201c,$e21d,$a41f,$661e,$281b,$ea1a,$ac18,$6e19,$3012,$f213,$b411,$7610,$3815,$fa14,$bc16,$7e17,
             $4038,$8239,$c43b,$063a,$483f,$8a3e,$cc3c,$0e3d,$5036,$9237,$d435,$1634,$5831,$9a30,$dc32,$1e33,
             $6024,$a225,$e427,$2626,$6823,$aa22,$ec20,$2e21,$702a,$b22b,$f429,$3628,$782d,$ba2c,$fc2e,$3e2f,
             $8070,$4271,$0473,$c672,$8877,$4a76,$0c74,$ce75,$907e,$527f,$147d,$d67c,$9879,$5a78,$1c7a,$de7b,
             $a06c,$626d,$246f,$e66e,$a86b,$6a6a,$2c68,$ee69,$b062,$7263,$3461,$f660,$b865,$7a64,$3c66,$fe67,
             $c048,$0249,$444b,$864a,$c84f,$0a4e,$4c4c,$8e4d,$d046,$1247,$5445,$9644,$d841,$1a40,$5c42,$9e43,
             $e054,$2255,$6457,$a656,$e853,$2a52,$6c50,$ae51,$f05a,$325b,$7459,$b658,$f85d,$3a5c,$7c5e,$be5f,
             $00e1,$c2e0,$84e2,$46e3,$08e6,$cae7,$8ce5,$4ee4,$10ef,$d2ee,$94ec,$56ed,$18e8,$dae9,$9ceb,$5eea,
             $20fd,$e2fc,$a4fe,$66ff,$28fa,$eafb,$acf9,$6ef8,$30f3,$f2f2,$b4f0,$76f1,$38f4,$faf5,$bcf7,$7ef6,
             $40d9,$82d8,$c4da,$06db,$48de,$8adf,$ccdd,$0edc,$50d7,$92d6,$d4d4,$16d5,$58d0,$9ad1,$dcd3,$1ed2,
             $60c5,$a2c4,$e4c6,$26c7,$68c2,$aac3,$ecc1,$2ec0,$70cb,$b2ca,$f4c8,$36c9,$78cc,$bacd,$fccf,$3ece,
             $8091,$4290,$0492,$c693,$8896,$4a97,$0c95,$ce94,$909f,$529e,$149c,$d69d,$9898,$5a99,$1c9b,$de9a,
             $a08d,$628c,$248e,$e68f,$a88a,$6a8b,$2c89,$ee88,$b083,$7282,$3480,$f681,$b884,$7a85,$3c87,$fe86,
             $c0a9,$02a8,$44aa,$86ab,$c8ae,$0aaf,$4cad,$8eac,$d0a7,$12a6,$54a4,$96a5,$d8a0,$1aa1,$5ca3,$9ea2,
             $e0b5,$22b4,$64b6,$a6b7,$e8b2,$2ab3,$6cb1,$aeb0,$f0bb,$32ba,$74b8,$b6b9,$f8bc,$3abd,$7cbf,$bebe);


{$ifndef BIT16}
{32/64-bit code}

{$ifdef BIT64}
{---------------------------------------------------------------------------}
function RB(A: longint): longint;  {$ifdef HAS_INLINE} inline; {$endif}
  {-reverse byte order in longint}
begin
  RB := ((A and $FF) shl 24) or ((A and $FF00) shl 8) or ((A and $FF0000) shr 8) or ((A and longint($FF000000)) shr 24);
end;
{$else}
{$ifdef CPUARM}
{---------------------------------------------------------------------------}
function RB(A: longint): longint;  {$ifdef HAS_INLINE} inline; {$endif}
  {-reverse byte order in longint}
begin
  RB := ((A and $FF) shl 24) or ((A and $FF00) shl 8) or ((A and $FF0000) shr 8) or ((A and longint($FF000000)) shr 24);
end;
{$else}
{---------------------------------------------------------------------------}
function RB(A: longint): longint; assembler;  {&frame-}
  {-reverse byte order in longint}
asm
  {$ifdef LoadArgs}
    mov eax,[A]
  {$endif}
    xchg al,ah
    rol  eax,16
    xchg al,ah
end;
{$endif}
{$endif}


{$ifndef HAS_INT64}
{---------------------------------------------------------------------------}
procedure Inc64(var whi, wlo: longint; BLen: longint);
  {-Add BLen to 64 bit value (wlo, whi)}
begin
  asm
    mov  edx, wlo
    mov  ecx, whi
    mov  eax, Blen
    add  [edx], eax
    adc  dword ptr [ecx], 0
  end;
end;
{$endif}


{---------------------------------------------------------------------------}
procedure mul_x(var a: TAESBlock; const b: TAESBlock);
  {-Multiply a = b*x in GF(2^128)}
var
  t: longint;
  y: TWA4 absolute b;
const
  MASK_80 = longint($80808080);
  MASK_7F = longint($7f7f7f7f);
begin
  t := gft_le[(y[3] shr 17) and MASK_80];
  TWA4(a)[3] :=  ((y[3] shr 1) and MASK_7F) or (((y[3] shl 15) or (y[2] shr 17)) and MASK_80);
  TWA4(a)[2] :=  ((y[2] shr 1) and MASK_7F) or (((y[2] shl 15) or (y[1] shr 17)) and MASK_80);
  TWA4(a)[1] :=  ((y[1] shr 1) and MASK_7F) or (((y[1] shl 15) or (y[0] shr 17)) and MASK_80);
  TWA4(a)[0] := (((y[0] shr 1) and MASK_7F) or ( (y[0] shl 15) and MASK_80)) xor t;
end;

{$else}

{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $86/$C6/      { xchg dh,al}
  $86/$E2);     { xchg dl,ah}


{---------------------------------------------------------------------------}
procedure Inc64(var whi, wlo: longint; BLen: longint);
  {-Add BLen to 64 bit value (wlo, whi)}
inline(
  $58/                 {pop  ax           }
  $5A/                 {pop  dx           }
  $5B/                 {pop  bx           }
  $07/                 {pop  es           }
  $26/$01/$07/         {add  es:[bx],ax   }
  $26/$11/$57/$02/     {adc  es:[bx+02],dx}
  $5B/                 {pop  bx           }
  $07/                 {pop  es           }
  $26/$83/$17/$00/     {adc  es:[bx],0    }
  $26/$83/$57/$02/$00);{adc  es:[bx+02],0 }


{---------------------------------------------------------------------------}
procedure mul_x(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} b: TAESBlock);
  {-Multiply a = b*x in GF(2^128)}
var
  x: TWA4;
  t: longint;
const
  hibit  : array[0..1] of longint = (0, longint($80000000));
  gf_poly: array[0..1] of longint = (0, longint($e1000000));
begin
  x[0] := RB(TWA4(b)[0]);
  x[1] := RB(TWA4(b)[1]);
  x[2] := RB(TWA4(b)[2]);
  x[3] := RB(TWA4(b)[3]);
  t    := gf_poly[x[3] and 1];
  x[3] := (x[3] shr 1) or hibit[x[2] and 1];
  x[2] := (x[2] shr 1) or hibit[x[1] and 1];
  x[1] := (x[1] shr 1) or hibit[x[0] and 1];
  TWA4(a)[0] := RB((x[0] shr 1) xor t);
  TWA4(a)[1] := RB(x[1]);
  TWA4(a)[2] := RB(x[2]);
  TWA4(a)[3] := RB(x[3]);
end;
{$endif}


{Note: At least on my machine inlining AES_Xorblock in gf_mul is slower for}
{32-bit Delphi! The conditional define can be adjusted on other machines,}
{$ifdef DELPHI}
  {$undef inline_xorblock}
{$else}
  {$define inline_xorblock}
{$endif}

{---------------------------------------------------------------------------}
procedure gf_mul(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} b: TAESBlock);
  {-multiply two GF(2**128) field elements, a := a*b}
var
  p: array[0..7] of TAESBlock;
  r: TAESBlock;
  x: TWA4 absolute r;
{$ifndef BIT16}
  t: longint;
{$else}
  w: word;
{$endif}
{$ifdef inline_xorblock}
  j: integer;
{$endif}
  i: integer;
  c: byte;
begin
  p[0] := b;
  for i:=1 to 7 do mul_x(p[i], p[i-1]);
  fillchar(r,sizeof(r),0);
  for i:=0 to 15 do begin
    c := a[15-i];
    if i>0 then begin
      {this is the inline code of the mul_x8 procedure}
      {$ifndef BIT16}
        t := gft_le[x[3] shr 24];
        x[3] := ((x[3] shl 8) or  (x[2] shr 24));
        x[2] := ((x[2] shl 8) or  (x[1] shr 24));
        x[1] := ((x[1] shl 8) or  (x[0] shr 24));
        x[0] := ((x[0] shl 8) xor t);
      {$else}
        w := gft_le[r[15]];
        r[15] := r[14];
        r[14] := r[13];
        r[13] := r[12];
        r[12] := r[11];
        r[11] := r[10];
        r[10] := r[09];
        r[09] := r[08];
        r[08] := r[07];
        r[07] := r[06];
        r[06] := r[05];
        r[05] := r[04];
        r[04] := r[03];
        r[03] := r[02];
        r[02] := r[01];
        r[01] := r[00] xor hi(w);
        r[00] := byte(w);
      {$endif}
    end;
    {$ifdef inline_xorblock}
      for j:=0 to 7 do begin
        if c and ($80 shr j) <> 0 then begin
          x[3] := x[3] xor TWA4(p[j])[3];
          x[2] := x[2] xor TWA4(p[j])[2];
          x[1] := x[1] xor TWA4(p[j])[1];
          x[0] := x[0] xor TWA4(p[j])[0];
        end;
      end;
    {$else}
      if c and $80 <> 0 then AES_Xorblock(r,p[0],r);
      if c and $40 <> 0 then AES_Xorblock(r,p[1],r);
      if c and $20 <> 0 then AES_Xorblock(r,p[2],r);
      if c and $10 <> 0 then AES_Xorblock(r,p[3],r);
      if c and $08 <> 0 then AES_Xorblock(r,p[4],r);
      if c and $04 <> 0 then AES_Xorblock(r,p[5],r);
      if c and $02 <> 0 then AES_Xorblock(r,p[6],r);
      if c and $01 <> 0 then AES_Xorblock(r,p[7],r);
    {$endif}
  end;
  a := r;
end;


{---------------------------------------------------------------------------}
procedure Make4K_Table(var ctx: TAES_GCMContext);
  {-Build 4K gf_mul table from ctx.ghash_h. Assumes gf_t4k is zero-filled}
var
  j,k: integer;
begin
  with ctx do begin
    gf_t4k[128] := ghash_h;
    j := 64;
    while j>0 do begin
      mul_x(gf_t4k[j], gf_t4k[j+j]);
      j := j shr 1;
    end;
    j := 2;
    while j<256 do begin
      for k:=1 to j-1 do aes_xorblock(gf_t4k[k], gf_t4k[j], gf_t4k[j+k]);
      j := j+j;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure gf_mul_h(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} ctx: TAES_GCMContext);
  {-Multiply a by ctx.ghash_h in GF(2^128}
var
  r: TAESBlock;
  x: TWA4 absolute r;
  i: integer;
  t: longint;
  p: pointer;
begin
  with ctx do begin
    r := gf_t4k[a[15]];
    for i:=14 downto 0 do begin
      p := @gf_t4k[a[i]];
      t := gft_le[x[3] shr 24];
      {perform mul_x8 and xor in pre-computed table entries}
      {$ifndef BIT16}
        x[3] := ((x[3] shl 8) or  (x[2] shr 24)) xor TWA4(p^)[3];
        x[2] := ((x[2] shl 8) or  (x[1] shr 24)) xor TWA4(p^)[2];
        x[1] := ((x[1] shl 8) or  (x[0] shr 24)) xor TWA4(p^)[1];
        x[0] := ((x[0] shl 8) xor t) xor TWA4(p^)[0];
      {$else}
        {$ifdef BASM16}
          asm
                      les di, [p]
             db $66;  mov bx, word ptr x[3*4]
             db $66;  mov cx, word ptr x[2*4]
             db $66;  mov dx, word ptr x[1*4]
             db $66;  mov si, word ptr x[0]

             db $66;  mov ax, cx
             db $66;  shr ax, 24
             db $66;  shl bx, 8
             db $66;  or  ax, bx
             db $66;  xor ax, es:[di+12]
             db $66;  mov word ptr x[3*4],ax

             db $66;  mov ax, dx
             db $66;  shr ax, 24
             db $66;  shl cx, 8
             db $66;  or  ax, cx
             db $66;  xor ax, es:[di+8]
             db $66;  mov word ptr x[2*4],ax

             db $66;  mov ax, si
             db $66;  shr ax, 24
             db $66;  shl dx, 8
             db $66;  or  ax, dx
             db $66;  xor ax, es:[di+4]
             db $66;  mov word ptr x[1*4],ax

             db $66;  shl si, 8
             db $66;  xor si, word ptr t
             db $66;  xor si, es:[di]
             db $66;  mov word ptr x[0],si
          end;
        {$else}
           t := gft_le[r[15]];
           r[15] := r[14];
           r[14] := r[13];
           r[13] := r[12];
           r[12] := r[11];
           r[11] := r[10];
           r[10] := r[09];
           r[09] := r[08];
           r[08] := r[07];
           r[07] := r[06];
           r[06] := r[05];
           r[05] := r[04];
           r[04] := r[03];
           r[03] := r[02];
           r[02] := r[01];
           r[01] := r[00] xor TBA4(t)[1];
           r[00] := byte(t);
           x[3] := x[3] xor TWA4(p^)[3];
           x[2] := x[2] xor TWA4(p^)[2];
           x[1] := x[1] xor TWA4(p^)[1];
           x[0] := x[0] xor TWA4(p^)[0];
        {$endif}
      {$endif}
    end;
    a := r;
  end;
end;


(*
{Use this for table-less versions}
{---------------------------------------------------------------------------}
procedure gf_mul_h(var a: TAESBlock; {$ifdef CONST}const{$else}var{$endif} ctx: TAES_GCMContext);
  {-Multiply a by ctx.ghash_h in GF(2^128}
begin
  {Simply compute a*ghash_h, pre-computing the p-array for ghash_h}
  {does not hurt, but does not give significant speed improvements.}
  gf_mul(a, ctx.ghash_h);
end;
*)

{---------------------------------------------------------------------------}
function AES_GCM_Init({$ifdef CONST}const{$else}var{$endif} Key; KeyBits: word; var ctx: TAES_GCMContext): integer;
  {-Init context, calculate key-dependent GF(2^128) element H=E(K,0) and gf_mul tables}
var
  err: integer;
begin
  fillchar(ctx, sizeof(ctx), 0);
  with ctx do begin
    err := AES_Init_Encr(Key, KeyBits, actx);
    if err=0 then begin
      AES_Encrypt(actx, ghash_h, ghash_h);
      Make4K_Table(ctx);
    end;
  end;
  AES_GCM_Init := err;
end;


{---------------------------------------------------------------------------}
procedure GCM_IncCtr(var x: TAESBlock);
  {-GCM IncProc, only 32 LSB bits are changed (big-endian notation)}
var
  j: integer;
begin
  for j:=15 downto CTR_POS do begin
    if x[j]=$FF then x[j] := 0
    else begin
      inc(x[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Reset_IV(pIV: pointer; IV_len: word; var ctx: TAES_GCMContext): integer;
  {-Reset: keep key but start new encryption with given IV}
var
  n_pos: longint;
  i: integer;
begin
  AES_GCM_Reset_IV := 0;

  if (pIV=nil) and (IV_Len<>0) then begin
    AES_GCM_Reset_IV := AES_Err_NIL_Pointer;
    exit;
  end;

  with ctx do begin
    {compute initial IV counter value Y0}
    if IV_len=CTR_POS then begin
      {if bitlen(IV)=96 then Y0=IV||1}
      move(pIV^, actx.IV, CTR_POS);
      TWA4(actx.IV)[3] := $01000000;  {big-endian 32-bit 1}
    end
    else begin
      {Y0 = GHASH(IV, H)}
      n_pos := IV_len;
      fillchar(actx.IV, sizeof(actx.IV),0);
      while n_pos >= AESBLKSIZE do begin
        AES_Xorblock(actx.IV, PAESBlock(pIV)^, actx.IV);
        inc(Ptr2Inc(pIV), AESBLKSIZE);
        dec(n_pos, AESBLKSIZE);
        gf_mul_h(actx.IV, ctx);
      end;
      if n_pos>0 then begin
        for i:=0 to n_pos-1 do begin
          actx.IV[i] := actx.IV[i] xor pByte(pIV)^;
          inc(Ptr2Inc(pIV));
        end;
        gf_mul_h(actx.IV, ctx);
      end;
      n_pos := longint(IV_len) shl 3;
      i := 15;
      while n_pos>0 do begin
        actx.IV[i] := actx.IV[i] xor byte(n_pos);
        n_pos := n_pos shr 8;
        dec(i);
      end;
      gf_mul_h(actx.IV, ctx);
    end;
    {save initial 32-bit ctr val for final operation}
    y0_val := TWA4(actx.IV)[3];
    {Reset other data}
    fillchar(aad_ghv, sizeof(aad_ghv),0);
    fillchar(txt_ghv, sizeof(txt_ghv),0);
    actx.Flag  := 0;
    actx.bLen  := 0;
    aad_cnt[0] := 0;
    aad_cnt[1] := 0;
    atx_cnt[0] := 0;
    atx_cnt[1] := 0;
  end;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Final(var tag: TAESBlock; var ctx: TAES_GCMContext): integer;
  {-Compute GCM tag from context}
var
  tbuf: TAESBlock;
  x: TWA4 absolute tbuf;
  {$ifdef HAS_INT64}
    ln: int64;
  {$else}
    ln: TBit64;
  {$endif}
begin
  with ctx do begin
    if actx.Flag and $02 <> 0 then begin
      AES_GCM_Final := AES_Err_GCM_Auth_After_Final;
      exit;
    end;
    actx.Flag := actx.Flag or $02;
    {compute GHASH(H, AAD, ctp)}
    gf_mul_h(aad_ghv, ctx);
    gf_mul_h(txt_ghv, ctx);

    {Compute len(AAD) || len(ctp) with each len as 64-bit big-endian }
    {Note: Tag calculation with Brian Gladman's original code will be}
    {incorrect if either of the following shifts produces carries:   }
    {(ctx->txt_acnt << 3) or (ctx->hdr_cnt << 3)}

    {$ifdef HAS_INT64}
      {Gladman's code with 64-bit counters}
      ln := (int64(atx_cnt) + AESBLKSIZE - 1) div AESBLKSIZE;
      if (int64(aad_cnt)>0) and (ln<>0) then begin
        tbuf := ghash_h;
        while ln<>0 do begin
          if odd(ln) then gf_mul(aad_ghv, tbuf);
          ln := ln shr 1;
          if ln<>0 then gf_mul(tbuf, tbuf);
        end;
      end;
    {$else}
      {equivalent code for compilers without int64}
      ln[0] := atx_cnt[0];
      ln[1] := atx_cnt[1];
      Inc64(ln[1], ln[0], AESBLKSIZE - 1);
      ln[0] := (ln[0] shr 4) or ((ln[1] and $F) shl 28);
      ln[1] := ln[1] shr 4;
      if (aad_cnt[0] or aad_cnt[1] <> 0) and (ln[0] or ln[1] <> 0) then begin
        tbuf := ghash_h;
        while (ln[0] or ln[1])<>0 do begin
          if odd(ln[0]) then gf_mul(aad_ghv, tbuf);
          ln[0] := ln[0] shr 1;
          if odd(ln[1]) then ln[0] := ln[0] or longint($80000000);
          ln[1] := ln[1] shr 1;
          if ln[0] or ln[1] <> 0 then gf_mul(tbuf, tbuf);
        end;
      end;
    {$endif}
    x[0] := RB((aad_cnt[0] shr 29) or (aad_cnt[1] shl 3));
    x[1] := RB((aad_cnt[0] shl  3));
    x[2] := RB((atx_cnt[0] shr 29) or (atx_cnt[1] shl 3));
    x[3] := RB((atx_cnt[0] shl  3));

    AES_Xorblock(tbuf, txt_ghv, tbuf);
    AES_Xorblock(aad_ghv, tbuf, aad_ghv);
    gf_mul_h(aad_ghv, ctx);
    {compute E(K,Y0)}
    tbuf := actx.IV;
    x[3] := y0_val;
    AES_Encrypt(actx, tbuf, tbuf);
    {tag = GHASH(H, AAD, ctp) xor E(K,Y0)}
    AES_Xorblock(aad_ghv, tbuf, tag);
  end;
  AES_GCM_Final := 0;
end;


{---------------------------------------------------------------------------}
function crypt_data(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Internal: en/decrypt ILen byte from ptp to ctp}
var
  cnt: longint;
  b_pos: integer;
begin
  crypt_data := 0;
  if ILen<=0 then exit;
  cnt := 0;
  with ctx do begin
    b_pos := actx.bLen;
    if b_pos=0 then b_pos := AESBLKSIZE
    else begin
      while (cnt < ILen) and (b_pos < AESBLKSIZE) do begin
        pByte(ctp)^ := pByte(ptp)^ xor actx.buf[b_pos];
        inc(b_pos);
        inc(Ptr2Inc(ptp));
        inc(Ptr2Inc(ctp));
        inc(cnt);
      end;
    end;
    while cnt + AESBLKSIZE <= ILen do begin
      GCM_IncCtr(actx.IV);
      AES_Encrypt(actx, actx.IV, actx.buf);
      AES_XorBlock(PAESBlock(ptp)^, actx.buf, PAESBlock(ctp)^);
      inc(Ptr2Inc(ptp), AESBLKSIZE);
      inc(Ptr2Inc(ctp), AESBLKSIZE);
      inc(cnt, AESBLKSIZE);
    end;
    while cnt < ILen do begin
      if b_pos=AESBLKSIZE then begin
        GCM_IncCtr(actx.IV);
        AES_Encrypt(actx, actx.IV, actx.buf);
        b_pos := 0;
      end;
      pByte(ctp)^ := actx.buf[b_pos] xor pByte(ptp)^;
      inc(b_pos);
      inc(Ptr2Inc(ptp));
      inc(Ptr2Inc(ctp));
      inc(cnt);
    end;
    actx.bLen := (actx.bLen + cnt) and BLK_MASK;;
  end;
end;


{---------------------------------------------------------------------------}
function auth_data(ctp: pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Internal: add ILen bytes from cipher text to auth ghash}
var
  cnt: longint;
  b_pos: integer;
begin
  auth_data := 0;
  if ILen<=0 then exit;
  cnt := 0;
  with ctx do begin
    if actx.Flag and $02 <> 0 then begin
      auth_data := AES_Err_GCM_Auth_After_Final;
      exit;
    end;
    b_pos := atx_cnt[0] and BLK_MASK;
    if (b_pos=0) and (atx_cnt[0] or atx_cnt[1] <> 0) then gf_mul_h(txt_ghv, ctx);
    while (cnt < ILen) and (b_pos < AESBLKSIZE) do begin
      txt_ghv[b_pos] := txt_ghv[b_pos] xor pByte(ctp)^;
      inc(b_pos);
      inc(Ptr2Inc(ctp));
      inc(cnt);
    end;
    while cnt + AESBLKSIZE <= ILen do begin
      gf_mul_h(txt_ghv, ctx);
      AES_Xorblock(txt_ghv, PAESBlock(ctp)^, txt_ghv);
      inc(Ptr2Inc(ctp), AESBLKSIZE);
      inc(cnt, AESBLKSIZE);
    end;
    while cnt < ILen do begin
      if b_pos=AESBLKSIZE then begin
        gf_mul_h(txt_ghv, ctx);
        b_pos := 0;
      end;
      txt_ghv[b_pos] := txt_ghv[b_pos] xor pByte(ctp)^;
      inc(b_pos);
      inc(Ptr2Inc(ctp));
      inc(cnt);
    end;
    {$ifdef HAS_INT64}
      Inc(int64(atx_cnt), cnt);
    {$else}
      Inc64(atx_cnt[1], atx_cnt[0],cnt);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Add_AAD(pAAD: pointer; aLen: longint; var ctx: TAES_GCMContext): integer;
  {-Add additional authenticated data (will not be encrypted)}
var
  cnt: longint;
  b_pos: integer;
begin
  AES_GCM_Add_AAD := 0;
  if aLen <= 0 then exit;

  if pAAD=nil then begin
    AES_GCM_Add_AAD := AES_Err_NIL_Pointer;
    exit;
  end;

  {$ifdef BIT16}
    if (aLen>$FFFF) or (ofs(pAAD^)+aLen>$FFFF) then begin
      AES_GCM_Add_AAD := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  cnt := 0;
  with ctx do begin
    if actx.Flag and $02 <> 0 then begin
      AES_GCM_Add_AAD := AES_Err_GCM_Auth_After_Final;
      exit;
    end;
    b_pos := aad_cnt[0] and BLK_MASK;
    if (b_pos=0) and (aad_cnt[0] or aad_cnt[1] <> 0) then gf_mul_h(aad_ghv, ctx);
    while (cnt < aLen) and (b_pos < AESBLKSIZE) do begin
      aad_ghv[b_pos] := aad_ghv[b_pos] xor pByte(pAAD)^;
      inc(b_pos);
      inc(Ptr2Inc(pAAD));
      inc(cnt);
    end;
    while cnt + AESBLKSIZE <= aLen do begin
      gf_mul_h(aad_ghv, ctx);
      AES_Xorblock(aad_ghv, PAESBlock(pAAD)^, aad_ghv);
      inc(Ptr2Inc(pAAD), AESBLKSIZE);
      inc(cnt, AESBLKSIZE);
    end;
    while cnt < aLen do begin
      if b_pos=AESBLKSIZE then begin
        gf_mul_h(aad_ghv, ctx);
        b_pos := 0;
      end;
      aad_ghv[b_pos] := aad_ghv[b_pos] xor pByte(pAAD)^;
      inc(b_pos);
      inc(Ptr2Inc(pAAD));
      inc(cnt);
    end;
    {$ifdef HAS_INT64}
      Inc(int64(aad_cnt),cnt);
    {$else}
      Inc64(aad_cnt[1],aad_cnt[0],cnt);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update auth data}
var
  err: integer;
begin
  if ILen <= 0 then begin
    AES_GCM_Encrypt := 0;
    exit;
  end;
  if (ptp=nil) or (ctp=nil) then begin
    AES_GCM_Encrypt := AES_Err_NIL_Pointer;
    exit;
  end;
  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_GCM_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}
  err := crypt_data(ptp, ctp, ILen, ctx);
  if err=0 then err := auth_data(ctp, ILen, ctx);
  AES_GCM_Encrypt := err;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode, update auth data}
var
  err: integer;
begin
  if ILen <= 0 then begin
    AES_GCM_Decrypt := 0;
    exit;
  end;
  if (ptp=nil) or (ctp=nil) then begin
    AES_GCM_Decrypt := AES_Err_NIL_Pointer;
    exit;
  end;
  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_GCM_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}
  err := auth_data(ctp, ILen, ctx);
  if err=0 then err := crypt_data(ctp, ptp, ILen, ctx);
  AES_GCM_Decrypt := err;
end;


{---------------------------------------------------------------------------}
function Internal_Dec_Veri(var ctx: TAES_GCMContext; ptag: pointer; tLen: word;
                           ctp: pointer; cLen: longint; ptp: pointer): integer;
  {-calculate and verify tLen bytes of ptag^, decrypt if OK}
var
  err,i: integer;
  diff: byte;
  tag: TAESBlock;
begin
  if cLen <= 0 then cLen := 0;
  if cLen>0 then begin
    if (ptp=nil) or (ctp=nil) then begin
      Internal_Dec_Veri := AES_Err_NIL_Pointer;
      exit;
     end;
    {$ifdef BIT16}
      if (ofs(ptp^)+cLen>$FFFF) or (ofs(ctp^)+cLen>$FFFF) then begin
        Internal_Dec_Veri := AES_Err_Invalid_16Bit_Length;
        exit;
      end;
    {$endif}
  end;
  err := auth_data(ctp, cLen, ctx);
  if err=0 then begin
    {Compute/verify tag before doing decryption}
    err := AES_GCM_Final(tag, ctx);
    if err=0 then begin
      diff :=0;
      for i:=0 to pred(tLen) do begin
        diff := diff or (pByte(ptag)^ xor tag[i]);
        inc(Ptr2Inc(ptag));
      end;
      err := (((integer(diff)-1) shr 8) and 1)-1;  {0 compare, -1 otherwise}
      err := err and AES_Err_GCM_Verify_Tag;
    end;
    if err=0 then err := crypt_data(ctp, ptp, cLen, ctx);
  end;
  Internal_Dec_Veri := err;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Enc_Auth(var tag: TAESBlock;                     {Tag record}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ptp: pointer; pLen: longint;        {plaintext: address / length}
                              ctp: pointer;                       {ciphertext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
  {-All-in-one call to encrypt/authenticate}
var
  err: integer;
begin
  err :=  AES_GCM_Init(Key, KBits, ctx);
  if err=0 then err := AES_GCM_Reset_IV(pIV, IV_len, ctx);
  if err=0 then err := AES_GCM_Add_AAD(pAAD, aLen, ctx);
  if err=0 then err := AES_GCM_Encrypt(ptp, ctp, pLen, ctx);
  if err=0 then err := AES_GCM_Final(tag, ctx);
  fillchar(ctx, sizeof(ctx), 0);
  AES_GCM_Enc_Auth := err;
end;


{---------------------------------------------------------------------------}
function AES_GCM_Dec_Veri(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ctp: pointer; cLen: longint;        {ciphertext: address / length}
                              ptp: pointer;                       {plaintext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}
var
  err: integer;
begin
  err :=  AES_GCM_Init(Key, KBits, ctx);
  if err=0 then err := AES_GCM_Reset_IV(pIV, IV_len, ctx);
  if err=0 then err := AES_GCM_Add_AAD(pAAD, aLen, ctx);
  if err=0 then err := Internal_Dec_Veri(ctx,ptag,tLen,ctp,cLen,ptp);
  fillchar(ctx, sizeof(ctx), 0);
  AES_GCM_Dec_Veri := err;
end;

end.
