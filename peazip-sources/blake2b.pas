unit blake2b;

{Blake2B - max 512 bit hash/MAC function}

interface

(*************************************************************************

 DESCRIPTION     :  Blake2B - max 512 bit hash/MAC function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - Saarinen et al: The BLAKE2 Cryptographic Hash and Message
                      Authentication  Code (MAC); https://tools.ietf.org/html/rfc7693
                    - Aumasson et al: BLAKE2: simpler, smaller, fast as MD5;
                      https://blake2.net/blake2.pdf
                    - Official reference code: https://github.com/BLAKE2/BLAKE2


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     06.10.17  W.Ehrhardt  Initial D6 implementation (from Blake2s / RFC code)
 0.11     06.10.17  we          removed updatelen, max input < 2^64
 0.12     06.10.17  we          FPC fix
 0.13     06.10.17  we          blake2b_selftest
 0.14     06.10.17  we          used int64 instead of u64
 0.15     07.10.17  we          unroll compress loop for Delphi
 0.16     07.10.17  we          adjust/assert context size
 0.17     03.11.17  we          Use THashContext and internal blake2b_ctx
 0.18     04.11.17  we          Dummy functions for TP5-7, D1-D3, IV with longints
 0.19     05.11.17  we          Some common code for 64 and 16/32 bit
 0.20     05.11.17  we          32-Bit BASM
 0.21     05.11.17  we          16-Bit BASM
 0.22     21.11.14  eh          Faster blake2b_compress routines from EddyHawk
 0.23     23.11.14  we          Replace RotR(,16) with word moves for non-int64
 0.24     24.11.14  we          RotR63 with BASM
 0.25     24.11.14  we          RotR24 with byte move
 0.26     24.11.14  we          RotR63, Add64 for VER5X

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2017 Wolfgang Ehrhardt

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
  BTypes, Hash;

const
  BLAKE2B_BlockLen  = 128;
  BLAKE2B_MaxDigLen = 64;
  BLAKE2B_MaxKeyLen = 64;

type
  TBlake2BDigest = packed array[0..BLAKE2B_MaxDigLen-1] of byte;  {max. blake2b digest}
  TBlake2BBlock  = packed array[0..BLAKE2B_BlockLen-1]  of byte;


function  blake2b_Init(var ctx: THashContext; key: pointer; keylen, diglen: word): integer;
  {-Initialize context for a digest of diglen bytes; keylen=0: no key}

procedure blake2b_update(var ctx: THashContext; msg: pointer; mlen: longint);
  {-Add "mlen" bytes from "msg" into the hash}

procedure blake2b_Final(var ctx: THashContext; var Digest: TBlake2BDigest);
  {-Finalize calculation, generate message digest, clear context}

function  blake2b_full(var dig: TBlake2BDigest; diglen: word;
                           key: pointer; keylen: word;
                           msg: pointer; mlen: longint): integer;
  {-Calculate hash digest of Msg with init/update/final}

function  blake2b_selftest: boolean;
  {-Return true, if self test is OK}


implementation


{The next comment is copy from blake2b-ref.c}

(*
   BLAKE2 reference source code package - reference C implementations

   Copyright 2012, Samuel Neves <sneves@dei.uc.pt>.  You may use this under the
   terms of the CC0, the OpenSSL Licence, or the Apache Public License 2.0, at
   your option.  The terms of these licenses can be found at:

   - CC0 1.0 Universal : http://creativecommons.org/publicdomain/zero/1.0
   - OpenSSL license   : https://www.openssl.org/source/license.html
   - Apache 2.0        : http://www.apache.org/licenses/LICENSE-2.0

   More information about the BLAKE2 hash function can be found at
   https://blake2.net.
*)

{Initialization Vector, longints for compatibility}
const
  blake2b_ivl: array[0..15] of longint = (
                 longint($F3BCC908), longint($6A09E667),
                 longint($84CAA73B), longint($BB67AE85),
                 longint($FE94F82B), longint($3C6EF372),
                 longint($5F1D36F1), longint($A54FF53A),
                 longint($ADE682D1), longint($510E527F),
                 longint($2B3E6C1F), longint($9B05688C),
                 longint($FB41BD6B), longint($1F83D9AB),
                 longint($137E2179), longint($5BE0CD19));

const
  sigma: array[0..11,0..15] of byte = (
           (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
           ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
           ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
           (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
           (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
           (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
           ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
           ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
           (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
           ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ),
           (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
           ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 )
         );


{$ifdef HAS_INT64}

type
  blake2b_ctx = packed record
                  h: packed array[0..7] of int64;
                  t: packed array[0..1] of int64;
                  b: TBlake2BBlock;
                  c: longint;
                  outlen: longint;
                  fill4: packed array[217..HASHCTXSIZE] of byte;
                end;

{---------------------------------------------------------------------------}
procedure blake2b_compress(var ctx: blake2b_ctx; last: boolean);
  {- Compression function, "last" indicates last block}
var
  v,m: array[0..15] of int64;
  tem: int64;
  round,k: integer;
begin
  with ctx do begin
    {init work variables}
    move(h, v, sizeof(h));
    move(blake2b_ivl, v[8], sizeof(h));
    v[12] := v[12] xor t[0];           {low 64 bits of offset}
    v[13] := v[13] xor t[1];           {high 64 bits}
    if last then v[14] := not v[14];   {last block flag set}

    {get little-endian words}
    move(b, m, sizeof(m));

    {do 12 rounds}
    for round:=0 to 11 do begin
      {** EddyHawk speed-ups **}
      {use same rearrangements as blake2s' 32/64 bit code}
      v[ 0] := (v[ 0] + v[ 4]) + m[sigma[round][ 0]];
      v[ 1] := (v[ 1] + v[ 5]) + m[sigma[round][ 2]];
      v[ 2] := (v[ 2] + v[ 6]) + m[sigma[round][ 4]];
      v[ 3] := (v[ 3] + v[ 7]) + m[sigma[round][ 6]];

      tem   := v[12] xor v[ 0];
      v[12] := (tem shr 32) or (tem shl (64-32));
      tem   := v[13] xor v[ 1];
      v[13] := (tem shr 32) or (tem shl (64-32));
      tem   := v[14] xor v[ 2];
      v[14] := (tem shr 32) or (tem shl (64-32));
      tem   := v[15] xor v[ 3];
      v[15] := (tem shr 32) or (tem shl (64-32));

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      tem   := v[ 4] xor v[ 8];
      v[ 4] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 5] xor v[ 9];
      v[ 5] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 6] xor v[10];
      v[ 6] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 7] xor v[11];
      v[ 7] := (tem shr 24) or (tem shl (64-24));

      {---}

      v[ 0] := (v[ 0] + v[ 4]) + m[sigma[round][ 1]];
      v[ 1] := (v[ 1] + v[ 5]) + m[sigma[round][ 3]];
      v[ 2] := (v[ 2] + v[ 6]) + m[sigma[round][ 5]];
      v[ 3] := (v[ 3] + v[ 7]) + m[sigma[round][ 7]];

      tem   := v[12] xor v[ 0];
      v[12] := (tem shr 16) or (tem shl (64-16));
      tem   := v[13] xor v[ 1];
      v[13] := (tem shr 16) or (tem shl (64-16));
      tem   := v[14] xor v[ 2];
      v[14] := (tem shr 16) or (tem shl (64-16));
      tem   := v[15] xor v[ 3];
      v[15] := (tem shr 16) or (tem shl (64-16));

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      tem   := v[ 4] xor v[ 8];
      v[ 4] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 5] xor v[ 9];
      v[ 5] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 6] xor v[10];
      v[ 6] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 7] xor v[11];
      v[ 7] := (tem shr 63) or (tem shl (64-63));

      {---}

      v[ 0] := (v[ 0] + v[ 5]) + m[sigma[round][ 8]];
      v[ 1] := (v[ 1] + v[ 6]) + m[sigma[round][10]];
      v[ 2] := (v[ 2] + v[ 7]) + m[sigma[round][12]];
      v[ 3] := (v[ 3] + v[ 4]) + m[sigma[round][14]];

      tem   := v[15] xor v[ 0];
      v[15] := (tem shr 32) or (tem shl (64-32));
      tem   := v[12] xor v[ 1];
      v[12] := (tem shr 32) or (tem shl (64-32));
      tem   := v[13] xor v[ 2];
      v[13] := (tem shr 32) or (tem shl (64-32));
      tem   := v[14] xor v[ 3];
      v[14] := (tem shr 32) or (tem shl (64-32));

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      tem   := v[ 5] xor v[10];
      v[ 5] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 6] xor v[11];
      v[ 6] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 7] xor v[ 8];
      v[ 7] := (tem shr 24) or (tem shl (64-24));
      tem   := v[ 4] xor v[ 9];
      v[ 4] := (tem shr 24) or (tem shl (64-24));

      {---}

      v[ 0] := (v[ 0] + v[ 5]) + m[sigma[round][ 9]];
      v[ 1] := (v[ 1] + v[ 6]) + m[sigma[round][11]];
      v[ 2] := (v[ 2] + v[ 7]) + m[sigma[round][13]];
      v[ 3] := (v[ 3] + v[ 4]) + m[sigma[round][15]];

      tem   := v[15] xor v[ 0];
      v[15] := (tem shr 16) or (tem shl (64-16));
      tem   := v[12] xor v[ 1];
      v[12] := (tem shr 16) or (tem shl (64-16));
      tem   := v[13] xor v[ 2];
      v[13] := (tem shr 16) or (tem shl (64-16));
      tem   := v[14] xor v[ 3];
      v[14] := (tem shr 16) or (tem shl (64-16));

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      tem   := v[ 5] xor v[10];
      v[ 5] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 6] xor v[11];
      v[ 6] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 7] xor v[ 8];
      v[ 7] := (tem shr 63) or (tem shl (64-63));
      tem   := v[ 4] xor v[ 9];
      v[ 4] := (tem shr 63) or (tem shl (64-63));
    end;

    {finalization}
    for k:=0 to 7 do begin
      h[k] := h[k] xor v[k] xor v[k+8];
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2b_update(var ctx: THashContext; msg: pointer; mlen: longint);
  {-Add "mlen" bytes from "msg" into the hash}
var
  left,fill: integer;
begin
  with blake2b_ctx(ctx) do begin
    if mlen > 0 then begin
      left := c;
      fill := BLAKE2B_BlockLen - left;
      if mlen > fill then begin
        c := 0;
        if fill>0 then move(msg^, b[left], fill);
        t[0] := t[0] + BLAKE2B_BlockLen;
        blake2b_compress(blake2b_ctx(ctx), false);
        inc(Ptr2Inc(Msg),fill);
        dec(mlen,fill);
        while mlen > BLAKE2B_BlockLen do begin
          move(msg^,b,BLAKE2B_BlockLen);
          t[0] := t[0] + BLAKE2B_BlockLen;
          blake2b_compress(blake2b_ctx(ctx), false);  {compress (not last)}
          inc(Ptr2Inc(Msg),BLAKE2B_BlockLen);
          dec(mlen,BLAKE2B_BlockLen);
        end;
      end;
      if mlen > 0 then begin
        move(msg^, b[c], mlen);
        c := c + mlen;
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2b_Final(var ctx: THashContext; var Digest: TBlake2BDigest);
  {-Finalize calculation, generate message digest, clear context}
var
  i: integer;
begin
  with blake2b_ctx(ctx) do begin
    t[0] := t[0] + c;
    while c < BLAKE2B_BlockLen do begin   {fill up with zeros}
      b[c] := 0;
      inc(c);
    end;
    blake2b_compress(blake2b_ctx(ctx), true);          {final block}
    {little endian convert and store}
    fillchar(Digest, sizeof(Digest),0);
    for i:=0 to outlen-1 do begin
      Digest[i] := (h[i shr 3] shr (8*(i and 7))) and $FF;
    end;
  end;
end;


{$else}

type
  blake2b_ctx = packed record
                  h: packed array[0..15] of longint;
                  t: packed array[0..3] of longint;
                  b: TBlake2BBlock;
                  c: longint;
                  outlen: longint;
                  fill4: packed array[217..HASHCTXSIZE] of byte;
                end;

type
  TW64    = packed record
              L,H: longint;
            end;
  TW16    = packed record
              w0,w1,w2,w3: word
            end;

{$ifdef BIT16}
{$ifdef BASM}

{---------------------------------------------------------------------------}
procedure Add64(var z: TW64; {$ifdef CONST} const {$else} var {$endif} x: TW64); assembler;
  {-Inc a 64 bit integer}
asm
          les  bx,[x]
  db $66; mov  ax,es:[bx]
  db $66; mov  dx,es:[bx+4]
          les  bx,[z]
  db $66; add  es:[bx],ax
  db $66; adc  es:[bx+4],dx
end;


{---------------------------------------------------------------------------}
procedure RotR63(var x: tw64);
  {-Rotate right 63 bits = rotate left 1}
begin
  asm
           les   bx,[x]
  db $66;  mov   ax,es:[bx]
  db $66;  mov   dx,es:[bx+4]
  db $66;  shl   ax,1
  db $66;  rcl   dx,1
           adc   ax,0
  db $66;  mov   es:[bx],ax
  db $66;  mov   es:[bx+4],dx
  end;
end;


{$else}

{16-bit compilers without BASM}

{---------------------------------------------------------------------------}
procedure Add64(var Z: TW64; var X: TW64);
  {-Inc a 64 bit integer}
inline(
  $8C/$DF/      {mov   di,ds     }
  $5E/          {pop   si        }
  $1F/          {pop   ds        }
  $8B/$04/      {mov   ax,[si]   }
  $8B/$5C/$02/  {mov   bx,[si+02]}
  $8B/$4C/$04/  {mov   cx,[si+04]}
  $8B/$54/$06/  {mov   dx,[si+06]}
  $5E/          {pop   si        }
  $1F/          {pop   ds        }
  $01/$04/      {add   [si],ax   }
  $11/$5C/$02/  {adc   [si+02],bx}
  $11/$4C/$04/  {adc   [si+04],cx}
  $11/$54/$06/  {adc   [si+06],dx}
  $8E/$DF);     {mov   ds,di     }


{---------------------------------------------------------------------------}
procedure RotR63(var x: tw64);
  {-Rotate right 63 bits = rotate left 1}
inline(
  $8C/$D9/      {mov   cx,ds     }
  $5B/          {pop   bx        }
  $1F/          {pop   ds        }
  $8B/$47/$06/  {mov   ax,[bx+06]}
  $D1/$E0/      {shl   ax,1      }
  $8B/$07/      {mov   ax,[bx]   }
  $D1/$D0/      {rcl   ax,1      }
  $89/$07/      {mov   [bx],ax   }
  $8B/$47/$02/  {mov   ax,[bx+02]}
  $D1/$D0/      {rcl   ax,1      }
  $89/$47/$02/  {mov   [bx+02],ax}
  $8B/$47/$04/  {mov   ax,[bx+04]}
  $D1/$D0/      {rcl   ax,1      }
  $89/$47/$04/  {mov   [bx+04],ax}
  $8B/$47/$06/  {mov   ax,[bx+06]}
  $D1/$D0/      {rcl   ax,1      }
  $89/$47/$06/  {mov   [bx+06],ax}
  $8E/$D9);     {mov   ds,cx     }
{$endif}

{$else}

{---------------------------------------------------------------------------}
procedure Add64(var z: TW64; const x: TW64); assembler;
  {-Inc a 64 bit integer z := z + x}
asm
  {$ifdef LoadArgs}
    mov eax,[z]
    mov edx,[x]
  {$endif}
  mov  ecx,[edx]
  add  [eax],ecx
  mov  ecx,[edx+4]
  adc  [eax+4],ecx
end;


{---------------------------------------------------------------------------}
procedure RotR63(var x: tw64); assembler;
  {-Rotate right 63 bits}
asm
  {$ifdef LoadArgs}
    mov eax,[x]
  {$endif}
    mov   ecx,[eax]
    mov   edx,[eax+4]
    shl   ecx,1
    rcl   edx,1
    adc   ecx,0
    mov   [eax],ecx
    mov   [eax+4],edx
end;

{$endif}


{---------------------------------------------------------------------------}
procedure RotR24(var x: tw64);
  {-Rotate right 24 bits}
var
  a: packed array[0..7] of byte absolute x;
  b0,b1,b2: byte;
begin
  b0 := a[0];
  b1 := a[1];
  b2 := a[2];
  a[0] := a[3];
  a[1] := a[4];
  a[2] := a[5];
  a[3] := a[6];
  a[4] := a[7];
  a[5] := b0;
  a[6] := b1;
  a[7] := b2;
end;


{---------------------------------------------------------------------------}
procedure inct01(var ctx: THashContext; cnt: longint);
  {-Increment byte counter in ctx}
begin
  with blake2b_ctx(ctx) do begin
    if t[0] < 0 then begin
      {Overflow if t[0]+cnt changes sign}
      inc(t[0], cnt);
      if t[0] >= 0 then inc(t[1]);
    end
    else inc(t[0], cnt);
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2b_compress(var ctx: blake2b_ctx; last: boolean);
  {-Compression function, "last" indicates last block}
var
  v,m: array[0..15] of tw64;
  tem: longint;
  round,k: integer;
  tw: word;
begin
  {get message}
  with ctx do begin
    {init work variables}
    move(h, v, sizeof(h));
    move(blake2b_ivl, v[8], sizeof(h));

    v[12].l := v[12].l xor t[0];       {low 64 bits of offset}
    v[12].h := v[12].h xor t[1];
    v[13].l := v[13].l xor t[2];       {high 64 bits}
    v[13].h := v[13].h xor t[3];
    if last then begin
      v[14].l := not v[14].l;   {last block flag set}
      v[14].h := not v[14].h;
    end;

    {get little-endian words}
    move(b, m, sizeof(m));

    {do 12 rounds}
    for round:=0 to 11 do begin
      {** EddyHawk speed-ups **}
      {replaces G64 with partial unroll}
      {replaces rotr64 by 32 with swap & temp var}
      { integrates xor-ing into that swapping}
      {splits 1 BLAKE2b round into quarter-rounds}
      { regroups them}
      {further splitting/regroupings, seems a bit better}
      {moves message addition to the front, seems a bit better}

      add64(v[0],m[sigma[round][2*0]]);
      add64(v[1],m[sigma[round][2*1]]);
      add64(v[2],m[sigma[round][2*2]]);
      add64(v[3],m[sigma[round][2*3]]);
      add64(v[0],v[4]);
      add64(v[1],v[5]);
      add64(v[2],v[6]);
      add64(v[3],v[7]);

      tem := v[12].L xor v[0].L;
      v[12].L := v[12].H xor v[0].H;
      v[12].H := tem;
      tem := v[13].L xor v[1].L;
      v[13].L := v[13].H xor v[1].H;
      v[13].H := tem;
      tem := v[14].L xor v[2].L;
      v[14].L := v[14].H xor v[2].H;
      v[14].H := tem;
      tem := v[15].L xor v[3].L;
      v[15].L := v[15].H xor v[3].H;
      v[15].H := tem;

      add64(v[ 8],v[12]);
      add64(v[ 9],v[13]);
      add64(v[10],v[14]);
      add64(v[11],v[15]);

      v[4].L := v[4].L xor v[ 8].L;
      v[5].L := v[5].L xor v[ 9].L;
      v[6].L := v[6].L xor v[10].L;
      v[7].L := v[7].L xor v[11].L;
      v[4].H := v[4].H xor v[ 8].H;
      v[5].H := v[5].H xor v[ 9].H;
      v[6].H := v[6].H xor v[10].H;
      v[7].H := v[7].H xor v[11].H;

      RotR24(v[4]);
      RotR24(v[5]);
      RotR24(v[6]);
      RotR24(v[7]);

      {---}

      add64(v[0],m[sigma[round][2*0+1]]);
      add64(v[1],m[sigma[round][2*1+1]]);
      add64(v[2],m[sigma[round][2*2+1]]);
      add64(v[3],m[sigma[round][2*3+1]]);
      add64(v[0],v[4]);
      add64(v[1],v[5]);
      add64(v[2],v[6]);
      add64(v[3],v[7]);

      v[12].L := v[12].L xor v[ 0].L;
      v[13].L := v[13].L xor v[ 1].L;
      v[14].L := v[14].L xor v[ 2].L;
      v[15].L := v[15].L xor v[ 3].L;
      v[12].H := v[12].H xor v[ 0].H;
      v[13].H := v[13].H xor v[ 1].H;
      v[14].H := v[14].H xor v[ 2].H;
      v[15].H := v[15].H xor v[ 3].H;

      {WE V0.23: Replace RotR(,16) with word moves}
      {RotR(v[12],16);}
      with TW16(v[12]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[13],16);}
      with TW16(v[13]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[14],16);}
      with TW16(v[14]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[15],16);}
      with TW16(v[15]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;

      add64(v[ 8],v[12]);
      add64(v[ 9],v[13]);
      add64(v[10],v[14]);
      add64(v[11],v[15]);

      v[4].L := v[4].L xor v[ 8].L;
      v[5].L := v[5].L xor v[ 9].L;
      v[6].L := v[6].L xor v[10].L;
      v[7].L := v[7].L xor v[11].L;
      v[4].H := v[4].H xor v[ 8].H;
      v[5].H := v[5].H xor v[ 9].H;
      v[6].H := v[6].H xor v[10].H;
      v[7].H := v[7].H xor v[11].H;

      RotR63(v[4]);
      RotR63(v[5]);
      RotR63(v[6]);
      RotR63(v[7]);
      {---}

      add64(v[0],m[sigma[round][2*4]]);
      add64(v[1],m[sigma[round][2*5]]);
      add64(v[2],m[sigma[round][2*6]]);
      add64(v[3],m[sigma[round][2*7]]);
      add64(v[0],v[5]);
      add64(v[1],v[6]);
      add64(v[2],v[7]);
      add64(v[3],v[4]);

      tem := v[15].L xor v[0].L;
      v[15].L := v[15].H xor v[0].H;
      v[15].H := tem;
      tem := v[12].L xor v[1].L;
      v[12].L := v[12].H xor v[1].H;
      v[12].H := tem;
      tem := v[13].L xor v[2].L;
      v[13].L := v[13].H xor v[2].H;
      v[13].H := tem;
      tem := v[14].L xor v[3].L;
      v[14].L := v[14].H xor v[3].H;
      v[14].H := tem;

      add64(v[10],v[15]);
      add64(v[11],v[12]);
      add64(v[ 8],v[13]);
      add64(v[ 9],v[14]);

      v[5].L := v[5].L xor v[10].L;
      v[6].L := v[6].L xor v[11].L;
      v[7].L := v[7].L xor v[ 8].L;
      v[4].L := v[4].L xor v[ 9].L;
      v[5].H := v[5].H xor v[10].H;
      v[6].H := v[6].H xor v[11].H;
      v[7].H := v[7].H xor v[ 8].H;
      v[4].H := v[4].H xor v[ 9].H;

      RotR24(v[5]);
      RotR24(v[6]);
      RotR24(v[7]);
      RotR24(v[4]);


      add64(v[0],m[sigma[round][2*4+1]]);
      add64(v[1],m[sigma[round][2*5+1]]);
      add64(v[2],m[sigma[round][2*6+1]]);
      add64(v[3],m[sigma[round][2*7+1]]);
      add64(v[0],v[5]);
      add64(v[1],v[6]);
      add64(v[2],v[7]);
      add64(v[3],v[4]);

      v[15].L := v[15].L xor v[ 0].L;
      v[12].L := v[12].L xor v[ 1].L;
      v[13].L := v[13].L xor v[ 2].L;
      v[14].L := v[14].L xor v[ 3].L;
      v[15].H := v[15].H xor v[ 0].H;
      v[12].H := v[12].H xor v[ 1].H;
      v[13].H := v[13].H xor v[ 2].H;
      v[14].H := v[14].H xor v[ 3].H;

      {WE V0.23: Replace RotR(,16) with word moves}
      {RotR(v[15],16);}
      with TW16(v[15]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[12],16);}
      with TW16(v[12]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[13],16);}
      with TW16(v[13]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;
      {RotR(v[14],16);}
      with TW16(v[14]) do begin
        tw := w0;
        w0 := w1;
        w1 := w2;
        w2 := w3;
        w3 := tw;
      end;

      add64(v[10],v[15]);
      add64(v[11],v[12]);
      add64(v[ 8],v[13]);
      add64(v[ 9],v[14]);

      v[5].L := v[5].L xor v[10].L;
      v[6].L := v[6].L xor v[11].L;
      v[7].L := v[7].L xor v[ 8].L;
      v[4].L := v[4].L xor v[ 9].L;
      v[5].H := v[5].H xor v[10].H;
      v[6].H := v[6].H xor v[11].H;
      v[7].H := v[7].H xor v[ 8].H;
      v[4].H := v[4].H xor v[ 9].H;

      RotR63(v[5]);
      RotR63(v[6]);
      RotR63(v[7]);
      RotR63(v[4]);
    end;

    {finalization}
    for k:=0 to 7 do begin
      h[2*k]   := h[2*k]   xor v[k].l xor v[k+8].l;
      h[2*k+1] := h[2*k+1] xor v[k].h xor v[k+8].h;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2b_update(var ctx: THashContext; msg: pointer; mlen: longint);
  {-Add "mlen" bytes from "msg" into the hash}
var
  left,fill: integer;
begin
  with blake2b_ctx(ctx) do begin
    if mlen > 0 then begin
      left := c;
      fill := BLAKE2B_BlockLen - left;
      if mlen > fill then begin
        c := 0;
        if fill>0 then move(msg^, b[left], fill);
        inct01(ctx, BLAKE2B_BlockLen);
        blake2b_compress(blake2b_ctx(ctx), false);
        inc(Ptr2Inc(Msg),fill);
        dec(mlen,fill);
        while mlen > BLAKE2B_BlockLen do begin
          move(msg^,b,BLAKE2B_BlockLen);
          inct01(ctx, BLAKE2B_BlockLen);
          blake2b_compress(blake2b_ctx(ctx), false);  {compress (not last)}
          inc(Ptr2Inc(Msg),BLAKE2B_BlockLen);
          dec(mlen,BLAKE2B_BlockLen);
        end;
      end;
      if mlen > 0 then begin
        move(msg^, b[c], mlen);
        c := c + mlen;
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2b_Final(var ctx: THashContext; var Digest: TBlake2BDigest);
  {-Finalize calculation, generate message digest, clear context}
begin
  with blake2b_ctx(ctx) do begin
    inct01(ctx, c);
    while c < BLAKE2B_BlockLen do begin   {fill up with zeros}
      b[c] := 0;
      inc(c);
    end;
    blake2b_compress(blake2b_ctx(ctx), true);  {final block}
    {little endian convert and store}
    fillchar(Digest, sizeof(Digest),0);
    move(h, Digest, outlen);
  end;
end;
{$endif}


{---------------------------------------------------------------------------}
function  blake2b_Init(var ctx: THashContext; key: pointer; keylen, diglen: word): integer;
  {-Initialize context for a digest of diglen bytes; keylen=0: no key}
var
  tb: TBlake2BBlock;
begin
  if (diglen=0) or (diglen > BLAKE2B_MaxDigLen) or (keylen > BLAKE2B_MaxKeyLen) then begin
    blake2b_Init := -1;  {illegal parameters}
    exit;
  end;
  blake2b_Init := 0;
  fillchar(ctx, sizeof(ctx), 0);
  with blake2b_ctx(ctx) do begin
    move(blake2b_ivl, h, sizeof(h));
    outlen := diglen;
    {Fill the lowest 32 bit of h, same for 16/32/64 bit}
    h[0] := h[0] xor (($01010000) xor (keylen shl 8) xor outlen);
    if keylen > 0 then begin
      fillchar(tb, sizeof(tb),0);
      move(key^, tb, keylen);
      blake2b_update(ctx, @tb, BLAKE2B_BlockLen);
    end;
  end;
end;


{---------------------------------------------------------------------------}
function blake2b_full(var dig: TBlake2BDigest; diglen: word;
                          key: pointer; keylen: word;
                          msg: pointer; mlen: longint): integer;
  {-Calculate hash digest of Msg with init/update/final}
var
  ctx: THashContext;
begin
  if blake2b_init(ctx, key, keylen, diglen) <> 0 then begin
    blake2b_full := -1;
  end
  else begin
    blake2b_update(ctx, msg, mlen);
    blake2b_final(ctx, dig);
    blake2b_full := 0;
  end;
end;


{---------------------------------------------------------------------------}
function blake2b_selftest: boolean;
  {-Return true, if self test is OK}
  procedure selftest_seq(outp: pbyte;  len, seed: integer);
  var
    t,a,b: longint;
    i: integer;
  begin
    a := longint($DEAD4BAD) * seed;
    b := 1;
    for i:=1 to len do begin
      t := a+b;
      a := b;
      b := t;
      outp^ := (t shr 24) and $FF;
      inc(Ptr2Inc(outp));
    end;
  end;
const
  {Grand hash of hash results}
  blake2b_res: array[0..31] of byte = (
                 $C2, $3A, $78, $00, $D9, $81, $23, $BD,
                 $10, $F5, $06, $C6, $1E, $29, $DA, $56,
                 $03, $D7, $63, $B8, $BB, $AD, $2E, $73,
                 $7F, $5E, $76, $5A, $7B, $CC, $D4, $75
               );
  {Parameter sets}
  b2s_md_len: array[0..3] of integer = (20, 32, 48, 64);
  b2s_in_len: array[0..5] of integer = (0,  3,  128, 129, 255, 1024);
var
  i,j, outlen, inlen: integer;
  md, key: TBlake2BDigest;
  ctx: THashContext;
  inb: array[0..1023] of byte;
begin
  blake2b_selftest := false;
  {256-bit hash for testing}
  if blake2b_init(ctx, nil, 0, 32) <> 0 then exit;
  for i:=0 to 3 do begin
    outlen := b2s_md_len[i];
    for j:=0 to 5 do begin
      inlen := b2s_in_len[j];
      selftest_seq(pbyte(@inb), inlen, inlen);      {unkeyed hash}
      if blake2b_full(md, outlen, nil, 0, @inb, inlen) <> 0 then exit;
      blake2b_update(ctx, @md, outlen);             {hash the hash}
      selftest_seq(pbyte(@key), outlen, outlen);    {keyed hash}
      if blake2b_full(md, outlen, @key, outlen, @inb, inlen) <> 0 then exit;
      blake2b_update(ctx, @md, outlen);             {hash the hash}
    end;
  end;
  {Compute and compare the hash of hashes.}
  blake2b_final(ctx, md);
  for i:=0 to 31 do begin
    if md[i] <> blake2b_res[i] then exit;
  end;
  blake2b_selftest := true;
end;


begin
  {$ifdef HAS_ASSERT}
    assert(sizeof(blake2b_ctx)=HASHCTXSIZE , '** Invalid sizeof(blake2b_ctx)');
  {$endif}
end.

