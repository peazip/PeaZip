unit blake2s;

{Blake2S - max 256 bit hash/MAC function}

interface

(*************************************************************************

 DESCRIPTION     :  Blake2S - max 256 bit hash/MAC function

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
 0.10     28.04.17  W.Ehrhardt  Initial BP7 implementation (from RFC code)
 0.11     29.04.17  we          Debug
 0.12     29.04.17  we          updatelen
 0.13     29.04.17  we          Fix: PurePascal if BIT64, adjust same names/comments
 0.14     30.04.17  we          inline G32 for FPC2+
 0.15     30.04.17  we          handcoded inline for G32
 0.16     02.05.17  we          rewrite update and init (from reference code)
 0.17     02.05.17  we          different compress code for FPC2+ and rest
 0.18     02.05.17  we          blake2s_full
 0.19     02.05.17  we          blake2s_selftest; fix bug in blake2s_full
 0.20     15.05.17  we          References, fill4 for HashContext
 0.21     15.05.17  we          Test fill, mlen > 0 in blake2s_update
 0.22     16.05.17  we          Use more named constants
 0.23     14.11.14  we          Faster 32-bit compression routines from EddyHawk
 0.24     25.11.14  we          Bit16: Reorder 16-bit round loop
 0.25     25.11.14  we          Bit16: RotR(,8) and RotR(,16) inline
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
  BLAKE2S_BlockLen  = 64;
  BLAKE2S_MaxDigLen = 32;
  BLAKE2S_MaxKeyLen = 32;

type
  TBlake2SDigest = packed array[0..BLAKE2S_MaxDigLen-1] of byte;  {max. blake2s digest}
  TBlake2SBlock  = packed array[0..BLAKE2S_BlockLen-1] of byte;

  blake2s_ctx = packed record
                  h: packed array[0..7] of longint;
                  t: packed array[0..1] of longint;
                  b: TBlake2SBlock;
                  c: longint;
                  outlen: longint;
                  fill4: packed array[113..HASHCTXSIZE] of byte;
                end;


function  blake2s_Init(var ctx: blake2s_ctx; key: pointer; keylen, diglen: word): integer;
  {-Initialize context for a digest of diglen bytes; keylen=0: no key}

procedure blake2s_update(var ctx: blake2s_ctx; msg: pointer; mlen: longint);
  {-Add "mlen" bytes from "msg" into the hash}

procedure blake2s_Final(var ctx: blake2s_ctx; var Digest: TBlake2SDigest);
  {-Finalize calculation, generate message digest, clear context}

function  blake2s_full(var dig: TBlake2SDigest; diglen: word;
                           key: pointer; keylen: word;
                           msg: pointer; mlen: longint): integer;
  {-Calculate hash digest of Msg with init/update/final}

function  blake2s_selftest: boolean;
  {-Return true, if self test is OK}


implementation

{The next comment is copy from blake2s-ref.c}

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


{$ifdef BIT16}

{---------------------------------------------------------------------------}
function RotR16(X: longint): longint;
  {-Rotate right 16 bits}
inline(
  $5A/           { pop  dx }
  $58);          { pop  ax }

{---------------------------------------------------------------------------}
function RotR8(X: longint): longint;
  {-Rotate right 8 bits}
inline(
  $58/           { pop   ax    }
  $5A/           { pop   dx    }
  $86/$C4/       { xchg  al,ah }
  $86/$E2/       { xchg  ah,dl }
  $86/$D6);      { xchg  dl,dh }

{$ifdef BASM16}

(** TP6-7/D1 **)
{---------------------------------------------------------------------------}
  function RotR(X: longint; c: word): longint;
  inline(
    $59/              {pop    cx     }
    $66/$58/          {pop    eax    }
    $66/$D3/$C8/      {ror    eax,cl }
    $66/$8B/$D0/      {mov    edx,eax}
    $66/$C1/$EA/$10); {shr    edx,16 }

{---------------------------------------------------------------------------}
  procedure UpdateLen(var whi, wlo: longint; BLen: longint); assembler;
    {-Add BLen to 64 bit value (wlo, whi)}
  asm
            les   di,[wlo]
    db $66; mov   ax,word ptr [BLen]
    db $66; sub   dx,dx
    db $66; add   es:[di],ax
            les   di,[whi]
    db $66; adc   es:[di],dx
  end;

{$else}

{** T5/5.5 **}
{---------------------------------------------------------------------------}
  function RotR(X: longint; c: word): longint;
    {-Rotate right}
  inline(
    $59/           {  pop    cx   }
    $58/           {  pop    ax   }
    $5A/           {  pop    dx   }

    $83/$F9/$10/   {  cmp    cx,16}
    $72/$06/       {  jb     S    }
    $92/           {  xchg   dx,ax}
    $83/$E9/$10/   {  sub    cx,16}
    $74/$09/       {  je     X    }

    $8B/$DA/       {  mov   bx,dx }
    $D1/$EB/       {L:shr   bx,1  }
    $D1/$D8/       {  rcr   ax,1  }
    $D1/$DA/       {  rcr   dx,1  }
    $49/           {  dec   cx    }
    $75/$F7);      {  jne   L     }
                   {X:            }

  {---------------------------------------------------------------------------}
  procedure UpdateLen(var whi, wlo: longint; BLen: longint);
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

{$endif BASM16}

{$else}

  {32/64 bit}
  {---------------------------------------------------------------------------}

  {$ifdef BIT64}
    {$define PurePascal}
  {$endif}

  {$ifdef PurePascal}
    {---------------------------------------------------------------------------}
    procedure UpdateLen(var whi, wlo: longint; BLen: longint);
      {-Add BLen to 64 bit value (wlo, whi)}
    var
      tmp: int64;
    begin
      tmp := int64(cardinal(wlo))+Blen;
      wlo := longint(tmp and $FFFFFFFF);
      inc(whi,longint(tmp shr 32));
    end;
  {$else}
    {---------------------------------------------------------------------------}
    procedure UpdateLen(var whi, wlo: longint; BLen: longint);
      {-Add BLen to 64 bit value (wlo, whi)}
    begin
      asm
        mov  edx, [wlo]
        mov  ecx, [whi]
        mov  eax, [Blen]
        add  [edx], eax
        adc  dword ptr [ecx], 0
      end;
    end;
  {$endif PurePascal}

{$endif BIT16}

const
  sigma: array[0..9,0..15] of byte = (
           (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
           ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
           ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
           (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
           (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
           (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
           ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
           ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
           (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
           ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 )
         );


{Initialization Vector}
const
  blake2s_iv: array[0..7] of longint = (
                longint($6A09E667), longint($BB67AE85), longint($3C6EF372), longint($A54FF53A),
                longint($510E527F), longint($9B05688C), longint($1F83D9AB), longint($5BE0CD19)
              );

{$ifdef BIT16}
{---------------------------------------------------------------------------}
procedure blake2s_compress(var ctx: blake2s_ctx; last: boolean);
  {- Compression function, "last" indicates last block}
var
  v,m: array[0..15] of longint;
  round,k: integer;
begin
  with ctx do begin
    {init work variables}
    for k:=0 to 7 do begin
      v[k] := h[k];
      v[k+8] := blake2s_iv[k];
    end;
    v[12] := v[12] xor t[0];           {low 32 bits of offset}
    v[13] := v[13] xor t[1];           {high 32 bits}
    if last then v[14] := not v[14];   {last block flag set}

    {get little-endian words}
    move(b, m, sizeof(m));

    for round:=0 to 9 do begin
      v[0]  := (v[0] + v[4]) + m[sigma[round][0]];
      v[1]  := (v[1] + v[5]) + m[sigma[round][2]];
      v[2]  := (v[2] + v[6]) + m[sigma[round][4]];
      v[3]  := (v[3] + v[7]) + m[sigma[round][6]];

      v[12] := RotR16(v[12] xor v[0]);
      v[13] := RotR16(v[13] xor v[1]);
      v[14] := RotR16(v[14] xor v[2]);
      v[15] := RotR16(v[15] xor v[3]);

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      v[4]  := RotR(v[4] xor v[ 8],12);
      v[5]  := RotR(v[5] xor v[ 9],12);
      v[6]  := RotR(v[6] xor v[10],12);
      v[7]  := RotR(v[7] xor v[11],12);

      v[0]  := (v[0] + v[4]) + m[sigma[round][1]];
      v[1]  := (v[1] + v[5]) + m[sigma[round][3]];
      v[2]  := (v[2] + v[6]) + m[sigma[round][5]];
      v[3]  := (v[3] + v[7]) + m[sigma[round][7]];

      v[12] := RotR8(v[12] xor v[0]);
      v[13] := RotR8(v[13] xor v[1]);
      v[14] := RotR8(v[14] xor v[2]);
      v[15] := RotR8(v[15] xor v[3]);

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      v[4]  := RotR(v[4] xor v[ 8], 7);
      v[5]  := RotR(v[5] xor v[ 9], 7);
      v[6]  := RotR(v[6] xor v[10], 7);
      v[7]  := RotR(v[7] xor v[11], 7);

      v[0]  := (v[0] + v[5]) + m[sigma[round][ 8]];
      v[1]  := (v[1] + v[6]) + m[sigma[round][10]];
      v[2]  := (v[2] + v[7]) + m[sigma[round][12]];
      v[3]  := (v[3] + v[4]) + m[sigma[round][14]];

      v[15] := RotR16(v[15] xor v[0]);
      v[12] := RotR16(v[12] xor v[1]);
      v[13] := RotR16(v[13] xor v[2]);
      v[14] := RotR16(v[14] xor v[3]);

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      v[5]  := RotR(v[5] xor v[10],12);
      v[6]  := RotR(v[6] xor v[11],12);
      v[7]  := RotR(v[7] xor v[ 8],12);
      v[4]  := RotR(v[4] xor v[ 9],12);

      v[0]  := (v[0] + v[5]) + m[sigma[round][ 9]];
      v[1]  := (v[1] + v[6]) + m[sigma[round][11]];
      v[2]  := (v[2] + v[7]) + m[sigma[round][13]];
      v[3]  := (v[3] + v[4]) + m[sigma[round][15]];

      v[15] := RotR8(v[15] xor v[0]);
      v[12] := RotR8(v[12] xor v[1]);
      v[13] := RotR8(v[13] xor v[2]);
      v[14] := RotR8(v[14] xor v[3]);

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      v[5]  := RotR(v[5] xor v[10], 7);
      v[6]  := RotR(v[6] xor v[11], 7);
      v[7]  := RotR(v[7] xor v[ 8], 7);
      v[4]  := RotR(v[4] xor v[ 9], 7);
    end;

    {finalization}
    for k:=0 to 7 do begin
      h[k] := h[k] xor v[k] xor v[k+8];
    end;
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure blake2s_compress(var ctx: blake2s_ctx; last: boolean);
  {-Compression function, "last" indicates last block}
var
  v,m: array[0..15] of longint;
  tem: longint;
  round,k: integer;
begin
  {Code contributed by EddyHawk}
  {----------------------------}
  with ctx do begin
    {init work variables}
    for k:=0 to 7 do begin
      v[k] := h[k];
      v[k+8] := blake2s_iv[k];
    end;
    v[12] := v[12] xor t[0];           {low 32 bits of offset}
    v[13] := v[13] xor t[1];           {high 32 bits}
    if last then v[14] := not v[14];   {last block flag set}

    {get little-endian words}
    move(b, m, sizeof(m));

    for round:=0 to 9 do begin
      {separates BLAKE2s round into quarter-rounds}
      {replaces the rotr with direct code}
      {uses tem var for xor-ed words}
      {moves message-additions to the front}
      {regroups the rest}

      v[ 0] := (v[ 0] + v[ 4]) + m[sigma[round][ 0]];
      v[ 1] := (v[ 1] + v[ 5]) + m[sigma[round][ 2]];
      v[ 2] := (v[ 2] + v[ 6]) + m[sigma[round][ 4]];
      v[ 3] := (v[ 3] + v[ 7]) + m[sigma[round][ 6]];

      tem   := v[12] xor v[ 0];
      v[12] := (tem shr 16) or (tem shl (32-16));
      tem   := v[13] xor v[ 1];
      v[13] := (tem shr 16) or (tem shl (32-16));
      tem   := v[14] xor v[ 2];
      v[14] := (tem shr 16) or (tem shl (32-16));
      tem   := v[15] xor v[ 3];
      v[15] := (tem shr 16) or (tem shl (32-16));

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      tem   := v[ 4] xor v[ 8];
      v[ 4] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 5] xor v[ 9];
      v[ 5] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 6] xor v[10];
      v[ 6] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 7] xor v[11];
      v[ 7] := (tem shr 12) or (tem shl (32-12));

      {2nd quarter-round}
      v[ 0]  := (v[ 0] + v[ 4]) + m[sigma[round][ 1]];
      v[ 1]  := (v[ 1] + v[ 5]) + m[sigma[round][ 3]];
      v[ 2]  := (v[ 2] + v[ 6]) + m[sigma[round][ 5]];
      v[ 3]  := (v[ 3] + v[ 7]) + m[sigma[round][ 7]];

      tem   := v[12] xor v[ 0];
      v[12] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[13] xor v[ 1];
      v[13] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[14] xor v[ 2];
      v[14] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[15] xor v[ 3];
      v[15] := (tem shr  8) or (tem shl (32- 8));

      v[ 8] := v[ 8] + v[12];
      v[ 9] := v[ 9] + v[13];
      v[10] := v[10] + v[14];
      v[11] := v[11] + v[15];

      tem   := v[ 4] xor v[ 8];
      v[ 4] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 5] xor v[ 9];
      v[ 5] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 6] xor v[10];
      v[ 6] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 7] xor v[11];
      v[ 7] := (tem shr  7) or (tem shl (32- 7));

      {3rd quarter-round}
      v[ 0] := (v[ 0] + v[ 5]) + m[sigma[round][ 8]];
      v[ 1] := (v[ 1] + v[ 6]) + m[sigma[round][10]];
      v[ 2] := (v[ 2] + v[ 7]) + m[sigma[round][12]];
      v[ 3] := (v[ 3] + v[ 4]) + m[sigma[round][14]];

      tem   := v[15] xor v[ 0];
      v[15] := (tem shr 16) or (tem shl (32-16));
      tem   := v[12] xor v[ 1];
      v[12] := (tem shr 16) or (tem shl (32-16));
      tem   := v[13] xor v[ 2];
      v[13] := (tem shr 16) or (tem shl (32-16));
      tem   := v[14] xor v[ 3];
      v[14] := (tem shr 16) or (tem shl (32-16));

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      tem   := v[ 5] xor v[10];
      v[ 5] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 6] xor v[11];
      v[ 6] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 7] xor v[ 8];
      v[ 7] := (tem shr 12) or (tem shl (32-12));
      tem   := v[ 4] xor v[ 9];
      v[ 4] := (tem shr 12) or (tem shl (32-12));

      {4th quarter-round}
      v[ 0] := (v[ 0] + v[ 5]) + m[sigma[round][ 9]];
      v[ 1] := (v[ 1] + v[ 6]) + m[sigma[round][11]];
      v[ 2] := (v[ 2] + v[ 7]) + m[sigma[round][13]];
      v[ 3] := (v[ 3] + v[ 4]) + m[sigma[round][15]];

      tem   := v[15] xor v[ 0];
      v[15] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[12] xor v[ 1];
      v[12] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[13] xor v[ 2];
      v[13] := (tem shr  8) or (tem shl (32- 8));
      tem   := v[14] xor v[ 3];
      v[14] := (tem shr  8) or (tem shl (32- 8));

      v[10] := v[10] + v[15];
      v[11] := v[11] + v[12];
      v[ 8] := v[ 8] + v[13];
      v[ 9] := v[ 9] + v[14];

      tem   := v[ 5] xor v[10];
      v[ 5] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 6] xor v[11];
      v[ 6] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 7] xor v[ 8];
      v[ 7] := (tem shr  7) or (tem shl (32- 7));
      tem   := v[ 4] xor v[ 9];
      v[ 4] := (tem shr  7) or (tem shl (32- 7));
    end;

    {finalization}
    for k:=0 to 7 do begin
      h[k] := h[k] xor v[k] xor v[k+8];
    end;
  end;
end;

{$endif}


{---------------------------------------------------------------------------}
function blake2s_Init(var ctx: blake2s_ctx; key: pointer; keylen, diglen: word): integer;
  {-Initialize context for a digest of diglen bytes; keylen=0: no key}
var
  tb: TBlake2SBlock;
begin
  if (diglen=0) or (diglen > BLAKE2S_MaxDigLen) or (keylen > BLAKE2S_MaxKeyLen) then begin
    blake2s_Init := -1;  {illegal parameters}
    exit;
  end;
  blake2s_Init := 0;
  with ctx do begin
    move(blake2s_iv, h, sizeof(h));
    outlen := diglen;
    h[0] := h[0] xor (($01010000) xor (keylen shl 8) xor outlen);
    t[0] := 0;                      {input count low word }
    t[1] := 0;                      {input count high word}
    c := 0;                         {pointer within buffer}
    if keylen > 0 then begin
      fillchar(tb, sizeof(tb),0);
      move(key^, tb, keylen);
      blake2s_update(ctx, @tb, BLAKE2S_BlockLen);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure blake2s_update(var ctx: blake2s_ctx; msg: pointer; mlen: longint);
  {-Add "mlen" bytes from "msg" into the hash}
var
  left,fill: integer;
begin
  with ctx do begin
    if mlen > 0 then begin
      left := c;
      fill := BLAKE2S_BlockLen - left;
      if mlen > fill then begin
        c := 0;
        if fill>0 then move(msg^, b[left], fill);
        UpdateLen(t[1], t[0], BLAKE2S_BlockLen);
        blake2s_compress(ctx, false);
        inc(Ptr2Inc(Msg),fill);
        dec(mlen,fill);
        while mlen > BLAKE2S_BlockLen do begin
          move(msg^,b,BLAKE2S_BlockLen);
          UpdateLen(t[1], t[0], BLAKE2S_BlockLen);
          blake2s_compress(ctx, false);  {compress (not last)}
          inc(Ptr2Inc(Msg),BLAKE2S_BlockLen);
          dec(mlen,BLAKE2S_BlockLen);
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
procedure blake2s_Final(var ctx: blake2s_ctx; var Digest: TBlake2SDigest);
  {-Finalize calculation, generate message digest, clear context}
var
  i: integer;
begin
  with ctx do begin
    UpdateLen(t[1], t[0], c);
    while c < BLAKE2S_BlockLen do begin   {fill up with zeros}
      b[c] := 0;
      inc(c);
    end;
    blake2s_compress(ctx, true);          {final block}
    {little endian convert and store}
    fillchar(Digest, sizeof(Digest),0);
    for i:=0 to outlen-1 do begin
      Digest[i] := (h[i shr 2] shr (8*(i and 3))) and $FF;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function blake2s_full(var dig: TBlake2SDigest; diglen: word;
                          key: pointer; keylen: word;
                          msg: pointer; mlen: longint): integer;
  {-Calculate hash digest of Msg with init/update/final}
var
  ctx: blake2s_ctx;
begin
  if blake2s_init(ctx, key, keylen, diglen) <> 0 then begin
    blake2s_full := -1;
  end
  else begin
    blake2s_update(ctx, msg, mlen);
    blake2s_final(ctx, dig);
    blake2s_full := 0;
  end;
end;


{---------------------------------------------------------------------------}
function blake2s_selftest: boolean;
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
  blake2s_res: TBlake2SDigest = (
    $6A, $41, $1F, $08, $CE, $25, $AD, $CD,
    $FB, $02, $AB, $A6, $41, $45, $1C, $EC,
    $53, $C5, $98, $B2, $4F, $4F, $C7, $87,
    $FB, $DC, $88, $79, $7F, $4C, $1D, $FE);
  {Parameter sets}
  b2s_md_len: array[0..3] of integer = (16, 20, 28, 32);
  b2s_in_len: array[0..5] of integer = (0,  3,  64, 65, 255, 1024);
var
  i,j, outlen, inlen: integer;
  md, key: TBlake2SDigest;
  ctx: blake2s_ctx;
  inb: array[0..2000] of byte;
begin
  blake2s_selftest := false;
  {256-bit hash for testing}
  if blake2s_init(ctx, nil, 0, 32) <> 0 then exit;
  for i:=0 to 3 do begin
    outlen := b2s_md_len[i];
    for j:=0 to 5 do begin
      inlen := b2s_in_len[j];
      selftest_seq(pbyte(@inb), inlen, inlen);      {unkeyed hash}
      if blake2s_full(md, outlen, nil, 0, @inb, inlen) <> 0 then exit;
      blake2s_update(ctx, @md, outlen);             {hash the hash}
      selftest_seq(pbyte(@key), outlen, outlen);    {keyed hash}
      if blake2s_full(md, outlen, @key, outlen, @inb, inlen) <> 0 then exit;
      blake2s_update(ctx, @md, outlen);             {hash the hash}
    end;
  end;
  {Compute and compare the hash of hashes.}
  blake2s_final(ctx, md);
  for i:=0 to 31 do begin
    if md[i] <> blake2s_res[i] then exit;
  end;
  blake2s_selftest := true;
end;

begin
  {$ifdef HAS_ASSERT}
    assert(sizeof(blake2s_ctx)=HASHCTXSIZE , '** Invalid sizeof(blake2s_ctx)');
  {$else}
    if sizeof(blake2s_ctx)<>HASHCTXSIZE then RunError(227);
  {$endif}
end.

