unit SHA256;

{SHA256 - 256 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA256 - 256 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - Latest specification of Secure Hash Standard:
                      http://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf
                    - Test vectors and intermediate values:
                      http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA_All.pdf

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.1      03.01.02  W.Ehrhardt  Reference implementation
 0.2      03.01.02  we          BP7 optimization
 0.21     03.01.02  we          TP6 changes
 0.3      03.01.02  we          Delphi32 optimization
 0.4      03.01.02  we          with TW32Buf and assignment via RB in SHA256Compress
 0.5      07.01.02  we          Opt. Delphi UpdateLen
 0.6      23.02.02  we          Free Pascal compatibility
 0.7      03.03.02  we          VirtualPascal compatibility
 0.71     03.03.02  we          FPC with ASM (intel)
 0.72     03.03.02  we          TP55 compatibility
 0.80     23.07.03  we          With SHA256File, SHA256Full
 0.81     26.07.03  we          With SHA256Full in self test, D6+ - warnings
 2.00     26.07.03  we          common vers., longint for word32, D4+ - warnings
 2.01     04.08.03  we          type TSHA256Block for HMAC
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 2.45     11.10.03  we          Speedup: Inline for Maj(), Ch()
 2.50     17.11.03  we          Speedup in update, don't clear W in compress
 2.51     20.11.03  we          Full range UpdateLen
 3.00     01.12.03  we          Common version 3.0
 3.01     22.12.03  we          TP5/5.5: RB, FS inline
 3.02     22.12.03  we          TP5/5.5: FS -> FS1, FS2
 3.03     22,12.03  we          Changed UpdateLen: Definition and TP5/5.5 inline
 3.04     22.12.03  we          TP5/5.5: inline function ISHR
 3.05     22.12.03  we          ExpandMessageBlocks/BASM
 3.06     24.12.03  we          FIPS notation: S[] -> A..H, partial unroll
 3.07     05.03.04  we          Update fips180-2 URL
 3.08     26.02.05  we          With {$ifdef StrictLong}
 3.09     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.10     17.12.05  we          Force $I- in SHA256File
 3.11     15.01.06  we          uses Hash unit and THashDesc
 3.12     15.01.06  we          BugFix for 16 bit without BASM
 3.13     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.14     22.01.06  we          Removed HSelfTest from descriptor
 3.15     11.02.06  we          Descriptor as typed const
 3.16     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.17     22.02.07  we          values for OID vector
 3.18     30.06.07  we          Use conditional define FPC_ProcVar
 3.19     04.10.07  we          FPC: {$asmmode intel}
 3.20     02.05.08  we          Bit-API: SHA256FinalBits/Ex
 3.21     05.05.08  we          THashDesc constant with HFinalBit field
 3.22     12.11.08  we          Uses BTypes, Ptr2Inc and/or Str255/Str127
 3.23     11.03.12  we          Updated references
 3.24     26.12.12  we          D17 and PurePascal
 3.25     16.08.15  we          Removed $ifdef DLL / stdcall
 3.26     15.05.17  we          adjust OID to new MaxOIDLen
 3.27     29.11.17  we          SHA256File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2017 Wolfgang Ehrhardt

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

{NOTE: FIPS Ch and May functions can be optimized. Wei Dai (Crypto++ 3.1)
credits Rich Schroeppel (rcs@cs.arizona.edu), V 5.1 does not!?}

{$i STD.INC}

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

{$define UNROLL}   {Speedup for all but TP5/5.5 and maybe VP}

{$ifdef VER50}
  {$undef UNROLL}  {Only VER50, VER55 uses UNROLL}
{$endif}

{$ifdef VirtualPascal}
  {$undef UNROLL}
{$endif}

uses
  BTypes,Hash;

procedure SHA256Init(var Context: THashContext);
  {-initialize context}

procedure SHA256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA256Final(var Context: THashContext; var Digest: TSHA256Digest);
  {-finalize SHA256 calculation, clear context}

procedure SHA256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA256 calculation, clear context}

procedure SHA256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA256FinalBits(var Context: THashContext; var Digest: TSHA256Digest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA256SelfTest: boolean;
  {-self test for string from SHA256 document}

procedure SHA256Full(var Digest: TSHA256Digest; Msg: pointer; Len: word);
  {-SHA256 of Msg with init/update/final}

procedure SHA256FullXL(var Digest: TSHA256Digest; Msg: pointer; Len: longint);
  {-SHA256 of Msg with init/update/final}

procedure SHA256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA256Digest; var buf; bsize: word; var Err: word);
  {-SHA256 of file, buf: buffer with at least bsize bytes}


implementation


{$ifdef BIT16}
  {$F-}
{$endif}

const
  SHA256_BlockLen  = 64;

{Internal types for type casting}
type
  TWorkBuf = array[0..63] of longint;


{2.16.840.1.101.3.4.2.1}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) sha256(1)}
const
  SHA256_OID : TOID_Vec = (2,16,840,1,101,3,4,2,1,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA256_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA256_BlockLen;
               HDigestlen: sizeof(TSHA256Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA256Init;
               HFinal    : @SHA256FinalEx;
               HUpdateXL : @SHA256UpdateXL;
             {$else}
               HInit     : SHA256Init;
               HFinal    : SHA256FinalEx;
               HUpdateXL : SHA256UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA256);
               HName     : 'SHA256';
               HPtrOID   : @SHA256_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA256FinalBitsEx;
             {$else}
               HFinalBit : SHA256FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA256_Desc: THashDesc;
{$endif}


{$ifndef BIT16}

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

  {---------------------------------------------------------------------------}
  function RB(A: longint): longint;
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

  {---------------------------------------------------------------------------}
  procedure UpdateLen(var whi, wlo: longint; BLen: longint);
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

  {---------------------------------------------------------------------------}
  function Sum0(x: longint): longint; assembler;   {&frame-}
    {-Big sigma 0: RotRight(x,2) xor RotRight(x,13) xor RotRight(x,22)}
  asm
    {$ifdef LoadArgs}
      mov eax,[x]
    {$endif}
    mov  ecx,eax
    mov  edx,eax
    ror  eax,2
    ror  edx,13
    ror  ecx,22
    xor  eax,edx
    xor  eax,ecx
  end;

  {---------------------------------------------------------------------------}
  function Sum1(x: longint): longint; assembler;  {&frame-}
    {-Big sigma 1: RotRight(x,6) xor RotRight(x,11) xor RotRight(x,25)}
  asm
    {$ifdef LoadArgs}
      mov eax,[x]
    {$endif}
    mov  ecx,eax
    mov  edx,eax
    ror  eax,6
    ror  edx,11
    ror  ecx,25
    xor  eax,edx
    xor  eax,ecx
  end;

  {$define USE_ExpandMessageBlocks}

  {---------------------------------------------------------------------------}
  procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuf32);
    {-Calculate "expanded message blocks"}
  begin
    asm
       push  esi
       push  edi
       push  ebx
       mov   esi,[W]
       mov   edx,[Buf]
       {part 1: W[i]:= RB(TW32Buf(Buf)[i])}
       mov   ecx,16
  @@1: mov   eax,[edx]
       xchg  al,ah
       rol   eax,16
       xchg  al,ah
       mov   [esi],eax
       add   esi,4
       add   edx,4
       dec   ecx
       jnz   @@1
       {part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);}
       mov   ecx,48
  @@2: mov   edi,[esi-7*4]    {W[i-7]}
       mov   eax,[esi-2*4]    {W[i-2]}
       mov   ebx,eax          {Sig1: RR17 xor RR19 xor SRx,10}
       mov   edx,eax
       ror   eax,17
       ror   edx,19
       shr   ebx,10
       xor   eax,edx
       xor   eax,ebx
       add   edi,eax
       mov   eax,[esi-15*4]   {W[i-15]}
       mov   ebx,eax          {Sig0: RR7 xor RR18 xor SR3}
       mov   edx,eax
       ror   eax,7
       ror   edx,18
       shr   ebx,3
       xor   eax,edx
       xor   eax,ebx
       add   eax,edi
       add   eax,[esi-16*4]  {W[i-16]}
       mov   [esi],eax
       add   esi,4
       dec   ecx
       jnz   @@2
       pop   ebx
       pop   edi
       pop   esi
    end;
  end;
{$endif}

{$else}

{$ifndef BASM16}

{TP5/5.5}

{$undef USE_ExpandMessageBlocks}

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


{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $86/$C6/      { xchg dh,al}
  $86/$E2);     { xchg dl,ah}


{---------------------------------------------------------------------------}
function FS1(x: longint; c: integer): longint;
  {-Rotate x right, c<=16!!}
inline(
  $59/          {  pop  cx   }
  $58/          {  pop  ax   }
  $5A/          {  pop  dx   }
  $8B/$DA/      {  mov  bx,dx}
  $D1/$EB/      {L:shr  bx,1 }
  $D1/$D8/      {  rcr  ax,1 }
  $D1/$DA/      {  rcr  dx,1 }
  $49/          {  dec  cx   }
  $75/$F7);     {  jne  L    }


{---------------------------------------------------------------------------}
function FS2(x: longint; c: integer): longint;
  {-Rotate x right, c+16, c<16!!}
inline(
  $59/          {  pop  cx   }
  $5A/          {  pop  dx   }
  $58/          {  pop  ax   }
  $8B/$DA/      {  mov  bx,dx}
  $D1/$EB/      {L:shr  bx,1 }
  $D1/$D8/      {  rcr  ax,1 }
  $D1/$DA/      {  rcr  dx,1 }
  $49/          {  dec  cx   }
  $75/$F7);     {  jne  L    }


{---------------------------------------------------------------------------}
function ISHR(x: longint; c: integer): longint;
  {-Shift x right}
inline(
  $59/          {  pop  cx   }
  $58/          {  pop  ax   }
  $5A/          {  pop  dx   }
  $D1/$EA/      {L:shr  dx,1 }
  $D1/$D8/      {  rcr  ax,1 }
  $49/          {  dec  cx   }
  $75/$F9);     {  jne  L    }


{---------------------------------------------------------------------------}
function Sig0(x: longint): longint;
  {-Small sigma 0}
begin
  Sig0 := FS1(x,7) xor FS2(x,18-16) xor ISHR(x,3);
end;


{---------------------------------------------------------------------------}
function Sig1(x: longint): longint;
  {-Small sigma 1}
begin
  Sig1 := FS2(x,17-16) xor FS2(x,19-16) xor ISHR(x,10);
end;


{---------------------------------------------------------------------------}
function Sum0(x: longint): longint;
  {-Big sigma 0}
begin
  Sum0 := FS1(x,2) xor FS1(x,13) xor FS2(x,22-16);
end;


{---------------------------------------------------------------------------}
function Sum1(x: longint): longint;
  {-Big sigma 1}
begin
  Sum1 := FS1(x,6) xor FS1(x,11) xor FS2(x,25-16);
end;


{$else}

{TP 6/7/Delphi1 for 386+}

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


{---------------------------------------------------------------------------}
function RB(A: longint): longint; assembler;
  {-reverse byte order in longint}
asm
  mov     ax,word ptr [A]
  mov     dx,word ptr [A+2]
  xchg    al,dh
  xchg    ah,dl
end;


{---------------------------------------------------------------------------}
function Sum0(x: longint): longint; assembler;
  {-Big sigma 0: RotRight(x,2) xor RotRight(x,13) xor RotRight(x,22)}
asm
  db $66;  mov  ax,word ptr x
  db $66;  mov  bx,ax
  db $66;  mov  dx,ax
  db $66;  ror  ax,2
  db $66;  ror  dx,13
  db $66;  ror  bx,22
  db $66;  xor  ax,dx
  db $66;  xor  ax,bx
  db $66;  mov  dx,ax
  db $66;  shr  dx,16
end;


{---------------------------------------------------------------------------}
function Sum1(x: longint): longint; assembler;
  {-Big sigma 1: RotRight(x,6) xor RotRight(x,11) xor RotRight(x,25)}
asm
  db $66;  mov  ax,word ptr x
  db $66;  mov  bx,ax
  db $66;  mov  dx,ax
  db $66;  ror  ax,6
  db $66;  ror  dx,11
  db $66;  ror  bx,25
  db $66;  xor  ax,dx
  db $66;  xor  ax,bx
  db $66;  mov  dx,ax
  db $66;  shr  dx,16
end;


{$define USE_ExpandMessageBlocks}
{---------------------------------------------------------------------------}
procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuf32); assembler;
  {-Calculate "expanded message blocks"}
asm
             push  ds
             {part 1: W[i]:= RB(TW32Buf(Buf)[i])}
             les   di,[Buf]
             lds   si,[W]
             mov   cx,16
@@1: db $66; mov   ax,es:[di]
             xchg  al,ah
     db $66; rol   ax,16
             xchg  al,ah
     db $66; mov   [si],ax
             add   si,4
             add   di,4
             dec   cx
             jnz   @@1
             {part 2: W[i]:= Sig1(W[i-2]) + W[i-7] + Sig0(W[i-15]) + W[i-16];}
             mov   cx,48
@@2: db $66; mov   di,[si-7*4]    {W[i-7]}
     db $66; mov   ax,[si-2*4]    {W[i-2]}
     db $66; mov   bx,ax          {Sig1: RR17 xor RR19 xor SRx,10}
     db $66; mov   dx,ax
     db $66; ror   ax,17
     db $66; ror   dx,19
     db $66; shr   bx,10
     db $66; xor   ax,dx
     db $66; xor   ax,bx
     db $66; add   di,ax
     db $66; mov   ax,[si-15*4]   {W[i-15]}
     db $66; mov   bx,ax          {Sig0: RR7 xor RR18 xor SR3}
     db $66; mov   dx,ax
     db $66; ror   ax,7
     db $66; ror   dx,18
     db $66; shr   bx,3
     db $66; xor   ax,dx
     db $66; xor   ax,bx
     db $66; add   ax,di
     db $66; add   ax,[si-16*4]  {W[i-16]}
     db $66; mov   [si],ax
             add   si,4
             dec   cx
             jnz   @@2
             pop   ds
end;


{$endif BASM16}

{$endif BIT16}



{$ifdef PurePascal}
{---------------------------------------------------------------------------}
procedure SHA256Compress(var Data: THashContext);
  {-Actual hashing function}
var
  i: integer;
  T, A, B, C, D, E, F, G, H: longint;
  W: TWorkBuf;
const
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
  K: array[0..63] of longint = (
       $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5,
       $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
       $d807aa98, $12835b01, $243185be, $550c7dc3,
       $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
       $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc,
       $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
       $983e5152, $a831c66d, $b00327c8, $bf597fc7,
       $c6e00bf3, $d5a79147, $06ca6351, $14292967,
       $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
       $650a7354, $766a0abb, $81c2c92e, $92722c85,
       $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3,
       $d192e819, $d6990624, $f40e3585, $106aa070,
       $19a4c116, $1e376c08, $2748774c, $34b0bcb5,
       $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
       $748f82ee, $78a5636f, $84c87814, $8cc70208,
       $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
     );
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin

  {-Calculate "expanded message blocks"}
  {Part 1: Transfer buffer with little -> big endian conversion}
  for i:=  0 to 15 do W[i] := RB(THashBuf32(Data.Buffer)[i]);
  {Part 2: Calculate remaining "expanded message blocks"}
  for i:= 16 to 63 do begin
    {A=Sig1(W[i-2]), B=Sig0(W[i-15])}
    A := W[i-2];  A := ((A shr 17) or (A shl 15)) xor ((A shr 19) or (A shl 13)) xor (A shr 10);
    B := W[i-15]; B := ((B shr 7) or (B shl 25)) xor ((B shr 18) or (B shl 14)) xor (B shr 3);
    W[i]:= A + W[i-7] + B + W[i-16];
  end;

  {Assign old working hasg to variables A..H}
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];
  F := Data.Hash[5];
  G := Data.Hash[6];
  H := Data.Hash[7];

  {SHA256 compression function}
  {partially unrolled loop}
  i := 0;
  repeat
    T := H + (((E shr 6) or (E shl 26)) xor ((E shr 11) or (E shl 21)) xor ((E shr 25) or (E shl  7))) +
             (((F xor G) and E) xor G) + W[i  ] + K[i  ];
    H := T + (((A shr 2) or (A shl 30)) xor ((A shr 13) or (A shl 19)) xor ((A shr 22) or (A shl 10))) +
             (((A or B) and C) or (A and B));
    inc(D,T);
    T := G + (((D shr 6) or (D shl 26)) xor ((D shr 11) or (D shl 21)) xor ((D shr 25) or (D shl  7))) +
             (((E xor F) and D) xor F) + W[i+1] + K[i+1];
    G := T + (((H shr 2) or (H shl 30)) xor ((H shr 13) or (H shl 19)) xor ((H shr 22) or (H shl 10))) +
             (((H or A) and B) or (H and A));
    inc(C,T);
    T := F + (((C shr 6) or (C shl 26)) xor ((C shr 11) or (C shl 21)) xor ((C shr 25) or (C shl  7))) +
             (((D xor E) and C) xor E) + W[i+2] + K[i+2];
    F := T + (((G shr 2) or (G shl 30)) xor ((G shr 13) or (G shl 19)) xor ((G shr 22) or (G shl 10))) +
             (((G or H) and A) or (G and H));
    inc(B,T);
    T := E + (((B shr 6) or (B shl 26)) xor ((B shr 11) or (B shl 21)) xor ((B shr 25) or (B shl  7))) +
             (((C xor D) and B) xor D) + W[i+3] + K[i+3];
    E := T + (((F shr 2) or (F shl 30)) xor ((F shr 13) or (F shl 19)) xor ((F shr 22) or (F shl 10))) +
             (((F or G) and H) or (F and G));
    inc(A,T);
    T := D + (((A shr 6) or (A shl 26)) xor ((A shr 11) or (A shl 21)) xor ((A shr 25) or (A shl  7))) +
             (((B xor C) and A) xor C) + W[i+4] + K[i+4];
    D := T + (((E shr 2) or (E shl 30)) xor ((E shr 13) or (E shl 19)) xor ((E shr 22) or (E shl 10))) +
             (((E or F) and G) or (E and F));
    inc(H,T);
    T := C + (((H shr 6) or (H shl 26)) xor ((H shr 11) or (H shl 21)) xor ((H shr 25) or (H shl  7))) +
             (((A xor B) and H) xor B) + W[i+5] + K[i+5];
    C := T + (((D shr 2) or (D shl 30)) xor ((D shr 13) or (D shl 19)) xor ((D shr 22) or (D shl 10))) +
             (((D or E) and F) or (D and E));
    inc(G,T);
    T := B + (((G shr 6) or (G shl 26)) xor ((G shr 11) or (G shl 21)) xor ((G shr 25) or (G shl  7))) +
             (((H xor A) and G) xor A) + W[i+6] + K[i+6];
    B := T + (((C shr 2) or (C shl 30)) xor ((C shr 13) or (C shl 19)) xor ((C shr 22) or (C shl 10))) +
             (((C or D) and E) or (C and D));
    inc(F,T);
    T := A + (((F shr 6) or (F shl 26)) xor ((F shr 11) or (F shl 21)) xor ((F shr 25) or (F shl  7))) +
             (((G xor H) and F) xor H) + W[i+7] + K[i+7];
    A := T + (((B shr 2) or (B shl 30)) xor ((B shr 13) or (B shl 19)) xor ((B shr 22) or (B shl 10))) +
             (((B or C) and D) or (B and C));
    inc(E,T);
    inc(i,8)
  until i>63;

  {Calculate new working hash}
  inc(Data.Hash[0],A);
  inc(Data.Hash[1],B);
  inc(Data.Hash[2],C);
  inc(Data.Hash[3],D);
  inc(Data.Hash[4],E);
  inc(Data.Hash[5],F);
  inc(Data.Hash[6],G);
  inc(Data.Hash[7],H);
end;

{$else}

{---------------------------------------------------------------------------}
procedure SHA256Compress(var Data: THashContext);
  {-Actual hashing function}
var
  i: integer;
{$ifdef UNROLL}
  T,
{$else}
  T1,T2: longint;
{$endif}
  A, B, C, D, E, F, G, H: longint;
  W: TWorkBuf;
const
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
  K: array[0..63] of longint = (
       $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5,
       $3956c25b, $59f111f1, $923f82a4, $ab1c5ed5,
       $d807aa98, $12835b01, $243185be, $550c7dc3,
       $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174,
       $e49b69c1, $efbe4786, $0fc19dc6, $240ca1cc,
       $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
       $983e5152, $a831c66d, $b00327c8, $bf597fc7,
       $c6e00bf3, $d5a79147, $06ca6351, $14292967,
       $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
       $650a7354, $766a0abb, $81c2c92e, $92722c85,
       $a2bfe8a1, $a81a664b, $c24b8b70, $c76c51a3,
       $d192e819, $d6990624, $f40e3585, $106aa070,
       $19a4c116, $1e376c08, $2748774c, $34b0bcb5,
       $391c0cb3, $4ed8aa4a, $5b9cca4f, $682e6ff3,
       $748f82ee, $78a5636f, $84c87814, $8cc70208,
       $90befffa, $a4506ceb, $bef9a3f7, $c67178f2
     );
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin
  {-Calculate "expanded message blocks"}
  {$ifdef USE_ExpandMessageBlocks}
    {Use BASM-Code}
    ExpandMessageBlocks(W, THashBuf32(Data.Buffer));
  {$else}
    {Avoid proc call overhead for TP5/5.5}
    {Part 1: Transfer buffer with little -> big endian conversion}
    for i:=  0 to 15 do W[i]:= RB(THashBuf32(Data.Buffer)[i]);
    {Part 2: Calculate remaining "expanded message blocks"}
    for i:= 16 to 63 do W[i]:= Sig1(W[i-2]) + W[i-7] + Sig0(W[i-15]) + W[i-16];
  {$endif}

  {Assign old working hasg to variables A..H}
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];
  F := Data.Hash[5];
  G := Data.Hash[6];
  H := Data.Hash[7];

  {SHA256 compression function}

{$ifdef UNROLL}

  {partially unrolled loop}
  i := 0;
  repeat
    T := H + Sum1(E) + (((F xor G) and E) xor G) + W[i  ] + K[i  ];
    H := T + Sum0(A) + (((A or B) and C) or (A and B));
    inc(D,T);
    T := G + Sum1(D) + (((E xor F) and D) xor F) + W[i+1] + K[i+1];
    G := T + Sum0(H) + (((H or A) and B) or (H and A));
    inc(C,T);
    T := F + Sum1(C) + (((D xor E) and C) xor E) + W[i+2] + K[i+2];
    F := T + Sum0(G) + (((G or H) and A) or (G and H));
    inc(B,T);
    T := E + Sum1(B) + (((C xor D) and B) xor D) + W[i+3] + K[i+3];
    E := T + Sum0(F) + (((F or G) and H) or (F and G));
    inc(A,T);
    T := D + Sum1(A) + (((B xor C) and A) xor C) + W[i+4] + K[i+4];
    D := T + Sum0(E) + (((E or F) and G) or (E and F));
    inc(H,T);
    T := C + Sum1(H) + (((A xor B) and H) xor B) + W[i+5] + K[i+5];
    C := T + Sum0(D) + (((D or E) and F) or (D and E));
    inc(G,T);
    T := B + Sum1(G) + (((H xor A) and G) xor A) + W[i+6] + K[i+6];
    B := T + Sum0(C) + (((C or D) and E) or (C and D));
    inc(F,T);
    T := A + Sum1(F) + (((G xor H) and F) xor H) + W[i+7] + K[i+7];
    A := T + Sum0(B) + (((B or C) and D) or (B and C));
    inc(E,T);
    inc(i,8)
  until i>63;

{$else}
  for i:=0 to 63 do begin
    T1:= H + Sum1(E) + (((F xor G) and E) xor G) + K[i] + W[i];
    T2:= Sum0(A) + (((A or B) and C) or (A and B));
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;
{$endif}

  {Calculate new working hash}
  inc(Data.Hash[0],A);
  inc(Data.Hash[1],B);
  inc(Data.Hash[2],C);
  inc(Data.Hash[3],D);
  inc(Data.Hash[4],E);
  inc(Data.Hash[5],F);
  inc(Data.Hash[6],G);
  inc(Data.Hash[7],H);
end;

{$endif}

{---------------------------------------------------------------------------}
procedure SHA256Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: array[0..7] of longint = ($6a09e667, $bb67ae85, $3c6ef372, $a54ff53a, $510e527f, $9b05688c, $1f83d9ab, $5be0cd19);
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin
  {Clear context, buffer=0!!}
  fillchar(Context,sizeof(Context),0);
  move(SIV,Context.Hash,sizeof(SIV));
end;


{---------------------------------------------------------------------------}
procedure SHA256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
var
  i: integer;
begin
  {Update message bit length}
  if Len<=$1FFFFFFF then UpdateLen(Context.MLen[1], Context.MLen[0], Len shl 3)
  else begin
    for i:=1 to 8 do UpdateLen(Context.MLen[1], Context.MLen[0], Len)
  end;
  while Len > 0 do begin
    {fill block with msg data}
    Context.Buffer[Context.Index]:= pByte(Msg)^;
    inc(Ptr2Inc(Msg));
    inc(Context.Index);
    dec(Len);
    if Context.Index=SHA256_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      SHA256Compress(Context);
      while Len>=SHA256_BlockLen do begin
        move(Msg^,Context.Buffer,SHA256_BlockLen);
        SHA256Compress(Context);
        inc(Ptr2Inc(Msg),SHA256_BlockLen);
        dec(Len,SHA256_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA256Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA256UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}
var
  i: integer;
begin
  {Message padding}
  {append bits from BData and a single '1' bit}
  if (bitlen>0) and (bitlen<=7) then begin
    Context.Buffer[Context.Index]:= (BData and BitAPI_Mask[bitlen]) or BitAPI_PBit[bitlen];
    UpdateLen(Context.MLen[1], Context.MLen[0], bitlen);
  end
  else Context.Buffer[Context.Index]:= $80;
  for i:=Context.Index+1 to 63 do Context.Buffer[i] := 0;
  {2. Compress if more than 448 bits, (no room for 64 bit length}
  if Context.Index>= 56 then begin
    SHA256Compress(Context);
    fillchar(Context.Buffer,56,0);
  end;
  {Write 64 bit msg length into the last bits of the last block}
  {(in big endian format) and do a final compress}
  THashBuf32(Context.Buffer)[14]:= RB(Context.MLen[1]);
  THashBuf32(Context.Buffer)[15]:= RB(Context.MLen[0]);
  SHA256Compress(Context);
  {Hash -> Digest to little endian format}
  for i:=0 to 7 do THashDig32(Digest)[i]:= RB(Context.Hash[i]);
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure SHA256FinalBits(var Context: THashContext; var Digest: TSHA256Digest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA256FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA256 calculation, clear context}
begin
  SHA256FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA256Final(var Context: THashContext; var Digest: TSHA256Digest);
  {-finalize SHA256 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA256FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA256SelfTest: boolean;
  {-self test for string from SHA256 document}
const
  s1: string[ 3] = 'abc';
  s2: string[56] = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  D1: TSHA256Digest = ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
                       $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest = ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
                       $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest = ($bd,$4f,$9e,$98,$be,$b6,$8c,$6e,$ad,$32,$43,$b1,$b4,$c7,$fe,$d7,
                       $5f,$a4,$fe,$aa,$b1,$f8,$47,$95,$cb,$d8,$a9,$86,$76,$a2,$a3,$75);
  D4: TSHA256Digest = ($f1,$54,$1d,$eb,$68,$d1,$34,$eb,$a9,$9f,$82,$cf,$d8,$7e,$2a,$b3,
                       $1d,$33,$af,$4b,$6d,$e0,$08,$6a,$9b,$ed,$15,$c2,$ec,$69,$cc,$cb);
var
  Context: THashContext;
  Digest : TSHA256Digest;

  function SingleTest(s: Str127; TDig: TSHA256Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA256Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA256_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA256Init(Context);
    for i:=1 to length(s) do SHA256Update(Context,@s[i],1);
    SHA256Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA256_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  SHA256SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  SHA256Init(Context);
  SHA256FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@SHA256_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  SHA256Init(Context);
  SHA256FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@SHA256_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from SHA256 document}
  SHA256SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA256FullXL(var Digest: TSHA256Digest; Msg: pointer; Len: longint);
  {-SHA256 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA256Init(Context);
  SHA256UpdateXL(Context, Msg, Len);
  SHA256Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA256Full(var Digest: TSHA256Digest; Msg: pointer; Len: word);
  {-SHA256 of Msg with init/update/final}
begin
  SHA256FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA256Digest; var buf; bsize: word; var Err: word);
  {-SHA256 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA256_Desc, tmp, buf, bsize, Err);
  move(tmp,Digest,sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA256_Desc, sizeof(SHA256_Desc), 0);
    with SHA256_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA256_BlockLen;
       HDigestlen:= sizeof(TSHA256Digest);
       HInit     := SHA256Init;
       HFinal    := SHA256FinalEx;
       HUpdateXL := SHA256UpdateXL;
       HAlgNum   := longint(_SHA256);
       HName     := 'SHA256';
       HPtrOID   := @SHA256_OID;
       HLenOID   := 9;
       HFinalBit := SHA256FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA256, @SHA256_Desc);
end.
