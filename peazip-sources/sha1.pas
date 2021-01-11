unit SHA1;

{SHA1 - 160 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA1 - 160 bit Secure Hash Function

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
 1.00     03.01.02  W.Ehrhardt  BP7 implementation
 1.01     14.03.02  we          D1-D6, FPC, VP
 1.02     14.03.02  we          TP6
 1.03     14.03.02  we          TP6/7 386-Code
 1.04     14.03.02  we          TP5.5
 1.10     15.03.02  we          self test with 2 strings
 1.11     02.01.03  we          const SFA with @ for FPC 1.0.6
 1.20     23.07.03  we          With SHA1File, SHA1Full
 1.21     26.07.03  we          With SHA1Full in self test
 2.00     26.07.03  we          common vers., longint for word32, D4+ - warnings
 2.01     03.08.03  we          type TSHA1Block for HMAC
 2.02     23.08.03  we          SHA1Compress in interface for prng
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 2.45     11.10.03  we          Speedup: partial unroll, no function calls
 2.50     16.11.03  we          Speedup in update, don't clear W in compress
 2.51     17.11.03  we          BIT16: partial unroll, BIT32: inline rot
 2.52     17.11.03  we          ExpandMessageBlocks
 2.53     18.11.03  we          LRot32, RB mit inline()
 2.54     20.11.03  we          Full range UpdateLen
 2.55     30.11.03  we          BIT16: {$F-}
 2.56     30.11.03  we          BIT16: LRot_5, LRot_30
 3.00     01.12.03  we          Common version 3.0
 3.01     22.12.03  we          BIT16: Two INCs
 3.02     22.12.03  we          BASM16: asm Lrot30
 3.03     22.12.03  we          TP5/5.5: LRot, RA inline
 3.04     22,12.03  we          Changed UpdateLen: Definition and TP5/5.5 inline
 3.05     05.03.04  we          Update fips180-2 URL
 3.06     26.02.05  we          With {$ifdef StrictLong}
 3.07     05.05.05  we          Use longint() in SH1Init to avoid D9 errors if $R+
 3.08     17.12.05  we          Force $I- in SHA1File
 3.09     08.01.06  we          SHA1Compress removed from interface
 3.10     15.01.06  we          uses Hash unit and THashDesc
 3.11     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.12     22.01.06  we          Removed HSelfTest from descriptor
 3.13     11.02.06  we          Descriptor as typed const
 3.14     26.03.06  we          Round constants K1..K4, code reordering
 3.15     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.16     22.02.07  we          values for OID vector
 3.17     30.06.07  we          Use conditional define FPC_ProcVar
 3.18     04.10.07  we          FPC: {$asmmode intel}
 3.19     02.05.08  we          Bit-API: SHA1FinalBits/Ex
 3.20     05.05.08  we          THashDesc constant with HFinalBit field
 3.21     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 3.22     12.03.10  we          Fix VP feature in ExpandMessageBlocks
 3.23     11.03.12  we          Updated references
 3.24     26.12.12  we          D17 and PurePascal
 3.25     16.08.15  we          Removed $ifdef DLL / stdcall
 3.26     15.05.17  we          adjust OID to new MaxOIDLen
 3.27     29.11.17  we          SHA1File - fname: string

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

uses
  BTypes,Hash;


procedure SHA1Init(var Context: THashContext);
  {-initialize context}

procedure SHA1Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA1UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA1Final(var Context: THashContext; var Digest: TSHA1Digest);
  {-finalize SHA1 calculation, clear context}

procedure SHA1FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA1 calculation, clear context}

procedure SHA1FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA1FinalBits(var Context: THashContext; var Digest: TSHA1Digest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA1SelfTest: boolean;
  {-self test SHA1: compare with known value}

procedure SHA1Full(var Digest: TSHA1Digest; Msg: pointer; Len: word);
  {-SHA1 of Msg with init/update/final}

procedure SHA1FullXL(var Digest: TSHA1Digest; Msg: pointer; Len: longint);
  {-SHA1 of Msg with init/update/final}

procedure SHA1File({$ifdef CONST} const {$endif} fname: string;
                   var Digest: TSHA1Digest; var buf; bsize: word; var Err: word);
  {-SHA1 of file, buf: buffer with at least bsize bytes}


implementation

{$ifdef BIT16}
  {$F-}
{$endif}

const
  SHA1_BlockLen  = 64;

const                             {round constants}
  K1 = longint($5A827999);        {round 00..19}
  K2 = longint($6ED9EBA1);        {round 20..39}
  K3 = longint($8F1BBCDC);        {round 40..59}
  K4 = longint($CA62C1D6);        {round 60..79}


{Internal types}
type
  TWorkBuf = array[0..79] of longint;

{1.3.14.3.2.26}
{iso(1) identified-organization(3) oiw(14) secsig(3) algorithms(2) hashAlgorithmIdentifier(26)}
const
  SHA1_OID : TOID_Vec = (1,3,14,3,2,26,-1,-1,-1,-1,-1); {Len=6}

{$ifndef VER5X}
const
  SHA1_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA1_BlockLen;
               HDigestlen: sizeof(TSHA1Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA1Init;
               HFinal    : @SHA1FinalEx;
               HUpdateXL : @SHA1UpdateXL;
             {$else}
               HInit     : SHA1Init;
               HFinal    : SHA1FinalEx;
               HUpdateXL : SHA1UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA1);
               HName     : 'SHA1';
               HPtrOID   : @SHA1_OID;
               HLenOID   : 6;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA1FinalBitsEx;
             {$else}
               HFinalBit : SHA1FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA1_Desc: THashDesc;
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

  {---------------------------------------------------------------------------}
  procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuffer);
    {-Calculate "expanded message blocks"}
  var
    i,T: longint;
  begin
    {Part 1: Transfer buffer with little -> big endian conversion}
    for i:=  0 to 15 do W[i]:= RB(THashBuf32(Buf)[i]);
    {Part 2: Calculate remaining "expanded message blocks"}
    for i:= 16 to 79 do begin
      T := W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
      W[i] := (T shl 1) or (T shr 31);
    end;
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

  {---------------------------------------------------------------------------}
  function RB(A: longint): longint; assembler;
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
  procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuffer); assembler;
    {-Calculate "expanded message blocks"}
  asm
    {$ifdef LoadArgs}
       mov   edx,Buf
       mov   ecx,W               {load W before push ebx to avoid VP crash}
       push  ebx                 {if compiling with no ASM stack frames}
       mov   ebx,ecx
    {$else}
       push  ebx
       mov   ebx,eax
    {$endif}
       {part1: W[i]:= RB(TW32Buf(Buf)[i])}
       mov   ecx,16
  @@1: mov   eax,[edx]
       xchg  al,ah
       rol   eax,16
       xchg  al,ah
       mov   [ebx],eax
       add   ebx,4
       add   edx,4
       dec   ecx
       jnz   @@1
       {part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);}
       mov   ecx,64
  @@2: mov   eax,[ebx- 3*4]
       xor   eax,[ebx- 8*4]
       xor   eax,[ebx-14*4]
       xor   eax,[ebx-16*4]
       rol   eax,1
       mov   [ebx],eax
       add   ebx,4
       dec   ecx
       jnz   @@2
       pop   ebx
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure SHA1Compress(var Data: THashContext);
  {-Actual hashing function}
var
  i: integer;
  A, B, C, D, E: longint;
  W: TWorkBuf;
begin

  ExpandMessageBlocks(W, Data.Buffer);

  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];

  {SHA1 compression function}
  {Partial unroll for more speed, full unroll is only slightly faster}
  {BIT32: rotateleft via inline}
  i := 0;
  while i<20 do begin
    inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[i  ] + K1); B := B shr 2 or B shl 30;
    inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[i+1] + K1); A := A shr 2 or A shl 30;
    inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[i+2] + K1); E := E shr 2 or E shl 30;
    inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[i+3] + K1); D := D shr 2 or D shl 30;
    inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[i+4] + K1); C := C shr 2 or C shl 30;
    inc(i,5);
  end;
  while i<40 do begin
    inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[i  ] + K2); B := B shr 2 or B shl 30;
    inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[i+1] + K2); A := A shr 2 or A shl 30;
    inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[i+2] + K2); E := E shr 2 or E shl 30;
    inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[i+3] + K2); D := D shr 2 or D shl 30;
    inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[i+4] + K2); C := C shr 2 or C shl 30;
    inc(i,5);
  end;
  while i<60 do begin
    inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[i  ] + K3); B := B shr 2 or B shl 30;
    inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[i+1] + K3); A := A shr 2 or A shl 30;
    inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[i+2] + K3); E := E shr 2 or E shl 30;
    inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[i+3] + K3); D := D shr 2 or D shl 30;
    inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[i+4] + K3); C := C shr 2 or C shl 30;
    inc(i,5);
  end;
  while i<80 do begin
    inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[i  ] + K4); B := B shr 2 or B shl 30;
    inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[i+1] + K4); A := A shr 2 or A shl 30;
    inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[i+2] + K4); E := E shr 2 or E shl 30;
    inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[i+3] + K4); D := D shr 2 or D shl 30;
    inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[i+4] + K4); C := C shr 2 or C shl 30;
    inc(i,5);
  end;

  {Calculate new working hash}
  inc(Data.Hash[0], A);
  inc(Data.Hash[1], B);
  inc(Data.Hash[2], C);
  inc(Data.Hash[3], D);
  inc(Data.Hash[4], E);
end;



{$else}


{$ifdef BASM16}

{TP6-7/Delphi1 for 386+}

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
function LRot_5(x: longint): longint;
  {-Rotate left 5}
inline(
  $66/$58/          {pop    eax    }
  $66/$C1/$C0/$05/  {rol    eax,5  }
  $66/$8B/$D0/      {mov    edx,eax}
  $66/$C1/$EA/$10); {shr    edx,16 }


{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
    $58/            {pop    ax     }
    $5A/            {pop    dx     }
    $86/$C6/        {xchg   dh,al  }
    $86/$E2);       {xchg   dl,ah  }


{---------------------------------------------------------------------------}
procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuffer); assembler;
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
             {part 2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);}
             mov   cx,64
@@2: db $66; mov   ax,[si- 3*4]
     db $66; xor   ax,[si- 8*4]
     db $66; xor   ax,[si-14*4]
     db $66; xor   ax,[si-16*4]
     db $66; rol   ax,1
     db $66; mov   [si],ax
             add   si,4
             dec   cx
             jnz   @@2
             pop   ds
end;

{---------------------------------------------------------------------------}
procedure SHA1Compress(var Data: THashContext);
  {-Actual hashing function}
var
  i: integer;
  A, B, C, D, E: longint;
  W: TWorkBuf;
begin
  ExpandMessageBlocks(W, Data.Buffer);
  {Assign old working hash to variables A..E}
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];

  {SHA1 compression function}
  {Partial unroll for more speed, full unroll only marginally faster}
  {Two INCs, LRot_30 via BASM}
  i := 0;
  while i<20 do begin
    inc(E,LRot_5(A)); inc(E,(D xor (B and (C xor D))) + W[i  ] + K1); asm db $66; rol word[B],30 end;
    inc(D,LRot_5(E)); inc(D,(C xor (A and (B xor C))) + W[i+1] + K1); asm db $66; rol word[A],30 end;
    inc(C,LRot_5(D)); inc(C,(B xor (E and (A xor B))) + W[i+2] + K1); asm db $66; rol word[E],30 end;
    inc(B,LRot_5(C)); inc(B,(A xor (D and (E xor A))) + W[i+3] + K1); asm db $66; rol word[D],30 end;
    inc(A,LRot_5(B)); inc(A,(E xor (C and (D xor E))) + W[i+4] + K1); asm db $66; rol word[C],30 end;
    inc(i,5);
  end;
  while i<40 do begin
    inc(E,LRot_5(A)); inc(E,(B xor C xor D) + W[i  ] + K2); asm db $66; rol word[B],30 end;
    inc(D,LRot_5(E)); inc(D,(A xor B xor C) + W[i+1] + K2); asm db $66; rol word[A],30 end;
    inc(C,LRot_5(D)); inc(C,(E xor A xor B) + W[i+2] + K2); asm db $66; rol word[E],30 end;
    inc(B,LRot_5(C)); inc(B,(D xor E xor A) + W[i+3] + K2); asm db $66; rol word[D],30 end;
    inc(A,LRot_5(B)); inc(A,(C xor D xor E) + W[i+4] + K2); asm db $66; rol word[C],30 end;
    inc(i,5);
  end;
  while i<60 do begin
    inc(E,LRot_5(A)); inc(E,((B and C) or (D and (B or C))) + W[i  ] + K3); asm db $66; rol word[B],30 end;
    inc(D,LRot_5(E)); inc(D,((A and B) or (C and (A or B))) + W[i+1] + K3); asm db $66; rol word[A],30 end;
    inc(C,LRot_5(D)); inc(C,((E and A) or (B and (E or A))) + W[i+2] + K3); asm db $66; rol word[E],30 end;
    inc(B,LRot_5(C)); inc(B,((D and E) or (A and (D or E))) + W[i+3] + K3); asm db $66; rol word[D],30 end;
    inc(A,LRot_5(B)); inc(A,((C and D) or (E and (C or D))) + W[i+4] + K3); asm db $66; rol word[C],30 end;
    inc(i,5);
  end;
  while i<80 do begin
    inc(E,LRot_5(A)); inc(E,(B xor C xor D) + W[i  ] + K4); asm db $66; rol word[B],30 end;
    inc(D,LRot_5(E)); inc(D,(A xor B xor C) + W[i+1] + K4); asm db $66; rol word[A],30 end;
    inc(C,LRot_5(D)); inc(C,(E xor A xor B) + W[i+2] + K4); asm db $66; rol word[E],30 end;
    inc(B,LRot_5(C)); inc(B,(D xor E xor A) + W[i+3] + K4); asm db $66; rol word[D],30 end;
    inc(A,LRot_5(B)); inc(A,(C xor D xor E) + W[i+4] + K4); asm db $66; rol word[C],30 end;
    inc(i,5);
  end;

  {Calculate new working hash}
  inc(Data.Hash[0], A);
  inc(Data.Hash[1], B);
  inc(Data.Hash[2], C);
  inc(Data.Hash[3], D);
  inc(Data.Hash[4], E);

end;


{$else}

{TP5/5.5}

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
function LRot_1(x: longint): longint;
  {-Rotate left 1}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $2B/$C9/      { sub  cx,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1);     { adc  ax,cx}


{---------------------------------------------------------------------------}
function LRot_5(x: longint): longint;
  {-Rotate left 5}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $2B/$C9/      { sub  cx,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1/      { adc  ax,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1/      { adc  ax,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1/      { adc  ax,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1/      { adc  ax,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1);     { adc  ax,cx}


{---------------------------------------------------------------------------}
function LRot_30(x: longint): longint;
  {-Rotate left 30 = rot right 2}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $8B/$CA/      { mov  cx,dx}
  $D1/$E9/      { shr  cx,1 }
  $D1/$D8/      { rcr  ax,1 }
  $D1/$DA/      { rcr  dx,1 }
  $8B/$CA/      { mov  cx,dx}
  $D1/$E9/      { shr  cx,1 }
  $D1/$D8/      { rcr  ax,1 }
  $D1/$DA);     { rcr  dx,1 }


{---------------------------------------------------------------------------}
procedure ExpandMessageBlocks(var W: TWorkBuf; var Buf: THashBuffer);
  {-Calculate "expanded message blocks"}
var
  i: integer;
begin
  {Part 1: Transfer buffer with little -> big endian conversion}
  for i:=  0 to 15 do W[i]:= RB(THashBuf32(Buf)[i]);
  {Part 2: Calculate remaining "expanded message blocks"}
  for i:= 16 to 79 do W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);
end;


{---------------------------------------------------------------------------}
procedure SHA1Compress(var Data: THashContext);
  {-Actual hashing function}
var
  i: integer;
  A, B, C, D, E: longint;
  W: TWorkBuf;
begin
  ExpandMessageBlocks(W, Data.Buffer);

  {Assign old working hash to variables A..E}
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];

  {SHA1 compression function}
  {Partial unroll for more speed, full unroll only marginally faster}
  {BIT16: rotateleft via function call}
  i := 0;
  while i<20 do begin
    inc(E,LRot_5(A) + (D xor (B and (C xor D))) + W[i  ] + K1); B := LRot_30(B);
    inc(D,LRot_5(E) + (C xor (A and (B xor C))) + W[i+1] + K1); A := LRot_30(A);
    inc(C,LRot_5(D) + (B xor (E and (A xor B))) + W[i+2] + K1); E := LRot_30(E);
    inc(B,LRot_5(C) + (A xor (D and (E xor A))) + W[i+3] + K1); D := LRot_30(D);
    inc(A,LRot_5(B) + (E xor (C and (D xor E))) + W[i+4] + K1); C := LRot_30(C);
    inc(i,5);
  end;
  while i<40 do begin
    inc(E,LRot_5(A) + (B xor C xor D) + W[i  ] + K2); B := LRot_30(B);
    inc(D,LRot_5(E) + (A xor B xor C) + W[i+1] + K2); A := LRot_30(A);
    inc(C,LRot_5(D) + (E xor A xor B) + W[i+2] + K2); E := LRot_30(E);
    inc(B,LRot_5(C) + (D xor E xor A) + W[i+3] + K2); D := LRot_30(D);
    inc(A,LRot_5(B) + (C xor D xor E) + W[i+4] + K2); C := LRot_30(C);
    inc(i,5);
  end;
  while i<60 do begin
    inc(E,LRot_5(A) + ((B and C) or (D and (B or C))) + W[i  ] + K3); B := LRot_30(B);
    inc(D,LRot_5(E) + ((A and B) or (C and (A or B))) + W[i+1] + K3); A := LRot_30(A);
    inc(C,LRot_5(D) + ((E and A) or (B and (E or A))) + W[i+2] + K3); E := LRot_30(E);
    inc(B,LRot_5(C) + ((D and E) or (A and (D or E))) + W[i+3] + K3); D := LRot_30(D);
    inc(A,LRot_5(B) + ((C and D) or (E and (C or D))) + W[i+4] + K3); C := LRot_30(C);
    inc(i,5);
  end;
  while i<80 do begin
    inc(E,LRot_5(A) + (B xor C xor D) + W[i  ] + K4); B := LRot_30(B);
    inc(D,LRot_5(E) + (A xor B xor C) + W[i+1] + K4); A := LRot_30(A);
    inc(C,LRot_5(D) + (E xor A xor B) + W[i+2] + K4); E := LRot_30(E);
    inc(B,LRot_5(C) + (D xor E xor A) + W[i+3] + K4); D := LRot_30(D);
    inc(A,LRot_5(B) + (C xor D xor E) + W[i+4] + K4); C := LRot_30(C);
    inc(i,5);
  end;

  {Calculate new working hash}
  inc(Data.Hash[0], A);
  inc(Data.Hash[1], B);
  inc(Data.Hash[2], C);
  inc(Data.Hash[3], D);
  inc(Data.Hash[4], E);

end;

{$endif BASM16}

{$endif BIT16}



{---------------------------------------------------------------------------}
procedure SHA1Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context, buffer=0!!}
  fillchar(Context,sizeof(Context),0);
  with Context do begin
     Hash[0] := longint($67452301);
     Hash[1] := longint($EFCDAB89);
     Hash[2] := longint($98BADCFE);
     Hash[3] := longint($10325476);
     Hash[4] := longint($C3D2E1F0);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA1UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
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
    if Context.Index=SHA1_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      SHA1Compress(Context);
      while Len>=SHA1_BlockLen do begin
        move(Msg^,Context.Buffer,SHA1_BlockLen);
        SHA1Compress(Context);
        inc(Ptr2Inc(Msg),SHA1_BlockLen);
        dec(Len,SHA1_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA1Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA1UpdateXL(Context, Msg, Len);
end;



{---------------------------------------------------------------------------}
procedure SHA1FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}
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
    SHA1Compress(Context);
    fillchar(Context.Buffer,56,0);
  end;
  {Write 64 bit msg length into the last bits of the last block}
  {(in big endian format) and do a final compress}
  THashBuf32(Context.Buffer)[14] := RB(Context.MLen[1]);
  THashBuf32(Context.Buffer)[15] := RB(Context.MLen[0]);
  SHA1Compress(Context);
  {Hash->Digest to little endian format}
  fillchar(Digest, sizeof(Digest), 0);
  for i:=0 to 4 do THashDig32(Digest)[i]:= RB(Context.Hash[i]);
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure SHA1FinalBits(var Context: THashContext; var Digest: TSHA1Digest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA1FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA1FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA1 calculation, clear context}
begin
  SHA1FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA1Final(var Context: THashContext; var Digest: TSHA1Digest);
  {-finalize SHA1 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA1FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA1SelfTest: boolean;
  {-self test SHA1: compare with known value}
const
  s1: string[ 3] = 'abc';
  s2: string[56] = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  D1: TSHA1Digest= ($a9,$99,$3e,$36,$47,$06,$81,$6a,$ba,$3e,$25,$71,$78,$50,$c2,$6c,$9c,$d0,$d8,$9d);
  D2: TSHA1Digest= ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
  D3: TSHA1Digest= ($bb,$6b,$3e,$18,$f0,$11,$5b,$57,$92,$52,$41,$67,$6f,$5b,$1a,$e8,$87,$47,$b0,$8a);
  D4: TSHA1Digest= ($98,$23,$2a,$15,$34,$53,$14,$9a,$f8,$d5,$2a,$61,$50,$3a,$50,$74,$b8,$59,$70,$e8);
var
  Context: THashContext;
  Digest : TSHA1Digest;

  function SingleTest(s: Str127; TDig: TSHA1Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA1Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA1_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA1Init(Context);
    for i:=1 to length(s) do SHA1Update(Context,@s[i],1);
    SHA1Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA1_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  SHA1SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  SHA1Init(Context);
  SHA1FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@SHA1_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  SHA1Init(Context);
  SHA1FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@SHA1_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from SHA1 document}
  SHA1SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA1FullXL(var Digest: TSHA1Digest; Msg: pointer; Len: longint);
  {-SHA1 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA1Init(Context);
  SHA1UpdateXL(Context, Msg, Len);
  SHA1Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA1Full(var Digest: TSHA1Digest; Msg: pointer; Len: word);
  {-SHA1 of Msg with init/update/final}
begin
  SHA1FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA1File({$ifdef CONST} const {$endif} fname: string;
                   var Digest: TSHA1Digest; var buf; bsize: word; var Err: word);
  {-SHA1 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA1_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA1_Desc, sizeof(SHA1_Desc), 0);
    with SHA1_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA1_BlockLen;
       HDigestlen:= sizeof(TSHA1Digest);
       HInit     := SHA1Init;
       HFinal    := SHA1FinalEx;
       HUpdateXL := SHA1UpdateXL;
       HAlgNum   := longint(_SHA1);
       HName     := 'SHA1';
       HPtrOID   := @SHA1_OID;
       HLenOID   := 6;
       HFinalBit := SHA1FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA1, @SHA1_Desc);
end.
