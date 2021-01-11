unit MD5;

{MD5 - 128 bit Hash function}


interface

(*************************************************************************

 DESCRIPTION     :  MD5 - 128 bit Hash function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 1321 (http://tools.ietf.org/html/rfc1321)


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.03.02  W.Ehrhardt  Initial version based on SHA1 layout
 0.20     16.03.02  we          Basic trans: 4 func calls in a loop
 0.21     25.01.03  we          removed const in MD5Transform (TP6)
 0.30     23.07.03  we          With MD5File, MD5Full
 0.31     26.07.03  we          With MD5Full in self test, D6+ - warnings
 2.00     26.07.03  we          common vers., longint for word32, D4+ - warnings
 2.01     03.08.03  we          type TMD5Block for HMAC
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 2.50     17.11.03  we          Speedup in update, don't clear W in compress
 2.51     17.11.03  we          MD5Transform(hash, msg^)
 2.52     17.11.03  we          Full unroll
 2.53     18.11.03  we          16 bit: partial unroll, 32 bit: full unroll via ifdef
 2.54     18.11.03  we          LRot32 mit inline()
 2.55     20.11.03  we          Full range UpdateLen
 3.00     01.12.03  we          Common version 3.0
 3.01     24.12.03  we          TP5/5.5: LRot inline
 3.02     24.12.03  we          Changed UpdateLen: Definition and TP5/5.5 inline
 3.03     26.02.05  we          With {$ifdef StrictLong}
 3.04     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.05     17.12.05  we          Force $I- in MD5File
 3.06     15.01.06  we          uses Hash unit and THashDesc
 3.07     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.08     22.01.06  we          Removed HSelfTest from descriptor
 3.09     11.02.06  we          Descriptor as typed const
 3.10     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.11     22.02.07  we          values for OID vector
 3.12     30.06.07  we          Use conditional define FPC_ProcVar
 3.13     04.10.07  we          FPC: {$asmmode intel}
 3.14     02.05.08  we          Bit-API: MD5FinalBits/Ex
 3.15     05.05.08  we          THashDesc constant with HFinalBit field
 3.16     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 3.17     25.04.09  we          updated RFC URL(s)
 3.18     26.12.12  we          D17 and PurePascal
 3.19     16.08.15  we          Removed $ifdef DLL / stdcall
 3.20     15.05.17  we          adjust OID to new MaxOIDLen
 3.21     29.11.17  we          MD5File - fname: string

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


{$i STD.INC}

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

uses
  BTypes,Hash;


procedure MD5Init(var Context: THashContext);
  {-initialize context}

procedure MD5Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure MD5UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure MD5Final(var Context: THashContext; var Digest: TMD5Digest);
  {-finalize MD5 calculation, clear context}

procedure MD5FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD5 calculation, clear context}

procedure MD5FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD5FinalBits(var Context: THashContext; var Digest: TMD5Digest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

function  MD5SelfTest: boolean;
  {-self test for string from MD5 document}

procedure MD5Full(var Digest: TMD5Digest; Msg: pointer; Len: word);
  {-MD5 of Msg with init/update/final}

procedure MD5FullXL(var Digest: TMD5Digest; Msg: pointer; Len: longint);
  {-MD5 of Msg with init/update/final}

procedure MD5File({$ifdef CONST} const {$endif} fname: string;
                  var Digest: TMD5Digest; var buf; bsize: word; var Err: word);
  {-MD5 of file, buf: buffer with at least bsize bytes}



implementation


{$ifdef BIT16}
  {$F-}
{$endif}


const
  MD5_BlockLen  = 64;


{1.2.840.113549.2.5}
{iso(1) member-body(2) us(840) rsadsi(113549) digestAlgorithm(2) md5(5)}
const
  MD5_OID : TOID_Vec = (1,2,840,113549,2,5,-1,-1,-1,-1,-1); {Len=6}


{$ifndef VER5X}
const
  MD5_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : MD5_BlockLen;
               HDigestlen: sizeof(TMD5Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @MD5Init;
               HFinal    : @MD5FinalEx;
               HUpdateXL : @MD5UpdateXL;
             {$else}
               HInit     : MD5Init;
               HFinal    : MD5FinalEx;
               HUpdateXL : MD5UpdateXL;
             {$endif}
               HAlgNum   : longint(_MD5);
               HName     : 'MD5';
               HPtrOID   : @MD5_OID;
               HLenOID   : 6;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @MD5FinalBitsEx;
             {$else}
               HFinalBit : MD5FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  MD5_Desc: THashDesc;
{$endif}


{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

const
  t: array[0..63] of longint = ($d76aa478, $e8c7b756, $242070db, $c1bdceee,
                                $f57c0faf, $4787c62a, $a8304613, $fd469501,
                                $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
                                $6b901122, $fd987193, $a679438e, $49b40821,
                                $f61e2562, $c040b340, $265e5a51, $e9b6c7aa,
                                $d62f105d, $02441453, $d8a1e681, $e7d3fbc8,
                                $21e1cde6, $c33707d6, $f4d50d87, $455a14ed,
                                $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a,
                                $fffa3942, $8771f681, $6d9d6122, $fde5380c,
                                $a4beea44, $4bdecfa9, $f6bb4b60, $bebfbc70,
                                $289b7ec6, $eaa127fa, $d4ef3085, $04881d05,
                                $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
                                $f4292244, $432aff97, $ab9423a7, $fc93a039,
                                $655b59c3, $8f0ccc92, $ffeff47d, $85845dd1,
                                $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
                                $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391);

{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}






{$ifndef BIT16}


{********* 32+ bit code *********}

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
{$endif}

{---------------------------------------------------------------------------}
procedure MD5Transform(var Hash: THashState; const Buffer: THashBuf32);
  {-MD5 basic transformation}
var
  A, B, C, D: longint;
begin
  A := Hash[0];
  B := Hash[1];
  C := Hash[2];
  D := Hash[3];

  inc(A, Buffer[ 0] + T[ 0] + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  inc(D, Buffer[ 1] + T[ 1] + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  inc(C, Buffer[ 2] + T[ 2] + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  inc(B, Buffer[ 3] + T[ 3] + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  inc(A, Buffer[ 4] + T[ 4] + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  inc(D, Buffer[ 5] + T[ 5] + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  inc(C, Buffer[ 6] + T[ 6] + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  inc(B, Buffer[ 7] + T[ 7] + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  inc(A, Buffer[ 8] + T[ 8] + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  inc(D, Buffer[ 9] + T[ 9] + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  inc(C, Buffer[10] + T[10] + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  inc(B, Buffer[11] + T[11] + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  inc(A, Buffer[12] + T[12] + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  inc(D, Buffer[13] + T[13] + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  inc(C, Buffer[14] + T[14] + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  inc(B, Buffer[15] + T[15] + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  inc(A, Buffer[ 1] + T[16] + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  inc(D, Buffer[ 6] + T[17] + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  inc(C, Buffer[11] + T[18] + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  inc(B, Buffer[ 0] + T[19] + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  inc(A, Buffer[ 5] + T[20] + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  inc(D, Buffer[10] + T[21] + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  inc(C, Buffer[15] + T[22] + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  inc(B, Buffer[ 4] + T[23] + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  inc(A, Buffer[ 9] + T[24] + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  inc(D, Buffer[14] + T[25] + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  inc(C, Buffer[ 3] + T[26] + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  inc(B, Buffer[ 8] + T[27] + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  inc(A, Buffer[13] + T[28] + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  inc(D, Buffer[ 2] + T[29] + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  inc(C, Buffer[ 7] + T[30] + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  inc(B, Buffer[12] + T[31] + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  inc(A, Buffer[ 5] + T[32] + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  inc(D, Buffer[ 8] + T[33] + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  inc(C, Buffer[11] + T[34] + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  inc(B, Buffer[14] + T[35] + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  inc(A, Buffer[ 1] + T[36] + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  inc(D, Buffer[ 4] + T[37] + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  inc(C, Buffer[ 7] + T[38] + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  inc(B, Buffer[10] + T[39] + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  inc(A, Buffer[13] + T[40] + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  inc(D, Buffer[ 0] + T[41] + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  inc(C, Buffer[ 3] + T[42] + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  inc(B, Buffer[ 6] + T[43] + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  inc(A, Buffer[ 9] + T[44] + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  inc(D, Buffer[12] + T[45] + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  inc(C, Buffer[15] + T[46] + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  inc(B, Buffer[ 2] + T[47] + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  inc(A, Buffer[ 0] + T[48] + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  inc(D, Buffer[ 7] + T[49] + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  inc(C, Buffer[14] + T[50] + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  inc(B, Buffer[ 5] + T[51] + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  inc(A, Buffer[12] + T[52] + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  inc(D, Buffer[ 3] + T[53] + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  inc(C, Buffer[10] + T[54] + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  inc(B, Buffer[ 1] + T[55] + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  inc(A, Buffer[ 8] + T[56] + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  inc(D, Buffer[15] + T[57] + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  inc(C, Buffer[ 6] + T[58] + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  inc(B, Buffer[13] + T[59] + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  inc(A, Buffer[ 4] + T[60] + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  inc(D, Buffer[11] + T[61] + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  inc(C, Buffer[ 2] + T[62] + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  inc(B, Buffer[ 9] + T[63] + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  inc(Hash[0], A);
  inc(Hash[1], B);
  inc(Hash[2], C);
  inc(Hash[3], D);
end;


{$else}

{********* 16 bit code *********}

{$ifdef BASM16}

(** TP6-7/D1 **)

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
function LRot32(X: longint; c: word): longint;
inline(
  $59/              {pop    cx     }
  $66/$58/          {pop    eax    }
  $66/$D3/$C0/      {rol    eax,cl }
  $66/$8B/$D0/      {mov    edx,eax}
  $66/$C1/$EA/$10); {shr    edx,16 }

{$else}

{** T5/5.5 **}

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
function LRot32(X: longint; c: word): longint;
  {-Rotate left}
inline(
  $59/           {  pop    cx    }
  $58/           {  pop    ax    }
  $5A/           {  pop    dx    }

  $83/$F9/$10/   {  cmp    cx,16 }
  $72/$06/       {  jb     L     }
  $92/           {  xchg   dx,ax }
  $83/$E9/$10/   {  sub    cx,16 }
  $74/$09/       {  je     X     }

  $2B/$DB/       {S:sub    bx,bx }
  $D1/$D0/       {L:rcl    ax,1  }
  $D1/$D2/       {  rcl    dx,1  }
  $13/$C3/       {  adc    ax,bx }
  $49/           {  dec    cx    }
  $75/$F7);      {  jne    L     }
                 {X:             }

{$endif BASM16}

const
  {buffer index in loop}
  PX: array[0..63] of word =   ( 0,  1,  2,  3,  4,  5,  6,  7,
                                 8,  9, 10, 11, 12, 13, 14, 15,
                                 1,  6, 11,  0,  5, 10, 15,  4,
                                 9, 14,  3,  8, 13,  2,  7, 12,
                                 5,  8, 11, 14,  1,  4,  7, 10,
                                13,  0,  3,  6,  9, 12, 15,  2,
                                 0,  7, 14,  5, 12,  3, 10,  1,
                                 8, 15,  6, 13,  4, 11,  2,  9);


{---------------------------------------------------------------------------}
procedure MD5Transform(var Hash: THashState; {$ifdef CONST} const {$else} var {$endif} Buffer: THashBuf32);
  {-MD5 basic transformation, 4 function calls in loop}
var
  A, B, C, D: longint;
  i,k: integer;
begin
  A := Hash[0];
  B := Hash[1];
  C := Hash[2];
  D := Hash[3];
  k := 0;
  {16 bit code: 4 functions inline, rotateleft via function}
  for i:=0 to 3 do begin
    inc(A, Buffer[k] + T[k] + (D xor (B and (C xor D)))); A := LRot32(A,  7) + B; inc(k);
    inc(D, Buffer[k] + T[k] + (C xor (A and (B xor C)))); D := LRot32(D, 12) + A; inc(k);
    inc(C, Buffer[k] + T[k] + (B xor (D and (A xor B)))); C := LRot32(C, 17) + D; inc(k);
    inc(B, Buffer[k] + T[k] + (A xor (C and (D xor A)))); B := LRot32(B, 22) + C; inc(k);
  end;
  for i:=0 to 3 do begin
    inc(A, Buffer[PX[k]] + T[k] + (C xor (D and (B xor C)))); A := LRot32(A,  5) + B; inc(k);
    inc(D, Buffer[PX[k]] + T[k] + (B xor (C and (A xor B)))); D := LRot32(D,  9) + A; inc(k);
    inc(C, Buffer[PX[k]] + T[k] + (A xor (B and (D xor A)))); C := LRot32(C, 14) + D; inc(k);
    inc(B, Buffer[PX[k]] + T[k] + (D xor (A and (C xor D)))); B := LRot32(B, 20) + C; inc(k);
  end;
  for i:=0 to 3 do begin
    inc(A, Buffer[PX[k]] + T[k] + (B xor C xor D)); A := LRot32(A,  4) + B; inc(k);
    inc(D, Buffer[PX[k]] + T[k] + (A xor B xor C)); D := LRot32(D, 11) + A; inc(k);
    inc(C, Buffer[PX[k]] + T[k] + (D xor A xor B)); C := LRot32(C, 16) + D; inc(k);
    inc(B, Buffer[PX[k]] + T[k] + (C xor D xor A)); B := LRot32(B, 23) + C; inc(k);
  end;
  for i:=0 to 3 do begin
    inc(A, Buffer[PX[k]] + T[k] + (C xor (B or not D))); A := LRot32(A,  6) + B; inc(k);
    inc(D, Buffer[PX[k]] + T[k] + (B xor (A or not C))); D := LRot32(D, 10) + A; inc(k);
    inc(C, Buffer[PX[k]] + T[k] + (A xor (D or not B))); C := LRot32(C, 15) + D; inc(k);
    inc(B, Buffer[PX[k]] + T[k] + (D xor (C or not A))); B := LRot32(B, 21) + C; inc(k);
  end;
  inc(Hash[0], A);
  inc(Hash[1], B);
  inc(Hash[2], C);
  inc(Hash[3], D);
end;


{$endif}


{---------------------------------------------------------------------------}
procedure MD5Init(var Context: THashContext);
  {-initialize context}
begin
  {-initialize context and buffer to 0}
  fillchar(Context,sizeof(Context),0);
  with Context do begin
     Hash[0] := longint($67452301);
     Hash[1] := longint($EFCDAB89);
     Hash[2] := longint($98BADCFE);
     Hash[3] := longint($10325476);
  end;
end;


{---------------------------------------------------------------------------}
procedure MD5UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
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
    if Context.Index=MD5_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      MD5Transform(Context.Hash, THashBuf32(Context.Buffer));
      while Len>=MD5_BlockLen do begin
        MD5Transform(Context.Hash, THashBuf32(Msg^));
        inc(Ptr2Inc(Msg),MD5_BlockLen);
        dec(Len,MD5_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure MD5Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  MD5UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure MD5FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}
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
    MD5Transform(Context.Hash, THashBuf32(Context.Buffer));
    fillchar(Context.Buffer,56,0);
  end;
  {Write 64 bit msg length into the last bits of the last block}
  {and do a final compress}
  THashBuf32(Context.Buffer)[14] := Context.MLen[0];
  THashBuf32(Context.Buffer)[15] := Context.MLen[1];
  MD5Transform(Context.Hash, THashBuf32(Context.Buffer));
  {Hash to Digest}
  Move(Context.Hash,Digest,sizeof(Digest));
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure MD5FinalBits(var Context: THashContext; var Digest: TMD5Digest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  MD5FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure MD5FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD5 calculation, clear context}
begin
  MD5FinalBitsEx(Context,Digest,0,0);
end;

{---------------------------------------------------------------------------}
procedure MD5Final(var Context: THashContext; var Digest: TMD5Digest);
  {-finalize MD5 calculation, clear context}
var
  tmp: THashDigest;
begin
  MD5FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function MD5SelfTest: boolean;
  {-self test for string from MD5 document}
const
  s1: string[ 3] = 'abc';
  s2: string[62] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  D1: TMD5Digest= ($90,$01,$50,$98,$3c,$d2,$4f,$b0,$d6,$96,$3f,$7d,$28,$e1,$7f,$72);
  D2: TMD5Digest= ($d1,$74,$ab,$98,$d2,$77,$d9,$f5,$a5,$61,$1c,$2c,$9f,$41,$9d,$9f);
  D3: TMD5Digest= ($1d,$a6,$35,$b1,$43,$0f,$17,$1c,$65,$72,$06,$fd,$69,$fe,$e0,$e8);
var
  Context: THashContext;
  Digest : TMD5Digest;

  function SingleTest(s: Str255; TDig: TMD5Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    MD5Full(Digest,@s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@MD5_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    MD5Init(Context);
    for i:=1 to length(s) do MD5Update(Context,@s[i],1);
    MD5Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@MD5_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  MD5SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  MD5Init(Context);
  MD5FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@MD5_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {strings from MD5 document}
  MD5SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure MD5FullXL(var Digest: TMD5Digest; Msg: pointer; Len: longint);
  {-MD5 of Msg with init/update/final}
var
  Context: THashContext;
begin
  MD5Init(Context);
  MD5UpdateXL(Context, Msg, Len);
  MD5Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure MD5Full(var Digest: TMD5Digest; Msg: pointer; Len: word);
  {-MD5 of Msg with init/update/final}
begin
  MD5FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure MD5File({$ifdef CONST} const {$endif} fname: string;
                  var Digest: TMD5Digest; var buf; bsize: word; var Err: word);
  {-MD5 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @MD5_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(MD5_Desc, sizeof(MD5_Desc), 0);
    with MD5_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := MD5_BlockLen;
       HDigestlen:= sizeof(TMD5Digest);
       HInit     := MD5Init;
       HFinal    := MD5FinalEx;
       HUpdateXL := MD5UpdateXL;
       HAlgNum   := longint(_MD5);
       HName     := 'MD5';
       HPtrOID   := @MD5_OID;
       HLenOID   := 6;
       HFinalBit := MD5FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_MD5, @MD5_Desc);
end.
