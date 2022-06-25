unit MD4;

{MD4 - 128 bit Hash function}


interface

(*************************************************************************

 DESCRIPTION     :  MD4 - 128 bit Hash function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 1320 (http://tools.ietf.org/html/rfc1320)


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     18.02.07  W.Ehrhardt  Initial version based on MD5 layout
 0.11     18.02.07  we          Improved BASM16 with inline rol
 0.12     18.02.07  we          Improved other 16 bit with inline LROT32
 0.13     22.02.07  we          values for OID vector
 0.14     30.06.07  we          Use conditional define FPC_ProcVar
 0.15     04.10.07  we          FPC: {$asmmode intel}
 0.16     02.05.08  we          Bit-API: MD4FinalBits/Ex
 0.17     05.05.08  we          THashDesc constant with HFinalBit field
 0.18     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 0.19     25.04.09  we          updated RFC URL(s)
 0.20     26.12.12  we          D17 and PurePascal
 0.21     16.08.15  we          Removed $ifdef DLL / stdcall
 0.22     15.05.17  we          adjust OID to new MaxOIDLen
 0.23     29.11.17  we          MD4File - fname: string;

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2007-2017 Wolfgang Ehrhardt

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


procedure MD4Init(var Context: THashContext);
  {-initialize context}

procedure MD4Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure MD4UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure MD4Final(var Context: THashContext; var Digest: TMD4Digest);
  {-finalize MD4 calculation, clear context}

procedure MD4FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD4 calculation, clear context}

procedure MD4FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD4FinalBits(var Context: THashContext; var Digest: TMD4Digest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

function  MD4SelfTest: boolean;
  {-self test for string from MD4 document}

procedure MD4Full(var Digest: TMD4Digest; Msg: pointer; Len: word);
  {-MD4 of Msg with init/update/final}

procedure MD4FullXL(var Digest: TMD4Digest; Msg: pointer; Len: longint);
  {-MD4 of Msg with init/update/final}

procedure MD4File({$ifdef CONST} const {$endif} fname: string;
                  var Digest: TMD4Digest; var buf; bsize: word; var Err: word);
  {-MD4 of file, buf: buffer with at least bsize bytes}



implementation

{$ifdef BIT16}
  {$F-}
{$endif}

const
  MD4_BlockLen  = 64;


{1.2.840.113549.2.4}
{iso(1) member-body(2) us(840) rsadsi(113549) digestAlgorithm(2) md4(4)}
const
  MD4_OID : TOID_Vec = (1,2,840,113549,2,4,-1,-1,-1,-1,-1); {Len=6}

{$ifndef VER5X}
const
  MD4_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : MD4_BlockLen;
               HDigestlen: sizeof(TMD4Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @MD4Init;
               HFinal    : @MD4FinalEx;
               HUpdateXL : @MD4UpdateXL;
             {$else}
               HInit     : MD4Init;
               HFinal    : MD4FinalEx;
               HUpdateXL : MD4UpdateXL;
             {$endif}
               HAlgNum   : longint(_MD4);
               HName     : 'MD4';
               HPtrOID   : @MD4_OID;
               HLenOID   : 6;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @MD4FinalBitsEx;
             {$else}
               HFinalBit : MD4FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  MD4_Desc: THashDesc;
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
procedure MD4Transform(var Hash: THashState; const Buffer: THashBuf32);
  {-MD4 basic transformation}
var
  A, B, C, D: longint;
begin
  A := Hash[0];
  B := Hash[1];
  C := Hash[2];
  D := Hash[3];

  inc(A, (D xor (B and (C xor D))) + Buffer[ 0]); A := (A shl  3) or (A shr 29);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 1]); D := (D shl  7) or (D shr 25);
  inc(C, (B xor (D and (A xor B))) + Buffer[ 2]); C := (C shl 11) or (C shr 21);
  inc(B, (A xor (C and (D xor A))) + Buffer[ 3]); B := (B shl 19) or (B shr 13);
  inc(A, (D xor (B and (C xor D))) + Buffer[ 4]); A := (A shl  3) or (A shr 29);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 5]); D := (D shl  7) or (D shr 25);
  inc(C, (B xor (D and (A xor B))) + Buffer[ 6]); C := (C shl 11) or (C shr 21);
  inc(B, (A xor (C and (D xor A))) + Buffer[ 7]); B := (B shl 19) or (B shr 13);
  inc(A, (D xor (B and (C xor D))) + Buffer[ 8]); A := (A shl  3) or (A shr 29);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 9]); D := (D shl  7) or (D shr 25);
  inc(C, (B xor (D and (A xor B))) + Buffer[10]); C := (C shl 11) or (C shr 21);
  inc(B, (A xor (C and (D xor A))) + Buffer[11]); B := (B shl 19) or (B shr 13);
  inc(A, (D xor (B and (C xor D))) + Buffer[12]); A := (A shl  3) or (A shr 29);
  inc(D, (C xor (A and (B xor C))) + Buffer[13]); D := (D shl  7) or (D shr 25);
  inc(C, (B xor (D and (A xor B))) + Buffer[14]); C := (C shl 11) or (C shr 21);
  inc(B, (A xor (C and (D xor A))) + Buffer[15]); B := (B shl 19) or (B shr 13);

  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 0] + $5a827999); A := (A shl  3) or (A shr 29);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 4] + $5a827999); D := (D shl  5) or (D shr 27);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 8] + $5a827999); C := (C shl  9) or (C shr 23);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[12] + $5a827999); B := (B shl 13) or (B shr 19);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 1] + $5a827999); A := (A shl  3) or (A shr 29);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 5] + $5a827999); D := (D shl  5) or (D shr 27);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 9] + $5a827999); C := (C shl  9) or (C shr 23);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[13] + $5a827999); B := (B shl 13) or (B shr 19);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 2] + $5a827999); A := (A shl  3) or (A shr 29);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 6] + $5a827999); D := (D shl  5) or (D shr 27);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[10] + $5a827999); C := (C shl  9) or (C shr 23);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[14] + $5a827999); B := (B shl 13) or (B shr 19);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 3] + $5a827999); A := (A shl  3) or (A shr 29);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 7] + $5a827999); D := (D shl  5) or (D shr 27);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[11] + $5a827999); C := (C shl  9) or (C shr 23);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[15] + $5a827999); B := (B shl 13) or (B shr 19);

  inc(A, (B xor C xor D) + Buffer[ 0] + $6ed9eba1); A := (A shl  3) or (A shr 29);
  inc(D, (A xor B xor C) + Buffer[ 8] + $6ed9eba1); D := (D shl  9) or (D shr 23);
  inc(C, (D xor A xor B) + Buffer[ 4] + $6ed9eba1); C := (C shl 11) or (C shr 21);
  inc(B, (C xor D xor A) + Buffer[12] + $6ed9eba1); B := (B shl 15) or (B shr 17);
  inc(A, (B xor C xor D) + Buffer[ 2] + $6ed9eba1); A := (A shl  3) or (A shr 29);
  inc(D, (A xor B xor C) + Buffer[10] + $6ed9eba1); D := (D shl  9) or (D shr 23);
  inc(C, (D xor A xor B) + Buffer[ 6] + $6ed9eba1); C := (C shl 11) or (C shr 21);
  inc(B, (C xor D xor A) + Buffer[14] + $6ed9eba1); B := (B shl 15) or (B shr 17);
  inc(A, (B xor C xor D) + Buffer[ 1] + $6ed9eba1); A := (A shl  3) or (A shr 29);
  inc(D, (A xor B xor C) + Buffer[ 9] + $6ed9eba1); D := (D shl  9) or (D shr 23);
  inc(C, (D xor A xor B) + Buffer[ 5] + $6ed9eba1); C := (C shl 11) or (C shr 21);
  inc(B, (C xor D xor A) + Buffer[13] + $6ed9eba1); B := (B shl 15) or (B shr 17);
  inc(A, (B xor C xor D) + Buffer[ 3] + $6ed9eba1); A := (A shl  3) or (A shr 29);
  inc(D, (A xor B xor C) + Buffer[11] + $6ed9eba1); D := (D shl  9) or (D shr 23);
  inc(C, (D xor A xor B) + Buffer[ 7] + $6ed9eba1); C := (C shl 11) or (C shr 21);
  inc(B, (C xor D xor A) + Buffer[15] + $6ed9eba1); B := (B shl 15) or (B shr 17);

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
procedure MD4Transform(var Hash: THashState; {$ifdef CONST} const {$else} var {$endif} Buffer: THashBuf32);
  {-MD4 basic transformation}
var
  A, B, C, D: longint;
begin
  A := Hash[0];
  B := Hash[1];
  C := Hash[2];
  D := Hash[3];

  inc(A, (D xor (B and (C xor D))) + Buffer[ 0]); asm db $66; rol word(A), 3 end;
  inc(D, (C xor (A and (B xor C))) + Buffer[ 1]); asm db $66; rol word(D), 7 end;
  inc(C, (B xor (D and (A xor B))) + Buffer[ 2]); asm db $66; rol word(C),11 end;
  inc(B, (A xor (C and (D xor A))) + Buffer[ 3]); asm db $66; rol word(B),19 end;
  inc(A, (D xor (B and (C xor D))) + Buffer[ 4]); asm db $66; rol word(A), 3 end;
  inc(D, (C xor (A and (B xor C))) + Buffer[ 5]); asm db $66; rol word(D), 7 end;
  inc(C, (B xor (D and (A xor B))) + Buffer[ 6]); asm db $66; rol word(C),11 end;
  inc(B, (A xor (C and (D xor A))) + Buffer[ 7]); asm db $66; rol word(B),19 end;
  inc(A, (D xor (B and (C xor D))) + Buffer[ 8]); asm db $66; rol word(A), 3 end;
  inc(D, (C xor (A and (B xor C))) + Buffer[ 9]); asm db $66; rol word(D), 7 end;
  inc(C, (B xor (D and (A xor B))) + Buffer[10]); asm db $66; rol word(C),11 end;
  inc(B, (A xor (C and (D xor A))) + Buffer[11]); asm db $66; rol word(B),19 end;
  inc(A, (D xor (B and (C xor D))) + Buffer[12]); asm db $66; rol word(A), 3 end;
  inc(D, (C xor (A and (B xor C))) + Buffer[13]); asm db $66; rol word(D), 7 end;
  inc(C, (B xor (D and (A xor B))) + Buffer[14]); asm db $66; rol word(C),11 end;
  inc(B, (A xor (C and (D xor A))) + Buffer[15]); asm db $66; rol word(B),19 end;

  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 0] + $5a827999); asm db $66; rol word(A), 3 end;
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 4] + $5a827999); asm db $66; rol word(D), 5 end;
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 8] + $5a827999); asm db $66; rol word(C), 9 end;
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[12] + $5a827999); asm db $66; rol word(B),13 end;
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 1] + $5a827999); asm db $66; rol word(A), 3 end;
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 5] + $5a827999); asm db $66; rol word(D), 5 end;
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 9] + $5a827999); asm db $66; rol word(C), 9 end;
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[13] + $5a827999); asm db $66; rol word(B),13 end;
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 2] + $5a827999); asm db $66; rol word(A), 3 end;
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 6] + $5a827999); asm db $66; rol word(D), 5 end;
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[10] + $5a827999); asm db $66; rol word(C), 9 end;
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[14] + $5a827999); asm db $66; rol word(B),13 end;
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 3] + $5a827999); asm db $66; rol word(A), 3 end;
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 7] + $5a827999); asm db $66; rol word(D), 5 end;
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[11] + $5a827999); asm db $66; rol word(C), 9 end;
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[15] + $5a827999); asm db $66; rol word(B),13 end;

  inc(A, (B xor C xor D) + Buffer[ 0] + $6ed9eba1); asm db $66; rol word(A), 3 end;
  inc(D, (A xor B xor C) + Buffer[ 8] + $6ed9eba1); asm db $66; rol word(D), 9 end;
  inc(C, (D xor A xor B) + Buffer[ 4] + $6ed9eba1); asm db $66; rol word(C),11 end;
  inc(B, (C xor D xor A) + Buffer[12] + $6ed9eba1); asm db $66; rol word(B),15 end;
  inc(A, (B xor C xor D) + Buffer[ 2] + $6ed9eba1); asm db $66; rol word(A), 3 end;
  inc(D, (A xor B xor C) + Buffer[10] + $6ed9eba1); asm db $66; rol word(D), 9 end;
  inc(C, (D xor A xor B) + Buffer[ 6] + $6ed9eba1); asm db $66; rol word(C),11 end;
  inc(B, (C xor D xor A) + Buffer[14] + $6ed9eba1); asm db $66; rol word(B),15 end;
  inc(A, (B xor C xor D) + Buffer[ 1] + $6ed9eba1); asm db $66; rol word(A), 3 end;
  inc(D, (A xor B xor C) + Buffer[ 9] + $6ed9eba1); asm db $66; rol word(D), 9 end;
  inc(C, (D xor A xor B) + Buffer[ 5] + $6ed9eba1); asm db $66; rol word(C),11 end;
  inc(B, (C xor D xor A) + Buffer[13] + $6ed9eba1); asm db $66; rol word(B),15 end;
  inc(A, (B xor C xor D) + Buffer[ 3] + $6ed9eba1); asm db $66; rol word(A), 3 end;
  inc(D, (A xor B xor C) + Buffer[11] + $6ed9eba1); asm db $66; rol word(D), 9 end;
  inc(C, (D xor A xor B) + Buffer[ 7] + $6ed9eba1); asm db $66; rol word(C),11 end;
  inc(B, (C xor D xor A) + Buffer[15] + $6ed9eba1); asm db $66; rol word(B),15 end;

  inc(Hash[0], A);
  inc(Hash[1], B);
  inc(Hash[2], C);
  inc(Hash[3], D);
end;


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


{---------------------------------------------------------------------------}
procedure MD4Transform(var Hash: THashState; {$ifdef CONST} const {$else} var {$endif} Buffer: THashBuf32);
  {-MD4 basic transformation}
var
  A, B, C, D: longint;
begin
  A := Hash[0];
  B := Hash[1];
  C := Hash[2];
  D := Hash[3];

  inc(A, (D xor (B and (C xor D))) + Buffer[ 0]); A := LRot32(A, 3);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 1]); D := LRot32(D, 7);
  inc(C, (B xor (D and (A xor B))) + Buffer[ 2]); C := LRot32(C,11);
  inc(B, (A xor (C and (D xor A))) + Buffer[ 3]); B := LRot32(B,19);
  inc(A, (D xor (B and (C xor D))) + Buffer[ 4]); A := LRot32(A, 3);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 5]); D := LRot32(D, 7);
  inc(C, (B xor (D and (A xor B))) + Buffer[ 6]); C := LRot32(C,11);
  inc(B, (A xor (C and (D xor A))) + Buffer[ 7]); B := LRot32(B,19);
  inc(A, (D xor (B and (C xor D))) + Buffer[ 8]); A := LRot32(A, 3);
  inc(D, (C xor (A and (B xor C))) + Buffer[ 9]); D := LRot32(D, 7);
  inc(C, (B xor (D and (A xor B))) + Buffer[10]); C := LRot32(C,11);
  inc(B, (A xor (C and (D xor A))) + Buffer[11]); B := LRot32(B,19);
  inc(A, (D xor (B and (C xor D))) + Buffer[12]); A := LRot32(A, 3);
  inc(D, (C xor (A and (B xor C))) + Buffer[13]); D := LRot32(D, 7);
  inc(C, (B xor (D and (A xor B))) + Buffer[14]); C := LRot32(C,11);
  inc(B, (A xor (C and (D xor A))) + Buffer[15]); B := LRot32(B,19);

  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 0] + $5a827999); A := LRot32(A, 3);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 4] + $5a827999); D := LRot32(D, 5);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 8] + $5a827999); C := LRot32(C, 9);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[12] + $5a827999); B := LRot32(B,13);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 1] + $5a827999); A := LRot32(A, 3);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 5] + $5a827999); D := LRot32(D, 5);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[ 9] + $5a827999); C := LRot32(C, 9);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[13] + $5a827999); B := LRot32(B,13);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 2] + $5a827999); A := LRot32(A, 3);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 6] + $5a827999); D := LRot32(D, 5);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[10] + $5a827999); C := LRot32(C, 9);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[14] + $5a827999); B := LRot32(B,13);
  inc(A, ((B and C) or (B and D) or (C and D)) + Buffer[ 3] + $5a827999); A := LRot32(A, 3);
  inc(D, ((A and B) or (A and C) or (B and C)) + Buffer[ 7] + $5a827999); D := LRot32(D, 5);
  inc(C, ((D and A) or (D and B) or (A and B)) + Buffer[11] + $5a827999); C := LRot32(C, 9);
  inc(B, ((C and D) or (C and A) or (D and A)) + Buffer[15] + $5a827999); B := LRot32(B,13);

  inc(A, (B xor C xor D) + Buffer[ 0] + $6ed9eba1); A := LRot32(A, 3);
  inc(D, (A xor B xor C) + Buffer[ 8] + $6ed9eba1); D := LRot32(D, 9);
  inc(C, (D xor A xor B) + Buffer[ 4] + $6ed9eba1); C := LRot32(C,11);
  inc(B, (C xor D xor A) + Buffer[12] + $6ed9eba1); B := LRot32(B,15);
  inc(A, (B xor C xor D) + Buffer[ 2] + $6ed9eba1); A := LRot32(A, 3);
  inc(D, (A xor B xor C) + Buffer[10] + $6ed9eba1); D := LRot32(D, 9);
  inc(C, (D xor A xor B) + Buffer[ 6] + $6ed9eba1); C := LRot32(C,11);
  inc(B, (C xor D xor A) + Buffer[14] + $6ed9eba1); B := LRot32(B,15);
  inc(A, (B xor C xor D) + Buffer[ 1] + $6ed9eba1); A := LRot32(A, 3);
  inc(D, (A xor B xor C) + Buffer[ 9] + $6ed9eba1); D := LRot32(D, 9);
  inc(C, (D xor A xor B) + Buffer[ 5] + $6ed9eba1); C := LRot32(C,11);
  inc(B, (C xor D xor A) + Buffer[13] + $6ed9eba1); B := LRot32(B,15);
  inc(A, (B xor C xor D) + Buffer[ 3] + $6ed9eba1); A := LRot32(A, 3);
  inc(D, (A xor B xor C) + Buffer[11] + $6ed9eba1); D := LRot32(D, 9);
  inc(C, (D xor A xor B) + Buffer[ 7] + $6ed9eba1); C := LRot32(C,11);
  inc(B, (C xor D xor A) + Buffer[15] + $6ed9eba1); B := LRot32(B,15);

  inc(Hash[0], A);
  inc(Hash[1], B);
  inc(Hash[2], C);
  inc(Hash[3], D);
end;


{$endif BASM16}

{$endif}


{---------------------------------------------------------------------------}
procedure MD4Init(var Context: THashContext);
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
procedure MD4UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
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
    if Context.Index=MD4_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      MD4Transform(Context.Hash, THashBuf32(Context.Buffer));
      while Len>=MD4_BlockLen do begin
        MD4Transform(Context.Hash, THashBuf32(Msg^));
        inc(Ptr2Inc(Msg),MD4_BlockLen);
        dec(Len,MD4_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure MD4Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  MD4UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure MD4FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}
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
    MD4Transform(Context.Hash, THashBuf32(Context.Buffer));
    fillchar(Context.Buffer,56,0);
  end;
  {Write 64 bit msg length into the last bits of the last block}
  {and do a final compress}
  THashBuf32(Context.Buffer)[14] := Context.MLen[0];
  THashBuf32(Context.Buffer)[15] := Context.MLen[1];
  MD4Transform(Context.Hash, THashBuf32(Context.Buffer));
  {Hash to Digest}
  Move(Context.Hash,Digest,sizeof(Digest));
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure MD4FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD4 calculation, clear context}
begin
  MD4FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure MD4FinalBits(var Context: THashContext; var Digest: TMD4Digest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  MD4FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure MD4Final(var Context: THashContext; var Digest: TMD4Digest);
  {-finalize MD4 calculation, clear context}
var
  tmp: THashDigest;
begin
  MD4FinalBitsEx(Context,tmp,0,0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function MD4SelfTest: boolean;
  {-self test for string from MD4 document}
const
  s1: string[ 3] = 'abc';
  s2: string[62] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  D1: TMD4Digest = ($a4,$48,$01,$7a,$af,$21,$d8,$52,$5f,$c1,$0a,$e8,$7a,$a6,$72,$9d);
  D2: TMD4Digest = ($04,$3f,$85,$82,$f2,$41,$db,$35,$1c,$e6,$27,$e1,$53,$e7,$f0,$e4);
  D3: TMD4Digest = ($8d,$62,$ec,$bf,$6f,$fb,$c4,$9d,$ec,$08,$bb,$4c,$53,$71,$89,$bb);
var
  Context: THashContext;
  Digest : TMD4Digest;

  function SingleTest(s: Str127; TDig: TMD4Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    MD4Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@MD4_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    MD4Init(Context);
    for i:=1 to length(s) do MD4Update(Context,@s[i],1);
    MD4Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@MD4_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  MD4SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  MD4Init(Context);
  MD4FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@MD4_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {strings from MD4 document}
  MD4SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure MD4FullXL(var Digest: TMD4Digest; Msg: pointer; Len: longint);
  {-MD4 of Msg with init/update/final}
var
  Context: THashContext;
begin
  MD4Init(Context);
  MD4UpdateXL(Context, Msg, Len);
  MD4Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure MD4Full(var Digest: TMD4Digest; Msg: pointer; Len: word);
  {-MD4 of Msg with init/update/final}
begin
  MD4FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure MD4File({$ifdef CONST} const {$endif} fname: string;
                  var Digest: TMD4Digest; var buf; bsize: word; var Err: word);
  {-MD4 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @MD4_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(MD4_Desc, sizeof(MD4_Desc), 0);
    with MD4_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := MD4_BlockLen;
       HDigestlen:= sizeof(TMD4Digest);
       HInit     := MD4Init;
       HFinal    := MD4FinalEx;
       HUpdateXL := MD4UpdateXL;
       HAlgNum   := longint(_MD4);
       HName     := 'MD4';
       HPtrOID   := @MD4_OID;
       HLenOID   := 6;
       HFinalBit := MD4FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_MD4, @MD4_Desc);
end.
