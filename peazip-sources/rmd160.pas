unit RMD160;

{RIPEMD-160 - 160 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  RIPEMD-160 - 160 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - H.Dobbertin, A.Bosselaers, B.Preneel: RIPEMD-160, a strengthened version of RIPEMD,
                      from http://www.esat.kuleuven.ac.be/~cosicart/pdf/AB-9601/AB-9601.pdf
                    - A.Bosselaers' page at http://homes.esat.kuleuven.be/~bosselae/ripemd160.html
                    - Crypto++: http://www.eskimo.com/~weidai/cryptlib.html


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     31.01.06  W.Ehrhardt  Initial version based on SHA1 layout
 0.11     31.01.06  we          BASM16 and TP5/5.5
 0.12     31.01.06  we          Compress with 2 parameters, no local copy for BIT32
 0.13     01.02.06  we          RL10 for TP5.x
 0.14     01.02.06  we          Speedup for Delphi32 by factor 2.5
 0.15     11.02.06  we          Descriptor as typed const
 0.16     28.03.06  we          Removed $ifdef StrictLong
 0.17     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 0.18     22.02.07  we          values for OID vector
 0.19     30.06.07  we          Use conditional define FPC_ProcVar
 0.20     04.10.07  we          FPC: {$asmmode intel}
 0.21     03.05.08  we          Bit-API: RMD160FinalBits/Ex
 0.22     05.05.08  we          THashDesc constant with HFinalBit field
 0.23     12.05.08  we          Test vectors for regression testing
 0.24     20.05.08  we          Undo change 'RMD160' instead of 'RIPEMD160'
 0.25     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 0.26     26.12.12  we          D17 and PurePascal
 0.27     16.08.15  we          Removed $ifdef DLL / stdcall
 0.28     28.03.17  we          No '$asmmode intel' for CPUARM
 0.29     15.05.17  we          adjust OID to new MaxOIDLen
 0.30     29.11.17  we          RMD160File - fname: string
 0.31     17.01.21  gt          CPUAARCH64 disable Intel ASM

**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006-2017 Wolfgang Ehrhardt

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


procedure RMD160Init(var Context: THashContext);
  {-initialize context}

procedure RMD160Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure RMD160UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure RMD160Final(var Context: THashContext; var Digest: TRMD160Digest);
  {-finalize RMD160 calculation, clear context}

procedure RMD160FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize RMD160 calculation, clear context}

procedure RMD160FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

procedure RMD160FinalBits(var Context: THashContext; var Digest: TRMD160Digest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

function  RMD160SelfTest: boolean;
  {-self test for strings from RMD160 page}

procedure RMD160Full(var Digest: TRMD160Digest; Msg: pointer; Len: word);
  {-RMD160 of Msg with init/update/final}

procedure RMD160FullXL(var Digest: TRMD160Digest; Msg: pointer; Len: longint);
  {-RMD160 of Msg with init/update/final}

procedure RMD160File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TRMD160Digest; var buf; bsize: word; var Err: word);
  {-RMD160 of file, buf: buffer with at least bsize bytes}


implementation


{$ifdef FPC}
  {$if not(defined(CPUARM)) and not(defined(CPUAARCH64))}
    {$asmmode intel}
  {$endif}
{$endif}

{$ifdef BIT16}
  {$F-}
{$endif}


const
  RMD160_BlockLen  = 64;

const
  k1 = longint($5a827999);       {2^30*2^(1/2)}
  k2 = longint($6ed9eba1);       {2^30*3^(1/2)}
  k3 = longint($8f1bbcdc);       {2^30*5^(1/2)}
  k4 = longint($a953fd4e);       {2^30*7^(1/2)}
  k5 = longint($50a28be6);       {2^30*2^(1/3)}
  k6 = longint($5c4dd124);       {2^30*3^(1/3)}
  k7 = longint($6d703ef3);       {2^30*5^(1/3)}
  k8 = longint($7a6d76e9);       {2^30*7^(1/3)}


{1.3.36.3.2.1}
{iso(1) identified-organization(3) teletrust(36) algorithm(3) hashAlgorithm(2) ripemd160(1)}
const
  RMD160_OID : TOID_Vec = (1,3,36,3,2,1,-1,-1,-1,-1,-1); {Len=6}


{$ifndef VER5X}
const
  RMD160_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : RMD160_BlockLen;
               HDigestlen: sizeof(TRMD160Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @RMD160Init;
               HFinal    : @RMD160FinalEx;
               HUpdateXL : @RMD160UpdateXL;
             {$else}
               HInit     : RMD160Init;
               HFinal    : RMD160FinalEx;
               HUpdateXL : RMD160UpdateXL;
             {$endif}
               HAlgNum   : longint(_RIPEMD160);
               HName     : 'RIPEMD160';
               HPtrOID   : @RMD160_OID;
               HLenOID   : 6;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @RMD160FinalBitsEx;
             {$else}
               HFinalBit : RMD160FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  RMD160_Desc: THashDesc;
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

{$else}

{$ifdef BASM16}

{TP5-7/Delphi1 for 386+}

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

{$endif BASM16}

{$endif BIT16}



{Note: The functions f2 and f4 from the specification }
{  f2(x, y, z) = (x and y) or (not x and z)           }
{  f4(x, y, z) = (x and z) or (y and not z)           }
{can be optimized as follows                          }
{  f2(x, y, z) = (z xor (x and (y xor z)))            }
{  f4(x, y, z) = (y xor (z and (x xor y)))            }
{found for example in Wei Dai's Crypto++ Library 5.2.1}

{$ifndef BIT16}


{---------------------------------------------------------------------------}
procedure RMD160Compress(var Data: THashState; const Buf: THashBuffer);
  {-Actual hashing function}
var
  a,b,c,d,e,a1,b1,c1,d1,e1: longint;
  X: THashBuf32 absolute Buf;
begin

  {Optimization info V0.14: Use the variables a,b,c,d,e in both}
  {parallel parts. Delphi32 optimizers keep these variables in }
  {registers, in the 'standard' version only a1,..,e1 are kept }
  {in registers. With VP/FPC the effect is smaller or vanishing}
  {Speedup:  79.6 -> 32.4 Cycles/Byte for D6 / 1.8 GHz P4 Win98}
  {         112.7 -> 93,8 for VP, 126.8 --> 126.8 for FPC 2.0.2}

  {Assign old working hash to working variables}
  a := Data[0];
  b := Data[1];
  c := Data[2];
  d := Data[3];
  e := Data[4];

  inc(a,(b xor c xor d)+X[ 0]); a:=(a shl 11 or a shr (32-11))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[ 1]); e:=(e shl 14 or e shr (32-14))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[ 2]); d:=(d shl 15 or d shr (32-15))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[ 3]); c:=(c shl 12 or c shr (32-12))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[ 4]); b:=(b shl  5 or b shr (32- 5))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[ 5]); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[ 6]); e:=(e shl  7 or e shr (32- 7))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[ 7]); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[ 8]); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[ 9]); b:=(b shl 13 or b shr (32-13))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[10]); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[11]); e:=(e shl 15 or e shr (32-15))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[12]); d:=(d shl  6 or d shr (32- 6))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[13]); c:=(c shl  7 or c shr (32- 7))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[14]); b:=(b shl  9 or b shr (32- 9))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[15]); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));

  inc(e,(c xor (a and (b xor c)))+X[ 7]+k1); e:=(e shl  7 or e shr (32- 7))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[ 4]+k1); d:=(d shl  6 or d shr (32- 6))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[13]+k1); c:=(c shl  8 or c shr (32- 8))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[ 1]+k1); b:=(b shl 13 or b shr (32-13))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[10]+k1); a:=(a shl 11 or a shr (32-11))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 6]+k1); e:=(e shl  9 or e shr (32- 9))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[15]+k1); d:=(d shl  7 or d shr (32- 7))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[ 3]+k1); c:=(c shl 15 or c shr (32-15))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[12]+k1); b:=(b shl  7 or b shr (32- 7))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[ 0]+k1); a:=(a shl 12 or a shr (32-12))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 9]+k1); e:=(e shl 15 or e shr (32-15))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[ 5]+k1); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[ 2]+k1); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[14]+k1); b:=(b shl  7 or b shr (32- 7))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[11]+k1); a:=(a shl 13 or a shr (32-13))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 8]+k1); e:=(e shl 12 or e shr (32-12))+d; b:=(b shl 10 or b shr (32-10));

  inc(d,(b xor (e or (not a)))+X[ 3]+k2); d:=(d shl 11 or d shr (32-11))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[10]+k2); c:=(c shl 13 or c shr (32-13))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[14]+k2); b:=(b shl  6 or b shr (32- 6))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[ 4]+k2); a:=(a shl  7 or a shr (32- 7))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 9]+k2); e:=(e shl 14 or e shr (32-14))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[15]+k2); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[ 8]+k2); c:=(c shl 13 or c shr (32-13))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[ 1]+k2); b:=(b shl 15 or b shr (32-15))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[ 2]+k2); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 7]+k2); e:=(e shl  8 or e shr (32- 8))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[ 0]+k2); d:=(d shl 13 or d shr (32-13))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[ 6]+k2); c:=(c shl  6 or c shr (32- 6))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[13]+k2); b:=(b shl  5 or b shr (32- 5))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[11]+k2); a:=(a shl 12 or a shr (32-12))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 5]+k2); e:=(e shl  7 or e shr (32- 7))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[12]+k2); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));

  inc(c,(e xor (a and (d xor e)))+X[ 1]+k3); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[ 9]+k3); b:=(b shl 12 or b shr (32-12))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[11]+k3); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[10]+k3); e:=(e shl 15 or e shr (32-15))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[ 0]+k3); d:=(d shl 14 or d shr (32-14))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[ 8]+k3); c:=(c shl 15 or c shr (32-15))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[12]+k3); b:=(b shl  9 or b shr (32- 9))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[ 4]+k3); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[13]+k3); e:=(e shl  9 or e shr (32- 9))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[ 3]+k3); d:=(d shl 14 or d shr (32-14))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[ 7]+k3); c:=(c shl  5 or c shr (32- 5))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[15]+k3); b:=(b shl  6 or b shr (32- 6))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[14]+k3); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[ 5]+k3); e:=(e shl  6 or e shr (32- 6))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[ 6]+k3); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[ 2]+k3); c:=(c shl 12 or c shr (32-12))+b; e:=(e shl 10 or e shr (32-10));

  inc(b,(c xor (d or (not e)))+X[ 4]+k4); b:=(b shl  9 or b shr (32- 9))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[ 0]+k4); a:=(a shl 15 or a shr (32-15))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[ 5]+k4); e:=(e shl  5 or e shr (32- 5))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[ 9]+k4); d:=(d shl 11 or d shr (32-11))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[ 7]+k4); c:=(c shl  6 or c shr (32- 6))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[12]+k4); b:=(b shl  8 or b shr (32- 8))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[ 2]+k4); a:=(a shl 13 or a shr (32-13))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[10]+k4); e:=(e shl 12 or e shr (32-12))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[14]+k4); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[ 1]+k4); c:=(c shl 12 or c shr (32-12))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[ 3]+k4); b:=(b shl 13 or b shr (32-13))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[ 8]+k4); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[11]+k4); e:=(e shl 11 or e shr (32-11))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[ 6]+k4); d:=(d shl  8 or d shr (32- 8))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[15]+k4); c:=(c shl  5 or c shr (32- 5))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[13]+k4); b:=(b shl  6 or b shr (32- 6))+a; d:=(d shl 10 or d shr (32-10));

  {Save result of first part}
  a1 := a;
  b1 := b;
  c1 := c;
  d1 := d;
  e1 := e;

  {Initialize for second part}
  a := Data[0];
  b := Data[1];
  c := Data[2];
  d := Data[3];
  e := Data[4];

  inc(a,(b xor (c or (not d)))+X[ 5]+k5); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[14]+k5); e:=(e shl  9 or e shr (32- 9))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[ 7]+k5); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[ 0]+k5); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[ 9]+k5); b:=(b shl 13 or b shr (32-13))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[ 2]+k5); a:=(a shl 15 or a shr (32-15))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[11]+k5); e:=(e shl 15 or e shr (32-15))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[ 4]+k5); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[13]+k5); c:=(c shl  7 or c shr (32- 7))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[ 6]+k5); b:=(b shl  7 or b shr (32- 7))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[15]+k5); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor (b or (not c)))+X[ 8]+k5); e:=(e shl 11 or e shr (32-11))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor (a or (not b)))+X[ 1]+k5); d:=(d shl 14 or d shr (32-14))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor (e or (not a)))+X[10]+k5); c:=(c shl 14 or c shr (32-14))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor (d or (not e)))+X[ 3]+k5); b:=(b shl 12 or b shr (32-12))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor (c or (not d)))+X[12]+k5); a:=(a shl  6 or a shr (32- 6))+e; c:=(c shl 10 or c shr (32-10));

  inc(e,(b xor (c and (a xor b)))+X[ 6]+k6); e:=(e shl  9 or e shr (32- 9))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[11]+k6); d:=(d shl 13 or d shr (32-13))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[ 3]+k6); c:=(c shl 15 or c shr (32-15))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[ 7]+k6); b:=(b shl  7 or b shr (32- 7))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[ 0]+k6); a:=(a shl 12 or a shr (32-12))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[13]+k6); e:=(e shl  8 or e shr (32- 8))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[ 5]+k6); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[10]+k6); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[14]+k6); b:=(b shl  7 or b shr (32- 7))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[15]+k6); a:=(a shl  7 or a shr (32- 7))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[ 8]+k6); e:=(e shl 12 or e shr (32-12))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(a xor (b and (e xor a)))+X[12]+k6); d:=(d shl  7 or d shr (32- 7))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(e xor (a and (d xor e)))+X[ 4]+k6); c:=(c shl  6 or c shr (32- 6))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(d xor (e and (c xor d)))+X[ 9]+k6); b:=(b shl 15 or b shr (32-15))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(c xor (d and (b xor c)))+X[ 1]+k6); a:=(a shl 13 or a shr (32-13))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(b xor (c and (a xor b)))+X[ 2]+k6); e:=(e shl 11 or e shr (32-11))+d; b:=(b shl 10 or b shr (32-10));

  inc(d,(b xor (e or (not a)))+X[15]+k7); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[ 5]+k7); c:=(c shl  7 or c shr (32- 7))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[ 1]+k7); b:=(b shl 15 or b shr (32-15))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[ 3]+k7); a:=(a shl 11 or a shr (32-11))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 7]+k7); e:=(e shl  8 or e shr (32- 8))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[14]+k7); d:=(d shl  6 or d shr (32- 6))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[ 6]+k7); c:=(c shl  6 or c shr (32- 6))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[ 9]+k7); b:=(b shl 14 or b shr (32-14))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[11]+k7); a:=(a shl 12 or a shr (32-12))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 8]+k7); e:=(e shl 13 or e shr (32-13))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[12]+k7); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d or (not e)))+X[ 2]+k7); c:=(c shl 14 or c shr (32-14))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c or (not d)))+X[10]+k7); b:=(b shl 13 or b shr (32-13))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b or (not c)))+X[ 0]+k7); a:=(a shl 13 or a shr (32-13))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a or (not b)))+X[ 4]+k7); e:=(e shl  7 or e shr (32- 7))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e or (not a)))+X[13]+k7); d:=(d shl  5 or d shr (32- 5))+c; a:=(a shl 10 or a shr (32-10));

  inc(c,(a xor (d and (e xor a)))+X[ 8]+k8); c:=(c shl 15 or c shr (32-15))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[ 6]+k8); b:=(b shl  5 or b shr (32- 5))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[ 4]+k8); a:=(a shl  8 or a shr (32- 8))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 1]+k8); e:=(e shl 11 or e shr (32-11))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[ 3]+k8); d:=(d shl 14 or d shr (32-14))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[11]+k8); c:=(c shl 14 or c shr (32-14))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[15]+k8); b:=(b shl  6 or b shr (32- 6))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[ 0]+k8); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 5]+k8); e:=(e shl  6 or e shr (32- 6))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[12]+k8); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[ 2]+k8); c:=(c shl 12 or c shr (32-12))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(e xor (c and (d xor e)))+X[13]+k8); b:=(b shl  9 or b shr (32- 9))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(d xor (b and (c xor d)))+X[ 9]+k8); a:=(a shl 12 or a shr (32-12))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(c xor (a and (b xor c)))+X[ 7]+k8); e:=(e shl  5 or e shr (32- 5))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(b xor (e and (a xor b)))+X[10]+k8); d:=(d shl 15 or d shr (32-15))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(a xor (d and (e xor a)))+X[14]+k8); c:=(c shl  8 or c shr (32- 8))+b; e:=(e shl 10 or e shr (32-10));

  inc(b,(c xor d xor e)+X[12]); b:=(b shl  8 or b shr (32- 8))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[15]); a:=(a shl  5 or a shr (32- 5))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[10]); e:=(e shl 12 or e shr (32-12))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[ 4]); d:=(d shl  9 or d shr (32- 9))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[ 1]); c:=(c shl 12 or c shr (32-12))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[ 5]); b:=(b shl  5 or b shr (32- 5))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[ 8]); a:=(a shl 14 or a shr (32-14))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[ 7]); e:=(e shl  6 or e shr (32- 6))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[ 6]); d:=(d shl  8 or d shr (32- 8))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[ 2]); c:=(c shl 13 or c shr (32-13))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[13]); b:=(b shl  6 or b shr (32- 6))+a; d:=(d shl 10 or d shr (32-10));
  inc(a,(b xor c xor d)+X[14]); a:=(a shl  5 or a shr (32- 5))+e; c:=(c shl 10 or c shr (32-10));
  inc(e,(a xor b xor c)+X[ 0]); e:=(e shl 15 or e shr (32-15))+d; b:=(b shl 10 or b shr (32-10));
  inc(d,(e xor a xor b)+X[ 3]); d:=(d shl 13 or d shr (32-13))+c; a:=(a shl 10 or a shr (32-10));
  inc(c,(d xor e xor a)+X[ 9]); c:=(c shl 11 or c shr (32-11))+b; e:=(e shl 10 or e shr (32-10));
  inc(b,(c xor d xor e)+X[11]); b:=(b shl 11 or b shr (32-11))+a; d:=(d shl 10 or d shr (32-10));

  {Combine parts 1 and 2}
  d       := Data[1] + c1 + d;
  Data[1] := Data[2] + d1 + e;
  Data[2] := Data[3] + e1 + a;
  Data[3] := Data[4] + a1 + b;
  Data[4] := Data[0] + b1 + c;
  Data[0] := d;

end;


{$else}

{$ifdef BASM16}

{---------------------------------------------------------------------------}
procedure RMD160Compress(var Data: THashState; var Buf: THashBuffer);
  {-Actual hashing function}
var
  a1, b1, c1, d1, e1, a2, b2, c2, d2, e2: longint;
  X: THashBuf32;
begin
  {Assign old working hash to working variables}
  a1 := Data[0];
  b1 := Data[1];
  c1 := Data[2];
  d1 := Data[3];
  e1 := Data[4];
  a2 := a1;
  b2 := b1;
  c2 := c1;
  d2 := d1;
  e2 := e1;

  {Using local copy is faster}
  asm
    {dest = X}
    mov dx, ds
    mov ax, ss
    mov es, ax
    lea di, [X]
    {src = Buf}
    mov dx, ds
    lds si, [Buf]
    {move words, movsd is slower!}
    mov cx, RMD160_BlockLen/2;
    cld
    rep movsw
    {restore ds}
    mov ds,dx
  end;

  inc(a1,(b1 xor c1 xor d1)+X[ 0]); asm db $66; rol word(a1),11; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor b1 xor c1)+X[ 1]); asm db $66; rol word(e1),14; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor a1 xor b1)+X[ 2]); asm db $66; rol word(d1),15; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor e1 xor a1)+X[ 3]); asm db $66; rol word(c1),12; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor d1 xor e1)+X[ 4]); asm db $66; rol word(b1), 5; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor c1 xor d1)+X[ 5]); asm db $66; rol word(a1), 8; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor b1 xor c1)+X[ 6]); asm db $66; rol word(e1), 7; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor a1 xor b1)+X[ 7]); asm db $66; rol word(d1), 9; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor e1 xor a1)+X[ 8]); asm db $66; rol word(c1),11; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor d1 xor e1)+X[ 9]); asm db $66; rol word(b1),13; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor c1 xor d1)+X[10]); asm db $66; rol word(a1),14; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor b1 xor c1)+X[11]); asm db $66; rol word(e1),15; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor a1 xor b1)+X[12]); asm db $66; rol word(d1), 6; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor e1 xor a1)+X[13]); asm db $66; rol word(c1), 7; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor d1 xor e1)+X[14]); asm db $66; rol word(b1), 9; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor c1 xor d1)+X[15]); asm db $66; rol word(a1), 8; db $66;rol word(c1),10 end; inc(a1,e1);

  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 7]+k1); asm db $66; rol word(e1), 7; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[ 4]+k1); asm db $66; rol word(d1), 6; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[13]+k1); asm db $66; rol word(c1), 8; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[ 1]+k1); asm db $66; rol word(b1),13; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[10]+k1); asm db $66; rol word(a1),11; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 6]+k1); asm db $66; rol word(e1), 9; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[15]+k1); asm db $66; rol word(d1), 7; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[ 3]+k1); asm db $66; rol word(c1),15; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[12]+k1); asm db $66; rol word(b1), 7; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[ 0]+k1); asm db $66; rol word(a1),12; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 9]+k1); asm db $66; rol word(e1),15; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[ 5]+k1); asm db $66; rol word(d1), 9; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[ 2]+k1); asm db $66; rol word(c1),11; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[14]+k1); asm db $66; rol word(b1), 7; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[11]+k1); asm db $66; rol word(a1),13; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 8]+k1); asm db $66; rol word(e1),12; db $66;rol word(b1),10 end; inc(e1,d1);

  inc(d1,(b1 xor (e1 or (not a1)))+X[ 3]+k2); asm db $66; rol word(d1),11; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[10]+k2); asm db $66; rol word(c1),13; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[14]+k2); asm db $66; rol word(b1), 6; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[ 4]+k2); asm db $66; rol word(a1), 7; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 9]+k2); asm db $66; rol word(e1),14; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[15]+k2); asm db $66; rol word(d1), 9; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[ 8]+k2); asm db $66; rol word(c1),13; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[ 1]+k2); asm db $66; rol word(b1),15; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[ 2]+k2); asm db $66; rol word(a1),14; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 7]+k2); asm db $66; rol word(e1), 8; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[ 0]+k2); asm db $66; rol word(d1),13; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[ 6]+k2); asm db $66; rol word(c1), 6; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[13]+k2); asm db $66; rol word(b1), 5; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[11]+k2); asm db $66; rol word(a1),12; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 5]+k2); asm db $66; rol word(e1), 7; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[12]+k2); asm db $66; rol word(d1), 5; db $66;rol word(a1),10 end; inc(d1,c1);

  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 1]+k3); asm db $66; rol word(c1),11; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[ 9]+k3); asm db $66; rol word(b1),12; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[11]+k3); asm db $66; rol word(a1),14; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[10]+k3); asm db $66; rol word(e1),15; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 0]+k3); asm db $66; rol word(d1),14; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 8]+k3); asm db $66; rol word(c1),15; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[12]+k3); asm db $66; rol word(b1), 9; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[ 4]+k3); asm db $66; rol word(a1), 8; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[13]+k3); asm db $66; rol word(e1), 9; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 3]+k3); asm db $66; rol word(d1),14; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 7]+k3); asm db $66; rol word(c1), 5; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[15]+k3); asm db $66; rol word(b1), 6; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[14]+k3); asm db $66; rol word(a1), 8; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[ 5]+k3); asm db $66; rol word(e1), 6; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 6]+k3); asm db $66; rol word(d1), 5; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 2]+k3); asm db $66; rol word(c1),12; db $66;rol word(e1),10 end; inc(c1,b1);

  inc(b1,(c1 xor (d1 or (not e1)))+X[ 4]+k4); asm db $66; rol word(b1), 9; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 0]+k4); asm db $66; rol word(a1),15; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[ 5]+k4); asm db $66; rol word(e1), 5; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[ 9]+k4); asm db $66; rol word(d1),11; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[ 7]+k4); asm db $66; rol word(c1), 6; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[12]+k4); asm db $66; rol word(b1), 8; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 2]+k4); asm db $66; rol word(a1),13; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[10]+k4); asm db $66; rol word(e1),12; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[14]+k4); asm db $66; rol word(d1), 5; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[ 1]+k4); asm db $66; rol word(c1),12; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[ 3]+k4); asm db $66; rol word(b1),13; db $66;rol word(d1),10 end; inc(b1,a1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 8]+k4); asm db $66; rol word(a1),14; db $66;rol word(c1),10 end; inc(a1,e1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[11]+k4); asm db $66; rol word(e1),11; db $66;rol word(b1),10 end; inc(e1,d1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[ 6]+k4); asm db $66; rol word(d1), 8; db $66;rol word(a1),10 end; inc(d1,c1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[15]+k4); asm db $66; rol word(c1), 5; db $66;rol word(e1),10 end; inc(c1,b1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[13]+k4); asm db $66; rol word(b1), 6; db $66;rol word(d1),10 end; inc(b1,a1);

  inc(a2,(b2 xor (c2 or (not d2)))+X[ 5]+k5); asm db $66; rol word(a2), 8; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[14]+k5); asm db $66; rol word(e2), 9; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 7]+k5); asm db $66; rol word(d2), 9; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[ 0]+k5); asm db $66; rol word(c2),11; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 9]+k5); asm db $66; rol word(b2),13; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[ 2]+k5); asm db $66; rol word(a2),15; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[11]+k5); asm db $66; rol word(e2),15; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 4]+k5); asm db $66; rol word(d2), 5; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[13]+k5); asm db $66; rol word(c2), 7; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 6]+k5); asm db $66; rol word(b2), 7; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[15]+k5); asm db $66; rol word(a2), 8; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[ 8]+k5); asm db $66; rol word(e2),11; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 1]+k5); asm db $66; rol word(d2),14; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[10]+k5); asm db $66; rol word(c2),14; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 3]+k5); asm db $66; rol word(b2),12; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[12]+k5); asm db $66; rol word(a2), 6; db $66;rol word(c2),10 end; inc(a2,e2);

  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 6]+k6); asm db $66; rol word(e2), 9; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[11]+k6); asm db $66; rol word(d2),13; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[ 3]+k6); asm db $66; rol word(c2),15; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[ 7]+k6); asm db $66; rol word(b2), 7; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[ 0]+k6); asm db $66; rol word(a2),12; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[13]+k6); asm db $66; rol word(e2), 8; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[ 5]+k6); asm db $66; rol word(d2), 9; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[10]+k6); asm db $66; rol word(c2),11; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[14]+k6); asm db $66; rol word(b2), 7; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[15]+k6); asm db $66; rol word(a2), 7; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 8]+k6); asm db $66; rol word(e2),12; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[12]+k6); asm db $66; rol word(d2), 7; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[ 4]+k6); asm db $66; rol word(c2), 6; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[ 9]+k6); asm db $66; rol word(b2),15; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[ 1]+k6); asm db $66; rol word(a2),13; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 2]+k6); asm db $66; rol word(e2),11; db $66;rol word(b2),10 end; inc(e2,d2);

  inc(d2,(b2 xor (e2 or (not a2)))+X[15]+k7); asm db $66; rol word(d2), 9; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 5]+k7); asm db $66; rol word(c2), 7; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[ 1]+k7); asm db $66; rol word(b2),15; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[ 3]+k7); asm db $66; rol word(a2),11; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 7]+k7); asm db $66; rol word(e2), 8; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[14]+k7); asm db $66; rol word(d2), 6; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 6]+k7); asm db $66; rol word(c2), 6; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[ 9]+k7); asm db $66; rol word(b2),14; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[11]+k7); asm db $66; rol word(a2),12; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 8]+k7); asm db $66; rol word(e2),13; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[12]+k7); asm db $66; rol word(d2), 5; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 2]+k7); asm db $66; rol word(c2),14; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[10]+k7); asm db $66; rol word(b2),13; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[ 0]+k7); asm db $66; rol word(a2),13; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 4]+k7); asm db $66; rol word(e2), 7; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[13]+k7); asm db $66; rol word(d2), 5; db $66;rol word(a2),10 end; inc(d2,c2);

  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[ 8]+k8); asm db $66; rol word(c2),15; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[ 6]+k8); asm db $66; rol word(b2), 5; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 4]+k8); asm db $66; rol word(a2), 8; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 1]+k8); asm db $66; rol word(e2),11; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[ 3]+k8); asm db $66; rol word(d2),14; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[11]+k8); asm db $66; rol word(c2),14; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[15]+k8); asm db $66; rol word(b2), 6; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 0]+k8); asm db $66; rol word(a2),14; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 5]+k8); asm db $66; rol word(e2), 6; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[12]+k8); asm db $66; rol word(d2), 9; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[ 2]+k8); asm db $66; rol word(c2),12; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[13]+k8); asm db $66; rol word(b2), 9; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 9]+k8); asm db $66; rol word(a2),12; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 7]+k8); asm db $66; rol word(e2), 5; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[10]+k8); asm db $66; rol word(d2),15; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[14]+k8); asm db $66; rol word(c2), 8; db $66;rol word(e2),10 end; inc(c2,b2);

  inc(b2,(c2 xor d2 xor e2)+X[12]); asm db $66; rol word(b2), 8; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor c2 xor d2)+X[15]); asm db $66; rol word(a2), 5; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor b2 xor c2)+X[10]); asm db $66; rol word(e2),12; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor a2 xor b2)+X[ 4]); asm db $66; rol word(d2), 9; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor e2 xor a2)+X[ 1]); asm db $66; rol word(c2),12; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor d2 xor e2)+X[ 5]); asm db $66; rol word(b2), 5; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor c2 xor d2)+X[ 8]); asm db $66; rol word(a2),14; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor b2 xor c2)+X[ 7]); asm db $66; rol word(e2), 6; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor a2 xor b2)+X[ 6]); asm db $66; rol word(d2), 8; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor e2 xor a2)+X[ 2]); asm db $66; rol word(c2),13; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor d2 xor e2)+X[13]); asm db $66; rol word(b2), 6; db $66;rol word(d2),10 end; inc(b2,a2);
  inc(a2,(b2 xor c2 xor d2)+X[14]); asm db $66; rol word(a2), 5; db $66;rol word(c2),10 end; inc(a2,e2);
  inc(e2,(a2 xor b2 xor c2)+X[ 0]); asm db $66; rol word(e2),15; db $66;rol word(b2),10 end; inc(e2,d2);
  inc(d2,(e2 xor a2 xor b2)+X[ 3]); asm db $66; rol word(d2),13; db $66;rol word(a2),10 end; inc(d2,c2);
  inc(c2,(d2 xor e2 xor a2)+X[ 9]); asm db $66; rol word(c2),11; db $66;rol word(e2),10 end; inc(c2,b2);
  inc(b2,(c2 xor d2 xor e2)+X[11]); asm db $66; rol word(b2),11; db $66;rol word(d2),10 end; inc(b2,a2);

  c1      := Data[1] + c1 + d2;
  Data[1] := Data[2] + d1 + e2;
  Data[2] := Data[3] + e1 + a2;
  Data[3] := Data[4] + a1 + b2;
  Data[4] := Data[0] + b1 + c2;
  Data[0] := c1;

end;

{$else}

{TP5/5.5}

{---------------------------------------------------------------------------}
function RL(x: longint; c: integer): longint;
  {-Rotate x left, c<=16!!}
inline(
  $59/          {  pop  cx   }
  $58/          {  pop  ax   }
  $5A/          {  pop  dx   }
  $8B/$DA/      {  mov  bx,dx}
  $D1/$E3/      {L:shl  bx,1 }
  $D1/$D0/      {  rcl  ax,1 }
  $D1/$D2/      {  rcl  dx,1 }
  $49/          {  dec  cx   }
  $75/$F7);     {  jne  L    }


{---------------------------------------------------------------------------}
function RL10(x: longint): longint;
  {-Rotate x left 10, optimized}
inline(
  $58/          { pop  ax   }
  $59/          { pop  cx   }
  $8A/$F1/      { mov  dh,cl}  {Rotate left 1 byte = 8 bits}
  $8A/$D4/      { mov  dl,ah}
  $8A/$E0/      { mov  ah,al}
  $8A/$C5/      { mov  al,ch}
  $D0/$E1/      { shl  cl,1 }  {Rotate left 2 bits}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $D0/$E1/      { shl  cl,1 }
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2);     { rcl  dx,1 }


{---------------------------------------------------------------------------}
procedure RMD160Compress(var Data: THashState; var Buf: THashBuffer);
  {-Actual hashing function}
var
  a1, b1, c1, d1, e1, a2, b2, c2, d2, e2: longint;
  X: THashBuf32;
begin
  {Assign old working hash to working variables}
  a1 := Data[0];
  b1 := Data[1];
  c1 := Data[2];
  d1 := Data[3];
  e1 := Data[4];
  a2 := a1;
  b2 := b1;
  c2 := c1;
  d2 := d1;
  e2 := e1;

  {Using local copy is faster}
  move(Buf, X, RMD160_BlockLen);

  inc(a1,(b1 xor c1 xor d1)+X[ 0]); a1:=RL(a1,11)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor b1 xor c1)+X[ 1]); e1:=RL(e1,14)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor a1 xor b1)+X[ 2]); d1:=RL(d1,15)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor e1 xor a1)+X[ 3]); c1:=RL(c1,12)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor d1 xor e1)+X[ 4]); b1:=RL(b1, 5)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor c1 xor d1)+X[ 5]); a1:=RL(a1, 8)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor b1 xor c1)+X[ 6]); e1:=RL(e1, 7)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor a1 xor b1)+X[ 7]); d1:=RL(d1, 9)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor e1 xor a1)+X[ 8]); c1:=RL(c1,11)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor d1 xor e1)+X[ 9]); b1:=RL(b1,13)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor c1 xor d1)+X[10]); a1:=RL(a1,14)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor b1 xor c1)+X[11]); e1:=RL(e1,15)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor a1 xor b1)+X[12]); d1:=RL(d1, 6)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor e1 xor a1)+X[13]); c1:=RL(c1, 7)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor d1 xor e1)+X[14]); b1:=RL(b1, 9)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor c1 xor d1)+X[15]); a1:=RL(a1, 8)+e1; c1:=RL10(c1);

  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 7]+k1); e1:=RL(e1, 7)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[ 4]+k1); d1:=RL(d1, 6)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[13]+k1); c1:=RL(c1, 8)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[ 1]+k1); b1:=RL(b1,13)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[10]+k1); a1:=RL(a1,11)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 6]+k1); e1:=RL(e1, 9)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[15]+k1); d1:=RL(d1, 7)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[ 3]+k1); c1:=RL(c1,15)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[12]+k1); b1:=RL(b1, 7)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[ 0]+k1); a1:=RL(a1,12)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 9]+k1); e1:=RL(e1,15)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 and (a1 xor b1)))+X[ 5]+k1); d1:=RL(d1, 9)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 and (e1 xor a1)))+X[ 2]+k1); c1:=RL(c1,11)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 and (d1 xor e1)))+X[14]+k1); b1:=RL(b1, 7)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 and (c1 xor d1)))+X[11]+k1); a1:=RL(a1,13)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 and (b1 xor c1)))+X[ 8]+k1); e1:=RL(e1,12)+d1; b1:=RL10(b1);

  inc(d1,(b1 xor (e1 or (not a1)))+X[ 3]+k2); d1:=RL(d1,11)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[10]+k2); c1:=RL(c1,13)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[14]+k2); b1:=RL(b1, 6)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[ 4]+k2); a1:=RL(a1, 7)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 9]+k2); e1:=RL(e1,14)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[15]+k2); d1:=RL(d1, 9)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[ 8]+k2); c1:=RL(c1,13)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[ 1]+k2); b1:=RL(b1,15)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[ 2]+k2); a1:=RL(a1,14)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 7]+k2); e1:=RL(e1, 8)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[ 0]+k2); d1:=RL(d1,13)+c1; a1:=RL10(a1);
  inc(c1,(a1 xor (d1 or (not e1)))+X[ 6]+k2); c1:=RL(c1, 6)+b1; e1:=RL10(e1);
  inc(b1,(e1 xor (c1 or (not d1)))+X[13]+k2); b1:=RL(b1, 5)+a1; d1:=RL10(d1);
  inc(a1,(d1 xor (b1 or (not c1)))+X[11]+k2); a1:=RL(a1,12)+e1; c1:=RL10(c1);
  inc(e1,(c1 xor (a1 or (not b1)))+X[ 5]+k2); e1:=RL(e1, 7)+d1; b1:=RL10(b1);
  inc(d1,(b1 xor (e1 or (not a1)))+X[12]+k2); d1:=RL(d1, 5)+c1; a1:=RL10(a1);

  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 1]+k3); c1:=RL(c1,11)+b1; e1:=RL10(e1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[ 9]+k3); b1:=RL(b1,12)+a1; d1:=RL10(d1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[11]+k3); a1:=RL(a1,14)+e1; c1:=RL10(c1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[10]+k3); e1:=RL(e1,15)+d1; b1:=RL10(b1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 0]+k3); d1:=RL(d1,14)+c1; a1:=RL10(a1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 8]+k3); c1:=RL(c1,15)+b1; e1:=RL10(e1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[12]+k3); b1:=RL(b1, 9)+a1; d1:=RL10(d1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[ 4]+k3); a1:=RL(a1, 8)+e1; c1:=RL10(c1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[13]+k3); e1:=RL(e1, 9)+d1; b1:=RL10(b1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 3]+k3); d1:=RL(d1,14)+c1; a1:=RL10(a1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 7]+k3); c1:=RL(c1, 5)+b1; e1:=RL10(e1);
  inc(b1,(d1 xor (e1 and (c1 xor d1)))+X[15]+k3); b1:=RL(b1, 6)+a1; d1:=RL10(d1);
  inc(a1,(c1 xor (d1 and (b1 xor c1)))+X[14]+k3); a1:=RL(a1, 8)+e1; c1:=RL10(c1);
  inc(e1,(b1 xor (c1 and (a1 xor b1)))+X[ 5]+k3); e1:=RL(e1, 6)+d1; b1:=RL10(b1);
  inc(d1,(a1 xor (b1 and (e1 xor a1)))+X[ 6]+k3); d1:=RL(d1, 5)+c1; a1:=RL10(a1);
  inc(c1,(e1 xor (a1 and (d1 xor e1)))+X[ 2]+k3); c1:=RL(c1,12)+b1; e1:=RL10(e1);

  inc(b1,(c1 xor (d1 or (not e1)))+X[ 4]+k4); b1:=RL(b1, 9)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 0]+k4); a1:=RL(a1,15)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[ 5]+k4); e1:=RL(e1, 5)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[ 9]+k4); d1:=RL(d1,11)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[ 7]+k4); c1:=RL(c1, 6)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[12]+k4); b1:=RL(b1, 8)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 2]+k4); a1:=RL(a1,13)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[10]+k4); e1:=RL(e1,12)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[14]+k4); d1:=RL(d1, 5)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[ 1]+k4); c1:=RL(c1,12)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[ 3]+k4); b1:=RL(b1,13)+a1; d1:=RL10(d1);
  inc(a1,(b1 xor (c1 or (not d1)))+X[ 8]+k4); a1:=RL(a1,14)+e1; c1:=RL10(c1);
  inc(e1,(a1 xor (b1 or (not c1)))+X[11]+k4); e1:=RL(e1,11)+d1; b1:=RL10(b1);
  inc(d1,(e1 xor (a1 or (not b1)))+X[ 6]+k4); d1:=RL(d1, 8)+c1; a1:=RL10(a1);
  inc(c1,(d1 xor (e1 or (not a1)))+X[15]+k4); c1:=RL(c1, 5)+b1; e1:=RL10(e1);
  inc(b1,(c1 xor (d1 or (not e1)))+X[13]+k4); b1:=RL(b1, 6)+a1; d1:=RL10(d1);

  inc(a2,(b2 xor (c2 or (not d2)))+X[ 5]+k5); a2:=RL(a2, 8)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[14]+k5); e2:=RL(e2, 9)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 7]+k5); d2:=RL(d2, 9)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[ 0]+k5); c2:=RL(c2,11)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 9]+k5); b2:=RL(b2,13)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[ 2]+k5); a2:=RL(a2,15)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[11]+k5); e2:=RL(e2,15)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 4]+k5); d2:=RL(d2, 5)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[13]+k5); c2:=RL(c2, 7)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 6]+k5); b2:=RL(b2, 7)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[15]+k5); a2:=RL(a2, 8)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor (b2 or (not c2)))+X[ 8]+k5); e2:=RL(e2,11)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor (a2 or (not b2)))+X[ 1]+k5); d2:=RL(d2,14)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor (e2 or (not a2)))+X[10]+k5); c2:=RL(c2,14)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor (d2 or (not e2)))+X[ 3]+k5); b2:=RL(b2,12)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor (c2 or (not d2)))+X[12]+k5); a2:=RL(a2, 6)+e2; c2:=RL10(c2);

  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 6]+k6); e2:=RL(e2, 9)+d2; b2:=RL10(b2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[11]+k6); d2:=RL(d2,13)+c2; a2:=RL10(a2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[ 3]+k6); c2:=RL(c2,15)+b2; e2:=RL10(e2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[ 7]+k6); b2:=RL(b2, 7)+a2; d2:=RL10(d2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[ 0]+k6); a2:=RL(a2,12)+e2; c2:=RL10(c2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[13]+k6); e2:=RL(e2, 8)+d2; b2:=RL10(b2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[ 5]+k6); d2:=RL(d2, 9)+c2; a2:=RL10(a2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[10]+k6); c2:=RL(c2,11)+b2; e2:=RL10(e2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[14]+k6); b2:=RL(b2, 7)+a2; d2:=RL10(d2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[15]+k6); a2:=RL(a2, 7)+e2; c2:=RL10(c2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 8]+k6); e2:=RL(e2,12)+d2; b2:=RL10(b2);
  inc(d2,(a2 xor (b2 and (e2 xor a2)))+X[12]+k6); d2:=RL(d2, 7)+c2; a2:=RL10(a2);
  inc(c2,(e2 xor (a2 and (d2 xor e2)))+X[ 4]+k6); c2:=RL(c2, 6)+b2; e2:=RL10(e2);
  inc(b2,(d2 xor (e2 and (c2 xor d2)))+X[ 9]+k6); b2:=RL(b2,15)+a2; d2:=RL10(d2);
  inc(a2,(c2 xor (d2 and (b2 xor c2)))+X[ 1]+k6); a2:=RL(a2,13)+e2; c2:=RL10(c2);
  inc(e2,(b2 xor (c2 and (a2 xor b2)))+X[ 2]+k6); e2:=RL(e2,11)+d2; b2:=RL10(b2);

  inc(d2,(b2 xor (e2 or (not a2)))+X[15]+k7); d2:=RL(d2, 9)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 5]+k7); c2:=RL(c2, 7)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[ 1]+k7); b2:=RL(b2,15)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[ 3]+k7); a2:=RL(a2,11)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 7]+k7); e2:=RL(e2, 8)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[14]+k7); d2:=RL(d2, 6)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 6]+k7); c2:=RL(c2, 6)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[ 9]+k7); b2:=RL(b2,14)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[11]+k7); a2:=RL(a2,12)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 8]+k7); e2:=RL(e2,13)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[12]+k7); d2:=RL(d2, 5)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 or (not e2)))+X[ 2]+k7); c2:=RL(c2,14)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 or (not d2)))+X[10]+k7); b2:=RL(b2,13)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 or (not c2)))+X[ 0]+k7); a2:=RL(a2,13)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 or (not b2)))+X[ 4]+k7); e2:=RL(e2, 7)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 or (not a2)))+X[13]+k7); d2:=RL(d2, 5)+c2; a2:=RL10(a2);

  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[ 8]+k8); c2:=RL(c2,15)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[ 6]+k8); b2:=RL(b2, 5)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 4]+k8); a2:=RL(a2, 8)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 1]+k8); e2:=RL(e2,11)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[ 3]+k8); d2:=RL(d2,14)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[11]+k8); c2:=RL(c2,14)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[15]+k8); b2:=RL(b2, 6)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 0]+k8); a2:=RL(a2,14)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 5]+k8); e2:=RL(e2, 6)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[12]+k8); d2:=RL(d2, 9)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[ 2]+k8); c2:=RL(c2,12)+b2; e2:=RL10(e2);
  inc(b2,(e2 xor (c2 and (d2 xor e2)))+X[13]+k8); b2:=RL(b2, 9)+a2; d2:=RL10(d2);
  inc(a2,(d2 xor (b2 and (c2 xor d2)))+X[ 9]+k8); a2:=RL(a2,12)+e2; c2:=RL10(c2);
  inc(e2,(c2 xor (a2 and (b2 xor c2)))+X[ 7]+k8); e2:=RL(e2, 5)+d2; b2:=RL10(b2);
  inc(d2,(b2 xor (e2 and (a2 xor b2)))+X[10]+k8); d2:=RL(d2,15)+c2; a2:=RL10(a2);
  inc(c2,(a2 xor (d2 and (e2 xor a2)))+X[14]+k8); c2:=RL(c2, 8)+b2; e2:=RL10(e2);

  inc(b2,(c2 xor d2 xor e2)+X[12]); b2:=RL(b2, 8)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor c2 xor d2)+X[15]); a2:=RL(a2, 5)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor b2 xor c2)+X[10]); e2:=RL(e2,12)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor a2 xor b2)+X[ 4]); d2:=RL(d2, 9)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor e2 xor a2)+X[ 1]); c2:=RL(c2,12)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor d2 xor e2)+X[ 5]); b2:=RL(b2, 5)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor c2 xor d2)+X[ 8]); a2:=RL(a2,14)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor b2 xor c2)+X[ 7]); e2:=RL(e2, 6)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor a2 xor b2)+X[ 6]); d2:=RL(d2, 8)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor e2 xor a2)+X[ 2]); c2:=RL(c2,13)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor d2 xor e2)+X[13]); b2:=RL(b2, 6)+a2; d2:=RL10(d2);
  inc(a2,(b2 xor c2 xor d2)+X[14]); a2:=RL(a2, 5)+e2; c2:=RL10(c2);
  inc(e2,(a2 xor b2 xor c2)+X[ 0]); e2:=RL(e2,15)+d2; b2:=RL10(b2);
  inc(d2,(e2 xor a2 xor b2)+X[ 3]); d2:=RL(d2,13)+c2; a2:=RL10(a2);
  inc(c2,(d2 xor e2 xor a2)+X[ 9]); c2:=RL(c2,11)+b2; e2:=RL10(e2);
  inc(b2,(c2 xor d2 xor e2)+X[11]); b2:=RL(b2,11)+a2; d2:=RL10(d2);

  c1      := Data[1] + c1 + d2;
  Data[1] := Data[2] + d1 + e2;
  Data[2] := Data[3] + e1 + a2;
  Data[3] := Data[4] + a1 + b2;
  Data[4] := Data[0] + b1 + c2;
  Data[0] := c1;

end;

{$endif}


{$endif}


{---------------------------------------------------------------------------}
procedure RMD160Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context, buffer=0!!}
  fillchar(Context,sizeof(Context),0);
  with Context do begin
     Hash[0] := longint($67452301);
     Hash[1] := longint($efcdab89);
     Hash[2] := longint($98badcfe);
     Hash[3] := longint($10325476);
     Hash[4] := longint($c3d2e1f0);
  end;
end;


{---------------------------------------------------------------------------}
procedure RMD160UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
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
    if Context.Index=RMD160_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      RMD160Compress(Context.Hash,Context.Buffer);
      while Len>=RMD160_BlockLen do begin
        move(Msg^,Context.Buffer,RMD160_BlockLen);
        RMD160Compress(Context.Hash,Context.Buffer);
        inc(Ptr2Inc(Msg),RMD160_BlockLen);
        dec(Len,RMD160_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure RMD160Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  RMD160UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure RMD160FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}
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
    RMD160Compress(Context.Hash,Context.Buffer);
    fillchar(Context.Buffer,56,0);
  end;
  {Write 64 bit msg length into the last bits of the last block}
  {(in big endian format) and do a final compress}
  THashBuf32(Context.Buffer)[14] := Context.MLen[0];
  THashBuf32(Context.Buffer)[15] := Context.MLen[1];
  RMD160Compress(Context.Hash,Context.Buffer);
  {Transfer Context.Hash to Digest, clear upper part of Digest}
  fillchar(Digest[sizeof(TRMD160Digest)], sizeof(Digest)-sizeof(TRMD160Digest), 0);
  move(Context.Hash, Digest, sizeof(TRMD160Digest));
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure RMD160FinalBits(var Context: THashContext; var Digest: TRMD160Digest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  RMD160FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;

{---------------------------------------------------------------------------}
procedure RMD160FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize RMD160 calculation, clear context}
begin
  RMD160FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure RMD160Final(var Context: THashContext; var Digest: TRMD160Digest);
  {-finalize RMD160 calculation, clear context}
var
  tmp: THashDigest;
begin
  RMD160FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function RMD160SelfTest: boolean;
  {-self test for strings from RMD160 page}
const
  s1: string[ 3] = 'abc';
  s2: string[56] = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  D1: TRMD160Digest= ($8e,$b2,$08,$f7,$e0,$5d,$98,$7a,$9b,$04,$4a,$8e,$98,$c6,$b0,$87,$f1,$5a,$0b,$fc);
  D2: TRMD160Digest= ($12,$a0,$53,$38,$4a,$9c,$0c,$88,$e4,$05,$a0,$6c,$27,$dc,$f4,$9a,$da,$62,$eb,$2b);
  D3: TRMD160Digest= ($63,$d0,$36,$56,$71,$3e,$6e,$8e,$a7,$b6,$be,$68,$03,$28,$f9,$50,$1f,$59,$f3,$5e);
  D4: TRMD160Digest= ($dd,$bd,$bb,$a7,$86,$27,$4f,$48,$63,$1a,$7d,$d3,$8f,$84,$b0,$b6,$a7,$e5,$53,$67);

var
  Context: THashContext;
  Digest : TRMD160Digest;

  function SingleTest(s: Str127; TDig: TRMD160Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    RMD160Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@RMD160_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    RMD160Init(Context);
    for i:=1 to length(s) do RMD160Update(Context,@s[i],1);
    RMD160Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@RMD160_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  RMD160SelfTest := false;
  {Note: No independent bit level test vector available. But HMAC-RMD160}
  {reproduces NESSIE HMAC-Ripemd-160-512.unverified.test-vectors. The   }
  {following vectors are calculated once and used for regression testing}
  RMD160Init(Context);
  RMD160FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@RMD160_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  RMD160Init(Context);
  RMD160FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@RMD160_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from RMD160 document}
  RMD160SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure RMD160FullXL(var Digest: TRMD160Digest; Msg: pointer; Len: longint);
  {-RMD160 of Msg with init/update/final}
var
  Context: THashContext;
begin
  RMD160Init(Context);
  RMD160UpdateXL(Context, Msg, Len);
  RMD160Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure RMD160Full(var Digest: TRMD160Digest; Msg: pointer; Len: word);
  {-RMD160 of Msg with init/update/final}
begin
  RMD160FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure RMD160File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TRMD160Digest; var buf; bsize: word; var Err: word);
  {-RMD160 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @RMD160_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(RMD160_Desc, sizeof(RMD160_Desc), 0);
    with RMD160_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := RMD160_BlockLen;
       HDigestlen:= sizeof(TRMD160Digest);
       HInit     := RMD160Init;
       HFinal    := RMD160FinalEx;
       HUpdateXL := RMD160UpdateXL;
       HAlgNum   := longint(_RIPEMD160);
       HName     := 'RIPEMD160';
       HPtrOID   := @RMD160_OID;
       HLenOID   := 6;
       HFinalBit := RMD160FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_RIPEMD160, @RMD160_Desc);
end.
