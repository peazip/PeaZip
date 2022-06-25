unit SHA512;

{512 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA512 - 512 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - Latest specification of Secure Hash Standard:
                      http://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf
                    - Test vectors and intermediate values:
                      http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA_All.pdf
                    * http://disastry.dhs.org/pgp/pgp263iamulti06.zip


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.11.02  W.Ehrhardt  Reference implementation (TP7, D1-D6, FPC, VP)
 0.11     19.11.02  we          TP5/5.5/6
 0.12     19.11.02  we          BASM: Add64
 0.13     19.11.02  we          S[x] := S[x-1] with .L and .H
 0.14     19.11.02  we          Maj/Ch inline
 0.15     19.11.02  we          BASM: RR2
 0.16     20.11.02  we          BIT32: RR2 inline
 0.17     20.11.02  we          BASM16: Sum0/1, Sig0/1
 0.18     20.11.02  we          BASM16: Add64 inline()
 0.19     20.11.02  we          BIT16: $F-
 0.20     20.11.02  we          128 bit UpdateLen, K array of TW64
 0.21     20.11.02  we          Ref: Add64 opt, removd RR2 in Sum0/1, Sig0/1
 0.22     21.11.02  we          Ref: Add64/RB more opt, interface SHA512UpdateXL
 0.23     24.11.02  we          Opt. UpdateLen, BASM16: RA inline()
 3.00     01.12.03  we          Common version 3.0
 3.01     22.12.03  we          TP5/5.5: shl/shr inline
 3.02     22.12.03  we          Changed Add64 def, TP5/5.5: Add64 inline
 3.03     24.12.03  we          Changed Ch() and Maj()
 3.04     14.01.04  we          Int64 support (default for D4+)
 3.05     15.01.04  we          Bit32: inline Sum/Sig0/1
 3.06     22.01.04  we          Inc64
 3.07     05.03.04  we          Update fips180-2 URL, no Add64 for Int64
 3.08     04.01.05  we          Bugfix SHA512Final
 3.09     04.01.05  we          Bugfix Int64 version of SHA512Compress
 3.10     26.02.05  we          With {$ifdef StrictLong}
 3.11     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.12     17.12.05  we          Force $I- in SHA512File
 3.13     15.01.06  we          uses Hash unit and THashDesc
 3.14     15.01.06  we          BugFix for UseInt64
 3.15     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.16     22.01.06  we          Removed HSelfTest from descriptor
 3.17     11.02.06  we          Descriptor as typed const
 3.18     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.19     22.02.07  we          values for OID vector
 3.20     30.06.07  we          Use conditional define FPC_ProcVar
 3.21     29.09.07  we          Bugfix for message bit lengths >= 2^32
 3.22     04.10.07  we          FPC: {$asmmode intel}
 3.23     03.05.08  we          Bit-API: SHA512FinalBits/Ex
 3.24     05.05.08  we          THashDesc constant with HFinalBit field
 3.25     12.11.08  we          Uses BTypes, Ptr2Inc and/or Str255/Str127
 3.26     11.03.12  we          Updated references
 3.27     26.12.12  we          D17 and PurePascal
 3.28     16.08.15  we          Removed $ifdef DLL / stdcall
 3.29     15.05.17  we          adjust OID to new MaxOIDLen
 3.30     29.11.17  we          SHA512File - fname: string

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


{ [*] Janis Jagars, known to the PGP community as "Disastry",
      perished on October 31, 2002 while on vacation in Nepal. }

{ NOTE: FIPS Ch and May functions can be optimized. Wei Dai (Crypto++ V 3.1)
  credits Rich Schroeppel (rcs@cs.arizona.edu), V 5.1 does not!?}


{$i STD.INC}

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

{$ifdef PurePascal}
  {$define UseInt64}
{$else}
  {$ifdef D4Plus}
    {$define UseInt64}
  {$endif}
  {$ifdef FPC}
    {$define UseInt64}
  {$endif}
{$endif}


uses
  BTypes,Hash;


procedure SHA512Init(var Context: THashContext);
  {-initialize context}

procedure SHA512Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA512Final(var Context: THashContext; var Digest: TSHA512Digest);
  {-finalize SHA512 calculation, clear context}

procedure SHA512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512 calculation, clear context}

procedure SHA512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA512FinalBits(var Context: THashContext; var Digest: TSHA512Digest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA512SelfTest: boolean;
  {-self test for string from SHA512 document}

procedure SHA512Full(var Digest: TSHA512Digest; Msg: pointer; Len: word);
  {-SHA512 of Msg with init/update/final}

procedure SHA512FullXL(var Digest: TSHA512Digest; Msg: pointer; Len: longint);
  {-SHA512 of Msg with init/update/final}

procedure SHA512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA512Digest; var buf; bsize: word; var Err: word);
  {-SHA512 of file, buf: buffer with at least bsize bytes}



implementation



{$ifdef BIT16}
  {$F-}
{$endif}

const
  SHA512_BlockLen = 128;

{Internal types for type casting}
type
  TW64    = packed record
              L,H: longint;
            end;


{2.16.840.1.101.3.4.2.3}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) sha512(3)}
const
  SHA512_OID : TOID_Vec = (2,16,840,1,101,3,4,2,3,-1,-1); {Len=9}

{$ifndef VER5X}
const
  SHA512_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA512_BlockLen;
               HDigestlen: sizeof(TSHA512Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA512Init;
               HFinal    : @SHA512FinalEx;
               HUpdateXL : @SHA512UpdateXL;
             {$else}
               HInit     : SHA512Init;
               HFinal    : SHA512FinalEx;
               HUpdateXL : SHA512UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA512);
               HName     : 'SHA512';
               HPtrOID   : @SHA512_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA512FinalBitsEx;
             {$else}
               HFinalBit : SHA512FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA512_Desc: THashDesc;
{$endif}


{$ifndef BIT16}

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  function RB(A: longint): longint;  {$ifdef HAS_INLINE} inline; {$endif}
    {-reverse byte order in longint}
  begin
    RB := ((A and $FF) shl 24) or ((A and $FF00) shl 8) or ((A and $FF0000) shr 8) or ((A and longint($FF000000)) shr 24);
  end;
{$else}
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
  procedure Inc64(var Z: TW64; const X: TW64); assembler;
    {-Inc a 64 bit integer}
  asm
    {$ifdef LoadArgs}
      mov eax,[Z]
      mov edx,[X]
    {$endif}
    mov  ecx,    [edx]
    add  [eax],  ecx
    mov  ecx,    [edx+4]
    adc  [eax+4],ecx
  end;
{$endif}


{$ifndef UseInt64}
{---------------------------------------------------------------------------}
procedure Add64(var Z: TW64; const X,Y: TW64);
  {-Add two 64 bit integers}
begin
 asm
    mov  ecx, [X]
    mov  eax, [ecx]
    mov  edx, [ecx+4]
    mov  ecx, [Y]
    add  eax, [ecx]
    adc  edx, [ecx+4]
    mov  ecx, [Z]
    mov  [ecx], eax
    mov  [ecx+4], edx
  end;
end;
{$endif}


{$else}

{*** 16 bit ***}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

(*** TP 5/5.5 ***)

{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/              {pop    ax   }
  $5A/              {pop    dx   }
  $86/$C6/          {xchg   dh,al}
  $86/$E2);         {xchg   dl,ah}


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
function ISHR0(x: longint; c: integer): longint;
  {-Shift x right, c+16}
inline(
  $59/          {  pop  cx   }
  $58/          {  pop  ax   }
  $58/          {  pop  ax   }
  $33/$D2/      {  xor  dx,dx}
  $D1/$EA/      {L:shr  dx,1 }
  $D1/$D8/      {  rcr  ax,1 }
  $49/          {  dec  cx   }
  $75/$F9);     {  jne  L    }


{---------------------------------------------------------------------------}
function ISHL(x: longint; c: integer): longint;
  {-Shift x left}
inline(
  $59/          {  pop  cx   }
  $58/          {  pop  ax   }
  $5A/          {  pop  dx   }
  $D1/$E0/      {L:shl  ax,1 }
  $D1/$D2/      {  rcl  dx,1 }
  $49/          {  dec  cx   }
  $75/$F9);     {  jne  L    }


{---------------------------------------------------------------------------}
function ISHL0(x: longint; c: integer): longint;
  {-Shift x left, c+16}
inline(
  $59/          {  pop  cx   }
  $5A/          {  pop  dx   }
  $58/          {  pop  ax   }
  $33/$C0/      {  xor  ax,ax}
  $D1/$E0/      {L:shl  ax,1 }
  $D1/$D2/      {  rcl  dx,1 }
  $49/          {  dec  cx   }
  $75/$F9);     {  jne  L    }


{---------------------------------------------------------------------------}
procedure Add64(var Z: TW64; var X,Y: TW64);
  {-Add two 64 bit integers: Z:=X+Y}
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
  $03/$04/      {add   ax,[si]   }
  $13/$5C/$02/  {adc   bx,[si+02]}
  $13/$4C/$04/  {adc   cx,[si+04]}
  $13/$54/$06/  {adc   dx,[si+06]}
  $5E/          {pop   si        }
  $1F/          {pop   ds        }
  $89/$04/      {mov   [si],ax   }
  $89/$5C/$02/  {mov   [si+02],bx}
  $89/$4C/$04/  {mov   [si+04],cx}
  $89/$54/$06/  {mov   [si+06],dx}
  $8E/$DF);     {mov   ds,di     }


{---------------------------------------------------------------------------}
procedure Inc64(var Z: TW64; var X: TW64);
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
procedure Sum0(var X: TW64; var R: TW64);
  {-Big-Sigma-0 function, 'preproccessed' for shift counts > 16}
begin
  R.L := (ISHR0(X.L,12) or ISHL(X.H,4)) xor (ISHR(X.H,2) or ISHL0(X.L,14)) xor (ISHR(X.H,7) or ISHL0(X.L,9));
  R.H := (ISHR0(X.H,12) or ISHL(X.L,4)) xor (ISHR(X.L,2) or ISHL0(X.H,14)) xor (ISHR(X.L,7) or ISHL0(X.H,9));
end;


{---------------------------------------------------------------------------}
procedure Sum1(var X: TW64; var R: TW64);
  {-Big-Sigma-1 function, 'preproccessed' for shift counts > 16}
begin
  R.L := (ISHR(X.L,14) or ISHL0(X.H,2)) xor (ISHR0(X.L,2) or ISHL(X.H,14)) xor (ISHR(X.H,9) or ISHL0(X.L,7));
  R.H := (ISHR(X.H,14) or ISHL0(X.L,2)) xor (ISHR0(X.H,2) or ISHL(X.L,14)) xor (ISHR(X.L,9) or ISHL0(X.H,7));
end;


{---------------------------------------------------------------------------}
procedure Sig0(var X: TW64; var R: TW64);
  {-Small-Sigma-0 function, 'preproccessed' for shift counts > 16}
begin
  R.L := (ISHR(X.L,1) or ISHL0(X.H,15)) xor (ISHR(X.L,8) or ISHL0(X.H,8)) xor (ISHR(X.L,7) or ISHL0(X.H,9));
  R.H := (ISHR(X.H,1) or ISHL0(X.L,15)) xor (ISHR(X.H,8) or ISHL0(X.L,8)) xor ISHR(X.H,7);
end;


{---------------------------------------------------------------------------}
procedure Sig1(var X: TW64; var R: TW64);
  {-Small-Sigma-1 function, 'preproccessed' for shift counts > 31}
begin
  R.L := (ISHR0(X.L,3) or ISHL(X.H,13)) xor (ISHR0(X.H,13) or ISHL(X.L,3)) xor (ISHR(X.L,6) or ISHL0(X.H,10));
  R.H := (ISHR0(X.H,3) or ISHL(X.L,13)) xor (ISHR0(X.L,13) or ISHL(X.H,3)) xor ISHR(X.H,6);
end;


{$else}


(**** TP 6/7/Delphi1 for 386+ *****)


{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/              {pop    ax   }
  $5A/              {pop    dx   }
  $86/$C6/          {xchg   dh,al}
  $86/$E2);         {xchg   dl,ah}


{---------------------------------------------------------------------------}
procedure Add64(var Z: TW64; {$ifdef CONST} const {$else} var {$endif} X,Y: TW64);
  {-Add two 64 bit integers}
inline(
  $8C/$D9/             {mov  cx,ds      }
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$8B/$07/         {mov  eax,[bx]   }
  $66/$8B/$57/$04/     {mov  edx,[bx+04]}
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$03/$07/         {add  eax,[bx]   }
  $66/$13/$57/$04/     {adc  edx,[bx+04]}
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$89/$07/         {mov  [bx],eax   }
  $66/$89/$57/$04/     {mov  [bx+04],edx}
  $8E/$D9);            {mov  ds,cx      }


{---------------------------------------------------------------------------}
procedure Inc64(var Z: TW64; {$ifdef CONST} const {$else} var {$endif} X: TW64);
  {-Inc a 64 bit integer}
inline(

(*
  {slower on Pentium 4}
  $8C/$D9/             {mov  cx,ds      }
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$8B/$07/         {mov  eax,[bx]   }
  $66/$8B/$57/$04/     {mov  edx,[bx+04]}
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$01/$07/         {add  [bx],eax   }
  $66/$11/$57/$04/     {adc  [bx+04],edx}
  $8E/$D9);            {mov  ds,cx      }
*)
  $8C/$D9/             {mov  cx,ds      }
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$8B/$07/         {mov  eax,[bx]   }
  $66/$8B/$57/$04/     {mov  edx,[bx+04]}
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$03/$07/         {add  eax,[bx]   }
  $66/$13/$57/$04/     {adc  edx,[bx+04]}
  $66/$89/$07/         {mov  [bx],eax   }
  $66/$89/$57/$04/     {mov  [bx+04],edx}
  $8E/$D9);            {mov  ds,cx      }


{--------------------------------------------------------------------------}
procedure Sum0({$ifdef CONST} const {$else} var {$endif} X: TW64; var R: TW64); assembler;
  {-Big-Sigma-0 function, 'preproccessed' for shift counts > 31}
asm
{ R.L := ((X.L shr 28) or (X.H shl 4)) xor ((X.H shr 2) or (X.L shl 30)) xor ((X.H shr 7) or (X.L shl 25));
  R.H := ((X.H shr 28) or (X.L shl 4)) xor ((X.L shr 2) or (X.H shl 30)) xor ((X.L shr 7) or (X.H shl 25));}
          les  bx,[X]
   db $66; mov  si,es:[bx]    {X.L}
   db $66; mov  di,es:[bx+4]  {X.H}

   db $66; mov  ax,si         {(X.L shr 28) or (X.H shl 4)}
   db $66; mov  dx,di
   db $66; shr  ax,28
   db $66; shl  dx,4
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,di         {(X.H shr 2) or (X.L shl 30)}
   db $66; mov  dx,si
   db $66; shr  ax,2
   db $66; shl  dx,30
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,di         {(X.H shr 7) or (X.L shl 25)}
   db $66; mov  dx,si
   db $66; shr  ax,7
   db $66; shl  dx,25
   db $66; or   ax,dx
   db $66; xor  ax,cx

           les  bx,[R]
   db $66; mov  es:[bx],ax

   db $66; mov  ax,di         {(X.H shr 28) or (X.L shl 4)}
   db $66; mov  dx,si
   db $66; shr  ax,28
   db $66; shl  dx,4
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,si         {(X.L shr 2) or (X.H shl 30)}
   db $66; mov  dx,di
   db $66; shr  ax,2
   db $66; shl  dx,30
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,si         {(X.L shr 7) or (X.H shl 25)}
   db $66; mov  dx,di
   db $66; shr  ax,7
   db $66; shl  dx,25
   db $66; or   ax,dx
   db $66; xor  ax,cx

   db $66; mov  es:[bx+4],ax
end;


{---------------------------------------------------------------------------}
procedure Sum1({$ifdef CONST} const {$else} var {$endif} X: TW64; var R: TW64); assembler;
  {-Big-Sigma-1 function, 'preproccessed' for shift counts > 31}
asm
{ R.L := ((X.L shr 14) or (X.H shl 18)) xor ((X.L shr 18) or (X.H shl 14)) xor ((X.H shr 9) or (X.L shl 23));
  R.H := ((X.H shr 14) or (X.L shl 18)) xor ((X.H shr 18) or (X.L shl 14)) xor ((X.L shr 9) or (X.H shl 23));}
           les  bx,[X]
   db $66; mov  si,es:[bx]    {X.L}
   db $66; mov  di,es:[bx+4]  {X.H}

   db $66; mov  ax,si         {(X.L shr 14) or (X.H shl 18)}
   db $66; mov  dx,di
   db $66; shr  ax,14
   db $66; shl  dx,18
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,si         {(X.L shr 18) or (X.H shl 14)}
   db $66; mov  dx,di
   db $66; shr  ax,18
   db $66; shl  dx,14
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,di         {(X.H shr 9) or (X.L shl 23)}
   db $66; mov  dx,si
   db $66; shr  ax,9
   db $66; shl  dx,23
   db $66; or   ax,dx
   db $66; xor  ax,cx

           les  bx,[R]
   db $66; mov  es:[bx],ax

   db $66; mov  ax,di         {(X.H shr 14) or (X.L shl 18)}
   db $66; mov  dx,si
   db $66; shr  ax,14
   db $66; shl  dx,18
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,di         {(X.H shr 18) or (X.L shl 14)}
   db $66; mov  dx,si
   db $66; shr  ax,18
   db $66; shl  dx,14
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,si         {(X.L shr 9) or (X.H shl 23)}
   db $66; mov  dx,di
   db $66; shr  ax,9
   db $66; shl  dx,23
   db $66; or   ax,dx
   db $66; xor  ax,cx

   db $66; mov  es:[bx+4],ax
end;


{---------------------------------------------------------------------------}
procedure Sig0({$ifdef CONST} const {$else} var {$endif} X: TW64; var R: TW64); assembler;
  {-Small-Sigma-0 function, 'preproccessed' for shift counts > 31}
asm
{ R.L := ((X.L shr 1) or (X.H shl 31)) xor ((X.L shr 8) or (X.H shl 24)) xor ((X.L shr 7) or (X.H shl 25));
  R.H := ((X.H shr 1) or (X.L shl 31)) xor ((X.H shr 8) or (X.L shl 24)) xor (X.H shr 7);}
           les  bx,[X]
   db $66; mov  si,es:[bx]    {X.L}
   db $66; mov  di,es:[bx+4]  {X.H}

   db $66; mov  ax,si         {(X.L shr 1) or (X.H shl 31)}
   db $66; mov  dx,di
   db $66; shr  ax,1
   db $66; shl  dx,31
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,si         {(X.L shr 8) or (X.H shl 24)}
   db $66; mov  dx,di
   db $66; shr  ax,8
   db $66; shl  dx,24
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,si         {(X.L shr 7) or (X.H shl 25)}
   db $66; mov  dx,di
   db $66; shr  ax,7
   db $66; shl  dx,25
   db $66; or   ax,dx
   db $66; xor  ax,cx

           les  bx,[R]
   db $66; mov  es:[bx],ax

   db $66; mov  ax,di         {(X.H shr 1) or (X.L shl 31)}
   db $66; mov  dx,si
   db $66; shr  ax,1
   db $66; shl  dx,31
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,di         {(X.H shr 8) or (X.L shl 24)}
   db $66; mov  dx,si
   db $66; shr  ax,8
   db $66; shl  dx,24
   db $66; or   ax,dx
   db $66; xor  ax,cx

   db $66; shr  di,7          {(X.H shr 7)}
   db $66; xor  ax,di

   db $66; mov  es:[bx+4],ax
end;


{---------------------------------------------------------------------------}
procedure Sig1({$ifdef CONST} const {$else} var {$endif} X: TW64; var R: TW64); assembler;
  {-Small-Sigma-1 function, 'preproccessed' for shift counts > 31}
asm
{ R.L := ((X.L shr 19) or (X.H shl 13)) xor ((X.H shr 29) or (X.L shl 3)) xor ((X.L shr 6) or (X.H shl 26));
  R.H := ((X.H shr 19) or (X.L shl 13)) xor ((X.L shr 29) or (X.H shl 3)) xor (X.H shr 6);}
           les  bx,[X]
   db $66; mov  si,es:[bx]    {X.L}
   db $66; mov  di,es:[bx+4]  {X.H}

   db $66; mov  ax,si         {(X.L shr 19) or (X.H shl 13)}
   db $66; mov  dx,di
   db $66; shr  ax,19
   db $66; shl  dx,13
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,di         {(X.H shr 29) or (X.L shl 3)}
   db $66; mov  dx,si
   db $66; shr  ax,29
   db $66; shl  dx,3
   db $66; or   ax,dx
   db $66; xor  cx,ax

   db $66; mov  ax,si         {(X.L shr 6) or (X.H shl 26)}
   db $66; mov  dx,di
   db $66; shr  ax,6
   db $66; shl  dx,26
   db $66; or   ax,dx
   db $66; xor  ax,cx

           les  bx,[R]
   db $66; mov  es:[bx],ax

   db $66; mov  ax,di         {(X.H shr 19) or (X.L shl 13)}
   db $66; mov  dx,si
   db $66; shr  ax,19
   db $66; shl  dx,13
   db $66; or   ax,dx
   db $66; mov  cx,ax

   db $66; mov  ax,si         {(X.L shr 29) or (X.H shl 3)}
   db $66; mov  dx,di
   db $66; shr  ax,29
   db $66; shl  dx,3
   db $66; or   ax,dx
   db $66; xor  ax,cx

   db $66; shr  di,6          {(X.H shr 6)}
   db $66; xor  ax,di

   db $66; mov  es:[bx+4],ax
end;


{$endif BASM16}

{$endif BIT16}


{$ifdef UseInt64}

{---------------------------------------------------------------------------}
procedure SHA512Compress(var Data: THashContext);
  {-Actual hashing function}
type
  THash64 = array[0..7] of int64;
  TBuf64  = array[0..79] of int64;
  THLA64  = array[0..79] of TW64;
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

{Use the round constant construct from non-int64 because}
{Delphi 2 does not compile even though code is not used }
{and FPC does not know int64 constants                  }

const
  KT: array[0..79] of TW64 = (
      (L:$d728ae22; H:$428a2f98), (L:$23ef65cd; H:$71374491),
      (L:$ec4d3b2f; H:$b5c0fbcf), (L:$8189dbbc; H:$e9b5dba5),
      (L:$f348b538; H:$3956c25b), (L:$b605d019; H:$59f111f1),
      (L:$af194f9b; H:$923f82a4), (L:$da6d8118; H:$ab1c5ed5),
      (L:$a3030242; H:$d807aa98), (L:$45706fbe; H:$12835b01),
      (L:$4ee4b28c; H:$243185be), (L:$d5ffb4e2; H:$550c7dc3),
      (L:$f27b896f; H:$72be5d74), (L:$3b1696b1; H:$80deb1fe),
      (L:$25c71235; H:$9bdc06a7), (L:$cf692694; H:$c19bf174),
      (L:$9ef14ad2; H:$e49b69c1), (L:$384f25e3; H:$efbe4786),
      (L:$8b8cd5b5; H:$0fc19dc6), (L:$77ac9c65; H:$240ca1cc),
      (L:$592b0275; H:$2de92c6f), (L:$6ea6e483; H:$4a7484aa),
      (L:$bd41fbd4; H:$5cb0a9dc), (L:$831153b5; H:$76f988da),
      (L:$ee66dfab; H:$983e5152), (L:$2db43210; H:$a831c66d),
      (L:$98fb213f; H:$b00327c8), (L:$beef0ee4; H:$bf597fc7),
      (L:$3da88fc2; H:$c6e00bf3), (L:$930aa725; H:$d5a79147),
      (L:$e003826f; H:$06ca6351), (L:$0a0e6e70; H:$14292967),
      (L:$46d22ffc; H:$27b70a85), (L:$5c26c926; H:$2e1b2138),
      (L:$5ac42aed; H:$4d2c6dfc), (L:$9d95b3df; H:$53380d13),
      (L:$8baf63de; H:$650a7354), (L:$3c77b2a8; H:$766a0abb),
      (L:$47edaee6; H:$81c2c92e), (L:$1482353b; H:$92722c85),
      (L:$4cf10364; H:$a2bfe8a1), (L:$bc423001; H:$a81a664b),
      (L:$d0f89791; H:$c24b8b70), (L:$0654be30; H:$c76c51a3),
      (L:$d6ef5218; H:$d192e819), (L:$5565a910; H:$d6990624),
      (L:$5771202a; H:$f40e3585), (L:$32bbd1b8; H:$106aa070),
      (L:$b8d2d0c8; H:$19a4c116), (L:$5141ab53; H:$1e376c08),
      (L:$df8eeb99; H:$2748774c), (L:$e19b48a8; H:$34b0bcb5),
      (L:$c5c95a63; H:$391c0cb3), (L:$e3418acb; H:$4ed8aa4a),
      (L:$7763e373; H:$5b9cca4f), (L:$d6b2b8a3; H:$682e6ff3),
      (L:$5defb2fc; H:$748f82ee), (L:$43172f60; H:$78a5636f),
      (L:$a1f0ab72; H:$84c87814), (L:$1a6439ec; H:$8cc70208),
      (L:$23631e28; H:$90befffa), (L:$de82bde9; H:$a4506ceb),
      (L:$b2c67915; H:$bef9a3f7), (L:$e372532b; H:$c67178f2),
      (L:$ea26619c; H:$ca273ece), (L:$21c0c207; H:$d186b8c7),
      (L:$cde0eb1e; H:$eada7dd6), (L:$ee6ed178; H:$f57d4f7f),
      (L:$72176fba; H:$06f067aa), (L:$a2c898a6; H:$0a637dc5),
      (L:$bef90dae; H:$113f9804), (L:$131c471b; H:$1b710b35),
      (L:$23047d84; H:$28db77f5), (L:$40c72493; H:$32caab7b),
      (L:$15c9bebc; H:$3c9ebe0a), (L:$9c100d4c; H:$431d67c4),
      (L:$cb3e42b6; H:$4cc5d4be), (L:$fc657e2a; H:$597f299c),
      (L:$3ad6faec; H:$5fcb6fab), (L:$4a475817; H:$6c44198c)
    );
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}

var
  i,j: integer;
  t0,t1: int64;
  A,B,C,D,E,F,G,H: int64;
  W: TBuf64;
  K: TBuf64 absolute KT;
begin
  {Assign old working hash to variables a..h}
  A := THash64(Data.Hash)[0];
  B := THash64(Data.Hash)[1];
  C := THash64(Data.Hash)[2];
  D := THash64(Data.Hash)[3];
  E := THash64(Data.Hash)[4];
  F := THash64(Data.Hash)[5];
  G := THash64(Data.Hash)[6];
  H := THash64(Data.Hash)[7];

  {Message schedule}
  {Part 1: Transfer buffer with little -> big endian conversion}
  j := 0;
  for i:=0 to 15 do begin
    {Old shl 32 version was buggy, use helper record}
    THLA64(W)[i].H := RB(THashBuf32(Data.Buffer)[j]);
    THLA64(W)[i].L := RB(THashBuf32(Data.Buffer)[j+1]);
    inc(j,2);
  end;

  {Part 2: Calculate remaining "expanded message blocks"}
  for i:=16 to 79 do begin
    W[i] :=  (((W[i-2] shr 19) or (W[i-2] shl 45)) xor ((W[i-2] shr 61) or (W[i-2] shl 3)) xor (W[i-2] shr 6))
           + W[i-7]
           + (((W[i-15] shr 1) or (W[i-15] shl 63)) xor ((W[i-15] shr 8) or (W[i-15] shl 56)) xor (W[i-15] shr 7))
           + W[i-16];
  end;

  {SHA512 compression function, partial unroll}
  {line length must be < 128 for 16bit compilers even if code is not used}

  i := 0;
  while i<79 do begin
    t0:=H+((E shr 14 or E shl 50)xor(E shr 18 or E shl 46)xor(E shr 41 or E shl 23))+((E and (F xor G)) xor G)+K[i  ]+W[i  ];
    t1:=((A shr 28 or A shl 36)xor(A shr 34 or A shl 30)xor(A shr 39 or A shl 25))+((A or B) and C or A and B);
    D :=D+t0;
    H :=t0+t1;
    t0:=G+((D shr 14 or D shl 50)xor(D shr 18 or D shl 46)xor(D shr 41 or D shl 23))+((D and (E xor F)) xor F)+K[i+1]+W[i+1];
    t1:=((H shr 28 or H shl 36)xor(H shr 34 or H shl 30)xor(H shr 39 or H shl 25))+((H or A) and B or H and A);
    C :=C+t0;
    G :=t0+t1;
    t0:=F+((C shr 14 or C shl 50)xor(C shr 18 or C shl 46)xor(C shr 41 or C shl 23))+((C and (D xor E)) xor E)+K[i+2]+W[i+2];
    t1:=((G shr 28 or G shl 36)xor(G shr 34 or G shl 30)xor(G shr 39 or G shl 25))+((G or H) and A or G and H);
    B :=B+t0;
    F :=t0+t1;
    t0:=E+((B shr 14 or B shl 50)xor(B shr 18 or B shl 46)xor(B shr 41 or B shl 23))+((B and (C xor D)) xor D)+K[i+3]+W[i+3];
    t1:=((F shr 28 or F shl 36)xor(F shr 34 or F shl 30)xor(F shr 39 or F shl 25))+((F or G) and H or F and G);
    A :=A+t0;
    E :=t0+t1;
    t0:=D+((A shr 14 or A shl 50)xor(A shr 18 or A shl 46)xor(A shr 41 or A shl 23))+((A and (B xor C)) xor C)+K[i+4]+W[i+4];
    t1:=((E shr 28 or E shl 36)xor(E shr 34 or E shl 30)xor(E shr 39 or E shl 25))+((E or F) and G or E and F);
    H :=H+t0;
    D :=t0+t1;
    t0:=C+((H shr 14 or H shl 50)xor(H shr 18 or H shl 46)xor(H shr 41 or H shl 23))+((H and (A xor B)) xor B)+K[i+5]+W[i+5];
    t1:=((D shr 28 or D shl 36)xor(D shr 34 or D shl 30)xor(D shr 39 or D shl 25))+((D or E) and F or D and E);
    G :=G+t0;
    C :=t0+t1;
    t0:=B+((G shr 14 or G shl 50)xor(G shr 18 or G shl 46)xor(G shr 41 or G shl 23))+((G and (H xor A)) xor A)+K[i+6]+W[i+6];
    t1:=((C shr 28 or C shl 36)xor(C shr 34 or C shl 30)xor(C shr 39 or C shl 25))+((C or D) and E or C and D);
    F :=F+t0;
    B :=t0+t1;
    t0:=A+((F shr 14 or F shl 50)xor(F shr 18 or F shl 46)xor(F shr 41 or F shl 23))+((F and (G xor H)) xor H)+K[i+7]+W[i+7];
    t1:=((B shr 28 or B shl 36)xor(B shr 34 or B shl 30)xor(B shr 39 or B shl 25))+((B or C) and D or B and C);
    E :=E+t0;
    A :=t0+t1;
    inc(i,8);
  end;

  {Calculate new working hash}
  inc(THash64(Data.Hash)[0], A);
  inc(THash64(Data.Hash)[1], B);
  inc(THash64(Data.Hash)[2], C);
  inc(THash64(Data.Hash)[3], D);
  inc(THash64(Data.Hash)[4], E);
  inc(THash64(Data.Hash)[5], F);
  inc(THash64(Data.Hash)[6], G);
  inc(THash64(Data.Hash)[7], H);
end;


{---------------------------------------------------------------------------}
procedure UpdateLen(var Context: THashContext; Len: longint; IsByteLen: boolean);
  {-Update 128 bit message bit length, Len = byte length if IsByteLen}
var
  t0,t1: TW64;
  i: integer;
begin
  if IsByteLen then int64(t0) := 8*int64(Len)
  else int64(t0) := int64(Len);
  t1.L := Context.MLen[0];
  t1.H := 0;
  Inc(int64(t0),int64(t1));
  Context.MLen[0] := t0.L;
  for i:=1 to 3 do begin
    if t0.H=0 then exit;
    t1.L := Context.MLen[i];
    t0.L := t0.H;
    t0.H := 0;
    Inc(int64(t0),int64(t1));
    Context.MLen[i] := t0.L;
  end;
end;


{$else}

{---------------------------------------------------------------------------}
procedure SHA512Compress(var Data: THashContext);
  {-Actual hashing function}
type
  THash64 = array[0..7] of TW64;
  TBuf64  = array[0..79] of TW64;
var
  i,j: integer;
  t,t0,t1: TW64;
  S: THash64;
  W: TBuf64;
const
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
  K: array[0..79] of TW64 = (
      (L:$d728ae22; H:$428a2f98), (L:$23ef65cd; H:$71374491),
      (L:$ec4d3b2f; H:$b5c0fbcf), (L:$8189dbbc; H:$e9b5dba5),
      (L:$f348b538; H:$3956c25b), (L:$b605d019; H:$59f111f1),
      (L:$af194f9b; H:$923f82a4), (L:$da6d8118; H:$ab1c5ed5),
      (L:$a3030242; H:$d807aa98), (L:$45706fbe; H:$12835b01),
      (L:$4ee4b28c; H:$243185be), (L:$d5ffb4e2; H:$550c7dc3),
      (L:$f27b896f; H:$72be5d74), (L:$3b1696b1; H:$80deb1fe),
      (L:$25c71235; H:$9bdc06a7), (L:$cf692694; H:$c19bf174),
      (L:$9ef14ad2; H:$e49b69c1), (L:$384f25e3; H:$efbe4786),
      (L:$8b8cd5b5; H:$0fc19dc6), (L:$77ac9c65; H:$240ca1cc),
      (L:$592b0275; H:$2de92c6f), (L:$6ea6e483; H:$4a7484aa),
      (L:$bd41fbd4; H:$5cb0a9dc), (L:$831153b5; H:$76f988da),
      (L:$ee66dfab; H:$983e5152), (L:$2db43210; H:$a831c66d),
      (L:$98fb213f; H:$b00327c8), (L:$beef0ee4; H:$bf597fc7),
      (L:$3da88fc2; H:$c6e00bf3), (L:$930aa725; H:$d5a79147),
      (L:$e003826f; H:$06ca6351), (L:$0a0e6e70; H:$14292967),
      (L:$46d22ffc; H:$27b70a85), (L:$5c26c926; H:$2e1b2138),
      (L:$5ac42aed; H:$4d2c6dfc), (L:$9d95b3df; H:$53380d13),
      (L:$8baf63de; H:$650a7354), (L:$3c77b2a8; H:$766a0abb),
      (L:$47edaee6; H:$81c2c92e), (L:$1482353b; H:$92722c85),
      (L:$4cf10364; H:$a2bfe8a1), (L:$bc423001; H:$a81a664b),
      (L:$d0f89791; H:$c24b8b70), (L:$0654be30; H:$c76c51a3),
      (L:$d6ef5218; H:$d192e819), (L:$5565a910; H:$d6990624),
      (L:$5771202a; H:$f40e3585), (L:$32bbd1b8; H:$106aa070),
      (L:$b8d2d0c8; H:$19a4c116), (L:$5141ab53; H:$1e376c08),
      (L:$df8eeb99; H:$2748774c), (L:$e19b48a8; H:$34b0bcb5),
      (L:$c5c95a63; H:$391c0cb3), (L:$e3418acb; H:$4ed8aa4a),
      (L:$7763e373; H:$5b9cca4f), (L:$d6b2b8a3; H:$682e6ff3),
      (L:$5defb2fc; H:$748f82ee), (L:$43172f60; H:$78a5636f),
      (L:$a1f0ab72; H:$84c87814), (L:$1a6439ec; H:$8cc70208),
      (L:$23631e28; H:$90befffa), (L:$de82bde9; H:$a4506ceb),
      (L:$b2c67915; H:$bef9a3f7), (L:$e372532b; H:$c67178f2),
      (L:$ea26619c; H:$ca273ece), (L:$21c0c207; H:$d186b8c7),
      (L:$cde0eb1e; H:$eada7dd6), (L:$ee6ed178; H:$f57d4f7f),
      (L:$72176fba; H:$06f067aa), (L:$a2c898a6; H:$0a637dc5),
      (L:$bef90dae; H:$113f9804), (L:$131c471b; H:$1b710b35),
      (L:$23047d84; H:$28db77f5), (L:$40c72493; H:$32caab7b),
      (L:$15c9bebc; H:$3c9ebe0a), (L:$9c100d4c; H:$431d67c4),
      (L:$cb3e42b6; H:$4cc5d4be), (L:$fc657e2a; H:$597f299c),
      (L:$3ad6faec; H:$5fcb6fab), (L:$4a475817; H:$6c44198c)
    );
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin

  {Assign old working hash to variables a..h=S[0]..S[7]}
  S := THash64(Data.Hash);

  {Message schedule}
  {Part 1: Transfer buffer with little -> big endian conversion}
  j := 0;
  for i:=0 to 15 do begin
    W[i].H:= RB(THashBuf32(Data.Buffer)[j]);
    W[i].L:= RB(THashBuf32(Data.Buffer)[j+1]);
    inc(j,2);
  end;

  {Part 2: Calculate remaining "expanded message blocks"}
  for i:=16 to 79 do begin
    {W[i]:= Sig1(W[i-2]) + W[i-7] + Sig0(W[i-15]) + W[i-16];}
    {$ifndef BIT16}
      t.L := W[i-2].L;
      t.H := W[i-2].H;
      t0.L := ((t.L shr 19) or (t.H shl 13)) xor ((t.H shr 29) or (t.L shl 3)) xor ((t.L shr 6) or (t.H shl 26));
      t0.H := ((t.H shr 19) or (t.L shl 13)) xor ((t.L shr 29) or (t.H shl 3)) xor (t.H shr 6);
      Inc64(t0,W[i-7]);
      t.L := W[i-15].L;
      t.H := W[i-15].H;
      t1.L := ((t.L shr 1) or (t.H shl 31)) xor ((t.L shr 8) or (t.H shl 24)) xor ((t.L shr 7) or (t.H shl 25));
      t1.H := ((t.H shr 1) or (t.L shl 31)) xor ((t.H shr 8) or (t.L shl 24)) xor (t.H shr 7);
      Inc64(t0,t1);
      Add64(W[i], W[i-16], t0);
    {$else}
      Sig1(W[i-2], t0);
      Inc64(t0,W[i-7]);
      Sig0(W[i-15], t);
      Inc64(t0,t);
      Add64(W[i], W[i-16], t0);
    {$endif}
  end;

  {SHA512 compression function}
  for i:=0 to 79 do begin
    {t0:= S[7] + Sum1(S[4]) + Ch(S[4],S[5],S[6]) + K[i] + W[i];}
    {$ifndef BIT16}
      t0.L := (S[4].L shr 14 or S[4].H shl 18) xor (S[4].L shr 18 or S[4].H shl 14) xor (S[4].H shr 9 or S[4].L shl 23);
      t0.H := (S[4].H shr 14 or S[4].L shl 18) xor (S[4].H shr 18 or S[4].L shl 14) xor (S[4].L shr 9 or S[4].H shl 23);
    {$else}
      Sum1(S[4],t0);
    {$endif}
    Inc64(t0, S[7]);
    t.L := ((S[5].L xor S[6].L) and S[4].L) xor S[6].L;
    t.H := ((S[5].H xor S[6].H) and S[4].H) xor S[6].H;
    Inc64(t0, t);
    Inc64(t0, K[i]);
    Inc64(t0, W[i]);

    {t1:= Sum0(S[0]) + Maj(S[0],S[1],S[2]));}
    {$ifndef BIT16}
      t1.L := (S[0].L shr 28 or S[0].H shl 4) xor (S[0].H shr 2 or S[0].L shl 30) xor (S[0].H shr 7 or S[0].L shl 25);
      t1.H := (S[0].H shr 28 or S[0].L shl 4) xor (S[0].L shr 2 or S[0].H shl 30) xor (S[0].L shr 7 or S[0].H shl 25);
    {$else}
      Sum0(S[0],t1);
    {$endif}
    t.L := ((S[0].L or S[1].L) and S[2].L) or (S[0].L and S[1].L);
    t.h := ((S[0].H or S[1].H) and S[2].H) or (S[0].H and S[1].H);

    Inc64(t1, t);
    S[7].L := S[6].L;
    S[7].H := S[6].H;
    S[6].L := S[5].L;
    S[6].H := S[5].H;
    S[5].H := S[4].H;
    S[5].L := S[4].L;
    Add64(S[4],t0, S[3]);
    S[3].L := S[2].L;
    S[3].H := S[2].H;
    S[2].L := S[1].L;
    S[2].H := S[1].H;
    S[1].L := S[0].L;
    S[1].H := S[0].H;
    Add64(S[0],t0,t1);
  end;
  {Calculate new working hash}
  for i:=0 to 7 do Inc64(THash64(Data.Hash)[i], S[i]);
end;


{---------------------------------------------------------------------------}
procedure UpdateLen(var Context: THashContext; Len: longint; IsByteLen: boolean);
  {-Update 128 bit message bit length, Len = byte length if IsByteLen}
var
  t0,t1: TW64;
  i: integer;
begin
  if IsByteLen then begin
    {Calculate bit length increment = 8*Len}
    if Len<=$0FFFFFFF then begin
      {safe to multiply without overflow}
      t0.L := 8*Len;
      t0.H := 0;
    end
    else begin
      t0.L := Len;
      t0.H := 0;
      Inc64(t0,t0);
      Inc64(t0,t0);
      Inc64(t0,t0);
    end;
  end
  else begin
    t0.L := Len;
    t0.H := 0;
  end;
  {Update 128 bit length}
  t1.L := Context.MLen[0];
  t1.H := 0;
  Inc64(t0,t1);
  Context.MLen[0] := t0.L;
  for i:=1 to 3 do begin
    {propagate carry into higher bits}
    if t0.H=0 then exit;
    t1.L := Context.MLen[i];
    t0.L := t0.H;
    t0.H := 0;
    Inc64(t0,t1);
    Context.MLen[i] := t0.L;
  end;
end;

{$endif}


{---------------------------------------------------------------------------}
procedure SHA512Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: THashState = ($f3bcc908, $6a09e667, $84caa73b, $bb67ae85,
                     $fe94f82b, $3c6ef372, $5f1d36f1, $a54ff53a,
                     $ade682d1, $510e527f, $2b3e6c1f, $9b05688c,
                     $fb41bd6b, $1f83d9ab, $137e2179, $5be0cd19);
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin
  {Clear context, buffer=0!!}
  fillchar(Context,sizeof(Context),0);
  Context.Hash := SIV;
end;


{---------------------------------------------------------------------------}
procedure SHA512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  {Update message bit length}
  UpdateLen(Context, Len, true);

  while Len > 0 do begin
    {fill block with msg data}
    Context.Buffer[Context.Index]:= pByte(Msg)^;
    inc(Ptr2Inc(Msg));
    inc(Context.Index);
    dec(Len);
    if Context.Index=SHA512_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      SHA512Compress(Context);
      while Len>=SHA512_BlockLen do begin
        move(Msg^,Context.Buffer,SHA512_BlockLen);
        SHA512Compress(Context);
        inc(Ptr2Inc(Msg),SHA512_BlockLen);
        dec(Len,SHA512_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA512Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA512UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}
var
  i: integer;
begin
  {Message padding}
  {append bits from BData and a single '1' bit}
  if (bitlen>0) and (bitlen<=7) then begin
    Context.Buffer[Context.Index]:= (BData and BitAPI_Mask[bitlen]) or BitAPI_PBit[bitlen];
    UpdateLen(Context, bitlen, false);
  end
  else Context.Buffer[Context.Index]:= $80;

  for i:=Context.Index+1 to 127 do Context.Buffer[i] := 0;
  {2. Compress if more than 448 bits, (no room for 64 bit length}
  if Context.Index>= 112 then begin
    SHA512Compress(Context);
    fillchar(Context.Buffer,sizeof(Context.Buffer),0);
  end;
  {Write 128 bit msg length into the last bits of the last block}
  {(in big endian format) and do a final compress}
  THashBuf32(Context.Buffer)[28]:= RB(Context.MLen[3]);
  THashBuf32(Context.Buffer)[29]:= RB(Context.MLen[2]);
  THashBuf32(Context.Buffer)[30]:= RB(Context.MLen[1]);
  THashBuf32(Context.Buffer)[31]:= RB(Context.MLen[0]);
  SHA512Compress(Context);
  {Hash -> Digest to little endian format}
  for i:=0 to 15 do THashDig32(Digest)[i]:= RB(Context.Hash[i xor 1]);
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure SHA512FinalBits(var Context: THashContext; var Digest: TSHA512Digest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512 calculation, clear context}
begin
  SHA512FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA512Final(var Context: THashContext; var Digest: TSHA512Digest);
  {-finalize SHA512 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, 0, 0);
  move(tmp,Digest,sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA512SelfTest: boolean;
  {-self test for string from SHA512 document}
const
  s1: string[3] = 'abc';
  s2: string[112] = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'
                   +'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';

  D1: TSHA512Digest = ($dd, $af, $35, $a1, $93, $61, $7a, $ba,
                       $cc, $41, $73, $49, $ae, $20, $41, $31,
                       $12, $e6, $fa, $4e, $89, $a9, $7e, $a2,
                       $0a, $9e, $ee, $e6, $4b, $55, $d3, $9a,
                       $21, $92, $99, $2a, $27, $4f, $c1, $a8,
                       $36, $ba, $3c, $23, $a3, $fe, $eb, $bd,
                       $45, $4d, $44, $23, $64, $3c, $e8, $0e,
                       $2a, $9a, $c9, $4f, $a5, $4c, $a4, $9f );

  D2: TSHA512Digest = ($8e, $95, $9b, $75, $da, $e3, $13, $da,
                       $8c, $f4, $f7, $28, $14, $fc, $14, $3f,
                       $8f, $77, $79, $c6, $eb, $9f, $7f, $a1,
                       $72, $99, $ae, $ad, $b6, $88, $90, $18,
                       $50, $1d, $28, $9e, $49, $00, $f7, $e4,
                       $33, $1b, $99, $de, $c4, $b5, $43, $3a,
                       $c7, $d3, $29, $ee, $b6, $dd, $26, $54,
                       $5e, $96, $e5, $5b, $87, $4b, $e9, $09 );

  D3: TSHA512Digest = ($b4, $59, $4e, $b1, $29, $59, $fc, $2e,
                       $69, $79, $b6, $78, $35, $54, $29, $9c,
                       $c0, $36, $9f, $44, $08, $3a, $8b, $09,
                       $55, $ba, $ef, $d8, $83, $0c, $da, $22,
                       $89, $4b, $0b, $46, $c0, $ed, $49, $49,
                       $0e, $39, $1a, $d9, $9a, $f8, $56, $cc,
                       $1b, $d9, $6f, $23, $8c, $7f, $2a, $17,
                       $cf, $37, $ae, $b7, $e7, $93, $39, $5a);

  D4: TSHA512Digest = ($46, $4a, $e5, $27, $7a, $3d, $9e, $35,
                       $14, $46, $90, $ac, $ef, $57, $18, $f2,
                       $17, $e3, $28, $56, $27, $26, $20, $8f,
                       $b1, $53, $bd, $31, $64, $1a, $fa, $9e,
                       $ab, $d6, $44, $90, $8d, $18, $b1, $86,
                       $68, $20, $ed, $a6, $14, $2e, $98, $37,
                       $2e, $ca, $db, $bd, $15, $53, $51, $c9,
                       $af, $c1, $8b, $17, $8f, $58, $4b, $82);
var
  Context: THashContext;
  Digest : TSHA512Digest;

  function SingleTest(s: Str127; TDig: TSHA512Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA512Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA512_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA512Init(Context);
    for i:=1 to length(s) do SHA512Update(Context,@s[i],1);
    SHA512Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA512_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  SHA512SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  SHA512Init(Context);
  SHA512FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@SHA512_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  SHA512Init(Context);
  SHA512FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@SHA512_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from SHA512 document}
  SHA512SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA512FullXL(var Digest: TSHA512Digest; Msg: pointer; Len: longint);
  {-SHA512 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA512Init(Context);
  SHA512UpdateXL(Context, Msg, Len);
  SHA512Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA512Full(var Digest: TSHA512Digest; Msg: pointer; Len: word);
  {-SHA512 of Msg with init/update/final}
begin
  SHA512FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA512Digest; var buf; bsize: word; var Err: word);
  {-SHA512 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA512_Desc, tmp, buf, bsize, Err);
  move(tmp,Digest,sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA512_Desc, sizeof(SHA512_Desc), 0);
    with SHA512_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA512_BlockLen;
       HDigestlen:= sizeof(TSHA512Digest);
       HInit     := SHA512Init;
       HFinal    := SHA512FinalEx;
       HUpdateXL := SHA512UpdateXL;
       HAlgNum   := longint(_SHA512);
       HName     := 'SHA512';
       HPtrOID   := @SHA512_OID;
       HLenOID   := 9;
       HFinalBit := SHA512FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA512, @SHA512_Desc);
end.
