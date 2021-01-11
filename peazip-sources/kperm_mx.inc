{KeccakF[1600] state permutation for 32 bit compilers with MMX/ASM support}
{Compiled from Pascal source to MMX with ad-hoc compiler by Eric Grange.  }
{Slightly changed by WE to make it compatible with plain D6+ and FPC.     }

{This code is used if the symbol USE_MMCODE is defined in unit sha3, FPC  }
{and Delphi 6+. For the variables, structure etc see also kperm_64.inc.   }

{$ifdef HAS_UINT64}
  type u64bit = uint64;
{$else}
  type u64bit = int64;
{$endif}

{$ifdef FPC}
  {$ASMMODE INTEL}
{$endif}

type
  pu64bit = ^u64bit;

const
  cRoundConstants: array[0..23] of u64bit = (
     u64bit($0000000000000001), u64bit($0000000000008082),
     u64bit($800000000000808A), u64bit($8000000080008000),
     u64bit($000000000000808B), u64bit($0000000080000001),
     u64bit($8000000080008081), u64bit($8000000000008009),
     u64bit($000000000000008A), u64bit($0000000000000088),
     u64bit($0000000080008009), u64bit($000000008000000A),
     u64bit($000000008000808B), u64bit($800000000000008B),
     u64bit($8000000000008089), u64bit($8000000000008003),
     u64bit($8000000000008002), u64bit($8000000000000080),
     u64bit($000000000000800A), u64bit($800000008000000A),
     u64bit($8000000080008081), u64bit($8000000000008080),
     u64bit($0000000080000001), u64bit($8000000080008008));


{---------------------------------------------------------------------------}
procedure KeccakPermutationKernel(B, A, C: pointer);
begin
 asm
   mov   eax, [B]
   mov   edx, [A]
   mov   ecx, [C]
   add   edx, 128
   add   eax, 128
   // Theta
   movq  mm2, [edx-112]
   movq  mm0, [edx-128]
   pxor  mm0, [edx-88]
   pxor  mm0, [edx-48]
   pxor  mm0, [edx-8]
   pxor  mm0, [edx+32]
   movq  mm1, [edx-120]
   movq  [ecx], mm0
   pxor  mm2, [edx-72]
   pxor  mm2, [edx-32]
   pxor  mm2, [edx+8]
   pxor  mm2, [edx+48]
   movq  mm3, [edx-104]
   movq  [ecx+16], mm2
   pxor  mm1, [edx-80]
   pxor  mm1, [edx-40]
   pxor  mm1, [edx]
   pxor  mm1, [edx+40]
   movq  mm4, [edx-96]
   movq  [ecx+8], mm1
   pxor  mm3, [edx-64]
   pxor  mm3, [edx-24]
   pxor  mm3, [edx+16]
   pxor  mm3, [edx+56]
   movq  [ecx+24], mm3
   pxor  mm4, [edx-56]
   pxor  mm4, [edx-16]
   pxor  mm4, [edx+24]
   pxor  mm4, [edx+64]
   movq  [ecx+32], mm4
   movq  mm6, mm2
   psllq mm2, 1
   psrlq mm6, 63
   por   mm2, mm6
   pxor  mm2, mm0
   movq  mm7, mm0
   psllq mm0, 1
   psrlq mm7, 63
   por   mm0, mm7
   pxor  mm0, mm3
   movq  mm5, mm3
   psllq mm3, 1
   psrlq mm5, 63
   por   mm3, mm5
   pxor  mm3, mm1
   movq  mm6, mm1
   psllq mm1, 1
   psrlq mm6, 63
   por   mm1, mm6
   pxor  mm1, mm4
   movq  mm7, mm4
   psllq mm4, 1
   psrlq mm7, 63
   por   mm4, mm7
   pxor  mm4, [ecx+16]
   // Rho Pi
   movq  mm5, [edx-24]
   movq  mm6, [edx-80]
   pxor  mm6, mm2
   movq  mm7, mm6
   psllq mm6, 44
   psrlq mm7, 20
   por   mm6, mm7
   movq  [eax-120], mm6
   movq  mm6, [edx+24]
   pxor  mm5, mm4
   movq  mm7, mm5
   psllq mm5, 25
   psrlq mm7, 39
   por   mm5, mm7
   movq  [eax-32], mm5
   movq  mm5, [edx-96]
   pxor  mm6, mm0
   movq  mm7, mm6
   psllq mm6, 8
   psrlq mm7, 56
   por   mm6, mm7
   movq  [eax-24], mm6
   movq  mm6, [edx-64]
   pxor  mm5, mm0
   movq  mm7, mm5
   psllq mm5, 27
   psrlq mm7, 37
   por   mm5, mm7
   movq  [eax-8], mm5
   pxor  mm6, mm4
   movq  mm7, mm6
   psllq mm6, 55
   psrlq mm7, 9
   por   mm6, mm7
   movq  mm5, [edx]
   movq  [eax+40], mm6
   movq  mm6, [edx-128]
   pxor  mm6, mm1
   movq  [eax-128], mm6
   movq  mm6, [edx-88]
   pxor  mm5, mm2
   movq  mm7, mm5
   psllq mm5, 45
   psrlq mm7, 19
   por   mm5, mm7
   movq  [eax-64], mm5
   movq  mm5, [edx-16]
   pxor  mm6, mm1
   movq  mm7, mm6
   psllq mm6, 36
   psrlq mm7, 28
   por   mm6, mm7
   movq  [eax], mm6
   movq  mm6, [edx+8]
   pxor  mm5, mm0
   movq  mm7, mm5
   psllq mm5, 39
   psrlq mm7, 25
   por   mm5, mm7
   movq  [eax+48], mm5
   movq  mm5, [edx-104]
   pxor  mm6, mm3
   movq  mm7, mm6
   psllq mm6, 15
   psrlq mm7, 49
   por   mm6, mm7
   movq  [eax+16], mm6
   movq  mm6, [edx-120]
   pxor  mm5, mm4
   movq  mm7, mm5
   psllq mm5, 28
   psrlq mm7, 36
   por   mm5, mm7
   movq  [eax-88], mm5
   movq  mm5, [edx-48]
   pxor  mm6, mm2
   movq  mm7, mm6
   psllq mm6, 1
   psrlq mm7, 63
   por   mm6, mm7
   movq  [eax-48], mm6
   movq  mm6, [edx+56]
   pxor  mm5, mm1
   movq  mm7, mm5
   psllq mm5, 3
   psrlq mm7, 61
   por   mm5, mm7
   movq  [eax-72], mm5
   movq  mm5, [edx+16]
   pxor  mm6, mm4
   movq  mm7, mm6
   psllq mm6, 56
   psrlq mm7, 8
   por   mm6, mm7
   movq  [eax+24], mm6
   movq  mm6, [edx+32]
   pxor  mm5, mm4
   movq  mm7, mm5
   psllq mm5, 21
   psrlq mm7, 43
   por   mm5, mm7
   movq  [eax-104], mm5
   movq  mm5, [edx+40]
   pxor  mm6, mm1
   movq  mm7, mm6
   psllq mm6, 18
   psrlq mm7, 46
   por   mm6, mm7
   movq  [eax-16], mm6
   movq  mm6, [edx-72]
   pxor  mm5, mm2
   movq  mm7, mm5
   psllq mm5, 2
   psrlq mm7, 62
   por   mm5, mm7
   movq  [eax+64], mm5
   movq  mm5, [edx-40]
   pxor  mm6, mm3
   movq  mm7, mm6
   psllq mm6, 6
   psrlq mm7, 58
   por   mm6, mm7
   movq  [eax-40], mm6
   movq  mm6, [edx-56]
   pxor  mm5, mm2
   movq  mm7, mm5
   psllq mm5, 10
   psrlq mm7, 54
   por   mm5, mm7
   movq  [eax+8], mm5
   movq  mm5, [edx-112]
   pxor  mm6, mm0
   movq  mm7, mm6
   psllq mm6, 20
   psrlq mm7, 44
   por   mm6, mm7
   movq  [eax-80], mm6
   movq  mm6, [edx+48]
   pxor  mm5, mm3
   movq  mm7, mm5
   psllq mm5, 62
   psrlq mm7, 2
   por   mm5, mm7
   movq  [eax+32], mm5
   movq  mm5, [edx-8]
   pxor  mm6, mm3
   movq  mm7, mm6
   psllq mm6, 61
   psrlq mm7, 3
   por   mm6, mm7
   movq  [eax-56], mm6
   movq  mm6, [edx+64]
   pxor  mm5, mm1
   movq  mm7, mm5
   psllq mm5, 41
   psrlq mm7, 23
   por   mm5, mm7
   movq  [eax+56], mm5
   movq  mm5, [edx-32]
   pxor  mm6, mm0
   movq  mm7, mm6
   psllq mm6, 14
   psrlq mm7, 50
   por   mm6, mm7
   movq  [eax-96], mm6
   pxor  mm5, mm3
   movq  mm7, mm5
   psllq mm5, 43
   psrlq mm7, 21
   por   mm5, mm7
   movq  [eax-112], mm5
   // Chi
   movq  mm4, [eax-128]
   movq  mm2, [eax-104]
   movq  mm1, mm5
   movq  mm3, mm6
   pandn mm3, mm4
   pxor  mm3, mm2
   movq  mm0, [eax-120]
   movq  [edx-104], mm3
   pandn mm1, mm2
   pxor  mm1, mm0
   movq  [edx-120], mm1
   pandn mm2, mm6
   pxor  mm2, mm5
   movq  [edx-112], mm2
   pandn mm0, mm5
   pxor  mm0, mm4
   movq  mm5, [eax-80]
   movq  [edx-128], mm0
   pandn mm4, [eax-120]
   pxor  mm4, mm6
   movq  mm0, [eax-56]
   movq  mm6, [eax-72]
   movq  mm1, [eax-88]
   movq  [edx-96], mm4
   pandn mm5, mm6
   pxor  mm5, mm1
   movq  mm7, [eax-64]
   movq  [edx-88], mm5
   pandn mm0, mm1
   pxor  mm0, mm7
   movq  [edx-64], mm0
   pandn mm6, mm7
   pxor  mm6, [eax-80]
   movq  mm5, [eax-16]
   movq  [edx-80], mm6
   pandn mm1, [eax-80]
   pxor  mm1, [eax-56]
   movq  mm6, [eax-48]
   movq  mm4, [eax-24]
   movq  [edx-56], mm1
   pandn mm5, mm6
   pxor  mm5, mm4
   movq  mm3, [eax-32]
   movq  [edx-24], mm5
   pandn mm7, [eax-56]
   pxor  mm7, [eax-72]
   movq  mm2, [eax-40]
   movq  [edx-72], mm7
   pandn mm3, mm4
   pxor  mm3, mm2
   movq  mm0, [eax+8]
   movq  [edx-40], mm3
   pandn mm2, [eax-32]
   pxor  mm2, mm6
   movq  mm7, [eax]
   movq  mm1, [eax+16]
   movq  [edx-48], mm2
   pandn mm0, mm1
   pxor  mm0, mm7
   movq  [edx], mm0
   pandn mm6, [eax-40]
   pxor  mm6, [eax-16]
   movq  mm3, [eax-8]
   movq  [edx-16], mm6
   movq  mm6, [eax+56]
   pandn mm7, [eax+8]
   pxor  mm7, mm3
   movq  mm5, [eax+48]
   movq  [edx-8], mm7
   movq  mm7, [eax+64]
   pandn mm6, mm7
   pxor  mm6, mm5
   movq  [edx+48], mm6
   pandn mm4, [eax-16]
   pxor  mm4, [eax-32]
   movq  mm2, [eax+24]
   movq  [edx-32], mm4
   pandn mm3, [eax]
   pxor  mm3, mm2
   movq  mm4, [eax+40]
   movq  mm0, [eax+32]
   movq  [edx+24], mm3
   pandn mm7, mm0
   pxor  mm7, [eax+56]
   movq  [edx+56], mm7
   pandn mm4, mm5
   pxor  mm4, mm0
   movq  [edx+32], mm4
   pandn mm1, mm2
   pxor  mm1, [eax+8]
   movq  [edx+8], mm1
   pandn mm0, [eax+40]
   pxor  mm0, [eax+64]
   movq  [edx+64], mm0
   pandn mm5, [eax+56]
   pxor  mm5, [eax+40]
   movq  [edx+40], mm5
   pandn mm2, [eax-8]
   pxor  mm2, [eax+16]
   movq  [edx+16], mm2
 end;
end;


{---------------------------------------------------------------------------}
procedure EMMS;
begin
 asm
   emms
 end;
end;


{---------------------------------------------------------------------------}
procedure KeccakPermutation(var state: TState_L);
var
  A: u64bit absolute state;
  B: array[0..24] of u64bit;
  C: array[0..4] of u64bit;
  i: integer;
begin
  for i:=0 to 23 do begin
    KeccakPermutationKernel(@B, @A, @C);
    A := A xor cRoundConstants[i];
  end;
  EMMS;
end;


{---------------------------------------------------------------------------}
procedure ExtractFromState(outp: pointer; const state: TState_L; laneCount: integer);
var
  pI, pS: pu64bit;
  i: integer;
begin
  pI := outp;
  pS := @state[0];
  for i:=laneCount-1 downto 0 do begin
    pI^ := pS^;
    inc(pI);
    inc(pS);
  end;
end;


{---------------------------------------------------------------------------}
procedure XorIntoState(var state: TState_L; inp: PLongint; laneCount: integer);
  {-Include input message data bits into the sponge state}
var
  pI, pS: pu64bit;
  i: integer;
begin
  pI := pu64bit(inp);
  pS := @state[0];
  for i:=laneCount-1 downto 0 do begin
    pS^ := pS^ xor pI^;
    inc(pI);
    inc(pS);
  end;
end;
