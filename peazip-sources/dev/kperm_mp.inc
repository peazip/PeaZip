{KeccakF[1600] state permutation for 32 bit compilers using MMX.  }

{The MMX code was developed by Anna Kaliszewicz, tweaked and con- }
{tributed by payl with Anna's permission. It is used if the symbol}
{USE_MMX_AKP is defined in unit sha3. Usable for FPC and Delphi 6+}
{For the variables, structure, origin etc see also kperm_64.inc.  }

{$ifdef HAS_UINT64}
  type u64bit = uint64;
{$else}
  type u64bit = int64;
{$endif}

type
  pu64bit = ^u64bit;

const
  RC: array[0..23] of u64bit = (
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
procedure KeccakPermutation(var A: TState_L);
var
  C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: u64bit;
  B: array[0..24] of u64bit;
begin
  {$ifdef FPC}
    {$ASMMODE INTEL}
  {$endif}
  asm
    push   ebx
    mov    eax,A
    lea    ebx,B
    {WE note: eax and ebx point roughly to the middle of the arrays}
    {in order to make address displacements fall into the -128..127}
    {range, which generates shorter opcodes and better caching. The}
    {code is interleaved heavily to increase execution performance.}
    add    eax,128
    add    ebx,128
    mov    ecx,0
  @@ptl:
    movq   mm0,[eax -128]
    pxor   mm0,[eax -88]
    pxor   mm0,[eax -48]
    movq   mm1,[eax -120]
    pxor   mm0,[eax -8]
    pxor   mm0,[eax +32]
    movq   C0, mm0
    movq   mm2,[eax -112]
    pxor   mm2,[eax -72]
    pxor   mm1,[eax -80]
    pxor   mm1,[eax -40]
    pxor   mm1,[eax +0]
    pxor   mm2,[eax -32]
    pxor   mm2,[eax +8]
    pxor   mm1,[eax +40]
    movq   C1, mm1
    movq   mm3,[eax -104]
    pxor   mm3,[eax -64]
    pxor   mm2,[eax +48]
    pxor   mm3,[eax -24]
    pxor   mm3,[eax +16]
    movq   mm4,[eax -96]
    pxor   mm3,[eax +56]
    pxor   mm4,[eax -56]
    pxor   mm4,[eax -16]
    movq   C2, mm2
    movq   C3, mm3
    pxor   mm4,[eax +24]
    pxor   mm4,[eax +64]
    movq   C4, mm4
    movq   mm5,C0
    movq   mm6, mm5
    psrlq  mm6, 63
    psllq  mm5, 1
    por    mm5, mm6
    pxor   mm5,C3
    movq   mm7,C1
    movq   D0, mm5
    movq   mm0, mm7
    psllq  mm7, 1
    psrlq  mm0, 63
    por    mm7, mm0
    pxor   mm7,C4
    movq   mm1,C2
    movq   D1, mm7
    movq   mm3,C3
    movq   mm2, mm1
    psllq  mm1, 1
    psrlq  mm2, 63
    por    mm1, mm2
    pxor   mm1,C0
    movq   D2, mm1
    movq   mm4, mm3
    psrlq  mm4, 63
    psllq  mm3, 1
    por    mm3, mm4
    pxor   mm3,C1
    movq   D3, mm3
    movq   mm5,C4
    movq   mm6, mm5
    psllq  mm5, 1
    movq   mm7,[eax -128]
    psrlq  mm6, 63
    por    mm5, mm6
    pxor   mm5,C2
    movq   D4, mm5
    pxor   mm7,D1
    movq   mm0,[eax -80]
    pxor   mm0,D2
    movq   mm1, mm0
    psrlq  mm1, 20
    movq   mm2,[eax -32]
    psllq  mm0, 44
    pxor   mm2,D3
    movq   [ebx -128], mm7
    movq   mm3, mm2
    por    mm0, mm1
    psllq  mm2, 43
    psrlq  mm3, 21
    movq   mm4,[eax +16]
    movq   [ebx -120], mm0
    por    mm2, mm3
    pxor   mm4,D4
    movq   [ebx -112], mm2
    movq   mm5, mm4
    movq   mm6,[eax +64]
    psllq  mm4, 21
    psrlq  mm5, 43
    pxor   mm6,D0
    por    mm4, mm5
    movq   [ebx -104], mm4
    movq   mm7, mm6
    psllq  mm6, 14
    psrlq  mm7, 50
    por    mm6, mm7
    movq   [ebx -96], mm6
    movq   mm0,[eax -104]
    pxor   mm0,D4
    movq   mm1, mm0
    psllq  mm0, 28
    psrlq  mm1, 36
    por    mm0, mm1
    movq   [ebx -88], mm0
    movq   mm2,[eax -56]
    pxor   mm2,D0
    movq   mm3, mm2
    psllq  mm2, 20
    psrlq  mm3, 44
    por    mm2, mm3
    movq   mm4,[eax -48]
    pxor   mm4,D1
    movq   [ebx -80], mm2
    movq   mm5, mm4
    psllq  mm4, 3
    psrlq  mm5, 61
    por    mm4, mm5
    movq   [ebx -72], mm4
    movq   mm6,[eax +0]
    pxor   mm6,D2
    movq   mm7, mm6
    psllq  mm6, 45
    psrlq  mm7, 19
    por    mm6, mm7
    movq   mm0,[eax +48]
    movq   [ebx -64], mm6
    movq   mm2,[eax -120]
    pxor   mm0,D3
    movq   mm1, mm0
    psllq  mm0, 61
    pxor   mm2,D2
    movq   mm3, mm2
    psrlq  mm3, 63
    psrlq  mm1, 3
    psllq  mm2, 1
    por    mm0, mm1
    por    mm2, mm3
    movq   [ebx -56], mm0
    movq   [ebx -48], mm2
    movq   mm4,[eax -72]
    pxor   mm4,D3
    movq   mm5, mm4
    psllq  mm4, 6
    psrlq  mm5, 58
    por    mm4, mm5
    movq   mm6,[eax -24]
    pxor   mm6,D4
    movq   [ebx -40], mm4
    movq   mm7, mm6
    psrlq  mm7, 39
    psllq  mm6, 25
    por    mm6, mm7
    movq   [ebx -32], mm6
    movq   mm0,[eax +24]
    movq   mm2,[eax +32]
    pxor   mm0,D0
    movq   mm1, mm0
    psllq  mm0, 8
    pxor   mm2,D1
    psrlq  mm1, 56
    por    mm0, mm1
    movq   [ebx -24], mm0
    movq   mm3, mm2
    psllq  mm2, 18
    psrlq  mm3, 46
    por    mm2, mm3
    movq   [ebx -16], mm2
    movq   mm4,[eax -96]
    pxor   mm4,D0
    movq   mm5, mm4
    movq   mm6,[eax -88]
    psllq  mm4, 27
    pxor   mm6,D1
    psrlq  mm5, 37
    movq   mm7, mm6
    psrlq  mm7, 28
    psllq  mm6, 36
    movq   mm0,[eax -40]
    por    mm6, mm7
    pxor   mm0,D2
    por    mm4, mm5
    movq   [ebx -8], mm4
    movq   [ebx +0], mm6
    movq   mm1, mm0
    movq   mm2,[eax +8]
    pxor   mm2,D3
    psrlq  mm1, 54
    psllq  mm0, 10
    por    mm0, mm1
    movq   [ebx +8], mm0
    movq   mm3, mm2
    movq   mm4,[eax +56]
    psrlq  mm3, 49
    pxor   mm4,D4
    psllq  mm2, 15
    movq   mm5, mm4
    por    mm2, mm3
    movq   [ebx +16], mm2
    psllq  mm4, 56
    movq   mm6,[eax -112]
    psrlq  mm5, 8
    pxor   mm6,D3
    movq   mm7, mm6
    por    mm4, mm5
    movq   [ebx +24], mm4
    psrlq  mm7, 2
    psllq  mm6, 62
    movq   mm0,[eax -64]
    por    mm6, mm7
    movq   [ebx +32], mm6
    pxor   mm0,D4
    movq   mm1, mm0
    psllq  mm0, 55
    psrlq  mm1, 9
    por    mm0, mm1
    movq   mm2,[eax -16]
    movq   [ebx +40], mm0
    pxor   mm2,D0
    movq   mm3, mm2
    psllq  mm2, 39
    psrlq  mm3, 25
    por    mm2, mm3
    movq   mm4,[eax -8]
    pxor   mm4,D1
    movq   [ebx +48], mm2
    movq   mm5, mm4
    movq   mm6,[eax +40]
    psrlq  mm5, 23
    psllq  mm4, 41
    pxor   mm6,D2
    por    mm4, mm5
    movq   mm7, mm6
    psrlq  mm7, 62
    psllq  mm6, 2
    por    mm6, mm7
    movq   [ebx +56], mm4
    movq   [ebx +64], mm6
    movq   mm0,[ebx -120]
    movq   mm1,[ebx -112]
    pandn  mm1,[ebx -104]
    movq   mm2,[ebx -104]
    pxor   mm1,[ebx -120]
    pandn  mm0,[ebx -112]
    pxor   mm0,[ebx -128]
    movq   [eax -128], mm0
    movq   [eax -120], mm1
    movq   mm3,[ebx -96]
    pandn  mm2,[ebx -96]
    pxor   mm2,[ebx -112]
    pandn  mm3,[ebx -128]
    movq   mm4,[ebx -128]
    pandn  mm4,[ebx -120]
    movq   [eax -112], mm2
    pxor   mm3,[ebx -104]
    movq   mm5,[ebx -80]
    pxor   mm4,[ebx -96]
    pandn  mm5,[ebx -72]
    movq   [eax -104], mm3
    movq   [eax -96], mm4
    pxor   mm5,[ebx -88]
    movq   mm7,[ebx -64]
    movq   [eax -88], mm5
    movq   mm6,[ebx -72]
    pandn  mm6,[ebx -64]
    pxor   mm6,[ebx -80]
    pandn  mm7,[ebx -56]
    movq   [eax -80], mm6
    pxor   mm7,[ebx -72]
    movq   [eax -72], mm7
    movq   mm0,[ebx -56]
    pandn  mm0,[ebx -88]
    movq   mm2,[ebx -40]
    pxor   mm0,[ebx -64]
    movq   mm1,[ebx -88]
    movq   [eax -64], mm0
    pandn  mm1,[ebx -80]
    pxor   mm1,[ebx -56]
    movq   mm3,[ebx -32]
    movq   mm4,[ebx -24]
    pandn  mm2,[ebx -32]
    pandn  mm3,[ebx -24]
    movq   [eax -56], mm1
    pandn  mm4,[ebx -16]
    pxor   mm2,[ebx -48]
    movq   [eax -48], mm2
    pxor   mm3,[ebx -40]
    movq   [eax -40], mm3
    movq   mm5,[ebx -16]
    pandn  mm5,[ebx -48]
    pxor   mm4,[ebx -32]
    movq   mm6,[ebx -48]
    pandn  mm6,[ebx -40]
    pxor   mm5,[ebx -24]
    movq   [eax -32], mm4
    pxor   mm6,[ebx -16]
    movq   [eax -24], mm5
    movq   [eax -16], mm6
    movq   mm7,[ebx +0]
    movq   mm0,[ebx +8]
    pandn  mm0,[ebx +16]
    pandn  mm7,[ebx +8]
    pxor   mm7,[ebx -8]
    movq   [eax -8], mm7
    movq   mm2,[ebx +24]
    movq   mm1,[ebx +16]
    pandn  mm1,[ebx +24]
    pxor   mm1,[ebx +8]
    pxor   mm0,[ebx +0]
    pandn  mm2,[ebx -8]
    movq   [eax +0], mm0
    pxor   mm2,[ebx +16]
    movq   [eax +8], mm1
    movq   mm3,[ebx -8]
    pandn  mm3,[ebx +0]
    pxor   mm3,[ebx +24]
    movq   [eax +16], mm2
    movq   mm4,[ebx +40]
    movq   [eax +24], mm3
    pandn  mm4,[ebx +48]
    movq   mm5,[ebx +48]
    pandn  mm5,[ebx +56]
    pxor   mm5,[ebx +40]
    pxor   mm4,[ebx +32]
    movq   [eax +40], mm5
    movq   mm7,[ebx +64]
    movq   mm6,[ebx +56]
    pandn  mm6,[ebx +64]
    pandn  mm7,[ebx +32]
    pxor   mm7,[ebx +56]
    pxor   mm6,[ebx +48]
    movq   [eax +32], mm4
    movq   mm0,[ebx +32]
    movq   [eax +48], mm6
    lea    edx, RC
    pandn  mm0,[ebx +40]
    pxor   mm0,[ebx +64]
    movq   [eax +64], mm0
    movq   [eax +56], mm7
    movq   mm1,[eax -128]
    pxor   mm1, [edx+ecx*8]
    movq   [eax -128], mm1
    inc    ecx
    cmp    ecx, 23
    jbe    @@ptl
    pop    ebx
    emms
  end;
end;


{---------------------------------------------------------------------------}
procedure extractFromState(outp: pointer; const state: TState_L; laneCount: integer);
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
procedure xorIntoState(var state: TState_L; inp: PLongint; laneCount: integer);
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
