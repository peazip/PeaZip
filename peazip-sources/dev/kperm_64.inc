{KeccakF[1600] state permutation for 32+ bit compilers with RotL as function }

{Note: This will compile with all compilers which do support int64, but it is}
{better than the 32 bit code only if HAS_INLINE is available, 64-bit code is }
{produced and executed on 64-bit OS. Pascal translation based on the C++ code}
{keccak.cpp from the Botan library available from http://botan.randombit.net/}

{$define USE_LOCALA}  {With FPC64/WIN64 about 20% faster}

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
function RotL(x: u64bit; c: integer): u64bit; {$ifdef HAS_INLINE} inline; {$endif}
begin
  RotL := (x shl c) or (x shr (64-c));
end;


{---------------------------------------------------------------------------}
procedure KeccakPermutation(var state: TState_L);
{$ifdef USE_LOCALA}
 var A: array[0..24] of u64bit;
{$else}
 var A: packed array[0..24] of u64bit absolute state;
{$endif}
var
  B: array[0..24] of u64bit;
  C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: u64bit;
  i: integer;
begin
  {$ifdef USE_LOCALA}
    TState_L(A) := state;
  {$endif}
  for i:=0 to 23 do begin
    C0 := A[00] xor A[05] xor A[10] xor A[15] xor A[20];
    C1 := A[01] xor A[06] xor A[11] xor A[16] xor A[21];
    C2 := A[02] xor A[07] xor A[12] xor A[17] xor A[22];
    C3 := A[03] xor A[08] xor A[13] xor A[18] xor A[23];
    C4 := A[04] xor A[09] xor A[14] xor A[19] xor A[24];

    D0 := RotL(C0, 1) xor C3;
    D1 := RotL(C1, 1) xor C4;
    D2 := RotL(C2, 1) xor C0;
    D3 := RotL(C3, 1) xor C1;
    D4 := RotL(C4, 1) xor C2;

    B[00] := A[00] xor D1;
    B[01] := RotL(A[06] xor D2, 44);
    B[02] := RotL(A[12] xor D3, 43);
    B[03] := RotL(A[18] xor D4, 21);
    B[04] := RotL(A[24] xor D0, 14);
    B[05] := RotL(A[03] xor D4, 28);
    B[06] := RotL(A[09] xor D0, 20);
    B[07] := RotL(A[10] xor D1, 3);
    B[08] := RotL(A[16] xor D2, 45);
    B[09] := RotL(A[22] xor D3, 61);
    B[10] := RotL(A[01] xor D2, 1);
    B[11] := RotL(A[07] xor D3, 6);
    B[12] := RotL(A[13] xor D4, 25);
    B[13] := RotL(A[19] xor D0, 8);
    B[14] := RotL(A[20] xor D1, 18);
    B[15] := RotL(A[04] xor D0, 27);
    B[16] := RotL(A[05] xor D1, 36);
    B[17] := RotL(A[11] xor D2, 10);
    B[18] := RotL(A[17] xor D3, 15);
    B[19] := RotL(A[23] xor D4, 56);
    B[20] := RotL(A[02] xor D3, 62);
    B[21] := RotL(A[08] xor D4, 55);
    B[22] := RotL(A[14] xor D0, 39);
    B[23] := RotL(A[15] xor D1, 41);
    B[24] := RotL(A[21] xor D2, 2);

    A[00] := B[00] xor ((not B[01]) and B[02]);
    A[01] := B[01] xor ((not B[02]) and B[03]);
    A[02] := B[02] xor ((not B[03]) and B[04]);
    A[03] := B[03] xor ((not B[04]) and B[00]);
    A[04] := B[04] xor ((not B[00]) and B[01]);
    A[05] := B[05] xor ((not B[06]) and B[07]);
    A[06] := B[06] xor ((not B[07]) and B[08]);
    A[07] := B[07] xor ((not B[08]) and B[09]);
    A[08] := B[08] xor ((not B[09]) and B[05]);
    A[09] := B[09] xor ((not B[05]) and B[06]);
    A[10] := B[10] xor ((not B[11]) and B[12]);
    A[11] := B[11] xor ((not B[12]) and B[13]);
    A[12] := B[12] xor ((not B[13]) and B[14]);
    A[13] := B[13] xor ((not B[14]) and B[10]);
    A[14] := B[14] xor ((not B[10]) and B[11]);
    A[15] := B[15] xor ((not B[16]) and B[17]);
    A[16] := B[16] xor ((not B[17]) and B[18]);
    A[17] := B[17] xor ((not B[18]) and B[19]);
    A[18] := B[18] xor ((not B[19]) and B[15]);
    A[19] := B[19] xor ((not B[15]) and B[16]);
    A[20] := B[20] xor ((not B[21]) and B[22]);
    A[21] := B[21] xor ((not B[22]) and B[23]);
    A[22] := B[22] xor ((not B[23]) and B[24]);
    A[23] := B[23] xor ((not B[24]) and B[20]);
    A[24] := B[24] xor ((not B[20]) and B[21]);
    A[00] := A[00] xor RC[i];
  end;
  {$ifdef USE_LOCALA}
    state := TState_L(A);
  {$endif}
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

