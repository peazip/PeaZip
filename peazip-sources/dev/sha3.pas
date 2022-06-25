unit SHA3;

{SHA3 functions (including SHAKE) based on Keccak}

interface

{$i STD.INC}

{.$define USE_64BITCODE}   {Use 64-bit for Keccak permutation}
{.$define USE_MMXCODE  }   {Use MMX for Keccak permutation, contributed by Eric Grange}
{.$define USE_MMX_AKP  }   {Use MMX for Keccak permutation, contributed by Anna Kaliszewicz / payl}

(*************************************************************************

 DESCRIPTION     :  SHA3 functions (including SHAKE) based on Keccak

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  SHA3:
                      FIPS 202 SHA-3 Standard: 'Permutation-Based Hash and
                      Extendable-Output Functions' available from
                      http://csrc.nist.gov/publications/PubsFIPS.html or
                      http://dx.doi.org/10.6028/NIST.FIPS.202 or
                      http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
                    Keccak:
                      https://github.com/gvanas/KeccakCodePackage
                      http://keccak.noekeon.org/KeccakReferenceAndOptimized-3.2.zip
                      http://keccak.noekeon.org/KeccakKAT-3.zip   (17MB)
                      http://csrc.nist.gov/groups/ST/hash/documents/SHA3-C-API.pdf

 REMARKS         :  1. For 32-bit compilers with int64 (FPC, D6+) there are
                       conditional defines to optionally use MMX or 64-bit code.
                    2. The current implementation needs little-endian machines

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.01     17.10.12  W.Ehrhardt  Initial BP7 version from Keccak-simple32BI.c
 0.02     18.10.12  we          Fixed buf in xorIntoState
 0.03     18.10.12  we          Other compilers
 0.04     19.10.12  we          Separate unit
 0.05     20.10.12  we          Functions from KeccakSponge
 0.06     21.10.12  we          Functions from KeccakNISTInterface
 0.07     21.10.12  we          D2-D6 with ASM RotL function
 0.08     22.10.12  we          Include files keccperm.i16 and .i32
 0.09     22.10.12  we          __P2I type casts removed
 0.10     22.10.12  we          References, comments, remarks
 0.11     25.10.12  we          Make partialBlock longint
 0.12     30.10.12  we          Packed arrays, type TKDQueue
 0.13     31.10.12  we          Partially unrolled 64-bit code from Keccak-inplace.c
 0.14     01.11.12  we          Compact 64-bit code from Botan
 0.15     02.11.12  we          64-bit code about 20% faster with local data
 0.16     09.11.12  we          KeccakFullBytes, TKeccakMaxDigest
 0.17     12.11.12  we          USE32BIT forces skipping of 64-bit code

 0.18     12.04.14  we          Unit renamed to SHA3, SHA3 type definitions
 0.19     12.04.14  we          SHA3_Init, SHA3_Update, SHA3_FinalEx
 0.20     13.04.14  we          SHA3_UpdateXL, SHA3_FinalHash, byte sized messages work
 0.21     14.04.14  we          LSB bit sized messages, SHA3_FinalBit_LSB, working SHAKE
 0.22     11.05.14  we          Fix duplicate return result and a few typos

 0.23     08.08.15  we          TSpongeState with words and Fill3, assert HASHCTXSIZE
 0.24     09.08.15  we          SHA3_FinalBit update final bits in MSB format
 0.25     09.08.15  we          Removed unused Keccak leftovers
 0.26     09.08.15  we          Error field in context, rewrite error handling
 0.27     16.08.15  we          Some code cleanup
 0.28     17.08.15  we          Updated references
 0.29     26.08.15  we          $defines USE_64BITCODE, USE_MMXCODE
 0.30     23.04.16  we          USE_MMX_AKP

 0.31     15.05.17  we          Fix 16-bit assert test in initialization

*************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2012-2017 Wolfgang Ehrhardt

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


---------------------------------------------------------------------------
*NOTE FROM THE DESIGNERS OF KECCAK*

The Keccak sponge function, designed by Guido Bertoni, Joan Daemen,
Michael Peeters and Gilles Van Assche. For more information, feedback or
questions, please refer to our website: http://keccak.noekeon.org/

Implementation by the designers (and Ronny Van Keer), hereby denoted
as "the implementer".

To the extent possible under law, the implementer has waived all copyright
and related or neighboring rights to the source code in this file.
http://creativecommons.org/publicdomain/zero/1.0/

----------------------------------------------------------------------------*)

uses
  BTypes, Hash;

const
  SHA3_ERR_INVALID_ALG = 1;
  SHA3_ERR_WRONG_FINAL = 2;

const
  KeccakPermutationSize        = 1600;
  KeccakMaximumRate            = 1536;
  KeccakPermutationSizeInBytes = KeccakPermutationSize div 8;
  KeccakMaximumRateInBytes     = KeccakMaximumRate div 8;

type
  TState_B = packed array[0..KeccakPermutationSizeInBytes-1] of byte;
  TState_L = packed array[0..(KeccakPermutationSizeInBytes) div 4 - 1] of longint;
  TKDQueue = packed array[0..KeccakMaximumRateInBytes-1] of byte;

type
  TSpongeState = packed record
                   state: TState_B;
                   dataQueue: TKDQueue;
                   rate: word;
                   capacity: word;
                   bitsInQueue: word;
                   fixedOutputLength: word;
                   bitsAvailableForSqueezing: word;
                   squeezing: word;
                   Error: int16;
                   Fill3: packed array[407..HASHCTXSIZE] of byte;
                 end;


{---------------------------------------------------------------------------}
{------------------ S H A 3  /  S H A K E  functions -----------------------}
{---------------------------------------------------------------------------}

type
  TSHA3State = TSpongeState;   {Hash state context}

type
  TSHA3_Algo = (__SHA3_224, __SHA3_256, __SHA3_384, __SHA3_512, __SHAKE_128, __SHAKE_256);

function SHA3_Init(var state: TSHA3State; algo: TSHA3_Algo): integer;
  {-Initialize the state of the Keccak[r, c] sponge function. The rate r and the}
  { capacity c values are determined from the SHA3 algorithm. Result 0=success. }

function SHA3_UpdateXL(var state: TSHA3State; Msg: pointer; Len: longint): integer;
  {-Update context with Msg data of Len bytes}

function SHA3_Update(var state: TSHA3State; Msg: pointer; Len: word): integer;
  {-Update context with Msg data of Len bytes}

function SHA3_FinalHash(var state: TSHA3State; digest: pointer): integer;
  {-Compute SHA3 hash digest and store into hashval. Only for hash}
  { algorithms, result WRONG_FINAL if called for SHAKE functions. }

function SHA3_FinalBit_LSB(var state: TSHA3State; bits: byte; bitlen: integer; hashval: pointer; numbits: longint): integer;
  {-Update final bits in LSB format, pad, and compute hashval}

function SHA3_FinalBit(var state: TSHA3State; bits: byte; bitlen: integer; hashval: pointer; numbits: longint): integer;
  {-Update final bits in MSB format, pad, and compute hashval}


{SHA3_LastError is set by SHA-3 functions which return an error code, where other}
{units/algorithms use procedures. Note that the error variable should be treated }
{as dummy because it is shared over all contexts/threads etc. The context field  }
{TSHA3State.error is used to handle context related errors. It will be set to    }
{0=no error during context initialization.}

var
  SHA3_LastError: integer;


implementation


const
  cKeccakNumberOfRounds = 24;

{---------------------------------------------------------------------------}
{Helper types}

{$ifndef BIT16}
type
  TBABytes = array[0..MaxLongint-1] of byte;
{$else}
type
  TBABytes = array[0..$FFF0-1] of byte;
{$endif}

type
  PBA = ^TBABytes;


{---------------------------------------------------------------------------}
{$ifndef BIT16}
  {$ifdef BIT64}
    {$define USE_64BITCODE}
  {$else}
    {$ifndef FPC}
      {$ifndef CONDITIONALEXPRESSIONS}
        {Delphi 5 or lower}
        {$undef USE_MMXCODE}
        {$undef USE_MMX_AKP}
      {$endif}
    {$endif}
  {$endif}
  {$ifdef USE_64BITCODE}
    {$i kperm_64.inc}
    {$ifdef HAS_MSG}
      {.$message '* using 64-bit code'}
    {$endif}
  {$else}
    {$ifdef USE_MMXCODE}
      {$i kperm_mx.inc}
      {$ifdef HAS_MSG}
        {$message '* using mmx code (32Bit/eg)'}
      {$endif}
    {$else}
      {$ifdef USE_MMX_AKP}
        {$i kperm_mp.inc}
        {$ifdef HAS_MSG}
          {$message '* using mmx code (32Bit/akp)'}
        {$endif}
      {$else}
        {$i kperm_32.inc}
      {$endif}
    {$endif}
  {$endif}
{$else}
  {$i kperm_16.inc}
{$endif}


{---------------------------------------------------------------------------}
procedure KeccakAbsorb(var state: TState_B; data: pointer; laneCount: integer);
begin
  xorIntoState(TState_L(state),data,laneCount);
  KeccakPermutation(TState_L(state));
end;


{---------------------------------------------------------------------------}
function InitSponge(var state: TSpongeState; rate, capacity: integer): integer;
  {-Function to initialize the state of the Keccak sponge function.}
  { The sponge function is set to the absorbing phase. Result=0 if }
  { success, 1 if rate and/or capacity are invalid.}
begin
  InitSponge := 1;
  {This is the only place where state.error is reset to 0 = SUCCESS}
  fillchar(state, sizeof(state),0);
  if (rate+capacity <> 1600) or (rate <= 0) or (rate >= 1600) or ((rate and 63) <> 0) then begin
    state.error := 1;
    exit;
  end;
  state.rate := rate;
  state.capacity := capacity;
  InitSponge := 0;
end;


{---------------------------------------------------------------------------}
procedure AbsorbQueue(var state: TSpongeState);
  {-Absorb remaining bits from queue}
begin
  {state.bitsInQueue is assumed to be equal to state.rate}
  KeccakAbsorb(state.state, @state.dataQueue, state.rate div 64);
  state.bitsInQueue := 0;
end;


{---------------------------------------------------------------------------}
function Absorb(var state: TSpongeState; data: pointer; databitlen: longint): integer;
  {-Function to give input data for the sponge function to absorb}
var
  i, j, wholeBlocks, partialBlock: longint;
  partialByte: integer;
  curData: pByte;
begin
  Absorb := 1;
  if state.error<>0 then exit; {No further action}
  if (state.bitsInQueue and 7 <> 0) or (state.squeezing<>0) then begin
    {Only the last call may contain a partial byte}
    {and additional input if squeezing}
    state.error := 1;
    exit;
  end;
  i := 0;
  while i < databitlen do begin
    if ((state.bitsInQueue=0) and (databitlen >= state.rate) and (i <= (databitlen-state.rate))) then begin
      wholeBlocks := (databitlen-i) div state.rate;
      curData := @PBA(data)^[i div 8];
      j := 0;
      while j<wholeBlocks do begin
        KeccakAbsorb(state.state, curData, state.rate div 64);
        inc(j);
        inc(Ptr2Inc(curData), state.rate div 8);
      end;
      inc(i, wholeBlocks*state.rate);
    end
    else begin
      partialBlock := databitlen - i;
      if partialBlock+state.bitsInQueue > state.rate then begin
        partialBlock := state.rate - state.bitsInQueue;
      end;
      partialByte := partialBlock and 7;
      dec(partialBlock, partialByte);
      move(PBA(data)^[i div 8], state.dataQueue[state.bitsInQueue div 8], partialBlock div 8);
      inc(state.bitsInQueue, partialBlock);
      inc(i, partialBlock);
      if state.bitsInQueue=state.rate then AbsorbQueue(state);
      if partialByte > 0 then begin
        state.dataQueue[state.bitsInQueue div 8] := PBA(data)^[i div 8] and ((1 shl partialByte)-1);
        inc(state.bitsInQueue, partialByte);
        inc(i, partialByte);
      end;
    end;
  end;
  Absorb := 0;
end;


{---------------------------------------------------------------------------}
procedure PadAndSwitchToSqueezingPhase(var state: TSpongeState);
var
  i: integer;
begin
  {Note: the bits are numbered from 0=LSB to 7=MSB}
  if (state.bitsInQueue + 1 = state.rate) then begin
    i := state.bitsInQueue div 8;
    state.dataQueue[i] := state.dataQueue[i] or (1 shl (state.bitsInQueue and 7));
    AbsorbQueue(state);
    fillchar(state.dataQueue, state.rate div 8, 0);
  end
  else begin
    i := state.bitsInQueue div 8;
    fillchar(state.dataQueue[(state.bitsInQueue+7) div 8], state.rate div 8 - (state.bitsInQueue+7) div 8,0);
    state.dataQueue[i] := state.dataQueue[i] or (1 shl (state.bitsInQueue and 7));
  end;
  i := (state.rate-1) div 8;
  state.dataQueue[i] := state.dataQueue[i] or (1 shl ((state.rate-1) and 7));
  AbsorbQueue(state);
  extractFromState(@state.dataQueue, TState_L(state.state), state.rate div 64);
  state.bitsAvailableForSqueezing := state.rate;
  state.squeezing := 1;
end;


{---------------------------------------------------------------------------}
function Squeeze(var state: TSpongeState; output: pointer; outputLength: longint): integer;
  {-Squeeze output data from the sponge function. If the sponge function was }
  { in the absorbing phase, this function switches it to the squeezing phase.}
  { Returns 0 if successful, 1 otherwise. output: pointer to the buffer where}
  { to store the output data; outputLength: number of output bits desired,   }
  { must be a multiple of 8.}
var
  i: longint;
  partialBlock: integer;
begin
  Squeeze := 1;
  if state.error<>0 then exit; {No further action}
  if state.squeezing=0 then PadAndSwitchToSqueezingPhase(state);
  if outputLength and 7 <> 0 then begin
    {Only multiple of 8 bits are allowed, truncation can be done at user level}
    state.error := 1;
    exit;
  end;
  i := 0;
  while i < outputLength do begin
    if state.bitsAvailableForSqueezing=0 then begin
      KeccakPermutation(TState_L(state.state));
      extractFromState(@state.dataQueue, TState_L(state.state), state.rate div 64);
      state.bitsAvailableForSqueezing := state.rate;
    end;
    partialBlock := state.bitsAvailableForSqueezing;
    if partialBlock > outputLength - i then partialBlock := outputLength - i;
    move(state.dataQueue[(state.rate - state.bitsAvailableForSqueezing) div 8], PBA(output)^[i div 8], partialBlock div 8);
    dec(state.bitsAvailableForSqueezing, partialBlock);
    inc(i,partialBlock);
  end;
  Squeeze := 0;
end;


{---------------------------------------------------------------------------}
function Update(var state: TSpongeState; data: pointer; databitlen: longint): integer;
  {-Update state with databitlen bits from data. May be called multiple times, }
  { only the last databitlen may be a non-multiple of 8 (the corresponding byte}
  { must be MSB aligned, i.e. in the (databitlen and 7) most significant bits. }
var
  ret: integer;
  lastByte: byte;
begin
  if state.error<>0 then begin
    Update := state.error;
    exit;
  end;
  if databitlen and 7 = 0 then ret := Absorb(state, data, databitlen)
  else begin
    ret := Absorb(state, data, databitlen - (databitlen and 7));
    if ret=0 then begin
      {Align the last partial byte to the least significant bits}
      lastByte := PBA(data)^[databitlen div 8] shr (8 - (databitlen and 7));
      ret := Absorb(state, @lastByte, databitlen and 7);
    end
  end;
  update := ret;
  {Update error only with old error=0, i.e. do no reset a non-zero value}
  if state.error=0 then state.error := ret;
end;


{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------}
function SHA3_Init(var state: TSHA3State; algo: TSHA3_Algo): integer;
  {-Initialize the state of the Keccak[r, c] sponge function. The rate r and the}
  { capacity c values are determined from the SHA3 algorithm. Result 0=success. }
const
  FOL: array[TSHA3_Algo] of word =  (224, 256, 384, 512, 0, 0);
begin
  case algo of
      __SHA3_224: SHA3_Init := InitSponge(state, 1152,  448);
      __SHA3_256: SHA3_Init := InitSponge(state, 1088,  512);
      __SHA3_384: SHA3_Init := InitSponge(state,  832,  768);
      __SHA3_512: SHA3_Init := InitSponge(state,  576, 1024);
     __SHAKE_128: SHA3_Init := InitSponge(state, 1344,  256);
     __SHAKE_256: SHA3_Init := InitSponge(state, 1088,  512);
             else begin
                    SHA3_Init   := SHA3_ERR_INVALID_ALG;
                    state.error := SHA3_ERR_INVALID_ALG;
                    exit;
                  end;
  end;
  state.fixedOutputLength := FOL[algo];
end;


{---------------------------------------------------------------------------}
function SHA3_UpdateXL(var state: TSHA3State; Msg: pointer; Len: longint): integer;
  {-Update context with Msg data of Len bytes}
begin
  SHA3_UpdateXL := Absorb(state, Msg, Len*8);
end;


{---------------------------------------------------------------------------}
function SHA3_Update(var state: TSHA3State; Msg: pointer; Len: word): integer;
  {-Update context with Msg data of Len bytes}
begin
  SHA3_Update := SHA3_UpdateXL(state, Msg, Len);
end;


{---------------------------------------------------------------------------}
function SHA3_FinalHash(var state: TSHA3State; digest: pointer): integer;
  {-Compute SHA3 hash digest and store into hashval. Only for hash}
  { algorithms, result WRONG_FINAL if called for Shake functions. }
var
  err: integer;
begin
  err := 1;
  if state.error=0 then begin
    if state.fixedOutputLength=0 then err := SHA3_ERR_WRONG_FINAL
    else err := SHA3_FinalBit_LSB(state, 0, 0, digest, state.fixedOutputLength);
  end;
  {Update error only with old error=0, i.e. do no reset a non-zero value}
  if state.error=0 then state.error := err;
  SHA3_FinalHash := err;
end;


{---------------------------------------------------------------------------}
function SHA3_FinalBit_LSB(var state: TSHA3State; bits: byte; bitlen: integer; hashval: pointer; numbits: longint): integer;
  {-Update final bits in LSB format, pad, and compute hashval}
var
  err,ll: integer;
  lw: word;
begin
  {normalize bitlen and bits (zero high bits)}
  bitlen := bitlen and 7;
  if bitlen=0 then lw := 0
  else lw := bits and pred(word(1) shl bitlen);

  {'append' (in LSB language) the domain separation bits}
  if state.fixedOutputLength=0 then begin
    {SHAKE: append four bits 1111}
    lw := lw or (word($F) shl bitlen);
    ll := bitlen+4;
  end
  else begin
    {SHA3: append two bits 01}
    lw := lw or (word($2) shl bitlen);
    ll := bitlen+2;
  end;

  {update state with final bits}
  if ll<9 then begin
    {0..8 bits, one call to update}
    lw := lw shl (8-ll);
    err := update(state, @lw, ll);
    {squeeze the digits from the sponge}
    if err=0 then err := Squeeze(state, hashval, numbits);
  end
  else begin
    {More than 8 bits, first a regular update with low byte}
    err := update(state, @lw, 8);
    if err=0 then begin
      {Finally update remaining last bits}
      dec(ll,8);
      lw := lw shr ll;
      err := update(state, @lw, ll);
      if err=0 then err := Squeeze(state, hashval, numbits);
    end;
  end;
  SHA3_FinalBit_LSB := err;
  if state.error=0 then state.error := err;
end;


{---------------------------------------------------------------------------}
function SHA3_FinalBit(var state: TSHA3State; bits: byte; bitlen: integer; hashval: pointer; numbits: longint): integer;
  {-Update final bits in MSB format, pad, and compute hashval}
var
  i,m: integer;
  r,b: byte;
begin
  r := 0;
  m := bitlen and $7;
  if m>0 then begin
    {right align the m bits}
    b := bits shr (8-m);
    {store reflected bits in r}
    for i:=m downto 1 do begin
      r := r + r + (b and 1);
      b := b shr 1;
    end;
  end;
  SHA3_FinalBit := SHA3_FinalBit_LSB(state,r,bitlen,hashval,numbits);
end;


begin
  {$ifdef HAS_ASSERT}
    assert(sizeof(TSHA3State)=HASHCTXSIZE , '** Invalid sizeof(TSHA3State)');
  {$else}
    if sizeof(TSHA3State)<>HASHCTXSIZE then RunError(227);
  {$endif}
  SHA3_LastError := 0;
end.
