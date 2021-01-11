{KeccakF[1600] state permutation for 32 bit compilers, RotL code inline}
{Pascal translation from C code in Keccak-simple32BI.c by Ronny Van Keer}

{---------------------------------------------------------------------------}
const
  KeccakF1600RoundConstants_int2: array[0..2*24-1] of longint =
    (longint($00000001), longint($00000000),
     longint($00000000), longint($00000089),
     longint($00000000), longint($8000008b),
     longint($00000000), longint($80008080),
     longint($00000001), longint($0000008b),
     longint($00000001), longint($00008000),
     longint($00000001), longint($80008088),
     longint($00000001), longint($80000082),
     longint($00000000), longint($0000000b),
     longint($00000000), longint($0000000a),
     longint($00000001), longint($00008082),
     longint($00000000), longint($00008003),
     longint($00000001), longint($0000808b),
     longint($00000001), longint($8000000b),
     longint($00000001), longint($8000008a),
     longint($00000001), longint($80000081),
     longint($00000000), longint($80000081),
     longint($00000000), longint($80000008),
     longint($00000000), longint($00000083),
     longint($00000000), longint($80008003),
     longint($00000001), longint($80008088),
     longint($00000000), longint($80000088),
     longint($00000001), longint($00008000),
     longint($00000000), longint($80008082));


{---------------------------------------------------------------------------}
procedure KeccakPermutation(var state: TState_L);
var
  Aba0, Abe0, Abi0, Abo0, Abu0: longint;
  Aba1, Abe1, Abi1, Abo1, Abu1: longint;
  Aga0, Age0, Agi0, Ago0, Agu0: longint;
  Aga1, Age1, Agi1, Ago1, Agu1: longint;
  Aka0, Ake0, Aki0, Ako0, Aku0: longint;
  Aka1, Ake1, Aki1, Ako1, Aku1: longint;
  Ama0, Ame0, Ami0, Amo0, Amu0: longint;
  Ama1, Ame1, Ami1, Amo1, Amu1: longint;
  Asa0, Ase0, Asi0, Aso0, Asu0: longint;
  Asa1, Ase1, Asi1, Aso1, Asu1: longint;
  BCa0, BCe0, BCi0, BCo0, BCu0: longint;
  BCa1, BCe1, BCi1, BCo1, BCu1: longint;
  Da0,  De0,  Di0,  Do0,  Du0:  longint;
  Da1,  De1,  Di1,  Do1,  Du1:  longint;
  Eba0, Ebe0, Ebi0, Ebo0, Ebu0: longint;
  Eba1, Ebe1, Ebi1, Ebo1, Ebu1: longint;
  Ega0, Ege0, Egi0, Ego0, Egu0: longint;
  Ega1, Ege1, Egi1, Ego1, Egu1: longint;
  Eka0, Eke0, Eki0, Eko0, Eku0: longint;
  Eka1, Eke1, Eki1, Eko1, Eku1: longint;
  Ema0, Eme0, Emi0, Emo0, Emu0: longint;
  Ema1, Eme1, Emi1, Emo1, Emu1: longint;
  Esa0, Ese0, Esi0, Eso0, Esu0: longint;
  Esa1, Ese1, Esi1, Eso1, Esu1: longint;

var
  round: integer;

begin
  {copyFromState(A, state)}
  Aba0 := state[ 0];
  Aba1 := state[ 1];
  Abe0 := state[ 2];
  Abe1 := state[ 3];
  Abi0 := state[ 4];
  Abi1 := state[ 5];
  Abo0 := state[ 6];
  Abo1 := state[ 7];
  Abu0 := state[ 8];
  Abu1 := state[ 9];
  Aga0 := state[10];
  Aga1 := state[11];
  Age0 := state[12];
  Age1 := state[13];
  Agi0 := state[14];
  Agi1 := state[15];
  Ago0 := state[16];
  Ago1 := state[17];
  Agu0 := state[18];
  Agu1 := state[19];
  Aka0 := state[20];
  Aka1 := state[21];
  Ake0 := state[22];
  Ake1 := state[23];
  Aki0 := state[24];
  Aki1 := state[25];
  Ako0 := state[26];
  Ako1 := state[27];
  Aku0 := state[28];
  Aku1 := state[29];
  Ama0 := state[30];
  Ama1 := state[31];
  Ame0 := state[32];
  Ame1 := state[33];
  Ami0 := state[34];
  Ami1 := state[35];
  Amo0 := state[36];
  Amo1 := state[37];
  Amu0 := state[38];
  Amu1 := state[39];
  Asa0 := state[40];
  Asa1 := state[41];
  Ase0 := state[42];
  Ase1 := state[43];
  Asi0 := state[44];
  Asi1 := state[45];
  Aso0 := state[46];
  Aso1 := state[47];
  Asu0 := state[48];
  Asu1 := state[49];

  round := 0;
  while round < cKeccakNumberOfRounds do begin
    {prepareTheta}
    BCa0 := Aba0 xor Aga0 xor Aka0 xor Ama0 xor Asa0;
    BCa1 := Aba1 xor Aga1 xor Aka1 xor Ama1 xor Asa1;
    BCe0 := Abe0 xor Age0 xor Ake0 xor Ame0 xor Ase0;
    BCe1 := Abe1 xor Age1 xor Ake1 xor Ame1 xor Ase1;
    BCi0 := Abi0 xor Agi0 xor Aki0 xor Ami0 xor Asi0;
    BCi1 := Abi1 xor Agi1 xor Aki1 xor Ami1 xor Asi1;
    BCo0 := Abo0 xor Ago0 xor Ako0 xor Amo0 xor Aso0;
    BCo1 := Abo1 xor Ago1 xor Ako1 xor Amo1 xor Aso1;
    BCu0 := Abu0 xor Agu0 xor Aku0 xor Amu0 xor Asu0;
    BCu1 := Abu1 xor Agu1 xor Aku1 xor Amu1 xor Asu1;

    {thetaRhoPiChiIota(round, A, E)}
    Da0 := BCu0 xor (BCe1 shl 1) xor (BCe1 shr (32-1));
    Da1 := BCu1 xor BCe0;
    De0 := BCa0 xor (BCi1 shl 1) xor (BCi1 shr (32-1));
    De1 := BCa1 xor BCi0;
    Di0 := BCe0 xor (BCo1 shl 1) xor (BCo1 shr (32-1));
    Di1 := BCe1 xor BCo0;
    Do0 := BCi0 xor (BCu1 shl 1) xor (BCu1 shr (32-1));
    Do1 := BCi1 xor BCu0;
    Du0 := BCo0 xor (BCa1 shl 1) xor (BCa1 shr (32-1));
    Du1 := BCo1 xor BCa0;

    Aba0 := Aba0 xor Da0;  BCa0 := Aba0;
    Age0 := Age0 xor De0;  BCe0 := (Age0 shl 22) xor (Age0 shr (32-22));
    Aki1 := Aki1 xor Di1;  BCi0 := (Aki1 shl 22) xor (Aki1 shr (32-22));
    Amo1 := Amo1 xor Do1;  BCo0 := (Amo1 shl 11) xor (Amo1 shr (32-11));
    Asu0 := Asu0 xor Du0;  BCu0 := (Asu0 shl  7) xor (Asu0 shr (32-7));

    Eba0 := BCa0 xor ((not BCe0) and BCi0) xor KeccakF1600RoundConstants_int2[round*2+0];
    Ebe0 := BCe0 xor ((not BCi0) and BCo0);
    Ebi0 := BCi0 xor ((not BCo0) and BCu0);
    Ebo0 := BCo0 xor ((not BCu0) and BCa0);
    Ebu0 := BCu0 xor ((not BCa0) and BCe0);

    Aba1 := Aba1 xor Da1;  BCa1 := Aba1;
    Age1 := Age1 xor De1;  BCe1 := (Age1 shl 22) xor (Age1 shr (32-22));
    Aki0 := Aki0 xor Di0;  BCi1 := (Aki0 shl 21) xor (Aki0 shr (32-21));
    Amo0 := Amo0 xor Do0;  BCo1 := (Amo0 shl 10) xor (Amo0 shr (32-10));
    Asu1 := Asu1 xor Du1;  BCu1 := (Asu1 shl  7) xor (Asu1 shr (32-7));

    Eba1 := BCa1 xor ((not BCe1) and BCi1) xor KeccakF1600RoundConstants_int2[round*2+1];
    Ebe1 := BCe1 xor ((not BCi1) and BCo1);
    Ebi1 := BCi1 xor ((not BCo1) and BCu1);
    Ebo1 := BCo1 xor ((not BCu1) and BCa1);
    Ebu1 := BCu1 xor ((not BCa1) and BCe1);

    Abo0 := Abo0 xor Do0;  BCa0 := (Abo0 shl 14) xor (Abo0 shr (32-14));
    Agu0 := Agu0 xor Du0;  BCe0 := (Agu0 shl 10) xor (Agu0 shr (32-10));
    Aka1 := Aka1 xor Da1;  BCi0 := (Aka1 shl  2) xor (Aka1 shr (32-2));
    Ame1 := Ame1 xor De1;  BCo0 := (Ame1 shl 23) xor (Ame1 shr (32-23));
    Asi1 := Asi1 xor Di1;  BCu0 := (Asi1 shl 31) xor (Asi1 shr (32-31));

    Ega0 := BCa0 xor ((not BCe0) and BCi0);
    Ege0 := BCe0 xor ((not BCi0) and BCo0);
    Egi0 := BCi0 xor ((not BCo0) and BCu0);
    Ego0 := BCo0 xor ((not BCu0) and BCa0);
    Egu0 := BCu0 xor ((not BCa0) and BCe0);

    Abo1 := Abo1 xor Do1;  BCa1 := (Abo1 shl 14) xor (Abo1 shr (32-14));
    Agu1 := Agu1 xor Du1;  BCe1 := (Agu1 shl 10) xor (Agu1 shr (32-10));
    Aka0 := Aka0 xor Da0;  BCi1 := (Aka0 shl  1) xor (Aka0 shr (32-1));
    Ame0 := Ame0 xor De0;  BCo1 := (Ame0 shl 22) xor (Ame0 shr (32-22));
    Asi0 := Asi0 xor Di0;  BCu1 := (Asi0 shl 30) xor (Asi0 shr (32-30));

    Ega1 := BCa1 xor ((not BCe1) and BCi1);
    Ege1 := BCe1 xor ((not BCi1) and BCo1);
    Egi1 := BCi1 xor ((not BCo1) and BCu1);
    Ego1 := BCo1 xor ((not BCu1) and BCa1);
    Egu1 := BCu1 xor ((not BCa1) and BCe1);

    Abe1 := Abe1 xor De1;  BCa0 := (Abe1 shl  1) xor (Abe1 shr (32-1));
    Agi0 := Agi0 xor Di0;  BCe0 := (Agi0 shl  3) xor (Agi0 shr (32-3));
    Ako1 := Ako1 xor Do1;  BCi0 := (Ako1 shl 13) xor (Ako1 shr (32-13));
    Amu0 := Amu0 xor Du0;  BCo0 := (Amu0 shl  4) xor (Amu0 shr (32-4));
    Asa0 := Asa0 xor Da0;  BCu0 := (Asa0 shl  9) xor (Asa0 shr (32-9));

    Eka0 := BCa0 xor ((not BCe0) and BCi0 );
    Eke0 := BCe0 xor ((not BCi0) and BCo0 );
    Eki0 := BCi0 xor ((not BCo0) and BCu0 );
    Eko0 := BCo0 xor ((not BCu0) and BCa0 );
    Eku0 := BCu0 xor ((not BCa0) and BCe0 );

    Abe0 := Abe0 xor De0;  BCa1 := Abe0;
    Agi1 := Agi1 xor Di1;  BCe1 := (Agi1 shl  3) xor (Agi1 shr (32-3));
    Ako0 := Ako0 xor Do0;  BCi1 := (Ako0 shl 12) xor (Ako0 shr (32-12));
    Amu1 := Amu1 xor Du1;  BCo1 := (Amu1 shl  4) xor (Amu1 shr (32-4));
    Asa1 := Asa1 xor Da1;  BCu1 := (Asa1 shl  9) xor (Asa1 shr (32-9));

    Eka1 := BCa1 xor ((not BCe1) and BCi1);
    Eke1 := BCe1 xor ((not BCi1) and BCo1);
    Eki1 := BCi1 xor ((not BCo1) and BCu1);
    Eko1 := BCo1 xor ((not BCu1) and BCa1);
    Eku1 := BCu1 xor ((not BCa1) and BCe1);

    Abu1 := Abu1 xor Du1;  BCa0 := (Abu1 shl 14) xor (Abu1 shr (32-14));
    Aga0 := Aga0 xor Da0;  BCe0 := (Aga0 shl 18) xor (Aga0 shr (32-18));
    Ake0 := Ake0 xor De0;  BCi0 := (Ake0 shl  5) xor (Ake0 shr (32-5));
    Ami1 := Ami1 xor Di1;  BCo0 := (Ami1 shl  8) xor (Ami1 shr (32-8));
    Aso0 := Aso0 xor Do0;  BCu0 := (Aso0 shl 28) xor (Aso0 shr (32-28));

    Ema0 := BCa0 xor ((not BCe0) and BCi0);
    Eme0 := BCe0 xor ((not BCi0) and BCo0);
    Emi0 := BCi0 xor ((not BCo0) and BCu0);
    Emo0 := BCo0 xor ((not BCu0) and BCa0);
    Emu0 := BCu0 xor ((not BCa0) and BCe0);

    Abu0 := Abu0 xor Du0;  BCa1 := (Abu0 shl 13) xor (Abu0 shr (32-13));
    Aga1 := Aga1 xor Da1;  BCe1 := (Aga1 shl 18) xor (Aga1 shr (32-18));
    Ake1 := Ake1 xor De1;  BCi1 := (Ake1 shl  5) xor (Ake1 shr (32-5));
    Ami0 := Ami0 xor Di0;  BCo1 := (Ami0 shl  7) xor (Ami0 shr (32-7));
    Aso1 := Aso1 xor Do1;  BCu1 := (Aso1 shl 28) xor (Aso1 shr (32-28));

    Ema1 := BCa1 xor ((not BCe1) and BCi1);
    Eme1 := BCe1 xor ((not BCi1) and BCo1);
    Emi1 := BCi1 xor ((not BCo1) and BCu1);
    Emo1 := BCo1 xor ((not BCu1) and BCa1);
    Emu1 := BCu1 xor ((not BCa1) and BCe1);

    Abi0 := Abi0 xor Di0;  BCa0 := (Abi0 shl 31) xor (Abi0 shr (32-31));
    Ago1 := Ago1 xor Do1;  BCe0 := (Ago1 shl 28) xor (Ago1 shr (32-28));
    Aku1 := Aku1 xor Du1;  BCi0 := (Aku1 shl 20) xor (Aku1 shr (32-20));
    Ama1 := Ama1 xor Da1;  BCo0 := (Ama1 shl 21) xor (Ama1 shr (32-21));
    Ase0 := Ase0 xor De0;  BCu0 := (Ase0 shl  1) xor (Ase0 shr (32-1));

    Esa0 := BCa0 xor ((not BCe0) and BCi0);
    Ese0 := BCe0 xor ((not BCi0) and BCo0);
    Esi0 := BCi0 xor ((not BCo0) and BCu0);
    Eso0 := BCo0 xor ((not BCu0) and BCa0);
    Esu0 := BCu0 xor ((not BCa0) and BCe0);

    Abi1 := Abi1 xor Di1;  BCa1 := (Abi1 shl 31) xor (Abi1 shr (32-31));
    Ago0 := Ago0 xor Do0;  BCe1 := (Ago0 shl 27) xor (Ago0 shr (32-27));
    Aku0 := Aku0 xor Du0;  BCi1 := (Aku0 shl 19) xor (Aku0 shr (32-19));
    Ama0 := Ama0 xor Da0;  BCo1 := (Ama0 shl 20) xor (Ama0 shr (32-20));
    Ase1 := Ase1 xor De1;  BCu1 := (Ase1 shl  1) xor (Ase1 shr (32-1));

    Esa1 := BCa1 xor ((not BCe1) and BCi1);
    Ese1 := BCe1 xor ((not BCi1) and BCo1);
    Esi1 := BCi1 xor ((not BCo1) and BCu1);
    Eso1 := BCo1 xor ((not BCu1) and BCa1);
    Esu1 := BCu1 xor ((not BCa1) and BCe1);

    {prepareTheta}
    BCa0 := Eba0 xor Ega0 xor Eka0 xor Ema0 xor Esa0;
    BCa1 := Eba1 xor Ega1 xor Eka1 xor Ema1 xor Esa1;
    BCe0 := Ebe0 xor Ege0 xor Eke0 xor Eme0 xor Ese0;
    BCe1 := Ebe1 xor Ege1 xor Eke1 xor Eme1 xor Ese1;
    BCi0 := Ebi0 xor Egi0 xor Eki0 xor Emi0 xor Esi0;
    BCi1 := Ebi1 xor Egi1 xor Eki1 xor Emi1 xor Esi1;
    BCo0 := Ebo0 xor Ego0 xor Eko0 xor Emo0 xor Eso0;
    BCo1 := Ebo1 xor Ego1 xor Eko1 xor Emo1 xor Eso1;
    BCu0 := Ebu0 xor Egu0 xor Eku0 xor Emu0 xor Esu0;
    BCu1 := Ebu1 xor Egu1 xor Eku1 xor Emu1 xor Esu1;

    {thetaRhoPiChiIota(round+1, E, A)}
    Da0 := BCu0 xor (BCe1 shl 1) xor (BCe1 shr (32-1));
    Da1 := BCu1 xor BCe0;
    De0 := BCa0 xor (BCi1 shl 1) xor (BCi1 shr (32-1));
    De1 := BCa1 xor BCi0;
    Di0 := BCe0 xor (BCo1 shl 1) xor (BCo1 shr (32-1));
    Di1 := BCe1 xor BCo0;
    Do0 := BCi0 xor (BCu1 shl 1) xor (BCu1 shr (32-1));
    Do1 := BCi1 xor BCu0;
    Du0 := BCo0 xor (BCa1 shl 1) xor (BCa1 shr (32-1));
    Du1 := BCo1 xor BCa0;

    Eba0 := Eba0 xor Da0;  BCa0 := Eba0;
    Ege0 := Ege0 xor De0;  BCe0 := (Ege0 shl 22) xor (Ege0 shr (32-22));
    Eki1 := Eki1 xor Di1;  BCi0 := (Eki1 shl 22) xor (Eki1 shr (32-22));
    Emo1 := Emo1 xor Do1;  BCo0 := (Emo1 shl 11) xor (Emo1 shr (32-11));
    Esu0 := Esu0 xor Du0;  BCu0 := (Esu0 shl  7) xor (Esu0 shr (32-7));

    Aba0 := BCa0 xor ((not BCe0) and BCi0) xor KeccakF1600RoundConstants_int2[round*2+2];
    Abe0 := BCe0 xor ((not BCi0) and BCo0);
    Abi0 := BCi0 xor ((not BCo0) and BCu0);
    Abo0 := BCo0 xor ((not BCu0) and BCa0);
    Abu0 := BCu0 xor ((not BCa0) and BCe0);

    Eba1 := Eba1 xor Da1;  BCa1 := Eba1;
    Ege1 := Ege1 xor De1;  BCe1 := (Ege1 shl 22) xor (Ege1 shr (32-22));
    Eki0 := Eki0 xor Di0;  BCi1 := (Eki0 shl 21) xor (Eki0 shr (32-21));
    Emo0 := Emo0 xor Do0;  BCo1 := (Emo0 shl 10) xor (Emo0 shr (32-10));
    Esu1 := Esu1 xor Du1;  BCu1 := (Esu1 shl  7) xor (Esu1 shr (32-7));

    Aba1 := BCa1 xor ((not BCe1) and BCi1) xor KeccakF1600RoundConstants_int2[round*2+3];
    Abe1 := BCe1 xor ((not BCi1) and BCo1);
    Abi1 := BCi1 xor ((not BCo1) and BCu1);
    Abo1 := BCo1 xor ((not BCu1) and BCa1);
    Abu1 := BCu1 xor ((not BCa1) and BCe1);

    Ebo0 := Ebo0 xor Do0;  BCa0 := (Ebo0 shl 14) xor (Ebo0 shr (32-14));
    Egu0 := Egu0 xor Du0;  BCe0 := (Egu0 shl 10) xor (Egu0 shr (32-10));
    Eka1 := Eka1 xor Da1;  BCi0 := (Eka1 shl  2) xor (Eka1 shr (32-2));
    Eme1 := Eme1 xor De1;  BCo0 := (Eme1 shl 23) xor (Eme1 shr (32-23));
    Esi1 := Esi1 xor Di1;  BCu0 := (Esi1 shl 31) xor (Esi1 shr (32-31));

    Aga0 := BCa0 xor ((not BCe0) and BCi0);
    Age0 := BCe0 xor ((not BCi0) and BCo0);
    Agi0 := BCi0 xor ((not BCo0) and BCu0);
    Ago0 := BCo0 xor ((not BCu0) and BCa0);
    Agu0 := BCu0 xor ((not BCa0) and BCe0);

    Ebo1 := Ebo1 xor Do1;  BCa1 := (Ebo1 shl 14) xor (Ebo1 shr (32-14));
    Egu1 := Egu1 xor Du1;  BCe1 := (Egu1 shl 10) xor (Egu1 shr (32-10));
    Eka0 := Eka0 xor Da0;  BCi1 := (Eka0 shl  1) xor (Eka0 shr (32-1));
    Eme0 := Eme0 xor De0;  BCo1 := (Eme0 shl 22) xor (Eme0 shr (32-22));
    Esi0 := Esi0 xor Di0;  BCu1 := (Esi0 shl 30) xor (Esi0 shr (32-30));

    Aga1 := BCa1 xor ((not BCe1) and BCi1);
    Age1 := BCe1 xor ((not BCi1) and BCo1);
    Agi1 := BCi1 xor ((not BCo1) and BCu1);
    Ago1 := BCo1 xor ((not BCu1) and BCa1);
    Agu1 := BCu1 xor ((not BCa1) and BCe1);

    Ebe1 := Ebe1 xor De1;  BCa0 := (Ebe1 shl  1) xor (Ebe1 shr (32-1));
    Egi0 := Egi0 xor Di0;  BCe0 := (Egi0 shl  3) xor (Egi0 shr (32-3));
    Eko1 := Eko1 xor Do1;  BCi0 := (Eko1 shl 13) xor (Eko1 shr (32-13));
    Emu0 := Emu0 xor Du0;  BCo0 := (Emu0 shl  4) xor (Emu0 shr (32-4));
    Esa0 := Esa0 xor Da0;  BCu0 := (Esa0 shl  9) xor (Esa0 shr (32-9));

    Aka0 := BCa0 xor ((not BCe0) and BCi0);
    Ake0 := BCe0 xor ((not BCi0) and BCo0);
    Aki0 := BCi0 xor ((not BCo0) and BCu0);
    Ako0 := BCo0 xor ((not BCu0) and BCa0);
    Aku0 := BCu0 xor ((not BCa0) and BCe0);

    Ebe0 := Ebe0 xor De0;  BCa1 := Ebe0;
    Egi1 := Egi1 xor Di1;  BCe1 := (Egi1 shl  3) xor (Egi1 shr (32-3));
    Eko0 := Eko0 xor Do0;  BCi1 := (Eko0 shl 12) xor (Eko0 shr (32-12));
    Emu1 := Emu1 xor Du1;  BCo1 := (Emu1 shl  4) xor (Emu1 shr (32-4));
    Esa1 := Esa1 xor Da1;  BCu1 := (Esa1 shl  9) xor (Esa1 shr (32-9));

    Aka1 := BCa1 xor ((not BCe1) and BCi1);
    Ake1 := BCe1 xor ((not BCi1) and BCo1);
    Aki1 := BCi1 xor ((not BCo1) and BCu1);
    Ako1 := BCo1 xor ((not BCu1) and BCa1);
    Aku1 := BCu1 xor ((not BCa1) and BCe1);

    Ebu1 := Ebu1 xor Du1;  BCa0 := (Ebu1 shl 14) xor (Ebu1 shr (32-14));
    Ega0 := Ega0 xor Da0;  BCe0 := (Ega0 shl 18) xor (Ega0 shr (32-18));
    Eke0 := Eke0 xor De0;  BCi0 := (Eke0 shl  5) xor (Eke0 shr (32-5));
    Emi1 := Emi1 xor Di1;  BCo0 := (Emi1 shl  8) xor (Emi1 shr (32-8));
    Eso0 := Eso0 xor Do0;  BCu0 := (Eso0 shl 28) xor (Eso0 shr (32-28));

    Ama0 := BCa0 xor ((not BCe0) and BCi0);
    Ame0 := BCe0 xor ((not BCi0) and BCo0);
    Ami0 := BCi0 xor ((not BCo0) and BCu0);
    Amo0 := BCo0 xor ((not BCu0) and BCa0);
    Amu0 := BCu0 xor ((not BCa0) and BCe0);

    Ebu0 := Ebu0 xor Du0;  BCa1 := (Ebu0 shl 13) xor (Ebu0 shr (32-13));
    Ega1 := Ega1 xor Da1;  BCe1 := (Ega1 shl 18) xor (Ega1 shr (32-18));
    Eke1 := Eke1 xor De1;  BCi1 := (Eke1 shl  5) xor (Eke1 shr (32-5));
    Emi0 := Emi0 xor Di0;  BCo1 := (Emi0 shl  7) xor (Emi0 shr (32-7));
    Eso1 := Eso1 xor Do1;  BCu1 := (Eso1 shl 28) xor (Eso1 shr (32-28));

    Ama1 := BCa1 xor ((not BCe1) and BCi1);
    Ame1 := BCe1 xor ((not BCi1) and BCo1);
    Ami1 := BCi1 xor ((not BCo1) and BCu1);
    Amo1 := BCo1 xor ((not BCu1) and BCa1);
    Amu1 := BCu1 xor ((not BCa1) and BCe1);

    Ebi0 := Ebi0 xor Di0;  BCa0 := (Ebi0 shl 31) xor (Ebi0 shr (32-31));
    Ego1 := Ego1 xor Do1;  BCe0 := (Ego1 shl 28) xor (Ego1 shr (32-28));
    Eku1 := Eku1 xor Du1;  BCi0 := (Eku1 shl 20) xor (Eku1 shr (32-20));
    Ema1 := Ema1 xor Da1;  BCo0 := (Ema1 shl 21) xor (Ema1 shr (32-21));
    Ese0 := Ese0 xor De0;  BCu0 := (Ese0 shl  1) xor (Ese0 shr (32-1));

    Asa0 := BCa0 xor ((not BCe0) and BCi0);
    Ase0 := BCe0 xor ((not BCi0) and BCo0);
    Asi0 := BCi0 xor ((not BCo0) and BCu0);
    Aso0 := BCo0 xor ((not BCu0) and BCa0);
    Asu0 := BCu0 xor ((not BCa0) and BCe0);

    Ebi1 := Ebi1 xor Di1;  BCa1 := (Ebi1 shl 31) xor (Ebi1 shr (32-31));
    Ego0 := Ego0 xor Do0;  BCe1 := (Ego0 shl 27) xor (Ego0 shr (32-27));
    Eku0 := Eku0 xor Du0;  BCi1 := (Eku0 shl 19) xor (Eku0 shr (32-19));
    Ema0 := Ema0 xor Da0;  BCo1 := (Ema0 shl 20) xor (Ema0 shr (32-20));
    Ese1 := Ese1 xor De1;  BCu1 := (Ese1 shl  1) xor (Ese1 shr (32-1));

    Asa1 := BCa1 xor ((not BCe1) and BCi1);
    Ase1 := BCe1 xor ((not BCi1) and BCo1);
    Asi1 := BCi1 xor ((not BCo1) and BCu1);
    Aso1 := BCo1 xor ((not BCu1) and BCa1);
    Asu1 := BCu1 xor ((not BCa1) and BCe1);
    inc(round,2);
  end;

  {copyToState(state, A)}
  state[ 0] := Aba0;
  state[ 1] := Aba1;
  state[ 2] := Abe0;
  state[ 3] := Abe1;
  state[ 4] := Abi0;
  state[ 5] := Abi1;
  state[ 6] := Abo0;
  state[ 7] := Abo1;
  state[ 8] := Abu0;
  state[ 9] := Abu1;
  state[10] := Aga0;
  state[11] := Aga1;
  state[12] := Age0;
  state[13] := Age1;
  state[14] := Agi0;
  state[15] := Agi1;
  state[16] := Ago0;
  state[17] := Ago1;
  state[18] := Agu0;
  state[19] := Agu1;
  state[20] := Aka0;
  state[21] := Aka1;
  state[22] := Ake0;
  state[23] := Ake1;
  state[24] := Aki0;
  state[25] := Aki1;
  state[26] := Ako0;
  state[27] := Ako1;
  state[28] := Aku0;
  state[29] := Aku1;
  state[30] := Ama0;
  state[31] := Ama1;
  state[32] := Ame0;
  state[33] := Ame1;
  state[34] := Ami0;
  state[35] := Ami1;
  state[36] := Amo0;
  state[37] := Amo1;
  state[38] := Amu0;
  state[39] := Amu1;
  state[40] := Asa0;
  state[41] := Asa1;
  state[42] := Ase0;
  state[43] := Ase1;
  state[44] := Asi0;
  state[45] := Asi1;
  state[46] := Aso0;
  state[47] := Aso1;
  state[48] := Asu0;
  state[49] := Asu1;
end;


{---------------------------------------------------------------------------}
procedure extractFromState(outp: pointer; const state: TState_L; laneCount: integer);
var
  pI, pS: PLongint;
  i: integer;
  t,x0,x1: longint;
const
  xFFFF0000 = longint($FFFF0000);   {Keep D9+ happy}
begin
   {Credit: Henry S. Warren, Hacker's Delight, Addison-Wesley, 2002}
   pI := outp;
   pS := @state[0];
   for i:=laneCount-1 downto 0 do begin
     x0 := pS^; inc(Ptr2Inc(pS),sizeof(pS^));
     x1 := pS^; inc(Ptr2Inc(pS),sizeof(pS^));
     t  := (x0 and $0000FFFF) or (x1 shl 16);
     x1 := (x0 shr 16) or (x1 and xFFFF0000);
     x0 := t;
     t  := (x0 xor (x0 shr  8)) and $0000FF00;  x0 := x0 xor t xor (t shl  8);
     t  := (x0 xor (x0 shr  4)) and $00F000F0;  x0 := x0 xor t xor (t shl  4);
     t  := (x0 xor (x0 shr  2)) and $0C0C0C0C;  x0 := x0 xor t xor (t shl  2);
     t  := (x0 xor (x0 shr  1)) and $22222222;  x0 := x0 xor t xor (t shl  1);
     t  := (x1 xor (x1 shr  8)) and $0000FF00;  x1 := x1 xor t xor (t shl  8);
     t  := (x1 xor (x1 shr  4)) and $00F000F0;  x1 := x1 xor t xor (t shl  4);
     t  := (x1 xor (x1 shr  2)) and $0C0C0C0C;  x1 := x1 xor t xor (t shl  2);
     t  := (x1 xor (x1 shr  1)) and $22222222;  x1 := x1 xor t xor (t shl  1);
     pI^:= x0; inc(Ptr2Inc(pI),sizeof(pI^));
     pI^:= x1; inc(Ptr2Inc(pI),sizeof(pI^));
   end;
end;


{---------------------------------------------------------------------------}
procedure xorIntoState(var state: TState_L; inp: pointer; laneCount: integer);
  {-Include input message data bits into the sponge state}
var
  t,x0,x1: longint;
  pI,pS: PLongint;
  i: integer;
const
  xFFFF0000 = longint($FFFF0000);   {Keep D9+ happy}
begin
  {Credit: Henry S. Warren, Hacker's Delight, Addison-Wesley, 2002}
  pI := inp;
  pS := @state[0];
  for i:=laneCount-1 downto 0 do begin
    x0 := pI^;
    inc(Ptr2Inc(pI),sizeof(pI^));
    t := (x0 xor (x0 shr 1)) and $22222222;  x0 := x0 xor t xor (t shl 1);
    t := (x0 xor (x0 shr 2)) and $0C0C0C0C;  x0 := x0 xor t xor (t shl 2);
    t := (x0 xor (x0 shr 4)) and $00F000F0;  x0 := x0 xor t xor (t shl 4);
    t := (x0 xor (x0 shr 8)) and $0000FF00;  x0 := x0 xor t xor (t shl 8);
    x1 := pI^;
    inc(Ptr2Inc(pI),sizeof(pI^));
    t := (x1 xor (x1 shr 1)) and $22222222;  x1 := x1 xor t xor (t shl 1);
    t := (x1 xor (x1 shr 2)) and $0C0C0C0C;  x1 := x1 xor t xor (t shl 2);
    t := (x1 xor (x1 shr 4)) and $00F000F0;  x1 := x1 xor t xor (t shl 4);
    t := (x1 xor (x1 shr 8)) and $0000FF00;  x1 := x1 xor t xor (t shl 8);
    pS^ := pS^ xor ((x0 and $0000FFFF) or (x1 shl 16)); inc(Ptr2Inc(pS),sizeof(pS^));
    pS^ := pS^ xor ((x0 shr 16) or (x1 and xFFFF0000)); inc(Ptr2Inc(pS),sizeof(pS^));
  end;
end;

