{-Test Cyc/B and MB/s for CRC/HASH, we 2012-2017}

program t_speeda;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


{$ifndef FPC}
  {$B-,N+}
{$endif}

{$ifdef BASM16}
  {$i ALIGN.INC}
{$endif}


uses
  {$ifdef WIN32or64}
    {$ifdef UNIT_SCOPE}
      winapi.windows,
    {$else}
      windows,
    {$endif}
  {$endif}
  hrtimer,
  {$ifdef WINCRT}
    wincrt,
  {$else}
    crt,
  {$endif}
  hash,
  whirl512,
  adler32,
  crc64,
  sha1,
  sha224,
  sha256,
  sha384,
  sha512,
  sha5_224,
  sha5_256,
  sha3_224,
  sha3_256,
  sha3_384,
  sha3_512,
  blaks224,
  blaks256,
  blakb384,
  blakb512,
  rmd160,
  ED2K,
  md4,
  md5,
  fcrc32,
  crc32,
  bjl3,
  crc24,
  crc16;

const
  NUMBYTES  = 50000;
  NUMROUNDS = 20;
  BYTECOUNT = NUMBYTES*NUMROUNDS;
  MEGCOUNT  = BYTECOUNT/1E6;
  DThresh   = 0.3;


{$ifndef BIT16}
  MinRnd = 10;
{$else}
  {$ifdef BASM16}
    MinRnd = 10;
  {$else}
    MinRnd = 5;
  {$endif}
{$endif}

type
  TCompArray = array[0..MinRnd] of comp;
  TBuf  = array[1..NUMBYTES] of byte;
  TTest = record
            name : string[11];
            adiff: TCompArray;
            mdiff: double;
            CpB  : double;
            MBs  : double;
            D100 : double;
            done : boolean;
          end;

var
  T_CRC16   : TTest;
  T_CRC24   : TTest;
  T_CRC32   : TTest;
  T_FCRC32  : TTest;
  T_bCRC32  : TTest;
  T_Adler32 : TTest;
  T_BJL3    : TTest;
  T_BJDelphi: TTest;
  T_CRC64   : TTest;
  T_bCRC64  : TTest;
  T_MD4     : TTest;
  T_ED2K    : TTest;
  T_MD5     : TTest;
  T_SHA1    : TTest;
  T_SHA224  : TTest;
  T_SHA256  : TTest;
  T_SHA384  : TTest;
  T_SHA512  : TTest;
  T_Whirl   : TTest;
  T_RMD160  : TTest;
  T_SHA5_224: TTest;
  T_SHA5_256: TTest;
  T_SHA3_224: TTest;
  T_SHA3_256: TTest;
  T_SHA3_384: TTest;
  T_SHA3_512: TTest;
  T_BLAKS224: TTest;
  T_BLAKS256: TTest;
  T_BLAKB384: TTest;
  T_BLAKB512: TTest;
  MaxD100   : double;
  rnd       : integer;
  start     : comp;
  stop      : comp;
  HR        : THRTimer;
  pbuf      : ^TBuf;


{---------------------------------------------------------------------------}
procedure CalcStat(var Test: TTest);
var
  sum,diff: comp;
  sec,mean,delta,t: double;
  i,n: integer;
begin

  if rnd=0 then Test.done := false;
  if Test.done then exit;

  diff:= stop-start;
  sec := diff/CPUFrequency;
  i := rnd mod (MinRnd+1);
  Test.adiff[i] := diff;
  if rnd>MinRnd then n:=MinRnd else n:=rnd;

  sum := 0;
  for i:=0 to n do sum := sum + Test.adiff[i];
  mean := sum/(n+1);

  if rnd>0 then begin
    delta := abs(mean-Test.adiff[0]);
    for i:=1 to n do begin
      t := abs(mean-Test.adiff[0]);
      if t>delta then delta := t;
    end;
  end
  else begin
    delta := diff;
  end;
  {FPC3+ -O4 generates buggy code for Test.CpB := diff/BYTECOUNT;}
  t := diff;
  Test.CpB  := t/BYTECOUNT;
  Test.MBs  := MEGCOUNT/sec;
  Test.D100 := 100*delta/diff;
  Test.done := (rnd>MinRnd) and (Test.D100<DThresh);
  if Test.D100>MaxD100 then MaxD100 := Test.D100;
end;


{---------------------------------------------------------------------------}
procedure ShowResult(var Test: TTest);
begin
  CalcStat(Test);
  writeln(' ',Test.name,'':12-length(Test.name), Test.CpB:8:1, Test.MBs:8:2, Test.D100:8:1);
end;


{---------------------------------------------------------------------------}
procedure CRC16_Test;
var
  bc: word;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_CRC16.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do CRC16Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_CRC16);
end;


{---------------------------------------------------------------------------}
procedure CRC24_Test;
var
  bc: longint;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_CRC24.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do CRC24Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_CRC24);
end;


{---------------------------------------------------------------------------}
procedure CRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_CRC32.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do CRC32Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_CRC32);
end;


{---------------------------------------------------------------------------}
procedure BJL3_Test;
var
  bc: longint;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_CRC32.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do BJL3Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_BJL3);
end;


{---------------------------------------------------------------------------}
procedure FCRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_FCRC32.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do FCRC32Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_FCRC32);
end;


{---------------------------------------------------------------------------}
procedure Adler32_Test;
var
  bc: longint;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_Adler32.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Adler32Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_Adler32);
end;


{---------------------------------------------------------------------------}
procedure CRC64_Test;
var
  bc: TCRC64;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_CRC64.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do CRC64Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_CRC64);
end;


{---------------------------------------------------------------------------}
procedure ED2K_Test;
var
  bc: TED2KResult;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_ED2K.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do ED2K_Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_ED2K);
end;


{---------------------------------------------------------------------------}
procedure MD4_Test;
var
  bc: TMD4Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_MD4.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do MD4Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_MD4);
end;


{---------------------------------------------------------------------------}
procedure MD5_Test;
var
  bc: TMD5Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_MD5.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do MD5Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_MD5);
end;


{---------------------------------------------------------------------------}
procedure RMD160_Test;
var
  bc: TRMD160Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_RMD160.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do RMD160Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_RMD160);
end;


{---------------------------------------------------------------------------}
procedure SHA1_Test;
var
  bc: TSHA1Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA1.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA1Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA1);
end;


{---------------------------------------------------------------------------}
procedure SHA224_Test;
var
  bc: TSHA224Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA224.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA224Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA224);
end;


{---------------------------------------------------------------------------}
procedure SHA256_Test;
var
  bc: TSHA256Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA256.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA256Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA256);
end;


{---------------------------------------------------------------------------}
procedure SHA384_Test;
var
  bc: TSHA384Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA384.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA384Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA384);
end;


{---------------------------------------------------------------------------}
procedure SHA512_Test;
var
  bc: TSHA512Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA512.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA512Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA512);
end;


{---------------------------------------------------------------------------}
procedure Whirl_Test;
var
  bc: TWhirlDigest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_Whirl.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Whirl_Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_Whirl);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224_Test;
var
  bc: TSHA5_224Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA5_224.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA5_224Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA5_224);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256_Test;
var
  bc: TSHA5_256Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA5_256.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA5_256Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA5_256);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224_Test;
var
  bc: TSHA3_224Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA3_224.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA3_224Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA3_224);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256_Test;
var
  bc: TSHA3_256Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA3_256.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA3_256Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA3_256);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384_Test;
var
  bc: TSHA3_384Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA3_384.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA3_384Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA3_384);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512_Test;
var
  bc: TSHA3_512Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_SHA3_512.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do SHA3_512Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_SHA3_512);
end;


{---------------------------------------------------------------------------}
procedure Blake2s_224_Test;
var
  bc: TBlake2S_224Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_BLAKS224.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Blaks224Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_BLAKS224);
end;

{---------------------------------------------------------------------------}
procedure Blake2s_256_Test;
var
  bc: TBlake2S_256Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_BLAKS256.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Blaks256Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_BLAKS256);
end;


{---------------------------------------------------------------------------}
procedure Blake2b_384_Test;
var
  bc: TBlake2B_384Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_BLAKB384.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Blakb384Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_BLAKB384);
end;


{---------------------------------------------------------------------------}
procedure Blake2b_512_Test;
var
  bc: TBlake2B_512Digest;
  rounds: integer;
begin
  if (rnd<=MinRnd) or not T_BLAKB512.done then begin
    start := ReadCycles(HR);
    for rounds:=1 to NUMROUNDS do Blakb512Full(bc, pbuf, sizeof(TBuf));
    stop := ReadCycles(HR);
  end;
  ShowResult(T_BLAKB512);
end;


var
  i: word;
  done: boolean;
begin

  {$ifdef BASM16}
    {$ifdef DumpAlign}
      if readkey=#27 then halt;
    {$endif}
  {$endif}

  {$ifdef VER90 }
    InitCRT;  {D2}
  {$endif}
  {$ifdef WIN32or64}
    if Paramcount=0 then SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
  {$endif}

  randseed := 1234567;
  new(pbuf);
  for i:=1 to NUMBYTES do pbuf^[i] := random(256);
  StartTimer(HR);

  T_CRC16.name   := 'CRC16';
  T_CRC24.name   := 'CRC24';
  T_CRC32.name   := 'CRC32';
  T_bCRC32.name  := 'bCRC32';
  T_FCRC32.name  := 'FCRC32';
  T_Adler32.name := 'Adler32';
  T_BJL3.name    := 'BJ lookup3';
  T_BJDelphi.name:= 'BJ Delphi';
  T_CRC64.name   := 'CRC64';
  T_bCRC64.name  := 'bCRC64';
  T_ED2K.name    := 'eDonkey';
  T_MD4.name     := 'MD4';
  T_MD5.name     := 'MD5';
  T_SHA1.name    := 'SHA1';
  T_SHA224.name  := 'SHA224';
  T_SHA256.name  := 'SHA256';
  T_SHA384.name  := 'SHA384';
  T_SHA512.name  := 'SHA512';
  T_SHA5_224.name:= 'SHA512/224';
  T_SHA5_256.name:= 'SHA512/256';
  T_Whirl.name   := 'Whirlpool';
  T_RMD160.name  := 'RIPEMD160';
  T_SHA3_224.name:= 'SHA3-224';
  T_SHA3_256.name:= 'SHA3-256';
  T_SHA3_384.name:= 'SHA3-384';
  T_SHA3_512.name:= 'SHA3-512';
  T_BLAKS224.name:= 'Blake2s-224';
  T_BLAKS256.name:= 'Blake2s-256';
  T_BLAKB384.name:= 'Blake2b-384';
  T_BLAKB512.name:= 'Blake2b-512';

  clrscr;
  {$ifdef WINCRT}
    writeln('Name        ':13, 'Cyc/B':8, 'MB/s':8, 'D[%]':8, CPUFrequency/1E6:10:1);
  {$else}
    textcolor(lightgreen);
    writeln('Name        ':13, 'Cyc/B':8, 'MB/s':8, 'D[%]':8, CPUFrequency/1E6:10:1);
    textcolor(lightgray);
  {$endif}
  done := false;
  rnd  := 0;
  repeat
    ReStartTimer(HR);
    gotoxy(1,2);
    MaxD100 := 0.0;
    CRC16_Test;
    CRC24_Test;
    CRC32_Test;
    FCRC32_Test;
    Adler32_Test;
    BJL3_Test;
    CRC64_Test;
    ED2K_Test;
    MD4_Test;
    MD5_Test;
    RMD160_Test;
    SHA1_Test;
    SHA224_Test;
    SHA256_Test;
    SHA384_Test;
    SHA512_Test;
    SHA5_224_Test;
    SHA5_256_Test;
    Whirl_Test;
    SHA3_224_Test;
    SHA3_256_Test;
    SHA3_384_Test;
    SHA3_512_Test;
    Blake2s_224_Test;
    Blake2s_256_Test;
    Blake2b_384_Test;
    Blake2b_512_Test;
    inc(rnd);
    writeln('Rounds: ',rnd);
    {Some compilers have no break!!}
    if keypressed and (readkey=#27) then done := true;
    if (rnd>MinRnd) and (MaxD100 < DThresh)  then done := true;
 until done;
end.
