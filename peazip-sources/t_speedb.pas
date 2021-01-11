{-Test Cyc/B and MB/s for CRC/HASH, we 2003-2017}

program t_speedb;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
  {$B-,N+}
{$endif}

{$i-}

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
  bcrc32,
  bcrc64,
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
  crc24,
  crc16;

type
  TMeth = (_CRC16, _CRC24, _CRC32, _FCRC32, _bCRC32, _Adler32, _CRC64, _bCRC64, _MD4, _ED2K,
           _MD5, _RMD160, _SHA1, _SHA224, _SHA256, _SHA384,
           _SHA512, _SHA5_224, _SHA5_256, _Whirl,
           _SHA3_224, _SHA3_256, _SHA3_384, _SHA3_512,
           _Blake_224, _Blake_256, _Blake_384, _Blake_512
           );

const
  tmlow  = _CRC16;     {no low() for Versions < 7}
  tmhigh = _SHA3_512;

const
  NUMBYTES  = 50000;
  NUMROUNDS = 20;
  BYTECOUNT = NUMBYTES*NUMROUNDS;
  MEGCOUNT  = BYTECOUNT/1E6;
  DThresh   = 0.5;


{$ifndef BIT16}
  MinRnd = 20;
{$else}
  {$ifdef BASM16}
    MinRnd = 10;
  {$else}
    MinRnd = 5;
  {$endif}
{$endif}

type
  TCompArray = array[0..MinRnd] of comp;
  TBuf       = array[1..NUMBYTES] of byte;
  TSnglTest  = record
                 name : string[11];
                 adiff: TCompArray;
                 start: comp;
                 stop : comp;
                 mdiff: double;
                 CpB  : double;
                 MBs  : double;
                 D100 : double;
                 done : boolean;
                 skip : boolean;
               end;

   TTestArr  = array[TMeth] of TSnglTest;

var
  Tests    : TTestArr;
  MaxD100  : double;
  rnd      : integer;
  pbuf     : ^TBuf;
  alldone  : boolean;
  HR       : THRTimer;


{---------------------------------------------------------------------------}
procedure CalcStat(var Test: TSnglTest);
  {-Calculate restult for single test}
var
  sum,diff: comp;
  sec,mean,delta,t: double;
  i,n: integer;
begin
  if Test.done then exit;
  diff:= Test.stop-Test.start;
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
procedure ShowResult(var Test: TSnglTest);
  {-Calculate and show single test results}
begin
  CalcStat(Test);
  writeln(' ',Test.name,'':11-length(Test.name), Test.CpB:8:1, Test.MBs:8:2, Test.D100:8:1);
end;


{---------------------------------------------------------------------------}
procedure CRC16_Test;
var
  bc: word;
  rounds: integer;
begin
  with Tests[_CRC16] do begin
    if skip then exit;
    if (rnd<=MinRnd) or not done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do CRC16Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_CRC16]);
  end;
end;


{---------------------------------------------------------------------------}
procedure CRC24_Test;
var
  bc: longint;
  rounds: integer;
begin
  with Tests[_CRC24] do begin
    if skip then exit;
    if not Tests[_CRC24].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do CRC24Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_CRC24]);
  end;
end;


{---------------------------------------------------------------------------}
procedure CRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  with Tests[_CRC32] do begin
    if skip then exit;
    if not Tests[_CRC32].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do CRC32Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_CRC32]);
  end;
end;


{---------------------------------------------------------------------------}
procedure bCRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  with Tests[_bCRC32] do begin
    if skip then exit;
    if not Tests[_bCRC32].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do bCRC32Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_bCRC32]);
  end;
end;


{---------------------------------------------------------------------------}
procedure FCRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  with Tests[_FCRC32] do begin
    if skip then exit;
    if not Tests[_FCRC32].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do FCRC32Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_FCRC32]);
  end;
end;


{---------------------------------------------------------------------------}
procedure Adler32_Test;
var
  bc: longint;
  rounds: integer;
begin
  with Tests[_Adler32] do begin
    if skip then exit;
    if not Tests[_Adler32].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Adler32Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Adler32]);
  end;
end;


{---------------------------------------------------------------------------}
procedure CRC64_Test;
var
  bc: TCRC64;
  rounds: integer;
begin
  with Tests[_CRC64] do begin
    if skip then exit;
    if not Tests[_CRC64].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do CRC64Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_CRC64]);
  end;
end;


{---------------------------------------------------------------------------}
procedure bCRC64_Test;
var
  bc: TCRC64b;
  rounds: integer;
begin
  with Tests[_bCRC64] do begin
    if skip then exit;
    if not Tests[_bCRC64].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do bCRC64Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_bCRC64]);
  end;
end;


{---------------------------------------------------------------------------}
procedure ED2K_Test;
var
  bc: TED2KResult;
  rounds: integer;
begin
  with Tests[_ED2K] do begin
    if skip then exit;
    if not Tests[_ED2K].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do ED2K_Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_ED2K]);
  end;
end;


{---------------------------------------------------------------------------}
procedure MD4_Test;
var
  bc: TMD4Digest;
  rounds: integer;
begin
  with Tests[_MD4] do begin
    if skip then exit;
    if not Tests[_MD4].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do MD4Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_MD4]);
  end;
end;


{---------------------------------------------------------------------------}
procedure MD5_Test;
var
  bc: TMD5Digest;
  rounds: integer;
begin
  with Tests[_MD5] do begin
    if skip then exit;
    if not Tests[_MD5].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do MD5Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_MD5]);
  end;
end;


{---------------------------------------------------------------------------}
procedure RMD160_Test;
var
  bc: TRMD160Digest;
  rounds: integer;
begin
  with Tests[_RMD160] do begin
    if skip then exit;
    if not Tests[_RMD160].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do RMD160Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_RMD160]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA1_Test;
var
  bc: TSHA1Digest;
  rounds: integer;
begin
  with Tests[_SHA1] do begin
    if skip then exit;
    if not Tests[_SHA1].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA1Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA1]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA224_Test;
var
  bc: TSHA224Digest;
  rounds: integer;
begin
  with Tests[_SHA224] do begin
    if skip then exit;
    if not Tests[_SHA224].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA224Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA224]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA256_Test;
var
  bc: TSHA256Digest;
  rounds: integer;
begin
  with Tests[_SHA256] do begin
    if skip then exit;
    if not Tests[_SHA256].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA256Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA256]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA384_Test;
var
  bc: TSHA384Digest;
  rounds: integer;
begin
  with Tests[_SHA384] do begin
    if skip then exit;
    if not Tests[_SHA384].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA384Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA384]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA512_Test;
var
  bc: TSHA512Digest;
  rounds: integer;
begin
  with Tests[_SHA512] do begin
    if skip then exit;
    if not Tests[_SHA512].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA512Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA512]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA5_224_Test;
var
  bc: TSHA5_224Digest;
  rounds: integer;
begin
  with Tests[_SHA5_224] do begin
    if skip then exit;
    if not Tests[_SHA5_224].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA5_224Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA5_224]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA5_256_Test;
var
  bc: TSHA5_256Digest;
  rounds: integer;
begin
  with Tests[_SHA5_256] do begin
    if skip then exit;
    if not Tests[_SHA5_256].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA5_256Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA5_256]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA3_224_Test;
var
  bc: TSHA3_224Digest;
  rounds: integer;
begin
  with Tests[_SHA3_224] do begin
    if skip then exit;
    if not Tests[_SHA3_224].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA3_224Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA3_224]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA3_256_Test;
var
  bc: TSHA3_256Digest;
  rounds: integer;
begin
  with Tests[_SHA3_256] do begin
    if skip then exit;
    if not Tests[_SHA3_256].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA3_256Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA3_256]);
  end;
end;

{---------------------------------------------------------------------------}
procedure SHA3_384_Test;
var
  bc: TSHA3_384Digest;
  rounds: integer;
begin
  with Tests[_SHA3_384] do begin
    if skip then exit;
    if not Tests[_SHA3_384].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA3_384Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA3_384]);
  end;
end;


{---------------------------------------------------------------------------}
procedure SHA3_512_Test;
var
  bc: TSHA3_512Digest;
  rounds: integer;
begin
  with Tests[_SHA3_512] do begin
    if skip then exit;
    if not Tests[_SHA3_512].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do SHA3_512Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_SHA3_512]);
  end;
end;


{---------------------------------------------------------------------------}
procedure Blake_224_Test;
var
  bc: TBlake2s_224Digest;
  rounds: integer;
begin
  with Tests[_Blake_224] do begin
    if skip then exit;
    if not Tests[_Blake_224].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Blaks224Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Blake_224]);
  end;
end;


{---------------------------------------------------------------------------}
procedure Blake_256_Test;
var
  bc: TBlake2s_256Digest;
  rounds: integer;
begin
  with Tests[_Blake_256] do begin
    if skip then exit;
    if not Tests[_Blake_256].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Blaks256Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Blake_256]);
  end;
end;

{---------------------------------------------------------------------------}
procedure Blake_384_Test;
var
  bc: TBlake2b_384Digest;
  rounds: integer;
begin
  with Tests[_Blake_384] do begin
    if skip then exit;
    if not Tests[_Blake_384].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Blakb384Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Blake_384]);
  end;
end;


{---------------------------------------------------------------------------}
procedure Blake_512_Test;
var
  bc: TBlake2b_512Digest;
  rounds: integer;
begin
  with Tests[_Blake_512] do begin
    if skip then exit;
    if not Tests[_Blake_512].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Blakb512Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Blake_512]);
  end;
end;



{---------------------------------------------------------------------------}
procedure Whirl_Test;
var
  bc: TWhirlDigest;
  rounds: integer;
begin
  with Tests[_Whirl] do begin
    if skip then exit;
    if not Tests[_Whirl].done then begin
      start := ReadCycles(HR);
      for rounds:=1 to NUMROUNDS do Whirl_Full(bc, pbuf, sizeof(TBuf));
      stop := ReadCycles(HR);
    end;
    ShowResult(Tests[_Whirl]);
  end;
end;


{---------------------------------------------------------------------------}
procedure InitTests;
  {-Initialize test environment, read ini file}
var
  i: word;
  tm: TMeth;
  s: string[20];
  ts: boolean;
  ini: text;
begin
  {$ifdef VER90 }
    InitCRT;  {D2}
  {$endif}
  randseed := 1234567;
  new(pbuf);
  for i:=1 to NUMBYTES do pbuf^[i] := random(256);

  fillchar(Tests, sizeof(Tests), 0);

  Tests[_CRC16   ].name := 'CRC16';
  Tests[_CRC24   ].name := 'CRC24';
  Tests[_CRC32   ].name := 'CRC32';
  Tests[_FCRC32  ].name := 'FCRC32';
  Tests[_bCRC32  ].name := 'bCRC32';
  Tests[_Adler32 ].name := 'Adler32';
  Tests[_CRC64   ].name := 'CRC64';
  Tests[_bCRC64  ].name := 'bCRC64';
  Tests[_ED2K    ].name := 'eDonkey';
  Tests[_MD4     ].name := 'MD4';
  Tests[_MD5     ].name := 'MD5';
  Tests[_RMD160  ].name := 'RIPEMD160';
  Tests[_SHA1    ].name := 'SHA1';
  Tests[_SHA224  ].name := 'SHA224';
  Tests[_SHA256  ].name := 'SHA256';
  Tests[_SHA384  ].name := 'SHA384';
  Tests[_SHA512  ].name := 'SHA512';
  Tests[_SHA5_224].name := 'SHA512/224';
  Tests[_SHA5_256].name := 'SHA512/256';
  Tests[_Whirl   ].name := 'Whirlpool';
  Tests[_SHA3_224].name := 'SHA3-224';
  Tests[_SHA3_256].name := 'SHA3-256';
  Tests[_SHA3_384].name := 'SHA3-384';
  Tests[_SHA3_512].name := 'SHA3-512';
  Tests[_Blake_224].name := 'Blake2s-224';
  Tests[_Blake_256].name := 'Blake2s-256';
  Tests[_Blake_384].name := 'Blake2b-384';
  Tests[_Blake_512].name := 'Blake2b-512';

  assign(ini,'T_SPEEDB.INI');
  reset(ini);
  if IOResult=0 then begin
    while not eof(ini) do begin
      readln(ini,s);
      if (IOResult=0) and (s<>'') then begin
        if (s[1]='+') or (s[1]='-') then begin
          ts := s[1]='-';
          delete(s,1,1);
          for tm:=tmlow to tmhigh do begin
            if s=Tests[tm].name then Tests[tm].skip := ts;
          end;
        end;
      end;
    end;
    close(ini);
    if IOResult<>0 then ;
  end;

  alldone := false;
  rnd  := 0;

  {$ifdef WIN32}
    if Paramcount=0 then SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
  {$endif}

  clrscr;
  {$ifdef WINCRT}
    writeln('Name      ':12, 'Cyc/B':8, 'MB/s':8, 'D[%]':8, CPUFrequency/1E6:10:1);
  {$else}
    textcolor(lightgreen);
    writeln('Name      ':12, 'Cyc/B':8, 'MB/s':8, 'D[%]':8, CPUFrequency/1E6:10:1);
    textcolor(lightgray);
  {$endif}
end;


var
  idx: TMeth;
  sum: double;
begin

  {$ifdef WIN32or64}
    if Paramcount=0 then SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
  {$endif}

  {$ifdef BASM16}
    {$ifdef DumpAlign}
      if readkey=#27 then halt;
    {$endif}
  {$endif}

  InitTests;

  repeat

    StartTimer(HR);
    gotoxy(1,2);
    MaxD100 := 0.0;

    CRC16_Test;
    CRC24_Test;
    CRC32_Test;
    FCRC32_Test;
    bCRC32_Test;
    Adler32_Test;
    CRC64_Test;
    bCRC64_Test;
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
    Blake_224_Test;
    Blake_256_Test;
    Blake_384_Test;
    Blake_512_Test;

    inc(rnd);
    writeln('Rounds: ',rnd);
    {Some compilers have no break!!}
    if keypressed and (readkey=#27) then alldone := true;
    if (rnd>MinRnd) and (MaxD100 < DThresh)  then alldone := true;

  until alldone;

  sum := 0;
  for idx := tmlow to tmhigh do with Tests[idx] do begin
    if not skip then sum := sum + CpB;
  end;
  if sum>0 then writeln('Overall':12, sum:8:1, CPUFrequency/1E6/sum:8:2);
end.
