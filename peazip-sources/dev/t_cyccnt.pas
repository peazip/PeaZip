{-Test prog for AES enc/dec cycle count, we 2003}

program T_CYCCNT;

{.$define decrypt}

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}
uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  {$ifdef decrypt}
    AES_Decr,
  {$else}
    AES_Encr,
  {$endif}
  AES_Type,
  HRTimer;


const
  MMAX = 31;

var
  key: array[0..15] of byte;
  plain,ct: TAESBlock;
  ctx: TAESContext;
  HR: THRtimer;

var
  d: array[0..MMAX] of comp;
  diff: comp;
  mv,md,t,MBs: double;
  n: longint;
  i: integer;
begin
  fillchar(key,sizeof(key),0);
  fillchar(plain,sizeof(plain),0);
  fillchar(ct,sizeof(ct),0);

  {$ifdef decrypt}
    if AES_Init_Decr(Key, 128, ctx)<>0 then begin
      writeln('*** Error AES Init');
      exit;
    end;
  {$else}
    if AES_Init_Encr(Key, 128, ctx)<>0 then begin
      writeln('*** Error AES Init');
      exit;
    end;
  {$endif}


  n := 0;
  repeat
    StartTimer(HR);
    {$ifdef decrypt}
      AES_Decrypt(ctx, ct, plain);
    {$else}
      AES_Encrypt(ctx, plain, ct);
    {$endif}
    diff := ReadCycles(HR);
    if diff < 0 then begin
      writeln('diff < 0 ');
      halt;
    end;
    i := n and MMAX;
    d[i] := diff;
    if i=MMAX then begin
      mv := d[0];
      for i:=1 to MMAX do mv := mv + d[i];
      mv := mv / (MMAX+1);
      md := abs(mv-d[0]);
      for i:=1 to MMAX do begin
        t := abs(mv-d[i]);
        if t>md then md := t;
      end;
      md := md/mv;
      MBs := 16E-6/mv*CPUFrequency;
      if md<0.006 then begin
        writeln('CPU Frq = ', CPUFrequency/1E6:1:0);
        writeln('Cyc/Blk = ', mv:1:1);
        writeln('max dev = ', md*100:1:2);
        writeln('   MB/s = ', MBs:1:1);
        halt;
      end;
    end;
    inc(n)
  until n=0;
end.
