{-Test prog for HRTimer/TSC, (c) WE Feb.2012}

program T_HRT;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}


uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  tsc,
  hrtimer;

var
  i: LongInt;
  Start, Stop, Cnt: comp;

begin
  if CPUFrequency<0 then begin
    writeln('No HR timer or CPUFrequency available');
    halt;
  end;
  writeln('Freq = ', CPUFrequency/1E6:1:1, ' MHz');

{$ifdef Win32or64}
  if PerformanceCounter(Cnt)   then writeln('PerformanceCounter  : ',Cnt);
  if PerformanceFrequency(cnt) then writeln('PerformanceFrequency: ',Cnt);
  {-Get frequency of the high-resolution performance counter}
{$endif}

  ReadTSC(Start);
  i:=1;
  while i<$2FFFFFF do inc(i);
  ReadTSC(Stop);
  _ReadCounter(TCtrRec(Cnt));
  WriteLn('Elapsed test time: ', 1000*(Stop-Start)/CPUFrequency:1:3, ' ms');
  {$ifdef FPC}
    {comp=int64 is no floating point!!}
    Writeln('HR Counter: ', Cnt);
    _HasRDTSC := false;
    _ReadCounter(TCtrRec(Cnt));
    Writeln('LR Counter: ', Cnt);
  {$else}
    Writeln('HR Counter: ', Cnt:1:0);
    _HasRDTSC := false;
    _ReadCounter(TCtrRec(Cnt));
    Writeln('LR Counter: ', Cnt:1:0);
  {$endif}
end.
