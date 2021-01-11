unit HRTimer;


{High resolution timer routines with comp data type}
{and PerformanceCounter routines for WIN32/64      }


interface


{$i STD.INC}


{$ifndef CPUARM}
  {$define AUTOHALT}  {halt(99) if no RDTSC support}
{$endif}


(*************************************************************************

 DESCRIPTION     :  High resolution timer routines with comp data type
                    PerformanceCounter routines for WIN32/64

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.11.03  W.Ehrhardt  Initial version: TP6/7, D2-D6, VP
 0.11     15.11.03  we          FPC
 0.12     15.11.03  we          BP7 WIN/DPMI, D1
 0.13     16.11.03  we          16 bit ASM, ReadTSC, TP5/5.5
 0.14     29.11.03  we          {$define AUTOHALT}
 0.15     06.13.03  we          Uses unit TSC
 0.16     01.01.04  we          Insert missing u in CPUFrequency :)
 0.17     02.01.04  we          Calibration, new functions
 0.18     06.01.04  we          FPC-Warnings off for comp:=comp-comp
 0.19     11.04.04  we          Delphi 7
 0.20     09.04.05  we          CPUFrequency from GetTickCount if high-res
                                performance counter N/A; Delphi 9, WDOSX
 0.21     11.12.05  we          RestartTimer
 0.22     20.11.07  we          Fix for WIN32 and $X-
 0.23     30.03.08  we          CalcCPUFrequency via sysutils.time on non-Windows 32 bit systems
 0.24     05.07.09  we          D12 fix (int64 absolute comp)
 0.25     06.07.09  we          CPUFrequency: double, check monotony
 0.26     19.01.12  we          BIT32or64
 0.27     17.12.12  we          D17 adjustment
 0.28     25.12.12  we          $J+ via J_OPT
 0.29     17.03.17  we          Simple hacks for FPC/CPUARM
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2003-2017 Wolfgang Ehrhardt

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


{$ifndef FPC}
  {$B-,N+}
{$endif}

{$ifdef J_OPT}
  {$J+}
{$endif}


uses TSC;

const
  CPUFrequency : double = -1;   {CPU Frequency in Hz}

type
  THRTimer = record
               Start: comp;   {First read after calib}
               Diff0: comp;   {Min diff for two reads}
               Last : comp;   {Last read after start }
             end;

function HasRDTSC: boolean;
  {-check if rdtsc supported}

procedure ReadTSC(var Cnt: Comp);
  {-Read Time Stamp Counter}

procedure StartTimer(var HR: THRTimer);
  {-Start and calibrates timer}

procedure RestartTimer(var HR: THRTimer);
  {-Read TSC into HR.Start}

function ReadSeconds(var HR: THRTimer): double;
  {-Return elapsed (calibrated) seconds since Start}

function ReadCycles(var HR: THRTimer): comp;
  {-Return elapsed (calibrated) cycles since Start}

function LastSeconds(var HR: THRTimer): double;
  {-Return elapsed (calibrated) seconds since Start using Last}

function LastCycles(var HR: THRTimer): comp;
  {-Return elapsed (calibrated) cycles since Start using Last}

procedure StoreLast(var HR: THRTimer);
  {-Read TSC into last}

{$ifdef WIN32or64}
function PerformanceCounter(var Cnt: Comp): boolean;
  {-Get value high-resolution performance counter}

function PerformanceFrequency(var Freq: Comp): boolean;
  {-Get frequency of the high-resolution performance counter}
{$endif}

procedure CalcCPUFrequency;
  {-Calculate CPU frequency, called from initialisation}

implementation



{$ifdef BIT32or64}
  {$ifdef WIN32or64}
    {$ifdef UNIT_SCOPE}
      uses winapi.windows;
    {$else}
      uses windows;
    {$endif}
  {$else}
    uses sysutils;
  {$endif}
{$endif}


{---------------------------------------------------------------------------}
procedure ReadTSC(var Cnt: Comp);
  {-Read Time Stamp Counter}
begin
  _ReadTSC(TCtrRec(Cnt));
end;

{$ifdef FPC} {$WARNINGS OFF} {$endif} {Warning for comp := comp-comp}

{---------------------------------------------------------------------------}
procedure StartTimer(var HR: THRTimer);
  {-Start and calibrates timer HR}
const
  CALCNT = 10;
var
  i: integer;
  Stop,Diff: comp;
begin
  with HR do begin
    {Measure empty statement several times, take minimum as Diff0}
    ReadTSC(Start);
    ReadTSC(Stop);
    Diff0 := Stop-Start;
    for i:=1 to CALCNT-1 do begin
      ReadTSC(Start);
      ReadTSC(Stop);
      Diff := Stop-Start;
      if Diff<Diff0 then Diff0 := Diff;
    end;
    if Diff0<0 then Diff0 := 0;
    {Start measurement}
    ReadTSC(Start);
  end;
end;


{---------------------------------------------------------------------------}
procedure RestartTimer(var HR: THRTimer);
  {-Read TSC into HR.Start}
begin
  ReadTSC(HR.Start);
end;


{---------------------------------------------------------------------------}
function LastCycles(var HR: THRTimer): comp;
  {-Return elapsed (calibrated) cycles since Start using Last}
begin
  with HR do LastCycles := Last-Start-Diff0;
end;


{---------------------------------------------------------------------------}
function LastSeconds(var HR: THRTimer): double;
  {-Return elapsed (calibrated) seconds since Start using Last}
begin
  LastSeconds := LastCycles(HR)/CPUFrequency;
end;


{---------------------------------------------------------------------------}
function ReadCycles(var HR: THRTimer): comp;
  {-Return elapsed (calibrated) cycles since Start}
begin
  with HR do begin
    ReadTSC(Last);
    ReadCycles := Last-Start-Diff0;
  end;
end;

{$ifdef FPC} {$WARNINGS ON} {$endif}

{---------------------------------------------------------------------------}
function ReadSeconds(var HR: THRTimer): double;
  {-Return elapsed (calibrated) seconds since Start}
begin
  ReadSeconds := ReadCycles(HR)/CPUFrequency;
end;


{---------------------------------------------------------------------------}
procedure StoreLast(var HR: THRTimer);
  {-Read TSC into last}
begin
  ReadTSC(HR.Last);
end;

{---------------------------------------------------------------------------}
function HasRDTSC: boolean;
  {-check if RDTSC supported}
begin
  HasRDTSC := _HasRDTSC;
end;



{$ifdef BIT32or64}

{$ifdef WIN32or64}
{---------------------------------------------------------------------------}
function PerformanceCounter(var Cnt: Comp): boolean;
  {-Get value high-resolution performance counter}
{$ifdef D4Plus}
  var c64: int64 absolute cnt; {D12 does not allow typecast Comp <-> TULargeInteger}
{$endif}
begin
  {$ifdef D4Plus}
    PerformanceCounter := QueryPerformanceCounter(c64);
  {$else}
    {FPC does not lile 'result'}
    PerformanceCounter := QueryPerformanceCounter(TLargeInteger(Cnt));
  {$endif}
end;

{---------------------------------------------------------------------------}
function PerformanceFrequency(var Freq: Comp): boolean;
  {-Get frequency of the high-resolution performance counter}

{$ifdef D4Plus}
  var c64: int64 absolute Freq;  {D12 does not allow typecast Comp <-> TULargeInteger}
{$endif}
begin
  {$ifdef D4Plus}
    PerformanceFrequency := QueryPerformanceFrequency(c64);
  {$else}
    PerformanceFrequency := QueryPerformanceFrequency(TLargeInteger(Freq));
  {$endif}
end;


{---------------------------------------------------------------------------}
procedure CalcCPUFrequency;
  {-Calculate CPU frequency from 100 ms}
var
  f,c1,c2,r1,r2: comp;
  t0, t1, t2: longint;
  done: boolean;
begin
  done := false;
  {note that this code is suboptimal for multicore systems!}
  if PerformanceFrequency(f) and PerformanceCounter(c1) then begin
    ReadTSC(r1);
    sleep(100);
    if PerformanceCounter(c2) then ;
    ReadTSC(r2);
    if (c2>c1) and (r2>r1) then begin
      CPUFrequency := (r2 - r1) * f / (c2 - c1);
      done := true;
    end;
  end;
  if not done then begin
    {high-resolution performance counter not available, calc from GetTickCount}
    t2:=GetTickCount;
    repeat
      t1:=GetTickCount;
    until t1<>t2;
    ReadTSC(r1);
    repeat
      t0:=GetTickCount;
    until t1<>t0;
    repeat
      t2:=GetTickCount;
    until t0<>t2;
    ReadTSC(r2);
    CPUFrequency := (r2 - r1) * 1000 / (t2 - t1);
  end;
end;
{$else}
{---------------------------------------------------------------------------}
procedure CalcCPUFrequency;
  {-Calculate CPU frequency for BIT32/64 and not WIN32/64}
var
  r1,r2: comp;
  t0, t1, t2: longint;
begin
  {Assume now high-resolution performance counter and no systick}
  {available. Calculate CPU frequency from TSC and 10ms changes.}
  {$ifdef debug}
    {$ifdef HAS_MSG}
      {$message 'CalcCPUFrequency via sysutils.time'}
    {$endif}
  {$endif}
  t2:=round(8640000*time);
  repeat
    t1:=round(8640000*time);
  until t1<>t2;
  ReadTSC(r1);
  repeat
    t0:=round(8640000*time);
  until t1<>t0;
  repeat
    t2:=round(8640000*time);
  until t0<>t2;
  ReadTSC(r2);
  CPUFrequency := (r2 - r1) * 100 / (t2 - t1);
end;
{$endif}

{$else}

{16 bit compiler}

{$ifdef DPMI}
{---------------------------------------------------------------------------}
function Tick: byte;
begin
  Tick := Mem[Seg0040:$6c];
end;
{$else}
var
  tick: byte absolute $40:$6c;
{$endif}


{---------------------------------------------------------------------------}
procedure CalcCPUFrequency;
  {-Calculate CPU frequency from 2 ticks}
var
  t0,t1: byte;
  Start, Stop: comp;
begin
  t0 := tick;
  repeat
    t1:=tick;
  until t1<>t0;
  ReadTSC(Start);
  repeat
    t0:=tick;
  until t1<>t0;
  repeat
    t1:=tick;
  until t1<>t0;
  ReadTSC(Stop);
  CPUFrequency := (Stop-Start)*1193181/$20000;
end;

{$endif}


begin
  {$ifdef D4Plus}
  assert(sizeof(int64)=sizeof(comp), 'sizeof(int64)=sizeof(comp)');
  {$endif}

  {$ifdef CPUARM}
    {This will give bogus frequency, do not use in applications}
    CalcCPUFrequency;
    if CPUFrequency<=0.0 then CPUFrequency := -1.0;
  {$else}
    if HasRDTSC then begin
      CalcCPUFrequency;
      if CPUFrequency<=0.0 then CPUFrequency := -1.0;
    end
    else begin
      {$ifdef AUTOHALT}
        halt(99);
      {$endif}
    end;
  {$endif}
end.
