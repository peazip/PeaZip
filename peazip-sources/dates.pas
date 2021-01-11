unit dates;


{Current date/time routines, Julian day numbers, UnixToDMYhms}


interface

{$i STD.INC}

(*************************************************************************

 DESCRIPTION   :  Current date/time routines, Julian day numbers, UnixToDMYhms

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REFERENCES    :  R.G. Tantzen, Algorithm 199: CACM vol.6, no.8, p.444


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     07.12.03  W.Ehrhardt  Initial version: TP5-7, FPC Go2
 0.20     07.12.03  we          WIN32
 0.21     07.12.03  we          BP7Win
 0.30     07.12.03  we          Delphi 1
 0.31     11.04.04  we          Delphi 7
 0.32     25.10.05  we          D9/WDOSX
 0.33     22.01.07  we          UnixToDMYhms
 0.34     18.02.12  we          64-bit changes
 0.35     25.12.12  we          D17 adjustment
 0.36     25.04.17  we          FPC/CPUARM adjustment
 0.37     09.05.17  we          nowval in GetCurrentDate for ARM
 0.38     10.11.17  we          Use date/time for FPC/WINCE
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


const
  UseLocal : boolean = true;   {Uses local or UTC time, only for WIN32or64 or ARM}

procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}

procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}

function  msCount: longint;
  {-Returns current time as ms}

function  JulianDay(y, m, d: word): longint;
  {-Converts a date to a Julian day}

procedure JulianToDMY(jd: longint; var y, m, d: word);
  {-Returns the date corresponding to a Julian day}

function  CurrentJulianDay: longint;
  {-Converts the current date to a Julian day}

procedure UnixToDMYhms(ts: longint; var yyyy,mm,dd,h,m,s: word);
  {-Converts unix timestamp ts to yyyy-mm-dd h:m:s, ts<=0: 1970-01-01 00:00:00}


implementation


{$ifdef CPUARM}
  {$ifdef WinCE}
    uses SysUtils;
  {$else}
    uses SysUtils, DateUtils;
  {$endif}
{$else}
  {$ifdef WIN32or64}
    {$ifdef UNIT_SCOPE}
      uses winapi.windows;
    {$else}
      uses windows;
    {$endif}
    {$define GetLocal}
  {$else}
    {$ifdef windows}
      {$ifdef VER80}
        uses SysUtils;
      {$else}
        uses winDOS;
      {$endif}
    {$else}
      uses DOS;
    {$endif}
    {$undef GetLocal}
  {$endif}
{$endif}


{$ifdef CPUARM}

{$ifdef WINCE}
{---------------------------------------------------------------------------}
procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}
var
  DT: TDateTime;
begin
  DT := Date;
  DecodeDate(DT, y, m, d);
  w := (DayOfWeek(Date)+6) mod 7;
end;


{---------------------------------------------------------------------------}
procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}
var
  DT: TDateTime;
begin
  DT := Time;
  DecodeTime(DT, h, m, s, ms);
end;

{$else}
{---------------------------------------------------------------------------}
procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}
var
  nowval: TDateTime;
  t1, t2, t3, t4: word;
begin
  nowval := now;
  DecodeDateTime(nowval, y, m, d, t1, t2, t3, t4);
  DecodeDateWeek(nowval, t1, t2, w);
end;


{---------------------------------------------------------------------------}
procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}
var
  ty, tm, td: word;
begin
  DecodeDateTime(now, ty, tm, td, h, m, s, ms) ;
end;

{$endif}


{$else}

{$ifdef WIN32or64}
{---------------------------------------------------------------------------}
procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}
var
  ST: TSystemTime;
begin
  if UseLocal then GetLocalTime(ST) else GetSystemTime(ST);
  with ST do begin
    y := wYear;
    m := wMonth;
    d := wDay;
    w := wDayOfWeek;
  end;
end;


{---------------------------------------------------------------------------}
procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}
var
  ST: TSystemTime;
begin
  if UseLocal then GetLocalTime(ST) else GetSystemTime(ST);
  with ST do begin
    h  := wHour;
    m  := wMinute;
    s  := wSecond;
    ms := wMilliseconds;
  end;
end;

{$else}

{$ifdef VER80}

{Delphi 1}
{---------------------------------------------------------------------------}
procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}
var
  DT: TDateTime;
begin
  DT := Date;
  DecodeDate(DT, y, m, d);
  {D1: 1=Sunday!!}
  w := (DayOfWeek(Date)+6) mod 7;
end;


{---------------------------------------------------------------------------}
procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}
var
  DT: TDateTime;
begin
  DT := Time;
  DecodeTime(DT, h, m, s, ms);
end;

{$else}

{---------------------------------------------------------------------------}
procedure GetCurrentDate(var y, m, d, w: word);
  {-Returns current date}
begin
  GetDate(y,m,d,w);
end;


{---------------------------------------------------------------------------}
procedure GetCurrentTime(var h, m, s, ms: word);
  {-Returns current time}
begin
  GetTime(h, m, s, ms);
  {GetTime returns 1/100 s}
  ms := 10*ms;
end;
{$endif VER80}
{$endif GetLocal}
{$endif CPUARM}

{---------------------------------------------------------------------------}
function msCount: longint;
  {-Returns current time as ms}
var
  h, m, s, ms: word;
begin
  GetCurrentTime(h, m, s, ms);
  msCount := ((longint(h)*60 + m)*60 + s) * 1000 + ms;
end;


(************************************************************************
Conversions between Gregorian calendar date and Julian day number.
Ref: Robert G. Tantzen, Algorithm 199, in CACM  6, 8 (Aug 1963), page 444.
*************************************************************************)


{---------------------------------------------------------------------------}
function JulianDay (y, m, d: word): longint;
  {-Converts a date to a Julian day}
var
  t1, t2, t3, t4, t5 : longint;
begin
  if m > 2 then begin
    t1 := m - 3;
    t2 := y
  end
  else begin
    t1 := m + 9;
    t2 := y - 1
  end;
  t3 := t2 div 100;
  t4 := t2 mod 100;
  t5 := d;
  JulianDay := (146097 * t3) div 4 + (1461 * t4) div 4 +
               (153 * t1 + 2) div 5 + t5 + 1721119
end;


{---------------------------------------------------------------------------}
procedure JulianToDMY(jd: longint; var y, m, d: word);
  {-Returns the date corresponding to a Julian day}
var
  t1, t2, t3 : longint;
begin
  t1 := jd - 1721119;
  t3 := (4 * t1 - 1) div 146097;
  t1 := (4 * t1 - 1) mod 146097;
  t2 := t1 div 4;
  t1 := (4 * t2 + 3) div 1461;
  t2 := (4 * t2 + 3) mod 1461;
  t2 := (t2 + 4) div 4;
  m  := (5 * t2 - 3) div 153;
  t2 := (5 * t2 - 3) mod 153;
  d  := (t2 + 5) div 5;
  y  := 100 * t3 + t1;
  if m<10 then m := m + 3
  else begin
    m := m - 9;
    y := y + 1
  end
end;


{---------------------------------------------------------------------------}
function CurrentJulianDay: longint;
  {-Converts the current date to a Julian day}
var
  y,m,d,w: word;
begin
  GetCurrentDate(y,m,d,w);
  CurrentJulianDay := JulianDay(y,m,d);
end;


{---------------------------------------------------------------------------}
procedure UnixToDMYhms(ts: longint; var yyyy,mm,dd,h,m,s: word);
  {-Converts unix timestamp ts to yyyy-mm-dd h:m:s, ts<=0: 1970-01-01 00:00:00}
const
  Ref: longint = 2440588; {=JulianDay(1970,1,1)}
begin
  if ts<0 then ts := 0;
  JulianToDMY(Ref+(ts div 86400), yyyy, mm, dd);
  ts:= ts mod 86400;
  h := ts div 3600;  ts := ts mod 3600;
  m := ts div 60;
  s := ts mod 60;
end;


end.
