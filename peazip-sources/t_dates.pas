{-Test prog for dates unit, we Dec. 2003}

program T_Dates;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  dates;
var
  dow, dum: word;
  y, m, d: word;
  hour,min,sec: word;
  JDM: Longint;
begin
  writeln('CJD: ', CurrentJulianDay);
  writeln(' ms: ', msCount);
  GetCurrentDate(dum,dum,dum,dow);
  writeln('dow: ', dow);
  writeln('30.12.1899=', JulianDay(1899, 12, 30));
  JDM := JulianDay(65535, 12, 31);
  writeln('31.12.65535=', JDM);
  JulianToDMY(JDM, y, m, d);
  writeln('JD ', JDM, ' = ',d,'.',m,'.',y);
  UnixToDMYhms(0, y,m,d,hour,min,sec);
  writeln('Unix ',0:10, ' = ',d:2,'.',m:2,'.',y, ' ',hour:2,':',min:2,':',sec:2);
  UnixToDMYhms(maxLongint, y,m,d,hour,min,sec);
  writeln('Unix ',maxLongint:10 , ' = ',d:2,'.',m:2,'.',y, ' ',hour:2,':',min:2,':',sec:2);
end.
