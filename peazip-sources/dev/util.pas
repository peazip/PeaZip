{Program that uses all util units, use it for "Make"}

program util;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  BTypes,
  base2n,
  bitarray,
  compvers,
  dates,
  hrtimer,
  memh,
  mem_util,
  ministat,
  sort,
  tsc;

begin
  writeln('"Make" file for util units    (c) 2009-2014 W.Ehrhardt');
  writeln('Compiler: ',Compiler_Str);
end.
