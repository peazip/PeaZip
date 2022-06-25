{-Test program for compvers unit, we Nov. 2005}

program t_cv;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  compvers;

begin
  writeln('Compiler: ',Compiler_Str, ' - #', ord(Compiler_Sym));
end.


