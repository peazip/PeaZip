{-Test prog for RIPEMD-160, we 31.01.06}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     rmd160;

begin
  writeln('RIPEMD-160 self test passed: ',RMD160SelfTest);
end.
