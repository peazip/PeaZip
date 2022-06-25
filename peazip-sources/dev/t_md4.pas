{-Test prog for MD4, we 18.02.07}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     md4;

begin
  writeln('MD4 self test passed: ',MD4SelfTest);
end.
