{-Test prog for SHA224, we 02.01.04}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT,{$endif}
     sha224;

begin
  writeln('SHA224 self test passed: ', SHA224SelfTest);
end.
