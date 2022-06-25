{-Test prog for SHA384, we 19.11.03}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT,{$endif}
     sha384;

begin
  writeln('SHA 384 self test passed: ', SHA384SelfTest);
end.
