{-Test prog for SHA256, we 03.01.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     sha256;

begin
  writeln('SHA 256 self test passed: ', SHA256SelfTest);
end.
