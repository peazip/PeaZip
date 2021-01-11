{-Test prog for SHA1, we 14.03.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     sha1;

begin
  writeln('SHA1 self test passed: ',SHA1SelfTest);
end.
