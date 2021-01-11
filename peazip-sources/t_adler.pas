{-Test prog for Adler32, we 30.08.03}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     Adler32;

begin
  writeln('Adler32 self test passed: ',Adler32SelfTest);
end.
