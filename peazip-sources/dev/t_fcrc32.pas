{-Test prog for FCRC32, we 28.06.07}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     fcrc32;

begin
  writeln('FCRC32 self test passed: ',FCRC32SelfTest);
end.
