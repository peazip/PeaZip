{-Test prog for ED2K, we 19.02.07}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses 
  {$ifdef WINCRT} WinCRT, {$endif}
  ed2k;

begin
  writeln('ED2K self test passed: ',ed2k_SelfTest);
end.
