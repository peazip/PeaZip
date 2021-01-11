{-Test prog for bCRC32, we 06.07.03}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     bcrc32;


begin
  writeln('bCRC32 self test passed: ',bCRC32SelfTest);
end.
