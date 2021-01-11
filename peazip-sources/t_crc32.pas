{-Test prog for CRC32, we 18.03.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     crc32;

begin
  writeln('CRC32 self test passed: ',CRC32SelfTest);
end.
