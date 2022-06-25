{-Test prog for CRC64, we 07.07.03}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     crc64;

begin
  writeln('CRC64  self test passed: ',CRC64SelfTest);
end.
