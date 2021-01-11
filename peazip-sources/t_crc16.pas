{-Test prog for CRC16, we 18.03.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     crc16;


begin
  writeln('CRC16 self test passed: ',CRC16SelfTest);
end.
