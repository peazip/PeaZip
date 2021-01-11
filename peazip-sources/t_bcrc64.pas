{-Test prog for bCRC64, we 07.07.03}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     bcrc64;


begin
  writeln('bCRC64 self test passed: ',bCRC64SelfTest);
end.
