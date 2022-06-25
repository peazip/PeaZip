{-Test prog for MD5, we 14.03.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     md5;

begin
  writeln('MD5 self test passed: ',MD5SelfTest);
end.
