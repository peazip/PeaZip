{-Test prog for Whirlpool}

program t_whirl;

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT,{$endif}
     mem_util, whirl512;


begin
  writeln('WhirlPool self test passed: ', Whirl_SelfTest);
end.
