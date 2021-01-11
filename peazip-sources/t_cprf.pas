{-Test program for aes_cprf, (c) we 05.2007}

program T_CPRF;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_cprf;

begin
  writeln('Selftest AES CMAC PRF-128: ', AES_CPRF128_selftest);
end.
