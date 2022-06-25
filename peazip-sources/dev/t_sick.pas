{-Test prog for crc_sick, we 17.12.10}

program t_sick;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  crc_sick;
begin
  writeln('CRC/Hash test program [CRC-16/Sick]   (c) 2002-2012 W.Ehrhardt');
  writeln('CRC-16/Sick selftest: ',crc_sick_selftest);
end.
