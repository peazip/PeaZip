{CRC24 table calculation     (C) 2006 W.Ehrhardt}

program T_CTab24;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  mem_util;

const
  CRC24_POLY = $1864cfb;

{---------------------------------------------------------------------------}
procedure CalcTable;
  {-Calclate and output CRC24 table}
var
  CTab: array[0..255] of longint;
  i,b: integer;
  crc: longint;
begin
  for i:=0 to 255 do begin
    crc:= longint(i) shl 16;
    for b:=1 to 8 do begin
      crc := crc shl 1;
      if crc and $1000000 <> 0 then crc := crc xor CRC24_POLY;
    end;
    CTab[i] := crc;
  end;
  writeln('const');
  writeln('  CT24: array[0..255] of longint = (');
  for i:=0 to 255 do begin
    if i and 7 = 0 then write('':4);
    write('$',HexLong(CTab[i]));
    if i=255 then writeln
    else begin
      write(',');
      if i and 7 = 7 then writeln
    end;
  end;
  writeln('  );')
end;

begin
  writeln('T_CTab24 - CRC24 table calculation     (C) 2006 W.Ehrhardt');
  writeln;
  CalcTable;
end.
