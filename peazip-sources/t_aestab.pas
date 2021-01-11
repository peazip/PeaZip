program t_aestab;

(*************************************************************************

 DESCRIPTION     :  Calculate static AES tables

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] http://csrc.nist.gov/fips/fips-197.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     17.09.03  we          Init version
 1.01     18.09.03  we          duplicate GF routines and rotword
 1.10     05.10.03  we          STD.INC, TP5-6
 1.20     09.01.04  we          Sbox is calculated, uses only mem_util
 1.21     11.04.04  we          D7, {$apptype console} if needed
 1.22     27.17.04  we          Te0..Te4, Td0..Td4
**************************************************************************)

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  mem_util;

type
  bytearray = array[byte] of byte;
  longarray = array[byte] of longint;


{encr}
var
  SBox: bytearray;
  Te0,Te1,Te2,Te3,Te4: longarray;

{decr}
var
  InvSBox: bytearray;
  GLog, GPow: bytearray;
  Td0,Td1,Td2,Td3,Td4: longarray;


{---------------------------------------------------------------------------}
procedure CalcBaseTables;
  {-Calculate dynamic tables: power, log}
var
  i, p: byte;
begin
  {Power/Log tables}
  p := 1;
  for i:=0 to 254 do begin
    GPow[i] := p;
    GLog[p] := i;
    if p and $80 = 0 then p := (p shl 1) xor p
    else p := (p shl 1) xor p xor $1B;
  end;
  GPow[255] := 1;
end;


{---------------------------------------------------------------------------}
function GMul(x,y: byte): byte;
  {-calculate x*y in GF(2^8)}
var
  i: word;
begin
  if (x=0) or (y=0) then GMul := 0
  else begin
    i := word(GLog[x])+word(GLog[y]);
    if i>=255 then dec(i,255);
    GMul := GPow[i];
  end;
end;


{---------------------------------------------------------------------------}
function GM32(x,y: byte): longint;
  {-calculate x*y in GF(2^8) result as longint}
begin
  GM32 := GMul(x,y);
end;


{---------------------------------------------------------------------------}
procedure RotWord(var w: longint);
  {-rotate AES word}
type
  TBA4   = packed array[0..3] of byte;
var
  b: TBA4 absolute w;
  t: byte;
begin
  t := b[0];
  b[0] := b[1];
  b[1] := b[2];
  b[2] := b[3];
  b[3] := t;
end;


{---------------------------------------------------------------------------}
procedure CalcEncrTables;
  {-Calculate dynamic encr tables Te0..Te4, SBox}
var
  i, p: byte;
  t: longint;

  function rot(b,n: byte): byte;
  begin
    rot := (b shr n) xor (b shl (8-n));
  end;

begin
  for i:=0 to 255 do begin
  end;
  for i:=0 to 255 do begin
    {SBox calculation, cf. [1] 5.1.1}
    if i=0 then p:=0 else p:=GPow[255-GLog[i]];  {p*i = 1}
    p := p xor rot(p,4) xor rot(p,5) xor rot(p,6) xor rot(p,7) xor $63;
    Sbox[i] := p;
    Te4[i] := $01010101*p;
    {Tex tables}
    t := GM32(2,p) or (longint(p) shl 8) or (longint(p) shl 16) or (GM32(3,p) shl 24);
    Te0[i] := t;
    RotWord(t);
    Te3[i] := t;
    RotWord(t);
    Te2[i] := t;
    RotWord(t);
    Te1[i] := t;
  end;
end;


{---------------------------------------------------------------------------}
procedure CalcDecrTables;
  {-Calculate dynamic decr. tables: Td0..Td4, inverse SBox}
var
  i, p: byte;
  t: longint;
begin
  {InvSBox}
  for i:=0 to 255 do InvSBox[SBox[i]] := i;
  {Tdx tables}
  for i:=0 to 255 do begin
    p := InvSBox[i];
    Td4[i] := $01010101*p;
    t := GM32(14,p) or (GM32(9,p) shl 8) or (GM32(13,p) shl 16) or (GM32(11,p) shl 24);
    Td0[i] := t;
    RotWord(t);
    Td3[i] := t;
    RotWord(t);
    Td2[i] := t;
    RotWord(t);
    Td1[i] := t;
  end;
end;


{---------------------------------------------------------------------------}
procedure DumpByteTab(VName: string; var BA: bytearray);
  {-dump an array of bytes}
var
  i: integer;
begin
  writeln;
  writeln(VName, ': array[byte] of byte = (');
  for i:= 0 to 255 do begin
    write(' $',HexByte(BA[i]));
    if i=255 then writeln(');')
    else if i and 15 = 15 then writeln(',')
    else write(',');
  end;
end;


{---------------------------------------------------------------------------}
procedure DumpLongTab(VName: string; var LA: longarray);
  {-dump an array of longint}
var
  i: integer;
begin
  writeln;
  writeln(VName, ': array[byte] of longint = (');
  for i:= 0 to 255 do begin
    write(' $',HexLong(LA[i]));
    if i=255 then writeln(');')
    else if i and 7 = 7 then writeln(',')
    else write(',');
  end;
end;


begin
  CalcBaseTables;
  CalcEncrTables;
  CalcDecrTables;
  DumpByteTab('GLog', GLog);
  DumpByteTab('GPow', GPow);
  DumpByteTab('SBox', SBox);
  DumpByteTab('InvSBox', InvSBox);
  DumpLongTab('Te0', Te0);
  DumpLongTab('Te1', Te1);
  DumpLongTab('Te2', Te2);
  DumpLongTab('Te3', Te3);
  DumpLongTab('Te4', Te4);
  DumpLongTab('Td0', Td0);
  DumpLongTab('Td1', Td1);
  DumpLongTab('Td2', Td2);
  DumpLongTab('Td3', Td3);
  DumpLongTab('Td4', Td4);
end.
