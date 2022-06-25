program t_mkctab;

(*************************************************************************

 DESCRIPTION     :  Calculate compressed AES tables

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] http://csrc.nist.gov/fips/fips-197.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     08.07.06  we          Initial version using existing static tables
 0.11     12.07.06  we          Use (Inv)SBox bytes instead of zero fill bytes
 0.12     13.07.06  we          Without static tables
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



{---------------------------------------------------------------------------}
{Encrypt}
{  3  2  1  0}
{ a5 63 63 c6}
{ xx xx xx c6 63 63 a5 xx
  xx xx a5 c6 63 63 xx xx
  xx 63 a5 c6 63 xx xx xx
  63 63 a5 c6 xx xx xx xx}

{If (b0,b1,b2,b3) are the bytes of an Te0 longint the}
{TCe entry has the 8 bytes (b1,b2,b3,b0,b1,b2,b3,SBox)  }

{---------------------------------------------------------------------------}
{Decrypt}
{  3  2  1  0}
{ 50 a7 f4 51}

{ xx xx xx 51 f4 a7 50 xx
  xx xx 50 51 f4 a7 xx xx
  xx a7 50 51 f4 xx xx xx
  f4 a7 50 51 xx xx xx xx}

{If (b0,b1,b2,b3) are the bytes of an Td0 longint the}
{TCd entry has the 8 bytes (b1,b2,b3,b0,b1,b2,b3,InvSBox)  }


{---------------------------------------------------------------------------}
{types to access table: Tex[i] = TCe[i].Ex.L, Tdx[i] = TCd[i].Ex.L}
(*
type
  TH0 = packed record             TH1 = packed record
          b0,b1,b2: byte;                 b0,b1: byte;
          L: longint;                     L: longint;
          box: byte;                      b2,box: byte;
        end;                            end;

  TH2 = packed record             TH3 = packed record
          b0: byte;                       L: longint;
          L: longint;                     b0,b1,b2,box: byte;
          b1,b2,box: byte;              end;
        end;

  THU = record                    TDU = record
          case integer of                 case integer of
            0: (E0: TH0);                   0: (D0: TH0);
            1: (E1: TH1);                   1: (D1: TH1);
            2: (E2: TH2);                   2: (D2: TH2);
            3: (E3: TH3);                   3: (D3: TH3);
        end;                            end;
*)


var
  GLog, GPow: array[byte] of byte;


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
function rot(b,n: byte): byte;
  {-rotate byte right n bits}
begin
  rot := (b shr n) xor (b shl (8-n));
end;


{---------------------------------------------------------------------------}
procedure MakeCompressedTables;
  {-Calculate and dump compressed AES tables}
var
  i,j: integer;
  p: byte;
  b: array[0..3] of byte;
  s: array[0..4] of string[4];
  InvSBox: array[byte] of byte;
begin
  CalcBaseTables;
  writeln('const');
  writeln('  TCe: packed array[0..2047] of byte = (');
  for i:=0 to 255 do begin
    {SBox calculation, cf. [1] 5.1.1}
    if i=0 then p:=0 else p:=GPow[255-GLog[i]];  {p*i = 1}
    p := p xor rot(p,4) xor rot(p,5) xor rot(p,6) xor rot(p,7) xor $63;
    InvSBox[p] := i;
    b[0] := GMul(2,p);
    b[1] := p;
    b[2] := p;
    b[3] := GMul(3,p);
    for j:=0 to 3 do s[j] := '$'+HexByte(b[j])+',';
    s[4] := '$'+HexByte(p);
    if odd(i) then begin
      write(s[1],s[2],s[3],s[0],s[1],s[2],s[3],s[4]);
      if i=255 then writeln(');') else writeln(',');
    end
    else write('':9,s[1],s[2],s[3],s[0],s[1],s[2],s[3],s[4],',');
  end;
  writeln;
  writeln('const');
  writeln('  TCd: packed array[0..2047] of byte = (');
  for i:=0 to 255 do begin
    p    := InvSbox[i];
    b[0] := GMul(14,p);
    b[1] := GMul( 9,p);
    b[2] := GMul(13,p);
    b[3] := GMul(11,p);
    for j:=0 to 3 do s[j] := '$'+HexByte(b[j])+',';
    s[4] := '$'+HexByte(p);
    if odd(i) then begin
      write(s[1],s[2],s[3],s[0],s[1],s[2],s[3],s[4]);
      if i=255 then writeln(');') else writeln(',');
    end
    else write('':9,s[1],s[2],s[3],s[0],s[1],s[2],s[3],s[4],',');
  end;
end;

begin
  MakeCompressedTables;
end.
