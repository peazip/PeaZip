program T_CTab64;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef HAS_INT64}
  error('No int64');
{$endif}


(*************************************************************************

 DESCRIPTION     :  Calculate CRC64 table for

                    x^64 + x^62 + x^57 + x^55 + x^54 + x^53 + x^52 + x^47 +
                    x^46 + x^45 + x^40 + x^39 + x^38 + x^37 + x^35 + x^33 +
                    x^32 + x^31 + x^29 + x^27 + x^24 + x^23 + x^22 + x^21 +
                    x^19 + x^17 + x^13 + x^12 + x^10 + x^9  + x^7  + x^4  +
                    x^1  + 1

 REQUIREMENTS    :  D4-D6, FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ----


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     31.08.03  we          Init. version for D6
 1.01     13.09.03  we          For D4,D5,FPC
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2004 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

{$ifdef UNIT_SCOPE}
uses
  System.SysUtils;
{$else}
uses
  SysUtils;
{$endif}


{---------------------------------------------------------------------------}
procedure getPoly(var poly: int64);
var
  x: int64;
begin
  x := 1;
  writeln('x^64 + x^62 + x^57 + x^55 + x^54 + x^53 + x^52 + x^47 + x^46 + x^45 +');
  writeln('x^40 + x^39 + x^38 + x^37 + x^35 + x^33 + x^32 + x^31 + x^29 + x^27 +');
  writeln('x^24 + x^23 + x^22 + x^21 + x^19 + x^17 + x^13 + x^12 + x^10 + x^9  +');
  writeln('x^7  + x^4  + x^1  + 1');
  poly := {x shl 64}+ x shl 62 + x shl 57 + x shl 55 + x shl 54 + x shl 53 + x shl 52 + x shl 47 + x shl 46 + x shl 45 +
           x shl 40 + x shl 39 + x shl 38 + x shl 37 + x shl 35 + x shl 33 + x shl 32 + x shl 31 + x shl 29 + x shl 27 +
           x shl 24 + x shl 23 + x shl 22 + x shl 21 + x shl 19 + x shl 17 + x shl 13 + x shl 12 + x shl 10 + x shl  9 +
           x shl  7 + x shl 4 +  x shl  1 + 1;

  writeln;
  writeln;
  writeln;
  writeln('const');
  writeln('  PolyLo : longint = longint($', IntToHex(Poly and $FFFFFFFF, 8), ');');
  writeln('  PolyHi : longint = longint($', IntToHex(Poly shr 32, 8), ');');
  writeln;
end;


{---------------------------------------------------------------------------}
procedure CalcTable(const poly: int64);
var
  NTab: array[0..255] of int64;
  i,b: integer;
  c64: int64;
begin
  for i:=0 to 255 do begin
    c64 := int64(i) shl 56;
    for b:=1 to 8 do begin
     if c64<0 then c64 := (c64 shl 1) xor Poly else c64 := c64 shl 1;
    end;
    NTab[i] := c64;
  end;
  writeln;
  writeln('const');
  writeln('  Tab64Lo : array[0..255] of longint = (');
  write('':4);
  for i:=0 to 255 do begin
    write('$',IntToHex(NTab[i] and $FFFFFFFF, 8));
    if i=255 then writeln(');')
    else begin
      write(',');
      if i and 7 = 7 then begin
        writeln;
        write('':4);
      end;
    end;
  end;
  writeln;
  writeln('const');
  writeln('  Tab64Hi : array[0..255] of longint = (');
  write('':4);
  for i:=0 to 255 do begin
    write('$',IntToHex((NTab[i] shr 32) and $FFFFFFFF, 8));
    if i=255 then writeln(');')
    else begin
      write(',');
      if i and 7 = 7 then begin
        writeln;
        write('':4);
      end;
    end;
  end;
  writeln;
end;

var
  Poly: int64;
begin
  writeln('T_CTab64 - CRC64 table calculation     (c) 2002-2004 W.Ehrhardt');
  writeln;
  writeln('Calculate CRC64 tables for polynomial:');
  writeln;
  GetPoly(Poly);
  CalcTable(Poly);
  {$ifdef D4Plus}
    if DebugHook<>0 then readln;
  {$endif}
end.
