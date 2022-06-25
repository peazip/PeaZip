{-Tab generation prog for FCRC32, we 27.06.07}

program t_by4;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses {$ifdef WINCRT} WinCRT, {$endif}
     mem_util;


const
  CRCPoly  = longint($EDB88320);

type
  TCRC32Tab  = array[byte] of longint;

var
  tab0,tab1,tab2,tab3: TCRC32Tab;


{---------------------------------------------------------------------------}
procedure maketabs;
  {-Create the slicing-by-4 tables}
var
  i,n: integer;
  c: longint;
begin
  for n:=0 to 255 do begin
    c := n;
    for i:=0 to 7 do begin
      if odd(c) then c := CRCPoly xor (c shr 1)
      else c := c shr 1;
    end;
    tab0[n] := c;
  end;
  for n:=0 to 255 do begin
    c := tab0[n];
    c := tab0[c and $ff] xor (c shr 8);  tab1[n] := c;
    c := tab0[c and $ff] xor (c shr 8);  tab2[n] := c;
    c := tab0[c and $ff] xor (c shr 8);  tab3[n] := c;
  end;
end;


{---------------------------------------------------------------------------}
procedure dumptab(Nr: integer; var Tab: TCRC32Tab);
  {-dump tables for as const arrays}
var
  i:integer;
begin
  writeln;
  writeln('  CTab',Nr,': array[0..255] of longint = (');
  write('':4);
  for i:=0 to 255 do begin
    write('$',HexLong(Tab[i]));
    if i and 7 = 7 then begin
      if i<>255 then begin
        writeln(',');
        write('':4);
      end
      else writeln(');');
    end
    else write(',');
  end;
end;


begin
  MakeTabs;
  DumpTab(0,Tab0);
  DumpTab(1,Tab1);
  DumpTab(2,Tab2);
  DumpTab(3,Tab3);
end.
