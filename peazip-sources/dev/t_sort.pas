{-Test program for sort unit,  we Sep.2008}

program t_sort;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  sort;

{$ifdef BIT16}
{$F+}
{$endif}


{$ifdef WINCRT}
const
  IMAX = $3E00+1;
{$else}
const
  IMAX = $7E00+1;
{$endif}


var
  IA: array[1..IMAX] of integer;

var
  lc,sc: longint;


function less(i,j: integer): boolean;
begin
  less := IA[i] < IA[j];
  inc(lc);
end;


procedure swap(i,j: integer);
var
  t: integer;
begin
  t := IA[i];
  IA[i] := IA[j];
  IA[j] := t;
  inc(sc);
end;

function lessp(i,j: integer; p: pointer): boolean;
begin
  lessp := IA[i] < IA[j];
  inc(lc);
end;


procedure swapp(i,j: integer; p: pointer);
var
  t: integer;
begin
  t := IA[i];
  IA[i] := IA[j];
  IA[j] := t;
  inc(sc);
end;


procedure Setup(N: integer);
var
  i: integer;
  q: word;
begin
  if N>35 then q := $7FFF else q := 10;
  randseed := 1;
  lc := 0;
  sc := 0;
  for i:=1 to N do IA[i] := random(q);
end;

procedure Test(N: integer);
var
  i: integer;
begin
  for i:=2 to N do begin
    if IA[i-1]>IA[i] then begin
      writeln('Error for ',i);
      exit;
    end;
  end;
  writeln('OK, swap=',sc:8,',   less=',lc:8);
end;

procedure List(N: integer);
var
  i: integer;
begin
  if N>35 then exit;
  for i:=1 to N do write(IA[i]:2);
  writeln;
end;

var
  N: integer;
begin
  N :=  IMAX;
  {$ifdef FPC}
    writeln('Quicksort:');
    setup(N); List(N); QuickSort(1, N, @less, @swap); List(N); test(N);
    lc := 0; sc := 0; QuickSort(1, N, @less, @swap); test(N);
    writeln;
    writeln('CombSort:');
    setup(N); List(N);  CombSort(1, N, @less, @swap); List(N); test(N);
    lc := 0; sc := 0;  CombSort(1, N, @less, @swap); test(N);
    writeln;
    writeln('HeapSort:');
    setup(N); List(N);  Heapsort(1, N, @less, @swap); List(N); test(N);
    lc := 0; sc := 0;  Heapsort(1, N, @less, @swap); test(N);
    writeln;
    writeln('BUHeapSort:');
    setup(N); List(N);  BUHeapSort(1, N, @less, @swap); List(N); test(N);
    lc := 0; sc := 0;  BUHeapSort(1, N, @less, @swap); test(N);
    writeln;
    writeln('QuicksortP:');
    setup(N); List(N); QuickSortP(1, N, @lessP, @swapP, nil); List(N); test(N);
    writeln;
    writeln('CombSortP:');
    setup(N); List(N);  CombSortP(1, N, @lessP, @swapP, nil); List(N); test(N);
    writeln;
    writeln('HeapSortP:');
    setup(N); List(N);  HeapsortP(1, N, @lessP, @swapP, nil); List(N); test(N);
    writeln;
    writeln('BUHeapSortP:');
    setup(N); List(N);  BUHeapSortP(1, N, @lessP, @swapP, nil); List(N); test(N);
    writeln;
  {$else}
    writeln('Quicksort:');
    setup(N); List(N); QuickSort(1, N, less, swap); List(N); test(N);
    lc := 0; sc := 0; QuickSort(1, N, less, swap); test(N);
    writeln;
    writeln('CombSort:');
    setup(N); List(N);  CombSort(1, N, less, swap); List(N); test(N);
    lc := 0; sc := 0;  CombSort(1, N, less, swap); test(N);
    writeln;
    writeln('HeapSort:');
    setup(N); List(N);  HeapSort(1, N, less, swap); List(N); test(N);
    lc := 0; sc := 0;  HeapSort(1, N, less, swap); test(N);
    writeln;
    writeln('BUHeapSort:');
    setup(N); List(N);  BUHeapSort(1, N, less, swap); List(N); test(N);
    lc := 0; sc := 0;  BUHeapSort(1, N, less, swap); test(N);
    writeln;
    writeln('QuicksortP:');
    setup(N); List(N); QuickSortP(1, N, lessP, swapP, nil); List(N); test(N);
    writeln;
    writeln('CombSortP:');
    setup(N); List(N);  CombSortP(1, N, lessP, swapP, nil); List(N); test(N);
    writeln;
    writeln('HeapSortP:');
    setup(N); List(N);  HeapSortP(1, N, lessP, swapP, nil); List(N); test(N);
    writeln;
    writeln('BUHeapSortP:');
    setup(N); List(N);  BUHeapSortP(1, N, lessP, swapP, nil); List(N); test(N);
    writeln;
    writeln('HSort:');
    setup(N); List(N);  HSort(N, less, swap); List(N); test(N);
    writeln;
    writeln('HSortP:');
    setup(N); List(N);  HSortP(N, lessP, swapP, nil); List(N); test(N);
    writeln;
  {$endif}
end.
