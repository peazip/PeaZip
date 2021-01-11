unit sort;

{General QuickSort, HeapSort, CombSort routines using swap/compare functions.}

{In the pointer versions P is a typeless data pointer that is carried through}
{the functions and can be used to access local data etc by the caller.}


interface


{$i std.inc}

{$define HSort1}  {Compile single bound HeapSort routines: sort range 1..n}

(*************************************************************************

 DESCRIPTION     :  General QuickSort, HeapSort, CombSort routines

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REMARK          :  bounds must be less than 2^15 for 16 bit integers

 REFERENCES      :  [1] G.H. Gonnet, R. Baeza-Yates, Handbook of Algorithms
                        and Data Structures, 2nd ed, 1991
                        http://www.dcc.uchile.cl/~rbaeza/handbook/
                    [2] TopSpeed Modula2 library, HSort in lib.mod, 1987,
                        Jensen & Partners International
                    [3] R.Box, S.Lacey, "A fast, easy sort", Byte, 1991 16(4), p.315
                        see also http://en.wikipedia.org/wiki/Comb_sort
                    [4] P.Thiemann, "Informatik II. Algorithmen und Datenstrukturen", SS 2002,
                        http://www.informatik.uni-freiburg.de/proglang/teaching/ss2002/info2/
                        Bottom-Up Heapsort in Kap04.pdf

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     09.04.91  W.Ehrhardt  Initial TP6 Quicksort version
 1.01     01.05.98  we          Quicksort: swap only if i<>j
                                Heapsort (BP7)
 1.10     22.10.00  we          Quicksort bugfix: keep pointer to pivot item
                                HSort2: sort from L..R
 1.11     22.10.06  we          Englisch comments
 1.12     22.10.06  we          Pointer versions
 1.13     23.10.06  we          CombSort
 1.14     24.10.06  we          QuickSort with InsertionSort and median of 3
 1.15     24.10.06  we          Raise upper bound limit for 16Bit to 2^15-x
 1.16     29.10.06  we          Bottom-Up Heapsort
 1.17     05.11.06  we          CombSort: exit if R-L<1 
 1.18     21.06.08  we          TP5-7 (without break)
 **************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 1991-2008 Wolfgang Ehrhardt

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

const
  QT_IS : integer =  8; {Quicksort threshold for insertion sort}
  QT_M3 : integer = 32; {Quicksort threshold for median of 3   }


type
  less_func = function(i,j: integer): boolean;
                {-compare function, returns true if item(i) < item(j)}

  swap_proc = procedure(i,j: integer);
                {-swap procedure, swaps item(i) and item(j)}

type
  less_funcP = function(i,j: integer; P: pointer): boolean;
                {-compare function (pointer version), returns true if item(i) < item(j)}

  swap_procP = procedure(i,j: integer; P: pointer);
                {-swap procedure (pointer version), swaps item(i) and item(j)}


procedure CombSort(L,R: integer; less: less_func; swap: swap_proc);
  {-General CombSort routine, sorts items L..R}

procedure CombSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General CombSort routine (pointer version), sorts items L..R}

procedure QuickSort(L,R: integer; less: less_func; swap: swap_proc);
  {-General QuickSort routine, sorts items L..R}

procedure QuickSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General QuickSort routine (pointer version), sorts items L..R}

procedure BUHeapSort(L, R: integer; less: less_func; swap: swap_proc);
  {-General Bottom-Up HeapSort routine, sorts items L..R}

procedure BUHeapSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General Bottom-Up HeapSort routine (pointer version), sorts items L..R}

procedure HeapSort(L,R: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items L..R}

procedure HeapSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items L..R}

{$ifdef HSort1}
procedure HSort(n: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items 1..n}

procedure HSortP(n: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items 1..n}

procedure BUHSort(n: integer; less: less_func; swap: swap_proc);
  {-General Bottom-Up HeapSort routine, sorts items 1..n}

procedure BUHSortP(n: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General Bottom-Up HeapSort routine (pointer version), sorts items 1..n}
{$endif}



implementation


{$ifdef V7PLUS}

{--------------------------------------------------------------------------}
procedure HeapSort(L, R: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items L..R}
var
  i,j,k,n,u: integer;
begin

  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then break;
      if (k < n) and less(L+k,L+k+1) then inc(k);
      if less(L+j,L+k) then swap(L+j,L+k) else break;
      j := k;
    until k>u;
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(L+j,L+i);
    dec(i);
    repeat
      k := j * 2;
      if k > i then break;
      if (k < i) and less(L+k,L+k+1) then inc(k);
      swap(L+j,L+k);
      j := k;
    until k>u;
    repeat
      k := j div 2;
      if (k > 0) and less(L+k,L+j) then begin
        swap(L+j,L+k);
        j := k;
      end
      else break;
    until false;
  until i = 1;     {Original i=0; skipped because only action is swap(L,L)}
end;


{--------------------------------------------------------------------------}
procedure HeapSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items L..R}
var
  i,j,k,n,u: integer;
begin

  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then break;
      if (k < n) and less(L+k,L+k+1,P) then inc(k);
      if less(L+j,L+k,P) then swap(L+j,L+k,P) else break;
      j := k;
    until k>u;
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(L+j,L+i,P);
    dec(i);
    repeat
      k := j * 2;
      if k > i then break;
      if (k < i) and less(L+k,L+k+1,P) then inc(k);
      swap(L+j,L+k,P);
      j := k;
    until k>u;
    repeat
      k := j div 2;
      if (k > 0) and less(L+k,L+j,P) then begin
        swap(L+j,L+k,P);
        j := k;
      end
      else break;
    until false;
  until i = 1;     {Original i=0; skipped because only action is swap(L,L)}
end;

{$else}

(***** Code for TP5-7 without break *****)

{--------------------------------------------------------------------------}
procedure HeapSort(L, R: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items L..R}
var
  i,j,k,n,u: integer;
label
  brk1, brk2, brk3;
begin

  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then goto brk1;
      if (k < n) and less(L+k,L+k+1) then inc(k);
      if less(L+j,L+k) then swap(L+j,L+k) else goto brk1;
      j := k;
    until k>u;
brk1:
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(L+j,L+i);
    dec(i);
    repeat
      k := j * 2;
      if k > i then goto brk2;
      if (k < i) and less(L+k,L+k+1) then inc(k);
      swap(L+j,L+k);
      j := k;
    until k>u;
brk2:
    repeat
      k := j div 2;
      if (k > 0) and less(L+k,L+j) then begin
        swap(L+j,L+k);
        j := k;
      end
      else goto brk3;
    until false;
brk3:
  until i = 1;     {Original i=0; skipped because only action is swap(L,L)}
end;


{--------------------------------------------------------------------------}
procedure HeapSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items L..R}
var
  i,j,k,n,u: integer;
label
  brk1, brk2, brk3;
begin

  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then goto brk1;
      if (k < n) and less(L+k,L+k+1,P) then inc(k);
      if less(L+j,L+k,P) then swap(L+j,L+k,P) else goto brk1;
      j := k;
    until k>u;
brk1:
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(L+j,L+i,P);
    dec(i);
    repeat
      k := j * 2;
      if k > i then goto brk2;
      if (k < i) and less(L+k,L+k+1,P) then inc(k);
      swap(L+j,L+k,P);
      j := k;
    until k>u;
brk2:
    repeat
      k := j div 2;
      if (k > 0) and less(L+k,L+j,P) then begin
        swap(L+j,L+k,P);
        j := k;
      end
      else goto brk3;
    until false;
brk3:
  until i = 1;     {Original i=0; skipped because only action is swap(L,L)}
end;
{$endif}



{---------------------------------------------------------------------------}
procedure BUHeapSort(L, R: integer; less: less_func; swap: swap_proc);
  {-General Bottom-Up HeapSort routine, sorts items L..R}

  procedure pushdown(root,n: integer);
  var
    r,r2: integer;
  begin
     r := root;
     {write condition '2*r < n' as 'r < n-r' to avoid overflow for 16Bit}
     while r<n-r do begin
       r2 := r+r;
       if less(L+r2, L+r2+1) then r := r2+1 else r := r2;
     end;
     {write condition '2*r = n' as 'r = n-r' to avoid overflow for 16Bit}
     if r=n-r then r:=n;
     while (r<>root) and less(L+r,root) do r := r div 2;
     while r<>root do begin
       swap(L+root,L+r);
       r := r div 2;
     end;
  end;

var
  i,n: integer;
begin
  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;
  {Build heap}
  for i:=n div 2 downto 1 do pushdown(i,n);
  for i:=n downto 2 do begin
    swap(L+1,L+i);
    pushdown(1,i-1);
  end;
end;


{---------------------------------------------------------------------------}
procedure BUHeapSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General Bottom-Up HeapSort routine (pointer version), sorts items L..R}

  procedure pushdown(root,n: integer);
  var
    r,r2: integer;
  begin
     r := root;
     {write condition '2*r < n' as 'r < n-r' to avoid overflow for 16Bit}
     while r<n-r do begin
       r2 := r+r;
       if less(L+r2, L+r2+1, P) then r := r2+1 else r := r2;
     end;
     {write condition '2*r = n' as 'r = n-r' to avoid overflow for 16Bit}
     if r=n-r then r:=n;
     while (r<>root) and less(L+r,root,P) do r := r div 2;
     while r<>root do begin
       swap(L+root,L+r,P);
       r := r div 2;
     end;
  end;

var
  i,n: integer;
begin
  {Method: Sort with offset for less and swap arguments}
  dec(L);
  n := R-L;
  {Build heap}
  for i:=n div 2 downto 1 do pushdown(i,n);
  for i:=n downto 2 do begin
    swap(L+1,L+i,P);
    pushdown(1,i-1);
  end;
end;



{--------------------------------------------------------------------------}
procedure QuickSort(L,R: integer; less: less_func; swap: swap_proc);
  {-General QuickSort routine, sorts items L..R}

  procedure sort(l,r: integer);
    {internal recursive sorting}
  var
    i,j,m:integer;
  begin
    if r-l<QT_IS then begin
      {Use insertion sort}
      for i:=succ(l) to r do begin
        j:=i;
        while (j>l) and less(j, j-1) do begin
          swap(j,j-1);
          dec(j);
        end;
      end;
      exit;
    end;
    i := l;
    j := r;
    {use middle as pivot element}
    m := (longint(l)+r) div 2;
    if r-l>QT_M3 then begin
      {use median of 3 as pivot}
      if less(m,l) then swap(l,m);
      if less(r,l) then swap(l,r);
      if less(r,m) then swap(m,r);
    end;
    repeat
      while less(i,m) do inc(i);
      while less(m,j) do dec(j);
      if i <= j then begin
        if i<>j then begin
          {swap only if necessary}
          swap(i,j);
          {m must keep on pointing to original pivot!!}
          if i=m then m:=j
          else if j=m then m:=i;
        end;
        inc(i);
        dec(j);
      end;
    until i>j;
    if l<j then sort(l, j);
    if i<r then sort(i, r);
  end;

begin
  sort(L,R);
end;




{--------------------------------------------------------------------------}
procedure QuickSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General QuickSort routine (pointer version), sorts items L..R}

  procedure sort(l,r: integer);
    {internal recursive sorting}
  var
    i,j,m:integer;
  begin
    if r-l<QT_IS then begin
      {Use insertion sort}
      for i:=succ(l) to r do begin
        j:=i;
        while (j>l) and less(j, j-1, P) do begin
          swap(j,j-1,P);
          dec(j);
        end;
      end;
      exit;
    end;
    i := l;
    j := r;
    {use middle as pivot element}
    m := (longint(l)+r) div 2;
    if r-l>QT_M3 then begin
      {use median of 3 as pivot}
      if less(m,l,P) then swap(l,m,P);
      if less(r,l,P) then swap(l,r,P);
      if less(r,m,P) then swap(m,r,P);
    end;
    repeat
      while less(i,m,P) do inc(i);
      while less(m,j,P) do dec(j);
      if i <= j then begin
        if i<>j then begin
          {swap only if necessary}
          swap(i,j,P);
          {m must keep on pointing to original pivot!!}
          if i=m then m:=j
          else if j=m then m:=i;
        end;
        inc(i);
        dec(j);
      end;
    until i>j;
    if l<j then sort(l, j);
    if i<r then sort(i, r);
  end;

begin
  sort(L,R);
end;



{---------------------------------------------------------------------------}
procedure CombSort(L,R: integer; less: less_func; swap: swap_proc);
  {-General CombSort routine, sorts items L..R}
var
  i,j,gap: integer;
  swapped: boolean;
begin
  gap := R-L;
  if gap<1 then exit;
  repeat
    gap := longint(gap)*10 div 13;
    if (gap=9) or (gap=10) then gap := 11
    else if gap<1 then gap:=1;
    swapped := false;
    for i:=L to R-gap do begin
      j := i + gap;
      if less(j,i) then begin
        swap(i,j);
        swapped := true;
      end
    end
  until (gap=1) and not swapped;
end;



{---------------------------------------------------------------------------}
procedure CombSortP(L,R: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General CombSort routine (pointer version), sorts items L..R}
var
  i,j,gap: integer;
  swapped: boolean;
begin
  gap := R-L;
  if gap<1 then exit;
  repeat
    gap := longint(gap)*10 div 13;
    if (gap=9) or (gap=10) then gap := 11
    else if gap<1 then gap:=1;
    swapped := false;
    for i:=L to R-gap do begin
      j := i + gap;
      if less(j,i,P) then begin
        swap(i,j,P);
        swapped := true;
      end
    end
  until (gap=1) and not swapped;
end;



{$ifdef HSort1}

{---------------------------------------------------------------------------}
{------------------ Single (upper) bound heap sort -------------------------}
{---------------------------------------------------------------------------}

{$ifdef V7Plus}

{--------------------------------------------------------------------------}
procedure HSort(n: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items 1..n}
var
  i,j,k,u: integer;
begin

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then break;
      if (k < n) and less(k,k+1) then inc(k);
      if less(j,k) then swap(j,k) else break;
      j := k;
    until k>u;
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(j,i);
    dec(i);
    repeat
      k := j * 2;
      if k > i then break;
      if (k < i) and less(k,k+1) then inc(k);
      swap(j,k);
      j := k;
    until k>u;
    repeat
      k := j div 2;
      if (k > 0) and less(k,j) then begin
        swap(j,k);
        j := k;
      end
      else break;
    until false;
  until i = 1;     {Original i=0; skipped because only action is swap(1,1)}
end;


{--------------------------------------------------------------------------}
procedure HSortP(n: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items 1..n}
var
  i,j,k,u: integer;
begin

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then break;
      if (k < n) and less(k,k+1,P) then inc(k);
      if less(j,k,P) then swap(j,k,P) else break;
      j := k;
    until k>u;
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(j,i,P);
    dec(i);
    repeat
      k := j * 2;
      if k > i then break;
      if (k < i) and less(k,k+1,P) then inc(k);
      swap(j,k,P);
      j := k;
    until k>u;
    repeat
      k := j div 2;
      if (k > 0) and less(k,j,P) then begin
        swap(j,k,P);
        j := k;
      end
      else break;
    until false;
  until i = 1;     {Original i=0; skipped because only action is swap(1,1)}
end;

{$else}

(***** Code for TP5-7 without break *****)

{---------------------------------------------------------------------------}
procedure HSort(n: integer; less: less_func; swap: swap_proc);
  {-General HeapSort routine, sorts items 1..n}
var
  i,j,k,u: integer;
label
  brk1, brk2, brk3;
begin

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then goto brk1;
      if (k < n) and less(k,k+1) then inc(k);
      if less(j,k) then swap(j,k) else goto brk1;
      j := k;
    until k>u;
brk1:
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(j,i);
    dec(i);
    repeat
      k := j * 2;
      if k > i then goto brk2;
      if (k < i) and less(k,k+1) then inc(k);
      swap(j,k);
      j := k;
    until k>u;
brk2:
    repeat
      k := j div 2;
      if (k > 0) and less(k,j) then begin
        swap(j,k);
        j := k;
      end
      else goto brk3;
    until false;
brk3:
  until i = 1;     {Original i=0; skipped because only action is swap(1,1)}
end;


{--------------------------------------------------------------------------}
procedure HSortP(n: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General HeapSort routine (pointer version), sorts items 1..n}
var
  i,j,k,u: integer;
label
  brk1, brk2, brk3;
begin

  if n<2 then exit;

  i := n div 2;
  u := (n+1) div 2;  {upper bound for k, avoid overflow of k:=2*j in repeat}
  repeat
    j := i;
    repeat {Note that total repeats <= n/4 * 1 + n/8 * 2 + n/16 * 3 + ....}
      k := j * 2;
      if k > n then goto brk1;
      if (k < n) and less(k,k+1,P) then inc(k);
      if less(j,k,P) then swap(j,k,P) else goto brk1;
      j := k;
    until k>u;
brk1:
    dec(i);
  until i = 0;
  i := n;
  repeat
    j := 1;
    swap(j,i,P);
    dec(i);
    repeat
      k := j * 2;
      if k > i then goto brk2;
      if (k < i) and less(k,k+1,P) then inc(k);
      swap(j,k,P);
      j := k;
    until k>u;
brk2:
    repeat
      k := j div 2;
      if (k > 0) and less(k,j,P) then begin
        swap(j,k,P);
        j := k;
      end
      else goto brk3;
    until false;
brk3:
  until i = 1;     {Original i=0; skipped because only action is swap(1,1)}
end;
{$endif}


{---------------------------------------------------------------------------}
procedure BUHSort(n: integer; less: less_func; swap: swap_proc);
  {-General Bottom-Up HeapSort routine, sorts items 1..n}

  procedure pushdown(root,n: integer);
  var
    r,r2: integer;
  begin
     r := root;
     {write condition '2*r < n' as 'r < n-r' to avoid overflow for 16Bit}
     while r<n-r do begin
       r2 := r+r;
       if less(r2, r2+1) then r := r2+1 else r := r2;
     end;
     {write condition '2*r = n' as 'r = n-r' to avoid overflow for 16Bit}
     if r=n-r then r:=n;
     while (r<>root) and less(r,root) do r := r div 2;
     while r<>root do begin
       swap(root,r);
       r := r div 2;
     end;
  end;

var
  i: integer;
begin
  {Build heap}
  for i:=n div 2 downto 1 do pushdown(i,n);
  for i:=n downto 2 do begin
    swap(1,i);
    pushdown(1,i-1);
  end;
end;


{---------------------------------------------------------------------------}
procedure BUHSortP(n: integer; less: less_funcP; swap: swap_procP; P: pointer);
  {-General Bottom-Up HeapSort routine (pointer version), sorts items 1..n}

  procedure pushdown(root,n: integer);
  var
    r,r2: integer;
  begin
     r := root;
     {write condition '2*r < n' as 'r < n-r' to avoid overflow for 16Bit}
     while r<n-r do begin
       r2 := r+r;
       if less(r2, r2+1, P) then r := r2+1 else r := r2;
     end;
     {write condition '2*r = n' as 'r = n-r' to avoid overflow for 16Bit}
     if r=n-r then r:=n;
     while (r<>root) and less(r,root,P) do r := r div 2;
     while r<>root do begin
       swap(root,r,P);
       r := r div 2;
     end;
  end;

var
  i: integer;
begin
  {Build heap}
  for i:=n div 2 downto 1 do pushdown(i,n);
  for i:=n downto 2 do begin
    swap(1,i,P);
    pushdown(1,i-1);
  end;
end;

{$endif}



end.


