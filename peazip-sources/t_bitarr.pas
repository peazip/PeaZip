program T_BITARR;

{-Test program for BitArray unit, W.Ehrhardt Oct. 2005 }
{ calculate 16 bit primes via sieving (only odd numbers are stored)}

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WINCRT, {$endif}
  BitArray;

var
  Sieve: TBitArray;


{---------------------------------------------------------------------------}
procedure DoSieve;
  {-Sieve primes, bit array version}
var
  i,j,d,p: word;
begin
  writeln('Sieving ... ');
  with Sieve do begin
    BA_SetAll(Sieve);
    BA_ClearBit(Sieve,0);
    for i:=1 to 127 do begin
      if BA_TestBit(Sieve,i) then begin
        d := 2*i+1;
        j := d+i;
        while j<=$7fff do begin
          BA_ClearBit(Sieve,j);
          inc(j,d);
        end;
      end;
    end;
  end;
  writeln('16 bit primes:');
  {output 2 (note in sieve)}
  write(2:7);
  p:=1;
  for i:=0 to $7FFF do begin
    if BA_TestBit(Sieve,i) then begin
      write(2*i+1:7);
      inc(p);
      if p mod 10 = 0 then writeln;
    end;
  end;
  writeln;
  writeln('Number of primes: ',p);
end;

var
  OK: boolean;
begin
  BA_Init(Sieve,$8000,OK);
  if OK then DoSieve;
end.

