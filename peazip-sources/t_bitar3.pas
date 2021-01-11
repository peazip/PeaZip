program T_BITAR3;

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
  i,j,d: word;
  p: longint;
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
  write('Product of 16 bit primes mod 2^16 = ');
  p := 2;
  j := 1;
  for i:=0 to $7FFF do begin
    if BA_TestBit(Sieve,i) then begin
      inc(j);
      p := p*longint(2*i+1) and $FFFF;
    end;
  end;
  writeln(p);
  writeln('Number of primes: ',j);
  {t_calc: [D]:=> (2^16)# mod 2^16, Result = 49618}
  writeln('Test passed: ',p=49618);
end;

var
  OK: boolean;
begin
  BA_Init(Sieve,$8000,OK);
  if OK then DoSieve;
end.

