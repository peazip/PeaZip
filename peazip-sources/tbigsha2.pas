{-Test prog for SHA256, we 03.01.02}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT, {$endif}
     mem_util,
     hash,
     sha256;


(*************************************************************************
From http://www.cryptosys.net (SHA-256 test vectors)

"Data = One million repetitions of 'a'
CDC76E5C 9914FB92 81A1C7E2 84D73E67 F1809A48 A497200E 046D39CC C7112CD0

Data = 2^29 repetitions of 'a'
B9045A71 3CAED5DF F3D3B783 E98D1CE5 778D8BC3 31EE4119 D7070723 12AF06A7

These agree with an implementation created by Aaron Gifford who has many more
additional vectors of SHA-256 (and SHA-384 and SHA-512) on his site as well as
open-source versions of his source code.

I think the test with 2^29 (0x20000000) byte repetitions is important because
it makes the bit count "overflow" into the high 32-bit word. That would seem to
me to be a key place where an implementation of any of the Secure Hash
Algorithms might fall over."
**************************************************************************)

(*
Timimg for 1.8GHz P4 Win98

        old       Unroll    New   V2.51
BP7  :  205.6 s    182.6  168.3   143.6
TP55 :~2600   s
D3   :   28.5 s     20.7   22.9    19.2
D6   :   28.0 s     20.2   22.5    19.3
FPC  :   48.1 s     39.4   40.1    33.0
VP   :   37.8 s     30.0   29.9    23.5
*)

const
  C1Mio: TSHA256Digest = ($cd,$c7,$6e,$5c,$99,$14,$fb,$92,
                          $81,$a1,$c7,$e2,$84,$d7,$3e,$67,
                          $f1,$80,$9a,$48,$a4,$97,$20,$0e,
                          $04,$6d,$39,$cc,$c7,$11,$2c,$d0);

  C2p29: TSHA256Digest = ($b9,$04,$5a,$71,$3c,$ae,$d5,$df,
                          $f3,$d3,$b7,$83,$e9,$8d,$1c,$e5,
                          $77,$8d,$8b,$c3,$31,$ee,$41,$19,
                          $d7,$07,$07,$23,$12,$af,$06,$a7);

const
  n : longint = 1024*1024;           {2^20}
  k = 64;
var
  buf: array[1..k*512] of byte;      {2^15 byte}
  Context: THashContext;
  Digest: TSHA256Digest;
  i,imax: longint;
begin
  fillchar(buf, sizeof(buf), 'a');

  SHA256Init(Context);
  for i:=1 to 1000 do begin
    SHA256Update(Context, @buf, 1000);
  end;
  SHA256Final(Context, Digest);
  writeln('One million repetitions of "a"');
  writeln('SHA256: ', HexStr(@digest, sizeof(digest)));
  writeln('Test passed: ', CompMem(@digest, @C1Mio, sizeof(digest)));
  writeln;

  SHA256Init(Context);
  imax := n div k;
  for i:=1 to imax do begin
    SHA256Update(Context, @buf, sizeof(buf));
  end;
  SHA256Final(Context, Digest);
  writeln('2^29 repetitions of "a"');
  writeln('SHA256: ', HexStr(@digest, sizeof(digest)));
  writeln('Test passed: ', CompMem(@digest, @C2p29, sizeof(digest)));
end.

