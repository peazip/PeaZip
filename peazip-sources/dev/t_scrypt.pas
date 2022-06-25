{Test program for scrypt key derivation unit, WE Aug.2014}

program t_scrypt;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  scrypt,
  BTypes, mem_util;


type
  TBA64 = array[0..63] of byte;

{---------------------------------------------------------------------------}
procedure test_scrypt_kdf;
  {-Tests from http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01}
  { Some test case are taken from scrypt_test.go, available under a BSD style liccense}
  { from http://code.google.com/p/go/source/browse/scrypt/scrypt_test.go?repo=crypto  }

const {Draft & GO}
  {empty pw, salt}
  n1=16; r1=1; p1=1;
  dk1: TBA64 = ($77,$d6,$57,$62,$38,$65,$7b,$20,$3b,$19,$ca,$42,$c1,$8a,$04,$97,
                $f1,$6b,$48,$44,$e3,$07,$4a,$e8,$df,$df,$fa,$3f,$ed,$e2,$14,$42,
                $fc,$d0,$06,$9d,$ed,$09,$48,$f8,$32,$6a,$75,$3a,$0f,$c8,$1f,$17,
                $e8,$d3,$e0,$fb,$2e,$0d,$36,$28,$cf,$35,$e2,$0c,$38,$d1,$89,$06);

const {GO test}
  pw2  : string[10] = 'password';
  salt2: string[10] = 'salt';
const
  n2=2; r2=10; p2=10;
  dk2: array[0..31] of byte = ($48,$2c,$85,$8e,$22,$90,$55,$e6,
                               $2f,$41,$e0,$ec,$81,$9a,$5e,$e1,
                               $8b,$db,$87,$25,$1a,$53,$4f,$75,
                               $ac,$d9,$5a,$c5,$e5,$0a,$a1,$5f);
const {GO test}
  pw3  : array[0..0] of char8 = 'p';
  salt3: array[0..0] of char8 = 's';
const
  n3=2; r3=1; p3=1;
  dk3: array[0..15] of byte = ($48,$b0,$d2,$a8,$a3,$27,$26,$11,
                               $98,$4c,$50,$eb,$d6,$30,$af,$52);

{$ifndef BIT16}
const {Draft & GO}
  pw4  : array[0..7] of char8 = 'password';
  salt4: array[0..3] of char8 = 'NaCl';
const
  n4=1024; r4=8; p4=16;
  dk4: TBA64 = ($fd,$ba,$be,$1c,$9d,$34,$72,$00,$78,$56,$e7,$19,$0d,$01,$e9,$fe,
                $7c,$6a,$d7,$cb,$c8,$23,$78,$30,$e7,$73,$76,$63,$4b,$37,$31,$62,
                $2e,$af,$30,$d9,$2e,$22,$a3,$88,$6f,$f1,$09,$27,$9d,$98,$30,$da,
                $c7,$27,$af,$b9,$4a,$83,$ee,$6d,$83,$60,$cb,$df,$a2,$cc,$06,$40);

const {Draft & GO}
  pw5  : array[0..12] of char8 = 'pleaseletmein';
  salt5: array[0..13] of char8 = 'SodiumChloride';
const
  n5=16384; r5=8; p5=1;
  dk5: TBA64 = ($70,$23,$bd,$cb,$3a,$fd,$73,$48,$46,$1c,$06,$cd,$81,$fd,$38,$eb,
                $fd,$a8,$fb,$ba,$90,$4f,$8e,$3e,$a9,$b5,$43,$f6,$54,$5d,$a1,$f2,
                $d5,$43,$29,$55,$61,$3f,$0f,$cf,$62,$d4,$97,$05,$24,$2a,$9a,$f9,
                $e6,$1e,$85,$dc,$0d,$65,$1e,$40,$df,$cf,$01,$7b,$45,$57,$58,$87);

const  {GO test}
  pw6  : array[0..7] of char8 = 'password';
  salt6: array[0..3] of char8 = 'salt';
const
  n6=16; r6=100; p6=100;
  dk6: array[0..31] of byte = ($88,$bd,$5e,$db,$52,$d1,$dd,$00,
                               $18,$87,$72,$ad,$36,$17,$12,$90,
                               $22,$4e,$74,$82,$95,$25,$b1,$8d,
                               $73,$23,$a5,$7f,$91,$96,$3c,$37);
const  {GO test}
  pw7  : string[30] = 'this is a long '#0' password';
  salt7: array[0..24] of char8 = 'and this is a long '#0' salt';
const
  n7=16384; r7=8; p7=1;
  dk7: array[0..76] of byte = ($c3,$f1,$82,$ee,$2d,$ec,$84,$6e,$70,$a6,
                               $94,$2f,$b5,$29,$98,$5a,$3a,$09,$76,$5e,
                               $f0,$4c,$61,$29,$23,$b1,$7f,$18,$55,$5a,
                               $37,$07,$6d,$eb,$2b,$98,$30,$d6,$9d,$e5,
                               $49,$26,$51,$e4,$50,$6a,$e5,$77,$6d,$96,
                               $d4,$0f,$67,$aa,$ee,$37,$e1,$77,$7b,$8a,
                               $d5,$c3,$11,$14,$32,$bb,$3b,$6f,$7e,$12,
                               $64,$40,$18,$79,$e6,$41,$ae);

{$endif}

{$ifdef BIT64}
{Needs more than 1GB and about 15/20 s with D18/FPC of Win7/64 Core i3-2350M}
const {Draft & GO}
  pw8  : array[0..12] of char8 = 'pleaseletmein';
  salt8: array[0..13] of char8 = 'SodiumChloride';
const
  n8=1048576; r8=8; p8=1;
  dk8: TBA64 = ($21,$01,$cb,$9b,$6a,$51,$1a,$ae,$ad,$db,$be,$09,$cf,$70,$f8,$81,
                $ec,$56,$8d,$57,$4a,$2f,$fd,$4d,$ab,$e5,$ee,$98,$20,$ad,$aa,$47,
                $8e,$56,$fd,$8f,$4b,$a5,$d0,$9f,$fa,$1c,$6d,$92,$7c,$40,$f4,$c3,
                $37,$30,$40,$49,$e8,$a9,$52,$fb,$cb,$f4,$5c,$6f,$a7,$7a,$41,$a4);
{$endif}

var
  err: integer;
  DK : array[0..127] of byte;
begin
  writeln('Test scrypt_kdf/s/ss:');

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(nil, 0, nil, 0, n1, r1, p1, DK, sizeof(DK));
  writeln('  Test case 1: ', err:6, compmem(@DK, @dk1, sizeof(dk1)):8);

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdfss(pw2, salt2, n2, r2, p2, DK, sizeof(dk2));
  writeln('  Test case 2: ', err:6, compmem(@DK, @dk2, sizeof(dk2)):8);

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(@pw3, sizeof(pw3), @salt3, sizeof(salt3), n3, r3, p3, DK, sizeof(dk3));
  writeln('  Test case 3: ', err:6, compmem(@DK, @dk3, sizeof(dk3)):8);

{$ifndef BIT16}
  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(@pw4, sizeof(pw4), @salt4, sizeof(salt4), n4, r4, p4, DK, sizeof(dk4));
  writeln('  Test case 4: ', err:6, compmem(@DK, @dk4, sizeof(dk4)):8);

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(@pw5, sizeof(pw5), @salt5, sizeof(salt5), n5, r5, p5, DK, sizeof(dk4));
  writeln('  Test case 5: ', err:6, compmem(@DK, @dk5, sizeof(dk5)):8);

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(@pw6, sizeof(pw6), @salt6, sizeof(salt6), n6, r6, p6, DK, sizeof(dk6));
  writeln('  Test case 6: ', err:6, compmem(@DK, @dk6, sizeof(dk6)):8);

  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdfs(pw7, @salt7, sizeof(salt7), n7, r7, p7, DK, sizeof(dk7));
  writeln('  Test case 7: ', err:6, compmem(@DK, @dk7, sizeof(dk7)):8);
{$endif}

{$ifdef BIT64}
  fillchar(DK, sizeof(DK), 0);
  err := scrypt_kdf(@pw8, sizeof(pw8), @salt8, sizeof(salt8), n8, r8, p8, DK, sizeof(dk8));
  writeln('  Test case 8: ', err:6, compmem(@DK, @dk8, sizeof(dk8)):8);
{$endif}

end;


begin
  writeln('Test program for scrypt key derivation unit  -  (c) 2014 W.Ehrhardt');
  {$ifdef VER70} writeln('MemAvail: ', MemAvail); {$endif}
  test_scrypt_kdf;
  {$ifdef VER70} writeln('MemAvail: ', MemAvail); {$endif}
end.
