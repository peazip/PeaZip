{-Test prog for OMAC1/2, we 05.2004}

program T_OMAC;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef J_OPT}
  {$J+}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  AES_Type, AES_OMAC, Mem_Util;

{Common keys and msg data}
{from http://www.nuee.nagoya-u.ac.jp/labs/tiwata/omac/tv/omac?-tv.txt}
const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);

  key192 : array[0..23] of byte = ($8e,$73,$b0,$f7,$da,$0e,$64,$52,
                                   $c8,$10,$f3,$2b,$80,$90,$79,$e5,
                                   $62,$f8,$ea,$d2,$52,$2c,$6b,$7b);

  key256 : array[0..31] of byte = ($60,$3d,$eb,$10,$15,$ca,$71,$be,
                                   $2b,$73,$ae,$f0,$85,$7d,$77,$81,
                                   $1f,$35,$2c,$07,$3b,$61,$08,$d7,
                                   $2d,$98,$10,$a3,$09,$14,$df,$f4);

const
  msg    : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

{from http://www.nuee.nagoya-u.ac.jp/labs/tiwata/omac/tv/omac1-tv.txt}
const
  tag00: TAESBlock = ($bb,$1d,$69,$29,$e9,$59,$37,$28,$7f,$a3,$7d,$12,$9b,$75,$67,$46);
  tag01: TAESBlock = ($07,$0a,$16,$b4,$6b,$4d,$41,$44,$f7,$9b,$dd,$9d,$d0,$4a,$28,$7c);
  tag02: TAESBlock = ($df,$a6,$67,$47,$de,$9a,$e6,$30,$30,$ca,$32,$61,$14,$97,$c8,$27);
  tag03: TAESBlock = ($51,$f0,$be,$bf,$7e,$3b,$9d,$92,$fc,$49,$74,$17,$79,$36,$3c,$fe);

  tag10: TAESBlock = ($d1,$7d,$df,$46,$ad,$aa,$cd,$e5,$31,$ca,$c4,$83,$de,$7a,$93,$67);
  tag11: TAESBlock = ($9e,$99,$a7,$bf,$31,$e7,$10,$90,$06,$62,$f6,$5e,$61,$7c,$51,$84);
  tag12: TAESBlock = ($8a,$1d,$e5,$be,$2e,$b3,$1a,$ad,$08,$9a,$82,$e6,$ee,$90,$8b,$0e);
  tag13: TAESBlock = ($a1,$d5,$df,$0e,$ed,$79,$0f,$79,$4d,$77,$58,$96,$59,$f3,$9a,$11);

  tag20: TAESBlock = ($02,$89,$62,$f6,$1b,$7b,$f8,$9e,$fc,$6b,$55,$1f,$46,$67,$d9,$83);
  tag21: TAESBlock = ($28,$a7,$02,$3f,$45,$2e,$8f,$82,$bd,$4b,$f2,$8d,$8c,$37,$c3,$5c);
  tag22: TAESBlock = ($aa,$f3,$d8,$f1,$de,$56,$40,$c2,$32,$f5,$b1,$69,$b9,$c9,$11,$e6);
  tag23: TAESBlock = ($e1,$99,$21,$90,$54,$9f,$6e,$d5,$69,$6a,$2c,$05,$6c,$31,$54,$10);

{http://www.nuee.nagoya-u.ac.jp/labs/tiwata/omac/tv/omac2-tv.txt}
const
  tag30: TAESBlock = ($f6,$bc,$6a,$41,$f4,$f8,$45,$93,$80,$9e,$59,$b7,$19,$29,$9c,$fe);
  tag31: TAESBlock = ($07,$0a,$16,$b4,$6b,$4d,$41,$44,$f7,$9b,$dd,$9d,$d0,$4a,$28,$7c);
  tag32: TAESBlock = ($23,$fd,$aa,$08,$31,$cd,$31,$44,$91,$ce,$4b,$25,$ac,$b6,$02,$3b);
  tag33: TAESBlock = ($51,$f0,$be,$bf,$7e,$3b,$9d,$92,$fc,$49,$74,$17,$79,$36,$3c,$fe);

  tag40: TAESBlock = ($14,$9f,$57,$9d,$f2,$12,$9d,$45,$a6,$92,$66,$89,$8f,$55,$ae,$b2);
  tag41: TAESBlock = ($9e,$99,$a7,$bf,$31,$e7,$10,$90,$06,$62,$f6,$5e,$61,$7c,$51,$84);
  tag42: TAESBlock = ($b3,$5e,$2d,$1b,$73,$ae,$d4,$9b,$78,$bd,$bd,$fe,$61,$f6,$46,$df);
  tag43: TAESBlock = ($a1,$d5,$df,$0e,$ed,$79,$0f,$79,$4d,$77,$58,$96,$59,$f3,$9a,$11);

  tag50: TAESBlock = ($47,$fb,$de,$71,$86,$6e,$ae,$60,$80,$35,$5b,$5f,$c7,$ff,$70,$4c);
  tag51: TAESBlock = ($28,$a7,$02,$3f,$45,$2e,$8f,$82,$bd,$4b,$f2,$8d,$8c,$37,$c3,$5c);
  tag52: TAESBlock = ($f0,$18,$e6,$05,$36,$11,$b3,$4b,$c8,$72,$d6,$b7,$ff,$24,$74,$9f);
  tag53: TAESBlock = ($e1,$99,$21,$90,$54,$9f,$6e,$d5,$69,$6a,$2c,$05,$6c,$31,$54,$10);

var
  ctx: TAESContext;
  tag: TAESBlock;

{---------------------------------------------------------------------------}
procedure Test(Alg: integer; var key; KL,ML: word; var st: TAESBlock; Hdr: string);
  {-Test for OMAC(Alg) with key and message length ML, st: known tag     }
  { tags are calculated two times: 1. single call of AES_OMAC_Update with}
  { complete msg, 2. AES_OMAC_Update for each byte of msg                }
const
  Res: array[boolean] of string[5] = ('Error', 'OK');
var
  i: word;
begin
  write(Alg:4, hdr);
  if AES_OMAC_Init(key, KL, ctx)<>0 then begin
    writeln('AES_OMAC_Init Error');
    halt;
  end;
  if AES_OMAC_Update(@msg, ML, ctx)<>0 then begin
    writeln('AES_OMAC_Update Error');
    halt;
  end;
  if Alg=2 then AES_OMAC2_Final(tag, ctx)
  else AES_OMAC1_Final(tag, ctx);
  write(Res[CompMem(@tag, @st, sizeof(tag))]:8);
  if AES_OMAC_Init(key, KL, ctx)<>0 then begin
    writeln('AES_OMAC_Init Error');
    halt;
  end;
  for i:=1 to ML do begin
    if AES_OMAC_Update(@msg[i-1], 1, ctx)<>0 then begin
      writeln('AES_OMAC_Update Error');
      halt;
    end;
  end;
  if Alg=2 then AES_OMAC2_Final(tag, ctx)
  else AES_OMAC1_Final(tag, ctx);
  writeln(Res[CompMem(@tag, @st, sizeof(tag))]:8);
end;

begin
  writeln('Test program AES OMAC 1/2 modes  (C) 2004-2006 W.Ehrhardt');
  writeln('KL/ML: Key/Message length in bits/bytes');
  writeln('Single/Multi: process message with one/multiple call(s)');
  writeln('OMAC  KL/ML  Single   Multi');
  Test(1, key128, 128,  0, tag00, ' 128/00');
  Test(1, key128, 128, 16, tag01, ' 128/16');
  Test(1, key128, 128, 40, tag02, ' 128/40');
  Test(1, key128, 128, 64, tag03, ' 128/64');

  Test(1, key192, 192,  0, tag10, ' 192/00');
  Test(1, key192, 192, 16, tag11, ' 192/16');
  Test(1, key192, 192, 40, tag12, ' 192/40');
  Test(1, key192, 192, 64, tag13, ' 192/64');

  Test(1, key256, 256,  0, tag20, ' 256/00');
  Test(1, key256, 256, 16, tag21, ' 256/16');
  Test(1, key256, 256, 40, tag22, ' 256/40');
  Test(1, key256, 256, 64, tag23, ' 256/64');

  Test(2, key128, 128,  0, tag30, ' 128/00');
  Test(2, key128, 128, 16, tag31, ' 128/16');
  Test(2, key128, 128, 40, tag32, ' 128/40');
  Test(2, key128, 128, 64, tag33, ' 128/64');

  Test(2, key192, 192,  0, tag40, ' 192/00');
  Test(2, key192, 192, 16, tag41, ' 192/16');
  Test(2, key192, 192, 40, tag42, ' 192/40');
  Test(2, key192, 192, 64, tag43, ' 192/64');

  Test(2, key256, 256,  0, tag50, ' 256/00');
  Test(2, key256, 256, 16, tag51, ' 256/16');
  Test(2, key256, 256, 40, tag52, ' 256/40');
  Test(2, key256, 256, 64, tag53, ' 256/64');
end.
