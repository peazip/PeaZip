{-Test prog for AES CBC, we Sep.2003}

program T_AESCBC;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_cbc, mem_util, BTypes;

var
  Context: TAESContext;
  Err: integer;


{---------------------------------------------------------------------------}
procedure CheckError;
begin
  if Err<>0 then writeln('Error ',Err);
end;


{---------------------------------------------------------------------------}
procedure SimpleTests;
  {-Simple encrypt/decrypt test for AES-CBC mode}
const
  Key128  : array[0..15] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f);
  Key192  : array[0..23] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
                                    $10, $11, $12, $13, $14, $15, $16, $17);
  Key256  : array[0..31] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
                                    $10, $11, $12, $13, $14, $15, $16, $17,
                                    $18, $19, $1a, $1b, $1c, $1d, $1e, $1f);

const
  sample = 'This is a short test sample for AES CBC mode'#0;


var
  i   : integer;
  ct, pt, plain: array[1..length(sample)] of char8;
  IV  : TAESBlock;

  procedure CheckRes;
  begin
    writeln('Test Dec(Enc)=Id: ',CompMem(@pt, @plain, sizeof(plain)));
  end;

begin
  for i:=0 to 15 do IV[i] := random(256);
  plain := sample;

  writeln;
  writeln('============================================');
  writeln('Simple encrypt/decrypt test for AES-CBC mode');
  writeln('Plain text: ', plain);
  writeln;

  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_CBC_Init_Encr(key128, 128, IV, context);
  Err := AES_CBC_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_CBC_Init_Decr(key128, 128, IV, context);
  Err := AES_CBC_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/Dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_CBC_Init_Decr(key128, 128, IV, context);
  Err := AES_CBC_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_CBC_Init_Encr(key192, 192, IV, context);
  Err := AES_CBC_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_CBC_Init_Decr(key192, 192, IV, context);
  Err := AES_CBC_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_CBC_Init_Decr(key192, 192, IV, context);
  Err := AES_CBC_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_CBC_Init_Encr(key256, 256, IV, context);
  Err := AES_CBC_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_CBC_Init_Decr(key256, 256, IV, context);
  Err := AES_CBC_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_CBC_Init_Decr(key256, 256, IV, context);
  Err := AES_CBC_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;
end;


{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A CBC/AES Tests}
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

      IV : TAESBlock =            ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

  plain  : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

     ct1 : array[0..63] of byte = ($76,$49,$ab,$ac,$81,$19,$b2,$46,
                                   $ce,$e9,$8e,$9b,$12,$e9,$19,$7d,
                                   $50,$86,$cb,$9b,$50,$72,$19,$ee,
                                   $95,$db,$11,$3a,$91,$76,$78,$b2,
                                   $73,$be,$d6,$b8,$e3,$c1,$74,$3b,
                                   $71,$16,$e6,$9e,$22,$22,$95,$16,
                                   $3f,$f1,$ca,$a1,$68,$1f,$ac,$09,
                                   $12,$0e,$ca,$30,$75,$86,$e1,$a7);

     ct2 : array[0..63] of byte = ($4f,$02,$1d,$b2,$43,$bc,$63,$3d,
                                   $71,$78,$18,$3a,$9f,$a0,$71,$e8,
                                   $b4,$d9,$ad,$a9,$ad,$7d,$ed,$f4,
                                   $e5,$e7,$38,$76,$3f,$69,$14,$5a,
                                   $57,$1b,$24,$20,$12,$fb,$7a,$e0,
                                   $7f,$a9,$ba,$ac,$3d,$f1,$02,$e0,
                                   $08,$b0,$e2,$79,$88,$59,$88,$81,
                                   $d9,$20,$a9,$e6,$4f,$56,$15,$cd);

     ct3 : array[0..63] of byte = ($f5,$8c,$4c,$04,$d6,$e5,$f1,$ba,
                                   $77,$9e,$ab,$fb,$5f,$7b,$fb,$d6,
                                   $9c,$fc,$4e,$96,$7e,$db,$80,$8d,
                                   $67,$9f,$77,$7b,$c6,$70,$2c,$7d,
                                   $39,$f2,$33,$69,$a9,$d9,$ba,$cf,
                                   $a5,$30,$e2,$63,$04,$23,$14,$61,
                                   $b2,$eb,$05,$e2,$c3,$9b,$e9,$fc,
                                   $da,$6c,$19,$07,$8c,$6a,$9d,$1b);

var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A CBC/AES tests');
  Err := AES_CBC_Init_Encr(key128, 128, IV, context);
  Err := AES_CBC_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.2.1 CBC-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_CBC_Init_Decr(key128, 128, IV, context);
  Err := AES_CBC_Decrypt(@ct{1}, @ct, sizeof(ct1), context);
  CheckError;
  writeln('Test F.2.2 CBC-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CBC_Init_Encr(key192, 192, IV, context);
  Err := AES_CBC_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.2.3 CBC-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_CBC_Init_Decr(key192, 192, IV, context);
  Err := AES_CBC_Decrypt(@ct{2}, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.2.4 CBC-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CBC_Init_Encr(key256, 256, IV, context);
  Err := AES_CBC_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.2.5 CBC-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_CBC_Init_Decr(key256, 256, IV, context);
  Err := AES_CBC_Decrypt(@ct{3}, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.2.6 CBC-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

end;

begin
  SimpleTests;
  NistTests;
end.
