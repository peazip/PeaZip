{-Test prog for AES ECB, we Sep.2003}

program T_AESECB;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_ecb, mem_util, BTypes;

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
  {-Simple encrypt/decrypt test for AES-ECB mode}
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
  sample = 'This is a short test sample for AES ECB mode'#0;

var
  ct, pt, plain: array[1..length(sample)] of char8;

  procedure CheckRes;
  begin
    writeln('Test Dec(Enc)=Id: ',CompMem(@pt, @plain, sizeof(plain)));
  end;


begin
  plain := sample;
  writeln;
  writeln('============================================');
  writeln('Simple encrypt/decrypt test for AES-ECB mode');
  writeln('Plain text: ', plain);
  writeln;

  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_ECB_Init_Encr(key128, 128, context);
  Err := AES_ECB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_ECB_Init_Decr(key128, 128, context);
  Err := AES_ECB_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/Dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_ECB_Init_Decr(key128, 128, context);
  Err := AES_ECB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_ECB_Init_Encr(key192, 192, context);
  Err := AES_ECB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_ECB_Init_Decr(key192, 192, context);
  Err := AES_ECB_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/Dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_ECB_Init_Decr(key192, 192, context);
  Err := AES_ECB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_ECB_Init_Encr(key256, 256, context);
  Err := AES_ECB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  Err := AES_ECB_Init_Decr(key256, 256, context);
  Err := AES_ECB_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/Dec @pt<>@ct: ', pt);
  CheckRes;
  pt := ct;
  Err := AES_ECB_Init_Decr(key256, 256, context);
  Err := AES_ECB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Enc/dec inplace : ', pt);
  CheckRes;
end;


{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A ECB/AES Tests}
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

  plain  : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

     ct1 : array[0..63] of byte = ($3a,$d7,$7b,$b4,$0d,$7a,$36,$60,
                                   $a8,$9e,$ca,$f3,$24,$66,$ef,$97,
                                   $f5,$d3,$d5,$85,$03,$b9,$69,$9d,
                                   $e7,$85,$89,$5a,$96,$fd,$ba,$af,
                                   $43,$b1,$cd,$7f,$59,$8e,$ce,$23,
                                   $88,$1b,$00,$e3,$ed,$03,$06,$88,
                                   $7b,$0c,$78,$5e,$27,$e8,$ad,$3f,
                                   $82,$23,$20,$71,$04,$72,$5d,$d4);

     ct2 : array[0..63] of byte = ($bd,$33,$4f,$1d,$6e,$45,$f2,$5f,
                                   $f7,$12,$a2,$14,$57,$1f,$a5,$cc,
                                   $97,$41,$04,$84,$6d,$0a,$d3,$ad,
                                   $77,$34,$ec,$b3,$ec,$ee,$4e,$ef,
                                   $ef,$7a,$fd,$22,$70,$e2,$e6,$0a,
                                   $dc,$e0,$ba,$2f,$ac,$e6,$44,$4e,
                                   $9a,$4b,$41,$ba,$73,$8d,$6c,$72,
                                   $fb,$16,$69,$16,$03,$c1,$8e,$0e);

     ct3 : array[0..63] of byte = ($f3,$ee,$d1,$bd,$b5,$d2,$a0,$3c,
                                   $06,$4b,$5a,$7e,$3d,$b1,$81,$f8,
                                   $59,$1c,$cb,$10,$d4,$10,$ed,$26,
                                   $dc,$5b,$a7,$4a,$31,$36,$28,$70,
                                   $b6,$ed,$21,$b9,$9c,$a6,$f4,$f9,
                                   $f1,$53,$e7,$b1,$be,$af,$ed,$1d,
                                   $23,$30,$4b,$7a,$39,$f9,$f3,$ff,
                                   $06,$7d,$8d,$8f,$9e,$24,$ec,$c7);


var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A ECB/AES tests');
  Err := AES_ECB_Init_Encr(key128, 128, context);
  Err := AES_ECB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.1.1 ECB-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_ECB_Init_Decr(key128, 128, context);
  Err := AES_ECB_Decrypt(@ct{1}, @ct, sizeof(ct1), context);
  CheckError;
  writeln('Test F.1.2 ECB-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_ECB_Init_Encr(key192, 192, context);
  Err := AES_ECB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.1.3 ECB-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_ECB_Init_Decr(key192, 192, context);
  Err := AES_ECB_Decrypt(@ct{2}, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.1.4 ECB-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_ECB_Init_Encr(key256, 256, context);
  Err := AES_ECB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.1.5 ECB-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_ECB_Init_Decr(key256, 256, context);
  Err := AES_ECB_Decrypt(@ct{3}, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.1.6 ECB-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

end;

begin
  SimpleTests;
  NistTests;
end.
