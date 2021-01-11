{-Test prog for AES CTR, we Sep.2003}

program T_AESCTR;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_ctr, mem_util, BTypes;

var
  Context: TAESContext;
  Err : integer;

{---------------------------------------------------------------------------}
procedure CheckError;
begin
  if Err<>0 then writeln('Error ',Err);
end;

{---------------------------------------------------------------------------}
procedure SimpleTests;
  {-Simple encrypt/decrypt test for AES-CTR mode}
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
  sample = 'This is a short test sample text for AES CTR mode'#0;

var
  IV  : TAESBlock;
  i   : integer;
  ct, pt, plain: array[1..length(sample)] of char8;

  procedure CheckRes;
  begin
    writeln('Decr(Encr)=Id  : ',CompMem(@pt, @plain, sizeof(plain)));
  end;

begin
  for i:=0 to 15 do IV[i] := random(256);
  plain := sample;

  writeln;
  writeln('============================================');
  writeln('Simple encrypt/decrypt test for AES-CTR mode');
  writeln('Org. plain text: ', plain);
  writeln;
  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_CTR_Init(key128, 128, IV, context);
  Err := AES_CTR_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CTR_Init(key128, 128, IV, context);
  Err := AES_CTR_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CTR_Init(key128, 128, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CTR_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_CTR_Init(key192, 192, IV, context);
  Err := AES_CTR_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CTR_Init(key192, 192, IV, context);
  Err := AES_CTR_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CTR_Init(key192, 192, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CTR_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_CTR_Init(key256, 256, IV, context);
  Err := AES_CTR_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CTR_Init(key256, 256, IV, context);
  Err := AES_CTR_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CTR_Init(key256, 256, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CTR_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;
end;


{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A CTR/AES Tests}
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

     CTR : TAESBlock =            ($f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,
                                   $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  plain  : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

     ct1 : array[0..63] of byte = ($87,$4d,$61,$91,$b6,$20,$e3,$26,
                                   $1b,$ef,$68,$64,$99,$0d,$b6,$ce,
                                   $98,$06,$f6,$6b,$79,$70,$fd,$ff,
                                   $86,$17,$18,$7b,$b9,$ff,$fd,$ff,
                                   $5a,$e4,$df,$3e,$db,$d5,$d3,$5e,
                                   $5b,$4f,$09,$02,$0d,$b0,$3e,$ab,
                                   $1e,$03,$1d,$da,$2f,$be,$03,$d1,
                                   $79,$21,$70,$a0,$f3,$00,$9c,$ee);



     ct2 : array[0..63] of byte = ($1a,$bc,$93,$24,$17,$52,$1c,$a2,
                                   $4f,$2b,$04,$59,$fe,$7e,$6e,$0b,
                                   $09,$03,$39,$ec,$0a,$a6,$fa,$ef,
                                   $d5,$cc,$c2,$c6,$f4,$ce,$8e,$94,
                                   $1e,$36,$b2,$6b,$d1,$eb,$c6,$70,
                                   $d1,$bd,$1d,$66,$56,$20,$ab,$f7,
                                   $4f,$78,$a7,$f6,$d2,$98,$09,$58,
                                   $5a,$97,$da,$ec,$58,$c6,$b0,$50);



     ct3 : array[0..63] of byte = ($60,$1e,$c3,$13,$77,$57,$89,$a5,
                                   $b7,$a7,$f5,$04,$bb,$f3,$d2,$28,
                                   $f4,$43,$e3,$ca,$4d,$62,$b5,$9a,
                                   $ca,$84,$e9,$90,$ca,$ca,$f5,$c5,
                                   $2b,$09,$30,$da,$a2,$3d,$e9,$4c,
                                   $e8,$70,$17,$ba,$2d,$84,$98,$8d,
                                   $df,$c9,$c5,$8d,$b6,$7a,$ad,$a6,
                                   $13,$c2,$dd,$08,$45,$79,$41,$a6);

var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A CTR/AES tests');

  Err := AES_CTR_Init(key128, 128, CTR, context);
  Err := AES_CTR_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.5.1 CTR-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_CTR_Init(key128, 128, CTR, context);
  Err := AES_CTR_Decrypt(@ct1, @ct, sizeof(ct1), context);
  CheckError;
  writeln('Test F.5.2 CTR-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CTR_Init(key192, 192, CTR, context);
  Err := AES_CTR_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.5.3 CTR-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_CTR_Init(key192, 192, CTR, context);
  Err := AES_CTR_Decrypt(@ct2, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.5.4 CTR-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CTR_Init(key256, 256, CTR, context);
  Err := AES_CTR_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.5.5 CTR-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_CTR_Init(key256, 256, CTR, context);
  Err := AES_CTR_Decrypt(@ct3, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.5.6 CTR-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));
end;

begin
  SimpleTests;
  NistTests;
end.
