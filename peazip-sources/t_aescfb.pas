{-Test prog for AES CFB, we Sep.2003}

program T_AESCFB;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_cfb, mem_util, BTypes;


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
  {-Simple encrypt/decrypt test for AES-CFB mode}
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
  sample = 'This is a short test sample text for AES CFB mode'#0;

var
  i   : integer;
  IV  : TAESBlock;
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
  writeln('Simple encrypt/decrypt test for AES-CFB mode');
  writeln('Org. plain text: ', plain);
  writeln;

  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_CFB_Init(key128, 128, IV, context);
  Err := AES_CFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB_Init(key128, 128, IV, context);
  Err := AES_CFB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;

  Err := AES_CFB_Init(key128, 128, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_CFB_Init(key192, 192, IV, context);
  Err := AES_CFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB_Init(key192, 192, IV, context);
  Err := AES_CFB_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CFB_Init(key192, 192, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_CFB_Init(key256, 256, IV, context);
  Err := AES_CFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB_Init(key256, 256, IV, context);
  Err := AES_CFB_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CFB_Init(key256, 256, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;
end;


{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A CFB/AES Tests}
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

     ct1 : array[0..63] of byte = ($3b,$3f,$d9,$2e,$b7,$2d,$ad,$20,
                                   $33,$34,$49,$f8,$e8,$3c,$fb,$4a,
                                   $c8,$a6,$45,$37,$a0,$b3,$a9,$3f,
                                   $cd,$e3,$cd,$ad,$9f,$1c,$e5,$8b,
                                   $26,$75,$1f,$67,$a3,$cb,$b1,$40,
                                   $b1,$80,$8c,$f1,$87,$a4,$f4,$df,
                                   $c0,$4b,$05,$35,$7c,$5d,$1c,$0e,
                                   $ea,$c4,$c6,$6f,$9f,$f7,$f2,$e6);

     ct2 : array[0..63] of byte = ($cd,$c8,$0d,$6f,$dd,$f1,$8c,$ab,
                                   $34,$c2,$59,$09,$c9,$9a,$41,$74,
                                   $67,$ce,$7f,$7f,$81,$17,$36,$21,
                                   $96,$1a,$2b,$70,$17,$1d,$3d,$7a,
                                   $2e,$1e,$8a,$1d,$d5,$9b,$88,$b1,
                                   $c8,$e6,$0f,$ed,$1e,$fa,$c4,$c9,
                                   $c0,$5f,$9f,$9c,$a9,$83,$4f,$a0,
                                   $42,$ae,$8f,$ba,$58,$4b,$09,$ff);

     ct3 : array[0..63] of byte = ($dc,$7e,$84,$bf,$da,$79,$16,$4b,
                                   $7e,$cd,$84,$86,$98,$5d,$38,$60,
                                   $39,$ff,$ed,$14,$3b,$28,$b1,$c8,
                                   $32,$11,$3c,$63,$31,$e5,$40,$7b,
                                   $df,$10,$13,$24,$15,$e5,$4b,$92,
                                   $a1,$3e,$d0,$a8,$26,$7a,$e2,$f9,
                                   $75,$a3,$85,$74,$1a,$b9,$ce,$f8,
                                   $20,$31,$62,$3d,$55,$b1,$e4,$71);

var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A CFB/AES tests');
  Err := AES_CFB_Init(key128, 128, IV, context);
  Err := AES_CFB_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.13 CFB128-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_CFB_Init(key128, 128, IV, context);
  Err := AES_CFB_Decrypt(@ct1, @ct, sizeof(ct1), context);
  writeln('Test F.3.14 CFB128-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CFB_Init(key192, 192, IV, context);
  Err := AES_CFB_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.15 CFB128-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_CFB_Init(key192, 192, IV, context);
  Err := AES_CFB_Decrypt(@ct2, @ct, sizeof(ct3), context);
  writeln('Test F.3.16 CFB128-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CFB_Init(key256, 256, IV, context);
  Err := AES_CFB_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.17 CFB128-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_CFB_Init(key256, 256, IV, context);
  Err := AES_CFB_Decrypt(@ct3, @ct, sizeof(ct3), context);
  writeln('Test F.3.18 CFB128-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

end;

begin
  SimpleTests;
  NistTests;
end.
