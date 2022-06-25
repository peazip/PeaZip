{-Test prog for AES OFB, we Sep.2003}

program T_AESOFB;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_OFB, mem_util, BTypes;

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
  {-Simple encrypt/decrypt test for AES-OFB mode}
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
  sample = 'This is a short test sample text for AES OFB mode'#0;

var
  i   : integer;
  ct, pt, plain: array[1..length(sample)] of char8;
  IV  : TAESBlock;

  procedure CheckRes;
  begin
    writeln('Decr(Encr)=Id  : ',CompMem(@pt, @plain, sizeof(plain)));
  end;


begin
  for i:=0 to 15 do IV[i] := random(256);
  plain := sample;

  writeln;
  writeln('============================================');
  writeln('Simple encrypt/decrypt test for AES-OFB mode');
  writeln('Org. plain text: ', plain);
  writeln;

  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_OFB_Init(key128, 128, IV, context);
  Err := AES_OFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_OFB_Init(key128, 128, IV, context);
  Err := AES_OFB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_OFB_Init(key128, 128, IV, context);
  for i:=1 to sizeof(plain) do begin
    Err := AES_OFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_OFB_Init(key192, 192, IV, context);
  Err := AES_OFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_OFB_Init(key192, 192, IV, context);
  Err := AES_OFB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_OFB_Init(key192, 192, IV, context);
  for i:=1 to sizeof(plain) do begin
    Err := AES_OFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_OFB_Init(key256, 256, IV, context);
  Err := AES_OFB_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_OFB_Init(key256, 256, IV, context);
  Err := AES_OFB_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_OFB_Init(key256, 256, IV, context);
  for i:=1 to sizeof(plain) do begin
    Err := AES_OFB_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
end;


{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A OFB/AES Tests}
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
                                   $77,$89,$50,$8d,$16,$91,$8f,$03,
                                   $f5,$3c,$52,$da,$c5,$4e,$d8,$25,
                                   $97,$40,$05,$1e,$9c,$5f,$ec,$f6,
                                   $43,$44,$f7,$a8,$22,$60,$ed,$cc,
                                   $30,$4c,$65,$28,$f6,$59,$c7,$78,
                                   $66,$a5,$10,$d9,$c1,$d6,$ae,$5e);

     ct2 : array[0..63] of byte = ($cd,$c8,$0d,$6f,$dd,$f1,$8c,$ab,
                                   $34,$c2,$59,$09,$c9,$9a,$41,$74,
                                   $fc,$c2,$8b,$8d,$4c,$63,$83,$7c,
                                   $09,$e8,$17,$00,$c1,$10,$04,$01,
                                   $8d,$9a,$9a,$ea,$c0,$f6,$59,$6f,
                                   $55,$9c,$6d,$4d,$af,$59,$a5,$f2,
                                   $6d,$9f,$20,$08,$57,$ca,$6c,$3e,
                                   $9c,$ac,$52,$4b,$d9,$ac,$c9,$2a);

     ct3 : array[0..63] of byte = ($dc,$7e,$84,$bf,$da,$79,$16,$4b,
                                   $7e,$cd,$84,$86,$98,$5d,$38,$60,
                                   $4f,$eb,$dc,$67,$40,$d2,$0b,$3a,
                                   $c8,$8f,$6a,$d8,$2a,$4f,$b0,$8d,
                                   $71,$ab,$47,$a0,$86,$e8,$6e,$ed,
                                   $f3,$9d,$1c,$5b,$ba,$97,$c4,$08,
                                   $01,$26,$14,$1d,$67,$f3,$7b,$e8,
                                   $53,$8f,$5a,$8b,$e7,$40,$e4,$84);

var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A OFB/AES tests');
  Err := AES_OFB_Init(key128, 128, IV, context);
  Err := AES_OFB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.4.1 OFB-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_OFB_Init(key128, 128, IV, context);
  Err := AES_OFB_Decrypt(@ct1, @ct, sizeof(ct1), context);
  CheckError;
  writeln('Test F.4.2 OFB-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_OFB_Init(key192, 192, IV, context);
  Err := AES_OFB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.4.3 OFB-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_OFB_Init(key192, 192, IV, context);
  Err := AES_OFB_Decrypt(@ct2, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.4.4 OFB-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_OFB_Init(key256, 256, IV, context);
  Err := AES_OFB_Encrypt(@plain, @ct, sizeof(plain), context);
  CheckError;
  writeln('Test F.4.5 OFB-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_OFB_Init(key256, 256, IV, context);
  Err := AES_OFB_Decrypt(@ct3, @ct, sizeof(ct3), context);
  CheckError;
  writeln('Test F.4.6 OFB-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

end;

begin
  SimpleTests;
  NistTests;
end.
