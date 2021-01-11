{-Test prog for AES CFB8, we Dec.2007}

program T_AESCF8;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_cfb8, mem_util, BTypes;


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
  {-Simple encrypt/decrypt test for AES-CFB8 mode}
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
  sample = 'This is a short test sample text for AES CFB8 mode'#0;

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
  writeln('Simple encrypt/decrypt test for AES-CFB8 mode');
  writeln('Org. plain text: ', plain);
  writeln;

  writeln('++++ 128 bit key ++++');
  pt  := plain;
  Err := AES_CFB8_Init(key128, 128, IV, context);
  Err := AES_CFB8_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB8_Init(key128, 128, IV, context);
  Err := AES_CFB8_Decrypt(@pt, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;

  Err := AES_CFB8_Init(key128, 128, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB8_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 192 bit key ++++');
  pt  := plain;
  Err := AES_CFB8_Init(key192, 192, IV, context);
  Err := AES_CFB8_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB8_Init(key192, 192, IV, context);
  Err := AES_CFB8_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CFB8_Init(key192, 192, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB8_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;

  writeln;
  writeln('++++ 256 bit key ++++');
  pt  := plain;
  Err := AES_CFB8_Init(key256, 256, IV, context);
  Err := AES_CFB8_Encrypt(@pt, @ct, sizeof(plain), context);
  CheckError;
  pt := ct;
  Err := AES_CFB8_Init(key256, 256, IV, context);
  Err := AES_CFB8_Decrypt(@ct, @pt, sizeof(plain), context);
  CheckError;
  writeln('Block Encr/decr: ', pt);
  CheckRes;
  Err := AES_CFB8_Init(key256, 256, IV, context);
  for i:=1 to sizeof(plain) do begin
    if Err=0 then Err := AES_CFB8_Decrypt(@ct[i], @pt[i], 1, context);
  end;
  CheckError;
  writeln(' Char Encr/decr: ', pt);
  CheckRes;
end;



{---------------------------------------------------------------------------}
procedure NistTests;
  {-NIST SP 800-38A CFB8/AES Tests}
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

  plain  : array[0..17] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d);

     ct1 : array[0..17] of byte = ($3b,$79,$42,$4c,$9c,$0d,$d4,$36,
                                   $ba,$ce,$9e,$0e,$d4,$58,$6a,$4f,
                                   $32,$b9);

     ct2 : array[0..17] of byte = ($cd,$a2,$52,$1e,$f0,$a9,$05,$ca,
                                   $44,$cd,$05,$7c,$bf,$0d,$47,$a0,
                                   $67,$8a);

     ct3 : array[0..17] of byte = ($dc,$1f,$1a,$85,$20,$a6,$4d,$b5,
                                   $5f,$cc,$8a,$c5,$54,$84,$4e,$88,
                                   $97,$00);

var
  ct: array[0..255] of byte;
begin
  writeln;
  writeln('=============================');
  writeln('NIST SP 800-38A CFB8/AES tests');
  Err := AES_CFB8_Init(key128, 128, IV, context);
  Err := AES_CFB8_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.7  CFB8-AES128.Encrypt - OK: ',CompMem(@ct1, @ct, sizeof(ct1)));

  Err := AES_CFB8_Init(key128, 128, IV, context);
  Err := AES_CFB8_Decrypt(@ct1, @ct, sizeof(ct1), context);
  writeln('Test F.3.8  CFB8-AES128.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CFB8_Init(key192, 192, IV, context);
  Err := AES_CFB8_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.9  CFB8-AES192.Encrypt - OK: ',CompMem(@ct2, @ct, sizeof(ct2)));

  Err := AES_CFB8_Init(key192, 192, IV, context);
  Err := AES_CFB8_Decrypt(@ct2, @ct, sizeof(ct3), context);
  writeln('Test F.3.10 CFB8-AES192.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

  Err := AES_CFB8_Init(key256, 256, IV, context);
  Err := AES_CFB8_Encrypt(@plain, @ct, sizeof(plain), context);
  writeln('Test F.3.11 CFB8-AES256.Encrypt - OK: ',CompMem(@ct3, @ct, sizeof(ct3)));

  Err := AES_CFB8_Init(key256, 256, IV, context);
  Err := AES_CFB8_Decrypt(@ct3, @ct, sizeof(ct3), context);
  writeln('Test F.3.12 CFB8-AES256.Decrypt - OK: ',CompMem(@plain, @ct, sizeof(plain)));

end;

begin
  SimpleTests;
  NistTests;
end.
