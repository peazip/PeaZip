{-Test prog for KDFs, we Jul.2008}
{ Use test iteration count 300000 with cmd line param 'big'}

program t_kdf;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  Hash, kdf,
  {include hash units in order to register the hash descriptors}
  MD4, MD5, RMD160, SHA1, SHA224, SHA256, SHA384, SHA512, Whirl512,
  BTypes, Mem_Util;


const
  pwd1  : array[1..8] of char8 = 'password';
  salt1 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);
  pwd2  = 'All n-entities must communicate with other n-entities via n-1 entiteeheehees';
  salt2 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);
  pwd3  = 'All n-entities must communicate with other n-entities via n-1 entiteeheehees';
  salt3 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);


{---------------------------------------------------------------------------}
procedure RFC_Tests;
  {-Test vectors from: RFC 3211 - Password-based Encryption for CMS}
const
  DK1_1 : array[0..07] of byte = ($d1,$da,$a7,$86,$15,$f2,$87,$e6);
  DK2_1 : array[0..15] of byte = ($6A,$89,$70,$BF,$68,$C9,$2C,$AE,$A8,$4A,$8D,$F2,$85,$10,$85,$86);
var
  key: array[0..63] of byte;
  err: integer;
  phash: PHashDesc;
begin
  phash := FindHash_by_ID(_SHA1);
  err := pbkdf2(phash, @pwd1, 8, @salt1, 8, 5, key, 8);
  write('pbkdf2 SHA1 RFC 3211 TV1: ');
  if err<>0 then writeln('Error ',Err,' from pbkdf2')
  else writeln(compmem(@key, @DK1_1, sizeof(DK1_1)));
  write('pbkdf2 SHA1 RFC 3211 TV2: ');
  Err := pbkdf2s(phash, pwd2, @salt2, 8, 500, key, 16);
  if err<>0 then writeln('Error ',Err,' from pbkdf2s')
  else writeln(compmem(@key, @DK2_1, sizeof(DK2_1)));
end;


{---------------------------------------------------------------------------}
procedure LTC_Tests;
  {-Test values calculated with LibTomCrypt 1.00, t_kdf.c}
  { sha1 values from t_kdf match DK1_1 and DK2_1}
const
  MinTSDH = _MD4;
  MaxTSDH = _Whirlpool;
type
  TSD_HASH = MinTSDH..MaxTSDH;
type
  T08Bytes = array[0..07] of byte;
  T16Bytes = array[0..15] of byte;
  T24Bytes = array[0..23] of byte;
const
  DK1: array[TSD_HASH] of T08Bytes = (
         ($83,$f7,$af,$fd,$da,$cc,$93,$3d),       {MD4}
         ($97,$49,$89,$e7,$0b,$be,$86,$7f),       {MD5}
         ($da,$e4,$c9,$4b,$d3,$83,$3c,$13),       {RMD160}
         ($d1,$da,$a7,$86,$15,$f2,$87,$e6),       {SHA1}
         ($d0,$74,$3f,$dc,$4d,$6f,$9b,$55),       {SHA224}
         ($74,$e9,$8b,$2e,$9e,$ed,$da,$ab),       {SHA256}
         ($d0,$ed,$7e,$b4,$71,$76,$61,$6f),       {SHA384}
         ($cb,$ef,$bd,$b1,$95,$e0,$ae,$48),       {SHA512}
         ($b7,$c9,$7c,$c6,$cf,$4c,$95,$1f));      {Whirlpool}

  DK2: array[TSD_HASH] of T16Bytes = (
         ($8d,$a7,$99,$3b,$d8,$28,$34,$d7,$87,$69,$6a,$50,$6c,$0d,$ba,$41),  {MD4}
         ($47,$e0,$38,$3b,$d7,$4b,$01,$56,$51,$4c,$66,$30,$db,$15,$93,$61),  {MD5}
         ($3d,$57,$c3,$f0,$1c,$42,$e3,$5a,$0a,$76,$e1,$11,$4e,$f2,$b4,$f8),  {RMD160}
         ($6a,$89,$70,$bf,$68,$c9,$2c,$ae,$a8,$4a,$8d,$f2,$85,$10,$85,$86),  {SHA1}
         ($60,$e3,$cf,$c4,$2e,$4a,$f2,$46,$40,$7a,$9d,$84,$e0,$58,$7b,$4c),  {SHA224}
         ($80,$0b,$1c,$9d,$6d,$00,$75,$a8,$f3,$df,$7a,$17,$ca,$32,$72,$2e),  {SHA256}
         ($5b,$43,$2d,$fa,$49,$be,$a2,$07,$8f,$89,$4f,$b2,$5c,$e3,$b1,$c5),  {SHA384}
         ($74,$0b,$0e,$1e,$94,$08,$b1,$35,$ab,$d0,$cd,$1c,$4f,$2e,$14,$eb),  {SHA512}
         ($19,$b1,$ef,$f8,$a2,$e3,$79,$ec,$16,$3e,$1b,$b4,$a1,$c8,$de,$5d));  {Whirlpool}

  DK3: array[TSD_HASH] of T16Bytes = (
         ($d4,$5e,$10,$7c,$75,$16,$30,$28,$4c,$a3,$83,$d4,$1f,$ae,$2e,$ea),  {MD4}
         ($d3,$67,$1b,$f9,$29,$cf,$ba,$e5,$76,$68,$06,$7e,$24,$dd,$b2,$26),  {MD5}
         ($44,$b3,$46,$d2,$62,$73,$b1,$f1,$d6,$00,$01,$93,$b5,$f5,$d3,$c3),  {RMD160}
         ($1e,$07,$dd,$01,$ee,$1e,$f1,$80,$ab,$77,$99,$0d,$ba,$95,$9e,$d5),  {SHA1}
         ($0b,$32,$8f,$f2,$1a,$66,$84,$80,$4b,$c5,$3c,$b1,$2a,$af,$30,$4a),  {SHA224}
         ($d1,$e9,$59,$f7,$62,$97,$db,$ff,$9b,$86,$06,$82,$15,$ad,$a4,$ec),  {SHA256}
         ($a6,$4c,$47,$47,$d6,$7f,$11,$ea,$26,$82,$43,$ff,$56,$70,$47,$63),  {SHA384}
         ($82,$74,$a0,$0b,$f8,$5b,$4c,$89,$c2,$69,$e9,$2c,$52,$fd,$c7,$8e),  {SHA512}
         ($a1,$53,$80,$ae,$8a,$4e,$74,$1d,$e4,$ed,$97,$60,$51,$25,$f2,$d0)); {Whirlpool}

  DK4: array[TSD_HASH] of T08Bytes = (
         ($07,$04,$7a,$af,$ac,$0a,$fc,$56),       {MD4}
         ($66,$7e,$65,$60,$2a,$ea,$77,$74),       {MD5}
         ($36,$29,$0f,$61,$82,$a0,$f3,$7d),       {RMD160}
         ($ab,$bd,$b0,$9c,$99,$7b,$3c,$94),       {SHA1}
         ($14,$fe,$48,$7e,$a2,$75,$5c,$35),       {SHA224}
         ($f9,$2e,$73,$70,$eb,$3a,$1a,$49),       {SHA256}
         ($02,$db,$75,$84,$e2,$02,$a6,$13),       {SHA384}
         ($eb,$13,$86,$4f,$1e,$0a,$a8,$35),       {SHA512}
         ($74,$55,$89,$8e,$53,$8f,$39,$e4));      {Whirlpool}

  DK5: array[TSD_HASH] of T16Bytes = (
         ($66,$ce,$d9,$89,$0b,$3c,$8e,$31,$9e,$00,$27,$7e,$9e,$a7,$81,$0f),  {MD4}
         ($8d,$e8,$f8,$8f,$4d,$05,$5a,$40,$74,$b1,$d8,$a6,$ce,$73,$d8,$f9),  {MD5}
         ($7c,$06,$bb,$ed,$59,$51,$8d,$34,$08,$af,$27,$6d,$b8,$74,$6f,$1d),  {RMD160}
         ($b6,$81,$bc,$f5,$5c,$7d,$40,$4c,$c1,$51,$5c,$f9,$b7,$fe,$a7,$bd),  {SHA1}
         ($25,$5d,$43,$84,$ff,$e7,$1c,$b3,$a8,$55,$0c,$55,$1a,$71,$da,$0e),  {SHA224}
         ($b3,$c1,$97,$10,$85,$92,$ab,$b6,$48,$7a,$4b,$e7,$92,$79,$a0,$14),  {SHA256}
         ($fe,$e3,$58,$23,$79,$7a,$81,$df,$f8,$51,$01,$b3,$77,$ce,$b0,$a5),  {SHA384}
         ($ba,$e2,$08,$a9,$8b,$63,$0e,$68,$7b,$51,$c6,$73,$b8,$ee,$59,$30),  {SHA512}
         ($4e,$9a,$0d,$75,$ad,$64,$41,$d8,$54,$d1,$db,$d4,$60,$97,$79,$a6)); {Whirlpool}

  DK6: array[TSD_HASH] of T16Bytes = (
         ($7d,$39,$de,$3a,$d7,$4a,$9a,$97,$3c,$8d,$8c,$7b,$c3,$48,$79,$68),  {MD4}
         ($56,$54,$ef,$2f,$45,$82,$17,$b1,$56,$b4,$ee,$e5,$18,$a7,$fb,$2b),  {MD5}
         ($ef,$68,$62,$5e,$fe,$c1,$33,$d5,$6b,$16,$59,$b4,$71,$7b,$07,$c6),  {RMD160}
         ($77,$b0,$71,$2d,$b7,$21,$4e,$f3,$0e,$1a,$00,$f7,$d8,$05,$fb,$00),  {SHA1}
         ($16,$da,$0f,$e2,$1a,$ef,$52,$58,$54,$b9,$0f,$b5,$11,$f0,$91,$0f),  {SHA224}
         ($db,$26,$d2,$0d,$d2,$ef,$f7,$63,$86,$f6,$b1,$82,$91,$f0,$0a,$4a),  {SHA256}
         ($1b,$72,$70,$8b,$44,$87,$d2,$ff,$f4,$e7,$4c,$8d,$1b,$d1,$6b,$c8),  {SHA384}
         ($4c,$27,$68,$91,$1c,$63,$34,$71,$7b,$5e,$8e,$59,$6d,$14,$3d,$bc),  {SHA512}
         ($2d,$df,$7f,$52,$4f,$63,$2c,$5e,$2a,$24,$20,$a2,$d3,$9e,$3c,$c8)); {Whirlpool}

  DK7: array[TSD_HASH] of T24Bytes = (
         ($09,$5e,$16,$e1,$33,$e3,$f6,$ee,$fc,$e1,$03,$cd,$9a,$44,$23,$34,$85,$e5,$44,$10,$fe,$08,$5e,$4b),  {MD4}
         ($3d,$de,$78,$45,$ea,$31,$55,$79,$cd,$93,$7d,$1c,$0b,$d9,$14,$43,$76,$8a,$c7,$a7,$1a,$44,$db,$ab),  {MD5}
         ($0a,$ad,$b7,$88,$27,$7b,$0b,$e3,$25,$84,$d9,$40,$c7,$f5,$36,$fb,$2c,$5d,$20,$97,$5d,$b8,$a8,$a9),  {RMD160}
         ($14,$9e,$e7,$d2,$31,$2f,$34,$05,$b4,$65,$9f,$37,$ff,$47,$5a,$9d,$6e,$33,$cf,$e8,$3b,$6e,$0b,$b0),  {SHA1}
         ($14,$f7,$ad,$4d,$fc,$66,$f9,$e6,$3f,$7c,$0a,$6f,$36,$03,$32,$bb,$0d,$3d,$b1,$57,$20,$3d,$7f,$75),  {SHA224}
         ($75,$56,$ab,$40,$d3,$04,$6b,$db,$37,$94,$71,$03,$ec,$82,$30,$a4,$e4,$0b,$0e,$e6,$a7,$24,$11,$88),  {SHA256}
         ($3f,$7a,$ee,$d3,$1b,$dd,$69,$5f,$d7,$0d,$ee,$4f,$9c,$6a,$31,$92,$cd,$6c,$92,$56,$c2,$20,$80,$41),  {SHA384}
         ($3f,$5d,$11,$82,$00,$e8,$06,$1d,$eb,$17,$83,$98,$38,$01,$18,$be,$f8,$91,$f3,$95,$65,$1c,$97,$ef),  {SHA512}
         ($df,$59,$9e,$00,$60,$1b,$ca,$03,$2b,$1f,$1e,$4d,$0e,$5a,$26,$a2,$a3,$40,$f7,$39,$38,$50,$da,$ce)); {Whirlpool}

var
  key: array[0..63] of byte;
  err: integer;
  algo : THashAlgorithm;
  phash: PHashDesc;
begin
  for algo := MinTSDH to MaxTSDH do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found')
    else begin
      write('pbkdf2 ',phash^.HName:11,' LibTom TV1: ');
      err := pbkdf2(phash, @pwd1, 8, @salt1, 8, 5, key, 8);
      if err<>0 then writeln('Error ',Err,' from pbkdf2')
      else writeln(compmem(@key, @DK1[algo], 8));

      write('pbkdf2 ',phash^.HName:11,' LibTom TV2: ');
      err := pbkdf2s(phash, pwd2, @salt2, 8, 500, key, 16);
      if err<>0 then writeln('Error ',Err,' from pbkdf2s')
      else writeln(compmem(@key, @DK2[algo], 16));

      write('pbkdf1 ',phash^.HName:11,' LibTom TV4: ');
      err := pbkdf1(phash, @pwd1, 8, @salt1, 5, key, 8);
      if err<>0 then writeln('Error ',Err,' from pbkdf1')
      else writeln(compmem(@key, @DK4[algo], 8));

      write('pbkdf1 ',phash^.HName:11,' LibTom TV5: ');
      err := pbkdf1s(phash, pwd2, @salt2, 500, key, 16);
      if err<>0 then writeln('Error ',Err,' from pbkdf1s')
      else writeln(compmem(@key, @DK5[algo], 16));

      write('  mgf1 ',phash^.HName:11,' LibTom TV7: ');
      err := mgf1(phash, @pwd1, 8, key, 24);
      if err<>0 then writeln('Error ',Err,' from mgf1')
      else writeln(compmem(@key, @DK7[algo], 24));
    end;
  end;
  if paramstr(1)<>'big' then exit;
  for algo := C_MinHash to C_MaxHash do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found')
    else begin
      write('pbkdf2 ',phash^.HName:11,' LibTom TV3: ');
      err := pbkdf2s(phash, pwd3, @salt3, 8, 300000, key, 16);;
      if err<>0 then writeln('Error ',Err,' from pbkdf2s')
      else writeln(compmem(@key, @DK3[algo], 16));

      write('pbkdf1 ',phash^.HName:11,' LibTom TV6: ');
      err := pbkdf1s(phash, pwd3, @salt3, 300000, key, 16);;
      if err<>0 then writeln('Error ',Err,' from pbkdf1s')
      else writeln(compmem(@key, @DK6[algo], 16));
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure CryptoSys_Tests;
  {-Vectors from http://www.di-mgt.com.au/cryptoKDFs.html}
var
  err: integer;
  phash: PHashDesc;
  key: array[0..63] of byte;
const
  salt: array[0..7]  of byte = ($78,$57,$8e,$5a,$5d,$63,$cb,$06);
  key1: array[0..15] of byte = ($dc,$19,$84,$7e,$05,$c6,$4d,$2f,
                                $af,$10,$eb,$fb,$4a,$3d,$2a,$20);
  key2: array[0..23] of byte = ($bf,$de,$6b,$e9,$4d,$f7,$e1,$1d,
                                $d4,$09,$bc,$e2,$0a,$02,$55,$ec,
                                $32,$7c,$b9,$36,$ff,$e9,$36,$43);
     Z: array[0..7]  of byte = ($de,$ad,$be,$ef,$fe,$eb,$da,$ed);
    K1: array[0..31] of byte = ($b0,$ad,$56,$5b,$14,$b4,$78,$ca,
                                $d4,$76,$38,$56,$ff,$30,$16,$b1,
                                $a9,$3d,$84,$0f,$87,$26,$1b,$ed,
                                $e7,$dd,$f0,$f9,$30,$5a,$6e,$44);
    K2: array[0..31] of byte = ($87,$26,$1b,$ed,$e7,$dd,$f0,$f9,
                                $30,$5a,$6e,$44,$a7,$4e,$6a,$08,
                                $46,$de,$de,$27,$f4,$82,$05,$c6,
                                $b1,$41,$88,$87,$42,$b0,$ce,$2c);
    K3: array[0..31] of byte = ($60,$ce,$f6,$70,$59,$af,$33,$f6,
                                $ae,$bc,$e1,$e1,$01,$88,$f4,$34,
                                $f8,$03,$06,$ac,$03,$60,$47,$0a,
                                $eb,$41,$f8,$1b,$af,$b3,$57,$90);


begin
  phash := FindHash_by_ID(_SHA1);
  if phash=nil then begin
    writeln('Hash SHA1 not registered/found');
    exit;
  end;

  err := kdf1(phash, @Z, sizeof(Z), nil, 0, key, 32);
  write('   kdf1: ');
  if err=0 then writeln(compmem(@key,@K1,sizeof(K1)))
  else writeln(' err=',err);

  err := kdf2(phash, @Z, sizeof(Z), nil, 0, key, 32);
  write('   kdf2: ');
  if err=0 then writeln(compmem(@key,@K2,sizeof(K2)))
  else writeln(' err=',err);

  err := kdf3(phash, @Z, sizeof(Z), nil, 0, key, 32);
  write('   kdf3: ');
  if err=0 then writeln(compmem(@key,@K3,sizeof(K3)))
  else writeln(' err=',err);

  err := pbkdf1(phash, @pwd1, 8, @salt, 1000, key, 16);
  write(' pbkdf1: ');
  if err=0 then writeln(compmem(@key,@key1,sizeof(key1)))
  else writeln(' err=',err);
  write('pbkdf1s: ');
  err := pbkdf1s(phash, 'password', @salt, 1000, key, 16);
  if err=0 then writeln(compmem(@key,@key1,sizeof(key1)))
  else writeln(' err=',err);

  err := pbkdf2(phash, @pwd1, 8, @salt, 8, 2048, key, 24);
  write(' pbkdf2: ');
  if err=0 then writeln(compmem(@key,@key2,sizeof(key2)))
  else writeln(' err=',err);
  write('pbkdf2s: ');
  err := pbkdf2s(phash, 'password', @salt, 8, 2048, key, 24);
  if err=0 then writeln(compmem(@key,@key2,sizeof(key2)))
  else writeln(' err=',err);

end;


{---------------------------------------------------------------------------}
procedure pkcs1_tests;
  {-Examples from ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1-vec.zip}

const
  {oaep-int.txt}
      seed: array[0.. 19] of byte = ($aa,$fd,$12,$f6,$59,$ca,$e6,$34,
                                     $89,$b4,$79,$e5,$07,$6d,$de,$c2,
                                     $f0,$6c,$b5,$8f);

    dbMask: array[0..106] of byte = ($06,$e1,$de,$b2,$36,$9a,$a5,$a5,
                                     $c7,$07,$d8,$2c,$8e,$4e,$93,$24,
                                     $8a,$c7,$83,$de,$e0,$b2,$c0,$46,
                                     $26,$f5,$af,$f9,$3e,$dc,$fb,$25,
                                     $c9,$c2,$b3,$ff,$8a,$e1,$0e,$83,
                                     $9a,$2d,$db,$4c,$dc,$fe,$4f,$f4,
                                     $77,$28,$b4,$a1,$b7,$c1,$36,$2b,
                                     $aa,$d2,$9a,$b4,$8d,$28,$69,$d5,
                                     $02,$41,$21,$43,$58,$11,$59,$1b,
                                     $e3,$92,$f9,$82,$fb,$3e,$87,$d0,
                                     $95,$ae,$b4,$04,$48,$db,$97,$2f,
                                     $3a,$c1,$4e,$af,$f4,$9c,$8c,$3b,
                                     $7c,$fc,$95,$1a,$51,$ec,$d1,$dd,
                                     $e6,$12,$64);

  maskedDB: array[0..106] of byte = ($dc,$d8,$7d,$5c,$68,$f1,$ee,$a8,
                                     $f5,$52,$67,$c3,$1b,$2e,$8b,$b4,
                                     $25,$1f,$84,$d7,$e0,$b2,$c0,$46,
                                     $26,$f5,$af,$f9,$3e,$dc,$fb,$25,
                                     $c9,$c2,$b3,$ff,$8a,$e1,$0e,$83,
                                     $9a,$2d,$db,$4c,$dc,$fe,$4f,$f4,
                                     $77,$28,$b4,$a1,$b7,$c1,$36,$2b,
                                     $aa,$d2,$9a,$b4,$8d,$28,$69,$d5,
                                     $02,$41,$21,$43,$58,$11,$59,$1b,
                                     $e3,$92,$f9,$82,$fb,$3e,$87,$d0,
                                     $95,$ae,$b4,$04,$48,$db,$97,$2f,
                                     $3a,$c1,$4f,$7b,$c2,$75,$19,$52,
                                     $81,$ce,$32,$d2,$f1,$b7,$6d,$4d,
                                     $35,$3e,$2d);

   seedMask: array[0..19] of byte = ($41,$87,$0b,$5a,$b0,$29,$e6,$57,
                                     $d9,$57,$50,$b5,$4c,$28,$3c,$08,
                                     $72,$5d,$be,$a9);
 {pss-int.txt }
       piH: array[0.. 19] of byte = ($df,$1a,$89,$6f,$9d,$8b,$c8,$16,
                                     $d9,$7c,$d7,$a2,$c4,$3b,$ad,$54,
                                     $6f,$be,$8c,$fe);

  pidbMask: array[0..106] of byte = ($66,$e4,$67,$2e,$83,$6a,$d1,$21,
                                     $ba,$24,$4b,$ed,$65,$76,$b8,$67,
                                     $d9,$a4,$47,$c2,$8a,$6e,$66,$a5,
                                     $b8,$7d,$ee,$7f,$bc,$7e,$65,$af,
                                     $50,$57,$f8,$6f,$ae,$89,$84,$d9,
                                     $ba,$7f,$96,$9a,$d6,$fe,$02,$a4,
                                     $d7,$5f,$74,$45,$fe,$fd,$d8,$5b,
                                     $6d,$3a,$47,$7c,$28,$d2,$4b,$a1,
                                     $e3,$75,$6f,$79,$2d,$d1,$dc,$e8,
                                     $ca,$94,$44,$0e,$cb,$52,$79,$ec,
                                     $d3,$18,$3a,$31,$1f,$c8,$97,$39,
                                     $a9,$66,$43,$13,$6e,$8b,$0f,$46,
                                     $5e,$87,$a4,$53,$5c,$d4,$c5,$9b,
                                     $10,$02,$8d);

var
  err: integer;
  phash: PHashDesc;
  key: array[0..200] of byte;
begin
  phash := FindHash_by_ID(_SHA1);
  if phash=nil then begin
    writeln('Hash SHA1 not registered/found');
    exit;
  end;
  write('Test   dbMask = mgf1(seed, length(..))     : ');
  err := mgf1(phash, @seed, sizeof(seed), key, sizeof(dbMask));
  if err<>0 then writeln('Error ',Err,' from mgf1')
  else writeln(compmem(@key, @dbMask, sizeof(dbMask)));
  write('Test seedMask = mgf1(maskedDB, length(..)) : ');
  err := mgf1(phash, @maskedDB, sizeof(maskedDB), key, sizeof(seedMask));
  if err<>0 then writeln('Error ',Err,' from mgf1')
  else writeln(compmem(@key, @seedMask, sizeof(seedMask)));
  write('Test pidbMask = mgf1(piH,length(..))       : ');
  err := mgf1(phash, @piH, sizeof(piH), key, sizeof(pidbMask));
  if err<>0 then writeln('Error ',Err,' from mgf1')
  else writeln(compmem(@key, @pidbMask, sizeof(pidbMask)));
end;


{---------------------------------------------------------------------------}
procedure BouncyISO;
  {-From bouncycastle\crypto\test\KDF2GeneratorTest.java bzw Shoup: std6.pdf}
const
  seed1 : array[0.. 63] of byte = ($03,$2e,$45,$32,$6f,$a8,$59,$a7,
                                   $2e,$c2,$35,$ac,$ff,$92,$9b,$15,
                                   $d1,$37,$2e,$30,$b2,$07,$25,$5f,
                                   $06,$11,$b8,$f7,$85,$d7,$64,$37,
                                   $41,$52,$e0,$ac,$00,$9e,$50,$9e,
                                   $7b,$a3,$0c,$d2,$f1,$77,$8e,$11,
                                   $3b,$64,$e1,$35,$cf,$4e,$22,$92,
                                   $c7,$5e,$fe,$52,$88,$ed,$fd,$a4);

  mask1 : array[0..127] of byte = ($10,$a2,$40,$3d,$b4,$2a,$87,$43,
                                   $cb,$98,$9d,$e8,$6e,$66,$8d,$16,
                                   $8c,$be,$60,$46,$e2,$3f,$f2,$6f,
                                   $74,$1e,$87,$94,$9a,$3b,$ba,$13,
                                   $11,$ac,$17,$9f,$81,$9a,$3d,$18,
                                   $41,$2e,$9e,$b4,$56,$68,$f2,$92,
                                   $3c,$08,$7c,$12,$99,$00,$5f,$8d,
                                   $5f,$d4,$2c,$a2,$57,$bc,$93,$e8,
                                   $fe,$e0,$c5,$a0,$d2,$a8,$aa,$70,
                                   $18,$54,$01,$fb,$bd,$99,$37,$9e,
                                   $c7,$6c,$66,$3e,$9a,$29,$d0,$b7,
                                   $0f,$3f,$e2,$61,$a5,$9c,$dc,$24,
                                   $87,$5a,$60,$b4,$aa,$cb,$13,$19,
                                   $fa,$11,$c3,$36,$5a,$8b,$79,$a4,
                                   $46,$69,$f2,$6f,$ba,$93,$3d,$01,
                                   $2d,$b2,$13,$d7,$e3,$b1,$63,$49);

  {Shoup std6.pdf C.6.2}
  seed2 : array[0.. 63] of byte = ($03,$2e,$45,$32,$6f,$a8,$59,$a7,
                                   $2e,$c2,$35,$ac,$ff,$92,$9b,$15,
                                   $d1,$37,$2e,$30,$b2,$07,$25,$5f,
                                   $06,$11,$b8,$f7,$85,$d7,$64,$37,
                                   $41,$52,$e0,$ac,$00,$9e,$50,$9e,
                                   $7b,$a3,$0c,$d2,$f1,$77,$8e,$11,
                                   $3b,$64,$e1,$35,$cf,$4e,$22,$92,
                                   $c7,$5e,$fe,$52,$88,$ed,$fd,$a4);


  mask2 : array[0..127] of byte = ($0e,$6a,$26,$eb,$7b,$95,$6c,$cb,
                                   $8b,$3b,$dc,$1c,$a9,$75,$bc,$57,
                                   $c3,$98,$9e,$8f,$ba,$d3,$1a,$22,
                                   $46,$55,$d8,$00,$c4,$69,$54,$84,
                                   $0f,$f3,$20,$52,$cd,$f0,$d6,$40,
                                   $56,$2b,$df,$ad,$fa,$26,$3c,$fc,
                                   $cf,$3c,$52,$b2,$9f,$2a,$f4,$a1,
                                   $86,$99,$59,$bc,$77,$f8,$54,$cf,
                                   $15,$bd,$7a,$25,$19,$29,$85,$a8,
                                   $42,$db,$ff,$8e,$13,$ef,$ee,$5b,
                                   $7e,$7e,$55,$bb,$e4,$d3,$89,$64,
                                   $7c,$68,$6a,$9a,$9a,$b3,$fb,$88,
                                   $9b,$2d,$77,$67,$d3,$83,$7e,$ea,
                                   $4e,$0a,$2f,$04,$b5,$3c,$a8,$f5,
                                   $0f,$b3,$12,$25,$c1,$be,$2d,$01,
                                   $26,$c8,$c7,$a4,$75,$3b,$08,$07);


  seed3 : array[0.. 19] of byte = ($ca,$7c,$0f,$8c,$3f,$fa,$87,$a9,
                                   $6e,$1b,$74,$ac,$8e,$6a,$f5,$94,
                                   $34,$7b,$b4,$0a);


  mask3 : array[0.. 19] of byte = ($74,$4a,$b7,$03,$f5,$bc,$08,$2e,
                                   $59,$18,$5f,$6d,$04,$9d,$2d,$36,
                                   $7d,$b2,$45,$c2);


  seed4 : array[0.. 20] of byte = ($04,$99,$b5,$02,$fc,$8b,$5b,$af,
                                   $b0,$f4,$04,$7e,$73,$1d,$1f,$9f,
                                   $d8,$cd,$0d,$88,$81);


  mask4 : array[0.. 39] of byte = ($03,$c6,$22,$80,$c8,$94,$e1,$03,
                                   $c6,$80,$b1,$3c,$d4,$b4,$ae,$74,
                                   $0a,$5e,$f0,$c7,$25,$47,$29,$2f,
                                   $82,$dc,$6b,$17,$77,$f4,$7d,$63,
                                   $ba,$9d,$1e,$a7,$32,$db,$f3,$86);
var
  err: integer;
  phash: PHashDesc;
  key: array[0..200] of byte;
begin
  phash := FindHash_by_ID(_SHA256);
  if phash=nil then begin
    writeln('Hash SHA256 not registered/found');
    exit;
  end;

  write('Test mask1 = kdf2(SHA256, seed1) : ');
  err := kdf2(phash, @seed1, sizeof(seed2), nil, 0, key, sizeof(mask1));
  if err<>0 then writeln('Error ',Err,' from kdf2')
  else writeln(compmem(@key, @mask1, sizeof(mask1)));

  phash := FindHash_by_ID(_SHA1);
  if phash=nil then begin
    writeln('Hash SHA1 not registered/found');
    exit;
  end;

  write('Test mask2 = kdf2(SHA1, seed2)   : ');
  err := kdf2(phash, @seed2, sizeof(seed2), nil, 0, key, sizeof(mask2));
  if err<>0 then writeln('Error ',Err,' from kdf2')
  else writeln(compmem(@key, @mask2, sizeof(mask2)));
  write('Test mask3 = kdf2(SHA1, seed3)   : ');
  err := kdf2(phash, @seed3, sizeof(seed3), nil, 0, key, sizeof(mask3));
  if err<>0 then writeln('Error ',Err,' from kdf2')
  else writeln(compmem(@key, @mask3, sizeof(mask3)));
  write('Test mask4 = kdf2(SHA1, seed4)   : ');
  err := kdf2(phash, @seed4, sizeof(seed4), nil, 0, key, sizeof(mask4));
  if err<>0 then writeln('Error ',Err,' from kdf2')
  else writeln(compmem(@key, @mask4, sizeof(mask4)));

end;


{---------------------------------------------------------------------------}
procedure hkdf_tests;
  {-Test of HKDF with test vectors from RFC5869}
const
  OKM1: array[0.. 41] of byte = ($3c,$b2,$5f,$25,$fa,$ac,$d5,$7a,
                                 $90,$43,$4f,$64,$d0,$36,$2f,$2a,
                                 $2d,$2d,$0a,$90,$cf,$1a,$5a,$4c,
                                 $5d,$b0,$2d,$56,$ec,$c4,$c5,$bf,
                                 $34,$00,$72,$08,$d5,$b8,$87,$18,
                                 $58,$65);
  OKM2: array[0.. 81] of byte = ($b1,$1e,$39,$8d,$c8,$03,$27,$a1,
                                 $c8,$e7,$f7,$8c,$59,$6a,$49,$34,
                                 $4f,$01,$2e,$da,$2d,$4e,$fa,$d8,
                                 $a0,$50,$cc,$4c,$19,$af,$a9,$7c,
                                 $59,$04,$5a,$99,$ca,$c7,$82,$72,
                                 $71,$cb,$41,$c6,$5e,$59,$0e,$09,
                                 $da,$32,$75,$60,$0c,$2f,$09,$b8,
                                 $36,$77,$93,$a9,$ac,$a3,$db,$71,
                                 $cc,$30,$c5,$81,$79,$ec,$3e,$87,
                                 $c1,$4c,$01,$d5,$c1,$f3,$43,$4f,
                                 $1d,$87);
  OKM3: array[0.. 41] of byte = ($8d,$a4,$e7,$75,$a5,$63,$c1,$8f,
                                 $71,$5f,$80,$2a,$06,$3c,$5a,$31,
                                 $b8,$a1,$1f,$5c,$5e,$e1,$87,$9e,
                                 $c3,$45,$4e,$5f,$3c,$73,$8d,$2d,
                                 $9d,$20,$13,$95,$fa,$a4,$b6,$1a,
                                 $96,$c8);
  OKM4: array[0.. 41] of byte = ($08,$5a,$01,$ea,$1b,$10,$f3,$69,
                                 $33,$06,$8b,$56,$ef,$a5,$ad,$81,
                                 $a4,$f1,$4b,$82,$2f,$5b,$09,$15,
                                 $68,$a9,$cd,$d4,$f1,$55,$fd,$a2,
                                 $c2,$2e,$42,$24,$78,$d3,$05,$f3,
                                 $f8,$96);
  OKM5: array[0.. 81] of byte = ($0b,$d7,$70,$a7,$4d,$11,$60,$f7,
                                 $c9,$f1,$2c,$d5,$91,$2a,$06,$eb,
                                 $ff,$6a,$dc,$ae,$89,$9d,$92,$19,
                                 $1f,$e4,$30,$56,$73,$ba,$2f,$fe,
                                 $8f,$a3,$f1,$a4,$e5,$ad,$79,$f3,
                                 $f3,$34,$b3,$b2,$02,$b2,$17,$3c,
                                 $48,$6e,$a3,$7c,$e3,$d3,$97,$ed,
                                 $03,$4c,$7f,$9d,$fe,$b1,$5c,$5e,
                                 $92,$73,$36,$d0,$44,$1f,$4c,$43,
                                 $00,$e2,$cf,$f0,$d0,$90,$0b,$52,
                                 $d3,$b4);
  OKM6: array[0.. 41] of byte = ($0a,$c1,$af,$70,$02,$b3,$d7,$61,
                                 $d1,$e5,$52,$98,$da,$9d,$05,$06,
                                 $b9,$ae,$52,$05,$72,$20,$a3,$06,
                                 $e0,$7b,$6b,$87,$e8,$df,$21,$d0,
                                 $ea,$00,$03,$3d,$e0,$39,$84,$d3,
                                 $49,$18);
  OKM7: array[0.. 41] of byte = ($2c,$91,$11,$72,$04,$d7,$45,$f3,
                                 $50,$0d,$63,$6a,$62,$f6,$4f,$0a,
                                 $b3,$ba,$e5,$48,$aa,$53,$d4,$23,
                                 $b0,$d1,$f2,$7e,$bb,$a6,$f5,$e5,
                                 $67,$3a,$08,$1d,$70,$cc,$e7,$ac,
                                 $fc,$48);
const
  sIKM4: string[20] = #11#11#11#11#11#11#11#11#11#11#11;

var
  IKM, salt, info: array[0..79] of byte;
  OKM: array[0..99] of byte;
  phash: PHashDesc;
  i: integer;
begin
  {Test case 1}
  phash := FindHash_by_ID(_SHA256);
  for i:=0 to 21 do  IKM[i] := $0b;
  for i:=0 to 12 do salt[i] := byte(i);
  for i:=0 to  9 do info[i] := byte($F0+i);
  i := hkdf(phash,  @IKM,22,  @salt,13, @info,10, OKM, 42);
  write('hkdf  test case 1: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM1, @OKM, sizeof(OKM1)));

  {Test case 2}
  phash := FindHash_by_ID(_SHA256);
  for i:=0 to 79 do  IKM[i] := byte(i);
  for i:=0 to 79 do salt[i] := byte(i+$60);
  for i:=0 to 79 do info[i] := byte(i+$b0);
  i := hkdf(phash,  @IKM,80,  @salt,80, @info,80, OKM, 82);
  write('hkdf  test case 2: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM2, @OKM, sizeof(OKM2)));

  {Test case 3}
  phash := FindHash_by_ID(_SHA256);
  for i:=0 to 21 do  IKM[i] := $0b;
  i := hkdf(phash,  @IKM,22,  @salt,0, @info,0, OKM, 42);
  write('hkdf  test case 3: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM3, @OKM, sizeof(OKM3)));

  {Test case 4}
  phash := FindHash_by_ID(_SHA1);
  for i:=0 to 12 do salt[i] := byte(i);
  for i:=0 to  9 do info[i] := byte(i+$F0);
  i := hkdfs(phash, sIKM4, @salt,13, @info,10, OKM, 42);
  write('hkdfs test case 4: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM4, @OKM, sizeof(OKM4)));

  {Test case 5}
  phash := FindHash_by_ID(_SHA1);
  for i:=0 to 79 do  IKM[i] := byte(i);
  for i:=0 to 79 do salt[i] := byte(i+$60);
  for i:=0 to 79 do info[i] := byte(i+$b0);
  i := hkdf(phash,  @IKM,80,  @salt,80, @info,80, OKM, 82);
  write('hkdf  test case 5: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM5, @OKM, sizeof(OKM5)));

  {Test case 6}
  phash := FindHash_by_ID(_SHA1);
  for i:=0 to 21 do IKM[i] := $0b;
  i := hkdf(phash,  @IKM,22,  @salt,0, @info,0, OKM, 42);
  write('hkdf  test case 6: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM6, @OKM, sizeof(OKM6)));

  {Test case 7}
  phash := FindHash_by_ID(_SHA1);
  for i:=0 to 21 do IKM[i] := $0c;
  i := hkdf(phash,  @IKM,22,  nil,0, @info, 0, OKM, 42);
  write('hkdf  test case 7: ');
  if i<>0 then writeln(' Error = ',i)
  else writeln(compmem(@OKM7, @OKM, sizeof(OKM7)));
end;


begin
  writeln('===== Test of unit kdf =====');

  writeln('** Bouncy/Shoup-ISO tests:');
  BouncyISO;

  writeln;
  writeln('** PKCS-1 tests:');
  pkcs1_tests;

  writeln;
  writeln('** RFC tests:');
  RFC_Tests;

  writeln;
  writeln('** CryptoSys tests:');
  CryptoSys_Tests;

  writeln;
  writeln('** LibTom tests:');
  LTC_Tests;

  writeln;
  writeln('** HKDF RFC 5869 tests:');
  HKDF_Tests;
end.
