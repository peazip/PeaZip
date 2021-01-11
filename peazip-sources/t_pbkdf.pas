{-Test prog for pb_kdf, we 17.01.2006}
{ compile with USEDLL for testing with DLL}
{ Use test iteration count 3000000 with cmd line param 'big'}

program t_pbkdf;

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      ch_intv,
    {$else}
      ch_intf,
    {$endif}
  {$else}
    Hash, pb_kdf,
    {include hash units in order to register the hash descriptors}
    MD4, MD5, RMD160, SHA1, SHA224, SHA256, SHA384, SHA512, Whirl512,
  {$endif}
  Mem_Util;


const
  pwd1  : array[1..08] of char = 'password';
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
  err := kdf2(phash, @pwd1, 8, @salt1, 8, 5, key, 8);
  write('HMAC SHA1 RFC 3211 TV1: ');
  if err<>0 then writeln('Error ',Err,' from kdf2')
  else writeln(compmem(@key, @DK1_1, sizeof(DK1_1)));
  write('HMAC SHA1 RFC 3211 TV2: ');
  Err := kdf2s(phash, pwd2, @salt2, 8, 500, key, 16);
  if err<>0 then writeln('Error ',Err,' from kdf2s')
  else writeln(compmem(@key, @DK2_1, sizeof(DK2_1)));
end;


{---------------------------------------------------------------------------}
procedure LTC_Tests;
  {-Test values calculated with LibTomCrypt 1.00, t_kdf.c}
  { sha1 values from t_kdf match DK1_1 and DK2_1}
type
  T08Bytes = array[0..07] of byte;
  T16Bytes = array[0..15] of byte;
const
  DK1: array[THashAlgorithm] of T08Bytes = (
         ($83,$f7,$af,$fd,$da,$cc,$93,$3d),       {MD4}
         ($97,$49,$89,$e7,$0b,$be,$86,$7f),       {MD5}
         ($da,$e4,$c9,$4b,$d3,$83,$3c,$13),       {RMD160}
         ($d1,$da,$a7,$86,$15,$f2,$87,$e6),       {SHA1}
         ($d0,$74,$3f,$dc,$4d,$6f,$9b,$55),       {SHA224}
         ($74,$e9,$8b,$2e,$9e,$ed,$da,$ab),       {SHA256}
         ($d0,$ed,$7e,$b4,$71,$76,$61,$6f),       {SHA384}
         ($cb,$ef,$bd,$b1,$95,$e0,$ae,$48),       {SHA512}
         ($b7,$c9,$7c,$c6,$cf,$4c,$95,$1f));       {Whirlpool}

  DK2: array[THashAlgorithm] of T16Bytes = (
         ($8d,$a7,$99,$3b,$d8,$28,$34,$d7,$87,$69,$6a,$50,$6c,$0d,$ba,$41),  {MD4}
         ($47,$e0,$38,$3b,$d7,$4b,$01,$56,$51,$4c,$66,$30,$db,$15,$93,$61),  {MD5}
         ($3d,$57,$c3,$f0,$1c,$42,$e3,$5a,$0a,$76,$e1,$11,$4e,$f2,$b4,$f8),  {RMD160}
         ($6a,$89,$70,$bf,$68,$c9,$2c,$ae,$a8,$4a,$8d,$f2,$85,$10,$85,$86),  {SHA1}
         ($60,$e3,$cf,$c4,$2e,$4a,$f2,$46,$40,$7a,$9d,$84,$e0,$58,$7b,$4c),  {SHA224}
         ($80,$0b,$1c,$9d,$6d,$00,$75,$a8,$f3,$df,$7a,$17,$ca,$32,$72,$2e),  {SHA256}
         ($5b,$43,$2d,$fa,$49,$be,$a2,$07,$8f,$89,$4f,$b2,$5c,$e3,$b1,$c5),  {SHA384}
         ($74,$0b,$0e,$1e,$94,$08,$b1,$35,$ab,$d0,$cd,$1c,$4f,$2e,$14,$eb),  {SHA512}
         ($19,$b1,$ef,$f8,$a2,$e3,$79,$ec,$16,$3e,$1b,$b4,$a1,$c8,$de,$5d));  {Whirlpool}

  DK3: array[THashAlgorithm] of T16Bytes = (
         ($d4,$5e,$10,$7c,$75,$16,$30,$28,$4c,$a3,$83,$d4,$1f,$ae,$2e,$ea),  {MD4}
         ($d3,$67,$1b,$f9,$29,$cf,$ba,$e5,$76,$68,$06,$7e,$24,$dd,$b2,$26),  {MD5}
         ($44,$b3,$46,$d2,$62,$73,$b1,$f1,$d6,$00,$01,$93,$b5,$f5,$d3,$c3),  {RMD160}
         ($1e,$07,$dd,$01,$ee,$1e,$f1,$80,$ab,$77,$99,$0d,$ba,$95,$9e,$d5),  {SHA1}
         ($0b,$32,$8f,$f2,$1a,$66,$84,$80,$4b,$c5,$3c,$b1,$2a,$af,$30,$4a),  {SHA224}
         ($d1,$e9,$59,$f7,$62,$97,$db,$ff,$9b,$86,$06,$82,$15,$ad,$a4,$ec),  {SHA256}
         ($a6,$4c,$47,$47,$d6,$7f,$11,$ea,$26,$82,$43,$ff,$56,$70,$47,$63),  {SHA384}
         ($82,$74,$a0,$0b,$f8,$5b,$4c,$89,$c2,$69,$e9,$2c,$52,$fd,$c7,$8e),  {SHA512}
         ($a1,$53,$80,$ae,$8a,$4e,$74,$1d,$e4,$ed,$97,$60,$51,$25,$f2,$d0)); {Whirlpool}

var
  key: array[0..63] of byte;
  err: integer;
  algo : THashAlgorithm;
  phash: PHashDesc;
begin
  for algo := C_MinHash to C_MaxHash do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found')
    else begin
      write('HMAC ',phash^.HName:11,' LibTom TV1: ');
      err := kdf2(phash, @pwd1, 8, @salt1, 8, 5, key, 8);
      if err<>0 then writeln('Error ',Err,' from kdf2')
      else writeln(compmem(@key, @DK1[algo], 8));
      write('HMAC ',phash^.HName:11,' LibTom TV2: ');
      err := kdf2s(phash, pwd2, @salt2, 8, 500, key, 16);
      if err<>0 then writeln('Error ',Err,' from kdf2s')
      else writeln(compmem(@key, @DK2[algo], 16));
    end;
  end;
  if paramstr(1)<>'big' then exit;
  for algo := C_MinHash to C_MaxHash do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found')
    else begin
      write('HMAC ',phash^.HName:11,' LibTom TV3: ');
      err := kdf2s(phash, pwd3, @salt3, 8, 300000, key, 16);;
      if err<>0 then writeln('Error ',Err,' from kdf2s')
      else writeln(compmem(@key, @DK3[algo], 16));
    end;
  end;
end;

begin
  writeln('==== Test of unit pb_kdf ======');
  RFC_Tests;
  LTC_Tests;
end.
