{-Test prog for keyderiv, we 04.01.2005}
{ compile with USEDLL for testing with DLL}
{ Use test iteration count 3000000 with cmd line param big}

program t_kderiv;

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
    KeyDeriv,
  {$endif}
  Mem_Util;


{---------------------------------------------------------------------------}
{Test vectors from: RFC 3211 - Password-based Encryption for CMS}

{Answers DKx_2, DKx_5 calculated with LibTomCrypt 1.00}
{t_kdf.c, sha1 values from t_kdf match DK1_1 and DK2_1}

const
  pwd1  : array[1..08] of char = 'password';
  salt1 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);
  DK1_1 : array[0..07] of byte = ($d1,$da,$a7,$86,$15,$f2,$87,$e6);
  DK1_2 : array[0..07] of byte = ($74,$e9,$8b,$2e,$9e,$ed,$da,$ab);
  DK1_5 : array[0..07] of byte = ($cb,$ef,$bd,$b1,$95,$e0,$ae,$48);
  DK1_W : array[0..07] of byte = ($b7,$c9,$7c,$c6,$cf,$4c,$95,$1f);

  pwd2  = 'All n-entities must communicate with other n-entities via n-1 entiteeheehees';
  salt2 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);
  DK2_1 : array[0..15] of byte = ($6A,$89,$70,$BF,$68,$C9,$2C,$AE,$A8,$4A,$8D,$F2,$85,$10,$85,$86);
  DK2_2 : array[0..15] of byte = ($80,$0b,$1c,$9d,$6d,$00,$75,$a8,$f3,$df,$7a,$17,$ca,$32,$72,$2e);
  DK2_5 : array[0..15] of byte = ($74,$0b,$0e,$1e,$94,$08,$b1,$35,$ab,$d0,$cd,$1c,$4f,$2e,$14,$eb);
  DK2_W : array[0..15] of byte = ($19,$b1,$ef,$f8,$a2,$e3,$79,$ec,$16,$3e,$1b,$b4,$a1,$c8,$de,$5d);

  pwd3  = 'All n-entities must communicate with other n-entities via n-1 entiteeheehees';
  salt3 : array[0..07] of byte = ($12,$34,$56,$78,$78,$56,$34,$12);
  DK3_1 : array[0..15] of byte = ($1e,$07,$dd,$01,$ee,$1e,$f1,$80,$ab,$77,$99,$0d,$ba,$95,$9e,$d5);
  DK3_2 : array[0..15] of byte = ($d1,$e9,$59,$f7,$62,$97,$db,$ff,$9b,$86,$06,$82,$15,$ad,$a4,$ec);
  DK3_5 : array[0..15] of byte = ($82,$74,$a0,$0b,$f8,$5b,$4c,$89,$c2,$69,$e9,$2c,$52,$fd,$c7,$8e);
  DK3_W : array[0..15] of byte = ($a1,$53,$80,$ae,$8a,$4e,$74,$1d,$e4,$ed,$97,$60,$51,$25,$f2,$d0);

var
  key: array[0..63] of byte;

begin
  writeln('==== Test keyderiv ======');

  PBKDF2(@pwd1, 8, @salt1, 8, 5, key, 8);
  writeln('HMACSHA1 RFC 3211 TV1: ', compmem(@key, @DK1_1, sizeof(DK1_1)));
  PBKDF2s(pwd2, @salt2, 8, 500, key, 16);
  writeln('HMACSHA1 RFC 3211 TV2: ', compmem(@key, @DK2_1, sizeof(DK2_1)));

  PBKDF2_256(@pwd1, 8, @salt1, 8, 5, key, 8);
  writeln('HMACSHA256 LibTom TV1: ', compmem(@key, @DK1_2, sizeof(DK1_2)));
  PBKDF2s_256(pwd2, @salt2, 8, 500, key, 16);
  writeln('HMACSHA256 LibTom TV2: ', compmem(@key, @DK2_2, sizeof(DK2_2)));

  PBKDF2_512(@pwd1, 8, @salt1, 8, 5, key, 8);
  writeln('HMACSHA512 LibTom TV1: ', compmem(@key, @DK1_5, sizeof(DK1_5)));
  PBKDF2s_512(pwd2, @salt2, 8, 500, key, 16);
  writeln('HMACSHA512 LibTom TV2: ', compmem(@key, @DK2_5, sizeof(DK2_5)));

  PBKDF2_Whirl(@pwd1, 8, @salt1, 8, 5, key, 8);
  writeln('HMACWhirl  LibTom TV1: ', compmem(@key, @DK1_W, sizeof(DK1_W)));
  PBKDF2s_Whirl(pwd2, @salt2, 8, 500, key, 16);
  writeln('HMACWhirl  LibTom TV2: ', compmem(@key, @DK2_W, sizeof(DK2_W)));

  if paramstr(1)='big' then begin
    PBKDF2s(pwd3, @salt3, 8, 300000, key, 16);
    writeln('HMACSHA1   LibTom TV3: ', compmem(@key, @DK3_1, sizeof(DK3_1)));
    PBKDF2s_256(pwd3, @salt3, 8, 300000, key, 16);
    writeln('HMACSHA256 LibTom TV3: ', compmem(@key, @DK3_2, sizeof(DK3_2)));
    PBKDF2s_512(pwd3, @salt3, 8, 300000, key, 16);
    writeln('HMACSHA512 LibTom TV3: ', compmem(@key, @DK3_5, sizeof(DK3_5)));
    PBKDF2s_Whirl(pwd3, @salt3, 8, 300000, key, 16);
    writeln('HMACWhirl  LibTom TV3: ', compmem(@key, @DK3_W, sizeof(DK3_5)));
  end;
end.
