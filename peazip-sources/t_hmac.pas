{-Test prog for HMACs, we 2012-2017}

program t_hmac;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  hash,hmac,md4,md5,sha1,sha224,sha256,sha384,
  sha512,sha5_256,sha5_224,whirl512,rmd160,
  sha3_512,sha3_384,
  sha3_256,sha3_224,
  BTypes, mem_util;

(***************************************************************************)
{ This is the old t_hmac                                                    }
(***************************************************************************)

procedure HMac_Test_Part1;

const
  data1 : array[1..8]  of char8 = 'Hi There';
  data2 : array[1..28] of char8 = 'what do ya want for nothing?';
  data6 : array[1..54] of char8 = 'Test Using Larger Than Block-Size Key - Hash Key First';
  data7 : array[1..73] of char8 = 'Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data';

  key2  = 'Jefe';

var
  key6: array[1..80] of char8; {#$aa repeated 80 times}
  key7: array[1..80] of char8; {#$aa repeated 80 times}


{---------------------------------------------------------------------------}
procedure Test_HMAC_MD5;
  {-MD5 test cases from RFC2202}
const
  DigLen = sizeof(TMD5Digest);
const
  key1  : string[16] = #$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b;
  dig1  : array[1..16] of byte = ($92, $94, $72, $7a, $36, $38, $bb, $1c,
                                  $13, $f4, $8e, $f8, $15, $8b, $fc, $9d);
const
  dig2  : array[1..16] of byte = ($75, $0c, $78, $3e, $6a, $b0, $b5, $03,
                                  $ea, $a8, $6e, $31, $0a, $5d, $b7, $38);
const
  dig6  : array[1..16] of byte = ($6b, $1a, $b7, $fe, $4b, $d7, $bf, $8f,
                                  $0b, $62, $e6, $ce, $61, $b9, $d0, $cd);
const
  dig7  : array[1..16] of byte = ($6f, $63, $0f, $ad, $67, $cd, $a0, $ee,
                                  $1f, $b1, $f5, $62, $db, $3a, $a5, $3e);

var
  ctx  : THMAC_Context;
  mac  : THashDigest;
  phash: PHashDesc;

begin
  writeln('=========== HMAC MD5 self test cf. RFC 2202');
  phash := FindHash_by_Name('MD5');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  write('Test  1');
  hmac_inits(ctx, phash, key1);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig1, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig1, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  2');
  hmac_inits(ctx, phash, key2);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig2, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig2, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  6');
  hmac_init(ctx, phash, @key6, sizeof(key6));
  hmac_update(ctx, @data6, sizeof(data6));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig6, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig6, DigLen));
    writeln('Code: ', HexStr(@mac , DigLen));
  end;

  write('Test  7');
  hmac_init(ctx, phash, @key7, sizeof(key7));
  hmac_update(ctx, @data7, sizeof(data7));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig7, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig7, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

end;


{---------------------------------------------------------------------------}
procedure Test_RMD160;
  {-Eight tests for HMAC-RIPEMD-160 from Antoon Bosselaers' page}
const
  key: array[1..2] of TRMD160Digest = (
    ($00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff,$01,$23,$45,$67),
    ($01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98,$76,$54,$32,$10,$00,$11,$22,$33));
  dig: array[1..8] of TRMD160Digest = (
    {'' (empty string)}
    ($cf,$38,$76,$77,$bf,$da,$84,$83,$e6,$3b,$57,$e0,$6c,$3b,$5e,$cd,$8b,$7f,$c0,$55),
    ($fe,$69,$a6,$6c,$74,$23,$ee,$a9,$c8,$fa,$2e,$ff,$8d,$9d,$af,$b4,$f1,$7a,$62,$f5),
    {'abc'}
    ($f7,$ef,$28,$8c,$b1,$bb,$cc,$61,$60,$d7,$65,$07,$e0,$a3,$bb,$f7,$12,$fb,$67,$d6),
    ($6e,$4a,$fd,$50,$1f,$a6,$b4,$a1,$82,$3c,$a3,$b1,$0b,$d9,$aa,$0b,$a9,$7b,$a1,$82),
    {'message digest'}
    ($f8,$36,$62,$cc,$8d,$33,$9c,$22,$7e,$60,$0f,$cd,$63,$6c,$57,$d2,$57,$1b,$1c,$34),
    ($2e,$06,$6e,$62,$4b,$ad,$b7,$6a,$18,$4c,$8f,$90,$fb,$a0,$53,$33,$0e,$65,$0e,$92),
    {'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'}
    ($e4,$9c,$13,$6a,$9e,$56,$27,$e0,$68,$1b,$80,$8a,$3b,$97,$e6,$a6,$e6,$61,$ae,$79),
    ($f1,$be,$3e,$e8,$77,$70,$31,$40,$d3,$4f,$97,$ea,$1a,$b3,$a0,$7c,$14,$13,$33,$e2));
  data: array[0..3] of string[80] = ( '', 'abc', 'message digest',
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789');
const
  DigLen = sizeof(TRMD160Digest);
var
  i,j,k: integer;
var
  ctx  : THMAC_Context;
  mac  : THashDigest;
  phash: PHashDesc;
begin
  writeln('=========== HMAC RIPEMD-160 from Antoon Bosselaers'' web page');
  phash := FindHash_by_Name('RIPEMD160');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  for i:=0 to 3 do begin
    for k:=1 to 2 do begin
      j := 2*i+k;
      write('Test ',j);
      hmac_init(ctx, phash, @key[k], sizeof(key[k]));
      hmac_update(ctx, @data[i][1], length(data[i]));
      hmac_final(ctx, mac);
      if compmem(@mac,  @dig[j], DigLen) then writeln('  TRUE')
      else begin
        writeln(' FALSE');
        writeln(' Ref: ', HexStr(@dig[j], DigLen));
        writeln('Code: ', HexStr(@mac,  DigLen));
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA1;
  {-SHA1 test cases from RFC2202}

const
  DigLen = sizeof(TSHA1Digest);

const
  key1  : string[20] = #$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b#$b;
  dig1  : array[1..20] of byte = ($b6, $17, $31, $86, $55, $05, $72, $64, $e2, $8b,
                                  $c0, $b6, $fb, $37, $8c, $8e, $f1, $46, $be, $00);
const
  dig2  : array[1..20] of byte = ($ef, $fc, $df, $6a, $e5, $eb, $2f, $a2, $d2, $74,
                                  $16, $d5, $f1, $84, $df, $9c, $25, $9a, $7c, $79);
const
  dig6  : array[1..20] of byte = ($aa, $4a, $e5, $e1, $52, $72, $d0, $0e, $95, $70,
                                  $56, $37, $ce, $8a, $3b, $55, $ed, $40, $21, $12);
const
  dig7  : array[1..20] of byte = ($e8, $e9, $9d, $0f, $45, $23, $7d, $78, $6d, $6b,
                                  $ba, $a7, $96, $5c, $78, $08, $bb, $ff, $1a, $91);

var
  ctx  : THMAC_Context;
  mac  : THashDigest;
  phash: PHashDesc;

begin
  writeln('=========== HMAC SHA1 self test cf. RFC 2202');
  phash := FindHash_by_Name('SHA1');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  write('Test  1');
  hmac_inits(ctx, phash, key1);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig1, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig1, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  2');
  hmac_inits(ctx, phash, key2);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig2, DigLen)  then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig2, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  6');
  hmac_init(ctx, phash, @key6, sizeof(key6));
  hmac_update(ctx, @data6, sizeof(data6));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig6, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig6, DigLen));
    writeln('Code: ', HexStr(@mac , DigLen));
  end;

  write('Test  7');
  hmac_init(ctx, phash, @key7, sizeof(key7));
  hmac_update(ctx, @data7, sizeof(data7));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig7, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig7, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

end;


{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA256;
  {-SHA256 test cases from draft-ietf-ipsec-ciph-sha-256-01.txt}

var
  key1 : array[1..32] of byte;

const
  DigLen = sizeof(TSHA256Digest);

const
  data1: array[0..2] of char8  = 'abc';
  dig1 : array[0..31] of byte  = ($a2, $1b, $1f, $5d, $4c, $f4, $f7, $3a,
                                  $4d, $d9, $39, $75, $0f, $7a, $06, $6a,
                                  $7f, $98, $cc, $13, $1c, $b1, $6a, $66,
                                  $92, $75, $90, $21, $cf, $ab, $81, $81);
const
  data2: array[1..56] of char8 = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  dig2 : array[0..31] of byte  = ($10, $4f, $dc, $12, $57, $32, $8f, $08,
                                  $18, $4b, $a7, $31, $31, $c5, $3c, $ae,
                                  $e6, $98, $e3, $61, $19, $42, $11, $49,
                                  $ea, $8c, $71, $24, $56, $69, $7d, $30);

const
  data3: array[1..112] of char8
       = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqabcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';
  dig3 : array[0..31] of byte  = ($47, $03, $05, $fc, $7e, $40, $fe, $34,
                                  $d3, $ee, $b3, $e7, $73, $d9, $5a, $ab,
                                  $73, $ac, $f0, $fd, $06, $04, $47, $a5,
                                  $eb, $45, $95, $bf, $33, $a9, $d1, $a3);

const
  data5: array[1..28] of char8 = 'what do ya want for nothing?';
  dig5 : array[0..31] of byte  = ($5b, $dc, $c1, $46, $bf, $60, $75, $4e,
                                  $6a, $04, $24, $26, $08, $95, $75, $c7,
                                  $5a, $00, $3f, $08, $9d, $27, $39, $83,
                                  $9d, $ec, $58, $b9, $64, $ec, $38, $43);
const
  dig9 : array[0..31] of byte = ($69, $53, $02, $5e, $d9, $6f, $0c, $09,
                                 $f8, $0a, $96, $f7, $8e, $65, $38, $db,
                                 $e2, $e7, $b8, $20, $e3, $dd, $97, $0e,
                                 $7d, $dd, $39, $09, $1b, $32, $35, $2f);
const
  dig10: array[0..31] of byte = ($63, $55, $ac, $22, $e8, $90, $d0, $a3,
                                 $c8, $48, $1a, $5c, $a4, $82, $5b, $c8,
                                 $84, $d3, $e7, $a1, $ff, $98, $a2, $fc,
                                 $2a, $c7, $d8, $e0, $64, $c3, $b2, $e6);
var
  i: byte;

var
  ctx  : THMAC_Context;
  mac  : THashDigest;
  phash: PHashDesc;

begin
  writeln('=========== HMAC SHA256 self test cf. IETF Draft');
  phash := FindHash_by_Name('SHA256');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  write('Test  1');
  for i:=1 to 32 do key1[i] := i;
  hmac_init(ctx, phash, @key1, sizeof(key1));
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig1, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig1, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  2');
  hmac_init(ctx, phash, @key1, sizeof(key1));
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig2, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig2, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  3');
  hmac_init(ctx, phash, @key1, sizeof(key1));
  hmac_update(ctx, @data3, sizeof(data3));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig3, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig3, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  5');
  hmac_inits(ctx, phash, 'Jefe');
  hmac_update(ctx, @data5, sizeof(data5));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig5, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig5, DigLen));
    writeln('Code: ', HexStr(@mac,  DigLen));
  end;

  write('Test  9');
  hmac_init(ctx, phash, @key6, sizeof(key6));
  hmac_update(ctx, @data6, sizeof(data6));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig9, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig10, DigLen));
    writeln('Code: ', HexStr(@mac,   DigLen));
  end;

  write('Test 10');
  hmac_init(ctx, phash, @key7, sizeof(key7));
  hmac_update(ctx, @data7, sizeof(data7));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig10, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig10, DigLen));
    writeln('Code: ', HexStr(@mac,   DigLen));
  end;
end;


{---------------------------------------------------------------------------}
procedure TSD_Test;
  {-From Tom St.Denis' LibTomCrypt 1.0+, last HMAC entries from hmac_tv.txt}
const
  MinTSDH = _MD4;
  MaxTSDH = _Whirlpool;
type
  TSD_HASH = MinTSDH..MaxTSDH;
const
  digs: array[TSD_HASH] of string[128] = (
            'B4FA8DFD3AD4C88EABC8505D4901B057',  {MD4}
            '09F1503BCD00E3A1B965B66B9609E998',  {MD5}
            'AD090CC9A6B381C0B3D87035274FBC056012A4E6', {RMD160}
            '6560BD2CDE7403083527E597C60988BB1EB21FF1', {SHA1}
            '0FF4DA564729A0E9984E15BC69B00FA2E54711573BEE3AD608F511B5', {SHA224}
            '8B185702392BC1E061414539546904553A62510BC2E9E045892D64DAA6B32A76', {SHA256}
            'AA5D7EA1126BF16DA2897AE036E94D1F96875AD306B19910EFE3F17B7A98F9A4163E4032EFD17DDBF78FE3321047509C', {SHA384}
            '6E6A3CDE12F2CB3A42EC8A5D21B435C4DA4DF6CA7E41537D361D8169158287BF1D2241581DE07F88FE92F5AE4E96EB9C'  {SHA512}
               +'489FC3B258EA3842EA2D511CE883883E',
            '4AABF1C3F24C20FFAA61D6106E32EF1BB7CDEB607354BD4B6251893941730054244E198EECD4943C77082CC9B406A2E1'  {Whirlpool}
               +'2271BCA455DF15D3613336615C36B22E');
  rounds: array[TSD_HASH] of integer = (128,128,128,128,128,128,256,256,128);
label
  _continue;  {TP < 7.0}
var
  ctx  : THMAC_Context;
  mac  : THashDigest;
  tdig : THashDigest;
  phash: PHashDesc;
  key  : THashDigest;
  inbuf: array[0..255] of byte;
  algo : THashAlgorithm;
  ldig : word;
  y,z  : integer;
begin
  writeln('=========== Tests from Tom St.Denis'' LibTomCrypt 1.0+');
  for algo := MinTSDH to MaxTSDH do begin
    phash := FindHash_by_ID(algo);
    if phash=nil then begin
      writeln('Hash function not found/registered.');
      goto _continue;
    end;
    write('HMAC '+phash^.HName+': ':18);
    Hex2Mem(digs[algo],@tdig, sizeof(tdig), ldig);
    if ldig<>phash^.HDigestLen then begin
      writeln('Invalid test digest length');
      goto _continue;
    end;
    for z:=0 to ldig-1 do key[z] := z and 255;
    for y:=0 to rounds[algo] do begin
      for z:=0 to y-1 do inbuf[z] := z and 255;
      hmac_init(ctx, phash, @key, ldig);
      hmac_update(ctx, @inbuf, y);
      hmac_final(ctx, mac);
      move(mac,key,ldig);
    end;
    writeln(compmem(@key, @tdig, ldig));
_continue:
  end;
end;


{Tests for HMAC SHA512/224 and SHA512/256}

{** Note: The test cases from this program are NOT taken from public sources}
{**       at the time of writing. They should be considered preliminary and }
{**       are useful for regression testing. They are confirmed in sci.crypt}
{**       see http://groups.google.com/group/sci.crypt/msg/335e9c0cc3827bc0 }

{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA5_256;
const
  DigLen = sizeof(TSHA5_256Digest);
const
  key1 : array[0..19] of byte = ($0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b);
  key2 : array[0..3]  of byte = ($4a,$65,$66,$65);
  dig1 : TSHA5_256Digest =      ($9f,$91,$26,$c3,$d9,$c3,$c3,$30,
                                 $d7,$60,$42,$5c,$a8,$a2,$17,$e3,
                                 $1f,$ea,$e3,$1b,$fe,$70,$19,$6f,
                                 $f8,$16,$42,$b8,$68,$40,$2e,$ab);
  dig2 : TSHA5_256Digest =      ($6d,$f7,$b2,$46,$30,$d5,$cc,$b2,
                                 $ee,$33,$54,$07,$08,$1a,$87,$18,
                                 $8c,$22,$14,$89,$76,$8f,$a2,$02,
                                 $05,$13,$b2,$d5,$93,$35,$94,$56);
var
  ctx: THMAC_Context;
  mac: THashDigest;
  phash: PHashDesc;
begin
  writeln('HMAC_SHA512_256');
  phash := FindHash_by_Name('SHA512/256');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  write(' Test 1');
  hmac_init(ctx, phash, @key1, sizeof(key1));
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig1, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('  Ref: ', HexStr(@dig1, DigLen));
    writeln(' Code: ', HexStr(@mac,  DigLen));
  end;
  write(' Test 2');
  hmac_init(ctx, phash, @key2, sizeof(key2));
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig2, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('  Ref: ', HexStr(@dig2, DigLen));
    writeln(' Code: ', HexStr(@mac,  DigLen));
  end;
end;


{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA5_224;
const
  DigLen = sizeof(TSHA5_224Digest);

const
  key1 : array[0..19] of byte = ($0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b);
  key2 : array[0..3]  of byte = ($4a,$65,$66,$65);
  dig1 : TSHA5_224Digest =      ($b2,$44,$ba,$01,$30,$7c,$0e,$7a,
                                 $8c,$ca,$ad,$13,$b1,$06,$7a,$4c,
                                 $f6,$b9,$61,$fe,$0c,$6a,$20,$bd,
                                 $a3,$d9,$20,$39);
  dig2 : TSHA5_224Digest =      ($4a,$53,$0b,$31,$a7,$9e,$bc,$ce,
                                 $36,$91,$65,$46,$31,$7c,$45,$f2,
                                 $47,$d8,$32,$41,$df,$b8,$18,$fd,
                                 $37,$25,$4b,$de);
var
  ctx: THMAC_Context;
  mac: THashDigest;
  phash: PHashDesc;
begin
  writeln('HMAC_SHA512/224');
  phash := FindHash_by_Name('SHA512/224');
  if phash=nil then begin
    writeln('Hash function not found/registered.');
    exit;
  end;
  write(' Test 1');
  hmac_init(ctx, phash, @key1, sizeof(key1));
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig1, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('  Ref: ', HexStr(@dig1, DigLen));
    writeln(' Code: ', HexStr(@mac,  DigLen));
  end;
  write(' Test 2');
  hmac_init(ctx, phash, @key2, sizeof(key2));
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  if compmem(@mac,  @dig2, DigLen) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('  Ref: ', HexStr(@dig2, DigLen));
    writeln(' Code: ', HexStr(@mac,  DigLen));
  end;
end;

begin
  fillchar(key6, sizeof(key6), #$aa);
  fillchar(key7, sizeof(key7), #$aa);
  Test_HMAC_MD5;    writeln;
  Test_RMD160;      writeln;
  Test_HMAC_SHA1;   writeln;
  Test_HMAC_SHA256; writeln;
  TSD_Test;
  writeln;
  writeln('=========== Tests for SHA512/224 and SHA512/256');
  Test_HMAC_SHA5_224;
  Test_HMAC_SHA5_256;
end;
{HMac_Test_Part1}


(***************************************************************************)
{ This is the old t_hmac2                                                    }
(***************************************************************************)
procedure HMac_Test_Part2;

var
  psha224,psha256,psha384,psha512: PHashDesc;
  ctx: THMAC_Context;
  mac: THashDigest;


{---------------------------------------------------------------------------}
procedure Check1(phash: PHashDesc;
                 key: pointer; klen: word;
                 msg: pointer; mlen: word;
                 dig: pointer; dlen: word);
begin
  hmac_init(ctx,phash,key,klen);
  hmac_update(ctx, msg, mlen);
  hmac_final(ctx,mac);
  writeln(phash^.HName:8, ' ',compmem(@mac,dig,dlen));
end;


{---------------------------------------------------------------------------}
procedure Testcase1;
const
  Key : array[0.. 19] of byte = ($0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b);
const
  data: array[0..  7] of byte = ($48,$69,$20,$54,$68,$65,$72,$65);
const
  H224: array[0.. 27] of byte = ($89,$6f,$b1,$12,$8a,$bb,$df,$19,
                                 $68,$32,$10,$7c,$d4,$9d,$f3,$3f,
                                 $47,$b4,$b1,$16,$99,$12,$ba,$4f,
                                 $53,$68,$4b,$22);
const
  H256: array[0.. 31] of byte = ($b0,$34,$4c,$61,$d8,$db,$38,$53,
                                 $5c,$a8,$af,$ce,$af,$0b,$f1,$2b,
                                 $88,$1d,$c2,$00,$c9,$83,$3d,$a7,
                                 $26,$e9,$37,$6c,$2e,$32,$cf,$f7);
const
  H384: array[0.. 47] of byte = ($af,$d0,$39,$44,$d8,$48,$95,$62,
                                 $6b,$08,$25,$f4,$ab,$46,$90,$7f,
                                 $15,$f9,$da,$db,$e4,$10,$1e,$c6,
                                 $82,$aa,$03,$4c,$7c,$eb,$c5,$9c,
                                 $fa,$ea,$9e,$a9,$07,$6e,$de,$7f,
                                 $4a,$f1,$52,$e8,$b2,$fa,$9c,$b6);
const
  H512: array[0.. 63] of byte = ($87,$aa,$7c,$de,$a5,$ef,$61,$9d,
                                 $4f,$f0,$b4,$24,$1a,$1d,$6c,$b0,
                                 $23,$79,$f4,$e2,$ce,$4e,$c2,$78,
                                 $7a,$d0,$b3,$05,$45,$e1,$7c,$de,
                                 $da,$a8,$33,$b7,$d6,$b8,$a7,$02,
                                 $03,$8b,$27,$4e,$ae,$a3,$f4,$e4,
                                 $be,$9d,$91,$4e,$eb,$61,$f1,$70,
                                 $2e,$69,$6c,$20,$3a,$12,$68,$54);
begin
  writeln('Test case 1');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure Testcase2;
const
  Key : array[0..  3] of byte = ($4a,$65,$66,$65);

const
  data: array[0.. 27] of byte = ($77,$68,$61,$74,$20,$64,$6f,$20,
                                 $79,$61,$20,$77,$61,$6e,$74,$20,
                                 $66,$6f,$72,$20,$6e,$6f,$74,$68,
                                 $69,$6e,$67,$3f);

const
  H224: array[0.. 27] of byte = ($a3,$0e,$01,$09,$8b,$c6,$db,$bf,
                                 $45,$69,$0f,$3a,$7e,$9e,$6d,$0f,
                                 $8b,$be,$a2,$a3,$9e,$61,$48,$00,
                                 $8f,$d0,$5e,$44);

const
  H256: array[0.. 31] of byte = ($5b,$dc,$c1,$46,$bf,$60,$75,$4e,
                                 $6a,$04,$24,$26,$08,$95,$75,$c7,
                                 $5a,$00,$3f,$08,$9d,$27,$39,$83,
                                 $9d,$ec,$58,$b9,$64,$ec,$38,$43);

const
  H384: array[0.. 47] of byte = ($af,$45,$d2,$e3,$76,$48,$40,$31,
                                 $61,$7f,$78,$d2,$b5,$8a,$6b,$1b,
                                 $9c,$7e,$f4,$64,$f5,$a0,$1b,$47,
                                 $e4,$2e,$c3,$73,$63,$22,$44,$5e,
                                 $8e,$22,$40,$ca,$5e,$69,$e2,$c7,
                                 $8b,$32,$39,$ec,$fa,$b2,$16,$49);

const
  H512: array[0.. 63] of byte = ($16,$4b,$7a,$7b,$fc,$f8,$19,$e2,
                                 $e3,$95,$fb,$e7,$3b,$56,$e0,$a3,
                                 $87,$bd,$64,$22,$2e,$83,$1f,$d6,
                                 $10,$27,$0c,$d7,$ea,$25,$05,$54,
                                 $97,$58,$bf,$75,$c0,$5a,$99,$4a,
                                 $6d,$03,$4f,$65,$f8,$f0,$e6,$fd,
                                 $ca,$ea,$b1,$a3,$4d,$4a,$6b,$4b,
                                 $63,$6e,$07,$0a,$38,$bc,$e7,$37);
begin
  writeln('Test case 2');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure Testcase3;
const
  Key : array[0.. 19] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa);

const
  data: array[0.. 49] of byte = ($dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd);

const
  H224: array[0.. 27] of byte = ($7f,$b3,$cb,$35,$88,$c6,$c1,$f6,
                                 $ff,$a9,$69,$4d,$7d,$6a,$d2,$64,
                                 $93,$65,$b0,$c1,$f6,$5d,$69,$d1,
                                 $ec,$83,$33,$ea);

const
  H256: array[0.. 31] of byte = ($77,$3e,$a9,$1e,$36,$80,$0e,$46,
                                 $85,$4d,$b8,$eb,$d0,$91,$81,$a7,
                                 $29,$59,$09,$8b,$3e,$f8,$c1,$22,
                                 $d9,$63,$55,$14,$ce,$d5,$65,$fe);

const
  H384: array[0.. 47] of byte = ($88,$06,$26,$08,$d3,$e6,$ad,$8a,
                                 $0a,$a2,$ac,$e0,$14,$c8,$a8,$6f,
                                 $0a,$a6,$35,$d9,$47,$ac,$9f,$eb,
                                 $e8,$3e,$f4,$e5,$59,$66,$14,$4b,
                                 $2a,$5a,$b3,$9d,$c1,$38,$14,$b9,
                                 $4e,$3a,$b6,$e1,$01,$a3,$4f,$27);

const
  H512: array[0.. 63] of byte = ($fa,$73,$b0,$08,$9d,$56,$a2,$84,
                                 $ef,$b0,$f0,$75,$6c,$89,$0b,$e9,
                                 $b1,$b5,$db,$dd,$8e,$e8,$1a,$36,
                                 $55,$f8,$3e,$33,$b2,$27,$9d,$39,
                                 $bf,$3e,$84,$82,$79,$a7,$22,$c8,
                                 $06,$b4,$85,$a4,$7e,$67,$c8,$07,
                                 $b9,$46,$a3,$37,$be,$e8,$94,$26,
                                 $74,$27,$88,$59,$e1,$32,$92,$fb);
begin
  writeln('Test case 3');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure Testcase4;
const
  Key : array[0.. 24] of byte = ($01,$02,$03,$04,$05,$06,$07,$08,
                                 $09,$0a,$0b,$0c,$0d,$0e,$0f,$10,
                                 $11,$12,$13,$14,$15,$16,$17,$18,
                                 $19);

const
  data: array[0.. 49] of byte = ($cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd);

const
  H224: array[0.. 27] of byte = ($6c,$11,$50,$68,$74,$01,$3c,$ac,
                                 $6a,$2a,$bc,$1b,$b3,$82,$62,$7c,
                                 $ec,$6a,$90,$d8,$6e,$fc,$01,$2d,
                                 $e7,$af,$ec,$5a);

const
  H256: array[0.. 31] of byte = ($82,$55,$8a,$38,$9a,$44,$3c,$0e,
                                 $a4,$cc,$81,$98,$99,$f2,$08,$3a,
                                 $85,$f0,$fa,$a3,$e5,$78,$f8,$07,
                                 $7a,$2e,$3f,$f4,$67,$29,$66,$5b);

const
  H384: array[0.. 47] of byte = ($3e,$8a,$69,$b7,$78,$3c,$25,$85,
                                 $19,$33,$ab,$62,$90,$af,$6c,$a7,
                                 $7a,$99,$81,$48,$08,$50,$00,$9c,
                                 $c5,$57,$7c,$6e,$1f,$57,$3b,$4e,
                                 $68,$01,$dd,$23,$c4,$a7,$d6,$79,
                                 $cc,$f8,$a3,$86,$c6,$74,$cf,$fb);

const
  H512: array[0.. 63] of byte = ($b0,$ba,$46,$56,$37,$45,$8c,$69,
                                 $90,$e5,$a8,$c5,$f6,$1d,$4a,$f7,
                                 $e5,$76,$d9,$7f,$f9,$4b,$87,$2d,
                                 $e7,$6f,$80,$50,$36,$1e,$e3,$db,
                                 $a9,$1c,$a5,$c1,$1a,$a2,$5e,$b4,
                                 $d6,$79,$27,$5c,$c5,$78,$80,$63,
                                 $a5,$f1,$97,$41,$12,$0c,$4f,$2d,
                                 $e2,$ad,$eb,$eb,$10,$a2,$98,$dd);
begin
  writeln('Test case 4');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;



{---------------------------------------------------------------------------}
procedure Testcase5;
const
  Key : array[0.. 19] of byte = ($0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,
                                 $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,
                                 $0c,$0c,$0c,$0c);

const
  data: array[0.. 19] of byte = ($54,$65,$73,$74,$20,$57,$69,$74,
                                 $68,$20,$54,$72,$75,$6e,$63,$61,
                                 $74,$69,$6f,$6e);

const
  H224: array[0.. 15] of byte = ($0e,$2a,$ea,$68,$a9,$0c,$8d,$37,
                                 $c9,$88,$bc,$db,$9f,$ca,$6f,$a8);

const
  H256: array[0.. 15] of byte = ($a3,$b6,$16,$74,$73,$10,$0e,$e0,
                                 $6e,$0c,$79,$6c,$29,$55,$55,$2b);

const
  H384: array[0.. 15] of byte = ($3a,$bf,$34,$c3,$50,$3b,$2a,$23,
                                 $a4,$6e,$fc,$61,$9b,$ae,$f8,$97);

const
  H512: array[0.. 15] of byte = ($41,$5f,$ad,$62,$71,$58,$0a,$53,
                                 $1d,$41,$79,$bc,$89,$1d,$87,$a6);
begin
  writeln('Test case 5');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;



{---------------------------------------------------------------------------}
procedure Testcase6;
const
  Key : array[0..130] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);

const
  data: array[0.. 53] of byte = ($54,$65,$73,$74,$20,$55,$73,$69,
                                 $6e,$67,$20,$4c,$61,$72,$67,$65,
                                 $72,$20,$54,$68,$61,$6e,$20,$42,
                                 $6c,$6f,$63,$6b,$2d,$53,$69,$7a,
                                 $65,$20,$4b,$65,$79,$20,$2d,$20,
                                 $48,$61,$73,$68,$20,$4b,$65,$79,
                                 $20,$46,$69,$72,$73,$74);

const
  H224: array[0.. 27] of byte = ($95,$e9,$a0,$db,$96,$20,$95,$ad,
                                 $ae,$be,$9b,$2d,$6f,$0d,$bc,$e2,
                                 $d4,$99,$f1,$12,$f2,$d2,$b7,$27,
                                 $3f,$a6,$87,$0e);

const
  H256: array[0.. 31] of byte = ($60,$e4,$31,$59,$1e,$e0,$b6,$7f,
                                 $0d,$8a,$26,$aa,$cb,$f5,$b7,$7f,
                                 $8e,$0b,$c6,$21,$37,$28,$c5,$14,
                                 $05,$46,$04,$0f,$0e,$e3,$7f,$54);

const
  H384: array[0.. 47] of byte = ($4e,$ce,$08,$44,$85,$81,$3e,$90,
                                 $88,$d2,$c6,$3a,$04,$1b,$c5,$b4,
                                 $4f,$9e,$f1,$01,$2a,$2b,$58,$8f,
                                 $3c,$d1,$1f,$05,$03,$3a,$c4,$c6,
                                 $0c,$2e,$f6,$ab,$40,$30,$fe,$82,
                                 $96,$24,$8d,$f1,$63,$f4,$49,$52);

const
  H512: array[0.. 63] of byte = ($80,$b2,$42,$63,$c7,$c1,$a3,$eb,
                                 $b7,$14,$93,$c1,$dd,$7b,$e8,$b4,
                                 $9b,$46,$d1,$f4,$1b,$4a,$ee,$c1,
                                 $12,$1b,$01,$37,$83,$f8,$f3,$52,
                                 $6b,$56,$d0,$37,$e0,$5f,$25,$98,
                                 $bd,$0f,$d2,$21,$5d,$6a,$1e,$52,
                                 $95,$e6,$4f,$73,$f6,$3f,$0a,$ec,
                                 $8b,$91,$5a,$98,$5d,$78,$65,$98);
begin
  writeln('Test case 6');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure Testcase7;
const
  Key : array[0..130] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);

const
  data: array[0..151] of byte = ($54,$68,$69,$73,$20,$69,$73,$20,
                                 $61,$20,$74,$65,$73,$74,$20,$75,
                                 $73,$69,$6e,$67,$20,$61,$20,$6c,
                                 $61,$72,$67,$65,$72,$20,$74,$68,
                                 $61,$6e,$20,$62,$6c,$6f,$63,$6b,
                                 $2d,$73,$69,$7a,$65,$20,$6b,$65,
                                 $79,$20,$61,$6e,$64,$20,$61,$20,
                                 $6c,$61,$72,$67,$65,$72,$20,$74,
                                 $68,$61,$6e,$20,$62,$6c,$6f,$63,
                                 $6b,$2d,$73,$69,$7a,$65,$20,$64,
                                 $61,$74,$61,$2e,$20,$54,$68,$65,
                                 $20,$6b,$65,$79,$20,$6e,$65,$65,
                                 $64,$73,$20,$74,$6f,$20,$62,$65,
                                 $20,$68,$61,$73,$68,$65,$64,$20,
                                 $62,$65,$66,$6f,$72,$65,$20,$62,
                                 $65,$69,$6e,$67,$20,$75,$73,$65,
                                 $64,$20,$62,$79,$20,$74,$68,$65,
                                 $20,$48,$4d,$41,$43,$20,$61,$6c,
                                 $67,$6f,$72,$69,$74,$68,$6d,$2e);

const
  H224: array[0.. 27] of byte = ($3a,$85,$41,$66,$ac,$5d,$9f,$02,
                                 $3f,$54,$d5,$17,$d0,$b3,$9d,$bd,
                                 $94,$67,$70,$db,$9c,$2b,$95,$c9,
                                 $f6,$f5,$65,$d1);

const
  H256: array[0.. 31] of byte = ($9b,$09,$ff,$a7,$1b,$94,$2f,$cb,
                                 $27,$63,$5f,$bc,$d5,$b0,$e9,$44,
                                 $bf,$dc,$63,$64,$4f,$07,$13,$93,
                                 $8a,$7f,$51,$53,$5c,$3a,$35,$e2);

const
  H384: array[0.. 47] of byte = ($66,$17,$17,$8e,$94,$1f,$02,$0d,
                                 $35,$1e,$2f,$25,$4e,$8f,$d3,$2c,
                                 $60,$24,$20,$fe,$b0,$b8,$fb,$9a,
                                 $dc,$ce,$bb,$82,$46,$1e,$99,$c5,
                                 $a6,$78,$cc,$31,$e7,$99,$17,$6d,
                                 $38,$60,$e6,$11,$0c,$46,$52,$3e);

const
  H512: array[0.. 63] of byte = ($e3,$7b,$6a,$77,$5d,$c8,$7d,$ba,
                                 $a4,$df,$a9,$f9,$6e,$5e,$3f,$fd,
                                 $de,$bd,$71,$f8,$86,$72,$89,$86,
                                 $5d,$f5,$a3,$2d,$20,$cd,$c9,$44,
                                 $b6,$02,$2c,$ac,$3c,$49,$82,$b1,
                                 $0d,$5e,$eb,$55,$c3,$e4,$de,$15,
                                 $13,$46,$76,$fb,$6d,$e0,$44,$60,
                                 $65,$c9,$74,$40,$fa,$8c,$6a,$58);
begin
  writeln('Test case 7');
  Check1(psha224,@key,sizeof(key),@data,sizeof(data),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@data,sizeof(data),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@data,sizeof(data),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@data,sizeof(data),@H512,sizeof(H512));
end;



{---------------------------------------------------------------------------}
procedure FindHashDescriptors;
  {-Find Hash descriptors for all SHAxxx}
  procedure Find1(AlgoName: THashName; var ph: PHashDesc);
  begin
    ph :=  FindHash_by_Name(AlgoName);
    if ph=nil then begin
      writeln('Hash descriptor not found for ', AlgoName);
      halt;
    end;
  end;
begin
  Find1('sha224', psha224);
  Find1('sha256', psha256);
  Find1('sha384', psha384);
  Find1('sha512', psha512);
end;

begin
  writeln('HMAC test cases from RFC4231');
  FindHashDescriptors;
  TestCase1;
  TestCase2;
  TestCase3;
  TestCase4;
  TestCase5;
  TestCase6;
  TestCase7;
end;



(***************************************************************************)
{ This is the old t_hmsh3n                                                  }
(***************************************************************************)
procedure HMac_Test_Part3;

var
  ctx  : THMAC_Context;
  mac  : THashDigest;

  ph224: PHashDesc;
  ph256: PHashDesc;
  ph384: PHashDesc;
  ph512: PHashDesc;

{Vectors from hmac-sha3-testvectors.html are an update of David Ireland's}
{former test vectors at http://www.di-mgt.com.au/hmac_sha3_testvectors.html}

{Note that these test vectors are used by regression testing }
{because there are currently no known published other vectors}


{---------------------------------------------------------------------------}
procedure test_case_1;
const
  key : array[0.. 19] of byte = ($0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,
                                 $0b,$0b,$0b,$0b);
  data: array[0..  7] of byte = ($48,$69,$20,$54,$68,$65,$72,$65);
  d224: array[0.. 27] of byte = ($3b,$16,$54,$6b,$bc,$7b,$e2,$70,
                                 $6a,$03,$1d,$ca,$fd,$56,$37,$3d,
                                 $98,$84,$36,$76,$41,$d8,$c5,$9a,
                                 $f3,$c8,$60,$f7);
  d256: array[0.. 31] of byte = ($ba,$85,$19,$23,$10,$df,$fa,$96,
                                 $e2,$a3,$a4,$0e,$69,$77,$43,$51,
                                 $14,$0b,$b7,$18,$5e,$12,$02,$cd,
                                 $cc,$91,$75,$89,$f9,$5e,$16,$bb);
  d384: array[0.. 47] of byte = ($68,$d2,$dc,$f7,$fd,$4d,$dd,$0a,
                                 $22,$40,$c8,$a4,$37,$30,$5f,$61,
                                 $fb,$73,$34,$cf,$b5,$d0,$22,$6e,
                                 $1b,$c2,$7d,$c1,$0a,$2e,$72,$3a,
                                 $20,$d3,$70,$b4,$77,$43,$13,$0e,
                                 $26,$ac,$7e,$3d,$53,$28,$86,$bd);
  d512: array[0.. 63] of byte = ($eb,$3f,$bd,$4b,$2e,$aa,$b8,$f5,
                                 $c5,$04,$bd,$3a,$41,$46,$5a,$ac,
                                 $ec,$15,$77,$0a,$7c,$ab,$ac,$53,
                                 $1e,$48,$2f,$86,$0b,$5e,$c7,$ba,
                                 $47,$cc,$b2,$c6,$f2,$af,$ce,$8f,
                                 $88,$d2,$2b,$6d,$c6,$13,$80,$f2,
                                 $3a,$66,$8f,$d3,$88,$8b,$b8,$05,
                                 $37,$c0,$a0,$b8,$64,$07,$68,$9e);
begin
  writeln('Test case 1:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_2;
const
  key : array[0..  3] of byte = ($4a,$65,$66,$65);
  data: array[0.. 27] of byte = ($77,$68,$61,$74,$20,$64,$6f,$20,
                                 $79,$61,$20,$77,$61,$6e,$74,$20,
                                 $66,$6f,$72,$20,$6e,$6f,$74,$68,
                                 $69,$6e,$67,$3f);
  d224: array[0.. 27] of byte = ($7f,$db,$8d,$d8,$8b,$d2,$f6,$0d,
                                 $1b,$79,$86,$34,$ad,$38,$68,$11,
                                 $c2,$cf,$c8,$5b,$fa,$f5,$d5,$2b,
                                 $ba,$ce,$5e,$66);
  d256: array[0.. 31] of byte = ($c7,$d4,$07,$2e,$78,$88,$77,$ae,
                                 $35,$96,$bb,$b0,$da,$73,$b8,$87,
                                 $c9,$17,$1f,$93,$09,$5b,$29,$4a,
                                 $e8,$57,$fb,$e2,$64,$5e,$1b,$a5);
  d384: array[0.. 47] of byte = ($f1,$10,$1f,$8c,$bf,$97,$66,$fd,
                                 $67,$64,$d2,$ed,$61,$90,$3f,$21,
                                 $ca,$9b,$18,$f5,$7c,$f3,$e1,$a2,
                                 $3c,$a1,$35,$08,$a9,$32,$43,$ce,
                                 $48,$c0,$45,$dc,$00,$7f,$26,$a2,
                                 $1b,$3f,$5e,$0e,$9d,$f4,$c2,$0a);
  d512: array[0.. 63] of byte = ($5a,$4b,$fe,$ab,$61,$66,$42,$7c,
                                 $7a,$36,$47,$b7,$47,$29,$2b,$83,
                                 $84,$53,$7c,$db,$89,$af,$b3,$bf,
                                 $56,$65,$e4,$c5,$e7,$09,$35,$0b,
                                 $28,$7b,$ae,$c9,$21,$fd,$7c,$a0,
                                 $ee,$7a,$0c,$31,$d0,$22,$a9,$5e,
                                 $1f,$c9,$2b,$a9,$d7,$7d,$f8,$83,
                                 $96,$02,$75,$be,$b4,$e6,$20,$24);
begin
  writeln('Test case 2:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_3;
const
  key : array[0.. 19] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa);
  data: array[0.. 49] of byte = ($dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,
                                 $dd,$dd);
  d224: array[0.. 27] of byte = ($67,$6c,$fc,$7d,$16,$15,$36,$38,
                                 $78,$03,$90,$69,$2b,$e1,$42,$d2,
                                 $df,$7c,$e9,$24,$b9,$09,$c0,$c0,
                                 $8d,$bf,$dc,$1a);
  d256: array[0.. 31] of byte = ($84,$ec,$79,$12,$4a,$27,$10,$78,
                                 $65,$ce,$dd,$8b,$d8,$2d,$a9,$96,
                                 $5e,$5e,$d8,$c3,$7b,$0a,$c9,$80,
                                 $05,$a7,$f3,$9e,$d5,$8a,$42,$07);
  d384: array[0.. 47] of byte = ($27,$5c,$d0,$e6,$61,$bb,$8b,$15,
                                 $1c,$64,$d2,$88,$f1,$f7,$82,$fb,
                                 $91,$a8,$ab,$d5,$68,$58,$d7,$2b,
                                 $ab,$b2,$d4,$76,$f0,$45,$83,$73,
                                 $b4,$1b,$6a,$b5,$bf,$17,$4b,$ec,
                                 $42,$2e,$53,$fc,$31,$35,$ac,$6e);
  d512: array[0.. 63] of byte = ($30,$9e,$99,$f9,$ec,$07,$5e,$c6,
                                 $c6,$d4,$75,$ed,$a1,$18,$06,$87,
                                 $fc,$f1,$53,$11,$95,$80,$2a,$99,
                                 $b5,$67,$74,$49,$a8,$62,$51,$82,
                                 $85,$1c,$b3,$32,$af,$b6,$a8,$9c,
                                 $41,$13,$25,$fb,$cb,$cd,$42,$af,
                                 $cb,$7b,$6e,$5a,$ab,$7e,$a4,$2c,
                                 $66,$0f,$97,$fd,$85,$84,$bf,$03);
begin
  writeln('Test case 3:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_4;
const
  key : array[0.. 24] of byte = ($01,$02,$03,$04,$05,$06,$07,$08,
                                 $09,$0a,$0b,$0c,$0d,$0e,$0f,$10,
                                 $11,$12,$13,$14,$15,$16,$17,$18,
                                 $19);
  data: array[0.. 49] of byte = ($cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd,$cd,$cd,$cd,$cd,$cd,$cd,
                                 $cd,$cd);
  d224: array[0.. 27] of byte = ($a9,$d7,$68,$5a,$19,$c4,$e0,$db,
                                 $d9,$df,$25,$56,$cc,$8a,$7d,$2a,
                                 $77,$33,$b6,$76,$25,$ce,$59,$4c,
                                 $78,$27,$0e,$eb);
  d256: array[0.. 31] of byte = ($57,$36,$6a,$45,$e2,$30,$53,$21,
                                 $a4,$bc,$5a,$a5,$fe,$2e,$f8,$a9,
                                 $21,$f6,$af,$82,$73,$d7,$fe,$7b,
                                 $e6,$cf,$ed,$b3,$f0,$ae,$a6,$d7);
  d384: array[0.. 47] of byte = ($3a,$5d,$7a,$87,$97,$02,$c0,$86,
                                 $bc,$96,$d1,$dd,$8a,$a1,$5d,$9c,
                                 $46,$44,$6b,$95,$52,$13,$11,$c6,
                                 $06,$fd,$c4,$e3,$08,$f4,$b9,$84,
                                 $da,$2d,$0f,$94,$49,$b3,$ba,$84,
                                 $25,$ec,$7f,$b8,$c3,$1b,$c1,$36);
  d512: array[0.. 63] of byte = ($b2,$7e,$ab,$1d,$6e,$8d,$87,$46,
                                 $1c,$29,$f7,$f5,$73,$9d,$d5,$8e,
                                 $98,$aa,$35,$f8,$e8,$23,$ad,$38,
                                 $c5,$49,$2a,$20,$88,$fa,$02,$81,
                                 $99,$3b,$bf,$ff,$9a,$0e,$9c,$6b,
                                 $f1,$21,$ae,$9e,$c9,$bb,$09,$d8,
                                 $4a,$5e,$ba,$c8,$17,$18,$2e,$a9,
                                 $74,$67,$3f,$b1,$33,$ca,$0d,$1d);
begin
  writeln('Test case 4:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_5;
const
  key : array[0.. 19] of byte = ($0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,
                                 $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,
                                 $0c,$0c,$0c,$0c);
  data: array[0.. 19] of byte = ($54,$65,$73,$74,$20,$57,$69,$74,
                                 $68,$20,$54,$72,$75,$6e,$63,$61,
                                 $74,$69,$6f,$6e);
  d224: array[0.. 15] of byte = ($49,$fd,$d3,$ab,$d0,$05,$eb,$b8,
                                 $ae,$63,$fe,$a9,$46,$d1,$88,$3c);
  d256: array[0.. 15] of byte = ($6e,$02,$c6,$45,$37,$fb,$11,$80,
                                 $57,$ab,$b7,$fb,$66,$a2,$3b,$3c);
  d384: array[0.. 15] of byte = ($47,$c5,$1a,$ce,$1f,$fa,$cf,$fd,
                                 $74,$94,$72,$46,$82,$61,$57,$83);
  d512: array[0.. 15] of byte = ($0f,$a7,$47,$59,$48,$f4,$3f,$48,
                                 $ca,$05,$16,$67,$1e,$18,$97,$8c);
begin
  writeln('Test case 5:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_6;
const
  key : array[0..130] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);
  data: array[0.. 53] of byte = ($54,$65,$73,$74,$20,$55,$73,$69,
                                 $6e,$67,$20,$4c,$61,$72,$67,$65,
                                 $72,$20,$54,$68,$61,$6e,$20,$42,
                                 $6c,$6f,$63,$6b,$2d,$53,$69,$7a,
                                 $65,$20,$4b,$65,$79,$20,$2d,$20,
                                 $48,$61,$73,$68,$20,$4b,$65,$79,
                                 $20,$46,$69,$72,$73,$74);
  d224: array[0.. 27] of byte = ($b4,$a1,$f0,$4c,$00,$28,$7a,$9b,
                                 $7f,$60,$75,$b3,$13,$d2,$79,$b8,
                                 $33,$bc,$8f,$75,$12,$43,$52,$d0,
                                 $5f,$b9,$99,$5f);
  d256: array[0.. 31] of byte = ($ed,$73,$a3,$74,$b9,$6c,$00,$52,
                                 $35,$f9,$48,$03,$2f,$09,$67,$4a,
                                 $58,$c0,$ce,$55,$5c,$fc,$1f,$22,
                                 $3b,$02,$35,$65,$60,$31,$2c,$3b);
  d384: array[0.. 47] of byte = ($0f,$c1,$95,$13,$bf,$6b,$d8,$78,
                                 $03,$70,$16,$70,$6a,$0e,$57,$bc,
                                 $52,$81,$39,$83,$6b,$9a,$42,$c3,
                                 $d4,$19,$e4,$98,$e0,$e1,$fb,$96,
                                 $16,$fd,$66,$91,$38,$d3,$3a,$11,
                                 $05,$e0,$7c,$72,$b6,$95,$3b,$cc);
  d512: array[0.. 63] of byte = ($00,$f7,$51,$a9,$e5,$06,$95,$b0,
                                 $90,$ed,$69,$11,$a4,$b6,$55,$24,
                                 $95,$1c,$dc,$15,$a7,$3a,$5d,$58,
                                 $bb,$55,$21,$5e,$a2,$cd,$83,$9a,
                                 $c7,$9d,$2b,$44,$a3,$9b,$af,$ab,
                                 $27,$e8,$3f,$de,$9e,$11,$f6,$34,
                                 $0b,$11,$d9,$91,$b1,$b9,$1b,$f2,
                                 $ee,$e7,$fc,$87,$24,$26,$c3,$a4);
begin
  writeln('Test case 6:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_6a;
const
  key : array[0..146] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);
  data: array[0.. 53] of byte = ($54,$65,$73,$74,$20,$55,$73,$69,
                                 $6e,$67,$20,$4c,$61,$72,$67,$65,
                                 $72,$20,$54,$68,$61,$6e,$20,$42,
                                 $6c,$6f,$63,$6b,$2d,$53,$69,$7a,
                                 $65,$20,$4b,$65,$79,$20,$2d,$20,
                                 $48,$61,$73,$68,$20,$4b,$65,$79,
                                 $20,$46,$69,$72,$73,$74);
  d224: array[0.. 27] of byte = ($b9,$6d,$73,$0c,$14,$8c,$2d,$aa,
                                 $d8,$64,$9d,$83,$de,$fa,$a3,$71,
                                 $97,$38,$d3,$47,$75,$39,$7b,$75,
                                 $71,$c3,$85,$15);
  d256: array[0.. 31] of byte = ($a6,$07,$2f,$86,$de,$52,$b3,$8b,
                                 $b3,$49,$fe,$84,$cd,$6d,$97,$fb,
                                 $6a,$37,$c4,$c0,$f6,$2a,$ae,$93,
                                 $98,$11,$93,$a7,$22,$9d,$34,$67);
  d384: array[0.. 47] of byte = ($71,$3d,$ff,$03,$02,$c8,$50,$86,
                                 $ec,$5a,$d0,$76,$8d,$d6,$5a,$13,
                                 $dd,$d7,$90,$68,$d8,$d4,$c6,$21,
                                 $2b,$71,$2e,$41,$64,$94,$49,$11,
                                 $14,$80,$23,$00,$44,$18,$5a,$99,
                                 $10,$3e,$d8,$20,$04,$dd,$bf,$cc);
  d512: array[0.. 63] of byte = ($b1,$48,$35,$c8,$19,$a2,$90,$ef,
                                 $b0,$10,$ac,$e6,$d8,$56,$8d,$c6,
                                 $b8,$4d,$e6,$0b,$c4,$9b,$00,$4c,
                                 $3b,$13,$ed,$a7,$63,$58,$94,$51,
                                 $e5,$dd,$74,$29,$28,$84,$d1,$bd,
                                 $ce,$64,$e6,$b9,$19,$dd,$61,$dc,
                                 $9c,$56,$a2,$82,$a8,$1c,$0b,$d1,
                                 $4f,$1f,$36,$5b,$49,$b8,$3a,$5b);
begin
  writeln('Test case 6a:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_7;
const
  key : array[0..130] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);
  data: array[0..151] of byte = ($54,$68,$69,$73,$20,$69,$73,$20,
                                 $61,$20,$74,$65,$73,$74,$20,$75,
                                 $73,$69,$6e,$67,$20,$61,$20,$6c,
                                 $61,$72,$67,$65,$72,$20,$74,$68,
                                 $61,$6e,$20,$62,$6c,$6f,$63,$6b,
                                 $2d,$73,$69,$7a,$65,$20,$6b,$65,
                                 $79,$20,$61,$6e,$64,$20,$61,$20,
                                 $6c,$61,$72,$67,$65,$72,$20,$74,
                                 $68,$61,$6e,$20,$62,$6c,$6f,$63,
                                 $6b,$2d,$73,$69,$7a,$65,$20,$64,
                                 $61,$74,$61,$2e,$20,$54,$68,$65,
                                 $20,$6b,$65,$79,$20,$6e,$65,$65,
                                 $64,$73,$20,$74,$6f,$20,$62,$65,
                                 $20,$68,$61,$73,$68,$65,$64,$20,
                                 $62,$65,$66,$6f,$72,$65,$20,$62,
                                 $65,$69,$6e,$67,$20,$75,$73,$65,
                                 $64,$20,$62,$79,$20,$74,$68,$65,
                                 $20,$48,$4d,$41,$43,$20,$61,$6c,
                                 $67,$6f,$72,$69,$74,$68,$6d,$2e);
  d224: array[0.. 27] of byte = ($05,$d8,$cd,$6d,$00,$fa,$ea,$8d,
                                 $1e,$b6,$8a,$de,$28,$73,$0b,$bd,
                                 $3c,$ba,$b6,$92,$9f,$0a,$08,$6b,
                                 $29,$cd,$62,$a0);
  d256: array[0.. 31] of byte = ($65,$c5,$b0,$6d,$4c,$3d,$e3,$2a,
                                 $7a,$ef,$87,$63,$26,$1e,$49,$ad,
                                 $b6,$e2,$29,$3e,$c8,$e7,$c6,$1e,
                                 $8d,$e6,$17,$01,$fc,$63,$e1,$23);
  d384: array[0.. 47] of byte = ($02,$6f,$df,$6b,$50,$74,$1e,$37,
                                 $38,$99,$c9,$f7,$d5,$40,$6d,$4e,
                                 $b0,$9f,$c6,$66,$56,$36,$fc,$1a,
                                 $53,$00,$29,$dd,$f5,$cf,$3c,$a5,
                                 $a9,$00,$ed,$ce,$01,$f5,$f6,$1e,
                                 $2f,$40,$8c,$df,$2f,$d3,$e7,$e8);
  d512: array[0.. 63] of byte = ($38,$a4,$56,$a0,$04,$bd,$10,$d3,
                                 $2c,$9a,$b8,$33,$66,$84,$11,$28,
                                 $62,$c3,$db,$61,$ad,$cc,$a3,$18,
                                 $29,$35,$5e,$af,$46,$fd,$5c,$73,
                                 $d0,$6a,$1f,$0d,$13,$fe,$c9,$a6,
                                 $52,$fb,$38,$11,$b5,$77,$b1,$b1,
                                 $d1,$b9,$78,$9f,$97,$ae,$5b,$83,
                                 $c6,$f4,$4d,$fc,$f1,$d6,$7e,$ba);
begin
  writeln('Test case 7:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_7a;
const
  key : array[0..146] of byte = ($aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,
                                 $aa,$aa,$aa);
  data: array[0..151] of byte = ($54,$68,$69,$73,$20,$69,$73,$20,
                                 $61,$20,$74,$65,$73,$74,$20,$75,
                                 $73,$69,$6e,$67,$20,$61,$20,$6c,
                                 $61,$72,$67,$65,$72,$20,$74,$68,
                                 $61,$6e,$20,$62,$6c,$6f,$63,$6b,
                                 $2d,$73,$69,$7a,$65,$20,$6b,$65,
                                 $79,$20,$61,$6e,$64,$20,$61,$20,
                                 $6c,$61,$72,$67,$65,$72,$20,$74,
                                 $68,$61,$6e,$20,$62,$6c,$6f,$63,
                                 $6b,$2d,$73,$69,$7a,$65,$20,$64,
                                 $61,$74,$61,$2e,$20,$54,$68,$65,
                                 $20,$6b,$65,$79,$20,$6e,$65,$65,
                                 $64,$73,$20,$74,$6f,$20,$62,$65,
                                 $20,$68,$61,$73,$68,$65,$64,$20,
                                 $62,$65,$66,$6f,$72,$65,$20,$62,
                                 $65,$69,$6e,$67,$20,$75,$73,$65,
                                 $64,$20,$62,$79,$20,$74,$68,$65,
                                 $20,$48,$4d,$41,$43,$20,$61,$6c,
                                 $67,$6f,$72,$69,$74,$68,$6d,$2e);
  d224: array[0.. 27] of byte = ($c7,$9c,$9b,$09,$34,$24,$e5,$88,
                                 $a9,$87,$8b,$bc,$b0,$89,$e0,$18,
                                 $27,$00,$96,$e9,$b4,$b1,$a9,$e8,
                                 $22,$0c,$86,$6a);
  d256: array[0.. 31] of byte = ($e6,$a3,$6d,$9b,$91,$5f,$86,$a0,
                                 $93,$ca,$c7,$d1,$10,$e9,$e0,$4c,
                                 $f1,$d6,$10,$0d,$30,$47,$55,$09,
                                 $c2,$47,$5f,$57,$1b,$75,$8b,$5a);
  d384: array[0.. 47] of byte = ($ca,$d1,$8a,$8f,$f6,$c4,$cc,$3a,
                                 $d4,$87,$b9,$5f,$97,$69,$e9,$b6,
                                 $1c,$06,$2a,$ef,$d6,$95,$25,$69,
                                 $e6,$e6,$42,$18,$97,$05,$4c,$fc,
                                 $70,$b5,$fd,$c6,$60,$5c,$18,$45,
                                 $71,$12,$fc,$6a,$aa,$d4,$55,$85);
  d512: array[0.. 63] of byte = ($dc,$03,$0e,$e7,$88,$70,$34,$f3,
                                 $2c,$f4,$02,$df,$34,$62,$2f,$31,
                                 $1f,$3e,$6c,$f0,$48,$60,$c6,$bb,
                                 $d7,$fa,$48,$86,$74,$78,$2b,$46,
                                 $59,$fd,$bd,$f3,$fd,$87,$78,$52,
                                 $88,$5c,$fe,$6e,$22,$18,$5f,$e7,
                                 $b2,$ee,$95,$20,$43,$62,$9b,$c9,
                                 $d5,$f3,$29,$8a,$41,$d0,$2c,$66);
begin
  writeln('Test case 7a:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final(ctx, mac);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{---------------------------------------------------------------------------}
procedure test_case_8;
const
  key : array[0..  3] of byte = ($4a,$65,$66,$65);
  data: array[0..  0] of byte = ($c8);  {Five bits '11001' from SHA3: LSB=$13, MSB=$c8}

  d224: array[0.. 27] of byte = ($5f,$8c,$0e,$a7,$fa,$fe,$cd,$0c,
                                 $34,$63,$aa,$d0,$97,$42,$ce,$ce,
                                 $b1,$42,$fe,$0a,$b6,$f4,$53,$94,
                                 $38,$c5,$9d,$e8);


  d256: array[0.. 31] of byte = ($ec,$82,$22,$77,$3f,$ac,$68,$b3,
                                 $d3,$dc,$b1,$82,$ae,$c8,$b0,$50,
                                 $7a,$ce,$44,$48,$d2,$0a,$11,$47,
                                 $e6,$82,$11,$8d,$a4,$e3,$f4,$4c);


  d384: array[0.. 47] of byte = ($21,$fb,$d3,$bf,$3e,$bb,$a3,$cf,
                                 $c9,$ef,$64,$c0,$59,$1c,$92,$c5,
                                 $ac,$b2,$65,$e9,$2d,$87,$61,$d1,
                                 $f9,$1a,$52,$a1,$03,$a6,$c7,$96,
                                 $94,$cf,$d6,$7a,$9a,$2a,$c1,$32,
                                 $4f,$02,$fe,$a6,$3b,$81,$ef,$fc);


  d512: array[0.. 63] of byte = ($27,$f9,$38,$8c,$15,$67,$ef,$4e,
                                 $f2,$00,$60,$2a,$6c,$f8,$71,$d6,
                                 $8a,$6f,$b0,$48,$d4,$73,$7a,$c4,
                                 $41,$8a,$2f,$02,$12,$89,$d1,$3d,
                                 $1f,$d1,$12,$0f,$ec,$b9,$cf,$96,
                                 $4c,$5b,$11,$7a,$b5,$b1,$1c,$61,
                                 $4b,$2d,$a3,$9d,$ad,$d5,$1f,$2f,
                                 $5e,$22,$aa,$cc,$ec,$7d,$57,$6e);
begin
  writeln('Test case 8:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


{ NIST test, vectors from
  =======================
  http://csrc.nist.gov/groups/ST/toolkit/examples.html#aMsgAuth
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/HMAC_SHA3-224.pdf
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/HMAC_SHA3-256.pdf
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/HMAC_SHA3-384.pdf
  http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/HMAC_SHA3-512.pdf
}

const
  nkey: array[0..171] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
                                 $20,$21,$22,$23,$24,$25,$26,$27,
                                 $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
                                 $30,$31,$32,$33,$34,$35,$36,$37,
                                 $38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
                                 $40,$41,$42,$43,$44,$45,$46,$47,
                                 $48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
                                 $50,$51,$52,$53,$54,$55,$56,$57,
                                 $58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
                                 $60,$61,$62,$63,$64,$65,$66,$67,
                                 $68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
                                 $70,$71,$72,$73,$74,$75,$76,$77,
                                 $78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
                                 $80,$81,$82,$83,$84,$85,$86,$87,
                                 $88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
                                 $90,$91,$92,$93,$94,$95,$96,$97,
                                 $98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
                                 $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,
                                 $a8,$a9,$aa,$ab);
const
  data1: array[0..33] of byte = ($53,$61,$6d,$70,$6c,$65,$20,$6d,
                                 $65,$73,$73,$61,$67,$65,$20,$66,
                                 $6f,$72,$20,$6b,$65,$79,$6c,$65,
                                 $6e,$3c,$62,$6c,$6f,$63,$6b,$6c,
                                 $65,$6e);
  data2: array[0..33] of byte = ($53,$61,$6d,$70,$6c,$65,$20,$6d,
                                 $65,$73,$73,$61,$67,$65,$20,$66,
                                 $6f,$72,$20,$6b,$65,$79,$6c,$65,
                                 $6e,$3d,$62,$6c,$6f,$63,$6b,$6c,
                                 $65,$6e);

  data3: array[0..33] of byte = ($53,$61,$6d,$70,$6c,$65,$20,$6d,
                                 $65,$73,$73,$61,$67,$65,$20,$66,
                                 $6f,$72,$20,$6b,$65,$79,$6c,$65,
                                 $6e,$3e,$62,$6c,$6f,$63,$6b,$6c,
                                 $65,$6e);

  data4: array[0..53] of byte = ($53,$61,$6d,$70,$6c,$65,$20,$6d,
                                 $65,$73,$73,$61,$67,$65,$20,$66,
                                 $6f,$72,$20,$6b,$65,$79,$6c,$65,
                                 $6e,$3c,$62,$6c,$6f,$63,$6b,$6c,
                                 $65,$6e,$2c,$20,$77,$69,$74,$68,
                                 $20,$74,$72,$75,$6e,$63,$61,$74,
                                 $65,$64,$20,$74,$61,$67);



{---------------------------------------------------------------------------}
procedure test_n224;
const
  mac1: array[0..27] of byte = ($33,$2c,$fd,$59,$34,$7f,$db,$8e,
                                $57,$6e,$77,$26,$0b,$e4,$ab,$a2,
                                $d6,$dc,$53,$11,$7b,$3b,$fb,$52,
                                $c6,$d1,$8c,$04);
  mac2: array[0..27] of byte = ($d8,$b7,$33,$bc,$f6,$6c,$64,$4a,
                                $12,$32,$3d,$56,$4e,$24,$dc,$f3,
                                $fc,$75,$f2,$31,$f3,$b6,$79,$68,
                                $35,$91,$00,$c7);
  mac3: array[0..27] of byte = ($07,$86,$95,$ee,$cc,$22,$7c,$63,
                                $6a,$d3,$1d,$06,$3a,$15,$dd,$05,
                                $a7,$e8,$19,$a6,$6e,$c6,$d8,$de,
                                $1e,$19,$3e,$59);
  mac4: array[0..13] of byte = ($85,$69,$c5,$4c,$bb,$00,$a9,$b7,
                                $8f,$f1,$b3,$91,$b0,$e5);
begin
  writeln('NIST test SHA3-224:');

  hmac_init(ctx, ph224, @nkey, 28);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  writeln('  Test case 1: ', compmem(@mac, @mac1, sizeof(mac1)));

  hmac_init(ctx, ph224, @nkey, 144);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  writeln('  Test case 2: ', compmem(@mac, @mac2, sizeof(mac2)));

  hmac_init(ctx, ph224, @nkey, 172);
  hmac_update(ctx, @data3, sizeof(data3));
  hmac_final(ctx, mac);
  writeln('  Test case 3: ', compmem(@mac, @mac3, sizeof(mac3)));

  hmac_init(ctx, ph224, @nkey, 28);
  hmac_update(ctx, @data4, sizeof(data4));
  hmac_final(ctx, mac);
  writeln('  Test case 4: ', compmem(@mac, @mac4, sizeof(mac4)));
end;


{---------------------------------------------------------------------------}
procedure test_n256;
const
  mac1: array[0..31] of byte = ($4f,$e8,$e2,$02,$c4,$f0,$58,$e8,
                                $dd,$dc,$23,$d8,$c3,$4e,$46,$73,
                                $43,$e2,$35,$55,$e2,$4f,$c2,$f0,
                                $25,$d5,$98,$f5,$58,$f6,$72,$05);
  mac2: array[0..31] of byte = ($68,$b9,$4e,$2e,$53,$8a,$9b,$e4,
                                $10,$3b,$eb,$b5,$aa,$01,$6d,$47,
                                $96,$1d,$4d,$1a,$a9,$06,$06,$13,
                                $13,$b5,$57,$f8,$af,$2c,$3f,$aa);
  mac3: array[0..31] of byte = ($9b,$cf,$2c,$23,$8e,$23,$5c,$3c,
                                $e8,$84,$04,$e8,$13,$bd,$2f,$3a,
                                $97,$18,$5a,$c6,$f2,$38,$c6,$3d,
                                $62,$29,$a0,$0b,$07,$97,$42,$58);
  mac4: array[0..15] of byte = ($c8,$dc,$71,$48,$d8,$c1,$42,$3a,
                                $a5,$49,$10,$5d,$af,$df,$9c,$ad);
begin
  writeln('NIST test SHA3-256:');

  hmac_init(ctx, ph256, @nkey, 32);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  writeln('  Test case 1: ', compmem(@mac, @mac1, sizeof(mac1)));

  hmac_init(ctx, ph256, @nkey, 136);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  writeln('  Test case 2: ', compmem(@mac, @mac2, sizeof(mac2)));

  hmac_init(ctx, ph256, @nkey, 168);
  hmac_update(ctx, @data3, sizeof(data3));
  hmac_final(ctx, mac);
  writeln('  Test case 3: ', compmem(@mac, @mac3, sizeof(mac3)));

  hmac_init(ctx, ph256, @nkey, 32);
  hmac_update(ctx, @data4, sizeof(data4));
  hmac_final(ctx, mac);
  writeln('  Test case 4: ', compmem(@mac, @mac4, sizeof(mac4)));
end;


{---------------------------------------------------------------------------}
procedure test_n384;
const
  mac1: array[0..47] of byte = ($d5,$88,$a3,$c5,$1f,$3f,$2d,$90,
                                $6e,$82,$98,$c1,$19,$9a,$a8,$ff,
                                $62,$96,$21,$81,$27,$f6,$b3,$8a,
                                $90,$b6,$af,$e2,$c5,$61,$77,$25,
                                $bc,$99,$98,$7f,$79,$b2,$2a,$55,
                                $7b,$65,$20,$db,$71,$0b,$7f,$42);
  mac2: array[0..47] of byte = ($a2,$7d,$24,$b5,$92,$e8,$c8,$cb,
                                $f6,$d4,$ce,$6f,$c5,$bf,$62,$d8,
                                $fc,$98,$bf,$2d,$48,$66,$40,$d9,
                                $eb,$80,$99,$e2,$40,$47,$83,$7f,
                                $5f,$3b,$ff,$be,$92,$dc,$ce,$90,
                                $b4,$ed,$5b,$1e,$7e,$44,$fa,$90);
  mac3: array[0..47] of byte = ($e5,$ae,$4c,$73,$9f,$45,$52,$79,
                                $36,$8e,$bf,$36,$d4,$f5,$35,$4c,
                                $95,$aa,$18,$4c,$89,$9d,$38,$70,
                                $e4,$60,$eb,$c2,$88,$ef,$1f,$94,
                                $70,$05,$3f,$73,$f7,$c6,$da,$2a,
                                $71,$bc,$ae,$c3,$8c,$e7,$d6,$ac);
  mac4: array[0..23] of byte = ($25,$f4,$bf,$53,$60,$6e,$91,$af,
                                $79,$d2,$4a,$4b,$b1,$fd,$6a,$ec,
                                $d4,$44,$14,$a3,$0c,$8e,$bb,$0a);
begin
  writeln('NIST test SHA3-384:');

  hmac_init(ctx, ph384, @nkey, 48);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  writeln('  Test case 1: ', compmem(@mac, @mac1, sizeof(mac1)));

  hmac_init(ctx, ph384, @nkey, 104);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  writeln('  Test case 2: ', compmem(@mac, @mac2, sizeof(mac2)));

  hmac_init(ctx, ph384, @nkey, 152);
  hmac_update(ctx, @data3, sizeof(data3));
  hmac_final(ctx, mac);
  writeln('  Test case 3: ', compmem(@mac, @mac3, sizeof(mac3)));

  hmac_init(ctx, ph384, @nkey, 48);
  hmac_update(ctx, @data4, sizeof(data4));
  hmac_final(ctx, mac);
  writeln('  Test case 4: ', compmem(@mac, @mac4, sizeof(mac4)));
end;


{---------------------------------------------------------------------------}
procedure test_n512;
const
  mac1: array[0..63] of byte = ($4e,$fd,$62,$9d,$6c,$71,$bf,$86,
                                $16,$26,$58,$f2,$99,$43,$b1,$c3,
                                $08,$ce,$27,$cd,$fa,$6d,$b0,$d9,
                                $c3,$ce,$81,$76,$3f,$9c,$bc,$e5,
                                $f7,$eb,$e9,$86,$80,$31,$db,$1a,
                                $8f,$8e,$b7,$b6,$b9,$5e,$5c,$5e,
                                $3f,$65,$7a,$89,$96,$c8,$6a,$2f,
                                $65,$27,$e3,$07,$f0,$21,$31,$96);
  mac2: array[0..63] of byte = ($54,$4e,$25,$7e,$a2,$a3,$e5,$ea,
                                $19,$a5,$90,$e6,$a2,$4b,$72,$4c,
                                $e6,$32,$77,$57,$72,$3f,$e2,$75,
                                $1b,$75,$bf,$00,$7d,$80,$f6,$b3,
                                $60,$74,$4b,$f1,$b7,$a8,$8e,$a5,
                                $85,$f9,$76,$5b,$47,$91,$19,$76,
                                $d3,$19,$1c,$f8,$3c,$03,$9f,$5f,
                                $fa,$b0,$d2,$9c,$c9,$d9,$b6,$da);
  mac3: array[0..63] of byte = ($5f,$46,$4f,$5e,$5b,$78,$48,$e3,
                                $88,$5e,$49,$b2,$c3,$85,$f0,$69,
                                $49,$85,$d0,$e3,$89,$66,$24,$2d,
                                $c4,$a5,$fe,$3f,$ea,$4b,$37,$d4,
                                $6b,$65,$ce,$ce,$d5,$dc,$f5,$94,
                                $38,$dd,$84,$0b,$ab,$22,$26,$9f,
                                $0b,$a7,$fe,$bd,$b9,$fc,$f7,$46,
                                $02,$a3,$56,$66,$b2,$a3,$29,$15);
  mac4: array[0..31] of byte = ($7b,$b0,$6d,$85,$92,$57,$b2,$5c,
                                $e7,$3c,$a7,$00,$df,$34,$c5,$cb,
                                $ef,$5c,$89,$8b,$ac,$91,$02,$9e,
                                $0b,$27,$97,$5d,$4e,$52,$6a,$08);
begin
  writeln('NIST test SHA3-512:');

  hmac_init(ctx, ph512, @nkey, 64);
  hmac_update(ctx, @data1, sizeof(data1));
  hmac_final(ctx, mac);
  writeln('  Test case 1: ', compmem(@mac, @mac1, sizeof(mac1)));

  hmac_init(ctx, ph512, @nkey, 72);
  hmac_update(ctx, @data2, sizeof(data2));
  hmac_final(ctx, mac);
  writeln('  Test case 2: ', compmem(@mac, @mac2, sizeof(mac2)));

  hmac_init(ctx, ph512, @nkey, 136);
  hmac_update(ctx, @data3, sizeof(data3));
  hmac_final(ctx, mac);
  writeln('  Test case 3: ', compmem(@mac, @mac3, sizeof(mac3)));

  hmac_init(ctx, ph512, @nkey, 64);
  hmac_update(ctx, @data4, sizeof(data4));
  hmac_final(ctx, mac);
  writeln('  Test case 4: ', compmem(@mac, @mac4, sizeof(mac4)));
end;


var
  anyerror: boolean;
begin
  writeln('HMAC-SHA3 tests');
  anyerror := false;

  ph224 := FindHash_by_ID(_SHA3_224);
  if ph224=nil then begin
    writeln('FindHash_by_ID error SHA3_224');
    anyerror := true;
  end;

  ph256 := FindHash_by_ID(_SHA3_256);
  if ph256=nil then begin
    writeln('FindHash_by_ID error SHA3_256');
    anyerror := true;
  end;

  ph384 := FindHash_by_ID(_SHA3_384);
  if ph384=nil then begin
    writeln('FindHash_by_ID error SHA3_384');
    anyerror := true;
  end;

  ph512 := FindHash_by_ID(_SHA3_512);
  if ph512=nil then begin
    writeln('FindHash_by_ID error SHA3_512');
    anyerror := true;
  end;

  if not anyerror then begin
    test_case_1;
    test_case_2;
    test_case_3;
    test_case_4;
    test_case_5;
    test_case_6;
    test_case_6a;
    test_case_7;
    test_case_7a;
    test_case_8;
    test_n224;
    test_n256;
    test_n384;
    test_n512;
  end;
end;


(***************************************************************************)
{ This is the old t_hmac3                                                    }
(***************************************************************************)
procedure HMac_Test_Part4;
var
  psha1,prmd160,psha224,psha256,psha384,psha512: PHashDesc;
  ctx: THMAC_Context;
  mac: THashDigest;


{---------------------------------------------------------------------------}
procedure Check1(phash: PHashDesc;
                 key: pointer; klen: word;
                 dig: pointer; dlen: word);
begin
  hmac_init(ctx,phash,key,klen);
  hmac_final_bits(ctx,mac,0,1);
  writeln(phash^.HName:10, ' ',compmem(@mac,dig,dlen));
end;


{[1]: NESSIE-TV}
{[2]: ShaTest RFC4634}

{---------------------------------------------------------------------------}
procedure Testcase1;
const
  Key : array[0..63] of byte = ($00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
                                $01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
                                $01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff);

const {from [1]}
  HRMD: array[0..19] of byte = ($58,$87,$ca,$80,$ec,$dd,$c6,$9b,
                                $d7,$6c,$4c,$94,$06,$75,$76,$66,
                                $e0,$b2,$83,$6d);

const {from [1] and [2]}
  HSHA: array[0..19] of byte = ($f8,$a7,$f9,$1b,$dc,$3e,$83,$d8,
                                $9b,$32,$ee,$6c,$5e,$a4,$5e,$b2,
                                $eb,$5b,$b1,$23);

const {from [2]}
  H224: array[0..27] of byte = ($65,$f1,$6f,$00,$1f,$e0,$48,$5c,
                                $2a,$7d,$93,$22,$72,$39,$41,$85,
                                $d2,$93,$30,$70,$c7,$4a,$b7,$4f,
                                $96,$fc,$69,$b8);

const {from [1] and [2]}
  H256: array[0..31] of byte = ($ca,$61,$f4,$b7,$45,$80,$5a,$32,
                                $94,$bd,$45,$38,$2e,$d2,$a6,$68,
                                $a2,$7e,$96,$97,$d5,$a9,$5c,$36,
                                $71,$ff,$45,$88,$3c,$6f,$fd,$ac);

const {from [2], values from [1] are different}
  H384: array[0..47] of byte = ($ed,$e9,$4e,$99,$b2,$af,$89,$ee,
                                $fb,$ac,$1b,$f1,$d4,$4d,$c3,$23,
                                $09,$c1,$a0,$d5,$a4,$0c,$c9,$a7,
                                $e3,$35,$28,$0c,$43,$14,$98,$fa,
                                $f0,$8b,$82,$e7,$65,$86,$80,$55,
                                $c8,$97,$e8,$47,$84,$53,$8a,$ae);

const {from [2], values from [1] are different}
  H512: array[0..63] of byte = ($81,$26,$b3,$5a,$9e,$f3,$e2,$71,
                                $01,$83,$9a,$84,$19,$18,$dd,$84,
                                $67,$17,$88,$e9,$d7,$10,$d2,$f7,
                                $5f,$ab,$75,$b9,$eb,$51,$c5,$4e,
                                $83,$50,$e6,$28,$ac,$9a,$e5,$f4,
                                $de,$18,$45,$c9,$74,$76,$93,$07,
                                $82,$14,$3d,$f9,$9b,$f1,$7e,$2d,
                                $cc,$e0,$e3,$b0,$56,$f4,$7a,$d6);

begin
  writeln('Test case 1');
  Check1(psha1,  @key,sizeof(key),@HSHA,sizeof(HSHA));
  Check1(prmd160,@key,sizeof(key),@HRMD,sizeof(HRMD));
  Check1(psha224,@key,sizeof(key),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure FindHashDescriptors;
  {-Find Hash descriptors for all SHAxxx}
  procedure Find1(AlgoName: THashName; var ph: PHashDesc);
  begin
    ph :=  FindHash_by_Name(AlgoName);
    if ph=nil then begin
      writeln('Hash descriptor not found for ', AlgoName);
      writeln('May be unit is not listed in uses statement');
      halt;
    end;
  end;
begin
  Find1('sha1',      psha1);
  Find1('ripemd160', prmd160);
  Find1('sha224',    psha224);
  Find1('sha256',    psha256);
  Find1('sha384',    psha384);
  Find1('sha512',    psha512);
end;


begin
  writeln('HMAC test cases for Bit-API');
  FindHashDescriptors;
  TestCase1;
end;


begin
  {$ifdef WINCRT}
    ScreenSize.Y := 50;  {D1: 50 lines screen}
  {$endif}
  writeln('HMAC tests  -  (c) 2012-2017 W. Ehrhardt');

  writeln;
  writeln('**************************************************************');
  HMac_Test_Part1;

  writeln;
  writeln('**************************************************************');
  HMac_Test_Part2;

  writeln;
  writeln('**************************************************************');
  HMac_Test_Part3;

  writeln;
  writeln('**************************************************************');
  HMac_Test_Part4;
end.




