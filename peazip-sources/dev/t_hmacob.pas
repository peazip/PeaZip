{-Test prog for 'obsolete' HMAC units}

program t_hmac;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  hash,hmac, hmacsha5, hmacsha2, hmacsha1, hmac_md5, hmacwhir,
  BTypes, mem_util;


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
  ctx: THMAC_Context;
  mac: TMD5Digest;

begin
  writeln('=========== HMAC MD5 self test cf. RFC 2202');
  write('Test  1');
  hmac_MD5_inits(ctx, key1);
  hmac_MD5_update(ctx, @data1, sizeof(data1));
  hmac_MD5_final(ctx, mac);
  if compmem(@mac,  @dig1, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig1, sizeof(dig1)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  2');
  hmac_MD5_inits(ctx, key2);
  hmac_MD5_update(ctx, @data2, sizeof(data2));
  hmac_MD5_final(ctx, mac);
  if compmem(@mac,  @dig2, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig2, sizeof(dig2)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  6');
  hmac_MD5_init(ctx, @key6, sizeof(key6));
  hmac_MD5_update(ctx, @data6, sizeof(data6));
  hmac_MD5_final(ctx, mac);
  if compmem(@mac,  @dig6, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig6, sizeof(dig6)));
    writeln('Code: ', HexStr(@mac , sizeof(mac)));
  end;

  write('Test  7');
  hmac_MD5_init(ctx, @key7, sizeof(key7));
  hmac_MD5_update(ctx, @data7, sizeof(data7));
  hmac_MD5_final(ctx, mac);
  if compmem(@mac,  @dig7, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig7, sizeof(dig7)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

end;



{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA1;
  {-SHA1 test cases from RFC2202}

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
  ctx: THMAC_Context;
  mac: TSHA1Digest;
begin
  writeln('=========== HMAC SHA1 self test cf. RFC 2202');
  write('Test  1');
  hmac_sha1_inits(ctx, key1);
  hmac_sha1_update(ctx, @data1, sizeof(data1));
  hmac_sha1_final(ctx, mac);
  if compmem(@mac,  @dig1, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig1, sizeof(dig1)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  2');
  hmac_sha1_inits(ctx, key2);
  hmac_sha1_update(ctx, @data2, sizeof(data2));
  hmac_sha1_final(ctx, mac);
  if compmem(@mac,  @dig2, sizeof(mac))  then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig2, sizeof(dig2)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  6');
  hmac_sha1_init(ctx, @key6, sizeof(key6));
  hmac_sha1_update(ctx, @data6, sizeof(data6));
  hmac_sha1_final(ctx, mac);
  if compmem(@mac,  @dig6, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig6, sizeof(dig6)));
    writeln('Code: ', HexStr(@mac , sizeof(mac)));
  end;

  write('Test  7');
  hmac_sha1_init(ctx, @key7, sizeof(key7));
  hmac_sha1_update(ctx, @data7, sizeof(data7));
  hmac_sha1_final(ctx, mac);
  if compmem(@mac,  @dig7, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln(' RFC: ', HexStr(@dig7, sizeof(dig7)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

end;



{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA256;
  {-SHA256 test cases from draft-ietf-ipsec-ciph-sha-256-01.txt}

var
  key1 : array[1..32] of byte;
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
  ctx: THMAC_Context;
  mac: TSHA256Digest;
  i  : byte;

begin
  writeln('=========== HMAC SHA256 self test cf. IETF Draft');
  write('Test  1');
  for i:=1 to 32 do key1[i] := i;
  hmac_sha256_init(ctx, @key1, sizeof(key1));
  hmac_sha256_update(ctx, @data1, sizeof(data1));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig1, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig1, sizeof(dig1)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  2');
  hmac_sha256_init(ctx, @key1, sizeof(key1));
  hmac_sha256_update(ctx, @data2, sizeof(data2));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig2, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig2, sizeof(dig2)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  3');
  hmac_sha256_init(ctx, @key1, sizeof(key1));
  hmac_sha256_update(ctx, @data3, sizeof(data3));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig3, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig3, sizeof(dig3)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  5');
  hmac_sha256_inits(ctx, 'Jefe');
  hmac_sha256_update(ctx, @data5, sizeof(data5));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig5, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig5, sizeof(dig5)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test  9');
  hmac_sha256_init(ctx, @key6, sizeof(key6));
  hmac_sha256_update(ctx, @data6, sizeof(data6));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig9, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig10, sizeof(dig10)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;

  write('Test 10');
  hmac_sha256_init(ctx, @key7, sizeof(key7));
  hmac_sha256_update(ctx, @data7, sizeof(data7));
  hmac_sha256_final(ctx, mac);
  if compmem(@mac,  @dig10, sizeof(mac)) then writeln('  TRUE')
  else begin
    writeln(' FALSE');
    writeln('IETF: ', HexStr(@dig10, sizeof(dig10)));
    writeln('Code: ', HexStr(@mac,  sizeof(mac)));
  end;
end;




{---------------------------------------------------------------------------}
procedure Test_HMAC_SHA512;
  {-From Tom St.Denis' LibTomCrypt 1.0, last of 257 of HMACSHA512 rounds}
const
  tdig : TSHA512Digest= ($6e,$6a,$3c,$de,$12,$f2,$cb,$3a,
                         $42,$ec,$8a,$5d,$21,$b4,$35,$c4,
                         $da,$4d,$f6,$ca,$7e,$41,$53,$7d,
                         $36,$1d,$81,$69,$15,$82,$87,$bf,
                         $1d,$22,$41,$58,$1d,$e0,$7f,$88,
                         $fe,$92,$f5,$ae,$4e,$96,$eb,$9c,
                         $48,$9f,$c3,$b2,$58,$ea,$38,$42,
                         $ea,$2d,$51,$1c,$e8,$83,$88,$3e);
var
  ctx: THMAC_Context;
  key: TSHA512Digest;
  inbuf: array[0..255] of byte;
  y,z: integer;
begin
  for z:=0 to sizeof(key)-1 do key[z] := z and 255;
  for y:=0 to 256 do begin
    for z:=0 to y-1 do inbuf[z] := z and 255;
    hmac_SHA512_init(ctx, @key, sizeof(key));
    hmac_SHA512_update(ctx, @inbuf, y);
    hmac_SHA512_final(ctx, key);
  end;
  write('=========== HMAC SHA512 test: ');
  writeln(compmem(@key, @tdig, sizeof(tdig)));
end;


{---------------------------------------------------------------------------}
procedure Test_HMAC_Whirl;
  {-From Tom St.Denis' LibTomCrypt 1.0, last of 129 of HMACWhirl rounds}
const
  tdig: array[0.. 63] of byte = ($4a,$ab,$f1,$c3,$f2,$4c,$20,$ff,
                                 $aa,$61,$d6,$10,$6e,$32,$ef,$1b,
                                 $b7,$cd,$eb,$60,$73,$54,$bd,$4b,
                                 $62,$51,$89,$39,$41,$73,$00,$54,
                                 $24,$4e,$19,$8e,$ec,$d4,$94,$3c,
                                 $77,$08,$2c,$c9,$b4,$06,$a2,$e1,
                                 $22,$71,$bc,$a4,$55,$df,$15,$d3,
                                 $61,$33,$36,$61,$5c,$36,$b2,$2e);
var
  ctx: THMAC_Context;
  key: TWhirlDigest;
  inbuf: array[0..255] of byte;
  y,z: integer;
begin
  for z:=0 to sizeof(key)-1 do key[z] := z and 255;
  for y:=0 to 128 do begin
    for z:=0 to y-1 do inbuf[z] := z and 255;
    hmac_Whirl_init(ctx, @key, sizeof(key));
    hmac_Whirl_update(ctx, @inbuf, y);
    hmac_Whirl_final(ctx, key);
  end;
  write('=========== HMAC Whirlpool test: ');
  writeln(compmem(@key, @tdig, sizeof(tdig)));
end;


begin
  {$ifdef VER80}
    ScreenSize.Y := 50;  {D1: 50 lines screen}
  {$endif}
  fillchar(key6, sizeof(key6), #$aa);
  fillchar(key7, sizeof(key7), #$aa);

  Test_HMAC_SHA1;
  writeln;
  Test_HMAC_MD5;
  writeln;
  Test_HMAC_SHA256;
  writeln;
  Test_HMAC_SHA512;
  Test_HMAC_Whirl;
end.
