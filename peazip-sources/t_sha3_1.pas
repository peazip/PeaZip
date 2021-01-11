{-Test prog for SHA3/SHAKE, WE Aug.2015}

program t_sha3_1;

{Test SHA3-225/256/384/512 and SHAKE128/256. Uses NIST test vectors}
{from http://csrc.nist.gov/groups/ST/toolkit/examples.html and from}
{KeccakCodePackage ... KeccakSpongeIntermediateValues_SHA3-xxx.txt }

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  BTypes, Mem_Util, Hash, SHA3;



var
  err:   integer;
  state: TSHA3State;
  buf:   array[0..511] of byte;

const    {NIST message with 1600, 1605, 1630 bits, $23 is LSB for length 1630}
  M160x: array[0..203] of byte = ($a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$a3,$a3,$a3,$a3,$a3,
                                  $a3,$a3,$a3,$23);



(* NIST's obscure bit message encoding
   -----------------------------------

Length 5
Msg bits:   '11001'
            Reverse
11001    -> 00010011 -> $13
Msg bytes:  ($13)

NOTE: For SHA3_FinalBit which uses MSB format this message with 5 bits is
11001    -> 11001000 -> $C8


Length 30
Msg bits:   '110010100001101011011110100110'
Split   :   '11001010 00011010 11011110 100110'
            Reverse
11001010 -> 01010011 -> $53
00011010 -> 01011000 -> $58
11011110 -> 01111011 -> $7B
100110   -> 00011001 -> $19
Msg bytes:  ($53,$58,$7B,$19);
*)


{---------------------------------------------------------------------------}
procedure test_sha3_224;
const
  {l1 = 0;}
  dig1: TSHA3_224Digest     = ($6B,$4E,$03,$42,$36,$67,$DB,$B7,$3B,$6E,
                               $15,$45,$4F,$0E,$B1,$AB,$D4,$59,$7F,$9A,
                               $1B,$07,$8E,$3F,$5B,$5A,$6B,$C7);
  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: TSHA3_224Digest     = ($FF,$BA,$D5,$DA,$96,$BA,$D7,$17,$89,$33,
                               $02,$06,$DC,$67,$68,$EC,$AE,$B1,$B3,$2D,
                               $CA,$6B,$33,$01,$48,$96,$74,$AB);
  {l3 = 30;}
  msg3: array[0..3] of byte = ($53,$58,$7B,$19);
  dig3: TSHA3_224Digest     = ($D6,$66,$A5,$14,$CC,$9D,$BA,$25,$AC,$1B,
                               $A6,$9E,$D3,$93,$04,$60,$DE,$AA,$C9,$85,
                               $1B,$5F,$0B,$AA,$B0,$07,$DF,$3B);
  {l4 = 1600}
  dx00: TSHA3_224Digest     = ($93,$76,$81,$6a,$ba,$50,$3f,$72,
                               $f9,$6c,$e7,$eb,$65,$ac,$09,$5d,
                               $ee,$e3,$be,$4b,$f9,$bb,$c2,$a1,
                               $cb,$7e,$11,$e0);
  {l5 = 1605}
  dx05 : TSHA3_224Digest    = ($22,$d2,$f7,$bb,$0b,$17,$3f,$d8,
                               $c1,$96,$86,$f9,$17,$31,$66,$e3,
                               $ee,$62,$73,$80,$47,$d7,$ea,$dd,
                               $69,$ef,$b2,$28);
  {l6 = 1630}
  dx30 : TSHA3_224Digest    = ($4e,$90,$7b,$b1,$05,$78,$61,$f2,
                               $00,$a5,$99,$e9,$d4,$f8,$5b,$02,
                               $d8,$84,$53,$bf,$5b,$8a,$ce,$9a,
                               $c5,$89,$13,$4c);
begin
  writeln('** Test SHA3-224 **');
  write('Test 1,       bit length  0: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_FinalHash(state,@buf);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2a (LSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 224);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 2b (MSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_FinalBit(state, $C8, 5,  @buf, 224);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3,       bit length 30: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 224);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);

  write('Test 4,     bit length 1600: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalHash(state, @buf);
  if err=0 then writeln(compmem(@dx00, @buf, sizeof(dx00)))
  else writeln('Err: ', err);

  write('Test 5,     bit length 1605: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalBit_LSB(state, $03, 5,  @buf, 224);
  if err=0 then writeln(compmem(@dx05, @buf, sizeof(dx05)))
  else writeln('Err: ', err);

  write('Test 6,     bit length 1630: ');
  err := SHA3_Init(state,__SHA3_224);
  if err=0 then err := SHA3_Update(state, @M160x, 1630 div 8);
  if err=0 then err := SHA3_FinalBit_LSB(state, M160x[203], 1630 mod 8,  @buf, 224);
  if err=0 then writeln(compmem(@dx30, @buf, sizeof(dx30)))
  else writeln('Err: ', err);

end;


{---------------------------------------------------------------------------}
procedure test_sha3_256;
const
  {l1 = 0;}
  dig1: TSHA3_256Digest     = ($A7,$FF,$C6,$F8,$BF,$1E,$D7,$66,
                               $51,$C1,$47,$56,$A0,$61,$D6,$62,
                               $F5,$80,$FF,$4D,$E4,$3B,$49,$FA,
                               $82,$D8,$0A,$4B,$80,$F8,$43,$4A);
  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: TSHA3_256Digest     = ($7B,$00,$47,$CF,$5A,$45,$68,$82,
                               $36,$3C,$BF,$0F,$B0,$53,$22,$CF,
                               $65,$F4,$B7,$05,$9A,$46,$36,$5E,
                               $83,$01,$32,$E3,$B5,$D9,$57,$AF);
  {l3 = 30;}
  msg3: array[0..3] of byte = ($53,$58,$7B,$19);
  dig3: TSHA3_256Digest     = ($C8,$24,$2F,$EF,$40,$9E,$5A,$E9,
                               $D1,$F1,$C8,$57,$AE,$4D,$C6,$24,
                               $B9,$2B,$19,$80,$9F,$62,$AA,$8C,
                               $07,$41,$1C,$54,$A0,$78,$B1,$D0);
  {l4 = 1600}
  dx00: TSHA3_256Digest     = ($79,$f3,$8a,$de,$c5,$c2,$03,$07,
                               $a9,$8e,$f7,$6e,$83,$24,$af,$bf,
                               $d4,$6c,$fd,$81,$b2,$2e,$39,$73,
                               $c6,$5f,$a1,$bd,$9d,$e3,$17,$87);
  {l5 = 1605}
  dx05: TSHA3_256Digest     = ($81,$ee,$76,$9b,$ed,$09,$50,$86,
                               $2b,$1d,$dd,$ed,$2e,$84,$aa,$a6,
                               $ab,$7b,$fd,$d3,$ce,$aa,$47,$1b,
                               $e3,$11,$63,$d4,$03,$36,$36,$3c);
  {l6 = 1630}
  dx30: TSHA3_256Digest     = ($52,$86,$0a,$a3,$01,$21,$4c,$61,
                               $0d,$92,$2a,$6b,$6c,$ab,$98,$1c,
                               $cd,$06,$01,$2e,$54,$ef,$68,$9d,
                               $74,$40,$21,$e7,$38,$b9,$ed,$20);
begin
  writeln('** Test SHA3-256 **');
  write('Test 1,       bit length  0: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_FinalHash(state,@buf);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2a (LSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 256);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 2a (MSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_FinalBit(state, $C8, 5,  @buf, 256);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3,       bit length 30: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 256);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);

  write('Test 4,     bit length 1600: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalHash(state, @buf);
  if err=0 then writeln(compmem(@dx00, @buf, sizeof(dx00)))
  else writeln('Err: ', err);

  write('Test 5,     bit length 1605: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalBit_LSB(state, $03, 5,  @buf, 256);
  if err=0 then writeln(compmem(@dx05, @buf, sizeof(dx05)))
  else writeln('Err: ', err);

  write('Test 6,     bit length 1630: ');
  err := SHA3_Init(state,__SHA3_256);
  if err=0 then err := SHA3_Update(state, @M160x, 1630 div 8);
  if err=0 then err := SHA3_FinalBit_LSB(state, M160x[203], 1630 mod 8,  @buf, 256);
  if err=0 then writeln(compmem(@dx30, @buf, sizeof(dx30)))
  else writeln('Err: ', err);
end;


{---------------------------------------------------------------------------}
procedure test_sha3_384;
const
  {l1 = 0;}
  dig1: TSHA3_384Digest     = ($0C,$63,$A7,$5B,$84,$5E,$4F,$7D,
                               $01,$10,$7D,$85,$2E,$4C,$24,$85,
                               $C5,$1A,$50,$AA,$AA,$94,$FC,$61,
                               $99,$5E,$71,$BB,$EE,$98,$3A,$2A,
                               $C3,$71,$38,$31,$26,$4A,$DB,$47,
                               $FB,$6B,$D1,$E0,$58,$D5,$F0,$04);
  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: TSHA3_384Digest     = ($73,$7C,$9B,$49,$18,$85,$E9,$BF,
                               $74,$28,$E7,$92,$74,$1A,$7B,$F8,
                               $DC,$A9,$65,$34,$71,$C3,$E1,$48,
                               $47,$3F,$2C,$23,$6B,$6A,$0A,$64,
                               $55,$EB,$1D,$CE,$9F,$77,$9B,$4B,
                               $6B,$23,$7F,$EF,$17,$1B,$1C,$64);
  {l3 = 30;}
  msg3: array[0..3] of byte = ($53,$58,$7B,$19);
  dig3: TSHA3_384Digest     = ($95,$5B,$4D,$D1,$BE,$03,$26,$1B,
                               $D7,$6F,$80,$7A,$7E,$FD,$43,$24,
                               $35,$C4,$17,$36,$28,$11,$B8,$A5,
                               $0C,$56,$4E,$7E,$E9,$58,$5E,$1A,
                               $C7,$62,$6D,$DE,$2F,$DC,$03,$0F,
                               $87,$61,$96,$EA,$26,$7F,$08,$C3);
  {l4 = 1600}
  dx00: TSHA3_384Digest     = ($18,$81,$de,$2c,$a7,$e4,$1e,$f9,
                               $5d,$c4,$73,$2b,$8f,$5f,$00,$2b,
                               $18,$9c,$c1,$e4,$2b,$74,$16,$8e,
                               $d1,$73,$26,$49,$ce,$1d,$bc,$dd,
                               $76,$19,$7a,$31,$fd,$55,$ee,$98,
                               $9f,$2d,$70,$50,$dd,$47,$3e,$8f);
  {l5 = 1605}
  dx05: TSHA3_384Digest     = ($a3,$1f,$db,$d8,$d5,$76,$55,$1c,
                               $21,$fb,$11,$91,$b5,$4b,$da,$65,
                               $b6,$c5,$fe,$97,$f0,$f4,$a6,$91,
                               $03,$42,$4b,$43,$f7,$fd,$b8,$35,
                               $97,$9f,$db,$ea,$e8,$b3,$fe,$16,
                               $cb,$82,$e5,$87,$38,$1e,$b6,$24);
  {l6 = 1630}
  dx30: TSHA3_384Digest     = ($34,$85,$d3,$b2,$80,$bd,$38,$4c,
                               $f4,$a7,$77,$84,$4e,$94,$67,$81,
                               $73,$05,$5d,$1c,$bc,$40,$c7,$c2,
                               $c3,$83,$3d,$9e,$f1,$23,$45,$17,
                               $2d,$6f,$cd,$31,$92,$3b,$b8,$79,
                               $5a,$c8,$18,$47,$d3,$d8,$85,$5c);
begin
  writeln('** Test SHA3-384 **');
  write('Test 1,       bit length  0: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_FinalHash(state,@buf);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2a (LSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 384);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 2b (MSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_FinalBit(state, $C8, 5,  @buf, 384);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3,       bit length 30: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 384);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);

  write('Test 4,     bit length 1600: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalHash(state, @buf);
  if err=0 then writeln(compmem(@dx00, @buf, sizeof(dx00)))
  else writeln('Err: ', err);

  write('Test 5,     bit length 1605: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalBit_LSB(state, $03, 5,  @buf, 384);
  if err=0 then writeln(compmem(@dx05, @buf, sizeof(dx05)))
  else writeln('Err: ', err);

  write('Test 6,     bit length 1630: ');
  err := SHA3_Init(state,__SHA3_384);
  if err=0 then err := SHA3_Update(state, @M160x, 1630 div 8);
  if err=0 then err := SHA3_FinalBit_LSB(state, M160x[203], 1630 mod 8,  @buf, 384);
  if err=0 then writeln(compmem(@dx30, @buf, sizeof(dx30)))
  else writeln('Err: ', err);
end;


{---------------------------------------------------------------------------}
procedure test_sha3_512;
const
  {l1 = 0;}
  dig1: TSHA3_512Digest     = ($A6,$9F,$73,$CC,$A2,$3A,$9A,$C5,
                               $C8,$B5,$67,$DC,$18,$5A,$75,$6E,
                               $97,$C9,$82,$16,$4F,$E2,$58,$59,
                               $E0,$D1,$DC,$C1,$47,$5C,$80,$A6,
                               $15,$B2,$12,$3A,$F1,$F5,$F9,$4C,
                               $11,$E3,$E9,$40,$2C,$3A,$C5,$58,
                               $F5,$00,$19,$9D,$95,$B6,$D3,$E3,
                               $01,$75,$85,$86,$28,$1D,$CD,$26);
  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: TSHA3_512Digest     = ($A1,$3E,$01,$49,$41,$14,$C0,$98,
                               $00,$62,$2A,$70,$28,$8C,$43,$21,
                               $21,$CE,$70,$03,$9D,$75,$3C,$AD,
                               $D2,$E0,$06,$E4,$D9,$61,$CB,$27,
                               $54,$4C,$14,$81,$E5,$81,$4B,$DC,
                               $EB,$53,$BE,$67,$33,$D5,$E0,$99,
                               $79,$5E,$5E,$81,$91,$8A,$DD,$B0,
                               $58,$E2,$2A,$9F,$24,$88,$3F,$37);
  {l3 = 30;}
  msg3: array[0..3] of byte = ($53,$58,$7B,$19);
  dig3: TSHA3_512Digest     = ($98,$34,$C0,$5A,$11,$E1,$C5,$D3,
                               $DA,$9C,$74,$0E,$1C,$10,$6D,$9E,
                               $59,$0A,$0E,$53,$0B,$6F,$6A,$AA,
                               $78,$30,$52,$5D,$07,$5C,$A5,$DB,
                               $1B,$D8,$A6,$AA,$98,$1A,$28,$61,
                               $3A,$C3,$34,$93,$4A,$01,$82,$3C,
                               $D4,$5F,$45,$E4,$9B,$6D,$7E,$69,
                               $17,$F2,$F1,$67,$78,$06,$7B,$AB);
  {l4 = 1600}
  dx00: TSHA3_512Digest     = ($e7,$6d,$fa,$d2,$20,$84,$a8,$b1,
                               $46,$7f,$cf,$2f,$fa,$58,$36,$1b,
                               $ec,$76,$28,$ed,$f5,$f3,$fd,$c0,
                               $e4,$80,$5d,$c4,$8c,$ae,$ec,$a8,
                               $1b,$7c,$13,$c3,$0a,$df,$52,$a3,
                               $65,$95,$84,$73,$9a,$2d,$f4,$6b,
                               $e5,$89,$c5,$1c,$a1,$a4,$a8,$41,
                               $6d,$f6,$54,$5a,$1c,$e8,$ba,$00);
  {l5 = 1605}
  dx05: TSHA3_512Digest     = ($fc,$4a,$16,$7c,$cb,$31,$a9,$37,
                               $d6,$98,$fd,$e8,$2b,$04,$34,$8c,
                               $95,$39,$b2,$8f,$0c,$9d,$3b,$45,
                               $05,$70,$9c,$03,$81,$23,$50,$e4,
                               $99,$0e,$96,$22,$97,$4f,$6e,$57,
                               $5c,$47,$86,$1c,$0d,$2e,$63,$8c,
                               $cf,$c2,$02,$3c,$36,$5b,$b6,$0a,
                               $93,$f5,$28,$55,$06,$98,$78,$6b);
  {l6 = 1630}
  dx30: TSHA3_512Digest     = ($cf,$9a,$30,$ac,$1f,$1f,$6a,$c0,
                               $91,$6f,$9f,$ef,$19,$19,$c5,$95,
                               $de,$be,$2e,$e8,$0c,$85,$42,$12,
                               $10,$fd,$f0,$5f,$1c,$6a,$f7,$3a,
                               $a9,$ca,$c8,$81,$d0,$f9,$1d,$b6,
                               $d0,$34,$a2,$bb,$ad,$c1,$cf,$7f,
                               $bc,$b2,$ec,$fa,$9d,$19,$1d,$3a,
                               $50,$16,$fb,$3f,$ad,$87,$09,$c9);
begin
  writeln('** Test SHA3-512 **');
  write('Test 1,       bit length  0: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_FinalHash(state,@buf);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2a (LSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 512);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 2b (MSB), bit length 5: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_FinalBit(state, $C8, 5,  @buf, 512);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3,       bit length 30: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 512);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);

  write('Test 4,     bit length 1600: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalHash(state, @buf);
  if err=0 then writeln(compmem(@dx00, @buf, sizeof(dx00)))
  else writeln('Err: ', err);

  write('Test 5,     bit length 1605: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_Update(state, @M160x, 200);
  if err=0 then err := SHA3_FinalBit_LSB(state, $03, 5,  @buf, 512);
  if err=0 then writeln(compmem(@dx05, @buf, sizeof(dx05)))
  else writeln('Err: ', err);

  write('Test 6,     bit length 1630: ');
  err := SHA3_Init(state,__SHA3_512);
  if err=0 then err := SHA3_Update(state, @M160x, 1630 div 8);
  if err=0 then err := SHA3_FinalBit_LSB(state, M160x[203], 1630 mod 8,  @buf, 512);
  if err=0 then writeln(compmem(@dx30, @buf, sizeof(dx30)))
  else writeln('Err: ', err);
end;


{---------------------------------------------------------------------------}
procedure test_sha3_shake128;
const
  {l1 = 0;}
  msg1: array[0..0] of byte = (0);
  dig1: array[0..511] of byte = (
          $7F,$9C,$2B,$A4,$E8,$8F,$82,$7D,$61,$60,$45,$50,$76,$05,$85,$3E,
          $D7,$3B,$80,$93,$F6,$EF,$BC,$88,$EB,$1A,$6E,$AC,$FA,$66,$EF,$26,
          $3C,$B1,$EE,$A9,$88,$00,$4B,$93,$10,$3C,$FB,$0A,$EE,$FD,$2A,$68,
          $6E,$01,$FA,$4A,$58,$E8,$A3,$63,$9C,$A8,$A1,$E3,$F9,$AE,$57,$E2,
          $35,$B8,$CC,$87,$3C,$23,$DC,$62,$B8,$D2,$60,$16,$9A,$FA,$2F,$75,
          $AB,$91,$6A,$58,$D9,$74,$91,$88,$35,$D2,$5E,$6A,$43,$50,$85,$B2,
          $BA,$DF,$D6,$DF,$AA,$C3,$59,$A5,$EF,$BB,$7B,$CC,$4B,$59,$D5,$38,
          $DF,$9A,$04,$30,$2E,$10,$C8,$BC,$1C,$BF,$1A,$0B,$3A,$51,$20,$EA,
          $17,$CD,$A7,$CF,$AD,$76,$5F,$56,$23,$47,$4D,$36,$8C,$CC,$A8,$AF,
          $00,$07,$CD,$9F,$5E,$4C,$84,$9F,$16,$7A,$58,$0B,$14,$AA,$BD,$EF,
          $AE,$E7,$EE,$F4,$7C,$B0,$FC,$A9,$76,$7B,$E1,$FD,$A6,$94,$19,$DF,
          $B9,$27,$E9,$DF,$07,$34,$8B,$19,$66,$91,$AB,$AE,$B5,$80,$B3,$2D,
          $EF,$58,$53,$8B,$8D,$23,$F8,$77,$32,$EA,$63,$B0,$2B,$4F,$A0,$F4,
          $87,$33,$60,$E2,$84,$19,$28,$CD,$60,$DD,$4C,$EE,$8C,$C0,$D4,$C9,
          $22,$A9,$61,$88,$D0,$32,$67,$5C,$8A,$C8,$50,$93,$3C,$7A,$FF,$15,
          $33,$B9,$4C,$83,$4A,$DB,$B6,$9C,$61,$15,$BA,$D4,$69,$2D,$86,$19,
          $F9,$0B,$0C,$DF,$8A,$7B,$9C,$26,$40,$29,$AC,$18,$5B,$70,$B8,$3F,
          $28,$01,$F2,$F4,$B3,$F7,$0C,$59,$3E,$A3,$AE,$EB,$61,$3A,$7F,$1B,
          $1D,$E3,$3F,$D7,$50,$81,$F5,$92,$30,$5F,$2E,$45,$26,$ED,$C0,$96,
          $31,$B1,$09,$58,$F4,$64,$D8,$89,$F3,$1B,$A0,$10,$25,$0F,$DA,$7F,
          $13,$68,$EC,$29,$67,$FC,$84,$EF,$2A,$E9,$AF,$F2,$68,$E0,$B1,$70,
          $0A,$FF,$C6,$82,$0B,$52,$3A,$3D,$91,$71,$35,$F2,$DF,$F2,$EE,$06,
          $BF,$E7,$2B,$31,$24,$72,$1D,$4A,$26,$C0,$4E,$53,$A7,$5E,$30,$E7,
          $3A,$7A,$9C,$4A,$95,$D9,$1C,$55,$D4,$95,$E9,$F5,$1D,$D0,$B5,$E9,
          $D8,$3C,$6D,$5E,$8C,$E8,$03,$AA,$62,$B8,$D6,$54,$DB,$53,$D0,$9B,
          $8D,$CF,$F2,$73,$CD,$FE,$B5,$73,$FA,$D8,$BC,$D4,$55,$78,$BE,$C2,
          $E7,$70,$D0,$1E,$FD,$E8,$6E,$72,$1A,$3F,$7C,$6C,$CE,$27,$5D,$AB,
          $E6,$E2,$14,$3F,$1A,$F1,$8D,$A7,$EF,$DD,$C4,$C7,$B7,$0B,$5E,$34,
          $5D,$B9,$3C,$C9,$36,$BE,$A3,$23,$49,$1C,$CB,$38,$A3,$88,$F5,$46,
          $A9,$FF,$00,$DD,$4E,$13,$00,$B9,$B2,$15,$3D,$20,$41,$D2,$05,$B4,
          $43,$E4,$1B,$45,$A6,$53,$F2,$A5,$C4,$49,$2C,$1A,$DD,$54,$45,$12,
          $DD,$A2,$52,$98,$33,$46,$2B,$71,$A4,$1A,$45,$BE,$97,$29,$0B,$6F);
  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: array[0..511] of byte = (
          $2E,$0A,$BF,$BA,$83,$E6,$72,$0B,$FB,$C2,$25,$FF,$6B,$7A,$B9,$FF,
          $CE,$58,$BA,$02,$7E,$E3,$D8,$98,$76,$4F,$EF,$28,$7D,$DE,$CC,$CA,
          $3E,$6E,$59,$98,$41,$1E,$7D,$DB,$32,$F6,$75,$38,$F5,$00,$B1,$8C,
          $8C,$97,$C4,$52,$C3,$70,$EA,$2C,$F0,$AF,$CA,$3E,$05,$DE,$7E,$4D,
          $E2,$7F,$A4,$41,$A9,$CB,$34,$FD,$17,$C9,$78,$B4,$2D,$5B,$7E,$7F,
          $9A,$B1,$8F,$FE,$FF,$C3,$C5,$AC,$2F,$3A,$45,$5E,$EB,$FD,$C7,$6C,
          $EA,$EB,$0A,$2C,$CA,$22,$EE,$F6,$E6,$37,$F4,$CA,$BE,$5C,$51,$DE,
          $D2,$E3,$FA,$D8,$B9,$52,$70,$A3,$21,$84,$56,$64,$F1,$07,$D1,$64,
          $96,$BB,$7A,$BF,$BE,$75,$04,$B6,$ED,$E2,$E8,$9E,$4B,$99,$6F,$B5,
          $8E,$FD,$C4,$18,$1F,$91,$63,$38,$1C,$BE,$7B,$C0,$06,$A7,$A2,$05,
          $98,$9C,$52,$6C,$D1,$BD,$68,$98,$36,$93,$B4,$BD,$C5,$37,$28,$B2,
          $41,$C1,$CF,$F4,$2B,$B6,$11,$50,$2C,$35,$20,$5C,$AB,$B2,$88,$75,
          $56,$55,$D6,$20,$C6,$79,$94,$F0,$64,$51,$18,$7F,$6F,$D1,$7E,$04,
          $66,$82,$BA,$12,$86,$06,$3F,$F8,$8F,$E2,$50,$8D,$1F,$CA,$F9,$03,
          $5A,$12,$31,$AD,$41,$50,$A9,$C9,$B2,$4C,$9B,$2D,$66,$B2,$AD,$1B,
          $DE,$0B,$D0,$BB,$CB,$8B,$E0,$5B,$83,$52,$29,$EF,$79,$19,$73,$73,
          $23,$42,$44,$01,$E1,$D8,$37,$B6,$6E,$B4,$E6,$30,$FF,$1D,$E7,$0C,
          $B3,$17,$C2,$BA,$CB,$08,$00,$1D,$34,$77,$B7,$A7,$0A,$57,$6D,$20,
          $86,$90,$33,$58,$9D,$85,$A0,$1D,$DB,$2B,$66,$46,$C0,$43,$B5,$9F,
          $C0,$11,$31,$1D,$A6,$66,$FA,$5A,$D1,$D6,$38,$7F,$A9,$BC,$40,$15,
          $A3,$8A,$51,$D1,$DA,$1E,$A6,$1D,$64,$8D,$C8,$E3,$9A,$88,$B9,$D6,
          $22,$BD,$E2,$07,$FD,$AB,$C6,$F2,$82,$7A,$88,$0C,$33,$0B,$BF,$6D,
          $F7,$33,$77,$4B,$65,$3E,$57,$30,$5D,$78,$DC,$E1,$12,$F1,$0A,$2C,
          $71,$F4,$CD,$AD,$92,$ED,$11,$3E,$1C,$EA,$63,$B9,$19,$25,$ED,$28,
          $19,$1E,$6D,$BB,$B5,$AA,$5A,$2A,$FD,$A5,$1F,$C0,$5A,$3A,$F5,$25,
          $8B,$87,$66,$52,$43,$55,$0F,$28,$94,$8A,$E2,$B8,$BE,$B6,$BC,$9C,
          $77,$0B,$35,$F0,$67,$EA,$A6,$41,$EF,$E6,$5B,$1A,$44,$90,$9D,$1B,
          $14,$9F,$97,$EE,$A6,$01,$39,$1C,$60,$9E,$C8,$1D,$19,$30,$F5,$7C,
          $18,$A4,$E0,$FA,$B4,$91,$D1,$CA,$DF,$D5,$04,$83,$44,$9E,$DC,$0F,
          $07,$FF,$B2,$4D,$2C,$6F,$9A,$9A,$3B,$FF,$39,$AE,$3D,$57,$F5,$60,
          $65,$4D,$7D,$75,$C9,$08,$AB,$E6,$25,$64,$75,$3E,$AC,$39,$D7,$50,
          $3D,$A6,$D3,$7C,$2E,$32,$E1,$AF,$3B,$8A,$EC,$8A,$E3,$06,$9C,$D9);
  {l3 = 30;}
  msg3: array[0..3] of byte = ($53,$58,$7B,$19);
  dig3: array[0..511] of byte = (
          $6D,$5D,$39,$C5,$5F,$3C,$CA,$56,$7F,$EA,$F4,$22,$DC,$64,$BA,$17,
          $40,$1D,$07,$75,$6D,$78,$B0,$FA,$3D,$54,$6D,$66,$AF,$C2,$76,$71,
          $E0,$01,$06,$85,$FC,$69,$A7,$EC,$3C,$53,$67,$B8,$FA,$5F,$DA,$39,
          $D5,$7C,$E5,$3F,$15,$3F,$A4,$03,$1D,$27,$72,$06,$77,$0A,$EC,$6B,
          $2D,$DF,$16,$AE,$FA,$B6,$69,$11,$0D,$6E,$4A,$29,$6A,$14,$FB,$14,
          $86,$B0,$84,$6B,$69,$05,$43,$E4,$05,$7F,$7F,$42,$AA,$8C,$0E,$6A,
          $5A,$56,$B6,$0B,$68,$8D,$55,$A1,$96,$DF,$6F,$39,$76,$E3,$06,$88,
          $CB,$B6,$AF,$D4,$85,$25,$D7,$64,$90,$35,$7F,$3F,$D8,$97,$BA,$FC,
          $87,$36,$D9,$07,$B9,$BA,$C8,$16,$59,$1F,$C2,$4E,$79,$36,$0B,$E3,
          $A7,$FF,$A6,$29,$82,$C4,$5A,$BB,$0E,$58,$4C,$07,$EC,$93,$A1,$95,
          $30,$50,$9D,$9F,$81,$62,$15,$D7,$27,$7B,$B9,$99,$43,$7C,$82,$14,
          $50,$F0,$75,$92,$81,$CD,$8E,$16,$A3,$48,$3E,$3C,$C7,$52,$09,$1B,
          $7A,$AE,$92,$90,$9D,$2F,$50,$1E,$F7,$DC,$E9,$89,$75,$98,$91,$B3,
          $37,$7C,$EA,$B4,$93,$FF,$E4,$96,$01,$0A,$0C,$7E,$51,$95,$99,$94,
          $F5,$6F,$56,$5E,$63,$3A,$F6,$09,$3A,$C6,$E1,$E0,$F0,$04,$88,$71,
          $EC,$47,$78,$F4,$8E,$F8,$BD,$5B,$CB,$80,$EA,$7D,$F9,$FF,$47,$11,
          $C8,$1E,$24,$C0,$22,$1C,$2A,$D9,$74,$4F,$BA,$79,$35,$EA,$EC,$A1,
          $14,$22,$4F,$D1,$08,$EF,$C5,$AC,$74,$C6,$62,$52,$08,$92,$75,$B4,
          $27,$76,$73,$70,$8C,$4A,$F9,$2F,$88,$13,$B1,$93,$59,$9F,$D6,$4B,
          $D7,$48,$4F,$2E,$5E,$C3,$69,$E3,$64,$64,$99,$76,$8E,$58,$1D,$D0,
          $53,$AA,$48,$14,$D8,$BF,$1A,$CF,$F5,$FD,$77,$45,$19,$A7,$49,$BE,
          $66,$75,$47,$41,$EB,$C5,$36,$22,$12,$A9,$FE,$A8,$A8,$14,$E9,$E0,
          $10,$BC,$27,$20,$B3,$B7,$D9,$4F,$AB,$74,$BC,$7F,$92,$3E,$10,$72,
          $B8,$A5,$DD,$DD,$A8,$3B,$A0,$15,$7D,$8C,$BA,$55,$C1,$92,$DF,$69,
          $65,$CB,$7D,$BA,$46,$A3,$34,$0D,$F8,$C3,$FA,$89,$C7,$C4,$DB,$53,
          $9D,$38,$DC,$40,$6F,$1D,$2C,$F5,$4E,$59,$05,$58,$0B,$44,$04,$BF,
          $D7,$B3,$71,$95,$61,$C5,$A5,$9D,$5D,$FD,$B1,$BF,$93,$DF,$13,$82,
          $52,$25,$ED,$CC,$E0,$FA,$7D,$87,$EF,$CD,$23,$9F,$EB,$49,$FC,$9E,
          $2D,$E9,$D8,$28,$FE,$EB,$1F,$2C,$F5,$79,$B9,$5D,$D0,$50,$AB,$2C,
          $A4,$71,$05,$A8,$D3,$0F,$3F,$D2,$A1,$15,$4C,$15,$F8,$7F,$B3,$7B,
          $2C,$71,$56,$BD,$7F,$3C,$F2,$B7,$45,$C9,$12,$A4,$0B,$C1,$B5,$59,
          $B6,$56,$E3,$E9,$03,$CC,$57,$33,$E8,$6B,$A1,$5D,$FE,$F7,$06,$78);
begin
  writeln('** Test SHAKE-128 / 4096 **');
  write('Test 1, bit length  0: ');
  err := SHA3_Init(state,__SHAKE_128);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg1[0], 0,  @buf, 4096);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2, bit length  5: ');
  err := SHA3_Init(state,__SHAKE_128);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 4096);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3, bit length 30: ');
  err := SHA3_Init(state,__SHAKE_128);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 4096);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);
end;


{---------------------------------------------------------------------------}
procedure test_sha3_shake256;
const
  {l1 = 0;}
  msg1: array[0..0] of byte  = (0);
  dig1: array[0..511] of byte = (
          $46,$B9,$DD,$2B,$0B,$A8,$8D,$13,$23,$3B,$3F,$EB,$74,$3E,$EB,$24,
          $3F,$CD,$52,$EA,$62,$B8,$1B,$82,$B5,$0C,$27,$64,$6E,$D5,$76,$2F,
          $D7,$5D,$C4,$DD,$D8,$C0,$F2,$00,$CB,$05,$01,$9D,$67,$B5,$92,$F6,
          $FC,$82,$1C,$49,$47,$9A,$B4,$86,$40,$29,$2E,$AC,$B3,$B7,$C4,$BE,
          $14,$1E,$96,$61,$6F,$B1,$39,$57,$69,$2C,$C7,$ED,$D0,$B4,$5A,$E3,
          $DC,$07,$22,$3C,$8E,$92,$93,$7B,$EF,$84,$BC,$0E,$AB,$86,$28,$53,
          $34,$9E,$C7,$55,$46,$F5,$8F,$B7,$C2,$77,$5C,$38,$46,$2C,$50,$10,
          $D8,$46,$C1,$85,$C1,$51,$11,$E5,$95,$52,$2A,$6B,$CD,$16,$CF,$86,
          $F3,$D1,$22,$10,$9E,$3B,$1F,$DD,$94,$3B,$6A,$EC,$46,$8A,$2D,$62,
          $1A,$7C,$06,$C6,$A9,$57,$C6,$2B,$54,$DA,$FC,$3B,$E8,$75,$67,$D6,
          $77,$23,$13,$95,$F6,$14,$72,$93,$B6,$8C,$EA,$B7,$A9,$E0,$C5,$8D,
          $86,$4E,$8E,$FD,$E4,$E1,$B9,$A4,$6C,$BE,$85,$47,$13,$67,$2F,$5C,
          $AA,$AE,$31,$4E,$D9,$08,$3D,$AB,$4B,$09,$9F,$8E,$30,$0F,$01,$B8,
          $65,$0F,$1F,$4B,$1D,$8F,$CF,$3F,$3C,$B5,$3F,$B8,$E9,$EB,$2E,$A2,
          $03,$BD,$C9,$70,$F5,$0A,$E5,$54,$28,$A9,$1F,$7F,$53,$AC,$26,$6B,
          $28,$41,$9C,$37,$78,$A1,$5F,$D2,$48,$D3,$39,$ED,$E7,$85,$FB,$7F,
          $5A,$1A,$AA,$96,$D3,$13,$EA,$CC,$89,$09,$36,$C1,$73,$CD,$CD,$0F,
          $AB,$88,$2C,$45,$75,$5F,$EB,$3A,$ED,$96,$D4,$77,$FF,$96,$39,$0B,
          $F9,$A6,$6D,$13,$68,$B2,$08,$E2,$1F,$7C,$10,$D0,$4A,$3D,$BD,$4E,
          $36,$06,$33,$E5,$DB,$4B,$60,$26,$01,$C1,$4C,$EA,$73,$7D,$B3,$DC,
          $F7,$22,$63,$2C,$C7,$78,$51,$CB,$DD,$E2,$AA,$F0,$A3,$3A,$07,$B3,
          $73,$44,$5D,$F4,$90,$CC,$8F,$C1,$E4,$16,$0F,$F1,$18,$37,$8F,$11,
          $F0,$47,$7D,$E0,$55,$A8,$1A,$9E,$DA,$57,$A4,$A2,$CF,$B0,$C8,$39,
          $29,$D3,$10,$91,$2F,$72,$9E,$C6,$CF,$A3,$6C,$6A,$C6,$A7,$58,$37,
          $14,$30,$45,$D7,$91,$CC,$85,$EF,$F5,$B2,$19,$32,$F2,$38,$61,$BC,
          $F2,$3A,$52,$B5,$DA,$67,$EA,$F7,$BA,$AE,$0F,$5F,$B1,$36,$9D,$B7,
          $8F,$3A,$C4,$5F,$8C,$4A,$C5,$67,$1D,$85,$73,$5C,$DD,$DB,$09,$D2,
          $B1,$E3,$4A,$1F,$C0,$66,$FF,$4A,$16,$2C,$B2,$63,$D6,$54,$12,$74,
          $AE,$2F,$CC,$86,$5F,$61,$8A,$BE,$27,$C1,$24,$CD,$8B,$07,$4C,$CD,
          $51,$63,$01,$B9,$18,$75,$82,$4D,$09,$95,$8F,$34,$1E,$F2,$74,$BD,
          $AB,$0B,$AE,$31,$63,$39,$89,$43,$04,$E3,$58,$77,$B0,$C2,$8A,$9B,
          $1F,$D1,$66,$C7,$96,$B9,$CC,$25,$8A,$06,$4A,$8F,$57,$E2,$7F,$2A);

  {l2 = 5;}
  msg2: array[0..0] of byte = ($13);
  dig2: array[0..511] of byte = (
          $48,$A5,$C1,$1A,$BA,$EE,$FF,$09,$2F,$36,$46,$EF,$0D,$6B,$3D,$3F,
          $F7,$6C,$2F,$55,$F9,$C7,$32,$AC,$64,$70,$C0,$37,$64,$00,$82,$12,
          $E2,$1B,$14,$67,$77,$8B,$18,$19,$89,$F8,$88,$58,$21,$1B,$45,$DF,
          $87,$99,$CF,$96,$1F,$80,$0D,$FA,$C9,$9E,$64,$40,$39,$E2,$97,$9A,
          $40,$16,$F5,$45,$6F,$F4,$21,$C5,$B3,$85,$DA,$2B,$85,$5D,$A7,$E3,
          $1C,$8C,$2E,$8E,$4B,$A4,$1E,$B4,$09,$5C,$B9,$99,$D9,$75,$9C,$B4,
          $03,$58,$DA,$85,$62,$A2,$E6,$13,$49,$E0,$5A,$2E,$13,$F1,$B7,$4E,
          $C9,$E6,$9F,$5B,$42,$6D,$C7,$41,$38,$FF,$CD,$C5,$71,$C3,$2B,$39,
          $B9,$F5,$55,$63,$E1,$A9,$9D,$C4,$22,$C3,$06,$02,$6D,$6A,$0F,$9D,
          $E8,$51,$62,$B3,$86,$79,$4C,$A0,$68,$8B,$76,$4B,$3D,$32,$20,$0C,
          $C4,$59,$74,$97,$32,$A0,$F3,$A3,$41,$C0,$EF,$C9,$6A,$22,$C6,$3B,
          $AD,$7D,$96,$CC,$9B,$A4,$76,$8C,$6F,$CF,$A1,$F2,$00,$10,$7C,$F9,
          $FA,$E5,$C0,$D7,$54,$95,$8C,$5A,$75,$6B,$37,$6A,$3B,$E6,$9F,$88,
          $07,$4F,$20,$0E,$9E,$95,$A8,$CA,$5B,$CF,$96,$99,$98,$DB,$1D,$C3,
          $7D,$0D,$3D,$91,$6F,$6C,$AA,$B3,$F0,$37,$82,$C9,$C4,$4A,$2E,$14,
          $E8,$07,$86,$BE,$CE,$45,$87,$B9,$EF,$82,$CB,$F4,$54,$E0,$E3,$4B,
          $D1,$75,$AE,$57,$D3,$6A,$F4,$E7,$26,$B2,$21,$33,$2C,$ED,$36,$C8,
          $CE,$2E,$06,$20,$3C,$65,$6A,$E8,$DA,$03,$7D,$08,$E7,$16,$0B,$48,
          $0C,$1A,$85,$16,$BF,$06,$DD,$97,$BF,$4A,$A4,$C0,$24,$93,$10,$DC,
          $0B,$06,$5D,$C6,$39,$57,$63,$55,$38,$4D,$16,$5C,$6A,$50,$9B,$12,
          $F7,$BB,$D1,$E1,$5B,$22,$BC,$E0,$2F,$A0,$48,$DD,$FA,$AC,$F7,$41,
          $5F,$49,$B6,$32,$4C,$1D,$06,$7B,$52,$64,$E1,$12,$5F,$7F,$75,$42,
          $7F,$31,$2B,$D9,$34,$6E,$B4,$E4,$00,$B1,$F7,$CB,$31,$28,$8C,$9E,
          $3F,$73,$5E,$CA,$9C,$ED,$0D,$B8,$88,$E2,$E2,$F4,$02,$24,$3B,$D6,
          $46,$18,$A2,$3E,$10,$F9,$C2,$29,$39,$74,$40,$54,$2D,$0A,$B1,$B2,
          $E1,$0D,$AC,$C5,$C9,$5E,$59,$7F,$2C,$7E,$A3,$84,$38,$10,$5F,$97,
          $80,$3D,$BB,$03,$FC,$C0,$FD,$41,$6B,$09,$05,$A4,$1D,$18,$4D,$EB,
          $23,$89,$05,$77,$58,$91,$F9,$35,$01,$FB,$41,$76,$A3,$BD,$6C,$46,
          $44,$61,$D3,$6E,$E8,$B0,$08,$AA,$BD,$9E,$26,$A3,$40,$55,$E8,$0C,
          $8C,$81,$3E,$EB,$A0,$7F,$72,$8A,$B3,$2B,$15,$60,$5A,$D1,$61,$A0,
          $66,$9F,$6F,$CE,$5C,$55,$09,$FB,$B6,$AF,$D2,$4A,$EA,$CC,$5F,$A4,
          $A5,$15,$23,$E6,$B1,$73,$24,$6E,$D4,$BF,$A5,$21,$D7,$4F,$C6,$BB);
  {l3 = 30;}
  msg3: array[0..3] of byte  = ($53,$58,$7B,$19);
  dig3: array[0..511] of byte = (
          $46,$5D,$08,$1D,$FF,$87,$5E,$39,$62,$00,$E4,$48,$1A,$3E,$9D,$CD,
          $88,$D0,$79,$AA,$6D,$66,$22,$6C,$B6,$BA,$45,$41,$07,$CB,$81,$A7,
          $84,$1A,$B0,$29,$60,$DE,$27,$9C,$CB,$E3,$4B,$42,$C3,$65,$85,$AD,
          $86,$96,$4D,$B0,$DB,$52,$B6,$E7,$B4,$36,$9E,$CE,$8F,$72,$48,$58,
          $9B,$A7,$8A,$B1,$82,$8F,$FC,$33,$5C,$B1,$23,$97,$11,$9B,$FD,$2B,
          $87,$EB,$78,$98,$AE,$B9,$56,$B6,$F2,$3D,$DF,$0B,$D4,$00,$43,$86,
          $A8,$E5,$26,$55,$4E,$F4,$E4,$83,$FA,$CE,$E3,$0D,$D3,$2E,$20,$4F,
          $FF,$8C,$36,$BB,$D6,$02,$A5,$76,$D1,$39,$08,$9C,$75,$A8,$05,$02,
          $66,$FC,$BF,$72,$1E,$44,$43,$DE,$46,$45,$83,$29,$22,$EB,$8A,$AE,
          $39,$D1,$F5,$72,$84,$53,$64,$81,$7B,$00,$33,$54,$38,$99,$94,$00,
          $23,$F2,$E9,$65,$A6,$0A,$80,$EB,$22,$1E,$B1,$9D,$C5,$7B,$12,$12,
          $91,$56,$4C,$6F,$69,$35,$83,$B3,$AC,$7C,$6F,$27,$2F,$4F,$67,$A1,
          $9A,$76,$78,$D4,$23,$4B,$0B,$F4,$A2,$EB,$C0,$8A,$A2,$35,$B9,$78,
          $8D,$B7,$87,$16,$1F,$66,$17,$02,$28,$65,$C0,$EF,$9A,$A5,$33,$80,
          $2D,$13,$6C,$DB,$C7,$AE,$BA,$53,$2A,$CF,$1B,$E1,$83,$B0,$29,$5A,
          $B0,$E3,$3A,$2E,$F6,$9B,$E3,$56,$DA,$AF,$30,$96,$87,$15,$3E,$2F,
          $99,$A1,$24,$36,$09,$D6,$03,$12,$6A,$8C,$82,$3E,$88,$43,$E4,$59,
          $BF,$C7,$2B,$30,$69,$1C,$DC,$C3,$DD,$B2,$7C,$F0,$28,$AF,$D5,$1E,
          $44,$37,$EE,$3B,$71,$C0,$C1,$EC,$87,$A9,$34,$36,$F0,$C2,$47,$B7,
          $E8,$C5,$0C,$E9,$68,$25,$C9,$70,$29,$99,$7A,$74,$C3,$18,$AF,$AC,
          $AA,$18,$A0,$18,$0B,$C7,$F2,$F0,$F1,$C5,$E7,$EF,$1A,$2D,$18,$3A,
          $C7,$EE,$7E,$49,$15,$C3,$B6,$8C,$30,$97,$8A,$B6,$C4,$28,$19,$34,
          $41,$DF,$47,$05,$B7,$22,$CE,$25,$A0,$8A,$1F,$AD,$CA,$0E,$EF,$1F,
          $AF,$E8,$3A,$DF,$13,$02,$1D,$52,$0D,$E5,$C8,$27,$FF,$9A,$97,$B7,
          $55,$46,$19,$3A,$9B,$92,$3F,$05,$90,$38,$5D,$C4,$BF,$F7,$C4,$9D,
          $49,$15,$B5,$A3,$65,$DB,$4C,$84,$DD,$CB,$18,$5D,$E8,$F9,$EE,$B3,
          $34,$96,$5A,$42,$F1,$38,$1C,$8B,$AD,$C2,$2B,$A1,$F8,$EE,$4C,$0E,
          $4D,$AA,$F7,$A8,$8E,$7F,$42,$DD,$B8,$14,$8F,$3B,$F8,$D3,$B8,$D7,
          $4F,$09,$81,$55,$A3,$7C,$B4,$CB,$27,$87,$6B,$85,$DA,$60,$2E,$5C,
          $78,$9C,$10,$E0,$3B,$E7,$34,$07,$BA,$B8,$C4,$92,$13,$F8,$C7,$4E,
          $12,$66,$CE,$9B,$11,$28,$6E,$67,$4C,$A9,$C1,$0C,$9C,$99,$55,$04,
          $9A,$66,$E9,$05,$1D,$9A,$2B,$1F,$C9,$AF,$E2,$67,$98,$E9,$CE,$C6);
begin
  writeln('** Test SHAKE-256 / 4096 **');
  write('Test 1, bit length  0: ');
  err := SHA3_Init(state,__SHAKE_256);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg1[0], 0,  @buf, 4096);
  if err=0 then writeln(compmem(@dig1, @buf, sizeof(dig1)))
  else writeln('Err: ', err);

  write('Test 2, bit length  5: ');
  err := SHA3_Init(state,__SHAKE_256);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg2[0], 5,  @buf, 4096);
  if err=0 then writeln(compmem(@dig2, @buf, sizeof(dig2)))
  else writeln('Err: ', err);

  write('Test 3, bit length 30: ');
  err := SHA3_Init(state,__SHAKE_256);
  if err=0 then err := SHA3_Update(state, @msg3, 3);
  if err=0 then err := SHA3_FinalBit_LSB(state, msg3[3], 6,  @buf, 4096);
  if err=0 then writeln(compmem(@dig3, @buf, sizeof(dig3)))
  else writeln('Err: ', err);
end;

begin
  writeln('Test SHA3 / SHAKE for NIST test vectors   (c) 2014-2015 W. Ehrhardt');
  writeln('See http://csrc.nist.gov/groups/ST/toolkit/examples.html');
  writeln('-------------------------------------------------------------------');
  test_sha3_224;
  writeln;
  test_sha3_256;
  writeln;
  test_sha3_384;
  writeln;
  test_sha3_512;
  writeln;
  test_sha3_shake128;
  writeln;
  test_sha3_shake256;
end.

