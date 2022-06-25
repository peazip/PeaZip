program t_hmsha3;

{-HMAC-SHA3 tests, WE Sep.2015}

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  hash, hmac,
  sha3_512,sha3_384,
  sha3_256,sha3_224,
  BTypes, mem_util;


(*
function  CompMem(psrc, pdest: pointer; size: word): boolean;
  {-compare memory block}
var
  i,m: word;
  s: string;
begin
  writeln;
  s := '';
  m := $1000;
  for i:=1 to size do begin
    write(s,HexByte(pByte(psrc)^));
    inc(Ptr2Inc(psrc));
    if i mod m = 0 then writeln;
  end;
  compmem := true;
  writeln;
end;
*)

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
  d384: array[0.. 31] of byte = ($68,$d2,$dc,$f7,$fd,$4d,$dd,$0a,
                                 $22,$40,$c8,$a4,$37,$30,$5f,$61,
                                 $fb,$73,$34,$cf,$b5,$d0,$22,$6e,
                                 $1b,$c2,$7d,$c1,$0a,$2e,$72,$3a);
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

  d224: array[0.. 27] of byte = ($94,$ab,$eb,$26,$e3,$9a,$0d,$fc,
                                 $b6,$9c,$ab,$a8,$30,$3f,$79,$0e,
                                 $f9,$1b,$a4,$51,$85,$21,$a6,$56,
                                 $45,$62,$b9,$57);
  d256: array[0.. 31] of byte = ($6a,$3d,$4f,$bd,$df,$5d,$dc,$2a,
                                 $cf,$95,$cd,$13,$cd,$9f,$34,$a5,
                                 $a4,$2a,$57,$21,$66,$a7,$99,$06,
                                 $62,$84,$20,$11,$cf,$b0,$64,$4d);
  d384: array[0.. 47] of byte = ($82,$8f,$3f,$d4,$b3,$f4,$66,$7d,
                                 $28,$46,$10,$70,$79,$0e,$79,$2b,
                                 $2d,$92,$ee,$7e,$0a,$b8,$25,$3b,
                                 $ce,$1e,$95,$77,$13,$0b,$3b,$47,
                                 $98,$f8,$7f,$ff,$3a,$34,$4a,$0c,
                                 $e0,$64,$58,$c5,$b2,$74,$be,$67);
  d512: array[0.. 63] of byte = ($b3,$92,$60,$e0,$a6,$64,$a3,$da,
                                 $f6,$f8,$4d,$e2,$e4,$b4,$43,$d1,
                                 $c3,$b7,$84,$c2,$0d,$64,$0a,$14,
                                 $9f,$a4,$72,$90,$61,$77,$dc,$93,
                                 $b5,$aa,$cb,$e5,$a8,$5d,$57,$6a,
                                 $72,$46,$be,$c7,$ab,$bb,$28,$81,
                                 $0b,$92,$49,$50,$b1,$ff,$0c,$80,
                                 $c5,$cd,$9a,$a1,$46,$73,$b2,$43);
begin
  writeln('Test case 8:');

  hmac_init(ctx, ph224, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-224: ', compmem(@mac, @d224, sizeof(d224)));

  hmac_init(ctx, ph256, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-256: ', compmem(@mac, @d256, sizeof(d256)));

  hmac_init(ctx, ph384, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-384: ', compmem(@mac, @d384, sizeof(d384)));

  hmac_init(ctx, ph512, @key, sizeof(key));
  hmac_update(ctx, @data, sizeof(data));
  hmac_final_bits(ctx, mac, data[0], 5);
  writeln('  HMAC-SHA3-512: ', compmem(@mac, @d512, sizeof(d512)));

end;


var
  anyerror: boolean;
begin
  writeln('HMAC-SHA3 tests  -  (c) 2015 W. Ehrhardt');
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
  end;
end.
