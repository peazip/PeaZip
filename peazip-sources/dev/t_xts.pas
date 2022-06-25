{Test program AES XTS mode functions, we Oct.2007}

program T_XTS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
  wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      AES_Intv,
    {$else}
      AES_Intf,
    {$endif}
  {$else}
    aes_type, aes_xts,
  {$endif}
  mem_util;


var
  ctx: TAES_XTSContext;
  tmp: array[0..511] of byte;

{Test vectors from IEEE P1619}

{---------------------------------------------------------------------------}
procedure test_v01;
var
  pt: array[0..31] of byte;
  k1,k2: array[0..15] of byte;
  twk: TAESBlock;
  err: integer;
const
 ct: array[0..31] of byte = ($91,$7c,$f6,$9e,$bd,$68,$b2,$ec,
                             $9b,$9f,$e9,$a3,$ea,$dd,$a6,$92,
                             $cd,$43,$d2,$f5,$95,$98,$ed,$85,
                             $8c,$02,$c2,$65,$2f,$bf,$92,$2e);
begin
  fillchar(pt,sizeof(pt),0);
  fillchar(k1,sizeof(k1),0);
  fillchar(k2,sizeof(k2),0);
  fillchar(twk,sizeof(twk),0);
  writeln('Test vector 01');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));

  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v02;
const
  k1 : array[0..15] of byte = ($11,$11,$11,$11,$11,$11,$11,$11,
                               $11,$11,$11,$11,$11,$11,$11,$11);

  k2 : array[0..15] of byte = ($22,$22,$22,$22,$22,$22,$22,$22,
                               $22,$22,$22,$22,$22,$22,$22,$22);

  twk: TAESBlock = ($33,$33,$33,$33,$33,0,0,0,0,0,0,0,0,0,0,0);

  pt : array[0..31] of byte = ($44,$44,$44,$44,$44,$44,$44,$44,
                               $44,$44,$44,$44,$44,$44,$44,$44,
                               $44,$44,$44,$44,$44,$44,$44,$44,
                               $44,$44,$44,$44,$44,$44,$44,$44);

  ct : array[0..31] of byte = ($c4,$54,$18,$5e,$6a,$16,$93,$6e,
                               $39,$33,$40,$38,$ac,$ef,$83,$8b,
                               $fb,$18,$6f,$ff,$74,$80,$ad,$c4,
                               $28,$93,$82,$ec,$d6,$d3,$94,$f0);

var
  err: integer;
begin
  writeln('Test vector 02');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));

  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;



{---------------------------------------------------------------------------}
procedure test_v04;
const
  k1 : array[0..15] of byte = ($27,$18,$28,$18,$28,$45,$90,$45,$23,$53,$60,$28,$74,$71,$35,$26);

  k2 : array[0..15] of byte = ($31,$41,$59,$26,$53,$58,$97,$93,$23,$84,$62,$64,$33,$83,$27,$95);

  pt : array[0..511] of byte =(
         $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
         $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
         $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
         $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
         $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
         $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
         $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
         $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
         $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
         $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
         $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
         $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
         $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
         $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
         $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
         $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff,
         $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
         $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
         $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
         $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
         $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
         $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
         $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
         $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
         $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
         $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
         $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
         $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
         $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
         $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
         $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
         $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  ct : array[0..511] of byte = (
         $27,$a7,$47,$9b,$ef,$a1,$d4,$76,$48,$9f,$30,$8c,$d4,$cf,$a6,$e2,
         $a9,$6e,$4b,$be,$32,$08,$ff,$25,$28,$7d,$d3,$81,$96,$16,$e8,$9c,
         $c7,$8c,$f7,$f5,$e5,$43,$44,$5f,$83,$33,$d8,$fa,$7f,$56,$00,$00,
         $05,$27,$9f,$a5,$d8,$b5,$e4,$ad,$40,$e7,$36,$dd,$b4,$d3,$54,$12,
         $32,$80,$63,$fd,$2a,$ab,$53,$e5,$ea,$1e,$0a,$9f,$33,$25,$00,$a5,
         $df,$94,$87,$d0,$7a,$5c,$92,$cc,$51,$2c,$88,$66,$c7,$e8,$60,$ce,
         $93,$fd,$f1,$66,$a2,$49,$12,$b4,$22,$97,$61,$46,$ae,$20,$ce,$84,
         $6b,$b7,$dc,$9b,$a9,$4a,$76,$7a,$ae,$f2,$0c,$0d,$61,$ad,$02,$65,
         $5e,$a9,$2d,$c4,$c4,$e4,$1a,$89,$52,$c6,$51,$d3,$31,$74,$be,$51,
         $a1,$0c,$42,$11,$10,$e6,$d8,$15,$88,$ed,$e8,$21,$03,$a2,$52,$d8,
         $a7,$50,$e8,$76,$8d,$ef,$ff,$ed,$91,$22,$81,$0a,$ae,$b9,$9f,$91,
         $72,$af,$82,$b6,$04,$dc,$4b,$8e,$51,$bc,$b0,$82,$35,$a6,$f4,$34,
         $13,$32,$e4,$ca,$60,$48,$2a,$4b,$a1,$a0,$3b,$3e,$65,$00,$8f,$c5,
         $da,$76,$b7,$0b,$f1,$69,$0d,$b4,$ea,$e2,$9c,$5f,$1b,$ad,$d0,$3c,
         $5c,$cf,$2a,$55,$d7,$05,$dd,$cd,$86,$d4,$49,$51,$1c,$eb,$7e,$c3,
         $0b,$f1,$2b,$1f,$a3,$5b,$91,$3f,$9f,$74,$7a,$8a,$fd,$1b,$13,$0e,
         $94,$bf,$f9,$4e,$ff,$d0,$1a,$91,$73,$5c,$a1,$72,$6a,$cd,$0b,$19,
         $7c,$4e,$5b,$03,$39,$36,$97,$e1,$26,$82,$6f,$b6,$bb,$de,$8e,$cc,
         $1e,$08,$29,$85,$16,$e2,$c9,$ed,$03,$ff,$3c,$1b,$78,$60,$f6,$de,
         $76,$d4,$ce,$cd,$94,$c8,$11,$98,$55,$ef,$52,$97,$ca,$67,$e9,$f3,
         $e7,$ff,$72,$b1,$e9,$97,$85,$ca,$0a,$7e,$77,$20,$c5,$b3,$6d,$c6,
         $d7,$2c,$ac,$95,$74,$c8,$cb,$bc,$2f,$80,$1e,$23,$e5,$6f,$d3,$44,
         $b0,$7f,$22,$15,$4b,$eb,$a0,$f0,$8c,$e8,$89,$1e,$64,$3e,$d9,$95,
         $c9,$4d,$9a,$69,$c9,$f1,$b5,$f4,$99,$02,$7a,$78,$57,$2a,$ee,$bd,
         $74,$d2,$0c,$c3,$98,$81,$c2,$13,$ee,$77,$0b,$10,$10,$e4,$be,$a7,
         $18,$84,$69,$77,$ae,$11,$9f,$7a,$02,$3a,$b5,$8c,$ca,$0a,$d7,$52,
         $af,$e6,$56,$bb,$3c,$17,$25,$6a,$9f,$6e,$9b,$f1,$9f,$dd,$5a,$38,
         $fc,$82,$bb,$e8,$72,$c5,$53,$9e,$db,$60,$9e,$f4,$f7,$9c,$20,$3e,
         $bb,$14,$0f,$2e,$58,$3c,$b2,$ad,$15,$b4,$aa,$5b,$65,$50,$16,$a8,
         $44,$92,$77,$db,$d4,$77,$ef,$2c,$8d,$6c,$01,$7d,$b7,$38,$b1,$8d,
         $eb,$4a,$42,$7d,$19,$23,$ce,$3f,$f2,$62,$73,$57,$79,$a4,$18,$f2,
         $0a,$28,$2d,$f9,$20,$14,$7b,$ea,$be,$42,$1e,$e5,$31,$9d,$05,$68);

  twk: TAESBlock = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 04');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));

  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' * Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v10;
const
  k1 : array[0.. 31] of byte = ($27,$18,$28,$18,$28,$45,$90,$45,$23,$53,$60,$28,$74,$71,$35,$26,
                                $62,$49,$77,$57,$24,$70,$93,$69,$99,$59,$57,$49,$66,$96,$76,$27);

  k2 : array[0.. 31] of byte = ($31,$41,$59,$26,$53,$58,$97,$93,$23,$84,$62,$64,$33,$83,$27,$95,
                                $02,$88,$41,$97,$16,$93,$99,$37,$51,$05,$82,$09,$74,$94,$45,$92);

  pt : array[0..511] of byte =(
         $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
         $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
         $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
         $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
         $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
         $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
         $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
         $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
         $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
         $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
         $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
         $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
         $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
         $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
         $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
         $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff,
         $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
         $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
         $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
         $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
         $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
         $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
         $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
         $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
         $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
         $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
         $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
         $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
         $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
         $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
         $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
         $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  ct : array[0..511] of byte = (
         $1c,$3b,$3a,$10,$2f,$77,$03,$86,$e4,$83,$6c,$99,$e3,$70,$cf,$9b,
         $ea,$00,$80,$3f,$5e,$48,$23,$57,$a4,$ae,$12,$d4,$14,$a3,$e6,$3b,
         $5d,$31,$e2,$76,$f8,$fe,$4a,$8d,$66,$b3,$17,$f9,$ac,$68,$3f,$44,
         $68,$0a,$86,$ac,$35,$ad,$fc,$33,$45,$be,$fe,$cb,$4b,$b1,$88,$fd,
         $57,$76,$92,$6c,$49,$a3,$09,$5e,$b1,$08,$fd,$10,$98,$ba,$ec,$70,
         $aa,$a6,$69,$99,$a7,$2a,$82,$f2,$7d,$84,$8b,$21,$d4,$a7,$41,$b0,
         $c5,$cd,$4d,$5f,$ff,$9d,$ac,$89,$ae,$ba,$12,$29,$61,$d0,$3a,$75,
         $71,$23,$e9,$87,$0f,$8a,$cf,$10,$00,$02,$08,$87,$89,$14,$29,$ca,
         $2a,$3e,$7a,$7d,$7d,$f7,$b1,$03,$55,$16,$5c,$8b,$9a,$6d,$0a,$7d,
         $e8,$b0,$62,$c4,$50,$0d,$c4,$cd,$12,$0c,$0f,$74,$18,$da,$e3,$d0,
         $b5,$78,$1c,$34,$80,$3f,$a7,$54,$21,$c7,$90,$df,$e1,$de,$18,$34,
         $f2,$80,$d7,$66,$7b,$32,$7f,$6c,$8c,$d7,$55,$7e,$12,$ac,$3a,$0f,
         $93,$ec,$05,$c5,$2e,$04,$93,$ef,$31,$a1,$2d,$3d,$92,$60,$f7,$9a,
         $28,$9d,$6a,$37,$9b,$c7,$0c,$50,$84,$14,$73,$d1,$a8,$cc,$81,$ec,
         $58,$3e,$96,$45,$e0,$7b,$8d,$96,$70,$65,$5b,$a5,$bb,$cf,$ec,$c6,
         $dc,$39,$66,$38,$0a,$d8,$fe,$cb,$17,$b6,$ba,$02,$46,$9a,$02,$0a,
         $84,$e1,$8e,$8f,$84,$25,$20,$70,$c1,$3e,$9f,$1f,$28,$9b,$e5,$4f,
         $bc,$48,$14,$57,$77,$8f,$61,$60,$15,$e1,$32,$7a,$02,$b1,$40,$f1,
         $50,$5e,$b3,$09,$32,$6d,$68,$37,$8f,$83,$74,$59,$5c,$84,$9d,$84,
         $f4,$c3,$33,$ec,$44,$23,$88,$51,$43,$cb,$47,$bd,$71,$c5,$ed,$ae,
         $9b,$e6,$9a,$2f,$fe,$ce,$b1,$be,$c9,$de,$24,$4f,$be,$15,$99,$2b,
         $11,$b7,$7c,$04,$0f,$12,$bd,$8f,$6a,$97,$5a,$44,$a0,$f9,$0c,$29,
         $a9,$ab,$c3,$d4,$d8,$93,$92,$72,$84,$c5,$87,$54,$cc,$e2,$94,$52,
         $9f,$86,$14,$dc,$d2,$ab,$a9,$91,$92,$5f,$ed,$c4,$ae,$74,$ff,$ac,
         $6e,$33,$3b,$93,$eb,$4a,$ff,$04,$79,$da,$9a,$41,$0e,$44,$50,$e0,
         $dd,$7a,$e4,$c6,$e2,$91,$09,$00,$57,$5d,$a4,$01,$fc,$07,$05,$9f,
         $64,$5e,$8b,$7e,$9b,$fd,$ef,$33,$94,$30,$54,$ff,$84,$01,$14,$93,
         $c2,$7b,$34,$29,$ea,$ed,$b4,$ed,$53,$76,$44,$1a,$77,$ed,$43,$85,
         $1a,$d7,$7f,$16,$f5,$41,$df,$d2,$69,$d5,$0d,$6a,$5f,$14,$fb,$0a,
         $ab,$1c,$bb,$4c,$15,$50,$be,$97,$f7,$ab,$40,$66,$19,$3c,$4c,$aa,
         $77,$3d,$ad,$38,$01,$4b,$d2,$09,$2f,$a7,$55,$c8,$24,$bb,$5e,$54,
         $c4,$f3,$6f,$fd,$a9,$fc,$ea,$70,$b9,$c6,$e6,$93,$e1,$48,$c1,$51);

  twk: TAESBlock = ($ff,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 10');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));

  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v15;
const
  k1 : array[0..15] of byte = ($ff,$fe,$fd,$fc,$fb,$fa,$f9,$f8,
                               $f7,$f6,$f5,$f4,$f3,$f2,$f1,$f0);
  k2 : array[0..15] of byte = ($bf,$be,$bd,$bc,$bb,$ba,$b9,$b8,
                               $b7,$b6,$b5,$b4,$b3,$b2,$b1,$b0);
  pt : array[0..16] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                               $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10);
  ct : array[0..16] of byte = ($6c,$16,$25,$db,$46,$71,$52,$2d,
                               $3d,$75,$99,$60,$1d,$e7,$ca,$09,$ed);
  twk: TAESBlock = ($9a,$78,$56,$34,$12,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 15');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));
  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v16;
const
  k1 : array[0..15] of byte = ($ff,$fe,$fd,$fc,$fb,$fa,$f9,$f8,
                               $f7,$f6,$f5,$f4,$f3,$f2,$f1,$f0);
  k2 : array[0..15] of byte = ($bf,$be,$bd,$bc,$bb,$ba,$b9,$b8,
                               $b7,$b6,$b5,$b4,$b3,$b2,$b1,$b0);
  pt : array[0..17] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                               $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11);
  ct : array[0..17] of byte = ($d0,$69,$44,$4b,$7a,$7e,$0c,$ab,
                               $09,$e2,$44,$47,$d2,$4d,$eb,$1f,$ed,$bf);
  twk: TAESBlock = ($9a,$78,$56,$34,$12,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 16');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));
  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v17;
const
  k1 : array[0..15] of byte = ($ff,$fe,$fd,$fc,$fb,$fa,$f9,$f8,
                               $f7,$f6,$f5,$f4,$f3,$f2,$f1,$f0);
  k2 : array[0..15] of byte = ($bf,$be,$bd,$bc,$bb,$ba,$b9,$b8,
                               $b7,$b6,$b5,$b4,$b3,$b2,$b1,$b0);
  pt : array[0..18] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                               $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12);
  ct : array[0..18] of byte = ($e5,$df,$13,$51,$c0,$54,$4b,$a1,
                               $35,$0b,$33,$63,$cd,$8e,$f4,$be,$ed,$bf,$9d);
  twk: TAESBlock = ($9a,$78,$56,$34,$12,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 17');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));
  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


{---------------------------------------------------------------------------}
procedure test_v18;
const
  k1 : array[0..15] of byte = ($ff,$fe,$fd,$fc,$fb,$fa,$f9,$f8,
                               $f7,$f6,$f5,$f4,$f3,$f2,$f1,$f0);
  k2 : array[0..15] of byte = ($bf,$be,$bd,$bc,$bb,$ba,$b9,$b8,
                               $b7,$b6,$b5,$b4,$b3,$b2,$b1,$b0);
  pt : array[0..19] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                               $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13);
  ct : array[0..19] of byte = ($9d,$84,$c8,$13,$f7,$19,$aa,$2c,
                               $7b,$e3,$f6,$61,$71,$c7,$c5,$c2,$ed,$bf,$9d,$ac);
  twk: TAESBlock = ($9a,$78,$56,$34,$12,0,0,0,0,0,0,0,0,0,0,0);
var
  err: integer;
begin
  writeln('Test vector 18');
  err := AES_XTS_Init_Encr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Encr = ', err);
    halt;
  end;
  err := AES_XTS_Encrypt(@pt, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Encrypt = ', err);
    halt;
  end;
  writeln(' Enc: ',CompMem(@ct, @tmp, sizeof(ct)));
  err := AES_XTS_Init_Decr(k1,k2,sizeof(k1)*8,ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Init_Decr = ', err);
    halt;
  end;
  err := AES_XTS_Decrypt(@tmp, @tmp, sizeof(pt), twk, ctx);
  if err<>0 then begin
    writeln(' - Error AES_XTS_Decrypt = ', err);
    halt;
  end;
  writeln(' Dec: ',CompMem(@pt, @tmp, sizeof(pt)));
end;


begin
  writeln('Test program AES-XTS mode    (c) 2007 W.Ehrhardt');
  {$ifdef USEDLL}
    writeln('DLL Version: ',AES_DLL_Version);
  {$endif}
  writeln('Test vectors from IEEE P1619:');
  test_v01;
  test_v02;
  test_v04;
  test_v10;
  test_v15;
  test_v16;
  test_v17;
  test_v18;
end.
