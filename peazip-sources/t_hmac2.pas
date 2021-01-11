{-Test prog for HMACs using test cases from RFC4231}

program t_hmac2;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  hash,hmac,sha224,sha256,sha384,sha512,
  mem_util;


{rfc4231.txt}


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
  {$ifdef WINCRT}
    ScreenSize.Y := 50;  {D1: 50 lines screen}
  {$endif}
  writeln('HMAC test cases from RFC4231');
  FindHashDescriptors;
  TestCase1;
  TestCase2;
  TestCase3;
  TestCase4;
  TestCase5;
  TestCase6;
  TestCase7;
end.
