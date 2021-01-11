{-Test prog for AES_GCM, we 09.2010}

program T_AESGCM;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef BIT16}
{$N+}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  BTypes,
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      AES_Intv,
    {$else}
      AES_Intf,
    {$endif}
  {$else}
    AES_Type, AES_Base, AES_GCM,
  {$endif}
  Mem_Util;

var
  tag : TAESBlock;
  ctx : TAES_GCMContext;
  err : integer;
  pt  : array[0..511] of byte;
  ct  : array[0..511] of byte;
  fail: longint;

const
  print: boolean = false;


{---------------------------------------------------------------------------}
procedure single_test(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
    {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
                          pIV: pointer; IV_len: word;         {IV: address / length}
                         pAAD: pointer; aLen: word;           {AAD: address / length}
                          ctp: pointer; cLen: longint;        {ciphertext: address / length}
                          ptp: pointer; tn: integer);         {plaintext: address}
var
  lf: integer;
  sn: string[10];
begin
  str(tn:3,sn);
  sn := 'TV '+sn+': ';
  lf := 0;

  {-------------------------------------------------------------------------}
  fillchar(pt,sizeof(pt),0);
  fillchar(ct,sizeof(ct),0);
  err := AES_GCM_Dec_Veri(ptag,tLen,Key,KBits,pIV,IV_Len,pAAD,aLen,ctp,cLen,@pt,ctx);
  if err<>0 then begin
    inc(lf);
    writeln(sn,'AES_GCM_Dec_Veri error: ',err);
  end
  else begin
    if not compmem(@pt, ptp, cLen) then begin
      writeln(sn,'AES_GCM_Dec_Veri - plaintext does not match');
      inc(lf);
    end;
  end;

  {-------------------------------------------------------------------------}
  fillchar(pt,sizeof(pt),0);
  fillchar(ct,sizeof(ct),0);
  err := AES_GCM_Enc_Auth(tag,Key,KBits,pIV,IV_Len,pAAD,aLen,ptp,cLen,@ct,ctx);
  if err<>0 then begin
    inc(lf);
    writeln(sn,'AES_GCM_Enc_Auth error: ',err);
  end
  else begin
    if not compmem(@tag, ptag, tLen) then begin
      writeln(sn,'AES_GCM_Enc_Auth - Tag does not match');
      inc(lf);
    end;
    if not compmem(@ct,  ctp, cLen) then begin
      writeln(sn,'AES_GCM_Enc_Auth - Ciphertext does not match');
      inc(lf);
    end;
  end;

  {-------------------------------------------------------------------------}
  fillchar(pt,sizeof(pt),0);
  fillchar(ct,sizeof(ct),0);
  err := AES_GCM_Init(Key, KBits, ctx);

  if err<>0 then   writeln(sn,'Enc - AES_GCM_Init     error: ',err);
  if err=0 then begin
    err := AES_GCM_Reset_IV(pIV, IV_Len, ctx);
    if err<>0 then writeln(sn,'Enc - AES_GCM_Reset_IV error: ',err);
  end;

  if err=0 then begin
    err := AES_GCM_Add_AAD(pAAD, aLen, ctx);
    if err<>0 then writeln(sn,'Enc - AES_GCM_Add_AAD  error: ',err);
  end;

  if err=0 then begin
    err := AES_GCM_Encrypt(ptp, @ct, cLen, ctx);
    if err<>0 then writeln(sn,'Enc - AES_GCM_Encrypt  error: ',err);
  end;
  if err=0 then begin
    err := AES_GCM_Final(tag, ctx);
    if err<>0 then writeln(sn,'Enc - AES_GCM_Final    error: ',err);
  end;
  if err=0 then begin
    if not compmem(@tag, ptag, tLen) then begin
      writeln(sn,'Enc - Tag does not match');
      inc(lf);
    end;
    if not compmem(@ct,  ctp, cLen) then begin
      writeln(sn,'Enc - Ciphertext does not match');
      inc(lf);
    end;
  end
  else inc(lf);

  {-------------------------------------------------------------------------}
  fillchar(pt,sizeof(pt),0);
  fillchar(ct,sizeof(ct),0);
  err := AES_GCM_Init(Key, KBits, ctx);
  if err<>0 then   writeln(sn,'Dec - AES_GCM_Init     error: ',err);

  if err=0 then begin
    err := AES_GCM_Reset_IV(pIV, IV_Len, ctx);
    if err<>0 then writeln(sn,'Dec - AES_GCM_Reset_IV error: ',err);
  end;

  if err=0 then begin
    err := AES_GCM_Add_AAD(pAAD, aLen, ctx);
    if err<>0 then writeln(sn,'Dec - AES_GCM_Add_AAD  error: ',err);
  end;

  if err=0 then begin
    err := AES_GCM_Decrypt(ctp, @pt, cLen, ctx);
    if err<>0 then writeln(sn,'Dec - AES_GCM_Encrypt  error: ',err);
  end;
  if err=0 then begin
    err := AES_GCM_Final(tag, ctx);
    if err<>0 then writeln(sn,'Dec - AES_GCM_Final    error: ',err);
  end;
  if err=0 then begin
    if not compmem(@tag, ptag, tLen) then begin
      writeln(sn,'Dec - Tag does not match');
      inc(lf);
    end;
    if not compmem(@pt, ptp, cLen) then begin
      writeln(sn,'Dec - Plaintext does not match');
      inc(lf);
    end;
  end
  else inc(lf);

  if lf<>0 then inc(fail);
end;



{---------------------------------------------------------------------------}
procedure testspec;
const
  K01: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I01: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  T01: array[0..15] of byte = ($58,$e2,$fc,$ce,$fa,$7e,$30,$61,
                               $36,$7f,$1d,$57,$a4,$e7,$45,$5a);


  K02: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  P02: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I02: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  C02: array[0..15] of byte = ($03,$88,$da,$ce,$60,$b6,$a3,$92,
                               $f3,$28,$c2,$b9,$71,$b2,$fe,$78);
  T02: array[0..15] of byte = ($ab,$6e,$47,$d4,$2c,$ec,$13,$bd,
                               $f5,$3a,$67,$b2,$12,$57,$bd,$df);


  K03: array[0..15] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P03: array[0..63] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39,$1a,$af,$d2,$55);
  I03: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C03: array[0..63] of byte = ($42,$83,$1e,$c2,$21,$77,$74,$24,
                               $4b,$72,$21,$b7,$84,$d0,$d4,$9c,
                               $e3,$aa,$21,$2f,$2c,$02,$a4,$e0,
                               $35,$c1,$7e,$23,$29,$ac,$a1,$2e,
                               $21,$d5,$14,$b2,$54,$66,$93,$1c,
                               $7d,$8f,$6a,$5a,$ac,$84,$aa,$05,
                               $1b,$a3,$0b,$39,$6a,$0a,$ac,$97,
                               $3d,$58,$e0,$91,$47,$3f,$59,$85);
  T03: array[0..15] of byte = ($4d,$5c,$2a,$f3,$27,$cd,$64,$a6,
                               $2c,$f3,$5a,$bd,$2b,$a6,$fa,$b4);


  K04: array[0..15] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P04: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A04: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I04: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C04: array[0..59] of byte = ($42,$83,$1e,$c2,$21,$77,$74,$24,
                               $4b,$72,$21,$b7,$84,$d0,$d4,$9c,
                               $e3,$aa,$21,$2f,$2c,$02,$a4,$e0,
                               $35,$c1,$7e,$23,$29,$ac,$a1,$2e,
                               $21,$d5,$14,$b2,$54,$66,$93,$1c,
                               $7d,$8f,$6a,$5a,$ac,$84,$aa,$05,
                               $1b,$a3,$0b,$39,$6a,$0a,$ac,$97,
                               $3d,$58,$e0,$91);
  T04: array[0..15] of byte = ($5b,$c9,$4f,$bc,$32,$21,$a5,$db,
                               $94,$fa,$e9,$5a,$e7,$12,$1a,$47);


  K05: array[0..15] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P05: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A05: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I05: array[0..07] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad);
  C05: array[0..59] of byte = ($61,$35,$3b,$4c,$28,$06,$93,$4a,
                               $77,$7f,$f5,$1f,$a2,$2a,$47,$55,
                               $69,$9b,$2a,$71,$4f,$cd,$c6,$f8,
                               $37,$66,$e5,$f9,$7b,$6c,$74,$23,
                               $73,$80,$69,$00,$e4,$9f,$24,$b2,
                               $2b,$09,$75,$44,$d4,$89,$6b,$42,
                               $49,$89,$b5,$e1,$eb,$ac,$0f,$07,
                               $c2,$3f,$45,$98);
  T05: array[0..15] of byte = ($36,$12,$d2,$e7,$9e,$3b,$07,$85,
                               $56,$1b,$e1,$4a,$ac,$a2,$fc,$cb);


  K06: array[0..15] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P06: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A06: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I06: array[0..59] of byte = ($93,$13,$22,$5d,$f8,$84,$06,$e5,
                               $55,$90,$9c,$5a,$ff,$52,$69,$aa,
                               $6a,$7a,$95,$38,$53,$4f,$7d,$a1,
                               $e4,$c3,$03,$d2,$a3,$18,$a7,$28,
                               $c3,$c0,$c9,$51,$56,$80,$95,$39,
                               $fc,$f0,$e2,$42,$9a,$6b,$52,$54,
                               $16,$ae,$db,$f5,$a0,$de,$6a,$57,
                               $a6,$37,$b3,$9b);
  C06: array[0..59] of byte = ($8c,$e2,$49,$98,$62,$56,$15,$b6,
                               $03,$a0,$33,$ac,$a1,$3f,$b8,$94,
                               $be,$91,$12,$a5,$c3,$a2,$11,$a8,
                               $ba,$26,$2a,$3c,$ca,$7e,$2c,$a7,
                               $01,$e4,$a9,$a4,$fb,$a4,$3c,$90,
                               $cc,$dc,$b2,$81,$d4,$8c,$7c,$6f,
                               $d6,$28,$75,$d2,$ac,$a4,$17,$03,
                               $4c,$34,$ae,$e5);
  T06: array[0..15] of byte = ($61,$9c,$c5,$ae,$ff,$fe,$0b,$fa,
                               $46,$2a,$f4,$3c,$16,$99,$d0,$50);


  K07: array[0..23] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I07: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  T07: array[0..15] of byte = ($cd,$33,$b2,$8a,$c7,$73,$f7,$4b,
                               $a0,$0e,$d1,$f3,$12,$57,$24,$35);


  K08: array[0..23] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  P08: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I08: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  C08: array[0..15] of byte = ($98,$e7,$24,$7c,$07,$f0,$fe,$41,
                               $1c,$26,$7e,$43,$84,$b0,$f6,$00);
  T08: array[0..15] of byte = ($2f,$f5,$8d,$80,$03,$39,$27,$ab,
                               $8e,$f4,$d4,$58,$75,$14,$f0,$fb);

  K09: array[0..23] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c);
  P09: array[0..63] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39,$1a,$af,$d2,$55);
  I09: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C09: array[0..63] of byte = ($39,$80,$ca,$0b,$3c,$00,$e8,$41,
                               $eb,$06,$fa,$c4,$87,$2a,$27,$57,
                               $85,$9e,$1c,$ea,$a6,$ef,$d9,$84,
                               $62,$85,$93,$b4,$0c,$a1,$e1,$9c,
                               $7d,$77,$3d,$00,$c1,$44,$c5,$25,
                               $ac,$61,$9d,$18,$c8,$4a,$3f,$47,
                               $18,$e2,$44,$8b,$2f,$e3,$24,$d9,
                               $cc,$da,$27,$10,$ac,$ad,$e2,$56);
  T09: array[0..15] of byte = ($99,$24,$a7,$c8,$58,$73,$36,$bf,
                               $b1,$18,$02,$4d,$b8,$67,$4a,$14);


  K10: array[0..23] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c);
  P10: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A10: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I10: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C10: array[0..59] of byte = ($39,$80,$ca,$0b,$3c,$00,$e8,$41,
                               $eb,$06,$fa,$c4,$87,$2a,$27,$57,
                               $85,$9e,$1c,$ea,$a6,$ef,$d9,$84,
                               $62,$85,$93,$b4,$0c,$a1,$e1,$9c,
                               $7d,$77,$3d,$00,$c1,$44,$c5,$25,
                               $ac,$61,$9d,$18,$c8,$4a,$3f,$47,
                               $18,$e2,$44,$8b,$2f,$e3,$24,$d9,
                               $cc,$da,$27,$10);
  T10: array[0..15] of byte = ($25,$19,$49,$8e,$80,$f1,$47,$8f,
                               $37,$ba,$55,$bd,$6d,$27,$61,$8c);


  K11: array[0..23] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c);
  P11: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A11: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I11: array[0.. 7] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad);
  C11: array[0..59] of byte = ($0f,$10,$f5,$99,$ae,$14,$a1,$54,
                               $ed,$24,$b3,$6e,$25,$32,$4d,$b8,
                               $c5,$66,$63,$2e,$f2,$bb,$b3,$4f,
                               $83,$47,$28,$0f,$c4,$50,$70,$57,
                               $fd,$dc,$29,$df,$9a,$47,$1f,$75,
                               $c6,$65,$41,$d4,$d4,$da,$d1,$c9,
                               $e9,$3a,$19,$a5,$8e,$8b,$47,$3f,
                               $a0,$f0,$62,$f7);
  T11: array[0..15] of byte = ($65,$dc,$c5,$7f,$cf,$62,$3a,$24,
                               $09,$4f,$cc,$a4,$0d,$35,$33,$f8);


  K12: array[0..23] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c);
  P12: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A12: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I12: array[0..59] of byte = ($93,$13,$22,$5d,$f8,$84,$06,$e5,
                               $55,$90,$9c,$5a,$ff,$52,$69,$aa,
                               $6a,$7a,$95,$38,$53,$4f,$7d,$a1,
                               $e4,$c3,$03,$d2,$a3,$18,$a7,$28,
                               $c3,$c0,$c9,$51,$56,$80,$95,$39,
                               $fc,$f0,$e2,$42,$9a,$6b,$52,$54,
                               $16,$ae,$db,$f5,$a0,$de,$6a,$57,
                               $a6,$37,$b3,$9b);
  C12: array[0..59] of byte = ($d2,$7e,$88,$68,$1c,$e3,$24,$3c,
                               $48,$30,$16,$5a,$8f,$dc,$f9,$ff,
                               $1d,$e9,$a1,$d8,$e6,$b4,$47,$ef,
                               $6e,$f7,$b7,$98,$28,$66,$6e,$45,
                               $81,$e7,$90,$12,$af,$34,$dd,$d9,
                               $e2,$f0,$37,$58,$9b,$29,$2d,$b3,
                               $e6,$7c,$03,$67,$45,$fa,$22,$e7,
                               $e9,$b7,$37,$3b);
  T12: array[0..15] of byte = ($dc,$f5,$66,$ff,$29,$1c,$25,$bb,
                               $b8,$56,$8f,$c3,$d3,$76,$a6,$d9);


  K13: array[0..31] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I13: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  T13: array[0..15] of byte = ($53,$0f,$8a,$fb,$c7,$45,$36,$b9,
                               $a9,$63,$b4,$f1,$c4,$cb,$73,$8b);


  K14: array[0..31] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  P14: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);
  I14: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);
  C14: array[0..15] of byte = ($ce,$a7,$40,$3d,$4d,$60,$6b,$6e,
                               $07,$4e,$c5,$d3,$ba,$f3,$9d,$18);
  T14: array[0..15] of byte = ($d0,$d1,$c8,$a7,$99,$99,$6b,$f0,
                               $26,$5b,$98,$b5,$d4,$8a,$b9,$19);


  K15: array[0..31] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);


  P15: array[0..63] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39,$1a,$af,$d2,$55);
  I15: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C15: array[0..63] of byte = ($52,$2d,$c1,$f0,$99,$56,$7d,$07,
                               $f4,$7f,$37,$a3,$2a,$84,$42,$7d,
                               $64,$3a,$8c,$dc,$bf,$e5,$c0,$c9,
                               $75,$98,$a2,$bd,$25,$55,$d1,$aa,
                               $8c,$b0,$8e,$48,$59,$0d,$bb,$3d,
                               $a7,$b0,$8b,$10,$56,$82,$88,$38,
                               $c5,$f6,$1e,$63,$93,$ba,$7a,$0a,
                               $bc,$c9,$f6,$62,$89,$80,$15,$ad);
  T15: array[0..15] of byte = ($b0,$94,$da,$c5,$d9,$34,$71,$bd,
                               $ec,$1a,$50,$22,$70,$e3,$cc,$6c);


  K16: array[0..31] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P16: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A16: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I16: array[0..11] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad,
                               $de,$ca,$f8,$88);
  C16: array[0..59] of byte = ($52,$2d,$c1,$f0,$99,$56,$7d,$07,
                               $f4,$7f,$37,$a3,$2a,$84,$42,$7d,
                               $64,$3a,$8c,$dc,$bf,$e5,$c0,$c9,
                               $75,$98,$a2,$bd,$25,$55,$d1,$aa,
                               $8c,$b0,$8e,$48,$59,$0d,$bb,$3d,
                               $a7,$b0,$8b,$10,$56,$82,$88,$38,
                               $c5,$f6,$1e,$63,$93,$ba,$7a,$0a,
                               $bc,$c9,$f6,$62);
  T16: array[0..15] of byte = ($76,$fc,$6e,$ce,$0f,$4e,$17,$68,
                               $cd,$df,$88,$53,$bb,$2d,$55,$1b);


  K17: array[0..31] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P17: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A17: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I17: array[0.. 7] of byte = ($ca,$fe,$ba,$be,$fa,$ce,$db,$ad);
  C17: array[0..59] of byte = ($c3,$76,$2d,$f1,$ca,$78,$7d,$32,
                               $ae,$47,$c1,$3b,$f1,$98,$44,$cb,
                               $af,$1a,$e1,$4d,$0b,$97,$6a,$fa,
                               $c5,$2f,$f7,$d7,$9b,$ba,$9d,$e0,
                               $fe,$b5,$82,$d3,$39,$34,$a4,$f0,
                               $95,$4c,$c2,$36,$3b,$c7,$3f,$78,
                               $62,$ac,$43,$0e,$64,$ab,$e4,$99,
                               $f4,$7c,$9b,$1f);
  T17: array[0..15] of byte = ($3a,$33,$7d,$bf,$46,$a7,$92,$c4,
                               $5e,$45,$49,$13,$fe,$2e,$a8,$f2);


  K18: array[0..31] of byte = ($fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08,
                               $fe,$ff,$e9,$92,$86,$65,$73,$1c,
                               $6d,$6a,$8f,$94,$67,$30,$83,$08);
  P18: array[0..59] of byte = ($d9,$31,$32,$25,$f8,$84,$06,$e5,
                               $a5,$59,$09,$c5,$af,$f5,$26,$9a,
                               $86,$a7,$a9,$53,$15,$34,$f7,$da,
                               $2e,$4c,$30,$3d,$8a,$31,$8a,$72,
                               $1c,$3c,$0c,$95,$95,$68,$09,$53,
                               $2f,$cf,$0e,$24,$49,$a6,$b5,$25,
                               $b1,$6a,$ed,$f5,$aa,$0d,$e6,$57,
                               $ba,$63,$7b,$39);
  A18: array[0..19] of byte = ($fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $fe,$ed,$fa,$ce,$de,$ad,$be,$ef,
                               $ab,$ad,$da,$d2);
  I18: array[0..59] of byte = ($93,$13,$22,$5d,$f8,$84,$06,$e5,
                               $55,$90,$9c,$5a,$ff,$52,$69,$aa,
                               $6a,$7a,$95,$38,$53,$4f,$7d,$a1,
                               $e4,$c3,$03,$d2,$a3,$18,$a7,$28,
                               $c3,$c0,$c9,$51,$56,$80,$95,$39,
                               $fc,$f0,$e2,$42,$9a,$6b,$52,$54,
                               $16,$ae,$db,$f5,$a0,$de,$6a,$57,
                               $a6,$37,$b3,$9b);
  C18: array[0..59] of byte = ($5a,$8d,$ef,$2f,$0c,$9e,$53,$f1,
                               $f7,$5d,$78,$53,$65,$9e,$2a,$20,
                               $ee,$b2,$b2,$2a,$af,$de,$64,$19,
                               $a0,$58,$ab,$4f,$6f,$74,$6b,$f4,
                               $0f,$c0,$c3,$b7,$80,$f2,$44,$45,
                               $2d,$a3,$eb,$f1,$c5,$d8,$2c,$de,
                               $a2,$41,$89,$97,$20,$0e,$f8,$2e,
                               $44,$ae,$7e,$3f);
  T18: array[0..15] of byte = ($a4,$4a,$82,$66,$ee,$1c,$8e,$b0,
                               $c8,$b5,$d4,$cf,$5a,$e9,$f1,$9a);

begin
  fail := 0;
  writeln('Test cases AES_GCM from GCM Spec');
  single_test(@T01,16,K01,8*sizeof(K01),@I01,sizeof(I01),nil,0,nil,0,nil,01);
  single_test(@T02,16,K02,8*sizeof(K02),@I02,sizeof(I02),nil,0,@C02,sizeof(C02),@P02,02);
  single_test(@T03,16,K03,8*sizeof(K03),@I03,sizeof(I03),nil,0,@C03,sizeof(C03),@P03,03);
  single_test(@T04,16,K04,8*sizeof(K04),@I04,sizeof(I04),@A04,sizeof(A04),@C04,sizeof(C04),@P04,04);
  single_test(@T05,16,K05,8*sizeof(K05),@I05,sizeof(I05),@A05,sizeof(A05),@C05,sizeof(C05),@P05,05);
  single_test(@T06,16,K06,8*sizeof(K06),@I06,sizeof(I06),@A06,sizeof(A06),@C06,sizeof(C06),@P06,06);
  single_test(@T07,16,K07,8*sizeof(K07),@I07,sizeof(I07),nil,0,nil,0,nil,07);
  single_test(@T08,16,K08,8*sizeof(K08),@I08,sizeof(I08),nil,0,@C08,sizeof(C08),@P08,08);
  single_test(@T09,16,K09,8*sizeof(K09),@I09,sizeof(I09),nil,0,@C09,sizeof(C09),@P09,09);
  single_test(@T10,16,K10,8*sizeof(K10),@I10,sizeof(I10),@A10,sizeof(A10),@C10,sizeof(C10),@P10,10);
  single_test(@T11,16,K11,8*sizeof(K11),@I11,sizeof(I11),@A11,sizeof(A11),@C11,sizeof(C11),@P11,11);
  single_test(@T12,16,K12,8*sizeof(K12),@I12,sizeof(I12),@A12,sizeof(A12),@C12,sizeof(C12),@P12,12);
  single_test(@T13,16,K13,8*sizeof(K13),@I13,sizeof(I13),nil,0,nil,0,nil,13);
  single_test(@T14,16,K14,8*sizeof(K14),@I14,sizeof(I14),nil,0,@C14,sizeof(C14),@P14,14);
  single_test(@T15,16,K15,8*sizeof(K15),@I15,sizeof(I15),nil,0,@C15,sizeof(C15),@P15,15);
  single_test(@T16,16,K16,8*sizeof(K16),@I16,sizeof(I16),@A16,sizeof(A16),@C16,sizeof(C16),@P16,16);
  single_test(@T17,16,K17,8*sizeof(K17),@I17,sizeof(I17),@A17,sizeof(A17),@C17,sizeof(C17),@P17,17);
  single_test(@T18,16,K18,8*sizeof(K18),@I18,sizeof(I18),@A18,sizeof(A18),@C18,sizeof(C18),@P18,18);
  if fail=0 then writeln('All tests passed.')
  else writeln('*** Number of failed tests: ', fail);
end;

{---------------------------------------------------------------------------}
procedure tsd_test;
  {-Reproduce AES part of Tom St Denis' GCM_TV.TXT, LTC V1.18}
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
  buf32: array[0..31] of byte = ($92,$4e,$17,$8a,$17,$fa,$1c,$a0,
                                 $e7,$48,$6f,$04,$04,$12,$3b,$91,
                                 $db,$f7,$97,$bb,$9d,$bd,$e9,$b1,
                                 $d4,$8d,$5c,$7f,$53,$16,$59,$12);


  tag32: array[0..15] of byte = ($10,$f9,$72,$b6,$f9,$e0,$a3,$c1,
                                 $cf,$9c,$cf,$56,$54,$3d,$ca,$79);

var
  err,n: integer;
  ctx: TAES_GCMContext;
  key, tag: TAESBlock;
  buf: array[0..63] of byte;
begin
  {Note: Contrary to what Tom writes in GCM_TV.TXT the length of nonce=IV is}
  {NOT fixed=13, but varies the same way as the header and plaintext length!}
  writeln('Test AES part of Tom St Denis'' GCM_TV.TXT (LTC V1.18)');
  {Uppercase from HexStr}
  HexUpper := true;
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=1 to 32 do begin
    err := AES_GCM_Init(key, 128, ctx);
    if err=0 then err := AES_GCM_Reset_IV(@hex32, n, ctx);
    if err=0 then err := AES_GCM_Add_AAD(@hex32,n,ctx);
    if err=0 then err := AES_GCM_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then err := AES_GCM_Final(tag, ctx);
    if err=0 then begin
      if print then writeln(n:3,': ', HexStr(@buf,n), ', ', HexStr(@tag,16));
      {key for step n>1 is the tag of the previous step repeated}
      key := tag;
    end
    else begin
      writeln('Error ',err);
      exit;
    end;
  end;
  {compare final values}
  writeln('buf32 compares: ', compmem(@buf32, @buf, sizeof(buf32)):5);
  writeln('tag32 compares: ', compmem(@tag32, @tag, sizeof(tag32)):5);
end;


{---------------------------------------------------------------------------}
procedure test_glad2;
const
  K01: array[0..31] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  I01: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);

  P01: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  C01: array[0..15] of byte = ($ce,$a7,$40,$3d,$4d,$60,$6b,$6e,
                               $07,$4e,$c5,$d3,$ba,$f3,$9d,$18);

  T01: array[0..15] of byte = ($d0,$d1,$c8,$a7,$99,$99,$6b,$f0,
                               $26,$5b,$98,$b5,$d4,$8a,$b9,$19);


  K02: array[0..31] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  I02: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);

  H02: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  T02: array[0..15] of byte = ($2d,$45,$55,$2d,$85,$75,$92,$2b,
                               $3c,$a3,$cc,$53,$84,$42,$fa,$26);


  K03: array[0..31] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  I03: array[0..11] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00);

  H03: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  P03: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                               $00,$00,$00,$00,$00,$00,$00,$00);

  C03: array[0..15] of byte = ($ce,$a7,$40,$3d,$4d,$60,$6b,$6e,
                               $07,$4e,$c5,$d3,$ba,$f3,$9d,$18);

  T03: array[0..15] of byte = ($ae,$9b,$17,$71,$db,$a9,$cf,$62,
                               $b3,$9b,$e0,$17,$94,$03,$30,$b4);


  K04: array[0..31] of byte = ($fb,$76,$15,$b2,$3d,$80,$89,$1d,
                               $d4,$70,$98,$0b,$c7,$95,$84,$c8,
                               $b2,$fb,$64,$ce,$60,$97,$8f,$4d,
                               $17,$fc,$e4,$5a,$49,$e8,$30,$b7);

  I04: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,
                               $02,$da,$7d,$6f);

  P04: array[0..15] of byte = ($a8,$45,$34,$8e,$c8,$c5,$b5,$f1,
                               $26,$f5,$0e,$76,$fe,$fd,$1b,$1e);

  C04: array[0..15] of byte = ($5d,$f5,$d1,$fa,$bc,$bb,$dd,$05,
                               $15,$38,$25,$24,$44,$17,$87,$04);

  T04: array[0..15] of byte = ($4c,$43,$cc,$e5,$a5,$74,$d8,$a8,
                               $8b,$43,$d4,$35,$3b,$d6,$0f,$9f);


  K05: array[0..31] of byte = ($40,$41,$42,$43,$44,$45,$46,$47,
                               $48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
                               $50,$51,$52,$53,$54,$55,$56,$57,
                               $58,$59,$5a,$5b,$5c,$5d,$5e,$5f);

  I05: array[0..11] of byte = ($10,$11,$12,$13,$14,$15,$16,$17,
                               $18,$19,$1a,$1b);

  H05: array[0..19] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                               $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                               $10,$11,$12,$13);

  P05: array[0..23] of byte = ($20,$21,$22,$23,$24,$25,$26,$27,
                               $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
                               $30,$31,$32,$33,$34,$35,$36,$37);

  C05: array[0..23] of byte = ($59,$1b,$1f,$f2,$72,$b4,$32,$04,
                               $86,$8f,$fc,$7b,$c7,$d5,$21,$99,
                               $35,$26,$b6,$fa,$32,$24,$7c,$3c);

  T05: array[0..15] of byte = ($7d,$e1,$2a,$56,$70,$e5,$70,$d8,
                               $ca,$e6,$24,$a1,$6d,$f0,$9c,$08);


  K07: array[0..31] of byte = ($40,$41,$42,$43,$44,$45,$46,$47,
                               $48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
                               $50,$51,$52,$53,$54,$55,$56,$57,
                               $58,$59,$5a,$5b,$5c,$5d,$5e,$5f);

  I07: array[0..11] of byte = ($10,$11,$12,$13,$14,$15,$16,$17,
                               $18,$19,$1a,$1b);

  H07: array[0..31] of byte = ($20,$21,$22,$23,$24,$25,$26,$27,
                               $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
                               $30,$31,$32,$33,$34,$35,$36,$37,
                               $38,$39,$3a,$3b,$3c,$3d,$3e,$3f);

  P07: array[0..255] of byte =($00,$01,$02,$03,$04,$05,$06,$07,
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
                               $a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
                               $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,
                               $b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
                               $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,
                               $c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
                               $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,
                               $d8,$d9,$da,$db,$dc,$dd,$de,$df,
                               $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,
                               $e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
                               $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,
                               $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  C07: array[0..255] of byte =($79,$3b,$3f,$d2,$52,$94,$12,$24,
                               $a6,$af,$dc,$5b,$e7,$f5,$01,$b9,
                               $15,$06,$96,$da,$12,$04,$5c,$1c,
                               $60,$77,$d3,$ca,$c7,$74,$ac,$cf,
                               $c3,$d5,$30,$d8,$48,$d6,$65,$d8,
                               $1a,$49,$cb,$b5,$00,$b8,$8b,$bb,
                               $62,$4a,$e6,$1d,$16,$67,$22,$9c,
                               $30,$2d,$c6,$ff,$0b,$b4,$d7,$0b,
                               $db,$bc,$85,$66,$d6,$f5,$b1,$58,
                               $da,$99,$a2,$ff,$2e,$01,$dd,$a6,
                               $29,$b8,$9c,$34,$ad,$1e,$5f,$eb,
                               $a7,$0e,$7a,$ae,$43,$28,$28,$9c,
                               $36,$29,$b0,$58,$83,$50,$58,$1c,
                               $a8,$b9,$7c,$cf,$12,$58,$fa,$3b,
                               $be,$2c,$50,$26,$04,$7b,$a7,$26,
                               $48,$96,$9c,$ff,$8b,$a1,$0a,$e3,
                               $0e,$05,$93,$5d,$f0,$c6,$93,$74,
                               $18,$92,$b7,$6f,$af,$67,$13,$3a,
                               $bd,$2c,$f2,$03,$11,$21,$bd,$8b,
                               $b3,$81,$27,$a4,$d2,$ee,$de,$ea,
                               $13,$27,$64,$94,$f4,$02,$cd,$7c,
                               $10,$7f,$b3,$ec,$3b,$24,$78,$48,
                               $34,$33,$8e,$55,$43,$62,$87,$09,
                               $2a,$c4,$a2,$6f,$5e,$a7,$ea,$4a,
                               $d6,$8d,$73,$15,$16,$39,$b0,$5b,
                               $24,$e6,$8b,$98,$16,$d1,$39,$83,
                               $76,$d8,$e4,$13,$85,$94,$75,$8d,
                               $b9,$ad,$3b,$40,$92,$59,$b2,$6d,
                               $cf,$c0,$6e,$72,$2b,$e9,$87,$b3,
                               $76,$7f,$70,$a7,$b8,$56,$b7,$74,
                               $b1,$ba,$26,$85,$b3,$68,$09,$14,
                               $29,$fc,$cb,$8d,$cd,$de,$09,$e4);

  T07: array[0..15] of byte = ($87,$ec,$83,$7a,$bf,$53,$28,$55,
                               $b2,$ce,$a1,$69,$d6,$94,$3f,$cd);


  K08: array[0..31] of byte = ($fb,$76,$15,$b2,$3d,$80,$89,$1d,
                               $d4,$70,$98,$0b,$c7,$95,$84,$c8,
                               $b2,$fb,$64,$ce,$60,$97,$87,$8d,
                               $17,$fc,$e4,$5a,$49,$e8,$30,$b7);

  I08: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,
                               $02,$da,$7d,$6f);

  H08: array[0.. 0] of byte = ($36);

  P08: array[0.. 0] of byte = ($a9);

  C08: array[0.. 0] of byte = ($0a);

  T08: array[0..15] of byte = ($be,$98,$7d,$00,$9a,$4b,$34,$9a,
                               $a8,$0c,$b9,$c4,$eb,$c1,$e9,$f4);


  K09: array[0..31] of byte = ($f8,$d4,$76,$cf,$d6,$46,$ea,$6c,
                               $23,$84,$cb,$1c,$27,$d6,$19,$5d,
                               $fe,$f1,$a9,$f3,$7b,$9c,$8d,$21,
                               $a7,$9c,$21,$f8,$cb,$90,$d2,$89);

  I09: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,
                               $02,$da,$7d,$6f);

  H09: array[0..19] of byte = ($7b,$d8,$59,$a2,$47,$96,$1a,$21,
                               $82,$3b,$38,$0e,$9f,$e8,$b6,$50,
                               $82,$ba,$61,$d3);

  P09: array[0..19] of byte = ($90,$ae,$61,$cf,$7b,$ae,$bd,$4c,
                               $ad,$e4,$94,$c5,$4a,$29,$ae,$70,
                               $26,$9a,$ec,$71);

  C09: array[0..19] of byte = ($ce,$20,$27,$b4,$7a,$84,$32,$52,
                               $01,$34,$65,$83,$4d,$75,$fd,$0f,
                               $07,$29,$75,$2e);

  T09: array[0..15] of byte = ($ac,$d8,$83,$38,$37,$ab,$0e,$de,
                               $84,$f4,$74,$8d,$a8,$89,$9c,$15);


  K10: array[0..31] of byte = ($db,$bc,$85,$66,$d6,$f5,$b1,$58,
                               $da,$99,$a2,$ff,$2e,$01,$dd,$a6,
                               $29,$b8,$9c,$34,$ad,$1e,$5f,$eb,
                               $a7,$0e,$7a,$ae,$43,$28,$28,$9c);

  I10: array[0..15] of byte = ($cf,$c0,$6e,$72,$2b,$e9,$87,$b3,
                               $76,$7f,$70,$a7,$b8,$56,$b7,$74);

  P10: array[0..15] of byte = ($ce,$20,$27,$b4,$7a,$84,$32,$52,
                               $01,$34,$65,$83,$4d,$75,$fd,$0f);

  C10: array[0..15] of byte = ($dc,$03,$e5,$24,$83,$0d,$30,$f8,
                               $8e,$19,$7f,$3a,$ca,$ce,$66,$ef);

  T10: array[0..15] of byte = ($99,$84,$ef,$f6,$90,$57,$55,$d1,
                               $83,$6f,$2d,$b0,$40,$89,$63,$4c);


  K11: array[0..31] of byte = ($0e,$05,$93,$5d,$f0,$c6,$93,$74,
                               $18,$92,$b7,$6f,$af,$67,$13,$3a,
                               $bd,$2c,$f2,$03,$11,$21,$bd,$8b,
                               $b3,$81,$27,$a4,$d2,$ee,$de,$ea);

  I11: array[0..16] of byte = ($74,$b1,$ba,$26,$85,$b3,$68,$09,
                               $14,$29,$fc,$cb,$8d,$cd,$de,$09,
                               $e4);

  H11: array[0..19] of byte = ($7b,$d8,$59,$a2,$47,$96,$1a,$21,
                               $82,$3b,$38,$0e,$9f,$e8,$b6,$50,
                               $82,$ba,$61,$d3);

  P11: array[0..19] of byte = ($90,$ae,$61,$cf,$7b,$ae,$bd,$4c,
                               $ad,$e4,$94,$c5,$4a,$29,$ae,$70,
                               $26,$9a,$ec,$71);

  C11: array[0..19] of byte = ($6b,$e6,$5e,$56,$06,$6c,$40,$56,
                               $73,$8c,$03,$fe,$23,$20,$97,$4b,
                               $a3,$f6,$5e,$09);

  T11: array[0..15] of byte = ($61,$08,$dc,$41,$7b,$f3,$2f,$7f,
                               $b7,$55,$4a,$e5,$2f,$08,$8f,$87);


begin
  fail := 0;
  writeln('Test cases AES_GCM from Brian Gladman/IEEE P1619.1');
  single_test(@T01,16,K01,8*sizeof(K01),@I01,sizeof(I01),nil ,0          ,@C01,sizeof(C01),@P01,01);
  single_test(@T02,16,K02,8*sizeof(K02),@I02,sizeof(I02),@H02,sizeof(H02),nil ,0          ,nil ,02);
  single_test(@T03,16,K03,8*sizeof(K03),@I03,sizeof(I03),@H03,sizeof(H03),@C03,sizeof(C03),@P03,03);
  single_test(@T04,16,K04,8*sizeof(K04),@I04,sizeof(I04),nil ,0          ,@C04,sizeof(C04),@P04,04);
  single_test(@T05,16,K05,8*sizeof(K05),@I05,sizeof(I05),@H05,sizeof(H05),@C05,sizeof(C05),@P05,05);
  single_test(@T07,16,K07,8*sizeof(K07),@I07,sizeof(I07),@H07,sizeof(H07),@C07,sizeof(C07),@P07,07);
  single_test(@T08,16,K08,8*sizeof(K08),@I08,sizeof(I08),@H08,sizeof(H08),@C08,sizeof(C08),@P08,08);
  single_test(@T09,16,K09,8*sizeof(K09),@I09,sizeof(I09),@H09,sizeof(H09),@C09,sizeof(C09),@P09,09);
  single_test(@T10,16,K10,8*sizeof(K10),@I10,sizeof(I10),nil ,0               ,@C10,sizeof(C10),@P10,10);
  single_test(@T11,16,K11,8*sizeof(K11),@I11,sizeof(I11),@H11,sizeof(H11),@C11,sizeof(C11),@P11,11);
  if fail=0 then writeln('All tests passed.')
  else writeln('*** Number of failed tests: ', fail);
end;

begin
  write('Test program for AES-GCM functions');
  {$ifdef USEDLL}
    write('  [AES_DLL V',AES_DLL_Version,']');
  {$endif}
  writeln('   (C) 2010  W.Ehrhardt');
  writeln;
  testspec;
  writeln;
  test_glad2;
  writeln;
  tsd_test;
end.




