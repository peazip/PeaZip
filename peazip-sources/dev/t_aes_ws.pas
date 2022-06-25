{-Speed test prog for AES modes, we 2003-2012}

{23.05.2004  we  TestOMAC}
{09.07.2006  we  TestCMAC}
{22.06.2007  we  Selftest AES CMAC PRF-128}
{25.12.2007  we  Test CFB8}
{20.07.2008  we  EAX All-in-one API}

program T_AES_WS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef J_OPT}
  {$J+}
{$endif}

{$ifndef FPC}
  {$N+}
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
    aes_type,aes_base,aes_ctr,aes_cfb,aes_cfb8,aes_ofb,aes_cbc,
    aes_ecb,aes_omac,aes_cmac,aes_eax,aes_cprf,
  {$endif}
  BTypes,mem_util;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);

      IV : TAESBlock =            ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

     CTR : TAESBlock =            ($f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,
                                   $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  plain  : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

  ct_cbc : array[0..63] of byte = ($76,$49,$ab,$ac,$81,$19,$b2,$46,
                                   $ce,$e9,$8e,$9b,$12,$e9,$19,$7d,
                                   $50,$86,$cb,$9b,$50,$72,$19,$ee,
                                   $95,$db,$11,$3a,$91,$76,$78,$b2,
                                   $73,$be,$d6,$b8,$e3,$c1,$74,$3b,
                                   $71,$16,$e6,$9e,$22,$22,$95,$16,
                                   $3f,$f1,$ca,$a1,$68,$1f,$ac,$09,
                                   $12,$0e,$ca,$30,$75,$86,$e1,$a7);

  ct_cfb : array[0..63] of byte = ($3b,$3f,$d9,$2e,$b7,$2d,$ad,$20,
                                   $33,$34,$49,$f8,$e8,$3c,$fb,$4a,
                                   $c8,$a6,$45,$37,$a0,$b3,$a9,$3f,
                                   $cd,$e3,$cd,$ad,$9f,$1c,$e5,$8b,
                                   $26,$75,$1f,$67,$a3,$cb,$b1,$40,
                                   $b1,$80,$8c,$f1,$87,$a4,$f4,$df,
                                   $c0,$4b,$05,$35,$7c,$5d,$1c,$0e,
                                   $ea,$c4,$c6,$6f,$9f,$f7,$f2,$e6);

  ct_ctr : array[0..63] of byte = ($87,$4d,$61,$91,$b6,$20,$e3,$26,
                                   $1b,$ef,$68,$64,$99,$0d,$b6,$ce,
                                   $98,$06,$f6,$6b,$79,$70,$fd,$ff,
                                   $86,$17,$18,$7b,$b9,$ff,$fd,$ff,
                                   $5a,$e4,$df,$3e,$db,$d5,$d3,$5e,
                                   $5b,$4f,$09,$02,$0d,$b0,$3e,$ab,
                                   $1e,$03,$1d,$da,$2f,$be,$03,$d1,
                                   $79,$21,$70,$a0,$f3,$00,$9c,$ee);

  ct_ofb : array[0..63] of byte = ($3b,$3f,$d9,$2e,$b7,$2d,$ad,$20,
                                   $33,$34,$49,$f8,$e8,$3c,$fb,$4a,
                                   $77,$89,$50,$8d,$16,$91,$8f,$03,
                                   $f5,$3c,$52,$da,$c5,$4e,$d8,$25,
                                   $97,$40,$05,$1e,$9c,$5f,$ec,$f6,
                                   $43,$44,$f7,$a8,$22,$60,$ed,$cc,
                                   $30,$4c,$65,$28,$f6,$59,$c7,$78,
                                   $66,$a5,$10,$d9,$c1,$d6,$ae,$5e);

  ct_ecb : array[0..63] of byte = ($3a,$d7,$7b,$b4,$0d,$7a,$36,$60,
                                   $a8,$9e,$ca,$f3,$24,$66,$ef,$97,
                                   $f5,$d3,$d5,$85,$03,$b9,$69,$9d,
                                   $e7,$85,$89,$5a,$96,$fd,$ba,$af,
                                   $43,$b1,$cd,$7f,$59,$8e,$ce,$23,
                                   $88,$1b,$00,$e3,$ed,$03,$06,$88,
                                   $7b,$0c,$78,$5e,$27,$e8,$ad,$3f,
                                   $82,$23,$20,$71,$04,$72,$5d,$d4);


  tag03  : TAESBlock = ($51,$f0,$be,$bf,$7e,$3b,$9d,$92,$fc,$49,$74,$17,$79,$36,$3c,$fe);

var
  ct: array[0..63] of byte;

var
  Context: TAESContext;

const
  N : longint = 8*1000000;  {512MB}


{---------------------------------------------------------------------------}
function test(px,py: pointer): str255;
begin
  if compmem(px,py,64) then test := 'OK' else test := 'Error';
end;


{---------------------------------------------------------------------------}
procedure TestCFB;
var
  i: longint;
begin
  if AES_CFB_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error CFB');
    exit;
  end;
  for i:=1 to N do begin
    if AES_CFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CFB');
      exit;
    end;
  end;
  if N=1 then begin
    writeln('CFB  test: ', test(@ct,@ct_cfb));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestCFB8;
const
  ct_cf8 : array[0..17] of byte = ($3b,$79,$42,$4c,$9c,$0d,$d4,$36,
                                   $ba,$ce,$9e,$0e,$d4,$58,$6a,$4f,
                                   $32,$b9);
begin
  {Note CFB8 is about 16 times slower than CFB. Therefore only}
  {the case N=1 is tested using NIST SP 800-38A Test F.3.7}
  if AES_CFB8_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error CFB8');
    exit;
  end;
  if AES_CFB8_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
    writeln('*** Error CFB8');
    exit;
  end;
  write('CFB8 test: ');
  if compmem(@ct,@ct_cf8,sizeof(ct_cf8)) then writeln('OK') else writeln('Error');
end;


{---------------------------------------------------------------------------}
procedure TestCBC;
var
  i: longint;
begin
  if AES_CBC_Init_Encr(key128, 128, IV, context)<>0 then begin
    writeln('*** Error CBC');
    exit;
  end;
  for i:=1 to N do begin
    if AES_CBC_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CBC');
      exit;
    end;
  end;
  if N=1 then begin
    writeln('CBC  test: ', test(@ct,@ct_cbc));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestECB;
var
  i: longint;
begin
  if AES_ECB_Init_Encr(key128, 128, context)<>0 then begin
    writeln('*** Error ECB');
    exit;
  end;
  for i:=1 to N do begin
    if AES_ECB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error ECB');
      exit;
    end;
  end;
  if N=1 then begin
    writeln('ECB  test: ', test(@ct,@ct_ECB));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestCTR;
var
  i: longint;
begin
  if AES_CTR_Init(key128, 128, CTR, context)<>0 then begin
    writeln('*** Error CTR');
    exit;
  end;
  for i:=1 to N do begin
    if AES_CTR_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CTR');
      exit;
    end;
  end;
  if N=1 then begin
    writeln('CTR  test: ', test(@ct,@ct_ctr));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestOFB;
var
  i: longint;
begin
  if AES_OFB_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error OFB');
    exit;
  end;
  for i:=1 to N do begin
    if AES_OFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error OFB');
      exit;
    end;
  end;
  if N=1 then begin
    writeln('OFB  test: ', test(@ct,@ct_ofb));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestOMAC;
var
  i: longint;
  tag: TAESBlock;
begin
  if AES_OMAC_Init(key128, 128, context)<>0 then begin
    writeln('*** Error OMAC Init');
    exit;
  end;
  for i:=1 to N do begin
    if AES_OMAC_Update(@plain, 64, context)<>0 then begin
      writeln('*** Error OMAC update');
      exit;
    end;
  end;
  AES_OMAC_Final(tag, context);
  if N=1 then begin
    write('OMAC test: ');
    if compmem(@tag, @tag03, sizeof(tag)) then writeln('OK') else writeln('Error');
 end;
end;


{---------------------------------------------------------------------------}
procedure TestCMAC;
var
  i: longint;
  tag: TAESBlock;
begin
  if AES_CMAC_Init(key128, 128, context)<>0 then begin
    writeln('*** Error OMAC Init');
    exit;
  end;
  for i:=1 to N do begin
    if AES_CMAC_Update(@plain, 64, context)<>0 then begin
      writeln('*** Error CMAC update');
      exit;
    end;
  end;
  AES_CMAC_Final(tag, context);
  if N=1 then begin
    write('CMAC test: ');
    if compmem(@tag, @tag03, sizeof(tag)) then writeln('OK') else writeln('Error');
 end;
end;


{---------------------------------------------------------------------------}
procedure TestEAX;
const
  {Test vector from Tom St Denis}
  hex32: array[1..32] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                 $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
                                 $10, $11, $12, $13, $14, $15, $16, $17,
                                 $18, $19, $1a, $1b, $1c, $1d, $1e, $1f);

  tag00: array[1..16] of byte = ($9a, $d0, $7e, $7d, $bf, $f3, $01, $f5,
                                 $05, $de, $59, $6b, $96, $15, $df, $ff);

  tag01: array[1..16] of byte = ($1c, $e1, $0d, $3e, $ff, $d4, $ca, $db,
                                 $e2, $e4, $4b, $58, $d6, $0a, $b9, $ec);

  tag02: array[1..16] of byte = ($3a, $69, $8f, $7a, $27, $0e, $51, $b0,
                                 $f6, $5b, $3d, $3e, $47, $19, $3c, $ff);

  ct03 : array[1..32] of byte = ($29, $d8, $78, $d1, $a3, $be, $85, $7b,
                                 $6f, $b8, $c8, $ea, $59, $50, $a7, $78,
                                 $33, $1f, $bf, $2c, $cf, $33, $98, $6f,
                                 $35, $e8, $cf, $12, $1d, $cb, $30, $bc);
  tag03: array[1..16] of byte = ($4f, $be, $03, $38, $be, $1c, $8c, $7e,
                                 $1d, $7a, $e7, $e4, $5b, $92, $c5, $87);

  ct04:  array[1..29] of byte = ($dd, $25, $c7, $54, $c5, $b1, $7c, $59,
                                 $28, $b6, $9b, $73, $15, $5f, $7b, $b8,
                                 $88, $8f, $af, $37, $09, $1a, $d9, $2c,
                                 $8a, $24, $db, $86, $8b);

  tag04: array[1..16] of byte = ($0d, $1a, $14, $e5, $22, $24, $ff, $d2,
                                 $3a, $05, $fa, $02, $cd, $ef, $52, $da);

  {Test vectors from <http://eprint.iacr.org/2003/069> App. E,}
  {reproduced by Brian Gladman, ctx is split into ctxx and tagxx}

  key05: array[1..16] of byte = ($23, $39, $52, $de, $e4, $d5, $ed, $5f,
                                 $9b, $9c, $6d, $6f, $f8, $0f, $f4, $78);
  non05: array[1..16] of byte = ($62, $ec, $67, $f9, $c3, $a4, $a4, $07,
                                 $fc, $b2, $a8, $c4, $90, $31, $a8, $b3);
  hdr05: array[1..08] of byte = ($6b, $fb, $91, $4f, $d0, $7e, $ae, $6b);
  tag05: array[1..16] of byte = ($e0, $37, $83, $0e, $83, $89, $f2, $7b,
                                 $02, $5a, $2d, $65, $27, $e7, $9d, $01);

  pt06 : array[1..02] of byte = ($f7, $fb);
  key06: array[1..16] of byte = ($91, $94, $5d, $3f, $4d, $cb, $ee, $0b,
                                 $f4, $5e, $f5, $22, $55, $f0, $95, $a4);
  non06: array[1..16] of byte = ($be, $ca, $f0, $43, $b0, $a2, $3d, $84,
                                 $31, $94, $ba, $97, $2c, $66, $de, $bd);
  hdr06: array[1..08] of byte = ($fa, $3b, $fd, $48, $06, $eb, $53, $fa);
  ct06 : array[1..02] of byte = ($19, $dd);
  tag06: array[1..16] of byte = ($5c, $4c, $93, $31, $04, $9d, $0b, $da,
                                 $b0, $27, $74, $08, $f6, $79, $67, $e5);

  pt07 : array[1..05] of byte = ($1a, $47, $cb, $49, $33);
  key07: array[1..16] of byte = ($01, $f7, $4a, $d6, $40, $77, $f2, $e7,
                                 $04, $c0, $f6, $0a, $da, $3d, $d5, $23);
  non07: array[1..16] of byte = ($70, $c3, $db, $4f, $0d, $26, $36, $84,
                                 $00, $a1, $0e, $d0, $5d, $2b, $ff, $5e);
  hdr07: array[1..08] of byte = ($23, $4a, $34, $63, $c1, $26, $4a, $c6);
  ct07 : array[1..05] of byte = ($d8, $51, $d5, $ba, $e0);
  tag07: array[1..16] of byte = ($3a, $59, $f2, $38, $a2, $3e, $39, $19,
                                 $9d, $c9, $26, $66, $26, $c4, $0f, $80);

  pt08 : array[1..05] of byte = ($48, $1c, $9e, $39, $b1);
  key08: array[1..16] of byte = ($d0, $7c, $f6, $cb, $b7, $f3, $13, $bd,
                                 $de, $66, $b7, $27, $af, $d3, $c5, $e8);
  non08: array[1..16] of byte = ($84, $08, $df, $ff, $3c, $1a, $2b, $12,
                                 $92, $dc, $19, $9e, $46, $b7, $d6, $17);
  hdr08: array[1..08] of byte = ($33, $cc, $e2, $ea, $bf, $f5, $a7, $9d);
  ct08 : array[1..05] of byte = ($63, $2a, $9d, $13, $1a);
  tag08: array[1..16] of byte = ($d4, $c1, $68, $a4, $22, $5d, $8e, $1f,
                                 $f7, $55, $93, $99, $74, $a7, $be, $de);

  pt09 : array[1..06] of byte = ($40, $d0, $c0, $7d, $a5, $e4);
  key09: array[1..16] of byte = ($35, $b6, $d0, $58, $00, $05, $bb, $c1,
                                 $2b, $05, $87, $12, $45, $57, $d2, $c2);
  non09: array[1..16] of byte = ($fd, $b6, $b0, $66, $76, $ee, $dc, $5c,
                                 $61, $d7, $42, $76, $e1, $f8, $e8, $16);
  hdr09: array[1..08] of byte = ($ae, $b9, $6e, $ae, $be, $29, $70, $e9);
  ct09 : array[1..06] of byte = ($07, $1d, $fe, $16, $c6, $75);
  tag09: array[1..16] of byte = ($cb, $06, $77, $e5, $36, $f7, $3a, $fe,
                                 $6a, $14, $b7, $4e, $e4, $98, $44, $dd);

  pt10 : array[1..12] of byte = ($4d, $e3, $b3, $5c, $3f, $c0, $39, $24,
                                 $5b, $d1, $fb, $7d);
  key10: array[1..16] of byte = ($bd, $8e, $6e, $11, $47, $5e, $60, $b2,
                                 $68, $78, $4c, $38, $c6, $2f, $eb, $22);
  non10: array[1..16] of byte = ($6e, $ac, $5c, $93, $07, $2d, $8e, $85,
                                 $13, $f7, $50, $93, $5e, $46, $da, $1b);
  hdr10: array[1..08] of byte = ($d4, $48, $2d, $1c, $a7, $8d, $ce, $0f);
  ct10 : array[1..12] of byte = ($83, $5b, $b4, $f1, $5d, $74, $3e, $35,
                                 $0e, $72, $84, $14);
  tag10: array[1..16] of byte = ($ab, $b8, $64, $4f, $d6, $cc, $b8, $69,
                                 $47, $c5, $e1, $05, $90, $21, $0a, $4f);

  pt11 : array[1..17] of byte = ($8b, $0a, $79, $30, $6c, $9c, $e7, $ed,
                                 $99, $da, $e4, $f8, $7f, $8d, $d6, $16, $36);
  key11: array[1..16] of byte = ($7c, $77, $d6, $e8, $13, $be, $d5, $ac,
                                 $98, $ba, $a4, $17, $47, $7a, $2e, $7d);
  non11: array[1..16] of byte = ($1a, $8c, $98, $dc, $d7, $3d, $38, $39,
                                 $3b, $2b, $f1, $56, $9d, $ee, $fc, $19);
  hdr11: array[1..08] of byte = ($65, $d2, $01, $79, $90, $d6, $25, $28);
  ct11 : array[1..17] of byte = ($02, $08, $3e, $39, $79, $da, $01, $48,
                                 $12, $f5, $9f, $11, $d5, $26, $30, $da, $30);
  tag11: array[1..16] of byte = ($13, $73, $27, $d1, $06, $49, $b0, $aa,
                                 $6e, $1c, $18, $1d, $b6, $17, $d7, $f2);

  pt12 : array[1..18] of byte = ($1b, $da, $12, $2b, $ce, $8a, $8d, $ba,
                                 $f1, $87, $7d, $96, $2b, $85, $92, $dd, $2d, $56);
  key12: array[1..16] of byte = ($5f, $ff, $20, $ca, $fa, $b1, $19, $ca,
                                 $2f, $c7, $35, $49, $e2, $0f, $5b, $0d);
  non12: array[1..16] of byte = ($dd, $e5, $9b, $97, $d7, $22, $15, $6d,
                                 $4d, $9a, $ff, $2b, $c7, $55, $98, $26);
  hdr12: array[1..08] of byte = ($54, $b9, $f0, $4e, $6a, $09, $18, $9a);
  ct12 : array[1..18] of byte = ($2e, $c4, $7b, $2c, $49, $54, $a4, $89,
                                 $af, $c7, $ba, $48, $97, $ed, $cd, $ae, $8c, $c3);
  tag12: array[1..16] of byte = ($3b, $60, $45, $05, $99, $bd, $02, $c9,
                                 $63, $82, $90, $2a, $ef, $7f, $83, $2a);

  pt13 : array[1..18] of byte = ($6c, $f3, $67, $20, $87, $2b, $85, $13,
                                 $f6, $ea, $b1, $a8, $a4, $44, $38, $d5, $ef, $11);
  key13: array[1..16] of byte = ($a4, $a4, $78, $2b, $cf, $fd, $3e, $c5,
                                 $e7, $ef, $6d, $8c, $34, $a5, $61, $23);
  non13: array[1..16] of byte = ($b7, $81, $fc, $f2, $f7, $5f, $a5, $a8,
                                 $de, $97, $a9, $ca, $48, $e5, $22, $ec);
  hdr13: array[1..08] of byte = ($89, $9a, $17, $58, $97, $56, $1d, $7e);
  ct13 : array[1..18] of byte = ($0d, $e1, $8f, $d0, $fd, $d9, $1e, $7a,
                                 $f1, $9f, $1d, $8e, $e8, $73, $39, $38, $b1, $e8);
  tag13: array[1..16] of byte = ($e7, $f6, $d2, $23, $16, $18, $10, $2f,
                                 $db, $7f, $e5, $5f, $f1, $99, $17, $00);

  pt14 : array[1..21] of byte = ($ca, $40, $d7, $44, $6e, $54, $5f, $fa,
                                 $ed, $3b, $d1, $2a, $74, $0a, $65, $9f,
                                 $fb, $bb, $3c, $ea, $b7);
  key14: array[1..16] of byte = ($83, $95, $fc, $f1, $e9, $5b, $eb, $d6,
                                 $97, $bd, $01, $0b, $c7, $66, $aa, $c3);
  non14: array[1..16] of byte = ($22, $e7, $ad, $d9, $3c, $fc, $63, $93,
                                 $c5, $7e, $c0, $b3, $c1, $7d, $6b, $44);
  hdr14: array[1..08] of byte = ($12, $67, $35, $fc, $c3, $20, $d2, $5a);
  ct14 : array[1..21] of byte = ($cb, $89, $20, $f8, $7a, $6c, $75, $cf,
                                 $f3, $96, $27, $b5, $6e, $3e, $d1, $97,
                                 $c5, $52, $d2, $95, $a7);
  tag14: array[1..16] of byte = ($cf, $c4, $6a, $fc, $25, $3b, $46, $52,
                                 $b1, $af, $37, $95, $b1, $24, $ab, $6e);

  function test(var key, hdr, nonce, pt, tct, ttag; nlen, hlen, plen: word): boolean;
  var
    ctx: TAES_EAXContext;
    tag: TAESBlock;
    buf: array[0..63] of byte;
  begin
    test := false;
    {Incremental API}
    {encrypt}
    if AES_EAX_Init(Key, 128, nonce, nlen, ctx)<>0 then exit;
    if AES_EAX_Provide_Header(@hdr,hLen,ctx)<>0 then exit;
    if AES_EAX_Encrypt(@pt, @buf, plen, ctx)<>0 then exit;
    AES_EAX_Final(tag, ctx);
    if not compmem(@buf,@tct,plen) then exit;
    if not compmem(@tag,@ttag,sizeof(tag)) then exit;
    {decrypt}
    if AES_EAX_Init(Key, 128, nonce, nlen, ctx)<>0 then exit;
    if AES_EAX_Provide_Header(@hdr,hLen,ctx)<>0 then exit;
    if AES_EAX_Decrypt(@tct, @buf, plen, ctx)<>0 then exit;
    AES_EAX_Final(tag, ctx);
    if not compmem(@buf,@pt,plen) then exit;
    if not compmem(@tag,@ttag,sizeof(tag)) then exit;
    {All-in-one API}
    {encrypt}
    if AES_EAX_Enc_Auth(tag,Key,128,nonce,nlen,@hdr,hLen, @pt,plen, @buf)<>0 then exit;
    if not compmem(@buf,@tct,plen) then exit;
    if not compmem(@tag,@ttag,sizeof(tag)) then exit;
    {decrypt}
    {adjust test procedure if taglen <> 16!!!}
    if AES_EAX_Dec_Veri(@ttag,16,key,128,nonce,nlen,@hdr,hLen,@tct,plen,@buf)<>0 then exit;
    {tag is OK, otherwise AES_Err_EAX_Verify_Tag would have been returned}
    if not compmem(@buf,@pt,plen) then exit;
    test := true;
  end;

var
  OK: boolean;
begin
  OK := true;
  write('EAX  test: ');
  if OK then OK := Test(hex32, hex32, hex32, hex32, hex32, tag00,  0,  0,  0);
  if OK then OK := Test(hex32, hex32, hex32, hex32, hex32, tag01, 16,  0,  0);
  if OK then OK := Test(hex32, hex32, hex32, hex32, hex32, tag02,  0, 16,  0);
  if OK then OK := Test(hex32, hex32, hex32, hex32,  ct03, tag03, 16, 16, 32);
  if OK then OK := Test(hex32, hex32, hex32, hex32,  ct04, tag04, 15, 14, 29);
  if OK then OK := Test(key05, hdr05, non05, hex32, hex32, tag05, 16, 08,  0);
  if OK then OK := Test(key06, hdr06, non06,  pt06,  ct06, tag06, 16, 08,  2);
  if OK then OK := Test(key07, hdr07, non07,  pt07,  ct07, tag07, 16, 08,  5);
  if OK then OK := Test(key08, hdr08, non08,  pt08,  ct08, tag08, 16, 08,  5);
  if OK then OK := Test(key09, hdr09, non09,  pt09,  ct09, tag09, 16, 08,  6);
  if OK then OK := Test(key10, hdr10, non10,  pt10,  ct10, tag10, 16, 08, 12);
  if OK then OK := Test(key11, hdr11, non11,  pt11,  ct11, tag11, 16, 08, 17);
  if OK then OK := Test(key12, hdr12, non12,  pt12,  ct12, tag12, 16, 08, 18);
  if OK then OK := Test(key13, hdr13, non13,  pt13,  ct13, tag13, 16, 08, 18);
  if OK then OK := Test(key14, hdr14, non14,  pt14,  ct14, tag14, 16, 08, 21);
  if OK then writeln('OK') else writeln('Error');
end;

var
  {$ifdef D12Plus}
    s: string;
  {$else}
    s: string[10];
  {$endif}
  i: integer;
begin
  AES_SetFastInit(true);
  {$ifdef USEDLL}
    writeln('Test program for AES_DLL V',AES_DLL_Version,'   (C) 2004-2012  W.Ehrhardt');
  {$else}
    {$ifdef AES_ComprTab}
      writeln('Test program for AES functions [compressed tables]   (C) 2004-2012  W.Ehrhardt');
    {$else}
      writeln('Test program for AES functions [full tables]   (C) 2004-2012  W.Ehrhardt');
    {$endif}
  {$endif}
  s := paramstr(1);
  for i:=1 to length(s) do s[i] := upcase(s[i]);
  if s='TEST' then begin
    N := 1;
    writeln('Selftest AES CMAC PRF-128: ', AES_CPRF128_selftest);
    TestCBC;
    TestCFB;
    TestCFB8;
    TestCTR;
    TestECB;
    TestOFB;
    TestOMAC;
    TestCMAC;
    TestEAX;
    writeln;
  end
  else if s='CBC'  then TestCBC
  else if s='CFB'  then TestCFB
  else if s='CTR'  then TestCTR
  else if s='ECB'  then TestECB
  else if s='OFB'  then TestOFB
  else if s='OMAC' then TestOMAC
  else if s='CMAC' then TestCMAC
  else begin
    writeln('Usage: T_AES_WS  [ TEST | CBC | CFB | CTR | ECB | OFB | OMAC | CMAC ]');
    halt;
  end;
end.
