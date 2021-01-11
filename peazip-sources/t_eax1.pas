{-Test prog for EAX, we Jun.2004}

program T_EAX1;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  AES_Type, AES_EAX, Mem_Util;

{---------------------------------------------------------------------------}
const
  {Test vectors from Tom St Denis}
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

  {Test vectors from Brian Gladman, ctx is split into ctxx and tagxx}
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
{---------------------------------------------------------------------------}

{$ifdef CONST}
procedure test(const key, hdr, nonce, pt, tct, ttag; nlen, hlen, plen, nr: word);
{$else}
procedure test(var key, hdr, nonce, pt, tct, ttag; nlen, hlen, plen, nr: word);
{$endif}
var
  err: integer;
  ctx: TAES_EAXContext;
  tag: TAESBlock;
  buf: array[0..63] of byte;
begin
  {encrypt}
  write('Test ', nr:2, '  encrypt');
  if plen>sizeof(buf) then begin
    writeln(' buffer too small!'#7);
    exit;
  end;
  err := AES_EAX_Init(Key, 128, nonce, nlen, ctx);
  if err=0 then err := AES_EAX_Provide_Header(@hdr,hLen,ctx);
  if err=0 then err := AES_EAX_Encrypt(@pt, @buf, plen, ctx);
  if err=0 then AES_EAX_Final(tag, ctx);
  write(':  err: ', err);
  if err=0 then begin
    write(',  ct ok: ',compmem(@buf,@tct,plen));
    write(',  tag ok: ',compmem(@tag,@ttag,sizeof(tag)));
  end;
  writeln;
  {decrypt}
  write('         decrypt');
  err := AES_EAX_Init(Key, 128, nonce, nlen, ctx);
  if err=0 then err := AES_EAX_Provide_Header(@hdr,hLen,ctx);
  if err=0 then err := AES_EAX_Decrypt(@tct, @buf, plen, ctx);
  if err=0 then AES_EAX_Final(tag, ctx);
  write(':  err: ', err);
  if err=0 then begin
    write(',  pt ok: ',compmem(@buf,@pt,plen));
    write(',  tag ok: ',compmem(@tag,@ttag,sizeof(tag)));
  end;
  writeln;
end;

begin
  {$ifdef WINCRT}
    ScreenSize.Y := 50;
  {$endif}
  writeln;
  writeln('T_EAX1 - EAX test program    (c) 2004-2007 W.Ehrhardt');
  {      key    hdr  nonce     pt    tct   ttag, nl  hl   pl  #}
  Test(hex32, hex32, hex32, hex32, hex32, tag00,  0,  0,  0,  0);
  Test(hex32, hex32, hex32, hex32, hex32, tag01, 16,  0,  0,  1);
  Test(hex32, hex32, hex32, hex32, hex32, tag02,  0, 16,  0,  2);
  Test(hex32, hex32, hex32, hex32,  ct03, tag03, 16, 16, 32,  3);
  Test(hex32, hex32, hex32, hex32,  ct04, tag04, 15, 14, 29,  4);

  Test(key05, hdr05, non05, hex32, hex32, tag05, 16, 08,  0,  5);
  Test(key06, hdr06, non06,  pt06,  ct06, tag06, 16, 08,  2,  6);
  Test(key07, hdr07, non07,  pt07,  ct07, tag07, 16, 08,  5,  7);
  Test(key08, hdr08, non08,  pt08,  ct08, tag08, 16, 08,  5,  8);
  Test(key09, hdr09, non09,  pt09,  ct09, tag09, 16, 08,  6,  9);
  Test(key10, hdr10, non10,  pt10,  ct10, tag10, 16, 08, 12, 10);
  Test(key11, hdr11, non11,  pt11,  ct11, tag11, 16, 08, 17, 11);
  Test(key12, hdr12, non12,  pt12,  ct12, tag12, 16, 08, 18, 12);
  Test(key13, hdr13, non13,  pt13,  ct13, tag13, 16, 08, 18, 13);
  Test(key14, hdr14, non14,  pt14,  ct14, tag14, 16, 08, 21, 14);
end.
