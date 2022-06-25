{-Test program for CCM, (c) we 05.2009}

program T_AESCCM;

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
    AES_Type, AES_Encr, AES_CCM,
  {$endif}
  mem_util;



{---------------------------------------------------------------------------}
procedure Simple_Tests;
  {-Two tests from RFC one from NIST}
const
  key1: array[0..15] of byte = ($C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF);
  iv1 : array[0..12] of byte = ($00,$00,$00,$03,$02,$01,$00,$A0,$A1,$A2,$A3,$A4,$A5);
  hdr1: array[0..07] of byte = ($00,$01,$02,$03,$04,$05,$06,$07);
  pt1 : array[0..22] of byte = ($08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
                                $10,$11,$12,$13,$14,$15,$16,$17,
                                $18,$19,$1A,$1B,$1C,$1D,$1E);
  ct1 : array[0..22] of byte = ($58,$8C,$97,$9A,$61,$C6,$63,$D2,
                                $F0,$66,$D0,$C2,$C0,$F9,$89,$80,
                                $6D,$5F,$6B,$61,$DA,$C3,$84);
  tag1: array[0..07] of byte = ($17,$e8,$d1,$2c,$fd,$f9,$26,$e0);

const
  key2: array[0..15] of byte = ($C0,$C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF);
  iv2 : array[0..12] of byte = ($00,$00,$00,$06,$05,$04,$03,$A0,$A1,$A2,$A3,$A4,$A5);
  hdr2: array[0..11] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B);
  pt2 : array[0..18] of byte = ($0C,$0D,$0E,$0F,$10,$11,$12,$13,
                                $14,$15,$16,$17,$18,$19,$1A,$1B,
                                $1C,$1D,$1E);
  ct2 : array[0..18] of byte = ($A2,$8C,$68,$65,$93,$9A,$9A,$79,
                                $FA,$AA,$5C,$4C,$2A,$9D,$4A,$91,
                                $CD,$AC,$8C);
  tag2: array[0..07] of byte = ($96,$C8,$61,$B9,$C9,$E6,$1E,$F1);

const
  key3: array[0..15] of byte = ($40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f);
  iv3 : array[0..06] of byte = ($10,$11,$12,$13,$14,$15,$16);
  hdr3: array[0..07] of byte = ($00,$01,$02,$03,$04,$05,$06,$07);
  pt3 : array[0..03] of byte = ($20,$21,$22,$23);
  ct3 : array[0..03] of byte = ($71,$62,$01,$5b);
  tag3: array[0..03] of byte = ($4d,$ac,$25,$5d);


var
  ccm_ctx: TAESContext;

var
  tag: TAESBlock;
  buf: array[0..63] of byte;
  err: integer;
begin

  {-----------------------------------------------------------------}
  writeln('Test 1: Ex functions');
  err := AES_Init_Encr(Key1, 8*sizeof(key1), ccm_ctx);
  if err=0 then err := AES_CCM_Enc_AuthEx(ccm_ctx, tag, sizeof(tag1),
                                          iv1, sizeof(iv1), @hdr1, sizeof(hdr1),
                                          @pt1, sizeof(pt1), @buf);
  if err<>0 then writeln('Err1: ', err)
  else begin
    writeln(' CT1: ', compmem(@buf, @ct1, sizeof(ct1)));
    writeln('Tag1: ', compmem(@tag, @tag1, sizeof(tag1)));
  end;
  err := AES_CCM_Dec_VeriEx(ccm_ctx, @tag1, sizeof(tag1),
                            iv1, sizeof(iv1), @hdr1, sizeof(hdr1),
                            @ct1, sizeof(ct1), @buf);
  if err<>0 then writeln('Err1: ', err)
  else begin
    writeln(' PT1: ', compmem(@buf, @pt1, sizeof(pt1)));
  end;

  writeln('Test 1: simple functions');
  err := AES_CCM_Enc_Auth(tag, sizeof(tag1), key1, sizeof(key1),
                          iv1, sizeof(iv1), @hdr1, sizeof(hdr1),
                          @pt1, sizeof(pt1), @buf);
  if err<>0 then writeln('Err1: ', err)
  else begin
    writeln(' CT1: ', compmem(@buf, @ct1, sizeof(ct1)));
    writeln('Tag1: ', compmem(@tag, @tag1, sizeof(tag1)));
  end;
  err := AES_CCM_Dec_Veri(@tag1, sizeof(tag1), key1, sizeof(key1),
                          iv1, sizeof(iv1), @hdr1, sizeof(hdr1),
                          @ct1, sizeof(ct1), @buf);
  if err<>0 then writeln('Err1: ', err)
  else begin
    writeln(' PT1: ', compmem(@buf, @pt1, sizeof(pt1)));
  end;

  {-----------------------------------------------------------------}
  writeln('Test 2: Ex functions');
  err := AES_Init_Encr(Key2, 8*sizeof(key2), ccm_ctx);
  if err=0 then err := AES_CCM_Enc_AuthEx(ccm_ctx, tag, sizeof(tag2),
                                          iv2, sizeof(iv2), @hdr2, sizeof(hdr2),
                                          @pt2, sizeof(pt2), @buf);
  if err<>0 then writeln('Err2: ', err)
  else begin
    writeln(' CT2: ', compmem(@buf, @ct2, sizeof(ct2)));
    writeln('Tag2: ', compmem(@tag, @tag2, sizeof(tag2)));
  end;
  err := AES_CCM_Dec_VeriEx(ccm_ctx, @tag2, sizeof(tag2),
                            iv2, sizeof(iv2), @hdr2, sizeof(hdr2),
                            @ct2, sizeof(ct2), @buf);
  if err<>0 then writeln('Err2: ', err)
  else begin
    writeln(' PT2: ', compmem(@buf, @pt2, sizeof(pt2)));
  end;

  writeln('Test 2: simple functions');
  err := AES_CCM_Enc_Auth(tag, sizeof(tag2), key2, sizeof(key2),
                          iv2, sizeof(iv2), @hdr2, sizeof(hdr2),
                          @pt2, sizeof(pt2), @buf);
  if err<>0 then writeln('Err2: ', err)
  else begin
    writeln(' CT2: ', compmem(@buf, @ct2, sizeof(ct2)));
    writeln('Tag2: ', compmem(@tag, @tag2, sizeof(tag2)));
  end;
  err := AES_CCM_Dec_Veri(@tag2, sizeof(tag2), key2, sizeof(key2),
                          iv2, sizeof(iv2), @hdr2, sizeof(hdr2),
                          @ct2, sizeof(ct2), @buf);
  if err<>0 then writeln('Err2: ', err)
  else begin
    writeln(' PT2: ', compmem(@buf, @pt2, sizeof(pt2)));
  end;

  {-----------------------------------------------------------------}
  writeln('Test 3: Ex functions');
  err := AES_Init_Encr(Key3, 8*sizeof(key3), ccm_ctx);
  if err=0 then err := AES_CCM_Enc_AuthEx(ccm_ctx, tag, sizeof(tag3),
                                          iv3, sizeof(iv3), @hdr3, sizeof(hdr3),
                                          @pt3, sizeof(pt3), @buf);
  if err<>0 then writeln('Err3: ', err)
  else begin
    writeln(' CT3: ', compmem(@buf, @ct3, sizeof(ct3)));
    writeln('Tag3: ', compmem(@tag, @tag3, sizeof(tag3)));
  end;
  err := AES_CCM_Dec_VeriEx(ccm_ctx, @tag3, sizeof(tag3),
                            iv3, sizeof(iv3), @hdr3, sizeof(hdr3),
                            @ct3, sizeof(ct3), @buf);
  if err<>0 then writeln('Err3: ', err)
  else begin
    writeln(' PT3: ', compmem(@buf, @pt3, sizeof(pt3)));
  end;

  writeln('Test 3: simple functions');
  err := AES_CCM_Enc_Auth(tag, sizeof(tag3), key3, sizeof(key3),
                          iv3, sizeof(iv3), @hdr3, sizeof(hdr3),
                          @pt3, sizeof(pt3), @buf);
  if err<>0 then writeln('Err3: ', err)
  else begin
    writeln(' CT3: ', compmem(@buf, @ct3, sizeof(ct3)));
    writeln('Tag3: ', compmem(@tag, @tag3, sizeof(tag3)));
  end;
  err := AES_CCM_Dec_Veri(@tag3, sizeof(tag3), key3, sizeof(key3),
                          iv3, sizeof(iv3), @hdr3, sizeof(hdr3),
                          @ct3, sizeof(ct3), @buf);
  if err<>0 then writeln('Err3: ', err)
  else begin
    writeln(' PT3: ', compmem(@buf, @pt3, sizeof(pt3)));
  end;
end;


{---------------------------------------------------------------------------}
procedure LTC_Test(print: boolean);
  {-reproduce LTC CCM-AES test vectors}
var
  key, nonce, tag: TAESBlock;
  buf: array[0..63] of byte;
  i,k,err: integer;
const
  final: TAESBlock = ($0f,$5a,$69,$f5,$2a,$a8,$d8,$50,$8d,$09,$e6,$42,$51,$1e,$54,$e5);
begin
  writeln('LibTomCrypt CCM-AES test');
  HexUpper := true;
  for i:=0 to 15 do key[i] := i and $FF;
  nonce := key;
  for k:=0 to 32 do begin
    for i:=0 to k-1 do buf[i] := i and $FF;
    err := AES_CCM_Enc_Auth(tag, sizeof(tag), key, sizeof(key), nonce, 13,  @buf, k, @buf, k, @buf);
    if err<>0 then begin
      writeln('AES_CCM_Enc_Auth error code ',err, ' at k=',k);
      exit;
    end;
    if print then writeln(k:2,': ',HexStr(@buf,k),', ',HexStr(@tag,sizeof(tag)));
    key := tag;
  end;
  writeln('Final tag OK: ', compmem(@tag, @final, sizeof(final)));
end;


{---------------------------------------------------------------------------}
procedure RFC_Packets;
  {-Check (non-random) CCM packets from RFC 3610}
type
  ta25 = array[0..24] of byte;
  ta10 = array[0..09] of byte;
const
  ctest: array[1..12] of ta25 = (
           ($58,$8C,$97,$9A,$61,$C6,$63,$D2,$F0,$66,$D0,$C2,$C0,$F9,$89,$80,$6D,$5F,$6B,$61,$DA,$C3,$84,$00,$00),
           ($72,$C9,$1A,$36,$E1,$35,$F8,$CF,$29,$1C,$A8,$94,$08,$5C,$87,$E3,$CC,$15,$C4,$39,$C9,$E4,$3A,$3B,$00),
           ($51,$B1,$E5,$F4,$4A,$19,$7D,$1D,$A4,$6B,$0F,$8E,$2D,$28,$2A,$E8,$71,$E8,$38,$BB,$64,$DA,$85,$96,$57),
           ($A2,$8C,$68,$65,$93,$9A,$9A,$79,$FA,$AA,$5C,$4C,$2A,$9D,$4A,$91,$CD,$AC,$8C,$00,$00,$00,$00,$00,$00),
           ($DC,$F1,$FB,$7B,$5D,$9E,$23,$FB,$9D,$4E,$13,$12,$53,$65,$8A,$D8,$6E,$BD,$CA,$3E,$00,$00,$00,$00,$00),
           ($6F,$C1,$B0,$11,$F0,$06,$56,$8B,$51,$71,$A4,$2D,$95,$3D,$46,$9B,$25,$70,$A4,$BD,$87,$00,$00,$00,$00),
           ($01,$35,$D1,$B2,$C9,$5F,$41,$D5,$D1,$D4,$FE,$C1,$85,$D1,$66,$B8,$09,$4E,$99,$9D,$FE,$D9,$6C,$00,$00),
           ($7B,$75,$39,$9A,$C0,$83,$1D,$D2,$F0,$BB,$D7,$58,$79,$A2,$FD,$8F,$6C,$AE,$6B,$6C,$D9,$B7,$DB,$24,$00),
           ($82,$53,$1A,$60,$CC,$24,$94,$5A,$4B,$82,$79,$18,$1A,$B5,$C8,$4D,$F2,$1C,$E7,$F9,$B7,$3F,$42,$E1,$97),
           ($07,$34,$25,$94,$15,$77,$85,$15,$2B,$07,$40,$98,$33,$0A,$BB,$14,$1B,$94,$7B,$00,$00,$00,$00,$00,$00),
           ($67,$6B,$B2,$03,$80,$B0,$E3,$01,$E8,$AB,$79,$59,$0A,$39,$6D,$A7,$8B,$83,$49,$34,$00,$00,$00,$00,$00),
           ($C0,$FF,$A0,$D6,$F0,$5B,$DB,$67,$F2,$4D,$43,$A4,$33,$8D,$2A,$A4,$BE,$D7,$B2,$0E,$43,$00,$00,$00,$00));
  ttest: array[1..12] of ta10 = (
           ($17,$E8,$D1,$2C,$FD,$F9,$26,$E0,$00,$00),
           ($A0,$91,$D5,$6E,$10,$40,$09,$16,$00,$00),
           ($4A,$DA,$A7,$6F,$BD,$9F,$B0,$C5,$00,$00),
           ($96,$C8,$61,$B9,$C9,$E6,$1E,$F1,$00,$00),
           ($51,$E8,$3F,$07,$7D,$9C,$2D,$93,$00,$00),
           ($40,$5A,$04,$43,$AC,$91,$CB,$94,$00,$00),
           ($04,$8C,$56,$60,$2C,$97,$AC,$BB,$74,$90),
           ($C1,$7B,$44,$33,$F4,$34,$96,$3F,$34,$B4),
           ($EA,$9C,$07,$E5,$6B,$5E,$B1,$7E,$5F,$4E),
           ($56,$6A,$A9,$40,$6B,$4D,$99,$99,$88,$DD),
           ($F5,$3A,$A2,$E9,$10,$7A,$8B,$6C,$02,$2C),
           ($CD,$1A,$A3,$16,$62,$E7,$AD,$65,$D6,$DB));
var
  pn: integer;
  key, nonce, tag, hdr: TAESBlock;
  buf: array[0..63] of byte;
  i,ih,it,k,err: integer;
  plen,tlen,hlen: word;
  x: longint;
  b: byte;
begin
  writeln('Test packet vectors 1 .. 12 from RFC 3610');
  nonce[00] := 0;
  nonce[01] := 0;
  nonce[02] := 0;
  nonce[07] := $A0;
  nonce[08] := $A1;
  nonce[09] := $A2;
  nonce[10] := $A3;
  nonce[11] := $A4;
  nonce[12] := $A5;
  pn := 0;
  for i:=0 to 15 do key[i] := $C0+i;
  for it:=0 to 1 do begin
    tlen := 8 + 2*it;
    for ih:=0 to 1 do begin
      hlen := 8 + 4*ih;
      for k := 31 to 33 do begin
        pLen := k-hlen;
        x := pn*$01010101+$03020100;
        inc(pn);
        nonce[03] := (x shr 24) and $ff;
        nonce[04] := (x shr 16) and $ff;
        nonce[05] := (x shr 08) and $ff;
        nonce[06] :=  x and $ff;
        b := 0;
        for i:=0 to pred(hlen) do begin
          hdr[i] := b;
          inc(b);
        end;
        for i:=0 to pred(pLen) do begin
          buf[i] := b;
          inc(b);
        end;
        err := AES_CCM_Enc_Auth(tag,tlen,key,16,nonce,13,@hdr,hlen,@buf,plen,@buf);
        write('Packet ',pn:2);
        if err<>0 then writeln(': AES_CCM_Enc_Auth error code ',err)
        else begin
          writeln(':  CT ',compmem(@buf,@ctest[pn],plen), ',  Tag ',compmem(@tag,@ttest[pn],tlen));
          err := AES_CCM_Dec_Veri(@tag,tlen,key,16,nonce,13,@hdr,hlen,@ctest[pn],plen,@buf);
          if err<>0 then writeln(' - AES_CCM_Dec_Veri error code ',err);
        end;
      end;
    end;
  end;
end;

begin
  writeln('Test program AES-CCM mode    (c) 2009 W.Ehrhardt');
  {$ifdef USEDLL}
    writeln('DLL Version: ',AES_DLL_Version);
  {$endif}
  Simple_Tests;
  RFC_Packets;
  writeln;
  LTC_Test(false);
end.
