{-Speed test prog for Serpent modes, we 04.2008}

program T_SP_WS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef J_OPT}
  {$J+}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      SP_Intv,
    {$else}
      SP_Intf,
    {$endif}
  {$else}
    SP_base, SP_ctr, SP_cfb, SP_ofb, SP_cbc, SP_ecb, SP_omac, SP_EAX,
  {$endif}
  BTypes, mem_util;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);

      IV : array[0..15] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

     CTR : array[0..15] of byte = ($f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,
                                   $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

  plain  : array[0..63] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c,$37,$10);

  {Test vectors from CryptoBench using Wei Dai's Crypto++ Version 5+}
  {http://www.addario.org/files/CryptoBench%20v1.0.1.zip}


  ct_cbc : array[0..63] of byte = ($dd,$73,$69,$1a,$b5,$66,$b6,$38,
                                   $e3,$b9,$62,$36,$c8,$c8,$a1,$dd,
                                   $a9,$b5,$d9,$db,$20,$fb,$8b,$82,
                                   $51,$40,$bf,$e6,$4d,$f2,$1c,$a8,
                                   $5f,$48,$bc,$29,$ff,$62,$27,$da,
                                   $09,$7c,$aa,$22,$75,$6f,$43,$ff,
                                   $31,$d8,$3e,$83,$4d,$92,$48,$eb,
                                   $49,$1c,$f8,$26,$80,$4e,$b9,$02);

  ct_cfb : array[0..63] of byte = ($e2,$c4,$36,$61,$3f,$e5,$8f,$75,
                                   $03,$3f,$1a,$7e,$9b,$3b,$dd,$d6,
                                   $25,$ce,$a5,$98,$4f,$ef,$a1,$b4,
                                   $0d,$65,$92,$52,$ff,$ab,$02,$1c,
                                   $6d,$95,$7c,$44,$fa,$73,$79,$09,
                                   $6f,$66,$86,$d6,$88,$da,$b8,$58,
                                   $ae,$83,$89,$28,$a2,$a2,$1d,$57,
                                   $a5,$c4,$24,$6d,$70,$0d,$a7,$2b);

  ct_ctr : array[0..63] of byte = ($7c,$2a,$0d,$21,$3f,$77,$84,$c3,
                                   $b7,$f7,$74,$d0,$dd,$49,$ca,$0b,
                                   $04,$b5,$17,$cc,$8e,$99,$a1,$7a,
                                   $95,$8d,$35,$00,$b3,$b2,$5b,$2b,
                                   $d7,$c7,$58,$e4,$91,$37,$22,$03,
                                   $83,$d8,$3b,$3e,$85,$31,$31,$73,
                                   $b5,$e5,$a2,$fa,$70,$66,$aa,$3a,
                                   $18,$22,$5f,$41,$e9,$be,$12,$7f);

  ct_ofb : array[0..63] of byte = ($e2,$c4,$36,$61,$3f,$e5,$8f,$75,
                                   $03,$3f,$1a,$7e,$9b,$3b,$dd,$d6,
                                   $d6,$e2,$3b,$25,$33,$7c,$bd,$c5,
                                   $54,$2a,$5d,$25,$68,$42,$8c,$50,
                                   $f1,$3c,$cc,$a8,$80,$ec,$74,$1c,
                                   $2c,$8a,$00,$21,$29,$35,$58,$f2,
                                   $f9,$5c,$42,$b0,$9a,$66,$82,$40,
                                   $fa,$95,$72,$20,$f3,$8b,$53,$69);

  ct_ecb : array[0..63] of byte = ($f7,$a7,$21,$e6,$c7,$56,$b6,$55,
                                   $cb,$df,$53,$3f,$c3,$b3,$1a,$c4,
                                   $4b,$c6,$04,$29,$3a,$81,$a6,$a6,
                                   $e4,$cb,$a7,$8d,$1a,$32,$a2,$9e,
                                   $cf,$c2,$8e,$50,$97,$dd,$6b,$49,
                                   $a9,$38,$b1,$51,$5e,$bc,$5a,$ac,
                                   $fe,$d2,$c4,$95,$92,$f9,$1c,$0c,
                                   $9f,$17,$cd,$86,$38,$65,$29,$eb);

var
  ct: array[0..63] of byte;

var
  {$ifdef BASM16}
    {$ifdef dumword}
      dummy: word;
    {$endif}
  {$endif}
  Context: TSPContext;

const
  Loops: longint = 8*1000000;  {512MB}


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
  if SP_CFB_Init(key128, 8*sizeof(key128), TSPBlock(IV), context)<>0 then begin
    writeln('*** Error CFB');
    exit;
  end;
  for i:=1 to Loops do begin
    if SP_CFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CFB');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('CFB  test: ', test(@ct,@ct_cfb));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestCBC;
var
  i: longint;
begin
  if SP_CBC_Init(key128, 8*sizeof(key128), TSPBlock(IV), context)<>0 then begin
    writeln('*** Error CBC');
    exit;
  end;
  for i:=1 to Loops do begin
    if SP_CBC_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CBC');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('CBC  test: ', test(@ct,@ct_cbc));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestECB;
var
  i: longint;
begin
  if SP_ECB_Init(key128, 8*sizeof(key128), context)<>0 then begin
    writeln('*** Error ECB');
    exit;
  end;
  for i:=1 to Loops do begin
    if SP_ECB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error ECB');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('ECB  test: ', test(@ct,@ct_ECB));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestCTR;
var
  i: longint;
begin
  if SP_CTR_Init(key128, 8*sizeof(key128), TSPBlock(CTR), context)<>0 then begin
    writeln('*** Error CTR');
    exit;
  end;
  for i:=1 to Loops do begin
    if SP_CTR_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error CTR');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('CTR  test: ', test(@ct,@ct_ctr));
  end;
end;


{---------------------------------------------------------------------------}
procedure TestOFB;
var
  i: longint;
begin
  if SP_OFB_Init(key128, 8*sizeof(key128), TSPBlock(IV), context)<>0 then begin
    writeln('*** Error OFB');
    exit;
  end;
  for i:=1 to Loops do begin
    if SP_OFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error OFB');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('OFB  test: ', test(@ct,@ct_ofb));
  end;
end;


var
  {$ifdef D12Plus}
    s: string;
  {$else}
    s: string[10];
  {$endif}
  i: integer;
begin
  {$ifdef USEDLL}
    writeln('Test program for SP_DLL V',SP_DLL_Version,'   (C) 2008-2009  W.Ehrhardt');
  {$else}
    writeln('Test program for Serpent modes    (C) 2008-2009  W.Ehrhardt');
  {$endif}
  s := paramstr(1);
  SP_SetFastInit(true);
  for i:=1 to length(s) do s[i] := upcase(s[i]);
  if s='TEST' then begin
    {$ifdef BASM16}
      writeln('Context offset: ',ofs(context) and 7);
    {$endif}
    Loops := 1;
    TestCBC;
    TestCFB;
    TestCTR;
    TestECB;
    TestOFB;
    writeln;
  end
  else if s='CBC'  then TestCBC
  else if s='CFB'  then TestCFB
  else if s='CTR'  then TestCTR
  else if s='ECB'  then TestECB
  else if s='OFB'  then TestOFB
  else begin
    writeln('Usage: T_SP_WS  [ TEST | CBC | CFB | CTR | ECB | OFB ]');
    halt;
  end;
end.
