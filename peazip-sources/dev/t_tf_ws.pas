{-Speed test prog for Twofish modes, we 05.2006}

program T_TF_WS;

{$i STD.INC}

{$ifdef BIT16}
{$M $4000,0,655360}
{$endif}

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
      TF_Intv,
    {$else}
      TF_Intf,
    {$endif}
  {$else}
    TF_base, TF_ctr, TF_cfb, TF_ofb, TF_cbc, TF_ecb, TF_omac, TF_EAX,
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


  ct_cbc : array[0..63] of byte = ($C2,$06,$7A,$C0,$F3,$66,$92,$C1,
                                   $5E,$0F,$BB,$EF,$48,$AC,$F4,$AB,
                                   $9A,$C6,$7B,$E6,$45,$E8,$A1,$62,
                                   $6F,$B2,$AC,$79,$85,$82,$52,$4E,
                                   $83,$E6,$98,$C4,$76,$34,$39,$F9,
                                   $A2,$CD,$A9,$83,$61,$30,$11,$58,
                                   $0A,$01,$DA,$9B,$CE,$A1,$24,$4C,
                                   $09,$91,$71,$4E,$ED,$75,$F5,$CD);


  ct_cfb : array[0..63] of byte = ($99,$27,$9B,$8C,$6C,$EA,$EC,$58,
                                   $2D,$EA,$F8,$BC,$7A,$A3,$80,$EE,
                                   $00,$D2,$27,$42,$4E,$59,$3B,$61,
                                   $29,$0A,$3D,$57,$77,$A2,$32,$81,
                                   $E2,$EC,$DC,$8B,$F2,$BB,$AF,$6D,
                                   $17,$61,$C4,$E1,$FC,$A3,$E7,$1A,
                                   $7C,$F5,$A0,$B2,$98,$E1,$FD,$2F,
                                   $0E,$A0,$7C,$96,$7E,$A0,$F8,$8B);

  ct_ctr : array[0..63] of byte = ($58,$3C,$41,$1E,$DD,$52,$A0,$93,
                                   $08,$8B,$83,$8D,$27,$90,$CA,$6B,
                                   $AF,$9C,$CB,$65,$4A,$BF,$72,$D2,
                                   $DE,$D8,$39,$D6,$58,$EB,$3E,$9F,
                                   $4F,$98,$6B,$4A,$E8,$56,$87,$86,
                                   $36,$17,$F1,$AA,$59,$9A,$BB,$6F,
                                   $F5,$DA,$8E,$DF,$75,$61,$41,$8D,
                                   $9B,$66,$EF,$C0,$32,$8D,$DC,$CA);

  ct_ofb : array[0..63] of byte = ($99,$27,$9B,$8C,$6C,$EA,$EC,$58,
                                   $2D,$EA,$F8,$BC,$7A,$A3,$80,$EE,
                                   $A8,$C3,$68,$CF,$8D,$2F,$58,$FE,
                                   $35,$B7,$71,$B0,$99,$40,$14,$56,
                                   $28,$17,$71,$AC,$BA,$01,$C9,$A6,
                                   $FF,$50,$80,$AD,$9D,$02,$42,$77,
                                   $C2,$09,$8D,$19,$66,$AB,$7C,$1E,
                                   $52,$A1,$FD,$FF,$2D,$05,$5D,$CA);

  ct_ecb : array[0..63] of byte = ($29,$1E,$D1,$1A,$7B,$14,$1A,$06,
                                   $7E,$77,$39,$59,$F1,$39,$74,$DF,
                                   $9B,$05,$AA,$1E,$CB,$B6,$65,$0F,
                                   $E3,$08,$0A,$A1,$0C,$23,$33,$D3,
                                   $69,$E2,$55,$A5,$4A,$F4,$61,$74,
                                   $52,$A6,$5F,$F0,$63,$75,$AB,$73,
                                   $EC,$93,$91,$50,$DF,$26,$B2,$4D,
                                   $37,$79,$43,$E2,$13,$33,$2F,$47);

var
  ct: array[0..63] of byte;

var
  {$ifdef BASM16}
    {$ifdef dumword}
      dummy: word;
    {$endif}
  {$endif}
  Context: TTFContext;

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
  if TF_CFB_Init(key128, 8*sizeof(key128), TTFBlock(IV), context)<>0 then begin
    writeln('*** Error CFB');
    exit;
  end;
  for i:=1 to Loops do begin
    if TF_CFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
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
  if TF_CBC_Init(key128, 8*sizeof(key128), TTFBlock(IV), context)<>0 then begin
    writeln('*** Error CBC');
    exit;
  end;
  for i:=1 to Loops do begin
    if TF_CBC_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
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
  if TF_ECB_Init(key128, 8*sizeof(key128), context)<>0 then begin
    writeln('*** Error ECB');
    exit;
  end;
  for i:=1 to Loops do begin
    if TF_ECB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
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
  if TF_CTR_Init(key128, 8*sizeof(key128), TTFBlock(CTR), context)<>0 then begin
    writeln('*** Error CTR');
    exit;
  end;
  {$ifdef FPC_ProcVar}
    i := TF_SetIncProc(@TF_IncMSBFull, context);
  {$else}
    i := TF_SetIncProc(TF_IncMSBFull, context);
  {$endif}

  for i:=1 to Loops do begin
    if TF_CTR_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
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
  if TF_OFB_Init(key128, 8*sizeof(key128), TTFBlock(IV), context)<>0 then begin
    writeln('*** Error OFB');
    exit;
  end;
  for i:=1 to Loops do begin
    if TF_OFB_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
      writeln('*** Error OFB');
      exit;
    end;
  end;
  if Loops=1 then begin
    writeln('OFB  test: ', test(@ct,@ct_ofb));
  end;
end;



{---------------------------------------------------------------------------}
procedure TestOMAC;
var
  i: longint;
  tag: TTFBlock;
const
  tsdtag: TTFBlock = ($8f,$6e,$4f,$c9,$d2,$40,$b2,$21,
                      $47,$5d,$2b,$e8,$27,$f9,$56,$ed);
begin
  if TF_OMAC_Init(key128, 128, context)<>0 then begin
    writeln('*** Error OMAC Init');
    exit;
  end;
  for i:=1 to Loops do begin
    if TF_OMAC_Update(@plain, 64, context)<>0 then begin
      writeln('*** Error OMAC update');
      exit;
    end;
  end;
  TF_OMAC_Final(tag, context);
  if Loops=1 then begin
    write('OMAC test: ');
    if compmem(@tsdtag, @tag, sizeof(tag)) then writeln('OK') else writeln('Error');
 end;
end;



{---------------------------------------------------------------------------}
procedure TestEAX;
  {-Twofish part of Tom St Denis' EAX_TV.TXT}
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);

  buf32: array[0..31] of byte = ($2b,$18,$90,$eb,$9f,$c0,$b8,$29,
                                 $3e,$45,$d4,$2d,$21,$26,$f4,$07,
                                 $27,$54,$aa,$54,$e2,$20,$c8,$53,
                                 $c5,$f2,$0f,$ba,$86,$be,$07,$95);

  tag32: array[0..15] of byte = ($1a,$1b,$15,$bb,$c2,$87,$37,$2f,
                                 $b9,$af,$03,$5f,$b1,$24,$b6,$a1);
var
  err,n: integer;
  ctx: TTF_EAXContext;
  key, tag: TTFBlock;
  buf: array[0..63] of byte;
begin
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=0 to 32 do begin
    err := TF_EAX_Init(key, 128, hex32, n, ctx);
    if err=0 then err := TF_EAX_Provide_Header(@hex32,n,ctx);
    if err=0 then err := TF_EAX_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then begin
      TF_EAX_Final(tag, ctx);
      if n<32 then key := tag;
    end
    else begin
      writeln('*** Enc EAX error');
      exit;
    end;
  end;
  if not compmem(@buf32, @buf, sizeof(buf32)) then begin
    writeln('*** Enc EAX diff buf');
    exit;
  end;
  if not compmem(@tag32, @tag, sizeof(tag32)) then begin
    writeln('*** Enc EAX diff tag');
    exit;
  end;
  n := 32;
  err := TF_EAX_Init(key, 128, hex32, n, ctx);
  if err=0 then err := TF_EAX_Provide_Header(@hex32,n,ctx);
  if err=0 then err := TF_EAX_Decrypt(@buf32, @buf, n, ctx);
  if err=0 then TF_EAX_Final(tag, ctx)
  else begin
    writeln('*** Dec EAX error');
    exit;
  end;
  if not compmem(@hex32, @buf, sizeof(buf32)) then begin
    writeln('*** Dec EAX diff buf');
    exit;
  end;
  if not compmem(@tag32, @tag, sizeof(tag32)) then begin
    writeln('*** Dec EAX diff tag');
    exit;
  end;
  write('EAX  test: OK');
end;


var
  {$ifdef D12Plus}
    s: string;
  {$else}
    s: string[20];
  {$endif}
  i: integer;
begin
  {$ifdef USEDLL}
    writeln('Test program for TF_DLL V',TF_DLL_Version,'   (C) 2006-2009  W.Ehrhardt');
  {$else}
    writeln('Test program for Twofish modes    (C) 2006-2009  W.Ehrhardt');
  {$endif}
  s := paramstr(1);
  TF_SetFastInit(true);
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
    TestOMAC;
    TestEAX;
    writeln;
  end
  else if s='CBC'  then TestCBC
  else if s='CFB'  then TestCFB
  else if s='CTR'  then TestCTR
  else if s='ECB'  then TestECB
  else if s='OFB'  then TestOFB
  else if s='OMAC' then TestOMAC
  else begin
    writeln('Usage: T_TF_WS  [ TEST | CBC | CFB | CTR | ECB | OFB | OMAC]');
    halt;
  end;
end.
