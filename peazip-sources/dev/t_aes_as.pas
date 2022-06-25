{-Prog for associativity of CFB,OFB,CTR modes, we Aug.2008}

program T_AES_AS;

{$i STD.INC}


{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  BTypes,aes_type,aes_base,aes_ctr,aes_cfb,aes_cfb8,aes_ofb,mem_util;

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

var
  ct: array[0..63] of byte;

var
  Context: TAESContext;


{---------------------------------------------------------------------------}
function test(px,py: pointer): string;
begin
  if compmem(px,py,64) then test := 'OK' else test := 'Error';
end;


{---------------------------------------------------------------------------}
procedure TestCFB;
var
  i: integer;
  pp,pc: pointer;
begin
  if AES_CFB_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error CFB');
    exit;
  end;
  pp := @plain;
  pc := @ct;
  for i:=1 to sizeof(plain) do begin
    if AES_CFB_Encrypt(pp, pc, 1, context)<>0 then begin
      writeln('*** Error CFB');
      exit;
    end;
    inc(Ptr2Inc(pp));
    inc(Ptr2Inc(pc));
  end;
  writeln('CFB  test: ', test(@ct,@ct_cfb));
end;


{---------------------------------------------------------------------------}
procedure TestCFB8;
const
  ct_cf8 : array[0..17] of byte = ($3b,$79,$42,$4c,$9c,$0d,$d4,$36,
                                   $ba,$ce,$9e,$0e,$d4,$58,$6a,$4f,
                                   $32,$b9);
var
  i: integer;
  pp,pc: pointer;
begin
  {Note CFB8 is about 16 times slower than CFB. Therefore only}
  {the case N=1 is tested using NIST SP 800-38A Test F.3.7}
  if AES_CFB8_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error CFB8');
    exit;
  end;
  pp := @plain;
  pc := @ct;
  for i:=1 to sizeof(plain) do begin
    if AES_CFB8_Encrypt(pp, pc, 1, context)<>0 then begin
      writeln('*** Error CFB8');
      exit;
    end;
    inc(Ptr2Inc(pp));
    inc(Ptr2Inc(pc));
  end;
  write('CFB8 test: ');
  if compmem(@ct,@ct_cf8,sizeof(ct_cf8)) then writeln('OK') else writeln('Error');
end;



{---------------------------------------------------------------------------}
procedure TestCTR;
var
  i: integer;
  pp,pc: pointer;
begin
  if AES_CTR_Init(key128, 128, CTR, context)<>0 then begin
    writeln('*** Error CTR');
    exit;
  end;
  pp := @plain;
  pc := @ct;
  for i:=1 to sizeof(plain) do begin
    if AES_CTR_Encrypt(pp, pc, 1, context)<>0 then begin
      writeln('*** Error CTR');
      exit;
    end;
    inc(Ptr2Inc(pp));
    inc(Ptr2Inc(pc));
  end;
  writeln('CTR  test: ', test(@ct,@ct_ctr));
end;


{---------------------------------------------------------------------------}
procedure TestOFB;
var
  i: integer;
  pp,pc: pointer;
begin
  if AES_OFB_Init(key128, 128, IV, context)<>0 then begin
    writeln('*** Error OFB');
    exit;
  end;
  pp := @plain;
  pc := @ct;
  for i:=1 to sizeof(plain) do begin
    if AES_OFB_Encrypt(pp, pc, 1, context)<>0 then begin
      writeln('*** Error OFB');
      exit;
    end;
    inc(Ptr2Inc(pp));
    inc(Ptr2Inc(pc));
  end;
  writeln('OFB  test: ', test(@ct,@ct_ofb));
end;


begin
  writeln('Test program "Associativity of CFB,OFB,CTR"    (C) 2008  W.Ehrhardt');
  AES_SetFastInit(true);
  TestCFB;
  TestCFB8;
  TestCTR;
  TestOFB;
end.
