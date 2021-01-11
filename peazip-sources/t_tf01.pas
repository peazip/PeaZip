{Twofish test program "Intermediate Values"  -  we May 2006}

program t_tf01;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
 {$N+}
{$endif}

{$ifdef X_Opt}
 {$x+}
{$endif}

uses
 {$ifdef WINCRT}
    wincrt,
 {$endif}
  mem_util,tf_base;


{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9+ errors!}
{$endif}

{values from ECB_IVAL.TXT}
const
  k128: array[0..15] of byte = ($00,$00,$00,$00,$00,$00,$00,$00,
                                $00,$00,$00,$00,$00,$00,$00,$00);
  k192: array[0..23] of byte = ($01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $fe,$dc,$ba,$98,$76,$54,$32,$10,
                                $00,$11,$22,$33,$44,$55,$66,$77);
  k256: array[0..31] of byte = ($01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $fe,$dc,$ba,$98,$76,$54,$32,$10,
                                $00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff);

const
  R128: TTFRndKey = (
   $52C54DDE,$11F0626D,$7CAC9D4A,$4D1B4AAA,$B7B83A10,$1E7D0BEB,$EE9C341F,$CFE14BE4,
   $F98FFEF9,$9C5B3C17,$15A48310,$342A4D81,$424D89FE,$C14724A7,$311B834C,$FDE87320,
   $3302778F,$26CD67B4,$7A6C6362,$C2BAF60E,$3411B994,$D972C87F,$84ADB1EA,$A7DEE434,
   $54D2960F,$A2F7CAA8,$A6B8FF8C,$8014C425,$6A748D1C,$EDBAF720,$928EF78C,$0338EE13,
   $9949D6BE,$C8314176,$07C07D68,$ECAE7EA7,$1FE71844,$85C05C89,$F298311E,$696EA672);

  R192: TTFRndKey = (
   $38394A24,$C36D1175,$E802528F,$219BFEB4,$B9141AB4,$BD3E70CD,$AF609383,$FD36908A,
   $03EFB931,$1D2EE7EC,$A7489D55,$6E44B6E8,$714AD667,$653AD51F,$B6315B66,$B27C05AF,
   $A06C8140,$9853D419,$4016E346,$8D1C0DD4,$F05480BE,$B6AF816F,$2D7DC789,$45B7BD3A,
   $57F8A163,$2BEFDA69,$26AE7271,$C2900D79,$ED323794,$3D3FFD80,$5DE68E49,$9C3D2478,
   $DF326FE3,$5911F70D,$C229F13B,$B1364772,$4235364D,$0CEC363A,$57C8DD1F,$6A1AD61E);

  R256: TTFRndKey = (
   $5EC769BF,$44D13C60,$76CD39B1,$16750474,$349C294B,$EC21F6D6,$4FBD10B4,$578DA0ED,
   $C3479695,$9B6958FB,$6A7FBC4E,$0BF1830B,$61B5E0FB,$D78D9730,$7C6CF0C4,$2F9109C8,
   $E69EA8D1,$ED99BDFF,$35DC0BBD,$A03E5018,$FB18EA0B,$38BD43D3,$76191781,$37A9A0D3,
   $72427BEA,$911CC0B8,$F1689449,$71009CA9,$B6363E89,$494D9855,$590BBC63,$F95A28B5,
   $FB72B4E1,$2A43505C,$BFD34176,$5C133D12,$3A9247F7,$9A3331DD,$EE7515E6,$F0D54DCD);

  CT128: TTFBlock = ($9f,$58,$9f,$5c,$f6,$12,$2c,$32,$b6,$bf,$ec,$2f,$2a,$e8,$c3,$5a);
  CT192: TTFBlock = ($cf,$d1,$d2,$e5,$a9,$be,$9c,$df,$50,$1f,$13,$b8,$92,$bd,$22,$48);
  CT256: TTFBlock = ($37,$52,$7b,$e0,$05,$23,$34,$b8,$9f,$0c,$fc,$ca,$e8,$7c,$fa,$20);


{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}


var
  ctx: TTFContext;
  err: integer;
  PT,CT,DT: TTFBlock;


{---------------------------------------------------------------------------}
procedure CheckRK({$ifdef CONST} const {$else} var {$endif} R: TTFRndKey);
  {-Check generated round key vector}
var
  i: integer;
begin
  for i:=0 to 39 do begin
    if ctx.RK[i] <> R[i] then begin
      writeln('** RK: ',i:2);
      exit;
    end;
  end;
  writeln('  RndKey OK');
end;


{---------------------------------------------------------------------------}
procedure CheckEncDec({$ifdef CONST} const {$else} var {$endif} Test: TTFBlock);
  {-Check ECB encrypt and decrypt}
var
  d: boolean;
begin
  TF_Encrypt(ctx, PT, CT);
  writeln('  Enc: ', compmem(@CT, @Test, sizeof(CT)));
  {First decrypt should five PT=0)}
  TF_Decrypt(ctx, CT, DT);
  d := compmem(@DT, @PT, sizeof(PT));
  {Use second dec/dec with non zero plaintext/ciphertext}
  TF_Encrypt(ctx, CT, DT);
  TF_Decrypt(ctx, DT, DT);
  writeln('  Dec: ', d and compmem(@DT, @CT, sizeof(CT)));
end;


begin
  writeln('-------------------------------------------------------------------');
  writeln('T_TF01 - Twofish test program (ECB_IVAL.TXT)   (c) 2006 W.Ehrhardt');
  writeln('-----------');
  writeln('128 bit key');
  err := TF_Init(k128, 128, ctx);
  if err<>0 then begin
    writeln('TF_Init: ', err);
    halt;
  end
  else CheckRK(R128);
  fillchar(PT,sizeof(PT),0);
  CheckEncDec(CT128);

  writeln('-----------');
  writeln('192 bit key');
  err := TF_Init(k192, 192, ctx);
  if err<>0 then begin
    writeln('TF_Init: ', err);
    halt;
  end
  else CheckRK(R192);
  CheckEncDec(CT192);

  writeln('-----------');
  writeln('256 bit key');
  err := TF_Init(k256, 256, ctx);
  if err<>0 then begin
    writeln(' TF_Init: ', err);
    halt;
  end
  else CheckRK(R256);
  CheckEncDec(CT256);
end.
