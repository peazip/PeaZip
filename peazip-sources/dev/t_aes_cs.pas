{-Test prog for AES CTR Seek, (c) we July 2010}

program T_AES_CS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef BIT16}
{$N+,F+}
{$endif}


uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  HRTimer,
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      AES_Intv;
    {$else}
      AES_Intf;
    {$endif}
  {$else}
    aes_type, aes_ctr;
  {$endif}

{USE_INT64: if Int64 and errout available}

{$ifdef FPC}
  {$ifdef FPC2Plus}
    {$define USE_INT64}
  {$endif}
{$endif}
{$ifdef CONDITIONALEXPRESSIONS}  {D6+}
  {$define USE_INT64}
{$endif}


{---------------------------------------------------------------------------}
procedure My_IncMSBFull(var CTR: TAESBlock);
{$ifdef USEDLL} stdcall; {$endif}
  {-Increment CTR[15]..CTR[0]}
var
  j: integer;
begin
  {This is the same as the standard pre-defined function, but it cannot be }
  {recognized by its @address and therefore the seek loop will be performed}
  for j:=15 downto 0 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


var
  HR: THRTimer;

var
  ctx1, ctx2: TAESContext;
  Err : integer;

{$ifdef USE_INT64}
const
  BSIZE=$8000;
{$else}
const
  BSIZE=8192;
{$endif}

var
  pbuf, cbuf1, cbuf2: array[0..BSIZE-1] of byte;

{---------------------------------------------------------------------------}
procedure CheckError;
begin
  if Err<>0 then begin
    writeln('Error ',Err);
    halt;
  end;
end;


{---------------------------------------------------------------------------}
procedure randomtest(userdef: boolean);
const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);

  key192 : array[0..23] of byte = ($8e,$73,$b0,$f7,$da,$0e,$64,$52,
                                   $c8,$10,$f3,$2b,$80,$90,$79,$e5,
                                   $62,$f8,$ea,$d2,$52,$2c,$6b,$7b);

  key256 : array[0..31] of byte = ($60,$3d,$eb,$10,$15,$ca,$71,$be,
                                   $2b,$73,$ae,$f0,$85,$7d,$77,$81,
                                   $1f,$35,$2c,$07,$3b,$61,$08,$d7,
                                   $2d,$98,$10,$a3,$09,$14,$df,$f4);

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

     ct1 : array[0..63] of byte = ($87,$4d,$61,$91,$b6,$20,$e3,$26,
                                   $1b,$ef,$68,$64,$99,$0d,$b6,$ce,
                                   $98,$06,$f6,$6b,$79,$70,$fd,$ff,
                                   $86,$17,$18,$7b,$b9,$ff,$fd,$ff,
                                   $5a,$e4,$df,$3e,$db,$d5,$d3,$5e,
                                   $5b,$4f,$09,$02,$0d,$b0,$3e,$ab,
                                   $1e,$03,$1d,$da,$2f,$be,$03,$d1,
                                   $79,$21,$70,$a0,$f3,$00,$9c,$ee);

     ct2 : array[0..63] of byte = ($1a,$bc,$93,$24,$17,$52,$1c,$a2,
                                   $4f,$2b,$04,$59,$fe,$7e,$6e,$0b,
                                   $09,$03,$39,$ec,$0a,$a6,$fa,$ef,
                                   $d5,$cc,$c2,$c6,$f4,$ce,$8e,$94,
                                   $1e,$36,$b2,$6b,$d1,$eb,$c6,$70,
                                   $d1,$bd,$1d,$66,$56,$20,$ab,$f7,
                                   $4f,$78,$a7,$f6,$d2,$98,$09,$58,
                                   $5a,$97,$da,$ec,$58,$c6,$b0,$50);

     ct3 : array[0..63] of byte = ($60,$1e,$c3,$13,$77,$57,$89,$a5,
                                   $b7,$a7,$f5,$04,$bb,$f3,$d2,$28,
                                   $f4,$43,$e3,$ca,$4d,$62,$b5,$9a,
                                   $ca,$84,$e9,$90,$ca,$ca,$f5,$c5,
                                   $2b,$09,$30,$da,$a2,$3d,$e9,$4c,
                                   $e8,$70,$17,$ba,$2d,$84,$98,$8d,
                                   $df,$c9,$c5,$8d,$b6,$7a,$ad,$a6,
                                   $13,$c2,$dd,$08,$45,$79,$41,$a6);

var
  ct: array[0..255] of byte;
  SO: integer;
begin

  writeln('NIST vector test: 128 bit key');
  Err := AES_CTR_Init(key128, 128, CTR, ctx2);
  CheckError;
  if userdef then begin
    Err := AES_SetIncProc({$ifdef FPC_ProcVar}@{$endif}My_IncMSBFull, ctx2);
    CheckError;
  end;
  for SO:=0 to 63 do begin
    write('.');
    Err := AES_CTR_Seek(CTR, SO, 0, ctx2);
    CheckError;
    Err := AES_CTR_Encrypt(@plain[SO], @ct[SO], 1, ctx2);
    if ct[SO]<>ct1[SO] then begin
      writeln('Diff:  SO=',SO:2,'  ct1[SO]=',ct1[SO]:3,'  ct[SO]=',ct[SO]:3);
    end;
  end;
  writeln(' done');

  writeln('NIST vector test: 192 bit key');
  Err := AES_CTR_Init(key192, 192, CTR, ctx2);
  CheckError;
  for SO:=0 to 63 do begin
    write('.');
    {$ifdef USE_INT64}
      Err := AES_CTR_Seek64(CTR, SO, ctx2);
    {$else}
      Err := AES_CTR_Seek(CTR, SO, 0, ctx2);
    {$endif}
    CheckError;
    Err := AES_CTR_Encrypt(@plain[SO], @ct[SO], 1, ctx2);
    if ct[SO]<>ct2[SO] then begin
      writeln('Diff:  SO=',SO:2,'  ct2[SO]=',ct2[SO]:3,'  ct[SO]=',ct[SO]:3);
    end;
  end;
  writeln(' done');

  writeln('NIST vector test: 256 bit key');
  Err := AES_CTR_Init(key256, 256, CTR, ctx2);
  CheckError;
  for SO:=63 downto 0 do begin
    write('.');
    Err := AES_CTR_Seek(CTR, SO, 0, ctx2);
    CheckError;
    Err := AES_CTR_Encrypt(@plain[SO], @ct[SO], 1, ctx2);
    if ct[SO]<>ct3[SO] then begin
      writeln('Diff:  SO=',SO:2,'  ct3[SO]=',ct2[SO]:3,'  ct[SO]=',ct[SO]:3);
    end;
  end;
  writeln(' done');
end;


{---------------------------------------------------------------------------}
procedure bigtest(n: integer);
const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);
     CTR : TAESBlock =            ($ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,
                                   $ff,$ff,$ff,$ff,$fd,$fc,$fb,$fa);

{$ifdef USE_INT64}
var
  ofs: int64;
const
  oma = int64($3FFFFFFF)*$100;  {avoid braindamaged D2 error}
{$else}
var
  ofs: longint;
const
  oma = $6FFFFFFF;
{$endif}
var
  i: integer;
begin
  for i:=0 to BSIZE-1 do pbuf[i] := random(256);
  Err := AES_CTR_Init(key128, 128, CTR, ctx1);
  CheckError;
  case n of
    1: begin
         writeln('IncProc = AES_IncMSBFull,   max. offset = ',oma);
         {$ifdef USE_INT64}
           writeln(erroutput, 'IncProc = AES_IncMSBFull,   max. offset = ',oma);
         {$endif}
         {$ifdef FPC_ProcVar}
           err := AES_SetIncProc(@AES_IncMSBFull, ctx1);
         {$else}
           err := AES_SetIncProc(AES_IncMSBFull, ctx1);
         {$endif}
       end;
    2: begin
         writeln('IncProc = AES_IncLSBFull,   max. offset = ',oma);
         {$ifdef USE_INT64}
           writeln(erroutput, 'IncProc = AES_IncLSBFull,   max. offset = ',oma);
         {$endif}
         {$ifdef FPC_ProcVar}
           err := AES_SetIncProc(@AES_IncLSBFull, ctx1);
         {$else}
           err := AES_SetIncProc(AES_IncLSBFull, ctx1);
         {$endif}
       end;

    3: begin
         writeln('IncProc = AES_IncMSBPart,   max. offset = ',oma);
         {$ifdef USE_INT64}
           writeln(erroutput, 'IncProc = AES_IncMSBPart,   max. offset = ',oma);
         {$endif}
         {$ifdef FPC_ProcVar}
           err := AES_SetIncProc(@AES_IncMSBPart, ctx1);
         {$else}
           err := AES_SetIncProc(AES_IncMSBPart, ctx1);
         {$endif}
       end;

    4: begin
         writeln('IncProc = AES_IncLSBPart,   max. offset = ',oma);
         {$ifdef USE_INT64}
           writeln(erroutput, 'IncProc = AES_IncLSBPart,   max. offset = ',oma);
         {$endif}
         {$ifdef FPC_ProcVar}
           err := AES_SetIncProc(@AES_IncLSBPart, ctx1);
         {$else}
           err := AES_SetIncProc(AES_IncLSBPart, ctx1);
         {$endif}
       end;
  end;

  CheckError;
  ofs := 0;
  ReStartTimer(HR);
  repeat
    for i:=1 to 99 do begin
      Err := AES_CTR_Encrypt(@pbuf, @cbuf1, BSIZE, ctx1);
      ofs := ofs + BSIZE;
    end;
    {$ifdef USE_INT64}
      write(erroutput, 100.0*ofs/oma:1:3,'%'#13);
    {$else}
      write(100.0*ofs/oma:1:3,'%'#13);
    {$endif}
    Err := AES_CTR_Encrypt(@pbuf, @cbuf1, BSIZE, ctx1);
    CheckError;
    i := random(BSIZE);
    Err := AES_CTR_Init(key128, 128, CTR, ctx2);
    CheckError;
    case n of
      1: begin
          (*
           {$ifdef FPC_ProcVar}
             err := AES_SetIncProc(@AES_IncMSBFull, ctx2);
           {$else}
             err := AES_SetIncProc(AES_IncMSBFull, ctx2);
           {$endif}
          *)
         end;
      2: begin
           {$ifdef FPC_ProcVar}
             err := AES_SetIncProc(@AES_IncLSBFull, ctx2);
           {$else}
             err := AES_SetIncProc(AES_IncLSBFull, ctx2);
           {$endif}
         end;

      3: begin
           {$ifdef FPC_ProcVar}
             err := AES_SetIncProc(@AES_IncMSBPart, ctx2);
           {$else}
             err := AES_SetIncProc(AES_IncMSBPart, ctx2);
           {$endif}
         end;

      4: begin
           {$ifdef FPC_ProcVar}
             err := AES_SetIncProc(@AES_IncLSBPart, ctx2);
           {$else}
             err := AES_SetIncProc(AES_IncLSBPart, ctx2);
           {$endif}
         end;
      else begin
             writeln('Invalid n');
             halt;
           end;
    end;
    CheckError;
    {$ifdef USE_INT64}
      Err := AES_CTR_Seek64(CTR, ofs+i, ctx2);
    {$else}
      Err := AES_CTR_Seek(CTR, ofs+i, 0, ctx2);
    {$endif}
    CheckError;
    Err := AES_CTR_Encrypt(@pbuf[i], @cbuf2[i], 1, ctx2);
    CheckError;
    if cbuf1[i]<>cbuf2[i] then begin
      writeln('Diff:  Offset=',ofs+i,'  cbuf1[]=',cbuf1[i]:3,'  cbuf2[]=',cbuf2[i]:3);
      halt;
    end;
    ofs := ofs + BSIZE;
  until ofs>oma;
  writeln('Done - no differences.');
  writeln('Time [s]: ', ReadSeconds(HR):1:3);
end;

var
  {$ifdef D12Plus}
    s: string;
  {$else}
    s: string[10];
  {$endif}

begin
  writeln('Test program "AES CTR Seek"    (C) 2010-2017  W.Ehrhardt');
  {$ifdef USEDLL}
    writeln('DLL Version: ',AES_DLL_Version);
  {$endif}
  writeln;
  writeln('Test using standard AES_IncMSBFull');
  randomtest(false);
  writeln;
  writeln('Test using user-defines My_IncMSBFull');
  randomtest(true);
  writeln;
  StartTimer(HR);
  s := paramstr(1);
  if s='big' then begin
    bigtest(1);
    bigtest(2);
    bigtest(3);
    bigtest(4);
  end;
end.
