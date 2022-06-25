{-Test prog for ED2K, we 19.02.07}

program t_ed2kv;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  hash,
  ed2k,
  mem_util;

var
  buf: array[0..$8000-1] of byte;

var
  res: TED2KResult;
  ctx: TED2KContext;


{---------------------------------------------------------------------------}
procedure Check_eDonkey(Len: longint; {$ifdef CONST} const {$endif} test: TMD4Digest);
  {-Calc and compare eDonkey for Len repetitions of 'a'}
begin
  write('Checking ',Len:8, ' bytes: ');
  fillchar(buf, sizeof(buf), $61 {='a'});
  ED2K_Init(ctx);
  while Len>0 do begin
    if Len<sizeof(buf) then ED2K_UpdateXL(ctx, @buf, Len)
    else ED2K_UpdateXL(ctx, @buf,sizeof(buf));
    dec(Len, sizeof(buf));
  end;
  ED2K_Final(ctx, res);
  writeln(CompMem(@res.eDonkey,@test,sizeof(test)));
end;

{---------------------------------------------------------------------------}
procedure Check_Both(Len: longint; {$ifdef CONST} const {$endif} ted,tem: TMD4Digest);
  {-Calc and compare eDonkey/eMule for Len repetitions of 'a'}
var
  res: TED2KResult;
  ctx: TED2KContext;
begin
  write('Checking ',Len:8, ' bytes: ');
  fillchar(buf, sizeof(buf), $61 {='a'});
  ED2K_Init(ctx);
  while Len>0 do begin
    if Len<sizeof(buf) then ED2K_UpdateXL(ctx, @buf, Len)
    else ED2K_UpdateXL(ctx, @buf,sizeof(buf));
    dec(Len, sizeof(buf));
  end;
  ED2K_Final(ctx, res);
  write(CompMem(@res.eDonkey,@ted,sizeof(ted)):7);
  writeln(CompMem(@res.eMule,@tem,sizeof(tem)):7);
end;



{Test vectors for Len = n*9728000 + k repetitions of 'a', n=0,1,2; k=-1,0,1}
{Files generated with t_ed2kg and processed with the freeware tools from}
{SlavaSoft Optimizing Checksum Utility - fsum 2.51 <www.slavasoft.com> and}
{ed2k_hash v0.3 (build Jun 28 2003) http://sourceforge.net/projects/ed2k-tools/}

const
  _n0p0: TMD4Digest = ($31,$d6,$cf,$e0,$d1,$6a,$e9,$31,$b7,$3c,$59,$d7,$e0,$c0,$89,$c0);
  _n0p1: TMD4Digest = ($bd,$e5,$2c,$b3,$1d,$e3,$3e,$46,$24,$5e,$05,$fb,$db,$d6,$fb,$24);
  _n1m1: TMD4Digest = ($54,$61,$27,$5e,$76,$83,$7a,$31,$3a,$0b,$2f,$67,$81,$1c,$10,$23);
  _n1p0: TMD4Digest = ($8e,$a8,$fb,$e0,$60,$d8,$10,$2e,$12,$a7,$7e,$6a,$75,$95,$73,$7c);
  _n1p1: TMD4Digest = ($74,$8c,$01,$71,$a2,$d4,$2d,$28,$af,$b6,$44,$ef,$3e,$17,$f4,$e7);
  _n2m1: TMD4Digest = ($ef,$ed,$a0,$79,$c5,$d7,$cf,$62,$c6,$c2,$86,$34,$36,$7b,$83,$21);
  _n2p0: TMD4Digest = ($01,$fe,$9a,$70,$b2,$bd,$81,$aa,$1a,$ad,$1d,$a1,$df,$86,$c3,$58);
  _n2p1: TMD4Digest = ($a5,$f2,$07,$62,$21,$e4,$38,$84,$3f,$1d,$fa,$87,$4c,$b2,$4b,$d0);

{Values are taken from the following forum and the supplied link to comparebetweenclients.7z}
{http://forums.shareaza.com/showthread.php?s=d1f25f5e820941388008c6b250442cb7&threadid=49009&goto=nextoldest}
const
  {eDonkey}
  F01: TMD4Digest = ($8e,$a8,$fb,$e0,$60,$d8,$10,$2e,$12,$a7,$7e,$6a,$75,$95,$73,$7c);
  F02: TMD4Digest = ($01,$fe,$9a,$70,$b2,$bd,$81,$aa,$1a,$ad,$1d,$a1,$df,$86,$c3,$58);
  F03: TMD4Digest = ($99,$ac,$00,$58,$a9,$32,$8a,$c2,$f5,$82,$e6,$fe,$87,$b0,$60,$84);
  F04: TMD4Digest = ($cd,$09,$bb,$ef,$97,$b7,$af,$0b,$8b,$73,$91,$a9,$c5,$a2,$87,$d8);
  F05: TMD4Digest = ($ea,$e8,$9d,$25,$1e,$ef,$46,$63,$ad,$ea,$f9,$c9,$bf,$bb,$22,$eb);
  F06: TMD4Digest = ($58,$7c,$bd,$af,$96,$12,$97,$48,$f8,$c8,$cb,$de,$7f,$43,$d1,$21);
  F07: TMD4Digest = ($3b,$14,$20,$75,$5a,$26,$af,$6e,$39,$99,$bf,$60,$ef,$df,$67,$ca);
  F08: TMD4Digest = ($17,$6b,$20,$dc,$ad,$a0,$bf,$ed,$c8,$5e,$58,$5d,$64,$cd,$f7,$5f);
  F09: TMD4Digest = ($03,$fe,$8a,$4d,$7e,$2b,$49,$ba,$b2,$67,$bc,$7b,$c7,$d2,$42,$a5);
  F10: TMD4Digest = ($c2,$b6,$5d,$39,$fc,$08,$cb,$53,$6a,$f6,$47,$99,$43,$44,$e6,$03);

  {eMule}
  T01: TMD4Digest = ($ee,$15,$06,$3d,$d1,$e9,$c5,$bd,$5c,$0e,$42,$05,$c0,$b8,$e6,$98);
  T02: TMD4Digest = ($fc,$ca,$57,$f6,$ae,$31,$dc,$fa,$2c,$e0,$e4,$11,$19,$73,$8e,$b1);
  T03: TMD4Digest = ($be,$09,$cc,$40,$16,$97,$a7,$14,$1b,$9d,$d6,$14,$62,$31,$da,$32);
  T04: TMD4Digest = ($8f,$71,$97,$8e,$a7,$47,$b6,$00,$90,$44,$90,$1b,$84,$af,$86,$65);
  T05: TMD4Digest = ($6e,$b2,$8f,$1b,$ee,$10,$98,$ba,$1b,$a8,$9a,$16,$61,$40,$21,$c9);
  T06: TMD4Digest = ($d9,$d3,$a6,$f1,$b1,$2b,$ad,$4c,$1e,$f0,$ca,$45,$e8,$41,$e8,$51);
  T07: TMD4Digest = ($de,$0d,$15,$ed,$2a,$87,$c9,$f1,$51,$e9,$30,$a5,$bf,$b4,$96,$73);
  T08: TMD4Digest = ($38,$89,$e3,$eb,$0a,$6c,$be,$62,$72,$f8,$9b,$6d,$7f,$40,$66,$3c);
  T09: TMD4Digest = ($60,$3f,$7a,$a9,$ec,$bf,$4a,$b7,$a7,$d2,$79,$14,$87,$88,$35,$9c);
  T10: TMD4Digest = ($68,$a1,$09,$52,$0e,$2a,$8e,$ec,$49,$69,$ee,$de,$12,$ff,$50,$af);

begin
  writeln('ED2K test: checking repetitions of ''a'',  (c) 2007-2012 W.Ehrhardt');
  writeln('Checking eDonkey for n*9728000+k');
  Check_eDonkey(0*9728000  , _n0p0);
  Check_eDonkey(0*9728000+1, _n0p1);
  Check_eDonkey(1*9728000-1, _n1m1);
  Check_eDonkey(1*9728000  , _n1p0);
  Check_eDonkey(1*9728000+1, _n1p1);
  Check_eDonkey(2*9728000-1, _n2m1);
  Check_eDonkey(2*9728000  , _n2p0);
  Check_eDonkey(2*9728000+1, _n2p1);

  writeln('Checking eDonkey/eMule for n*9728000');
  Check_Both(01*9728000, F01, T01);
  Check_Both(02*9728000, F02, T02);
  Check_Both(03*9728000, F03, T03);
{$ifndef BIT16}
  Check_Both(04*9728000, F04, T04);
  Check_Both(05*9728000, F05, T05);
  Check_Both(06*9728000, F06, T06);
  Check_Both(07*9728000, F07, T07);
  Check_Both(08*9728000, F08, T08);
  Check_Both(09*9728000, F09, T09);
  Check_Both(10*9728000, F10, T10);
{$endif}

end.
