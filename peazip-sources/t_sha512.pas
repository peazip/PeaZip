{-Test prog for SHA512, we 19.11.03}

program t_sha512;

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$i STD.INC}

uses {$ifdef WINCRT} WinCRT,{$endif}
     mem_util, hash, sha512;


procedure Test_SHA512_1;
const
  tdig: TSHA512Digest = ($4e,$bd,$fa,$0e,$60,$e1,$a3,$e7,
                         $fe,$fb,$8d,$b4,$24,$a5,$c3,$a5,
                         $23,$65,$f3,$25,$ec,$7f,$51,$38,
                         $9a,$49,$55,$ee,$34,$53,$bb,$fc,
                         $94,$69,$2d,$ea,$c3,$ff,$6a,$4e,
                         $94,$10,$5c,$27,$d6,$32,$df,$26,
                         $25,$0f,$f3,$73,$14,$c8,$82,$fd,
                         $eb,$65,$d5,$35,$34,$f8,$a9,$61);

var
  ctx: THashContext;
  tst: TSHA512Digest;
  buf: array[0..71] of byte;
  i: integer;
begin
  for i:=0 to sizeof(buf)-1 do buf[i] := i and 255;
  SHA512Init(ctx);
  SHA512Update(ctx, @buf, sizeof(buf));
  SHA512Final(ctx, tst);
  if not CompMem(@tdig, @tst, sizeof(tst)) then writeln('Bug 1');
end;

procedure Test_SHA512_2;
const
  tdig: array[0.. 63] of byte = ($4e,$c5,$4b,$09,$e2,$b2,$09,$dd,
                                 $b9,$a6,$78,$52,$2b,$b4,$51,$74,
                                 $0c,$51,$3f,$48,$8c,$b2,$7a,$08,
                                 $83,$63,$07,$18,$57,$17,$45,$14,
                                 $19,$20,$03,$6a,$eb,$db,$78,$c0,
                                 $b4,$cd,$78,$3a,$4a,$6e,$ec,$c9,
                                 $37,$a4,$0c,$61,$04,$e4,$27,$51,
                                 $2d,$70,$9a,$63,$4b,$41,$2f,$60);
var
  ctx: THashContext;
  tst: TSHA512Digest;
  buf: array[0..3] of byte;
  i: integer;
begin
  for i:=0 to sizeof(buf)-1 do buf[i] := i and 255;
  SHA512Init(ctx);
  SHA512Update(ctx, @buf, sizeof(buf));
  SHA512Final(ctx, tst);
  if not CompMem(@tdig, @tst, sizeof(tst)) then writeln('Bug 2');
end;

begin
  writeln('SHA 512 self test passed: ', SHA512SelfTest);
  Test_SHA512_1;
  Test_SHA512_2;
end.
