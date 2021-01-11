{-Test prog for Serpent-OMAC, we 11.2017}

program t_caomac;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  SP_Base, SP_OMAC, Mem_Util;

var
  writeIVal: boolean;

{---------------------------------------------------------------------------}
procedure omac_tv;
var
  err, i, n: integer;
  key, tag: TSPBlock;
  inbuf: array[0..2*SPBLKSIZE] of byte;
  ctx: TSPContext;
const
  final: array[0..15] of byte = ($6e,$45,$81,$87,$ec,$66,$4a,$77,
                                 $60,$05,$ea,$14,$01,$54,$ac,$bf);

begin
  writeln('TSD format OMAC test vectors');
  {https://github.com/libtom/libtomcrypt/commit/5a63e7ef7df514bb68341c863262a1bd0f9b8807}
  {Uppercase from HexStr}
  HexUpper := true;
  for i:=0 to SPBLKSIZE-1 do key[i] := i;
  for n:=0 to 2*SPBLKSIZE do begin
    for i:=0 to n-1 do inbuf[i] := i;
    err := SP_OMAC_Init(key,8*SPBLKSIZE,ctx);
    if err=0 then err := SP_OMAC_Update(@inbuf,n,ctx);
    if err<>0 then begin
      writeln('SP_OMAC error: ', err);
    end;
    SP_OMAC_Final(tag,ctx);
    if writeIVal then writeln(n:3,': ', HexStr(@tag,16));
    key := tag;
  end;
  {Note: final is not tested against other implementations! Used for regression tests.}
  if compmem(@final,@tag, sizeof(final)) then writeln('Final tag: OK')
  else writeln('Diff for final tag');
end;

begin
  writeIVal := paramstr(1)<>'test';
  omac_tv;
end.
