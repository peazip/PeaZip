{-Test prog for Twofish-OMAC, we Jun.2007}
{ Reproduce Twofish part of Tom St Denis' OMAC_TV.TXT}

program t_tfomac;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  TF_Base, TF_OMAC, Mem_Util;

const
  final: array[0..15] of byte = ($dc,$67,$38,$08,$04,$78,$af,$9a,
                                 $f7,$ca,$83,$32,$95,$03,$1e,$06);

{---------------------------------------------------------------------------}
procedure omac_tv;
var
  err, i, n: integer;
  key, tag: TTFBlock;
  inbuf: array[0..2*TFBLKSIZE] of byte;
  ctx: TTFContext;
begin
  {Uppercase from HexStr}
  HexUpper := true;
  for i:=0 to TFBLKSIZE-1 do key[i] := i;
  for n:=0 to 2*TFBLKSIZE do begin
    for i:=0 to n-1 do inbuf[i] := i;
    err := TF_OMAC_Init(key,8*TFBLKSIZE,ctx);
    if err=0 then err := TF_OMAC_Update(@inbuf,n,ctx);
    if err<>0 then begin
      writeln('TF_OMAC error: ', err);
    end;
    TF_OMAC_Final(tag,ctx);
    writeln(n:3,': ', HexStr(@tag,16));
    key := tag;
  end;
  if not compmem(@final,@tag, sizeof(final)) then writeln('Diff for final tag');
end;


begin
  omac_tv;
end.
