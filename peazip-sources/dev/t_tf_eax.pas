{-Test prog for Twofish-EAX, we Jun.2007}
{Reproduce Twofish part of Tom St Denis' EAX_TV.TXT}

program T_tf_EAX;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  TF_Base, TF_EAX, Mem_Util;



{---------------------------------------------------------------------------}
procedure test;
  {-Reproduce Twofish part of Tom St Denis' EAX_TV.TXT}
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
  {Uppercase from HexStr}
  HexUpper := true;
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=0 to 32 do begin
    err := TF_EAX_Init(key, 128, hex32, n, ctx);
    if err=0 then err := TF_EAX_Provide_Header(@hex32,n,ctx);
    if err=0 then err := TF_EAX_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then begin
      TF_EAX_Final(tag, ctx);
      writeln(n:3,': ', HexStr(@buf,n), ', ', HexStr(@tag,16));
      {key for step n>1 is the tag of the previous step repeated}
      key := tag;
    end
    else begin
      writeln('Error ',err);
      halt;
    end;
  end;
  {compare only final values}
  if not compmem(@buf32, @buf, sizeof(buf32)) then writeln('** Diff: buf32');
  if not compmem(@tag32, @tag, sizeof(tag32)) then writeln('** Diff: tag32');
end;

begin
  test;
end.
