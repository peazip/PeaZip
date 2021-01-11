{-Test prog for Serpent-EAX, we 11.2017}
{-Serpent EAX test vectors, format from Tom St Denis' EAX_TV.TXT}

program T_SP_EAX;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
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
    SP_Base, SP_EAX,
  {$endif}
  Mem_Util;



{---------------------------------------------------------------------------}
procedure test;
  {-Serpent EAX test vectors, format from Tom St Denis' EAX_TV.TXT}
  { https://github.com/libtom/libtomcrypt/commit/5a63e7ef7df514bb68341c863262a1bd0f9b8807}
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
  buf32: array[0..31] of byte = ($1d,$fd,$e8,$71,$9f,$4f,$c7,$c2,
                                 $35,$a1,$bb,$98,$62,$e1,$e6,$e1,
                                 $32,$ec,$0c,$77,$ef,$ec,$71,$fd,
                                 $7e,$48,$c6,$b0,$00,$c1,$42,$91);


  tag32: array[0.. 15] of byte = ($0c,$d8,$51,$7e,$1b,$79,$fc,$a1,
                                 $66,$f9,$d7,$ca,$1f,$b6,$33,$6f);

var
  err,n: integer;
  ctx: TSP_EAXContext;
  key, tag: TSPBlock;
  buf: array[0..63] of byte;
begin
  {Uppercase from HexStr}
  HexUpper := true;
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=0 to 32 do begin
    err := SP_EAX_Init(key, 128, hex32, n, ctx);
    if err=0 then err := SP_EAX_Provide_Header(@hex32,n,ctx);
    if err=0 then err := SP_EAX_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then begin
      SP_EAX_Final(tag, ctx);
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
  writeln('Compare buf32: ', compmem(@buf32, @buf, sizeof(buf32)));
  writeln('Compare tag32: ', compmem(@tag32, @tag, sizeof(tag32)));
end;

begin
  test;
end.
