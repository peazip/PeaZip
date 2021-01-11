{-Test prog for EAX, we AUg.2008}
{ 1. Reproduce AES part of Tom St Denis' EAX_TV.TXT}
{ 2. All-in-one EAX functions for message length >= 60K}

program T_EAX2;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  AES_Type, AES_EAX, Mem_Util;


var
  print: boolean;

{---------------------------------------------------------------------------}
procedure test;
  {-Reproduce AES part of Tom St Denis' EAX_TV.TXT}
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
  buf32: array[0..31] of byte = ($64,$d8,$42,$b6,$67,$96,$a7,$97,
                                 $c2,$b4,$c6,$90,$57,$42,$fd,$f2,
                                 $14,$8f,$fc,$44,$5e,$19,$2f,$9e,
                                 $03,$b5,$38,$10,$c0,$82,$f7,$88);

  tag32: array[0..15] of byte = ($97,$78,$b3,$45,$ec,$12,$d2,$22,
                                 $dc,$c6,$db,$ab,$d2,$65,$17,$50);
var
  err,n: integer;
  ctx: TAES_EAXContext;
  key, tag: TAESBlock;
  buf: array[0..63] of byte;
begin
  writeln('Reproduce AES part of Tom St Denis'' EAX_TV.TXT');
  {Uppercase from HexStr}
  HexUpper := true;
  {Initial key from hex32}
  move(hex32, key, sizeof(key));
  for n:=0 to 32 do begin
    err := AES_EAX_Init(key, 128, hex32, n, ctx);
    if err=0 then err := AES_EAX_Provide_Header(@hex32,n,ctx);
    if err=0 then err := AES_EAX_Encrypt(@hex32, @buf, n, ctx);
    if err=0 then begin
      AES_EAX_Final(tag, ctx);
      if print then writeln(n:3,': ', HexStr(@buf,n), ', ', HexStr(@tag,16));
      {key for step n>1 is the tag of the previous step repeated}
      key := tag;
    end
    else begin
      writeln('Error ',err);
      exit;
    end;
  end;
  {compare final values}
  writeln('buf32 compares: ', compmem(@buf32, @buf, sizeof(buf32)));
  writeln('tag32 compares: ', compmem(@tag32, @tag, sizeof(tag32)));
end;

{$ifndef BIT16}
const
  PAKETSIZE = $23456;
{$else}
const
  PAKETSIZE = $F000;
{$endif}


{---------------------------------------------------------------------------}
procedure testallin1;
type
  tpaket=array[1..PAKETSIZE] of byte;
  ppaket=^ tpaket;
var
  pt,ct: ppaket;
  tag: TAESBlock;
  i: longint;
  err: integer;
const
  key: array[1..16] of byte = ($91, $94, $5d, $3f, $4d, $cb, $ee, $0b,
                               $f4, $5e, $f5, $22, $55, $f0, $95, $a4);
  non: array[1..16] of byte = ($be, $ca, $f0, $43, $b0, $a2, $3d, $84,
                               $31, $94, $ba, $97, $2c, $66, $de, $bd);
  hdr: array[1..08] of byte = ($fa, $3b, $fd, $48, $06, $eb, $53, $fa);
begin
  writeln('Test all-in-one EAX functions for large message length: ',PAKETSIZE);
  new(pt);
  new(ct);
  for i:=1 to PAKETSIZE do begin
    pt^[i] := i and $ff;
    ct^[i] := (i and $ff) xor $ff;
  end;
  err := AES_EAX_Enc_Auth(tag,Key,128,non,sizeof(non),@hdr,sizeof(hdr),pt,PAKETSIZE,ct);
  if err<>0 then writeln('Error from AES_EAX_Enc_Auth: ', err)
  else begin
    err := AES_EAX_Dec_Veri(@tag,sizeof(tag),key,128,non,sizeof(non),@hdr,sizeof(hdr),ct,PAKETSIZE,ct);
    if err<>0 then writeln('Error from AES_EAX_Dec_Veri: ', err)
    else begin
      {change ciphertest, veri should fail and plaintext should be untouched}
      ct^[2] := ct^[2] xor $ff;
      ct^[PAKETSIZE-1] := ct^[PAKETSIZE-1] xor $ff;
      err := AES_EAX_Dec_Veri(@tag,sizeof(tag),key,128,non,sizeof(non),@hdr,sizeof(hdr),ct,PAKETSIZE,pt);
      if err=AES_Err_EAX_Verify_Tag then begin
        err := 0;
        for i:=1 to PAKETSIZE do begin
          if pt^[i] <> (i and $ff) then err := 42;
        end;
        if err<>0 then writeln('Verification failed BUT decryption done!');
      end
      else writeln('Detection of change in ciphertext failed!');
    end;
  end;
  if err=0 then writeln('OK');
  dispose(pt);
  dispose(ct);
end;


var
  {$ifdef D12Plus}
    s: string;
  {$else}
    s: string[10];
  {$endif}
  i: integer;
begin
  s := paramstr(1);
  for i:=1 to length(s) do s[i] := upcase(s[i]);
  print := s<>'TEST';
  test;
  writeln;
  testallin1;
end.
