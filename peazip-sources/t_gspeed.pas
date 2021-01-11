{-Test prog to compare AES encr/decr speed with Gladmann, we 01.2004}
{ To be roughly compatible, the test layout is analog to AESTMR.CPP }

program t_gspeed;

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
  AES_Type, AES_Encr, AES_Decr, hrtimer;


const
  LOOPS = 100;

var
  ctx: TAESContext;
  key: array[0..31] of byte;
  ct : TAESBlock;
  pt : TAESBlock;



{---------------------------------------------------------------------------}
procedure RandFill(var block; size: word);
var
  ba: array[1..$F000] of byte absolute block;
  i: word;
begin
  for i:=1 to size do ba[i] := random(256);
end;


{---------------------------------------------------------------------------}
function EncrCycles(keybits: word): longint;
var
  i: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  i := AES_Init_Encr(Key, KeyBits, ctx);
  if i<>0 then begin
    writeln('Error AES_Init_Encr');
    halt;
  end;
  AES_Encrypt(ctx, pt, ct);
  c1 := MaxLongint;
  c2 := MaxLongint;
  for i:=1 to LOOPS do begin
    RandFill(pt, sizeof(pt));
    ReadTSC(cyc0);
    AES_Encrypt(ctx, pt, ct);
    ReadTSC(cyc1);
    AES_Encrypt(ctx, ct, ct);
    AES_Encrypt(ctx, ct, ct);
    AES_Encrypt(ctx, ct, ct);
    AES_Encrypt(ctx, ct, ct);
    AES_Encrypt(ctx, ct, ct);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  EncrCycles := (c2-c1+1) shr 2;
end;


{---------------------------------------------------------------------------}
function DecrCycles(keybits: word): longint;
var
  i: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  i := AES_Init_Decr(Key, KeyBits, ctx);
  if i<>0 then begin
    writeln('Error AES_Init_Decr');
    halt;
  end;
  AES_Decrypt(ctx, pt, ct);
  AES_Decrypt(ctx, pt, ct);
  c1 := MaxLongint;
  c2 := MaxLongint;
  for i:=1 to LOOPS do begin
    RandFill(pt, sizeof(pt));
    ReadTSC(cyc0);
    AES_Decrypt(ctx, pt, ct);
    ReadTSC(cyc1);
    AES_Decrypt(ctx, ct, ct);
    AES_Decrypt(ctx, ct, ct);
    AES_Decrypt(ctx, ct, ct);
    AES_Decrypt(ctx, ct, ct);
    AES_Decrypt(ctx, ct, ct);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  DecrCycles := (c2-c1+1) shr 2;
end;


{---------------------------------------------------------------------------}
function EncrKeyCycles(keybits: word): longint;
var
  i,j: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  c1 := MaxLongint;
  c2 := MaxLongint;
  j := AES_Init_Encr(Key, KeyBits, ctx);
  if j<>0 then begin
    writeln('Error AES_Init_Encr');
    halt;
  end;
  for i:=1 to LOOPS do begin
    RandFill(key, sizeof(key));
    ReadTSC(cyc0);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    ReadTSC(cyc1);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Encr(Key, KeyBits, ctx);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  EncrKeyCycles := (c2-c1+1) shr 2;
end;


{---------------------------------------------------------------------------}
function DecrKeyCycles(keybits: word): longint;
var
  i,j: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  c1 := MaxLongint;
  c2 := MaxLongint;
  j := AES_Init_Decr(Key, KeyBits, ctx);
  if j<>0 then begin
    writeln('Error AES_Init_Encr');
    halt;
  end;
  for i:=1 to LOOPS do begin
    RandFill(key, sizeof(key));
    ReadTSC(cyc0);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    ReadTSC(cyc1);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  AES_Init_Decr(Key, KeyBits, ctx);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  DecrKeyCycles := (c2-c1+1) shr 2;
end;

var
  k: word;
  ec,dc : array[2..4] of longint;
  ek,dk : array[2..4] of longint;
  MB    : array[2..4] of double;
begin
  {$ifdef AES_ComprTab}
    writeln('AES Encr/Decr cycles [compressed tables]   -  (c) W.Ehrhardt 2004-2012');
  {$else}
    writeln('AES Encr/Decr cycles [full tables]   -  (c) W.Ehrhardt 2004-2012');
  {$endif}
  writeln('KeyBit  EncCyc  DecCyc  EK-Cyc  DK-Cyc    MB/s (Enc)');

  for k:=4 downto 2 do begin
    ec[k] := EncrCycles(k*64);
    dc[k] := DecrCycles(k*64);
    ek[k] := EncrKeyCycles(k*64);
    dk[k] := DecrKeyCycles(k*64);
    MB[k] := 16*CPUFrequency/ec[k]/1E6;
  end;
  for k:=4 downto 2 do writeln(k*64:6, ec[k]:8, dc[k]:8, ek[k]:8, dk[k]:8, MB[k]:8:1);
end.
