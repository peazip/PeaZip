{-Test prog Serpent encr/decr speed, we 03.2008}

program t_sp_cyc;

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
  SP_Base, hrtimer;


const
  LOOPS = 100;

var
  ctx: TSPContext;
  key: array[0..31] of byte;
  ct : TSPBlock;
  pt : TSPBlock;



{---------------------------------------------------------------------------}
procedure RandFill(var block; size: word);
var
  ba: array[1..$F000] of byte absolute block;
  i: word;
begin
  for i:=1 to size do ba[i] := random(256);
end;


{---------------------------------------------------------------------------}
function EncrCycles(KeyBits: word): longint;
var
  i: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  i := SP_Init(Key, KeyBits, ctx);
  if i<>0 then begin
    writeln('Error SP_Init');
    halt;
  end;
  SP_Encrypt(ctx, pt, ct);
  c1 := MaxLongint;
  c2 := MaxLongint;
  for i:=1 to LOOPS do begin
    RandFill(pt, sizeof(pt));
    ReadTSC(cyc0);
    SP_Encrypt(ctx, pt, ct);
    ReadTSC(cyc1);
    SP_Encrypt(ctx, ct, ct);
    SP_Encrypt(ctx, ct, ct);
    SP_Encrypt(ctx, ct, ct);
    SP_Encrypt(ctx, ct, ct);
    SP_Encrypt(ctx, ct, ct);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  EncrCycles := (c2-c1+1) shr 2;
end;


{---------------------------------------------------------------------------}
function DecrCycles(KeyBits: word): longint;
var
  i: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  i := SP_Init(Key, KeyBits, ctx);
  if i<>0 then begin
    writeln('Error SP_Init_Decr');
    halt;
  end;
  SP_Decrypt(ctx, pt, ct);
  c1 := MaxLongint;
  c2 := MaxLongint;
  for i:=1 to LOOPS do begin
    RandFill(pt, sizeof(pt));
    ReadTSC(cyc0);
    SP_Decrypt(ctx, pt, ct);
    ReadTSC(cyc1);
    SP_Decrypt(ctx, ct, ct);
    SP_Decrypt(ctx, ct, ct);
    SP_Decrypt(ctx, ct, ct);
    SP_Decrypt(ctx, ct, ct);
    SP_Decrypt(ctx, ct, ct);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  DecrCycles := (c2-c1+1) shr 2;
end;


{---------------------------------------------------------------------------}
function KeyCycles(KeyBits: word): longint;
var
  i,j: integer;
  cyc0, cyc1, cyc2: comp;
  t1,t2,c1,c2: longint;
begin
  RandFill(key, sizeof(key));
  RandFill(pt, sizeof(pt));
  c1 := MaxLongint;
  c2 := MaxLongint;
  j := SP_Init(Key, KeyBits, ctx);
  if j<>0 then begin
    writeln('Error SP_Initr');
    halt;
  end;
  for i:=1 to LOOPS do begin
    RandFill(key, sizeof(key));
    ReadTSC(cyc0);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    ReadTSC(cyc1);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    {$ifndef X_Opt} j := {$endif}  SP_Init(Key, KeyBits, ctx);
    ReadTSC(cyc2);
    t2 := round(cyc2-cyc1);
    t1 := round(cyc1-cyc0);
    if t1<c1 then c1 := t1;
    if t2<c2 then c2 := t2;
  end;
  KeyCycles := (c2-c1+1) shr 2;
end;


var
  k: word;
  ec,dc,kc : array[2..4] of longint;
  avg: longint;
  MB,sec: double;
begin
  writeln('Serpent Encr/Decr cycles   (c) W.Ehrhardt 2008');
  writeln('KeyBit  EncCyc  DecCyc   InitCyc');
  for k:=4 downto 2 do begin
    ec[k] := EncrCycles(k*64);
    dc[k] := DecrCycles(k*64);
    kc[k] := KeyCycles(k*64);
  end;
  avg := 0;
  for k:=4 downto 2 do begin
    avg := avg + ec[k] + dc[k];
    writeln(k*64:6, ec[k]:8, dc[k]:8, kc[k]:10);
  end;
  MB  := sizeof(TSPBlock)/1E6;
  sec := avg/6.0/CPUFrequency;
  writeln('Avg Cyc: ', avg/6.0:5:0, '   MB/s: ',MB/sec:7:2);
end.
