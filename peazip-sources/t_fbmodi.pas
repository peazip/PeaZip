{-Test prog for AES CTR/CFB/OFB with full blocks first, we Jan.2004}

program T_FBModi;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  AES_type, AES_CTR, AES_CFB, AES_OFB, mem_util;


const
  BSIZE = $400;
  EXT   = 15;
var
  Context: TAESContext;
  GErr,Err: integer;
  pt, pt0, ct, ct0, pd: array[1..BSIZE+EXT] of byte;


const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);
      IV : TAESBlock =            ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

{---------------------------------------------------------------------------}
procedure Test_CTR;
var
  i,n: integer;
begin
  GErr := 0;
  writeln('- CTR with full blocks first');

  for i:=1 to BSIZE do pt0[i] := random(256);
  pt := pt0;

  for n:=1 to BSIZE do begin
    Err := AES_CTR_Init(key128, 128, IV, context);
    Err := AES_CTR_Encrypt(@pt, @ct, n, context);
    GErr:= GErr or Err;
    if not compmem(@pt,@pt0,n+EXT) then begin
      writeln('  Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      Err := AES_CTR_Init(key128, 128, IV, context);
      Err := AES_CTR_Decrypt(@ct, @pd, n, context);
      GErr:= GErr or Err;
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln('  Diff:', n:6);
      end;
      if not compmem(@ct,@ct0,n+EXT) then begin
        writeln('  Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then writeln(n:6, '  Error: ', Err);
  end;
  if GErr=0 then writeln('  OK.');
end;


{---------------------------------------------------------------------------}
procedure Test_CFB;
var
  i,n: integer;
begin
  GErr := 0;
  writeln('- CFB with full blocks first');

  for i:=1 to BSIZE do pt0[i] := random(256);
  pt := pt0;

  for n:=1 to BSIZE do begin
    Err := AES_CFB_Init(key128, 128, IV, context);
    Err := AES_CFB_Encrypt(@pt, @ct, n, context);
    GErr:= GErr or Err;
    if not compmem(@pt,@pt0,n+EXT) then begin
      writeln('  Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      Err := AES_CFB_Init(key128, 128, IV, context);
      Err := AES_CFB_Decrypt(@ct, @pd, n, context);
      GErr:= GErr or Err;
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln('  Diff:', n:6);
      end;
      if not compmem(@ct,@ct0,n+EXT) then begin
        writeln('  Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then writeln(n:6, '  Error: ', Err);
  end;
  if GErr=0 then writeln('  OK.');
end;


{---------------------------------------------------------------------------}
procedure Test_OFB;
var
  i,n: integer;
begin
  GErr := 0;
  writeln('- OFB with full blocks first');

  for i:=1 to BSIZE do pt0[i] := random(256);
  pt := pt0;

  for n:=1 to BSIZE do begin
    Err := AES_OFB_Init(key128, 128, IV, context);
    Err := AES_OFB_Encrypt(@pt, @ct, n, context);
    GErr:= GErr or Err;
    if not compmem(@pt,@pt0,n+EXT) then begin
      writeln('  Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      Err := AES_OFB_Init(key128, 128, IV, context);
      Err := AES_OFB_Decrypt(@ct, @pd, n, context);
      GErr:= GErr or Err;
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln('  Diff:', n:6);
      end;
      if not compmem(@ct,@ct0,n+EXT) then begin
        writeln('  Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then writeln(n:6, '  Error: ', Err);
  end;
  if GErr=0 then writeln('  OK.');
end;


begin
  writeln;
  writeln('Test AES CTR/CFB/OFB with full blocks first, WE Jan.2004');
  Test_CTR;
  Test_CFB;
  Test_OFB;
end.
