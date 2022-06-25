{-Test prog for AES modes, ILen > $FFFF for 32 bit, we July 2010}

program T_AES_XL;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      AES_Intv,
    {$else}
      AES_Intf,
    {$endif}
  {$else}
    AES_Type, AES_CTR, AES_CFB, AES_CFB8, AES_OFB, AES_CBC, AES_ECB, AES_OMAC, AES_EAX,
  {$endif}
  BTypes, mem_util;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);

      IV : array[0..15] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

     CTR : array[0..15] of byte = ($f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,
                                   $f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);

{$ifndef BIT16}
const BSIZE=400000;
{$else}
const BSIZE=10000;
{$endif}

const
  BS1 = AESBLKSIZE*(BSIZE div (2*AESBLKSIZE));

type
  TBuf = array[0..BSIZE-1] of byte;

var
  pt, ct, dt: Tbuf;

var
  Context: TAESContext;


{---------------------------------------------------------------------------}
function test(px,py: pointer): Str255;
begin
  if compmemxl(px,py,sizeof(TBuf)) then test := 'OK' else test := 'Error';
end;


{---------------------------------------------------------------------------}
procedure TestCFB;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_CFB_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CFB_Init');
    exit;
  end;
  if AES_CFB_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error CFB_Encrypt 1');
    exit;
  end;
  if AES_CFB_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error CFB_Encrypt 2');
    exit;
  end;
  if AES_CFB_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CFB_Init');
    exit;
  end;
  if AES_CFB_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error CFB_Decrypt');
    exit;
  end;
  writeln('CFB  test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestCFB8;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_CFB8_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CFB8_Init');
    exit;
  end;
  if AES_CFB8_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error CFB8_Encrypt 1');
    exit;
  end;
  if AES_CFB8_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error CFB8_Encrypt 2');
    exit;
  end;
  if AES_CFB8_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CFB8_Init');
    exit;
  end;
  if AES_CFB8_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error CFB8_Decrypt');
    exit;
  end;
  writeln('CFB8 test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestCBC;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_CBC_Init_Encr(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CBC_Init_Encr');
    exit;
  end;
  if AES_CBC_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error CBC_Encrypt 1');
    exit;
  end;
  if AES_CBC_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error CBC_Encrypt 2');
    exit;
  end;
  if AES_CBC_Init_Decr(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error CBC_Init_Decr');
    exit;
  end;
  if AES_CBC_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error CBC_Decrypt');
    exit;
  end;
  writeln('CBC  test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestECB;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_ECB_Init_Encr(key128, 8*sizeof(key128), context)<>0 then begin
    writeln('*** Error ECB_Init_Encr');
    exit;
  end;
  if AES_ECB_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error ECB_Encrypt 1');
    exit;
  end;
  if AES_ECB_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error ECB_Encrypt 2');
    exit;
  end;
  if AES_ECB_Init_Decr(key128, 8*sizeof(key128), context)<>0 then begin
    writeln('*** Error ECB_Init_Decr');
    exit;
  end;
  if AES_ECB_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error ECB_Decrypt');
    exit;
  end;
  writeln('ECB  test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestCTR;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_CTR_Init(key128, 8*sizeof(key128), TAESBlock(CTR), context)<>0 then begin
    writeln('*** Error CTR_Init');
    exit;
  end;
  if AES_CTR_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error CTR_Encrypt 1');
    exit;
  end;
  if AES_CTR_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error CTR_Encrypt 2');
    exit;
  end;
  if AES_CTR_Init(key128, 8*sizeof(key128), TAESBlock(CTR), context)<>0 then begin
    writeln('*** Error CTR_Init');
    exit;
  end;
  if AES_CTR_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error CTR_Decrypt');
    exit;
  end;
  writeln('CTR  test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestOFB;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_OFB_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error OFB_Init');
    exit;
  end;
  if AES_OFB_Encrypt(@pt, @ct, BS1, context)<>0 then begin
    writeln('*** Error OFB_Encrypt 1');
    exit;
  end;
  if AES_OFB_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, context)<>0 then begin
    writeln('*** Error OFB_Encrypt 2');
    exit;
  end;
  if AES_OFB_Init(key128, 8*sizeof(key128), TAESBlock(IV), context)<>0 then begin
    writeln('*** Error OFB_Init');
    exit;
  end;
  if AES_OFB_Decrypt(@ct, @dt, sizeof(TBuf), context)<>0 then begin
    writeln('*** Error OFB_Decrypt');
    exit;
  end;
  writeln('OFB  test: ', test(@pt,@dt));
end;


{---------------------------------------------------------------------------}
procedure TestEAX;
const
  hex32: array[1..32] of byte = ($00,$01,$02,$03,$04,$05,$06,$07,
                                 $08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
                                 $10,$11,$12,$13,$14,$15,$16,$17,
                                 $18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
var
  ctx: TAES_EAXContext;
  te,td: TAESBlock;
begin
  fillchar(dt,sizeof(dt),0);
  if AES_EAX_Init(key128, 128, hex32, sizeof(hex32), ctx) <>0 then begin
    writeln('*** Error EAX_Init');
    exit;
  end;
  if AES_EAX_Provide_Header(@hex32, sizeof(hex32),ctx) <>0 then begin
    writeln('*** Error EAX_Provide_Header');
    exit;
  end;
  if AES_EAX_Encrypt(@pt, @ct, BS1, ctx) <>0 then begin
    writeln('*** Error EAX_Encrypt 1');
    exit;
  end;
  if AES_EAX_Encrypt(@pt[BS1], @ct[BS1], sizeof(TBuf)-BS1, ctx) <>0 then begin
    writeln('*** Error EAX_Encrypt 2');
    exit;
  end;
  AES_EAX_Final(te, ctx);

  if AES_EAX_Init(key128, 128, hex32, sizeof(hex32), ctx) <>0 then begin
    writeln('*** Error EAX_Init');
    exit;
  end;
  if AES_EAX_Provide_Header(@hex32, sizeof(hex32),ctx) <>0 then begin
    writeln('*** Error EAX_Provide_Header');
    exit;
  end;
  if AES_EAX_Decrypt(@ct, @dt, sizeof(TBuf), ctx) <>0 then begin
    writeln('*** Error EAX_Encrypt');
    exit;
  end;
  AES_EAX_Final(td, ctx);

  if not compmemxl(@pt, @dt, sizeof(TBuf)) then begin
    writeln('*** Dec EAX diff buf');
    exit;
  end;
  if not compmem(@te, @td, sizeof(td)) then begin
    writeln('*** Dec EAX diff tag');
    exit;
  end;
  write('EAX  test: OK');
end;


begin
  {$ifdef USEDLL}
    writeln('Test program for AES_DLL V',AES_DLL_Version,'   (C) 2010  W.Ehrhardt');
  {$else}
    writeln('Test program for AES modes    (C) 2010  W.Ehrhardt');
  {$endif}
  writeln('Test of encrypt/decrypt routines using single calls with ',BS1,'/',BSize, ' bytes.');
  RandMemXL(@pt, sizeof(TBuf));
  TestCBC;
  TestCFB;
  TestCFB8;
  TestCTR;
  TestECB;
  TestOFB;
  TestEAX;
  writeln;
end.
