{-Test prog for Serpent ECB/CBC cipher text stealing, we Apr.2008}

program T_SP_CTS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  SP_Base, SP_ECB, SP_CBC, mem_util;


const
  BSIZE = $400;

var
  Context: TSPContext;
  pt, pt0, ct, ct0, pd: array[1..BSIZE+2] of byte;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);
      IV : TSPBlock =             ($00,$01,$02,$03,$04,$05,$06,$07,
                                   $08,$09,$0a,$0b,$0c,$0d,$0e,$0f);

  plain  : array[0..61] of byte = ($6b,$c1,$be,$e2,$2e,$40,$9f,$96,
                                   $e9,$3d,$7e,$11,$73,$93,$17,$2a,
                                   $ae,$2d,$8a,$57,$1e,$03,$ac,$9c,
                                   $9e,$b7,$6f,$ac,$45,$af,$8e,$51,
                                   $30,$c8,$1c,$46,$a3,$5c,$e4,$11,
                                   $e5,$fb,$c1,$19,$1a,$0a,$52,$ef,
                                   $f6,$9f,$24,$45,$df,$4f,$9b,$17,
                                   $ad,$2b,$41,$7b,$e6,$6c);

  {Test vector from CryptoBench using Wei Dai's Crypto++ Version 5+}
  {http://www.addario.org/files/CryptoBench%20v1.0.1.zip}
  {http://mywebpage.netscape.com/cryptobench/}
  ct_cbc : array[0..61] of byte = ($dd,$73,$69,$1a,$b5,$66,$b6,$38,
                                   $e3,$b9,$62,$36,$c8,$c8,$a1,$dd,
                                   $a9,$b5,$d9,$db,$20,$fb,$8b,$82,
                                   $51,$40,$bf,$e6,$4d,$f2,$1c,$a8,
                                   $e6,$85,$ca,$e2,$c1,$66,$d8,$c9,
                                   $43,$35,$54,$e7,$fd,$c3,$d8,$d7,
                                   $5f,$48,$bc,$29,$ff,$62,$27,$da,
                                   $09,$7c,$aa,$22,$75,$6f);

procedure EBC_Rand_Test;
  {-Test with random plain text}
var
  n,Err: integer;
begin

  writeln('ECB random test');
  randmem(@pt0, sizeof(pt0));
  pt := pt0;

  for n:=1 to BSIZE do begin
    if SP_ECB_Init(key128, 128, context)<>0 then begin
      writeln('*** Error ECB_Init');
      exit;
    end;
    Err := SP_ECB_Encrypt(@pt, @ct, n, context);
    if not compmem(@pt,@pt0,n+2) then begin
      writeln('Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      if SP_ECB_Init(key128, 128, context)<>0 then begin
        writeln('*** Error ECB_Init_Decr');
        exit;
      end;
      Err := SP_ECB_Decrypt(@ct, @pd, n, context);
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln(n:6, ' Diff');
      end;
      if not compmem(@ct,@ct0,n+2) then begin
        writeln('Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then begin
      write(n:6, ' Error: ', Err);
      if (n<SPBLKSIZE) and (Err=SP_Err_Invalid_Length) then write('  (OK)');
      writeln;
    end;
  end;
end;



procedure CBC_Rand_Test;
  {-Test with random plain text}
var
  n,Err: integer;
begin

  writeln('CBC random test');
  randmem(@pt0, sizeof(pt0));
  pt := pt0;

  for n:=1 to BSIZE do begin
    if SP_CBC_Init(key128, 128, IV, context)<>0 then begin
      writeln('*** Error CBC_Init');
      exit;
    end;
    Err := SP_CBC_Encrypt(@pt, @ct, n, context);
    if not compmem(@pt,@pt0,n+2) then begin
      writeln('Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      if SP_CBC_Init(key128, 128, IV, context)<>0 then begin
        writeln('*** Error CBC_Init_Decr');
        exit;
      end;
      Err := SP_CBC_Decrypt(@ct, @pd, n, context);
      if Err=0 then begin
        if not CompMem(@pt, @pd, n) then writeln(n:6, ' Diff');
      end;
      if not compmem(@ct,@ct0,n+2) then begin
        writeln('Decr: src overwrite, n: ',n);
        halt;
      end;
    end;
    if Err<>0 then begin
      write(n:6, ' Error: ', Err);
      if (n<SPBLKSIZE) and (Err=SP_Err_Invalid_Length) then write('  (OK)');
      writeln;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure TestCBC;
begin
  write('CBC-CTS known answer test: ');
  if SP_CBC_Init(key128, 8*sizeof(key128), TSPBlock(IV), context)<>0 then begin
    writeln('*** Error SP_CBC_Init');
    exit;
  end;
  if SP_CBC_Encrypt(@plain, @ct, sizeof(plain), context)<>0 then begin
    writeln('*** Error SP_CBC_Encrypt');
    exit;
  end;
  writeln(compmem(@ct,@ct_cbc,sizeof(ct_cbc)));
end;

begin
  writeln;
  writeln('=====================================');
  writeln('Test for Serpent cipher text stealing');


  EBC_Rand_Test;
  CBC_Rand_Test;
  TestCBC;

end.
