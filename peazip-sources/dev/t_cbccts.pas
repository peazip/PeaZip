{-Test prog for AES CBC cipher text stealing, we Sep.2003}

program T_CBCCTS;

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
      AES_Intv,
    {$else}
      AES_Intf,
    {$endif}
  {$else}
    aes_type, aes_cbc,
  {$endif}
  mem_util;


const
  BSIZE = $400;

var
  Context: TAESContext;
  pt, pt0, ct, ct0, pd: array[1..BSIZE+2] of byte;


{RFC 3962  Advanced Encryption Standard (AES) Encryption for Kerberos 5}
{Appendix B.  Sample Test Vectors}

const
  key128 : array[0..15] of byte = ($63,$68,$69,$63,$6b,$65,$6e,$20,
                                   $74,$65,$72,$69,$79,$61,$6b,$69);

      IV : TAESBlock =            ($00,$00,$00,$00,$00,$00,$00,$00,
                                   $00,$00,$00,$00,$00,$00,$00,$00);

      pt1: array[0..16] of byte = ($49,$20,$77,$6f,$75,$6c,$64,$20,
                                   $6c,$69,$6b,$65,$20,$74,$68,$65,
                                   $20);

      ct1: array[0..16] of byte = ($c6,$35,$35,$68,$f2,$bf,$8c,$b4,
                                   $d8,$a5,$80,$36,$2d,$a7,$ff,$7f,
                                   $97);


      pt2: array[0..30] of byte = ($49,$20,$77,$6f,$75,$6c,$64,$20,
                                   $6c,$69,$6b,$65,$20,$74,$68,$65,
                                   $20,$47,$65,$6e,$65,$72,$61,$6c,
                                   $20,$47,$61,$75,$27,$73,$20);

      ct2: array[0..30] of byte = ($fc,$00,$78,$3e,$0e,$fd,$b2,$c1,
                                   $d4,$45,$d4,$c8,$ef,$f7,$ed,$22,
                                   $97,$68,$72,$68,$d6,$ec,$cc,$c0,
                                   $c0,$7b,$25,$e2,$5e,$cf,$e5);


      pt3: array[0..31] of byte = ($49,$20,$77,$6f,$75,$6c,$64,$20,
                                   $6c,$69,$6b,$65,$20,$74,$68,$65,
                                   $20,$47,$65,$6e,$65,$72,$61,$6c,
                                   $20,$47,$61,$75,$27,$73,$20,$43);

      ct3: array[0..31] of byte = ($39,$31,$25,$23,$a7,$86,$62,$d5,
                                   $be,$7f,$cb,$cc,$98,$eb,$f5,$a8,
                                   $97,$68,$72,$68,$d6,$ec,$cc,$c0,
                                   $c0,$7b,$25,$e2,$5e,$cf,$e5,$84);


      pt4: array[0..46] of byte = ($49,$20,$77,$6f,$75,$6c,$64,$20,
                                   $6c,$69,$6b,$65,$20,$74,$68,$65,
                                   $20,$47,$65,$6e,$65,$72,$61,$6c,
                                   $20,$47,$61,$75,$27,$73,$20,$43,
                                   $68,$69,$63,$6b,$65,$6e,$2c,$20,
                                   $70,$6c,$65,$61,$73,$65,$2c);

      ct4: array[0..46] of byte = ($97,$68,$72,$68,$d6,$ec,$cc,$c0,
                                   $c0,$7b,$25,$e2,$5e,$cf,$e5,$84,
                                   $b3,$ff,$fd,$94,$0c,$16,$a1,$8c,
                                   $1b,$55,$49,$d2,$f8,$38,$02,$9e,
                                   $39,$31,$25,$23,$a7,$86,$62,$d5,
                                   $be,$7f,$cb,$cc,$98,$eb,$f5);



      pt5: array[0..47] of byte = ($49,$20,$77,$6f,$75,$6c,$64,$20,
                                   $6c,$69,$6b,$65,$20,$74,$68,$65,
                                   $20,$47,$65,$6e,$65,$72,$61,$6c,
                                   $20,$47,$61,$75,$27,$73,$20,$43,
                                   $68,$69,$63,$6b,$65,$6e,$2c,$20,
                                   $70,$6c,$65,$61,$73,$65,$2c,$20);

      ct5: array[0..47] of byte = ($97,$68,$72,$68,$d6,$ec,$cc,$c0,
                                   $c0,$7b,$25,$e2,$5e,$cf,$e5,$84,
                                   $9d,$ad,$8b,$bb,$96,$c4,$cd,$c0,
                                   $3b,$c1,$03,$e1,$a1,$94,$bb,$d8,
                                   $39,$31,$25,$23,$a7,$86,$62,$d5,
                                   $be,$7f,$cb,$cc,$98,$eb,$f5,$a8);



{---------------------------------------------------------------------------}
procedure RFC_Test;
  {-Test with known vectors}
  procedure SingleTest(pp,pc: pointer; lt,n: word);
  var
    cmp: boolean;
  begin
    if AES_CBC_Init_Encr(key128, 128, IV, context)<>0 then begin
      writeln('*** Error CBC_Init');
      exit;
    end;
    if AES_CBC_Encrypt(pp, @ct, lt, context)<>0 then begin
      writeln('*** Error CBC');
      exit;
    end;
    cmp := compmem(@ct,pc,lt);
    write('Test vector ',n,': ',cmp:6);
    {if lt multiple of block size results must not compare}
    if (lt mod AESBLKSIZE=0) <> cmp then writeln('  OK')
    else writeln('Error');
  end;

begin
  SingleTest(@pt1,@ct1,sizeof(pt1),1);
  SingleTest(@pt2,@ct2,sizeof(pt2),2);
  SingleTest(@pt3,@ct3,sizeof(pt3),3);
  SingleTest(@pt4,@ct4,sizeof(pt4),4);
  SingleTest(@pt5,@ct5,sizeof(pt5),5);
end;


{---------------------------------------------------------------------------}
procedure Rand_Test;
  {-Test with random plain text}
var
  n,Err: integer;
begin

  randmem(@pt0, sizeof(pt0));
  pt := pt0;

  for n:=1 to BSIZE do begin
    if AES_CBC_Init_Encr(key128, 128, IV, context)<>0 then begin
      writeln('*** Error CBC_Init_Encr');
      exit;
    end;
    Err := AES_CBC_Encrypt(@pt, @ct, n, context);
    if not compmem(@pt,@pt0,n+2) then begin
      writeln('Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      if AES_CBC_Init_Decr(key128, 128, IV, context)<>0 then begin
        writeln('*** Error CBC_Init_Decr');
        exit;
      end;
      Err := AES_CBC_Decrypt(@ct, @pd, n, context);
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
      if (n<AESBLKSIZE) and (Err=AES_Err_Invalid_Length) then write('  (OK)');
      writeln;
    end;
  end;
end;

begin
  writeln;
  {$ifdef USEDLL}
    writeln('Test program for AES_DLL V',AES_DLL_Version,'   (C) 2004-2008  W.Ehrhardt');
  {$else}
    writeln('Test program for AES functions    (C) 2004-2008  W.Ehrhardt');
  {$endif}
  writeln('AES-CBC cipher text stealing');
  writeln;
  writeln('Test with random plain text');
  writeln('---------------------------');
  Rand_Test;
  writeln;
  writeln('Test vectors from RFC 3962');
  writeln('--------------------------');
  RFC_Test;
end.
