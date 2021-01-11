{-Test prog for AES ECB cipher text stealing, we Sep.2003}

program T_ECBCTS;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_ECB, mem_util;


const
  BSIZE = $400;

var
  Context: TAESContext;
  i,n,Err: integer;
  pt, pt0, ct, ct0, pd: array[1..BSIZE+2] of byte;

const
  key128 : array[0..15] of byte = ($2b,$7e,$15,$16,$28,$ae,$d2,$a6,
                                   $ab,$f7,$15,$88,$09,$cf,$4f,$3c);
begin
  writeln;
  writeln('=====================================');
  writeln('Test for AES-ECB cipher text stealing');

  for i:=1 to BSIZE do pt0[i] := random(256);
  pt := pt0;

  for n:=1 to BSIZE do begin
    Err := AES_ECB_Init_Encr(key128, 128, context);
    Err := Err or AES_ECB_Encrypt(@pt, @ct, n, context);
    if not compmem(@pt,@pt0,n+2) then begin
      writeln('Encr: src overwrite, n: ',n);
      halt;
    end;
    if Err=0 then begin
      ct0 := ct;
      Err := AES_ECB_Init_Decr(key128, 128, context);
      Err := Err or AES_ECB_Decrypt(@ct, @pd, n, context);
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
end.
