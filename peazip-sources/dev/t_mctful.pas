{AES 'Monte Carlo Tests' from rijndael-vals.zip,  we 06.2006}

program T_MCTFUL;

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
  BTypes,aes_type,aes_base,aes_cbc,aes_ecb,mem_util;


var
  logfile: text;

const
  IMAX = 399;
  JMAX = 9999;

{---------------------------------------------------------------------------}
procedure output({$ifdef CONST} const {$endif} s: str255);
  {-writeln to logfile}
begin
  writeln(logfile,s);
end;


{---------------------------------------------------------------------------}
function i2s(L: longint): str255;
var
  s: string[20];
begin
  str(L,s);
  i2s := s;
end;


{---------------------------------------------------------------------------}
procedure ECBEncr;
  {-Reproduce ecb_e_m.txt}

  procedure TestBits(kbits: word);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
    output('=========================');
    output('');
    output('KEYSIZE='+i2s(kbits));
    output('');
    fillchar(Key, sizeof(Key), 0);
    fillchar(PT, sizeof(PT), 0);
    CT := PT;
    for i:=0 to IMAX do begin
      if i and 7 = 0 then write('.');
      Err := AES_ECB_Init_Encr(Key, kbits, ctx);
      if Err<>0 then begin
        writeln('AES_ECB_Init_Encr error: ', Err);
        halt;
      end;
      output('I='+i2s(I));
      output('KEY='+HexStr(@Key, kbits div 8));
      output('PT='+HexStr(@CT,16));
      for j:=0 to JMAX do begin
        PT := CT;
        Err := AES_ECB_Encrypt(@CT, @CT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_ECB_Encrypt error: ', Err);
          halt;
        end;
      end;
      output('CT='+HexStr(@CT,16));
      output('');
      case kbits of
        128: for j:=0 to 15 do Key[j] := Key[j] xor CT[j];
        192: begin
               for j:=0 to  7 do Key[j]   := Key[j]   xor PT[8+j];
               for j:=0 to 15 do Key[j+8] := Key[j+8] xor CT[j];
             end;
        256: begin
               for j:=0 to 15 do Key[j]    := Key[j]    xor PT[j];
               for j:=0 to 15 do Key[j+16] := Key[j+16] xor CT[j];
             end;
      end;
    end;
    writeln;
  end;

begin
  assign(logfile, 'ecb_e_m.log');
  rewrite(logfile);
  writeln('ecb_e_m.log');
  output('');
  output('=========================');
  output('');
  output('FILENAME:  "ecb_e_m.txt"');
  output('');
  output('Electronic Codebook (ECB) Mode - ENCRYPTION');
  output('Monte Carlo Test');
  output('');
  output('Algorithm Name: Rijndael');
  output('Principal Submitter: Joan Daemen');
  output('');
  TestBits(128);
  TestBits(192);
  TestBits(256);
  output('===========');
  close(logfile);
end;

{---------------------------------------------------------------------------}
procedure ECBDecr;
  {-Reproduce ecb_d_m.txt}

  procedure TestBits(kbits: word);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
    output('=========================');
    output('');
    output('KEYSIZE='+i2s(kbits));
    output('');
    fillchar(Key, sizeof(Key), 0);
    fillchar(PT, sizeof(PT), 0);
    CT := PT;
    for i:=0 to IMAX do begin
      if i and 7 = 0 then write('.');
      Err := AES_ECB_Init_Decr(Key, kbits, ctx);
      if Err<>0 then begin
        writeln('AES_ECB_Init_Decr error: ', Err);
        halt;
      end;
      output('I='+i2s(I));
      output('KEY='+HexStr(@Key, kbits div 8));
      output('CT='+HexStr(@CT,16));
      for j:=0 to JMAX do begin
        PT := CT;
        Err := AES_ECB_Decrypt(@CT, @CT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_ECB_Decrypt error: ', Err);
          halt;
        end;
      end;
      output('PT='+HexStr(@CT,16));
      output('');
      case kbits of
        128: for j:=0 to 15 do Key[j] := Key[j] xor CT[j];
        192: begin
               for j:=0 to  7 do Key[j]   := Key[j]   xor PT[8+j];
               for j:=0 to 15 do Key[j+8] := Key[j+8] xor CT[j];
             end;
        256: begin
               for j:=0 to 15 do Key[j]    := Key[j]    xor PT[j];
               for j:=0 to 15 do Key[j+16] := Key[j+16] xor CT[j];
             end;
      end;
    end;
    writeln;
  end;

begin
  assign(logfile, 'ecb_d_m.log');
  rewrite(logfile);
  writeln('ecb_d_m.log');
  output('');
  output('=========================');
  output('');
  output('FILENAME:  "ecb_d_m.txt"');
  output('');
  output('Electronic Codebook (ECB) Mode - DECRYPTION');
  output('Monte Carlo Test');
  output('');
  output('Algorithm Name: Rijndael');
  output('Principal Submitter: Joan Daemen');
  output('');
  TestBits(128);
  TestBits(192);
  TestBits(256);
  output('===========');
  close(logfile);
end;


{---------------------------------------------------------------------------}
procedure CBCEncr;
  {-Reproduce cbc_e_m.txt}

  procedure TestBits(kbits: word);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    IV, PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
    output('==========');
    output('');
    output('KEYSIZE='+i2s(kbits));
    output('');
    fillchar(Key, sizeof(Key), 0);
    fillchar(PT, sizeof(PT), 0);
    fillchar(IV, sizeof(IV), 0);
    CT := PT;
    for i:=0 to IMAX do begin
      if i and 7 = 0 then write('.');
      Err := AES_CBC_Init_Encr(Key, kbits, IV, ctx);
      if Err<>0 then begin
        writeln('AES_CBC_Init_Encr error: ', Err);
        halt;
      end;
      output('I='+i2s(I));
      output('KEY='+HexStr(@Key, kbits div 8));
      output('IV='+HexStr(@IV,16));
      output('PT='+HexStr(@PT,16));
      for j:=0 to JMAX do begin
        CT := PT;
        PT := ctx.IV;
        Err := AES_CBC_Encrypt(@CT, @CT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_CBC_Encrypt error: ', Err);
          halt;
        end;
      end;
      IV := CT;
      output('CT='+HexStr(@CT,16));
      output('');
      case kbits of
        128: for j:=0 to 15 do Key[j] := Key[j] xor CT[j];
        192: begin
               for j:=0 to  7 do Key[j]   := Key[j]   xor PT[8+j];
               for j:=0 to 15 do Key[j+8] := Key[j+8] xor CT[j];
             end;
        256: begin
               for j:=0 to 15 do Key[j]    := Key[j]    xor PT[j];
               for j:=0 to 15 do Key[j+16] := Key[j+16] xor CT[j];
             end;
      end;
    end;
    writeln;
  end;

begin
  assign(logfile, 'cbc_e_m.log');
  rewrite(logfile);
  writeln('cbc_e_m.log');
  output('');
  output('=========================');
  output('');
  output('FILENAME:  "cbc_e_m.txt"');
  output('');
  output('Cipher Block Chaining (CBC) Mode - ENCRYPTION');
  output('Monte Carlo Test');
  output('');
  output('Algorithm Name: Rijndael');
  output('Principal Submitter: Joan Daemen');
  output('');
  TestBits(128);
  TestBits(192);
  TestBits(256);
  output('===========');
  close(logfile);
end;


{---------------------------------------------------------------------------}
procedure CBCDecr;
  {-Reproduce cbc_d_m.txt}

  procedure TestBits(kbits: word);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    IV, PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
    output('==========');
    output('');
    output('KEYSIZE='+i2s(kbits));
    output('');
    fillchar(Key, sizeof(Key), 0);
    fillchar(PT, sizeof(PT), 0);
    fillchar(IV, sizeof(IV), 0);
    for i:=0 to IMAX do begin
      if i and 7 = 0 then write('.');
      CT := PT;
      Err := AES_CBC_Init_Decr(Key, kbits, IV, ctx);
      if Err<>0 then begin
        writeln('AES_CBC_Init_Decr error: ', Err);
        halt;
      end;
      output('I='+i2s(I));
      output('KEY='+HexStr(@Key, kbits div 8));
      output('IV='+HexStr(@IV,16));
      output('CT='+HexStr(@CT,16));
      PT := CT;
      for j:=0 to JMAX do begin
        CT := PT;
        Err := AES_CBC_Decrypt(@PT, @PT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_CBC_Decrypt error: ', Err);
          halt;
        end;
      end;
      IV := ctx.IV;
      output('PT='+HexStr(@PT,16));
      output('');
      case kbits of
        128: for j:=0 to 15 do Key[j] := Key[j] xor PT[j];
        192: begin
               for j:=0 to  7 do Key[j]   := Key[j]   xor CT[8+j];
               for j:=0 to 15 do Key[j+8] := Key[j+8] xor PT[j];
             end;
        256: begin
               for j:=0 to 15 do Key[j]    := Key[j]    xor CT[j];
               for j:=0 to 15 do Key[j+16] := Key[j+16] xor PT[j];
             end;
      end;
    end;
    writeln;
  end;

begin
  assign(logfile, 'cbc_d_m.log');
  rewrite(logfile);
  writeln('cbc_d_m.log');
  output('');
  output('=========================');
  output('');
  output('FILENAME:  "cbc_d_m.txt"');
  output('');
  output('Cipher Block Chaining (CBC) Mode - DECRYPTION');
  output('Monte Carlo Test');
  output('');
  output('Algorithm Name: Rijndael');
  output('Principal Submitter: Joan Daemen');
  output('');
  TestBits(128);
  TestBits(192);
  TestBits(256);
  output('===========');
  close(logfile);
end;


begin
  writeln('T_MCTFUL - Full Monte Carlo Tests to <name>.LOG  (c) 2006 W.Ehrhardt');
  HexUpper := true;
  ECBEncr;
  ECBDecr;
  CBCEncr;
  CBCDecr;
end.

