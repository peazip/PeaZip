{AES 'Monte Carlo Self Tests' from rijndael-vals.zip,  we 06.2006}

program T_MCST;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}

{$r+}
uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  BTypes,aes_type,aes_base,aes_cbc,aes_ecb,mem_util;

const
  IMAX = 399;
  JMAX = 9999;

{---------------------------------------------------------------------------}
procedure ECBEncr;
  {-Reproduce ecb_e_m.txt}

  procedure TestBits(kbits: word; ts: BString);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
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
      for j:=0 to JMAX do begin
        PT := CT;
        Err := AES_ECB_Encrypt(@CT, @CT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_ECB_Encrypt error: ', Err);
          halt;
        end;
      end;
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
    writeln(' ', ts=HexStr(@CT,16));
  end;

const
  CT128='A04377ABE259B0D0B5BA2D40A501971B';
  CT192='4E46F8C5092B29E29A971A0CD1F610FB';
  CT256='1F6763DF807A7E70960D4CD3118E601A';
begin
  writeln('ecb_e_m');
  TestBits(128, CT128);
  TestBits(192, CT192);
  TestBits(256, CT256);
end;

{---------------------------------------------------------------------------}
procedure ECBDecr;
  {-Reproduce ecb_d_m.txt}

  procedure TestBits(kbits: word; ts: BString);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
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
      for j:=0 to JMAX do begin
        PT := CT;
        Err := AES_ECB_Decrypt(@CT, @CT, 16, ctx);
        if Err<>0 then begin
          writeln('AES_ECB_Decrypt error: ', Err);
          halt;
        end;
      end;
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
    writeln(' ', ts=HexStr(@CT,16));
  end;

const
  PT128='F5BF8B37136F2E1F6BEC6F572021E3BA';
  PT192='F1A81B68F6E5A6271A8CB24E7D9491EF';
  PT256='4DE0C6DF7CB1697284604D60271BC59A';
begin
  writeln('ecb_d_m');
  TestBits(128, PT128);
  TestBits(192, PT192);
  TestBits(256, PT256);
end;


{---------------------------------------------------------------------------}
procedure CBCEncr;
  {-Reproduce cbc_e_m.txt}

  procedure TestBits(kbits: word; ts: BString);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    IV, PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
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
    writeln(' ',ts=HexStr(@CT,16));
  end;

const
  CT128='2F844CBF78EBA70DA7A49601388F1AB6';
  CT192='BA50C94440C04A8C0899D42658E25437';
  CT256='C0FEFFF07506A0B4CD7B8B0CF25D3664';
begin
  writeln('cbc_e_m');
  TestBits(128, CT128);
  TestBits(192, CT192);
  TestBits(256, CT256);
end;


{---------------------------------------------------------------------------}
procedure CBCDecr;
  {-Reproduce cbc_d_m.txt}

  procedure TestBits(kbits: word; ts: BString);
    {-generate part for keysize kbits}
  var
    i,j,Err: Integer;
    IV, PT, CT: TAESBlock;
    Key: array[0..31] of byte;
    ctx: TAESContext;
  begin
    write(kbits, ' bits ');
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
    writeln(' ',ts=HexStr(@PT,16));
  end;

const
  PT128='9B8FB71E035CEFF9CBFA1346E5ACEFE0';
  PT192='6342BFDDD2F6610350458B6695463484';
  PT256='CD6429CF3F81F8B4F82BC627A8283096';
begin
  writeln('cbc_d_m');
  TestBits(128, PT128);
  TestBits(192, PT192);
  TestBits(256, PT256);
end;


begin
  writeln('T_MCST - AES Monte Carlo Self Tests     (c) 2006 W.Ehrhardt');
  HexUpper := true;
  ECBEncr;
  ECBDecr;
  CBCEncr;
  CBCDecr;
end.

