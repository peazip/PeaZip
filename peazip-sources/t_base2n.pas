{-Test prog for Base2N unit, we Sep. 2004}

program T_Base2N;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}




uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  BTypes,
  base2n,
  mem_util;




{---------------------------------------------------------------------------}
procedure rfc3548_test;
  {- From: RFC 3548 - The Base16, Base32, and Base64 Data Encodings}
  { Input data:  0x14fb9c03d97e
    Output:  F      P      u      c        A      9      l      +
    Input data:  0x14fb9c03d9
    Output:  F      P      u      c        A      9      k      =
    Input data:  0x14fb9c03
    Output:  F      P      u      c        A      w      =      =}
const
  t1: array[1..6] of byte = ($14, $fb, $9c, $03, $d9, $7e);
  r1 = 'FPucA9l+';
  r2 = 'FPucA9k=';
  r3 = 'FPucAw==';
var
  s1,s2,s3: string[80];
  buf: array[byte] of byte;
  L: word;
begin
  writeln;
  writeln('RFC3548 test');
  writeln('RFC3548 encode');
  writeln('string':10, 'Enc OK':9);
  s1 := EncodeBase64Str(@t1,6); writeln(s1:10,s1=r1:9);
  s2 := EncodeBase64Str(@t1,5); writeln(s2:10,s2=r2:9);
  s3 := EncodeBase64Str(@t1,4); writeln(s3:10,s3=r3:9);
  writeln('RFC3548 decode');
  writeln('string':10, 'Len OK':9, 'Dec OK':9);

  fillchar(buf, sizeof(buf), 0);
  DecodeBase64Str(s1, @buf, sizeof(buf), L);
  writeln(r1:10, L=6:9, CompMem(@buf, @t1, 6):9);

  fillchar(buf, sizeof(buf), 0);
  DecodeBase64Str(s2, @buf, sizeof(buf), L);
  writeln(r2:10, L=5:9, CompMem(@buf, @t1, 5):9);

  fillchar(buf, sizeof(buf), 0);
  DecodeBase64Str(s3, @buf, sizeof(buf), L);
  writeln(r3:10, L=4:9, CompMem(@buf, @t1, 4):9);
end;


{---------------------------------------------------------------------------}
procedure random_test;
var
  i: integer;
  L: word;
  b0, b1: array[0..255] of byte;
  s: Str255;
begin
  writeln;
  writeln('Random test 16/32/64');
  for i:=0 to 255 do b0[i] := random(256);
  for i:=0 to 127 do begin
    s := EncodeBase64Str(@b0, i);
    DecodeBase64Str(s, @b1, sizeof(b1), L);
    if (L<>i) or not CompMem(@b0, @b1, L) then begin
      writeln('Base64 error: i=',i, ', b0=$',HexStr(@b0, i));
      exit;
    end;
    s := EncodeBase32Str(@b0, i);
    DecodeBase32Str(s, @b1, sizeof(b1), L);
    if (L<>i) or not CompMem(@b0, @b1, L) then begin
      writeln('Base32 error: i=',i, ', b0=$',HexStr(@b0, i));
      exit;
    end;
    s := EncodeBase16Str(@b0, i);
    DecodeBase16Str(s, @b1, sizeof(b1), L);
    if (L<>i) or not CompMem(@b0, @b1, L) then begin
      writeln('Base16 error: i=',i, ', b0=$',HexStr(@b0, i));
      exit;
    end;
  end;
  writeln('Done');
end;


{---------------------------------------------------------------------------}
procedure random_test_N;
  {-Random test for base 2**N, N=1..6}
var
  i: integer;
  L,N: word;
  b0, b1: array[0..255] of byte;
  s: Str255;
begin
  writeln;
  writeln('Random test N');
  for i:=0 to 255 do b0[i] := random(256);
  for N:=1 to 6 do begin
    for i:=0 to (N*255 div 8) do begin
      s := EncodeBase2NStr(@b0, i, N);
      DecodeBase2NStr(s, @b1, sizeof(b1), N, L);
      if (L<>i) or not CompMem(@b0, @b1, L) then begin
        writeln('Base2N error: N=',N ,', i=',i, ', b0=$',HexStr(@b0, i));
        exit;
      end;
    end;
  end;
  writeln('Done');
end;


{---------------------------------------------------------------------------}
procedure tsd_test;
  {-From Tom StDenis, LTC1.0+ base64_tv.txt}
const
  tsdarr: array[0..32] of string[50] = (
            '',
            'AA==',
            'AAE=',
            'AAEC',
            'AAECAw==',
            'AAECAwQ=',
            'AAECAwQF',
            'AAECAwQFBg==',
            'AAECAwQFBgc=',
            'AAECAwQFBgcI',
            'AAECAwQFBgcICQ==',
            'AAECAwQFBgcICQo=',
            'AAECAwQFBgcICQoL',
            'AAECAwQFBgcICQoLDA==',
            'AAECAwQFBgcICQoLDA0=',
            'AAECAwQFBgcICQoLDA0O',
            'AAECAwQFBgcICQoLDA0ODw==',
            'AAECAwQFBgcICQoLDA0ODxA=',
            'AAECAwQFBgcICQoLDA0ODxAR',
            'AAECAwQFBgcICQoLDA0ODxAREg==',
            'AAECAwQFBgcICQoLDA0ODxAREhM=',
            'AAECAwQFBgcICQoLDA0ODxAREhMU',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFQ==',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRY=',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYX',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGA==',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBk=',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBka',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGw==',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxw=',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwd',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHg==',
            'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=');
var
  i,j,L: word;
  buf: array[1..80] of byte;
begin
  writeln;
  writeln('TSD test');
  for i:=0 to 32 do begin
    DecodeBase64Str(tsdarr[i], @buf, sizeof(buf), L);
    for j:=1 to L do begin
      if buf[j]<>j-1 then begin
        writeln('Diff i=',i, ', j=',j);;
        exit;
      end;
    end;
  end;
  writeln('Done');
end;



{---------------------------------------------------------------------------}
procedure rfc4648test;
type
  Tfbs = array[0..6] of string[16];
const
  BASE16   : Tfbs = ('','66','666F','666F6F','666F6F62','666F6F6261','666F6F626172');
  BASE32   : Tfbs = ('','MY======','MZXQ====','MZXW6===','MZXW6YQ=','MZXW6YTB','MZXW6YTBOI======');
  BASE32HEX: Tfbs = ('','CO======','CPNG====','CPNMU===','CPNMUOG=','CPNMUOJ1','CPNMUOJ1E8======');
  BASE64   : Tfbs = ('','Zg==','Zm8=','Zm9v','Zm9vYg==','Zm9vYmE=','Zm9vYmFy');
const
  foobar: string[6] = 'foobar';
var
  i: integer;
  L: word;
  s: str255;
  buf: array[0..63] of byte;
begin
  writeln;
  writeln('RFC4648 test');
  writeln('  Base64');
  for i:=0 to 6 do begin
    s := EncodeBase64Str(@foobar[1], i);
    if s<>Base64[i] then writeln('EncodeBase64Str: ', i);
    DecodeBase64Str(s, @buf, sizeof(buf), L);
    if (L<>i) or not CompMem(@buf, @foobar[1], i) then writeln('DecodeBase64Str: ', i);
  end;

  writeln('  Base32');
  for i:=0 to 6 do begin
    s := EncodeBase32Str(@foobar[1], i);
    if s<>Base32[i] then writeln('EncodeBase32Str: ', i);
    DecodeBase32Str(s, @buf, sizeof(buf), L);
    if (L<>i) or not CompMem(@buf, @foobar[1], i) then writeln('DecodeBase32Str: ', i);
  end;

  writeln('  Base32-Hex');
  for i:=0 to 6 do begin
    s := EncodeBase32HexStr(@foobar[1], i);
    if s<>Base32Hex[i] then writeln('EncodeBase32HexStr: ', i);
    DecodeBase32HexStr(s, @buf, sizeof(buf), L);
    if (L<>i) or not CompMem(@buf, @foobar[1], i) then writeln('DecodeBase32HexStr: ', i);
  end;

  writeln('  Base16');
  for i:=0 to 6 do begin
    s := EncodeBase16Str(@foobar[1], i);
    if s<>Base16[i] then writeln('EncodeBase16Str: ', i);
    DecodeBase16Str(s, @buf, sizeof(buf), L);
    if (L<>i) or not CompMem(@buf, @foobar[1], i) then writeln('DecodeBase16Str: ', i);
  end;
    writeln('Done');
end;

begin
  rfc3548_test;
  random_test;
  random_test_N;
  tsd_test;
  rfc4648test;
end.
