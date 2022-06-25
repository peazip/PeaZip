{-Test prog for mem_util/base64 string routines, we Oct. 2005}

program T_Base64;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


(*
From: RFC 3548 - The Base16, Base32, and Base64 Data Encodings

Input data:  0x14fb9c03d97e
Output:  F      P      u      c        A      9      l      +

Input data:  0x14fb9c03d9
Output:  F      P      u      c        A      9      k      =

Input data:  0x14fb9c03
Output:  F      P      u      c        A      w      =      =
*)



uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  BTypes,mem_util;



{---------------------------------------------------------------------------}
procedure rfc3548_test;
  {-Small test with vector from rfc}
const
  t1 : string[6] = #$14#$fb#$9c#$03#$d9#$7e;
  r1 = 'FPucA9l+';
  r2 = 'FPucA9k=';
  r3 = 'FPucAw==';
var
  t,s0,s1,s2,s3: string[80];
begin
  writeln('RFC3548 test');
  writeln('============================');

  writeln('RFC3548 encode');
  writeln('string':10, 'Enc OK':9);
  s0 := Base64EncStr(t1);    writeln(s0:10,s0=r1:9);
  s1 := Base64Str(@t1[1],6); writeln(s1:10,s1=r1:9);
  s2 := Base64Str(@t1[1],5); writeln(s2:10,s2=r2:9);
  s3 := Base64Str(@t1[1],4); writeln(s3:10,s3=r3:9);

  writeln('RFC3548 decode');
  writeln('string':10, 'Dec OK':9);

  t := Base64DecStr(s0);   writeln(r1:10, t=t1:9);
  t := Base64DecStr(s1);   writeln(r1:10, t=t1:9);
  t := Base64DecStr(s2);   writeln(r1:10, t=copy(t1,1,5):9);
  t := Base64DecStr(s3);   writeln(r1:10, t=copy(t1,1,4):9);
end;


{---------------------------------------------------------------------------}
procedure randomtest;
  {-Encode/decode random strings}
const
  nmax=255*6 div 8;
  loop=50;
var
  i,l,n: integer;
  r,s,t: BString;
begin
  writeln('Random test');
  for l:=1 to loop do begin
    for n:=0 to nmax do begin
      r := '';
      for i:=1 to n do r := r + char8(random(256));
      s := Base64EncStr(r);
      t := Base64DecStr(s);
      if r<>t then begin
        writeln('Enc/Dec error for n=',n, ' random input:');
        for i:=1 to n do write(ord(r[i]):4);
        writeln;
        exit;
      end;
    end;
  end;
  writeln('Done');
end;

begin
  rfc3548_test;
  randomtest;
end.
