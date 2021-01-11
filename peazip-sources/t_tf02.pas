{Twofish test program, we May 2006}

(* from http://www.schneier.com/code/twofish-kat.zip
FILENAME:  "ecb_tbl.txt"
Electronic Codebook (ECB) Mode
Tables Known Answer Test
Tests permutation tables and MDS matrix multiply tables.
*)

program t_tf02;

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
  mem_util,tf_base;


const
  CT128: TTFBlock = ($5d,$9d,$4e,$ef,$fa,$91,$51,$57,$55,$24,$f1,$15,$81,$5a,$12,$e0);
  CT192: TTFBlock = ($e7,$54,$49,$21,$2b,$ee,$f9,$f4,$a3,$90,$bd,$86,$0a,$64,$09,$41);
  CT256: TTFBlock = ($37,$fe,$26,$ff,$1c,$f6,$61,$75,$f5,$dd,$f4,$c3,$3b,$97,$a2,$05);


var
  ctx: TTFContext;
  err: integer;
  key: array[0..31] of byte;
  PT,CT: TTFBlock;


procedure Test128;
var
  i:integer;
begin
  write('128 bit key - ');
  fillchar(CT,sizeof(CT),0);
  fillchar(PT,sizeof(PT),0);
  for i:=1 to 49 do begin
    move(PT, key, sizeof(PT));
    PT := CT;
    err := TF_Init(key, 128, ctx);
    if err<>0 then begin
      writeln(' ** TF_Init: ', err);
      halt;
    end;
    TF_Encrypt(ctx, PT, CT);
  end;
  writeln(' Passed: ', compmem(@ct,@CT128,sizeof(ct)));
end;

procedure Test192;
var
  i,j:integer;
begin
  write('192 bit key - ');
  fillchar(CT,sizeof(CT),0);
  fillchar(PT,sizeof(PT),0);
  fillchar(key,sizeof(key),0);
  for i:=1 to 49 do begin
    for j:=23 downto 16 do key[j] := key[j-16];
    move(PT, key, sizeof(PT));
    PT := CT;
    err := TF_Init(key, 192, ctx);
    if err<>0 then begin
      writeln(' ** TF_Init: ', err);
      halt;
    end;
    TF_Encrypt(ctx, PT, CT);
  end;
  writeln(' Passed: ', compmem(@ct,@CT192,sizeof(ct)));
end;


procedure Test256;
var
  i,j:integer;
begin
  write('256 bit key - ');
  fillchar(CT,sizeof(CT),0);
  fillchar(PT,sizeof(PT),0);
  fillchar(key,sizeof(key),0);
  for i:=1 to 49 do begin
    for j:=31 downto 16 do key[j] := key[j-16];
    move(PT, key, sizeof(PT));
    PT := CT;
    err := TF_Init(key, 256, ctx);
    if err<>0 then begin
      writeln(' ** TF_Init: ', err);
      halt;
    end;
    TF_Encrypt(ctx, PT, CT);
  end;
  writeln(' Passed: ', compmem(@ct,@CT256,sizeof(ct)));
end;

begin
  writeln('T_TF02 - Twofish test program (ECB_TBL.TXT)   (c) 2006 W.Ehrhardt');
  Test128;
  Test192;
  Test256;
end.
