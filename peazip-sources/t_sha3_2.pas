{-Test prog for SHA3/SHAKE, WE Aug.2015}

program t_sha3_2;

{Test SHA3-225/256/384/512. Uses renamed and EOL converted test vectors}
{from the Keccak team: https://github.com/gvanas/KeccakCodePackage}

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  BTypes, Base2n, Mem_Util, SHA3;

var
  bitlen: longint;
  state:  TSHA3State;

  msg: array[0..1023] of byte;
  dig: array[0..63] of byte;
  buf: array[0..128] of byte;
  LM, LD, LDig: word;
  tf: text;
  tst: integer;

{$ifdef BIT16}
var
  i,k: integer;
  s: string;
const
  BITMAX = 990;
{$else}
var
  i,k: longint;
  {$ifdef UNICODE}
    s: string;
  {$else}
    s: ansistring;
  {$endif}
const
  BITMAX = 2047;
{$endif}


const
  algo_arr: array[1..4] of TSHA3_Algo = (__SHA3_224, __SHA3_256, __SHA3_384, __SHA3_512);
  fkat_arr: array[1..4] of string[20] = ('SMK3_224.txt', 'SMK3_256.txt', 'SMK3_384.txt', 'SMK3_512.txt');
  ldig_arr: array[1..4] of word = (224 div 8, 256 div 8, 384 div 8, 512 div 8);

{Original file names from Keccak site:
 ShortMsgKAT_SHA3-224.txt
 ShortMsgKAT_SHA3-256.txt
 ShortMsgKAT_SHA3-384.txt
 ShortMsgKAT_SHA3-512.txt
}

label
  done;

begin

  writeln('Test program for SHA3-225/256/384/512  (c) 2014-2015 W.Ehrhardt');
  writeln('---------------------------------------------------------------');

  for tst:=1 to 4 do begin
    writeln('*** KAT file ', fkat_arr[tst]);
    {$ifdef UNICODE}
      assign(tf, string(fkat_arr[tst]));
    {$else}
      assign(tf, fkat_arr[tst]);
    {$endif}
    reset(tf);
    bitlen := 0;
    ldig := ldig_arr[tst];
    repeat
      repeat
        if eof(tf) then begin
          writeln('No (more) test case found');
          goto done;
        end;
        readln(tf,s);
      until pos('Len = ',s)=1;
      {$ifdef debug}
        write(s,#13);
      {$endif}

      s := copy(s,7,length(s));
      val(s,bitlen,i);
      if i<>0 then begin
        writeln('Error bitlength for ',s);
        halt;
      end;

      readln(tf,s);
      if pos('Msg = ',s)<>1 then begin
        writeln('Expected "Msg = " not found');
        halt;
      end;

      s := copy(s,7,length(s));
      {$ifdef BIT16}
        DecodeBase16Str(s,@msg,sizeof(msg),LM);
      {$else}
        DecodeBase16AStr({$ifdef UNICODE}ansistring{$endif}(s),@msg,sizeof(msg),LM);
      {$endif}
      if (bitlen>0) and (LM <> (bitlen+7) div 8) then begin
        writeln('Msg length conflict with Len = ', bitlen);
        writeln('Read=',s);
        halt;
      end;

      readln(tf,s);
      if pos('MD = ',s)<>1 then begin
        writeln('Expected "MD = " not found');
        halt;
      end;

      s := copy(s,6,length(s));
      {$ifdef BIT16}
        DecodeBase16Str(s,@dig,sizeof(dig),LD);
      {$else}
        DecodeBase16AStr({$ifdef UNICODE}ansistring{$endif}(s),@dig,sizeof(dig),LD);
      {$endif}
      if LD<>ldig then begin
        writeln('Invalid digest length');
        halt;
      end;

      i := SHA3_Init(state,algo_arr[tst]);
      k := bitlen shr 3;
      if i=0 then i := SHA3_Update(state, @msg, k);
      if bitlen and 7 =0 then begin
        if i=0 then i := SHA3_FinalHash(state,@buf);
      end
      else begin
        if i=0 then i := SHA3_FinalBit_LSB(state, msg[k], bitlen and 7,  @buf, ldig*8);
      end;
      if i=0 then begin
        if not compmem(@buf, @dig, LDig) then writeln('Failed for Len = ',bitlen);
      end
      else writeln('Error ',i,' for Len = ',bitlen);
    until bitlen>=BITMAX;

  done:
    {$ifdef debug}
      writeln;
    {$endif}
    writeln('Done. Max. bit length = ', bitlen);
    close(tf);
  end;
end.

