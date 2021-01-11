program t_shak02;

{Test SHAKE128 and SHAKE256. Uses renamed and EOL converted test vectors}
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
  dig: array[0..1023] of byte;
  buf: array[0..1023] of byte;
  LM, LD: word;
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
  algo_arr: array[1..2] of TSHA3_Algo = (__SHAKE_128, __SHAKE_256);
  fkat_arr: array[1..2] of string[20] = ('shake128.txt', 'shake256.txt');

{Original file names from Keccak site:
 ShortMsgKAT_SHAKE128.txt
 ShortMsgKAT_SHAKE256.txt
}

const
{$ifdef BIT16}
  ldig = 122;  {truncated Str255}
{$else}
  ldig = 512;
{$endif}

label
  done;

begin
  writeln('Test program for SHAKE128/256    (c) 2014-2015 W.Ehrhardt');
  writeln('---------------------------------------------------------');

  for tst:=1 to 2 do begin
    writeln('*** KAT file ', fkat_arr[tst]);
    {$ifdef UNICODE}
     assign(tf, string(fkat_arr[tst]));
    {$else}
     assign(tf, fkat_arr[tst]);
    {$endif}
    reset(tf);
    bitlen := 0;
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
      if pos('Squeezed = ',s)<>1 then begin
        writeln('Expected "Squeezed = " not found');
        halt;
      end;

      s := copy(s,12,length(s));
      {$ifdef BIT16}
        DecodeBase16Str(s,@dig,sizeof(dig),LD);
      {$else}
        DecodeBase16AStr({$ifdef UNICODE}ansistring{$endif}(s),@dig,sizeof(dig),LD);
      {$endif}
      if LD<>LDig then begin
        writeln('Invalid digest length');
        halt;
      end;

      i := SHA3_Init(state,algo_arr[tst]);
      k := bitlen shr 3;
      if i=0 then i := SHA3_Update(state, @msg, k);
      if i=0 then i := SHA3_FinalBit_LSB(state, msg[k], bitlen and 7, @buf, ldig*8);
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

