unit Base2N;


{Base 2**N conversion (1 <= N <= 6), N=0 is treated as Base32-Hex}


interface


(*************************************************************************

 DESCRIPTION     :  Base 2**N conversion, 1 <= N <= 6
                    (N=0 is treated as Base32-Hex)


 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX


 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  o RFC 4648 - The Base16, Base32, and Base64 Data Encodings
                    o RFC 3548 (obsolete)- The Base16, Base32, and Base64 Data Encodings

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.09.04  W.Ehrhardt  Initial version
 0.20     18.09.04  we          array of chars, CONST
 0.30     23.10.04  we          S2BPrim, Str2NBase, SxxB routines (BP7+)
 0.31     24.10.04  we          Encode2NPrim, EncodeBasexx/Str, TBaseStr
 0.32     24.10.04  we          Decode2NPrim, TP5-6
 0.33     24.10.04  we          DecodeBasexx/Str
 0.34     25.10.04  we          AnsiString versions
 0.35     31.10.04  we          Rangecheck code in Decode2NPrim
 0.36     28.11.06  we          Base32-Hex, bugfix NPB=8 for Base32
 0.37     01.12.06  we          StrictCase
 0.38     28.03.08  we          Ansistring for BIT32 instead of WIN32
 0.39     14.11.08  we          BTypes, str255, char8/pchar8, Ptr2Inc
 0.40     05.07.09  we          D12 fix for EncodeBase2NStr (use setlength)
 0.41     18.02.12  we          64-bit changes
 0.42     17.05.17  we          Fix debug mode range check error for @s[1] in DecodeBase2NAStr
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2017 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

(*todo
- var error, check decode for strict cases (correct length, trailing '=')
- upper/lowercase for base2 ... base32
- win32/prim procs with longint
*)

{$i STD.INC}

uses
  BTypes;


const
  StrictCase: boolean = false;  {strict case for N=1..5: if true uppercase only,}
                                {else lowercase allowed}

procedure EncodeBase2N(psrc,pdest: pointer; lsrc,ldest,N: word; var LA: word);
  {-Base 2**N encode src to dest, LA result length of dest string, Base32-Hex if N=0}

procedure EncodeBase64(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base64 encode src to dest, LA result length of dest string}

procedure EncodeBase32(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base32 encode src to dest, LA result length of dest string}

procedure EncodeBase32Hex(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base32-Hex encode src to dest, LA result length of dest string}

procedure EncodeBase16(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base16 encode src to dest, LA result length of dest string}


function  EncodeBase2NStr(psrc: pointer; lsrc, N: word): str255;
  {-Base 2**N string of memory block of length lsrc pointed by psrc, Base32-Hex if N=0}

function  EncodeBase64Str(psrc: pointer; lsrc: word): str255;
  {-Base64 string of memory block of length lsrc pointed by psrc}

function  EncodeBase32Str(psrc: pointer; lsrc: word): str255;
  {-Base32 string of memory block of length lsrc pointed by psrc}

function  EncodeBase32HexStr(psrc: pointer; lsrc: word): str255;
  {-Base32-Hex string of memory block of length lsrc pointed by psrc}

function  EncodeBase16Str(psrc: pointer; lsrc: word): str255;
  {-Base16 string of memory block of length lsrc pointed by psrc}


procedure DecodeBase2N(psrc,pdest: pointer; lsrc,ldest,N: word; var LA: word);
  {-Decode Base 2**N src to dest, LA result length of dest string, Base32-Hex if N=0}

procedure DecodeBase64(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base64 src to dest, LA result length of dest string}

procedure DecodeBase32(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base32 src to dest, LA result length of dest string}

procedure DecodeBase32Hex(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base32-Hex src to dest, LA result length of dest string}

procedure DecodeBase16(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base16 src to dest, LA result length of dest string}


procedure DecodeBase2NStr({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest, N: word; var LA: word);
  {-Decode Base 2**N string to dest, LA/ldest result/max length of dest, Base32-Hex if N=0}

procedure DecodeBase64Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base64 src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase32Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase32HexStr({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32-Hex src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase16Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base16 src string to dest, LA/ldest result/max length of dest}


{$ifndef BIT16}
function  EncodeBase2NAStr(psrc: pointer; lsrc, N: word): ansistring;
  {-Base 2**N string of memory block of length lsrc pointed by psrc, Base32-Hex if N=0}

function  EncodeBase64AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base64 string of memory block of length lsrc pointed by psrc}

function  EncodeBase32AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base32 string of memory block of length lsrc pointed by psrc}

function  EncodeBase32HexAStr(psrc: pointer; lsrc: word): ansistring;
  {-Base32-Hex string of memory block of length lsrc pointed by psrc}

function  EncodeBase16AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base16 string of memory block of length lsrc pointed by psrc}

procedure DecodeBase2NAStr(const s: ansistring; pdest: pointer; ldest, N: word; var LA: word);
  {-Decode Base 2**N string to dest, LA/ldest result/max length of dest, Base32-Hex if N=0}

procedure DecodeBase64AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base64 src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase32AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase32HexAStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}

procedure DecodeBase16AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base16 src string to dest, LA/ldest result/max length of dest}
{$endif}


implementation


const
  CB64: array[0..63] of char8 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  CB32: array[0..31] of char8 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567';
 CB32H: array[0..31] of char8 = '0123456789ABCDEFGHIJKLMNOPQRSTUV';
  CB16: array[0..15] of char8 = '0123456789ABCDEF';


const
  mask: array[0..15] of word = ($0000,$0001,$0003,$0007,$000F,$001F,$003F,$007F,
                                $00FF,$01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF);

{---------------------------------------------------------------------------}
procedure Encode2NPrim(psrc,pdest: pointer; lsrc,ldest, N, NPB: word;
                       {$ifdef CONST} const {$else} var {$endif} c; var LA: word);
  {-Encode bytes from psrc to Base2**N chars in pdest; lsrc=length of src;}
  { ldest=max length of dest, LA=result length of encoded string, NPB=number}
  { of pad bytes, c=encode table}
var
  buf: word;
  bits,rest: integer;
  ca: array[0..63] of char8 absolute c;

  procedure app(c: char8);
    {-append one char to dest with: check length, inc pointers}
  begin
    if LA<ldest then begin
      pchar8(pdest)^ := c;
      inc(Ptr2Inc(pdest));
      inc(LA);
    end;
  end;

begin
  LA := 0;
  if (N<1) or (N>6) or (psrc=nil) or (pdest=nil) or (ldest<1) or (lsrc<1) then exit;

  bits := 0;
  buf  := 0;

  {Calculate count of = padding chars, note that if s has enough}
  {capacity to hold all output, pad to a multiple of block size}
  if NPB>0 then begin
    rest := ((longint(lsrc)*8 + N-1) div N) mod NPB;
    if rest>0 then rest := NPB - rest;
  end
  else rest:=0;

  {process input bytes}
  while lsrc>0 do begin
    buf := (buf shl 8) or pByte(psrc)^;
    inc(Ptr2Inc(psrc));
    dec(lsrc);
    inc(bits,8);
    while bits > N do begin
      dec(bits,N);
      app(ca[buf shr bits]);
      buf := buf and mask[bits];
    end;
  end;

  {remaining partial input}
  if bits>0 then begin
    if LA<ldest then app(ca[buf shl (N-bits)]);
  end;

  {pad with '='}
  while rest>0 do begin
    app('=');
    dec(rest);
  end;
end;


{---------------------------------------------------------------------------}
procedure EncodeBase2N(psrc,pdest: pointer; lsrc,ldest,N: word; var LA: word);
  {-Base 2**N encode src to dest, LA result length of dest string, Base32-Hex if N=0}
begin
  case N of
      0: Encode2NPrim(psrc,pdest,lsrc,ldest,5,8,CB32H,LA);
      5: Encode2NPrim(psrc,pdest,lsrc,ldest,N,8,CB32,LA);
      6: Encode2NPrim(psrc,pdest,lsrc,ldest,N,4,CB64,LA);
    else Encode2NPrim(psrc,pdest,lsrc,ldest,N,0,CB16,LA);
  end;
end;


{---------------------------------------------------------------------------}
procedure EncodeBase64(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base64 encode src to dest, LA result length of dest string}
begin
  EncodeBase2N(psrc,pdest,lsrc,ldest,6,LA);
end;


{---------------------------------------------------------------------------}
procedure EncodeBase32(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base32 encode src to dest, LA result length of dest string}
begin
  EncodeBase2N(psrc,pdest,lsrc,ldest,5,LA);
end;


{---------------------------------------------------------------------------}
procedure EncodeBase32Hex(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base32-Hex encode src to dest, LA result length of dest string}
begin
  EncodeBase2N(psrc,pdest,lsrc,ldest,0,LA);
end;


{---------------------------------------------------------------------------}
procedure EncodeBase16(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Base16 encode src to dest, LA result length of dest string}
begin
  EncodeBase2N(psrc,pdest,lsrc,ldest,4,LA);
end;


{---------------------------------------------------------------------------}
function EncodeBase2NStr(psrc: pointer; lsrc, N: word): str255;
  {-Base 2**N string of memory block of length lsrc pointed by psrc}
var
  LA: word;
  s: str255;
begin
  EncodeBase2N(psrc,@s[1],lsrc,255,N,LA);
  {$ifndef BIT16}
    if LA>255 then LA := 255;
    setlength(s,LA);
  {$else}
    if LA<255 then s[0]:=chr(LA) else s[0]:=#255;
  {$endif}
  EncodeBase2NStr := s;
end;


{---------------------------------------------------------------------------}
function EncodeBase64Str(psrc: pointer; lsrc: word): str255;
  {-Base64 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase64Str := EncodeBase2NStr(psrc,lsrc,6);
end;


{---------------------------------------------------------------------------}
function EncodeBase32Str(psrc: pointer; lsrc: word): str255;
  {-Base32 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase32Str := EncodeBase2NStr(psrc,lsrc,5);
end;


{---------------------------------------------------------------------------}
function  EncodeBase32HexStr(psrc: pointer; lsrc: word): str255;
  {-Base32-Hex string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase32HexStr := EncodeBase2NStr(psrc,lsrc,0);
end;


{---------------------------------------------------------------------------}
function EncodeBase16Str(psrc: pointer; lsrc: word): str255;
  {-Base16 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase16Str := EncodeBase2NStr(psrc,lsrc,4);
end;


{---------------------------------------------------------------------------}
procedure Decode2NPrim(psrc,pdest: pointer; lsrc,ldest,N: word;
                       {$ifdef CONST} const {$else} var {$endif} c; var LA: word);
  {-Decodes Base2**N chars from psrc to bytes in pdest; lsrc=length of src;}
var
  i,bits,buf: word;
  {$ifopt R+}
    hmask: word;
  {$endif}

  IC: array[char8] of byte;
  b: byte;
  cu: char8;
  ca: array[0..63] of char8 absolute c;
const
  UInc = ord('a')-ord('A');
begin
  LA := 0;
  if (N<1) or (N>6) or (psrc=nil) or (pdest=nil) or (ldest<1) or (lsrc<1) then exit;

  {Fill input array with Base2**N digit values, $FF if not valid}
  fillchar(IC, sizeof(IC), $FF);
  for i:=0 to pred(1 shl N) do ic[ca[i]] := i;
  if not StrictCase then begin
    for i:=0 to pred(1 shl N) do begin
      cu := ca[i];
      if (cu>='A') and (cu<='Z') then inc(byte(cu),UInc);
      ic[cu] := i;
    end;
  end;

  {$ifopt R+}
    hmask:= mask[16-N];
  {$endif}
  buf  := 0;
  bits := 0;
  while lsrc>0 do begin
    b := IC[pchar8(psrc)^];
    inc(Ptr2Inc(psrc));
    dec(lsrc);
    if b>127 then exit;
    {include next input character into buffer}
    {if range checking is on, we must prevent the following shift}
    {from overflowing buf. Either here with hmask or in [*] below}
    {$ifopt R+}
      buf := ((buf and hmask) shl N) or b;
    {$else}
      buf := (buf  shl N) or b;
    {$endif}
    inc(bits,N);
    if bits>7 then begin
      {output a byte if at least 8 bits in input buf}
      dec(bits,8);
      if LA<ldest then begin
        inc(LA);
        pByte(pdest)^ := (buf shr bits) and $FF;
        {[*] buf := buf and mask[bits];}
        inc(Ptr2Inc(pdest));
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure DecodeBase2N(psrc,pdest: pointer; lsrc,ldest,N: word; var LA: word);
  {-Decode Base 2**N src to dest, LA result length of dest string, Base32-Hex if N=0}
begin
  case N of
      0: Decode2NPrim(psrc,pdest,lsrc,ldest,5,CB32H,LA);
      5: Decode2NPrim(psrc,pdest,lsrc,ldest,N,CB32,LA);
      6: Decode2NPrim(psrc,pdest,lsrc,ldest,N,CB64,LA);
    else Decode2NPrim(psrc,pdest,lsrc,ldest,N,CB16,LA);
  end;
end;


{---------------------------------------------------------------------------}
procedure DecodeBase64(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base64 src to dest, LA result length of dest string}
begin
  DecodeBase2N(psrc,pdest,lsrc,ldest,6,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base32 src to dest, LA result length of dest string}
begin
  DecodeBase2N(psrc,pdest,lsrc,ldest,5,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32Hex(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base32-Hex src to dest, LA result length of dest string}
begin
  Decode2NPrim(psrc,pdest,lsrc,ldest,5,CB32H,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase16(psrc,pdest: pointer; lsrc,ldest: word; var LA: word);
  {-Decode Base16 src to dest, LA result length of dest string}
begin
  DecodeBase2N(psrc,pdest,lsrc,ldest,4,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase2NStr({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest, N: word; var LA: word);
  {-Decode Base 2**N string to dest, LA/ldest result/max length of dest, Base32-Hex if N=0}
begin
  DecodeBase2N(@s[1],pdest,length(s),ldest,N,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase64Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base64 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NStr(s,pdest,ldest,6,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NStr(s,pdest,ldest,5,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32HexStr({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32-Hex src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NStr(s,pdest,ldest,0,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase16Str({$ifdef CONST} const {$endif} s: str255; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base64 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NStr(s,pdest,ldest,4,LA);
end;


{$ifndef BIT16}
{---------------------------------------------------------------------------}
function  EncodeBase2NAStr(psrc: pointer; lsrc, N: word): ansistring;
  {-Base 2**N string of memory block of length lsrc pointed by psrc, Base32-Hex if N=0}
var
  LA: word;
  LL: longint;
  {$ifndef RESULT}
    result: ansistring;
  {$endif}
begin
  if N=0 then LA:=5 else LA:=N;
  LL := 6+((longint(lsrc)*8 + LA-1) div LA);
  if LL > $FFFE then LL := $FFFE;
  setlength(result, LL);
  EncodeBase2N(psrc,@result[1],lsrc, LL and $FFFF,N,LA);
  if LA<LL then setlength(result,LA);
  {$ifndef RESULT}
    EncodeBase2NAStr := result;
  {$endif}
end;


{---------------------------------------------------------------------------}
function EncodeBase64AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base64 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase64AStr := EncodeBase2NAStr(psrc,lsrc,6);
end;


{---------------------------------------------------------------------------}
function EncodeBase32AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base32 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase32AStr := EncodeBase2NAStr(psrc,lsrc,5);
end;


{---------------------------------------------------------------------------}
function  EncodeBase32HexAStr(psrc: pointer; lsrc: word): ansistring;
  {-Base32-Hex string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase32HexAStr := EncodeBase2NAStr(psrc,lsrc,0);
end;

{---------------------------------------------------------------------------}
function EncodeBase16AStr(psrc: pointer; lsrc: word): ansistring;
  {-Base16 string of memory block of length lsrc pointed by psrc}
begin
  EncodeBase16AStr := EncodeBase2NAStr(psrc,lsrc,4);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase2NAStr(const s: ansistring; pdest: pointer; ldest, N: word; var LA: word);
  {-Decode Base 2**N string to dest, LA/ldest result/max length of dest}
var
  lsrc: word;
begin
  if s='' then  LA := 0
  else begin
    lsrc :=$FFFE;
    if length(s)<lsrc then lsrc:=length(s);
    DecodeBase2N(@s[1],pdest,lsrc,ldest,N,LA);
  end;
end;


{---------------------------------------------------------------------------}
procedure DecodeBase64AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base64 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NAStr(s,pdest,ldest,6,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NAStr(s,pdest,ldest,5,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase32HexAStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base32 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NAStr(s,pdest,ldest,0,LA);
end;


{---------------------------------------------------------------------------}
procedure DecodeBase16AStr(const s: ansistring; pdest: pointer; ldest: word; var LA: word);
  {-Decode Base16 src string to dest, LA/ldest result/max length of dest}
begin
  DecodeBase2NAStr(s,pdest,ldest,4,LA);
end;

{$endif}

end.
