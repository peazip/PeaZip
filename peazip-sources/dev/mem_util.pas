unit Mem_Util;


{Utility procedures for Hex/Base64 and memory compare}


interface


{$i STD.INC}

(*************************************************************************

 DESCRIPTION     :  Utility procedures for Hex/Base64 and memory compare

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 3548 - The Base16, Base32, and Base64 Data Encodings


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     01.01.02  W.Ehrhardt  Initial version
 0.20     30.08.03  we          with pointer valid for all compilers
 0.30     17.09.03  we          with HexLong
 0.40     27.09.03  we          FPC/go32v2
 0.50     05.10.03  we          STD.INC
 0.60     10.10.03  we          english comments
 0.70     26.12.03  we          Base64Str
 0.80     12.04.04  we          HexUpper, Delphi 7
 0.81     12.06.04  we          handle nil pointers
 0.90     05.12.04  we          Hex2Mem
 0.91     31.10.05  we          Simple Base64Enc/DecStr, D9/WDOSX, Base64Str with result
 0.92     11.12.05  we          Bugfix: Hex2Mem and $R+
 0.93     07.02.06  we          RandMem
 0.94     14.10.07  we          HexWord
 0.95     25.09.08  we          uses BTypes
 0.96     14.11.08  we          BString, char8, Ptr2Inc
 0.97     05.07.09  we          D12 fix for Hex2Mem
 0.98     27.07.10  we          CompMemXL, RandMemXL
 0.99     25.09.10  we          CompMemXL returns true if size <= 0
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2010 Wolfgang Ehrhardt

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

uses
  BTypes;

var
  HexUpper: boolean;  {Hex strings in uppercase}

function  HexByte(b: byte): BString;
  {-byte as hex string}

function  HexWord(w: word): BString;
  {-word as hex string}

function  HexLong(L: longint): BString;
  {-longint as hex string, LSB first}

function  HexStr(psrc: pointer; L: integer): BString;
  {-hex string of memory block of length L pointed by psrc}

procedure Hex2Mem({$ifdef CONST}const{$endif} s: BString; pdest: pointer; MaxL: word; var L: word);
  {-Convert hex string to mem pointed by pdest, MaxL bytes, actual byte count in L}

function  Base64Str(psrc: pointer; L: integer): BString;
  {-Base64 string of memory block of length L pointed by psrc}

function  Base64EncStr({$ifdef CONST}const{$endif} s: BString): BString;
  {-Simple Base64 encoder, uses Base64Str}

function  Base64DecStr({$ifdef CONST}const{$endif} es: BString): BString;
  {-Simple Base64 decoder, stops conversion on first invalid char}

function  CompMem(psrc, pdest: pointer; size: word): boolean;
  {-compare memory block}

procedure RandMem(pdest: pointer; size: word);
  {-fill memory block with size random bytes}

function  CompMemXL(psrc, pdest: pointer; size: longint): boolean;
  {-compare memory block}

procedure RandMemXL(pdest: pointer; size: longint);
  {-fill memory block with size random bytes}


implementation


const
  CT64: array[0..63] of char8 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';


{---------------------------------------------------------------------------}
function HexByte(b: byte): BString;
  {-byte as hex string}
const
  nib: array[0..15] of char8 = '0123456789abcdef';
begin
  if HexUpper then HexByte := upcase(nib[b div 16]) + upcase(nib[b and 15])
  else HexByte := nib[b div 16] + nib[b and 15];
end;


{---------------------------------------------------------------------------}
function HexWord(w: word): BString;
  {-word as hex string}
begin
  HexWord := HexByte(w shr 8)+HexByte(w and $FF);
end;


{---------------------------------------------------------------------------}
function HexLong(L: longint): BString;
  {-longint as hex string, LSB first}
var
  i: integer;
  s: string[8];
begin
  s := '';
  for i:=0 to 3 do begin
    s := HexByte(L and $FF) + s;
    L := L shr 8;
  end;
  HexLong := s;
end;


{---------------------------------------------------------------------------}
function HexStr(psrc: pointer; L: integer): BString;
  {-hex string of memory block of length L pointed by psrc}
var
  i: integer;
  s: BString;
begin
  s := '';
  if psrc<>nil then begin
    for i:=0 to L-1 do begin
      s := s + HexByte(pByte(psrc)^);
      inc(Ptr2Inc(psrc));
    end;
  end;
  HexStr := s;
end;


{---------------------------------------------------------------------------}
procedure Hex2Mem({$ifdef CONST}const{$endif} s: BString; pdest: pointer; MaxL: word; var L: word);
  {-Convert hex string to mem pointed by pdest, MaxL bytes, actual byte count in L}
const
  nib: array[0..15] of char8 = '0123456789ABCDEF';
  wsp: array[0..3]  of char8 = #32#9#13#10;
label
  _break; {for versions without break}
var
  i,p: integer;
  b: byte;
  c: char8;
  bdone: boolean;  {flag byte complete}
begin
  L := 0;
  if MaxL=0 then exit;
  bdone := true;
  b := 0;
  for i:=1 to length(s) do begin
    c := upcase(s[i]);
    p := pos(c,nib)-1;
    if p>=0 then begin
      {Insert new nibble into b. If range checking is on, we}
      {must prevent the following shift from overflowing b. }
      {$ifopt R+}
        b := ((b and $F) shl 4) or (p and $0F);
      {$else}
        b := (b shl 4) or (p and $0F);
      {$endif}
      bdone := not bdone;
      if bdone then begin
        {byte complete, store or break}
        if L<MaxL then begin
          pByte(pdest)^ := b;
          inc(Ptr2Inc(pdest));
          inc(L);
        end
        else goto _break;
      end;
    end
    else begin
      {ignore white space}
      if pos(c,wsp)=0 then goto _break;
    end;
  end;

_break:

  if (not bdone) and (L<MaxL) then begin
    {store remaining nibble}
    pByte(pdest)^ := (b and $0F) shl 4;
    inc(L);
  end;
end;


{---------------------------------------------------------------------------}
function Base64Str(psrc: pointer; L: integer): BString;
  {-Base64 string of memory block of length L pointed by psrc}
var
  q,r: integer;
  b0,b1,b2: byte;
  {$ifndef RESULT}
    result: BString;
  {$endif}
begin
  result := '';
  if (L>0) and (psrc<>nil) then begin
    q := L div 3;
    r := L mod 3;
    while q>0 do begin
      b0 := pByte(psrc)^; inc(Ptr2Inc(psrc));
      b1 := pByte(psrc)^; inc(Ptr2Inc(psrc));
      b2 := pByte(psrc)^; inc(Ptr2Inc(psrc));
      result := result + CT64[(b0 shr 2) and $3f]
                       + CT64[((b0 shl 4) and $30) or ((b1 shr 4) and $0f)]
                       + CT64[((b1 shl 2) and $3c) or ((b2 shr 6) and $03)]
                       + CT64[b2 and $3f];
      dec(q);
    end;
    if r=2 then begin
      b0 := pByte(psrc)^; inc(Ptr2Inc(psrc));
      b1 := pByte(psrc)^;
      result := result + CT64[(b0 shr 2) and $3f]
                       + CT64[((b0 shl 4) and $30) or ((b1 shr 4) and $0f)]
                       + CT64[(b1 shl 2) and $3c]
                       + '=';
    end
    else if r=1 then begin
      b0 := pByte(psrc)^;
      result := result + CT64[(b0 shr 2) and $3f]
                       + CT64[(b0 shl 4) and $30]
                       + '==';
    end;
  end;
  {$ifndef RESULT}
    Base64Str := result;
  {$endif}
end;


{---------------------------------------------------------------------------}
function  Base64EncStr({$ifdef CONST}const{$endif} s: BString): BString;
  {-Simple Base64 encoder, uses Base64Str}
begin
  Base64EncStr := Base64Str(@s[1], length(s));
end;


{---------------------------------------------------------------------------}
function Base64DecStr({$ifdef CONST}const{$endif} es: BString): BString;
  {-Simple Base64 decoder, stops conversion on first invalid char}
var
  i,bits,buf: word;
  {$ifndef RESULT}
    result: BString;
  {$endif}
  ic: array[char8] of byte;
  b: byte;
label
  _break;  {for TP5/5.5}
begin
  {Note: this is a stripped down version of Base2N.Decode2NPrim}
  result := '';
  {Fill input array with Base64 digit values, $FF if not valid}
  fillchar(IC, sizeof(IC), $FF);
  for i:=0 to 63 do ic[CT64[i]] := i;
  buf  := 0;
  bits := 0;
  for i:=1 to length(es) do begin
    b := IC[es[i]];
    if b>127 then goto _break;
    {Include next input into buffer. If range checking is on, }
    {we must prevent the following shift from overflowing buf.}
    {$ifopt R+}
      buf := ((buf and $03FF) shl 6) or b;
    {$else}
      buf := (buf shl 6) or b;
    {$endif}
    inc(bits,6);
    if bits>7 then begin
      {output a byte if at least 8 bits in input buf}
      dec(bits,8);
      result := result + char8((buf shr bits) and $FF);
    end;
  end;

_break:

  {$ifndef RESULT}
    Base64DecStr := result;
  {$endif}
end;


{---------------------------------------------------------------------------}
function  CompMemXL(psrc, pdest: pointer; size: longint): boolean;
  {-compare memory block}
var
  i: longint;
begin
  if size>0 then begin
    CompMemXL := false;
    if (psrc=nil) or (pdest=nil) then exit;
    for i:=1 to size do begin
      if pByte(psrc)^<>pByte(pdest)^ then exit;
      inc(Ptr2Inc(psrc));
      inc(Ptr2Inc(pdest));
    end;
  end;
  CompMemXL := true;
end;


{---------------------------------------------------------------------------}
procedure RandMemXL(pdest: pointer; size: longint);
  {-fill memory block with size random bytes}
var
  i: longint;
begin
  if pdest<>nil then begin
    for i:=1 to size do begin
      pByte(pdest)^ := random(256);
      inc(Ptr2Inc(pdest));
    end;
  end;
end;


{---------------------------------------------------------------------------}
function CompMem(psrc, pdest: pointer; size: word): boolean;
  {-compare memory block}
begin
  CompMem := CompMemXL(psrc, pdest, size);
end;


{---------------------------------------------------------------------------}
procedure RandMem(pdest: pointer; size: word);
  {-fill memory block with size random bytes}
begin
  RandMemXL(pdest, size);
end;


begin
  HexUpper := false;
end.
