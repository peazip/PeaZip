unit PPP;

(*************************************************************************

 DESCRIPTION   :  AES PPP routines (GRC's Perfect Paper Passwords)

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REFERENCES    :  http://www.grc.com/ppp/algorithm.htm

 REMARKS       :  This unit provides functions for using the PPP system
                  described on the http://www.grc.com/ppp.htm page.

                  Although there are hints to a PPP V3 Specification, there
                  seems to be no formal definition. IMO the best available
                  description is from the algorithm page:

                  "The 256-bit PPP "Sequence Key" directly provides the key
                  for the AES-standard keyed Rijndael block cipher. A 128-bit
                  sequence counter is initialized to zero for the first
                  passcode, then increments once for every subsequent
                  passcode to provide encrypted data that are translated into
                  individual passcodes for printing on PPP passcards."

                  Some programs linked by the GRC pages do not comply to this,
                  and produce e.g. five standard passcodes from one 128-bit
                  counter: Instead of '32YT 65!@' from the GRC example they
                  give '32YT YNBq LhY# sGsm cT47 65!@ V2o6 VFjK WPFn ?aWE'.


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     08.08.10  W.Ehrhardt  Initial basic version
 0.11     08.08.10  we          Improved DivBlock
 0.12     08.08.10  we          PPP_First32, PPP_Next
 0.13     08.08.10  we          Map constants
 0.14     08.08.10  we          Error checking
 0.15     09.08.10  we          PPP_FirstCard, PPP_Init4Standard, PPP_Init4Extended
 0.16     09.08.10  we          Fix for mlen=0, PPP_SetCodesPerCard
 0.17     09.08.10  we          Sort character map
 0.18     27.09.10  we          Add $N- for TP5
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2010 Wolfgang Ehrhardt

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


{$i STD.INC}

{$ifdef VER50}
  {$N-}  {Once again: brain-damaged TP5 floating-point arithmetic!}
{$endif}

interface


uses
  BTypes, AES_Type, AES_Base, AES_Encr;

type
  TPPPKey = packed array[0..31] of byte;    {PPP uses 256 bit keys}
  TPPPMap = packed array[0..255] of char8;

type
  TPPPctx = record
              actx: TAESContext;
              map : TPPPMap;  {character map}
              mlen: byte;     {map length}
              clen: byte;     {code length}
              cpc : word;     {codes per card}
            end;

const
  PPP_Err_non_increasing  = -30;  {Characters in map are not strictly increasing}
  PPP_Err_invalid_maplen  = -31;  {Invalid number of characters in map}
  PPP_Err_invalid_codelen = -32;  {Passcode length too large or zero}

const
  map64: string[64] = '!#%+23456789:=?@ABCDEFGHJKLMNPRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  map88: string[88] = '!"#$%&''()*+,-./23456789:;<=>?@ABCDEFGHJKLMNOPRSTUVWXYZ[\]^_abcdefghijkmnopqrstuvwxyz{|}~';


procedure PPP_Init(var pctx: TPPPctx; SeqKey: TPPPKey; smap: str255; codelen: word; var Err: integer);
  {-Initialize context pctx with Seqkey, character map smap, and passcode length.}
  { If smap is empty, all 256 characters #0..#255 are used.}

procedure PPP_Init4Standard(var pctx: TPPPctx; SeqKey: TPPPKey; var Err: integer);
  {-Initialize context pctx with Seqkey, standard map64, passcode length = 4}

procedure PPP_Init4Extended(var pctx: TPPPctx; SeqKey: TPPPKey; var Err: integer);
  {-Initialize context pctx with Seqkey, extended map88, passcode length = 4}

procedure PPP_SetCodesPerCard(var pctx: TPPPctx; newcpc: word);
  {-Set new "codes per card" value, 70 if 0 }

function  PPP_First32(var pctx: TPPPctx; startcode: longint): str255;
  {-Get first PPP passcode starting with passcode startcode}

function  PPP_First128(var pctx: TPPPctx; start128: TAESBlock): str255;
  {-Get first PPP passcode for LSB 128 bit number start128}

function  PPP_FirstCard(var pctx: TPPPctx; cardnum: word): str255;
  {-Get first PPP passcode of card cardnum (use card 1 if cardnum=0)}

function  PPP_Next(var pctx: TPPPctx): str255;
  {-Return the next passcode from context pctx}


implementation


{---------------------------------------------------------------------------}
procedure DivBlock(var a: TAESBlock; b: byte; var r: byte);
  {-Divide an AES LSB block by b (256 if b=0):  r = a mod b; a = a div b}
var
  i: integer;
  q,w: word;
begin
  q := b;
  if q=0 then q := 256;
  {initialize "carry"}
  w := 0;
  for i:=15 downto 0 do begin
    {loop invariant: 0 <= w < q}
    w := (w shl 8) or a[i];
    r := w div q;
    w := w mod q;
    a[i] := r;
  end;
  {set r to remainder, w is still < q!}
  r := byte(w);
end;


{---------------------------------------------------------------------------}
procedure IncBlock(var a: TAESBlock);
  {-Increment an AES LSB block}
var
  j: integer;
begin
  for j:=0 to 15 do begin
    if a[j]=$FF then a[j] := 0
    else begin
      inc(a[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure SortMap(var pctx: TPPPctx);
  {-Sort character map with CombSort routine}
var
  i,j,gap,r: integer;
  swapped: boolean;
  c: char8;
begin
  {See my CombSort routine from util archive}
  with pctx do begin
    if mlen=0 then r := 255 else r := mlen - 1;
    gap := r;
    if gap<1 then exit;
    repeat
      gap := longint(gap)*10 div 13;
      if (gap=9) or (gap=10) then gap := 11
      else if gap<1 then gap:=1;
      swapped := false;
      for i:=0 to r-gap do begin
        j := i + gap;
        if ord(map[j]) < ord(map[i]) then begin
          c := map[j];
          map[j] := map[i];
          map[i] := c;
          swapped := true;
        end
      end
    until (gap=1) and not swapped;
  end;
end;


{---------------------------------------------------------------------------}
procedure PPP_Init(var pctx: TPPPctx; SeqKey: TPPPKey; smap: str255; codelen: word; var Err: integer);
  {-Initialize context pctx with Seqkey, character map smap, and passcode length.}
  { If smap is empty, all 256 characters #0..#255 are used.}
var
  i: integer;
  sorted: boolean;
begin
  with pctx do begin
    Err := AES_Init_Encr(SeqKey, 256, actx);
    if Err<>0 then exit;
    fillchar(map, sizeof(map),0);
    if smap='' then begin
      {use all chars #0..#255}
      mlen := 0;
      for i:=0 to 255 do map[i] := char8(i);
    end
    else begin
      mlen := length(smap);
      if mlen=1 then begin
        Err := PPP_Err_invalid_maplen;
        exit;
      end;
      move(smap[1],map[0],mlen);
      sorted := true;
      i:= 1;
      while (i<mlen) and sorted do begin
        sorted := ord(map[i-1]) < ord(map[i]);
        inc(i);
      end;
      if not sorted then begin
        SortMap(pctx);
        for i:=1 to mlen-1 do begin
          if ord(map[i-1]) >= ord(map[i]) then begin
            Err := PPP_Err_non_increasing;
            exit;
          end;
        end;
      end;
    end;
    {here 2 <= mlen <= 256 or mlen=0}
    if mlen=0 then i := 16
    else i := trunc(128.0*ln(2.0)/ln(mlen));
    if (codelen > i) or (codelen=0) then begin
      Err := PPP_Err_invalid_codelen;
      exit;
    end;
    clen := codelen;
    cpc  := 70;     {default codes pre card}
  end;
end;


{---------------------------------------------------------------------------}
procedure PPP_Init4Standard(var pctx: TPPPctx; SeqKey: TPPPKey; var Err: integer);
  {-Initialize context pctx with Seqkey, standard map64, passcode length = 4}
begin
  PPP_Init(pctx, SeqKey, map64, 4, Err);
end;


{---------------------------------------------------------------------------}
procedure PPP_Init4Extended(var pctx: TPPPctx; SeqKey: TPPPKey; var Err: integer);
  {-Initialize context pctx with Seqkey, extended map88, passcode length = 4}
begin
  PPP_Init(pctx, SeqKey, map88, 4, Err);
end;


{---------------------------------------------------------------------------}
procedure PPP_SetCodesPerCard(var pctx: TPPPctx; newcpc: word);
  {-Set new "codes per card" value, 70 if 0 }
begin
  if newcpc=0 then newcpc := 70;
  pctx.cpc := newcpc;
end;


{---------------------------------------------------------------------------}
function PPP_Next(var pctx: TPPPctx): str255;
  {-Return the next passcode from context pctx}
var
  i: integer;
  idx: byte;
  s: str255;
begin
  s := '';
  with pctx do begin
    AES_Encrypt(actx, actx.iv, actx.buf);
    for i:=1 to clen do begin
      DivBlock(actx.buf, mlen, idx);
      s := s + map[idx];
    end;
    IncBlock(actx.IV);
  end;
  PPP_Next := s;
end;


{---------------------------------------------------------------------------}
function PPP_First32(var pctx: TPPPctx; startcode: longint): str255;
  {-Get first PPP passcode starting with passcode startcode}
begin
  with pctx do begin
    fillchar(actx.iv, sizeof(actx.iv), 0);
    TWA4(actx.iv)[0] := startcode;
  end;
  PPP_First32 := PPP_Next(pctx);
end;


{---------------------------------------------------------------------------}
function  PPP_FirstCard(var pctx: TPPPctx; cardnum: word): str255;
  {-Get first PPP passcode of card cardnum (use card 1 if cardnum=0)}
begin
  if cardnum=0 then cardnum := 1;
  PPP_FirstCard := PPP_First32(pctx, longint(cardnum-1)*pctx.cpc);
end;


{---------------------------------------------------------------------------}
function PPP_First128(var pctx: TPPPctx; start128: TAESBlock): str255;
  {-Get first PPP passcode for LSB 128 bit number start128}
begin
  pctx.actx.iv := start128;
  PPP_First128 := PPP_Next(pctx);
end;


end.
