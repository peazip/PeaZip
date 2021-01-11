unit crcmodel;

{"Rocksoft^tm Model CRC Algorithm" for up to 32 bits}


interface

(*************************************************************************

 DESCRIPTION     :  "Rocksoft^tm Model CRC Algorithm" for up to 32 bits

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] Ross Williams' public domain C sources crcmodel.c, crcmodel.h
                        in "A Painless Guide to CRC Error Detection Algorithms"
                        http://www.ross.net/crc/download/crc_v3.txt
                    [2] Mark Adler's crc32_combine in crc32-c from zlib 1.2.3
                        http://www.gzip.org/zlib/

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     14.04.06  W.Ehrhardt  Initial version for table calculation, CRC32_Zip
 0.11     06.12.06  we          CRC standard functions for tableless versions
 0.12     06.12.06  we          For Pascals without const
 0.13     06.12.06  we          Code for cm_File
 0.14     06.12.06  we          Update for table versions
 0.15     21.01.07  we          cm_next updated to use ptab<>nil
                                Interfaced cm_reflect, cm_next
 0.16     10.02.07  we          cm_File: no eof, XL and filemode via $ifdef
 0.17     05.07.07  we          Force $I- in cm_File
 0.18     11.07.08  we          Moved to crcm_cat: CRC32_Zip, CRC16_CCITT, CRC24_PGP
 0.19     11.07.08  we          cm_next for width<8, ptab=nil
 0.20     11.07.08  we          name: string[19], no tables for width<8
 0.21     12.07.08  we          Reflect for table version with refin<>refout, e.g. CRC-16/BT-CHIP
 0.22     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 0.23     14.11.08  we          char8
 0.24     25.04.09  we          cm_Init: cm_reflect(init,width) for table version with refin
 0.25     11.06.09  we          type PCRCParam = ^TCRCParam
 0.26     19.07.09  we          D12 fix: assign with typecast string(fname)
 0.27     06.09.09  we          cm_combine
 0.28     10.03.12  we          cm_File: {$ifndef BIT16} instead of {$ifdef WIN32}
 0.29     16.08.15  we          Removed $ifdef DLL / stdcall
 0.30     29.11.17  we          cm_File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2006-2017 Wolfgang Ehrhardt

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

uses
  BTypes;

type
  PCRCParam  = ^TCRCParam;
  TCRCParam  = packed record
                 poly  : longint;    {CRC polynomial, top bit omitted}
                 init  : longint;    {Initial value of crc register}
                 xorout: longint;    {final xormask for crc register}
                 check : longint;    {CRC value for '123456789'}
                 width : word;       {width of algorithm, deg(poly)-1}
                 refin : boolean;    {reflect input bytes before processing}
                 refout: boolean;    {reflect reg before xor with xorout}
                 name  : string[19]; {name of the CRC algorithm}
               end;
type
  PCRC32Tab  = ^TCRC32Tab;             {Pointer to CRC table}
  TCRC32Tab  = array[byte] of longint; {CRC table type}

type
  TCRC_ctx   = packed record
                 reg   : longint;    {CRC state register}
                 poly  : longint;    {CRC polynomial, top bit omitted}
                 init  : longint;    {Initial value of crc register}
                 xorout: longint;    {final xormask for crc register}
                 check : longint;    {CRC value for '123456789'}
                 wmask : longint;    {mask with lowest width bits set}
                 ptab  : PCRC32Tab;  {pointer to table, may be nil}
                 width : word;       {width of algorithm, deg(poly)-1}
                 shift : word;       {shift value for table processing}
                 refin : boolean;    {reflect input bytes before processing}
                 refout: boolean;    {reflect reg before xor with xorout}
                 name  : string[19]; {name of the CRC algorithm}
               end;


procedure cm_CalcTab({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam; var Tab: TCRC32Tab);
  {-Calculate CRC table from CRCPara, does nothing if CRCPara.width<8}

procedure cm_Create({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam; ptab: PCRC32Tab; var ctx: TCRC_ctx);
  {-Create crc context from CRCPara, ptab may be nil}

procedure cm_Init(var ctx: TCRC_ctx);
  {-initialize context}

procedure cm_Update(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-update ctx with Msg data}

procedure cm_Final(var ctx: TCRC_ctx; var CRC: longint);
  {-finalize calculation and return CRC}

function  cm_SelfTest({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam) : boolean;
  {-Self test for CRCPara (no table version)}

procedure cm_Full(var ctx: TCRC_ctx; var CRC: longint; Msg: pointer; Len: word);
  {-process Msg with init/update/final using ctx}

procedure cm_File({$ifdef CONST} const {$endif} fname: string;
                  var ctx: TCRC_ctx; var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC of file, buf: buffer with at least bsize bytes}

function  cm_combine({$ifdef CONST}const{$else}var{$endif} para: TCRCParam; crc1, crc2: longint; len2: longint): longint;
  {-combine two CRCs calculated with para, i.e. if crc1 = CRC(m1) and}
  { crc2 = CRC(m2) then cm_combine = CRC(m1||m2); len2 = length(m2).}

procedure cm_next(var ctx: TCRC_ctx; b: byte);
  {-update ctx with data byte b}

function  cm_reflect(v: longint; b: integer): longint;
  {-returns the reflected lowest b bits of v}


implementation


{---------------------------------------------------------------------------}
function BitMaskW(w: word): longint;
  {-Bit mask for w bits}
begin
  if w>31 then BitMaskW := longint($FFFFFFFF)
  else BitMaskW := (longint(1) shl w)-1;
end;


{---------------------------------------------------------------------------}
function cm_reflect(v: longint; b: integer): longint;
  {-returns the reflected lowest b bits of v}
var
  r: longint;
begin
  r := 0;
  b := b and $3F;
  while b>0 do begin
    inc(r,r);
    if odd(v) then inc(r);
    v := v shr 1;
    dec(b);
  end;
  cm_reflect := r;
end;


{---------------------------------------------------------------------------}
function cm_tab({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam; idx: integer): longint;
  {-calculate tab entry for idx}
var
  i: integer;
  r, topbit, inbyte: longint;
begin
  topbit := longint(1) shl (CRCPara.width - 1);
  inbyte := idx;
  if CRCPara.refin then inbyte := cm_reflect(inbyte,8);
  r := inbyte shl (CRCPara.width-8);
  for i:=0 to 7 do begin
    if r and topbit = 0 then r := r shl 1
    else r := (r shl 1) xor CRCPara.poly
  end;
  if CRCPara.refin then r := cm_reflect(r, CRCPara.width);
  cm_tab := r and BitMaskW(CRCPara.width);
end;


{---------------------------------------------------------------------------}
procedure cm_CalcTab({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam; var Tab: TCRC32Tab);
  {-Calculate CRC table from CRCPara, does nothing if CRCPara.width<8}
var
  i: integer;
begin
  if CRCPara.width>7 then begin
    for i:=0 to 255 do Tab[i] := cm_tab(CRCPara, i);
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Create({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam; ptab: PCRC32Tab; var ctx: TCRC_ctx);
  {-Create crc context from CRCPara, ptab may be nil}
begin
  ctx.poly  := CRCPara.poly;
  ctx.init  := CRCPara.init;
  ctx.xorout:= CRCPara.xorout;
  ctx.check := CRCPara.check;
  ctx.refin := CRCPara.refin;
  ctx.refout:= CRCPara.refout;
  ctx.name  := CRCPara.name;
  ctx.width := CRCPara.width;
  ctx.wmask := BitMaskW(CRCPara.width);
  ctx.reg   := ctx.init;
  if ctx.width>7 then begin
    ctx.ptab  := ptab;
    ctx.shift := ctx.width-8;
  end
  else begin
    ctx.ptab  := nil;
    ctx.shift := 0;
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_next(var ctx: TCRC_ctx; b: byte);
  {-update ctx with data byte b}
var
  i,j: integer;
  topbit, uch: longint;
begin
  with ctx do begin
    if ptab=nil then begin
      topbit := longint(1) shl (ctx.width - 1);
      uch := b;
      if refin then uch := cm_reflect(uch,8);
      if width>7 then begin
        reg := reg xor (uch shl shift);
        for i:=0 to 7 do begin
          if reg and topbit = 0 then reg := (reg shl 1) and wmask
          else reg := ((reg shl 1) xor poly) and wmask;
        end;
      end
      else begin
        i := $80;
        while i>0 do begin
          if uch and i = 0 then j := reg and topbit
          else j := (reg and topbit) xor topbit;
          if j=0 then reg := (reg shl 1) and wmask
          else reg := ((reg shl 1) xor poly) and wmask;
          i := i shr 1;
        end;
      end;
    end
    else begin
      if refin then reg := ptab^[byte(reg) xor b] xor (reg shr 8)
      else reg := ptab^[byte(reg shr shift) xor b] xor (reg shl 8);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Init(var ctx: TCRC_ctx);
  {-initialize context}
begin
  with ctx do begin
    if (ptab<>nil) and refin then reg := cm_reflect(init,width)
    else reg := init;
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Update_normal(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-update ctx with Msg data, normal/table}
var
  i: word;
begin
  with ctx do begin
    for i:=1 to Len do begin
      reg := ptab^[byte(reg shr shift) xor pByte(Msg)^] xor (reg shl 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Update_reflect(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-update ctx with Msg data, reflected/table}
var
  i: word;
begin
  with ctx do begin
    for i:=1 to Len do begin
      reg := ptab^[byte(reg) xor pByte(Msg)^] xor (reg shr 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Update_byte(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-bytewise update ctx with Msg data}
var
  i: word;
begin
  for i:=1 to Len do begin
    cm_next(ctx, pByte(Msg)^);
    inc(Ptr2Inc(Msg));
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Update(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-update ctx with Msg data}
begin
  with ctx do begin
    if ptab=nil then cm_Update_byte(ctx,Msg,Len)
    else if refin then cm_Update_reflect(ctx,Msg,Len)
    else cm_Update_normal(ctx,Msg,Len);
  end;
end;


{---------------------------------------------------------------------------}
procedure cm_Final(var ctx: TCRC_ctx; var CRC: longint);
  {-finalize calculation and return CRC}
begin
  with ctx do begin
    {Reflect for table version with refin<>refout, e.g. CRC-16/BT-CHIP}
    if (refout and (ptab=nil)) or ((ptab<>nil) and (refin<>refout)) then begin
      CRC := xorout xor cm_reflect(reg,width)
    end
    else CRC := xorout xor reg;
    CRC := CRC and wmask;
  end;
end;


{---------------------------------------------------------------------------}
function  cm_SelfTest({$ifdef CONST}const {$else} var {$endif} CRCPara: TCRCParam) : boolean;
  {-Self test for CRCPara (no table version)}
var
  ctx: TCRC_ctx;
  CRC: longint;
const
  cs: array[0..8] of char8 = '123456789';
begin
  cm_Create(CRCPara,nil,ctx);
  cm_Full(ctx,CRC,@cs,9);
  cm_SelfTest := CRC = ctx.check;
end;


{---------------------------------------------------------------------------}
procedure cm_Full(var ctx: TCRC_ctx; var CRC: longint; Msg: pointer; Len: word);
  {-process Msg with init/update/final using ctx}
begin
  cm_init(ctx);
  cm_update(ctx, Msg, Len);
  cm_final(ctx, CRC);
end;


{---------------------------------------------------------------------------}
function cm_combine({$ifdef CONST}const{$else}var{$endif} para: TCRCParam; crc1, crc2: longint; len2: longint): longint;
  {-combine two CRCs calculated with para, i.e. if crc1 = CRC(m1) and}
  { crc2 = CRC(m2) then cm_combine = CRC(m1||m2); len2 = length(m2).}
type
  TMat32 = array[0..31] of longint;

  {---------------------------------------------------------------------------}
  function gf2_matrix_times({$ifdef CONST}const{$else}var{$endif} mat: TMat32; vec: longint): longint;
    {-return mat*vec using GF(2) arithmetic}
  var
    i: integer;
    sum: longint;
  begin
    i := 0;
    sum := 0;
    while vec<>0 do begin
      if odd(vec) then sum := sum xor mat[i];
      vec := vec shr 1;
      inc(i);
    end;
    gf2_matrix_times := sum;
  end;

  {---------------------------------------------------------------------------}
  procedure gf2_matrix_square(w: word; var square: TMat32; {$ifdef CONST}const{$else}var{$endif} mat: TMat32);
    {-Calculate the square of a w x w matrix mat using GF(2) arithmetic}
  var
    i: integer;
  begin
    for i:=0 to w-1 do square[i] := gf2_matrix_times(mat, mat[i]);
  end;

var
  w,i: integer;
  row: longint;
  M1,M2: TMat32;
begin

  {This function is essentially based on Mark Adler's crc32_combine from}
  {the crc32.c zlib source. I generalized it to the full Rocksoft range }
  {up to 32 bit, i.e. added support for non 32 bit CRCs, reflection code}
  {different and/or non-symmetric values of xorout/init, etc.}

  {The main loop has an iteration count of log2(len2). The key ideas are}
  {the linearity of CRC and the CRC calculation of len2 zero bytes using}
  {a fast matrix powering algorithm. See Mark's comments posted to the}
  {comp.compression and sci.crypt news groups from 2008-09-17, Message-ID:}
  {<95e1583e-3def-4931-92d3-c33b00373ff6@p10g2000prf.googlegroups.com>}

  {handle degenerate cases}
  if len2<1 then begin
    {nothing to do}
    cm_combine := crc1;
    exit;
  end;

  w := para.width;
  if (w<1) or (w>32) then begin
    {This should be treated as an error, but normally it will not happen}
    {because in almost all cases pre-defined parameter records are used.}
    cm_combine := 0;
    exit;
  end;

  {Reflect start crc value if necessary}
  if para.refin<>para.refout then crc1 := cm_reflect(crc1,w);

  {Put operator for one zero bit in M1, i.e. for i=0..w-1 initialize the}
  {CRC state ctx.reg = 1 shl i, feed in a 0 bit and set M1[i] = ctx.reg.}
  {In praxis we don't not have to perform these calculation since we know}
  {in advance the outcomes (depending on the refin parameter).}

  if para.refin then begin
    {adjust start crc if xorout and init are different and/or not symmetric}
    crc1  := crc1 xor cm_reflect(para.xorout,w) xor cm_reflect(para.init,w);
    M1[0] := cm_reflect(para.poly,w);
    row := 1;
    for i:=1 to pred(w) do begin
      M1[i] := row;
      row := row shl 1;
    end;
  end
  else begin
    {adjust start crc if xorout and init are different}
    crc1 := crc1 xor para.xorout xor para.init;
    row  := 2;
    for i:=0 to w-2 do begin
      M1[i] := row;
      row := row shl 1;
    end;
    M1[w-1] := para.poly;
  end;

  {Put operator for two zero bits in M2 = M1.M1}
  gf2_matrix_square(w, M2, M1);

  {Put operator for four zero bits in M1}
  gf2_matrix_square(w, M1, M2);

  {Apply len2 zeros to crc1 (first square will put the}
  {operator for one zero byte, eight zero bits, in M2)}
  repeat
    {apply zeros operator for this bit of len2}
    gf2_matrix_square(w, M2, M1);
    if odd(len2)  then crc1 := gf2_matrix_times(M2, crc1);
    len2 := len2 shr 1;
    {if no more bits set, then done}
    if len2<>0 then begin
      {another iteration of the loop with M1 and M2 swapped}
      gf2_matrix_square(w, M1, M2);
      if odd(len2) then crc1 := gf2_matrix_times(M1, crc1);
      len2 := len2 shr 1;
    end;
  until len2=0;

  {Reflect final crc if necessary}
  if para.refin<>para.refout then crc1 := cm_reflect(crc1,w);

  {Return combined crc}
  cm_combine := crc1 xor crc2;

end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure cm_File({$ifdef CONST} const {$endif} fname: string;
                  var ctx: TCRC_ctx; var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC of file, buf: buffer with at least bsize bytes}
var
  {$ifdef VirtualPascal}
    fms: word;
  {$else}
    fms: byte;
  {$endif}
  {$ifndef BIT16}
    L: longint;
  {$else}
    L: word;
  {$endif}
  f: file;
begin
  fms := FileMode;
  {$ifdef VirtualPascal}
    FileMode := $40; {open_access_ReadOnly or open_share_DenyNone;}
  {$else}
    FileMode := 0;
  {$endif}
  system.assign(f,{$ifdef D12Plus} string {$endif} (fname));
  system.reset(f,1);
  Err := IOResult;
  FileMode := fms;
  if Err<>0 then exit;
  cm_init(ctx);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    cm_update(ctx, @buf, L);
  end;
  system.close(f);
  if IOResult=0 then;
  cm_final(ctx, CRC);
end;

end.
