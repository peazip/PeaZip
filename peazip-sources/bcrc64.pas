unit bCRC64;

{64 Bit CRC, bitwise without table}


interface

(*************************************************************************

 DESCRIPTION     :  64 Bit CRC, bitwise without table

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://www.ecma-international.org/publications/files/ecma-st/
                    Ecma-182.pdf: DLT1 spec  or  Ecma-231.pdf: DLT4 spec


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     07.07.03  W.Ehrhardt  Initial version based on bCRC32 layout
 1.10     30.08.03  we          Common vers., XL versions for Win32
 2.10     31.08.03  we          Common vers., new polynomial
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 3.00     01.12.03  we          Common version 3.0
 3.01     17.12.05  we          Force $I- in bCRC64File
 3.02     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.03     10.02.07  we          bCRC64File: no eof, XL and filemode via $ifdef
 3.04     04.10.07  we          FPC: {$asmmode intel}
 3.05     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.06     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.07     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.08     26.12.12  we          D17 and PurePascal
 3.09     28.03.17  we          No '$asmmode intel' for CPUARM
 3.10     29.11.17  we          bCRC64File - fname: string

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

{$i STD.INC}

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

uses
  BTypes;

type
  TCRC64b = packed record
              lo32, hi32: longint;
            end;

procedure bCRC64Init(var CRC: TCRC64b);
  {-CRC64 initialization}

procedure bCRC64Update(var CRC: TCRC64b; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}

procedure bCRC64Final(var CRC: TCRC64b);
  {-CRC64: finalize calculation}

function  bCRC64SelfTest: boolean;
  {-Self test for CRC64}

procedure bCRC64Full(var CRC: TCRC64b; Msg: pointer; Len: word);
  {-CRC64 of Msg with init/update/final}

procedure bCRC64File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: TCRC64b; var buf; bsize: word; var Err: word);
  {-CRC64 of file, buf: buffer with at least bsize bytes}

{$ifndef BIT16}
procedure bCRC64UpdateXL(var CRC: TCRC64b; Msg: pointer; Len: longint);
  {-update CRC64 with Msg data}

procedure bCRC64FullXL(var CRC: TCRC64b; Msg: pointer; Len: longint);
  {-CRC64 of Msg with init/update/final}
{$endif}



implementation


{$ifdef FPC}
  {$ifndef CPUARM}
    {$asmmode intel}
  {$endif}
{$endif}


(*************************************************************************
T_CTab64 - CRC64 table calculation     (c) 2002-2004 W.Ehrhardt

Calculate CRC64 tables for polynomial:

x^64 + x^62 + x^57 + x^55 + x^54 + x^53 + x^52 + x^47 + x^46 + x^45 +
x^40 + x^39 + x^38 + x^37 + x^35 + x^33 + x^32 + x^31 + x^29 + x^27 +
x^24 + x^23 + x^22 + x^21 + x^19 + x^17 + x^13 + x^12 + x^10 + x^9  +
x^7  + x^4  + x^1  + 1

const
  PolyLo : longint = longint($A9EA3693);
  PolyHi : longint = longint($42F0E1EB);
*************************************************************************)



const
  Mask64: TCRC64b = (lo32:-1; hi32:-1);
  Poly  : TCRC64b = (lo32:longint($A9EA3693); hi32:longint($42F0E1EB));

{
  for i:=1 to length(test) do begin
    c64 := c64 xor test[i] shl 56;
    for b := 1 to 8 do begin
      if c64<0 then c64 := (c64 shl 1) xor Poly else c64 := c64 shl 1;
    end;
  end;
}



{$ifndef BIT16}

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  procedure bCRC64UpdateXL(var CRC: TCRC64b; Msg: pointer; Len: longint);
    {-update CRC64 with Msg data}
  var
    i,b: longint;
    c64: int64 absolute CRC;
  var
    Poly64: int64 absolute Poly;  {FPC1 does not like int64 typed consts}
  begin
    for i:=1 to Len do begin
      c64 := c64 xor (int64(PByte(Msg)^) shl 56);
      for b := 1 to 8 do begin
        if c64<0 then c64 := (c64 shl 1) xor Poly64 else c64 := c64 shl 1;
      end;
      inc(Ptr2Inc(Msg));
    end;
  end;
{$else}
  {---------------------------------------------------------------------------}
  procedure bCRC64UpdateXL(var CRC: TCRC64b; Msg: pointer; Len: longint);
    {-update CRC64 with Msg data}
  begin
    asm
         push  ebx
         push  esi
         mov   ecx,[Len]
         jecxz @@4

         mov   eax,[CRC]
         mov   edx,[eax+4]
         mov   eax,[eax]
         mov   esi,[Msg]

    @@1: mov   bl,byte ptr [esi]
         inc   esi
         shl   ebx,24
         xor   edx,ebx
         mov   ebx,8

    @@2: shl   eax,1
         rcl   edx,1
         jnc   @@3
         xor   eax,Poly.lo32
         xor   edx,Poly.hi32
    @@3: dec   ebx
         jnz   @@2

         dec   ecx
         jnz   @@1

         mov   ebx,[CRC]
         mov   [ebx],eax
         mov   [ebx+4],edx
    @@4: pop   esi
         pop   ebx
    end;
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure bCRC64Update(var CRC: TCRC64b; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}
begin
  bCRC64UpdateXL(CRC, Msg, Len);
end;


{$else}


(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure bCRC64Update(var CRC: TCRC64b; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}
var
  i,b: word;
  xp: boolean;
begin
  for i:=1 to Len do begin
    CRC.hi32 := CRC.hi32 xor (longint(pByte(Msg)^) shl 24);
    for b := 1 to 8 do begin
      xp := CRC.hi32<0;
      CRC.hi32 := CRC.hi32 shl 1;
      if CRC.lo32<0 then inc(CRC.hi32);
      CRC.lo32 := CRC.lo32 shl 1;
      if xp then begin
        CRC.lo32 := CRC.lo32 xor Poly.lo32;
        CRC.hi32 := CRC.hi32 xor Poly.hi32;
      end;
    end;
    inc(Ptr2Inc(Msg));
  end;
end;

{$else}

{TP 6/7/Delphi1}

{---------------------------------------------------------------------------}
procedure bCRC64Update(var CRC: TCRC64b; Msg: pointer; Len: word); assembler;
  {-update CRC64 with Msg data}

asm
               les     si,[CRC]
       db $66; mov     ax,es:[si]
       db $66; mov     dx,es:[si+4]
               les     si,[Msg]
               mov     cx,[len]
               or      cx,cx
               jz      @@4

  @@1:         mov     bl,es:[si]           {loop for Len bytes}
               inc     si
       db $66; shl     bx,24
       db $66; xor     dx,bx

               mov     bx,8

  @@2: db $66; shl     ax,1                 {loop for 8 bit}
       db $66; rcl     dx,1
               jnc     @@3
       db $66; xor     ax,word ptr Poly.lo32
       db $66; xor     dx,word ptr Poly.hi32
  @@3:         dec     bx
               jnz     @@2

               dec     cx
               jnz     @@1

               les     si,CRC               {store result}
       db $66; mov     es:[si],ax
       db $66; mov     es:[si+4],dx

  @@4:
end;


{$endif BASM16}
{$endif BIT16}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure bCRC64FullXL(var CRC: TCRC64b; Msg: pointer; Len: longint);
  {-CRC64 of Msg with init/update/final}
begin
  bCRC64Init(CRC);
  bCRC64UpdateXL(CRC, Msg, Len);
  bCRC64Final(CRC);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure bCRC64Init(var CRC: TCRC64b);
  {-CRC64 initialization}
begin
  CRC := Mask64;
end;


{---------------------------------------------------------------------------}
procedure bCRC64Final(var CRC: TCRC64b);
  {-CRC64: finalize calculation}
begin
  CRC.lo32 := CRC.lo32 xor Mask64.lo32;
  CRC.hi32 := CRC.hi32 xor Mask64.hi32;
end;


{---------------------------------------------------------------------------}
function  bCRC64SelfTest: boolean;
  {-self test for CRC64}
const
  s: string[20] = '123456789';
  CHKhi: longint = longint($62EC59E3);
  CHKlo: longint = longint($F1A4F00A);
var
  CRC: TCRC64b;
begin
  bCRC64Init(CRC);
  bCRC64Update(CRC, @s[1], length(s));
  bCRC64Final(CRC);
  bCRC64SelfTest := (CRC.lo32=CHKlo) and (CRC.hi32=CHKhi);
end;


{---------------------------------------------------------------------------}
procedure bCRC64Full(var CRC: TCRC64b; Msg: pointer; Len: word);
  {-CRC64 of Msg with init/update/final}
begin
  bCRC64Init(CRC);
  bCRC64Update(CRC, Msg, Len);
  bCRC64Final(CRC);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure bCRC64File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: TCRC64b; var buf; bsize: word; var Err: word);
  {-CRC64 of file, buf: buffer with at least bsize bytes}
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
  bCRC64Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      bCRC64UpdateXL(CRC, @buf, L);
    {$else}
      bCRC64Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  bCRC64Final(CRC);
end;

end.
