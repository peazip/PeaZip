unit bCRC32;

{32 Bit CRC, bitwise without table}


interface

(*************************************************************************

 DESCRIPTION     :  32 Bit CRC, bitwise without table

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     06.07.03  W.Ehrhardt  Initial version based on CRC32
 1.10     23.07.03  W.Ehrhardt  With bCRC32File, bCRC32Full
 1.11     26.07.03  W.Ehrhardt  new self test with  bCRC32Full
 2.00     26.07.03  we          common vers., longint for word32, D4+ - warnings
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0, 386+
 2.35     08.10.03  we          BASM16 improvements, start english comments
 2.40     10.10.03  we          common version, english comments
 3.00     01.12.03  we          Common version 3.0
 3.01     17.12.05  we          Force $I- in bCRC32File
 3.02     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.03     10.02.07  we          bCRC32File: no eof, XL and filemode via $ifdef
 3.04     04.10.07  we          FPC: {$asmmode intel}
 3.05     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.06     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.07     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.08     26.12.12  we          D17 and PurePascal
 3.09     29.11.17  we          bCRC32File - fname: string
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

procedure bCRC32Init(var CRC: longint);
  {-initialize context}

procedure bCRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}

procedure bCRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}

function  bCRC32SelfTest: boolean;
  {-Self test for CRC32}

procedure bCRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}

procedure bCRC32File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}

{$ifndef BIT16}
procedure bCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC32 with Msg data}

procedure bCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
{$endif}


implementation

const
  Mask32 = $FFFFFFFF;
  CRC_PolyReflect: longint = longint($EDB88320);


{$ifndef BIT16}

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  procedure bCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC32 with Msg data}
  var
    i,b: longint;
  begin
    for i:=1 to Len do begin
      CRC := CRC xor PByte(Msg)^;
      for b := 1 to 8 do begin
        if odd(CRC) then CRC := (CRC shr 1) xor CRC_PolyReflect
        else CRC := CRC shr 1;
      end;
      inc(Ptr2Inc(Msg));
    end;
  end;

{$else}
  {---------------------------------------------------------------------------}
  procedure bCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC32 with Msg data}
  begin
    asm
         push  ebx
         mov   ecx,[Len]
         jecxz @@4
         mov   eax,[CRC]
         mov   eax,[eax]
         mov   edx,[Msg]

    @@1: xor   al, byte ptr [edx]
         inc   edx
         mov   ebx,8

    @@2: shr   eax,1
         jnc   @@3
         xor   eax, CRC_PolyReflect
    @@3: dec   ebx
         jnz   @@2

         dec   ecx
         jnz   @@1

         mov   edx,[CRC]
         mov   [edx],eax
    @@4: pop   ebx
    end;
  end;
{$endif}

{---------------------------------------------------------------------------}
procedure bCRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}
begin
  bCRC32UpdateXL(CRC, Msg, Len);
end;


{$else}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure bCRC32Update(var CRC: longint; Msg: pointer; Len: word);
   {-update CRC32 with Msg data}
var
  i,b: word;
begin
  for i:=1 to Len do begin
    CRC := CRC xor pByte(Msg)^;
    for b := 1 to 8 do begin
      if odd(CRC) then CRC := (CRC shr 1) xor CRC_PolyReflect
      else CRC := CRC shr 1;
    end;
    inc(Ptr2Inc(Msg));
  end;
end;

{$else}

{TP 6/7/Delphi1}

{---------------------------------------------------------------------------}
procedure bCRC32Update(var CRC: longint; Msg: pointer; Len: word); assembler;
   {-update CRC32 with Msg data}
asm
               les   si,[CRC]
       db $66; mov   ax,es:[si]
               les   si,[Msg]
               mov   cx,[len]
               or    cx,cx
               jz    @@4
       db $66; mov   dx, word ptr CRC_PolyReflect

  @@1:         xor   al,es:[si]           {Loop Len bytes}
               inc   si

               mov   bx,8
  @@2: db $66; shr   ax,1                 {Loop 8 bits}
               jnc   @@3
       db $66; xor   ax, dx
  @@3:         dec   bx
               jnz   @@2

               dec   cx
               jnz   @@1

               les   si,CRC
       db $66; mov   es:[si],ax
  @@4:
end;


{$endif BASM}
{$endif BIT16}

{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure bCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
begin
  bCRC32Init(CRC);
  bCRC32UpdateXL(CRC, Msg, Len);
  bCRC32Final(CRC);
end;
{$endif}

{---------------------------------------------------------------------------}
procedure bCRC32Init(var CRC: longint);
  {-CRC initialization}
begin
  CRC := longint(Mask32);
end;


{---------------------------------------------------------------------------}
procedure bCRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}
begin
  CRC := CRC xor longint(Mask32);
end;


{---------------------------------------------------------------------------}
function  bCRC32SelfTest: boolean;
  {-self test for CRC32}
const
  s: string[3] = 'abc';
var
  i: integer;
  CRC, CRCF: longint;
begin
  bCRC32Full(CRCF, @s[1], length(s));
  bCRC32Init(CRC);
  for i:=1 to length(s) do bCRC32Update(CRC, @s[i], 1);
  bCRC32Final(CRC);
  bCRC32SelfTest := (CRC=$352441C2) and (CRCF=$352441C2);
end;


{---------------------------------------------------------------------------}
procedure bCRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}
begin
  bCRC32Init(CRC);
  bCRC32Update(CRC, Msg, Len);
  bCRC32Final(CRC);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure bCRC32File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}
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
  bCRC32Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      bCRC32UpdateXL(CRC, @buf, L);
    {$else}
      bCRC32Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  bCRC32Final(CRC);
end;

end.
