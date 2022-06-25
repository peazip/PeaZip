unit Adler32;

{ZLib - Adler32 checksum function}


interface

(*************************************************************************

 DESCRIPTION     :  ZLib - Adler32 checksum function

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 1950 (http://tools.ietf.org/html/rfc1950)


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     30.08.03  W.Ehrhardt  Initial version based on MD5 layout
 2.10     30.08.03  we          Common vers., XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 3.00     01.12.03  we          Common version 3.0
 3.01     22.05.05  we          Adler32UpdateXL (i,n: integer)
 3.02     17.12.05  we          Force $I- in Adler32File
 3.03     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.04     10.02.07  we          Adler32File: no eof, XL and filemode via $ifdef
 3.05     04.07.07  we          BASM16: speed-up factor 15
 3.06     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.07     25.04.09  we          updated RFC URL(s)
 3.08     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.09     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.10     26.12.12  we          D17 and PurePascal
 3.11     16.08.15  we          Removed $ifdef DLL / stdcall
 3.12     29.11.17  we          Adler32File - fname: string

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

uses
  BTypes;

procedure Adler32Init(var adler: longint);
  {-Adler32 initialization}

procedure Adler32Update(var adler: longint; Msg: pointer; Len: word);
  {-update Adler32 with Msg data}

procedure Adler32Final(var adler: longint);
  {-finalize Adler32 calculation, clear context}

function  Adler32SelfTest: boolean;
  {-Self test for Adler32}

procedure Adler32Full(var adler: longint; Msg: pointer; Len: word);
  {-Adler32 of Msg with Init/Update/Final}

procedure Adler32File({$ifdef CONST} const {$endif} fname: string;
                      var adler: longint; var buf; bsize: word; var Err: word);
  {-Adler32 of file, buf: buffer with at least bsize bytes}

{$ifndef BIT16}
procedure Adler32UpdateXL(var adler: longint; Msg: pointer; Len: longint);
  {-update Adler32 with Msg data}

procedure Adler32FullXL(var adler: longint; Msg: pointer; Len: longint);
  {-Adler32 of Msg with Init/Update/Final}
{$endif}


implementation


{---------------------------------------------------------------------------}
procedure Adler32Init(var adler: longint);
  {-Adler32 initialization}
begin
  adler := 1;
end;


{$ifdef BASM16}

{---------------------------------------------------------------------------}
procedure Adler32UpdateXL(var adler: longint; Msg: pointer; Len: longint);
  {-update Adler32 with Msg data}
const
  BASE = 65521; {max. prime < 65536 }
  NMAX =  5552; {max. n with 255n(n+1)/2 + (n+1)(BASE-1) < 2^32}
type
  LH    = packed record
            L,H: word;
          end;
var
  s1,s2: longint;
  n: integer;
begin
  s1 := LH(adler).L;
  s2 := LH(adler).H;
  while Len > 0 do begin
    if Len<NMAX then n := Len else n := NMAX;
    {BASM increases speed from about 52 cyc/byte to about 3.7 cyc/byte}
    asm
                    mov  cx,[n]
            db $66; mov  ax,word ptr [s1]
            db $66; mov  di,word ptr [s2]
                    les  si,[msg]
      @@1:  db $66, $26, $0f, $b6, $1c      {movzx ebx,es:[si]}
                    inc  si
            db $66; add  ax,bx              {inc(s1, pByte(Msg)^)}
            db $66; add  di,ax              {inc(s2, s1}
                    dec  cx
                    jnz  @@1
            db $66; sub  cx,cx
                    mov  cx,BASE
            db $66; sub  dx,dx
            db $66; div  cx
            db $66; mov  word ptr [s1],dx   {s1 := s1 mod BASE}
            db $66; sub  dx,dx
            db $66; mov  ax,di
            db $66; div  cx
            db $66; mov  word ptr [s2],dx   {s2 := s2 mod BASE}
                    mov  word ptr [msg],si  {save offset for next chunk}
    end;
    dec(len, n);
  end;
  LH(adler).L := word(s1);
  LH(adler).H := word(s2);
end;

{$else}

{---------------------------------------------------------------------------}
procedure Adler32UpdateXL(var adler: longint; Msg: pointer; Len: longint);
  {-update Adler32 with Msg data}
const
  BASE = 65521; {max. prime < 65536 }
  NMAX =  3854; {max. n with 255n(n+1)/2 + (n+1)(BASE-1) < 2^31}
type
  LH    = packed record
            L,H: word;
          end;
var
  s1,s2: longint;
  i,n: integer;
begin
  s1 := LH(adler).L;
  s2 := LH(adler).H;
  while Len > 0 do begin
    if Len<NMAX then n := Len else n := NMAX;
    for i:=1 to n do begin
      inc(s1, pByte(Msg)^);
      inc(Ptr2Inc(Msg));
      inc(s2, s1);
    end;
    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
    dec(len, n);
  end;
  LH(adler).L := word(s1);
  LH(adler).H := word(s2);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure Adler32Update(var adler: longint; Msg: pointer; Len: word);
  {-update Adler32 with Msg data}
begin
  Adler32UpdateXL(adler, Msg, Len);
end;



{---------------------------------------------------------------------------}
procedure Adler32Final(var adler: longint);
  {-finalize Adler32 calculation, clear context}
begin
  {Dummy}
end;


{---------------------------------------------------------------------------}
function Adler32SelfTest: boolean;
  {-Self test for Adler32}
const
  vec: array[1..3] of byte = ($11,$22,$33);
var
  i: integer;
  A1, AF: longint;
begin
  Adler32Full(AF, @vec, sizeof(vec));
  Adler32Init(A1);
  for i:=1 to sizeof(vec) do Adler32Update(A1, @vec[i], 1);
  Adler32Final(A1);
  Adler32SelfTest := (A1=$00AD0067) and (AF=$00AD0067);
end;


{---------------------------------------------------------------------------}
procedure Adler32FullXL(var adler: longint; Msg: pointer; Len: longint);
  {-Adler32 of Msg with Init/Update/Final}
begin
  Adler32Init(adler);
  Adler32UpdateXL(adler, Msg, Len);
  Adler32Final(adler);
end;


{---------------------------------------------------------------------------}
procedure Adler32Full(var adler: longint; Msg: pointer; Len: word);
  {-Adler32 of Msg with Init/Update/Final}
begin
  Adler32FullXL(adler, Msg, Len);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure Adler32File({$ifdef CONST} const {$endif} fname: string;
                      var adler: longint; var buf; bsize: word; var Err: word);
  {-Adler32 of file, buf: buffer with at least bsize bytes}
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
var
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
  Adler32Init(adler);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    Adler32UpdateXL(adler, @buf, L);
  end;
  system.close(f);
  if IOResult=0 then;
  Adler32Final(adler);
end;

end.
