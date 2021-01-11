unit CRC16;

{16 Bit CRC routines for CCITT polynomial $1021}


interface

(*************************************************************************

 DESCRIPTION     :  16 Bit CRC

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  Numerical recipes, 2nd ed.


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.03.02  W.Ehrhardt  Initial version based on CRC32 layout
 0.20     23.07.03  W.Ehrhardt  With CRC16File, CRC16Full
 0.21     26.07.03  W.Ehrhardt  new self test with  CRC16Full
 2.00     26.07.03  we          common vers.
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 2.45     11.10.03  we          BASM speedups
 2.46     26.10.03  we          fix VP Basm quirk, use [ebx*scale]
 3.00     01.12.03  we          Common version 3.0
 3.01     24.12.03  we          BASM16: shl bx,1 -->  add bx,bx
 3.02     04.01.05  we          BASM16 uses: xor ax,[edx+2*ebx]
 3.03     17.12.05  we          Force $I- in CRC16File
 3.04     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.05     10.02.07  we          CRC16File: no eof, XL and filemode via $ifdef
 3.06     04.10.07  we          FPC: {$asmmode intel}
 3.07     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.08     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.09     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.10     26.12.12  we          D17 and PurePascal
 3.11     16.08.15  we          Removed $ifdef DLL / stdcall
 3.12     29.11.17  we          CRC16File - fname: string

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

procedure CRC16Init(var CRC: word);
  {-initialize context}

procedure CRC16Update(var CRC: word; Msg: pointer; Len: word);
  {-update CRC16 with Msg data}

procedure CRC16Final(var CRC: word);
  {-CRC16: finalize calculation (dummy)}

function  CRC16SelfTest: boolean;
  {-Self test for CRC16}

procedure CRC16Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC16 of Msg with init/update/final}

procedure CRC16File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC16 of file, buf: buffer with at least bsize bytes}


{$ifndef BIT16}
procedure CRC16UpdateXL(var CRC: word; Msg: pointer; Len: longint);
  {-update CRC16 with Msg data}

procedure CRC16FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC16 of Msg with init/update/final}
{$endif}


implementation



{CRC table for CCITT polynomial $1021}
const
  ctab: packed array [0..255] of word =
          ( $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
            $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
            $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
            $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
            $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
            $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
            $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
            $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
            $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
            $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
            $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
            $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
            $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
            $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
            $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
            $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
            $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
            $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
            $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
            $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
            $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
            $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
            $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
            $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
            $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
            $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
            $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
            $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
            $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
            $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
            $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
            $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0  );


{$ifndef BIT16}

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  procedure CRC16UpdateXL(var CRC: word; Msg: pointer; Len: longint);
     {-update CRC16 with Msg data}
  var
    i: longint;
  begin
    for i:=1 to Len do begin
      CRC := CTab[(CRC shr 8) xor PByte(Msg)^] xor ((CRC and $FF) shl 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
{$else}
  {---------------------------------------------------------------------------}
  procedure CRC16UpdateXL(var CRC: word; Msg: pointer; Len: longint);
    {-update CRC16 with Msg data}
  begin
    asm
         push    ebx
         mov     ecx,[Len]
         jecxz   @@2                       {no update if Len=0}
         mov     ebx,[CRC]
         mov     ax, [ebx]
         mov     edx,[Msg]
    @@1: xor     ah,[edx]
         movzx   ebx,ah
         shl     ax,8
         inc     edx
         xor     ax,word ptr CTab[ebx*2]
         dec     ecx
         jnz     @@1
         mov     ebx,[CRC]
         mov     [ebx],ax
    @@2: pop     ebx
    end;
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC16Update(var CRC: word; Msg: pointer; Len: word);
  {-update CRC16 with Msg data}
begin
  CRC16UpdateXL(CRC, Msg, Len);
end;


{$else}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure CRC16Update(var CRC: word; Msg: pointer; Len: word);
   {-update CRC16 with Msg data}
var
  i: word;
begin
  for i:=1 to Len do begin
    CRC := CTab[hi(CRC) xor pByte(Msg)^] xor (CRC shl 8);
    inc(Ptr2Inc(Msg));
  end;
end;


{$else}

(*************************************************************************
  BASM main loop for 8086
  @@1: xor     ah,es:[si]
       inc     si
       sub     bx,bx
       xchg    al,ah
       xchg    al,bl
       shl     bx,1
       xor     ax,word ptr ctab[bx]      {seg crctab = ds}
       loop    @@1
*************************************************************************)


{---------------------------------------------------------------------------}
procedure CRC16Update(var CRC: word; Msg: pointer; Len: word); assembler;
   {-update CRC16 with Msg data}
asm
           mov     cx,[Len]
           jcxz    @@2                       {no update if Len=0}
           les     di,[crc]
           mov     ax,es:[di]
           les     si,[Msg]
  db $66;  sub     dx,dx
           mov     dx,offset CTab            {ds:[edx] -> CTab}
  @@1:     xor     ah,es:[si]
           inc     si
           db      $66,$0f,$b6,$dc           {movzx ebx,ah}
           shl     ax,8
           db      $67,$33,$04,$5A           {xor ax,[edx+2*ebx]}
           dec     cx
           jnz     @@1
           les     di,[crc]
           stosw
  @@2:
end;

{$endif BASM16}
{$endif BIT16}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure CRC16FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC of Msg with init/update/final}
begin
  CRC16Init(CRC);
  CRC16UpdateXL(CRC, Msg, Len);
  CRC16Final(CRC);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC16Init(var CRC: word);
  {-CRC initialization}
begin
  CRC := 0;
end;


{---------------------------------------------------------------------------}
procedure CRC16Final(var CRC: word);
  {-CRC16: finalize calculation (dummy)}
begin
end;


{---------------------------------------------------------------------------}
function  CRC16SelfTest: boolean;
  {-Test from Numerical recipes, 2nd ed. p. 901: CRC('CatMouse987654321')=$E556}
const
  s: string[20] = 'CatMouse987654321';
var
  i,CRC,CRCF: word;
begin
  CRC16Full(CRCF, @s[1], length(s));
  CRC16Init(CRC);
  for i:=1 to length(s) do CRC16Update(CRC, @s[i], 1);
  CRC16Final(CRC);
  CRC16SelfTest := (CRC=$E556) and (CRCF=$E556);
end;


{---------------------------------------------------------------------------}
procedure CRC16Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC16 of Msg with init/update/final}
begin
  CRC16Init(CRC);
  CRC16Update(CRC, Msg, Len);
  CRC16Final(CRC);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure CRC16File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC16 of file, buf: buffer with at least bsize bytes}
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
  CRC16Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      CRC16UpdateXL(CRC, @buf, L);
    {$else}
      CRC16Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  CRC16Final(CRC);
end;

end.
