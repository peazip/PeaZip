unit crc_sick;

{16 bit 'CRC' used in Sick devices}


interface

(*************************************************************************

 DESCRIPTION     :  16 bit 'CRC' used in Sick devices

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     27.03.10  W.Ehrhardt  Initial version
 0.11     08.12.10  we          WIN32 functions
 0.12     17.12.10  we          Swap result bytes in CRC_Sick_Final
 0.13     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 0.14     16.08.15  we          Removed $ifdef DLL / stdcall
 0.15     11.05.17  we          Added {$R-}
 0.16     29.11.17  we          CRC_Sick_File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2010-2017 Wolfgang Ehrhardt

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
  TSickCTX = packed record
               crc: word; {must be swapped for final result}
               prv: word; {temporary result for previous byte}
             end;

procedure CRC_Sick_Init(var ctx: TSickCTX);
  {-initialize context}

procedure CRC_Sick_Update(var ctx: TSickCTX; Msg: pointer; Len: word);
  {-update CRC_Sick with Msg data}

function CRC_Sick_Final(var ctx: TSickCTX): word;
  {-CRC_Sick: finalize calculation (dummy)}

function  CRC_Sick_SelfTest: boolean;
  {-Self test for CRC_Sick with '123456789' and 'abcdefghijklmnopqrstuvwxyz'}

procedure CRC_Sick_Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC_Sick of Msg with init/update/final}

procedure CRC_Sick_File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC_Sick of file, buf: buffer with at least bsize bytes}


{$ifndef BIT16}
procedure CRC_Sick_UpdateXL(var ctx: TSickCTX; Msg: pointer; Len: longint);
  {-update CRC_Sick with Msg data}

procedure CRC_Sick_FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC of Msg with init/update/final}
{$endif}


implementation


{$R-} {Needed for debug version and not 16-bit}
      {left-shifts are range-checked          }

{---------------------------------------------------------------------------}
procedure CRC_Sick_Update(var ctx: TSickCTX; Msg: pointer; Len: word);
   {-update CRC_Sick with Msg data}
var
  i: word;
begin
  with ctx do begin
    for i:=1 to Len do begin
      if crc and $8000 = 0 then crc := crc shl 1
      else crc := (crc shl 1) xor $8005;
      prv := (prv shl 8) or pByte(Msg)^;
      crc := crc xor prv;
      inc(Ptr2Inc(Msg));
    end;
  end;
end;


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure CRC_Sick_UpdateXL(var ctx: TSickCTX; Msg: pointer; Len: longint);
  {-update CRC_Sick with Msg data}
var
  i: longint;
begin
  with ctx do begin
    for i:=1 to Len do begin
      if crc and $8000 = 0 then crc := crc shl 1
      else crc := (crc shl 1) xor $8005;
      prv := (prv shl 8) or pByte(Msg)^;
      crc := crc xor prv;
      inc(Ptr2Inc(Msg));
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure CRC_Sick_FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC of Msg with init/update/final}
var
  ctx: TSickCTX;
begin
  CRC_Sick_Init(ctx);
  CRC_Sick_UpdateXL(ctx, Msg, Len);
  CRC := CRC_Sick_Final(ctx);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC_Sick_Init(var ctx: TSickCTX);
  {-CRC initialization}
begin
  ctx.crc := 0;
  ctx.prv := 0;
end;


{---------------------------------------------------------------------------}
function CRC_Sick_Final(var ctx: TSickCTX): word;
  {-CRC_Sick: finalize calculation (dummy)}
begin
  CRC_Sick_Final := swap(ctx.crc);
end;


{---------------------------------------------------------------------------}
function  CRC_Sick_SelfTest: boolean;
  {-Self test for CRC_Sick with '123456789' and 'abcdefghijklmnopqrstuvwxyz'}
const
  s1: array[0..8] of char8 = '123456789';
  s2: string[26] = 'abcdefghijklmnopqrstuvwxyz';
var
  i,CRC,CRCF: word;
  ctx: TSickCTX;
begin
  {Values from http://www.lammertbies.nl/comm/info/crc-calculation.html}
  {(also available as http://www.lammertbies.nl/download/lib_crc.zip)}
  {Verified with Easy Hash 1.5 from http://ziin.pl/en/easyhash}
  {$ifndef BIT16}
    CRC_Sick_FullXL(CRCF, @s1, sizeof(s1));
  {$else}
    CRC_Sick_Full(CRCF, @s1, sizeof(s1));
  {$endif}
  CRC_Sick_Init(ctx);
  for i:=1 to length(s2) do CRC_Sick_Update(ctx, @s2[i], 1);
  CRC := CRC_Sick_Final(ctx);
  CRC_Sick_SelfTest := (CRC=$42FE) and (CRCF=$56A6);
end;


{---------------------------------------------------------------------------}
procedure CRC_Sick_Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC_Sick of Msg with init/update/final}
var
  ctx: TSickCTX;
begin
  CRC_Sick_Init(ctx);
  CRC_Sick_Update(ctx, Msg, Len);
  CRC := CRC_Sick_Final(ctx);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure CRC_Sick_File({$ifdef CONST} const {$endif} fname: string;
                     var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC_Sick of file, buf: buffer with at least bsize bytes}
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
  ctx: TSickCTX;
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
  CRC_Sick_Init(ctx);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      CRC_Sick_UpdateXL(ctx, @buf, L);
    {$else}
      CRC_Sick_Update(ctx, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  CRC := CRC_Sick_Final(ctx);
end;

end.
