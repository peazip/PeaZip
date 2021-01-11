unit CRC32;

{32 Bit CRC, polynomial (used in Zip and others)}


interface

(*************************************************************************

 DESCRIPTION     :  32 Bit CRC

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.03.02  W.Ehrhardt  Initial version based on SHA1 layout
 0.20     23.07.03  W.Ehrhardt  With CRC32File, CRC32Full
 0.21     26.07.03  W.Ehrhardt  new self test with  CRC32Full, D6+ - warnings
 2.00     26.07.03  we          common vers., longint for word32, D4+ - warnings
 2.10     29.08.03  we          XL versions for Win32
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0, 386+
 2.31     08.10.03  we          BASM16 improvements, start english comments
 2.40     10.10.03  we          common version, english comments
 2.41     26.10.03  we          D4Plus {$warnings on}, Mask32=longint($FFFFFFFF);
                                fix VP Basm quirk, use [ebx*scale]
 2.50     24.11.03  we          new selfcheck string (unroll tests)
 3.00     01.12.03  we          Common version 3.0
 3.01     04.01.05  we          BASM16 uses:  xor eax,[edx+4*ebx]
 3.02     26.02.05  we          With {$ifdef StrictLong}
 3.03     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.04     17.12.05  we          Force $I- in CRC32File
 3.05     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.06     10.02.07  we          CRC32File: no eof, XL and filemode via $ifdef
 3.07     29.06.07  we          BASM16: align helpers
 3.08     04.10.07  we          FPC: {$asmmode intel}
 3.09     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.10     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.11     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.12     26.12.12  we          D17 and PurePascal
 3.13     16.08.15  we          Removed $ifdef DLL / stdcall
 3.14     29.11.17  we          CRC32File - fname: string

**************************************************************************)

{Credit goes to  Aleksandr Sharahov <alsha@mailru.com>}
{who communicated a speed up for the BIT32 ASM code}


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

procedure CRC32Init(var CRC: longint);
  {-initialize context}

procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}

procedure CRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}

function  CRC32SelfTest: boolean;
  {-Self test for CRC32}

procedure CRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}

procedure CRC32File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}


{$ifndef BIT16}
procedure CRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC32 with Msg data}

procedure CRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
{$endif}

implementation

const
  Mask32 = longint($FFFFFFFF);


{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

{$ifdef BASM16}
  {$i ALIGN.INC}
{$endif}

const
  {$ifdef BASM16}
    {$ifdef A4_CRC32}
      AlignDummy_CRC32: word = 0;
    {$endif}
  {$endif}
  CTab: array[0..255] of longint = (
    $00000000,$77073096,$ee0e612c,$990951ba,$076dc419,$706af48f,$e963a535,$9e6495a3,
    $0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988,$09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91,
    $1db71064,$6ab020f2,$f3b97148,$84be41de,$1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,
    $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,$fa0f3d63,$8d080df5,
    $3b6e20c8,$4c69105e,$d56041e4,$a2677172,$3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,
    $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940,$32d86ce3,$45df5c75,$dcd60dcf,$abd13d59,
    $26d930ac,$51de003a,$c8d75180,$bfd06116,$21b4f4b5,$56b3c423,$cfba9599,$b8bda50f,
    $2802b89e,$5f058808,$c60cd9b2,$b10be924,$2f6f7c87,$58684c11,$c1611dab,$b6662d3d,
    $76dc4190,$01db7106,$98d220bc,$efd5102a,$71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433,
    $7807c9a2,$0f00f934,$9609a88e,$e10e9818,$7f6a0dbb,$086d3d2d,$91646c97,$e6635c01,
    $6b6b51f4,$1c6c6162,$856530d8,$f262004e,$6c0695ed,$1b01a57b,$8208f4c1,$f50fc457,
    $65b0d9c6,$12b7e950,$8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65,
    $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,$4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb,
    $4369e96a,$346ed9fc,$ad678846,$da60b8d0,$44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9,
    $5005713c,$270241aa,$be0b1010,$c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
    $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,$2eb40d81,$b7bd5c3b,$c0ba6cad,
    $edb88320,$9abfb3b6,$03b6e20c,$74b1d29a,$ead54739,$9dd277af,$04db2615,$73dc1683,
    $e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8,$e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,
    $f00f9344,$8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,$196c3671,$6e6b06e7,
    $fed41b76,$89d32be0,$10da7a5a,$67dd4acc,$f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,
    $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252,$d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b,
    $d80d2bda,$af0a1b4c,$36034af6,$41047a60,$df60efc3,$a867df55,$316e8eef,$4669be79,
    $cb61b38c,$bc66831a,$256fd2a0,$5268e236,$cc0c7795,$bb0b4703,$220216b9,$5505262f,
    $c5ba3bbe,$b2bd0b28,$2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d,
    $9b64c2b0,$ec63f226,$756aa39c,$026d930a,$9c0906a9,$eb0e363f,$72076785,$05005713,
    $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,$92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21,
    $86d3d2d4,$f1d4e242,$68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,$18b74777,
    $88085ae6,$ff0f6a70,$66063bca,$11010b5c,$8f659eff,$f862ae69,$616bffd3,$166ccf45,
    $a00ae278,$d70dd2ee,$4e048354,$3903b3c2,$a7672661,$d06016f7,$4969474d,$3e6e77db,
    $aed16a4a,$d9d65adc,$40df0b66,$37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
    $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,$cdd70693,$54de5729,$23d967bf,
    $b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94,$b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d
);

{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}


{$ifndef BIT16}

(**** 32+ Bit Delphi2+/FPC/VP *****)

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  procedure CRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC32 with Msg data}
  var
    i: longint;
  begin
    for i:=1 to Len do begin
      CRC := CTab[byte(CRC) xor PByte(Msg)^] xor (CRC shr 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
{$else}
  {---------------------------------------------------------------------------}
  procedure CRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC32 with Msg data}
  begin
    {*we: unrolling decrease speed from 6 cycles/byte to 6.5..7.0 on P4 !!!}
    asm
         push  ebx
         mov   ecx,[Len]
         jecxz @@2
         mov   eax,[CRC]
         mov   eax,[eax]
         mov   edx,[Msg]

    @@1: movzx ebx,byte ptr [edx]
         inc   edx
         xor   bl,al
         shr   eax,8                        {CRC shr 8}
         xor   eax,dword ptr CTab[ebx*4]    {CTab[CRC xor Byte] xor (CRC shr 8)}
         dec   ecx
         jnz   @@1

         mov   edx,[CRC]
         mov   [edx],eax
    @@2: pop   ebx
    end;
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}
begin
  CRC32UpdateXL(CRC, Msg, Len);
end;


{$else}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}
var
  i: word;
begin
  for i:=1 to Len do begin
    CRC := CTab[byte(CRC) xor pByte(Msg)^] xor (CRC shr 8);
    inc(Ptr2Inc(Msg));
  end;
end;


{$else}


{---------------------------------------------------------------------------}
procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word); assembler;
  {-update CRC32 with Msg data}
asm
             mov   cx,[len]
             jcxz  @@2
             les   si,[CRC]
     db $66; mov   ax,es:[si]
             les   si,[Msg]
     db $66; sub   dx,dx
             mov   dx,offset CTab
   @@1:      db    $66, $26, $0f, $b6, $1c   {movzx ebx,es:[si]}
             inc   si
             xor   bl,al
     db $66; shr   ax,8
             db    $66,$67,$33,$04,$9A       {xor  eax,[edx+4*ebx]}
             dec   cx
             jnz   @@1

             les   si,CRC
     db $66; mov   es:[si],ax
  @@2:
end;


(*
{old version}
{---------------------------------------------------------------------------}
procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word); assembler;
  {-update CRC32 with Msg data}
asm
             mov   cx,[len]
             jcxz  @@2
             les   si,[CRC]
     db $66; mov   ax,es:[si]
             les   si,[Msg]

   @@1:      db    $26,$0F,$B6,$1C      {movzx  bx,es:byte ptr [si]}
             inc   si
             xor   bl,al
             shl   bx,2
     db $66; shr   ax,8
     db $66; xor   ax, word ptr CTab[bx]
             dec   cx
             jnz   @@1

             les   si,CRC
     db $66; mov   es:[si],ax
  @@2:
end;
*)

{$endif BASM16}
{$endif BIT16}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure CRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
begin
  CRC32Init(CRC);
  CRC32UpdateXL(CRC, Msg, Len);
  CRC32Final(CRC);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC32Init(var CRC: longint);
  {-CRC initialization}
begin
  CRC := Mask32;
end;


{---------------------------------------------------------------------------}
procedure CRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}
begin
  CRC := CRC xor Mask32;
end;


{---------------------------------------------------------------------------}
procedure CRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}
begin
  CRC32Init(CRC);
  CRC32Update(CRC, Msg, Len);
  CRC32Final(CRC);
end;


{---------------------------------------------------------------------------}
function  CRC32SelfTest: boolean;
  {-self test for CRC32}
const
  s: string[17] = '0123456789abcdefg';
  Check = longint($BE6CBE90);
var
  i: integer;
  CRC, CRCF: longint;
begin
  CRC32Full(CRCF, @s[1], length(s));
  CRC32Init(CRC);
  for i:=1 to length(s) do CRC32Update(CRC, @s[i], 1);
  CRC32Final(CRC);
  CRC32SelfTest := (CRC=Check) and (CRCF=Check);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure CRC32File({$ifdef CONST} const {$endif} fname: string;
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
  CRC32Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      CRC32UpdateXL(CRC, @buf, L);
    {$else}
      CRC32Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  CRC32Final(CRC);
end;

{$ifdef DumpAlign}
begin
  writeln('Align  CRC32: ',ofs(CTab) and 3:2);
{$endif}

end.
