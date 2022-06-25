unit CRC24;

{24 Bit CRC, polynomial $1864CFB (used in OpenPGP)}


interface

(*************************************************************************

 DESCRIPTION     :  24 Bit CRC, polynomial $1864CFB (used in OpenPGP)

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  J. Callas et al, "OpenPGP Message Format", available at
                    http://tools.ietf.org/html/rfc2440

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.04.06  W.Ehrhardt  Initial TP55 version based on CRC32 layout
 0.11     02.04.06  we          With array of byte absolute CRC
 0.12     02.04.06  we          BIT32 version
 0.13     02.04.06  we          BASM16 version
 0.14     02.04.06  we          BASM16: reordering of main loop
 0.15     03.04.06  we          TPGPDigest, Long2PGP
 0.16     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 0.17     10.02.07  we          CRC24File: no eof, XL and filemode via $ifdef
 0.18     29.06.07  we          BASM16: align helpers
 0.19     04.10.07  we          FPC: {$asmmode intel}
 0.20     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 0.21     25.04.09  we          updated RFC URL(s)
 0.22     19.07.09  we          D12 fix: assign with typecast string(fname)
 0.23     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 0.24     26.12.12  we          D17 and PurePascal
 0.25     16.08.15  we          Removed $ifdef DLL / stdcall
 0.26     29.11.17  we          CRC24File - fname: string

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

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

uses
  BTypes;

type
  TPGPDigest = array[0..2] of byte;  {OpenPGP 3 byte MSB first CRC24 digest}


procedure Long2PGP(CRC: longint; var PGPCRC: TPGPDigest);
  {-convert longint CRC24 to OpenPGP MSB first format}

procedure CRC24Init(var CRC: longint);
  {-initialize context}

procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC24 with Msg data}

procedure CRC24Final(var CRC: longint);
  {-CRC24: finalize calculation}

function  CRC24SelfTest: boolean;
  {-Self test for CRC24}

procedure CRC24Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC24 of Msg with init/update/final}

procedure CRC24File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC24 of file, buf: buffer with at least bsize bytes}


{$ifndef BIT16}
procedure CRC24UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC24 with Msg data}

procedure CRC24FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC24 of Msg with init/update/final}
{$endif}


implementation


{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

{$ifdef BASM16}
  {$i ALIGN.INC}
{$endif}

const
  {$ifdef BASM16}
    {$ifdef A4_CRC24}
      AlignDummy_CRC24: word = 0;
    {$endif}
  {$endif}
  CT24: array[0..255] of longint = (
    $00000000,$00864cfb,$008ad50d,$000c99f6,$0093e6e1,$0015aa1a,$001933ec,$009f7f17,
    $00a18139,$0027cdc2,$002b5434,$00ad18cf,$003267d8,$00b42b23,$00b8b2d5,$003efe2e,
    $00c54e89,$00430272,$004f9b84,$00c9d77f,$0056a868,$00d0e493,$00dc7d65,$005a319e,
    $0064cfb0,$00e2834b,$00ee1abd,$00685646,$00f72951,$007165aa,$007dfc5c,$00fbb0a7,
    $000cd1e9,$008a9d12,$008604e4,$0000481f,$009f3708,$00197bf3,$0015e205,$0093aefe,
    $00ad50d0,$002b1c2b,$002785dd,$00a1c926,$003eb631,$00b8faca,$00b4633c,$00322fc7,
    $00c99f60,$004fd39b,$00434a6d,$00c50696,$005a7981,$00dc357a,$00d0ac8c,$0056e077,
    $00681e59,$00ee52a2,$00e2cb54,$006487af,$00fbf8b8,$007db443,$00712db5,$00f7614e,
    $0019a3d2,$009fef29,$009376df,$00153a24,$008a4533,$000c09c8,$0000903e,$0086dcc5,
    $00b822eb,$003e6e10,$0032f7e6,$00b4bb1d,$002bc40a,$00ad88f1,$00a11107,$00275dfc,
    $00dced5b,$005aa1a0,$00563856,$00d074ad,$004f0bba,$00c94741,$00c5deb7,$0043924c,
    $007d6c62,$00fb2099,$00f7b96f,$0071f594,$00ee8a83,$0068c678,$00645f8e,$00e21375,
    $0015723b,$00933ec0,$009fa736,$0019ebcd,$008694da,$0000d821,$000c41d7,$008a0d2c,
    $00b4f302,$0032bff9,$003e260f,$00b86af4,$002715e3,$00a15918,$00adc0ee,$002b8c15,
    $00d03cb2,$00567049,$005ae9bf,$00dca544,$0043da53,$00c596a8,$00c90f5e,$004f43a5,
    $0071bd8b,$00f7f170,$00fb6886,$007d247d,$00e25b6a,$00641791,$00688e67,$00eec29c,
    $003347a4,$00b50b5f,$00b992a9,$003fde52,$00a0a145,$0026edbe,$002a7448,$00ac38b3,
    $0092c69d,$00148a66,$00181390,$009e5f6b,$0001207c,$00876c87,$008bf571,$000db98a,
    $00f6092d,$007045d6,$007cdc20,$00fa90db,$0065efcc,$00e3a337,$00ef3ac1,$0069763a,
    $00578814,$00d1c4ef,$00dd5d19,$005b11e2,$00c46ef5,$0042220e,$004ebbf8,$00c8f703,
    $003f964d,$00b9dab6,$00b54340,$00330fbb,$00ac70ac,$002a3c57,$0026a5a1,$00a0e95a,
    $009e1774,$00185b8f,$0014c279,$00928e82,$000df195,$008bbd6e,$00872498,$00016863,
    $00fad8c4,$007c943f,$00700dc9,$00f64132,$00693e25,$00ef72de,$00e3eb28,$0065a7d3,
    $005b59fd,$00dd1506,$00d18cf0,$0057c00b,$00c8bf1c,$004ef3e7,$00426a11,$00c426ea,
    $002ae476,$00aca88d,$00a0317b,$00267d80,$00b90297,$003f4e6c,$0033d79a,$00b59b61,
    $008b654f,$000d29b4,$0001b042,$0087fcb9,$001883ae,$009ecf55,$009256a3,$00141a58,
    $00efaaff,$0069e604,$00657ff2,$00e33309,$007c4c1e,$00fa00e5,$00f69913,$0070d5e8,
    $004e2bc6,$00c8673d,$00c4fecb,$0042b230,$00ddcd27,$005b81dc,$0057182a,$00d154d1,
    $0026359f,$00a07964,$00ace092,$002aac69,$00b5d37e,$00339f85,$003f0673,$00b94a88,
    $0087b4a6,$0001f85d,$000d61ab,$008b2d50,$00145247,$00921ebc,$009e874a,$0018cbb1,
    $00e37b16,$006537ed,$0069ae1b,$00efe2e0,$00709df7,$00f6d10c,$00fa48fa,$007c0401,
    $0042fa2f,$00c4b6d4,$00c82f22,$004e63d9,$00d11cce,$00575035,$005bc9c3,$00dd8538
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
  procedure CRC24UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC24 with Msg data}
  var
    i: longint;
  begin
    for i:=1 to Len do begin
      CRC := CT24[byte(CRC shr 16) xor PByte(Msg)^] xor (CRC shl 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
{$else}
  {---------------------------------------------------------------------------}
  procedure CRC24UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC24 with Msg data}
  begin
    {CRC := CT24[byte(CRC shr 16) xor pByte(Msg)^] xor (CRC shl 8);}
    asm
         push  esi
         push  ebx
         mov   ecx,[Len]
         jecxz @@2
         mov   eax,[CRC]
         mov   eax,[eax]                    {eax holds CRC during main loop}
         mov   esi,[Msg]

    @@1: movzx ebx,byte ptr [esi]
         inc   esi
         mov   edx,eax
         shr   edx,16
         xor   bl,dl
         shl   eax,8                        {CRC shl 8}
         xor   eax,dword ptr CT24[ebx*4]    {CTab[(CRC shr 16) xor Byte] xor (CRC shl 8)}
         dec   ecx
         jnz   @@1

         mov   edx,[CRC]
         mov   [edx],eax
    @@2: pop   ebx
         pop   esi
    end;
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC24 with Msg data}
begin
  CRC24UpdateXL(CRC, Msg, Len);
end;


{$else}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC24 with Msg data}
var
  i: word;
  CA: packed array[0..3] of byte absolute CRC;
begin
  for i:=1 to Len do begin
    {CRC := CT24[byte(CRC shr 16) xor pByte(Msg)^] xor (CRC shl 8);}
    CRC := CT24[CA[2] xor pByte(Msg)^] xor (CRC shl 8);
    inc(Ptr2Inc(Msg));
  end;
end;


{$else}


{---------------------------------------------------------------------------}
procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word); assembler;
  {-update CRC24 with Msg data}
asm
               {CRC := CT24[byte(CRC shr 16) xor pByte(Msg)^] xor (CRC shl 8);}
               mov   cx,[len]
               jcxz  @@2
               les   si,[CRC]
       db $66; mov   ax,es:[si]
               les   si,[Msg]
       db $66; sub   di,di
               mov   di,offset CT24
  @@1: db $66; mov   dx,ax
       db $66; shr   dx,16                     {dx = CRC shr 16}
               db    $66, $26, $0f, $b6, $1c   {movzx ebx,es:[si]}
               inc   si
               xor   bl,dl
       db $66; shl   ax,8
               db    $66,$67,$33,$04,$9F       {xor eax,[edi+4*ebx]}
               dec   cx
               jnz   @@1

               les   si,CRC
       db $66; mov   es:[si],ax
  @@2:
end;


{$endif BASM16}
{$endif BIT16}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure CRC24FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC24 of Msg with init/update/final}
begin
  CRC24Init(CRC);
  CRC24UpdateXL(CRC, Msg, Len);
  CRC24Final(CRC);
end;
{$endif}



{---------------------------------------------------------------------------}
procedure CRC24Init(var CRC: longint);
  {-CRC initialization}
begin
  CRC := $B704CE;
end;


{---------------------------------------------------------------------------}
procedure CRC24Final(var CRC: longint);
  {-CRC24: finalize calculation}
begin
  {Mask 24 bits}
  CRC := CRC and $FFFFFF;
end;


{---------------------------------------------------------------------------}
procedure CRC24Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC24 of Msg with init/update/final}
begin
  CRC24Init(CRC);
  CRC24Update(CRC, Msg, Len);
  CRC24Final(CRC);
end;


{---------------------------------------------------------------------------}
procedure Long2PGP(CRC: longint; var PGPCRC: TPGPDigest);
  {-convert longint CRC24 to OpenPGP MSB first format}
var
  CA: packed array[0..3] of byte absolute CRC;
begin
  PGPCRC[0] := CA[2];
  PGPCRC[1] := CA[1];
  PGPCRC[2] := CA[0];
end;


{---------------------------------------------------------------------------}
function  CRC24SelfTest: boolean;
  {-self test for CRC24}
const
  s: string[17] = '0123456789abcdefg';
  Check = $30B593; {calculated with trfc2440.c}
var
  i: integer;
  CRC, CRCF: longint;
begin
  CRC24Full(CRCF, @s[1], length(s));
  CRC24Init(CRC);
  for i:=1 to length(s) do CRC24Update(CRC, @s[i], 1);
  CRC24Final(CRC);
  CRC24SelfTest := (CRC=Check) and (CRCF=Check);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure CRC24File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC24 of file, buf: buffer with at least bsize bytes}
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
  CRC24Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      CRC24UpdateXL(CRC, @buf, L);
    {$else}
      CRC24Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  CRC24Final(CRC);
end;

{$ifdef DumpAlign}
begin
  writeln('Align  CRC24: ',ofs(CT24) and 3:2);
{$endif}

end.
