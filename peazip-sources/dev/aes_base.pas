unit AES_Base;

(*************************************************************************

 DESCRIPTION     :  AES basic routines

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] http://csrc.nist.gov/fips/fips-197.pdf
                    [2] rijndael-alg-fst.c V2.0/3.0: Rijmen et al Aug1999/Dec2000


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.23     16.08.03  we          From AESCrypt
 0.24     16.08.03  we          new xor_block
 0.25     18.09.03  we          Static tables, GF routines moved to aes_decr
 0.26     21.09.03  we          routines as functions
 0.27     27.09.03  we          FPC/go32v2
 0.28     05.10.03  we          STD.INC, TP5-6
 0.29     07.12.03  we          BugFix: exit if invalid key length
 0.30     27.12.03  we          BASM16: xorblock
 0.31     01.01.04  we          RotWord inline via shl/shr, SubWord function
 0.32     15.01.04  we          Keysetup like [2]
 0.33     15.01.04  we          BIT16: Keysetup with byte arrays
 0.34     06.03.04  we          removed exit in 128 bit key setup
 0.35     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.36     12.10.04  we          key setup with pointers
 0.37     29.11.04  we          FastInit
 0.38     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.39     24.12.04  we          Helper types PWA4, PLong
 0.40     24.12.04  we          FastInit, AES_Get/SetFastInit
 0.41     09.07.06  we          Checked: D9-D10
 0.42     25.12.12  we          {$J+} if needed
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2012 Wolfgang Ehrhardt

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

interface


uses AES_Type;


{helper types}
type
  TWA4  = packed array[0..3] of longint;      {AES block as array of longint}
  TBA4  = packed array[0..3] of byte;         {AES "word" as array of byte  }
  TAWk  = packed array[0..4*(AESMaxRounds+1)-1] of longint; {Key as array of longint}
  PWA4  = ^TWA4;
  PAWk  = ^TAWk;

{-AES static tables}
const
  SBox: array[byte] of byte =
   ($63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
    $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
    $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
    $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
    $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
    $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
    $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
    $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
    $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
    $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
    $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
    $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
    $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
    $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
    $e1, $f8, $98, $11, $69, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df,
    $8c, $a1, $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16);


{$ifdef CONST}

procedure AES_XorBlock(const B1, B2: TAESBlock; var B3: TAESBlock);
  {-xor two blocks, result in third}
  {$ifdef DLL} stdcall; {$endif}

function AES_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

{$else}

procedure AES_XorBlock(var B1, B2: TAESBlock; var B3: TAESBlock);
  {-xor two blocks, result in third}

function AES_Init(var Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}

{$endif}


procedure AES_SetFastInit(value: boolean);
  {-set FastInit variable}
  {$ifdef DLL} stdcall; {$endif}

function AES_GetFastInit: boolean;
  {-Returns FastInit variable}
  {$ifdef DLL} stdcall; {$endif}


implementation


{$ifdef D4Plus}
var
{$else}
{$ifdef J_OPT} {$J+} {$endif}
const
{$endif}
  FastInit : boolean = true;    {Clear only necessary context data at init}
                                {IV and buf remain uninitialized}

const
  RCon: array[0..9] of longint= ($01,$02,$04,$08,$10,$20,$40,$80,$1b,$36);


{$ifdef BASM16}
{---------------------------------------------------------------------------}
procedure AES_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TAESBlock; var B3: TAESBlock);
  {-xor two blocks, result in third}
begin
  asm
             mov   di,ds
             lds   si,[B1]
    db $66;  mov   ax,[si]
    db $66;  mov   bx,[si+4]
    db $66;  mov   cx,[si+8]
    db $66;  mov   dx,[si+12]
             lds   si,[B2]
    db $66;  xor   ax,[si]
    db $66;  xor   bx,[si+4]
    db $66;  xor   cx,[si+8]
    db $66;  xor   dx,[si+12]
             lds   si,[B3]
    db $66;  mov   [si],ax
    db $66;  mov   [si+4],bx
    db $66;  mov   [si+8],cx
    db $66;  mov   [si+12],dx
             mov   ds,di
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure AES_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TAESBlock; var B3: TAESBlock);
  {-xor two blocks, result in third}
var
  a1: TWA4 absolute B1;
  a2: TWA4 absolute B2;
  a3: TWA4 absolute B3;
begin
  a3[0] := a1[0] xor a2[0];
  a3[1] := a1[1] xor a2[1];
  a3[2] := a1[2] xor a2[2];
  a3[3] := a1[3] xor a2[3];
end;

{$endif BASM16}



{---------------------------------------------------------------------------}
function AES_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
var
  pK: ^TAWK;
  i : integer;
  temp: longint;
  {$ifdef BIT16}
    s: TBA4;
    t: TBA4 absolute temp;
  {$endif}
  Nk: word;
begin
  AES_Init := 0;

  if FastInit then with ctx do begin
    {Clear only the necessary context data at init. IV and buf}
    {remain uninitialized, other fields are initialized below.}
    bLen :=0;
    Flag :=0;
    {$ifdef CONST}
      IncProc := nil;
    {$else}
      {TP5-6 do not like IncProc := nil;}
      fillchar(IncProc, sizeof(IncProc), 0);
    {$endif}
  end
  else fillchar(ctx, sizeof(ctx), 0);

  if (KeyBits<>128) and (KeyBits<>192) and (KeyBits<>256) then begin
    AES_Init := AES_Err_Invalid_Key_Size;
    exit;
  end;

  Nk := KeyBits div 32;
  Move(Key, ctx.RK, 4*Nk);

  ctx.KeyBits := KeyBits;
  ctx.Rounds  := 6 + Nk;
  ctx.Decrypt := 0;

  {Calculate encryption round keys, cf.[2]}
  pK := addr(ctx.RK);

{$ifdef BIT16}
  {16 bit: use byte arrays}
  if keybits=128 then begin
    for i:=0 to 9 do begin
      temp := pK^[3];
      {SubWord(RotWord(temp)) if "word" count mod 4 = 0}
      s[0] := SBox[t[1]];
      s[1] := SBox[t[2]];
      s[2] := SBox[t[3]];
      s[3] := SBox[t[0]];
      pK^[4] := longint(s) xor pK^[0] xor RCon[i];
      pK^[5] := pK^[1] xor pK^[4];
      pK^[6] := pK^[2] xor pK^[5];
      pK^[7] := pK^[3] xor pK^[6];
      pK := addr(pK^[4]);
    end;
  end
  else if keybits=192 then begin
    for i:=0 to 7 do begin
      temp := pK^[5];
      {SubWord(RotWord(temp)) if "word" count mod 6 = 0}
      s[0] := SBox[t[1]];
      s[1] := SBox[t[2]];
      s[2] := SBox[t[3]];
      s[3] := SBox[t[0]];
      pK^[ 6] := longint(s) xor pK^[0] xor RCon[i];
      pK^[ 7] := pK^[1] xor pK^[6];
      pK^[ 8] := pK^[2] xor pK^[7];
      pK^[ 9] := pK^[3] xor pK^[8];
      if i=7 then exit;
      pK^[10] := pK^[4] xor pK^[ 9];
      pK^[11] := pK^[5] xor pK^[10];
      pK := addr(pK^[6]);
    end;
  end
  else begin
    for i:=0 to 6 do begin
      temp := pK^[7];
      {SubWord(RotWord(temp)) if "word" count mod 8 = 0}
      s[0] := SBox[t[1]];
      s[1] := SBox[t[2]];
      s[2] := SBox[t[3]];
      s[3] := SBox[t[0]];
      pK^[ 8] := longint(s) xor pK^[0] xor RCon[i];
      pK^[ 9] := pK^[1] xor pK^[ 8];
      pK^[10] := pK^[2] xor pK^[ 9];
      pK^[11] := pK^[3] xor pK^[10];
      if i=6 then exit;
      temp := pK^[11];
      {SubWord(temp) if "word" count mod 8 = 4}
      s[0] := SBox[t[0]];
      s[1] := SBox[t[1]];
      s[2] := SBox[t[2]];
      s[3] := SBox[t[3]];
      pK^[12] := longint(s) xor pK^[4];
      pK^[13] := pK^[5] xor pK^[12];
      pK^[14] := pK^[6] xor pK^[13];
      pK^[15] := pK^[7] xor pK^[14];
      pK := addr(pK^[8]);
    end;
  end;

{$else}
  {32 bit use shift and mask}
  if keybits=128 then begin
    for i:=0 to 9 do begin
      temp := pK^[3];
      {SubWord(RotWord(temp)) if "word" count mod 4 = 0}
      pK^[4] := (longint(SBox[(temp shr  8) and $ff])       ) xor
                (longint(SBox[(temp shr 16) and $ff]) shl  8) xor
                (longint(SBox[(temp shr 24)        ]) shl 16) xor
                (longint(SBox[(temp       ) and $ff]) shl 24) xor
                pK^[0] xor RCon[i];
      pK^[5] := pK^[1] xor pK^[4];
      pK^[6] := pK^[2] xor pK^[5];
      pK^[7] := pK^[3] xor pK^[6];
      pK := addr(pK^[4]);
    end;
  end
  else if keybits=192 then begin
    for i:=0 to 7 do begin
      temp := pK^[5];
      {SubWord(RotWord(temp)) if "word" count mod 6 = 0}
      pK^[ 6] := (longint(SBox[(temp shr  8) and $ff])       ) xor
                 (longint(SBox[(temp shr 16) and $ff]) shl  8) xor
                 (longint(SBox[(temp shr 24)        ]) shl 16) xor
                 (longint(SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 7] := pK^[1] xor pK^[6];
      pK^[ 8] := pK^[2] xor pK^[7];
      pK^[ 9] := pK^[3] xor pK^[8];
      if i=7 then exit;
      pK^[10] := pK^[4] xor pK^[ 9];
      pK^[11] := pK^[5] xor pK^[10];
      pK := addr(pK^[6]);
    end;
  end
  else begin
    for i:=0 to 6 do begin
      temp := pK^[7];
      {SubWord(RotWord(temp)) if "word" count mod 8 = 0}
      pK^[ 8] := (longint(SBox[(temp shr  8) and $ff])       ) xor
                 (longint(SBox[(temp shr 16) and $ff]) shl  8) xor
                 (longint(SBox[(temp shr 24)        ]) shl 16) xor
                 (longint(SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 9] := pK^[1] xor pK^[ 8];
      pK^[10] := pK^[2] xor pK^[ 9];
      pK^[11] := pK^[3] xor pK^[10];
      if i=6 then exit;
      temp := pK^[11];
      {SubWord(temp) if "word" count mod 8 = 4}
      pK^[12] := (longint(SBox[(temp       ) and $ff])       ) xor
                 (longint(SBox[(temp shr  8) and $ff]) shl  8) xor
                 (longint(SBox[(temp shr 16) and $ff]) shl 16) xor
                 (longint(SBox[(temp shr 24)        ]) shl 24) xor
                 pK^[4];
      pK^[13] := pK^[5] xor pK^[12];
      pK^[14] := pK^[6] xor pK^[13];
      pK^[15] := pK^[7] xor pK^[14];
      pK := addr(pK^[8]);
    end;
  end;

{$endif}

end;



{---------------------------------------------------------------------------}
procedure AES_SetFastInit(value: boolean);
  {-set FastInit variable}
begin
  FastInit := value;
end;


{---------------------------------------------------------------------------}
function AES_GetFastInit: boolean;
  {-Returns FastInit variable}
begin
  AES_GetFastInit := FastInit;
end;

end.
