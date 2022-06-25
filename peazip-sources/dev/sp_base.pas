unit sp_base;

(*************************************************************************

 DESCRIPTION   :  Serpent basic routines

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REFERENCES    :  [1] Source code and test vectors available from the Serpent
                      page http://www.cl.cam.ac.uk/~rja14/serpent.html
                  [2] Wei Dai's public domain Serpent.cpp in
                      Crypto++ Library 5.5.2 from http://www.cryptopp.com
                  [3] Dag Arne Osvik, Speeding up Serpent, available as
                      http://www.ii.uib.no/~osvik/pub/aes3.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.01     25.03.08  W.Ehrhardt  Initial BP7 key init
 0.02     25.03.08  we          Encrypt
 0.03     25.03.08  we          Decrypt, fix keysetup, TP5-7
 0.04     25.03.08  we          RotR and RotL inline
 0.05     26.03.08  we          RND/Trans functions with conditional inline
 0.06     18.04.08  we          Transform and RND functions with BASM16
 0.07     19.04.08  we          InvTrans and InvRND functions with BASM16
 0.08     19.04.08  we          Keysetup like [2]
 0.09     19.04.08  we          Removed keying procedure
 0.10     13.04.09  we          Fixed missing URLs for references
 0.11     01.08.10  we          SP_Err_CTR_SeekOffset, SP_Err_Invalid_16Bit_Length
 0.12     22.07.12  we          64-bit compatibility
 0.13     25.12.12  we          {$J+} if needed
 0.14     26.07.14  we          Modified BASM32 routines from EddyHawk
 **************************************************************************)



(*-------------------------------------------------------------------------
 (C) Copyright 2008-2014 Wolfgang Ehrhardt

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

{$i std.inc}

{$ifdef BIT32}
  {$ifdef PurePascal}
    {$undef Use_BASM32}
  {$else}
    {$define Use_BASM32}   {default use EddyHawk's BASM32 routines}
  {$endif}
{$endif}


interface

const
  SP_Err_Invalid_Key_Size       = -1;  {Key size in bits not 128, 192, or 256}
  SP_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  SP_Err_Data_After_Short_Block = -4;  {Short block must be last}
  SP_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting}
  SP_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}

  SP_Err_CTR_SeekOffset         = -15; {Negative offset in SP_CTR_Seek}
  SP_Err_Invalid_16Bit_Length   = -20; {Pointer + Offset > $FFFF for 16 bit code}

type
  TSPRndKey  = packed array[0..131] of longint;
  TSPBlock   = packed array[0..15]  of byte;
  PSPBlock   = ^TSPBlock;

type
  TSPIncProc = procedure(var CTR: TSPBlock);   {user supplied IncCTR proc}
                {$ifdef DLL} stdcall; {$endif}
type
  TSPContext = packed record
                 IV      : TSPBlock;   {IV or CTR              }
                 buf     : TSPBlock;   {Work buffer            }
                 bLen    : word;       {Bytes used in buf      }
                 Flag    : word;       {Bit 1: Short block     }
                 IncProc : TSPIncProc; {Increment proc CTR-Mode}
                 RK      : TSPRndKey;  {Round keys             }
               end;

const
  SPBLKSIZE  = sizeof(TSPBlock);       {Serpent block size}


{$ifdef CONST}

function  SP_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent round key and key-dependent sbox initialisation}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_Encrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
  {-encrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_Decrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
  {-decrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_XorBlock(const B1, B2: TSPBlock; var B3: TSPBlock);
  {-xor two blocks, result in third}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  SP_Init(var Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent round key and key-dependent sbox initialisation}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_Encrypt(var ctx: TSPContext; var BI: TSPBlock; var BO: TSPBlock);
  {-encrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_Decrypt(var ctx: TSPContext; var BI: TSPBlock; var BO: TSPBlock);
  {-decrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_XorBlock(var B1, B2: TSPBlock; var B3: TSPBlock);
  {-xor two blocks, result in third}

{$endif}

procedure SP_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_SetFastInit(value: boolean);
  {-set FastInit variable}
  {$ifdef DLL} stdcall; {$endif}

function  SP_GetFastInit: boolean;
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

type
  TWA4 = packed array[0..3] of longint;  {SP block as array of longint}
  TWA8 = packed array[0..7] of longint;  {longint type cast for key}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
function RotL(x: longint; c: word): longint; {$ifdef HAS_INLINE} inline; {$endif}
 {-Rotate left c bits, room for optimization}
begin
  RotL := (x shl c) or (x shr (32-c));
end;


{---------------------------------------------------------------------------}
function RotR(x: longint; c: word): longint; {$ifdef HAS_INLINE} inline; {$endif}
 {-Rotate right c bits, room for optimization}
begin
  RotR := (x shr c) or (x shl (32-c));
end;
{$else}

{$ifdef BASM16}

(** TP6-7/D1 **)

{---------------------------------------------------------------------------}
function RotL(X: longint; c: word): longint;
inline(
  $59/              {pop    cx     }
  $66/$58/          {pop    eax    }
  $66/$D3/$C0/      {rol    eax,cl }
  $66/$8B/$D0/      {mov    edx,eax}
  $66/$C1/$EA/$10); {shr    edx,16 }


{---------------------------------------------------------------------------}
function RotR(X: longint; c: word): longint;
inline(
  $59/              {pop    cx     }
  $66/$58/          {pop    eax    }
  $66/$D3/$C8/      {ror    eax,cl }
  $66/$8B/$D0/      {mov    edx,eax}
  $66/$C1/$EA/$10); {shr    edx,16 }

{$else}

{** T5/5.5 **}

{---------------------------------------------------------------------------}
function RotL(X: longint; c: word): longint;
  {-Rotate left}
inline(
  $59/           {  pop    cx    }
  $58/           {  pop    ax    }
  $5A/           {  pop    dx    }

  $83/$F9/$10/   {  cmp    cx,16 }
  $72/$06/       {  jb     S     }
  $92/           {  xchg   dx,ax }
  $83/$E9/$10/   {  sub    cx,16 }
  $74/$09/       {  je     X     }

  $2B/$DB/       {S:sub    bx,bx }
  $D1/$D0/       {L:rcl    ax,1  }
  $D1/$D2/       {  rcl    dx,1  }
  $13/$C3/       {  adc    ax,bx }
  $49/           {  dec    cx    }
  $75/$F7);      {  jne    L     }
                 {X:             }

{---------------------------------------------------------------------------}
function RotR(X: longint; c: word): longint;
  {-Rotate left}
inline(
  $59/           {  pop    cx   }
  $58/           {  pop    ax   }
  $5A/           {  pop    dx   }

  $83/$F9/$10/   {  cmp    cx,16}
  $72/$06/       {  jb     S    }
  $92/           {  xchg   dx,ax}
  $83/$E9/$10/   {  sub    cx,16}
  $74/$09/       {  je     X    }

  $8B/$DA/       {  mov   bx,dx }
  $D1/$EB/       {L:shr   bx,1  }
  $D1/$D8/       {  rcr   ax,1  }
  $D1/$DA/       {  rcr   dx,1  }
  $49/           {  dec   cx    }
  $75/$F7);      {  jne   L     }
                 {X:            }

{$endif}
{$endif}

{$ifdef BASM16}
{---------------------------------------------------------------------------}
procedure SP_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TSPBlock; var B3: TSPBlock);
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
procedure SP_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TSPBlock; var B3: TSPBlock);
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



{$ifdef BASM16}

{BASM16 S-box expressions are from the [3]}

{---------------------------------------------------------------------------}
procedure RND0(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  xor  dx,ax                {x3 := x3 xor x0; }
    db $66;  mov  si,bx                {x4 :=        x1; }
    db $66;  and  bx,dx                {x1 := x1 and x3; }
    db $66;  xor  si,cx                {x4 := x4 xor x2; }
    db $66;  xor  bx,ax                {x1 := x1 xor x0; }
    db $66;  or   ax,dx                {x0 := x0 or  x3; }
    db $66;  xor  ax,si                {x0 := x0 xor x4; }
    db $66;  xor  si,dx                {x4 := x4 xor x3; }
    db $66;  xor  dx,cx                {x3 := x3 xor x2; }
    db $66;  or   cx,bx                {x2 := x2 or  x1; }
    db $66;  xor  cx,si                {x2 := x2 xor x4; }
    db $66;  not  si                   {x4 :=    not x4; }
    db $66;  or   si,bx                {x4 := x4 or  x1; }
    db $66;  xor  bx,dx                {x1 := x1 xor x3; }
    db $66;  xor  bx,si                {x1 := x1 xor x4; }
    db $66;  or   dx,ax                {x3 := x3 or  x0; }
    db $66;  xor  bx,dx                {x1 := x1 xor x3; }
    db $66;  xor  si,dx                {x4 := x4 xor x3; }
             les  di,[x3]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],si
             les  di,[x2]
    db $66;  mov  word ptr es:[di],cx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND1(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  not  ax                   {x0 := not x0;    }
    db $66;  not  cx                   {x2 := not x2;    }
    db $66;  mov  si,ax                {x4 := x0;        }
    db $66;  and  ax,bx                {x0 := x0 and x1; }
    db $66;  xor  cx,ax                {x2 := x2 xor x0; }
    db $66;  or   ax,dx                {x0 := x0 or  x3; }
    db $66;  xor  dx,cx                {x3 := x3 xor x2; }
    db $66;  xor  bx,ax                {x1 := x1 xor x0; }
    db $66;  xor  ax,si                {x0 := x0 xor x4; }
    db $66;  or   si,bx                {x4 := x4 or  x1; }
    db $66;  xor  bx,dx                {x1 := x1 xor x3; }
    db $66;  or   cx,ax                {x2 := x2 or  x0; }
    db $66;  and  cx,si                {x2 := x2 and x4; }
    db $66;  xor  ax,bx                {x0 := x0 xor x1; }
    db $66;  and  bx,cx                {x1 := x1 and x2; }
    db $66;  xor  bx,ax                {x1 := x1 xor x0; }
    db $66;  and  ax,cx                {x0 := x0 and x2; }
    db $66;  xor  ax,si                {x0 := x0 xor x4; }
             les  di,[x0]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],ax
  end
end;


{---------------------------------------------------------------------------}
procedure RND2(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,ax                {x4 := x0;         }
    db $66;  and  ax,cx                {x0 := x0 and x2;  }
    db $66;  xor  ax,dx                {x0 := x0 xor x3;  }
    db $66;  xor  cx,bx                {x2 := x2 xor x1;  }
    db $66;  xor  cx,ax                {x2 := x2 xor x0;  }
    db $66;  or   dx,si                {x3 := x3 or  x4;  }
    db $66;  xor  dx,bx                {x3 := x3 xor x1;  }
    db $66;  xor  si,cx                {x4 := x4 xor x2;  }
    db $66;  mov  bx,dx                {x1 := x3;         }
    db $66;  or   dx,si                {x3 := x3 or  x4;  }
    db $66;  xor  dx,ax                {x3 := x3 xor x0;  }
    db $66;  and  ax,bx                {x0 := x0 and x1;  }
    db $66;  xor  si,ax                {x4 := x4 xor x0;  }
    db $66;  xor  bx,dx                {x1 := x1 xor x3;  }
    db $66;  xor  bx,si                {x1 := x1 xor x4;  }
    db $66;  not  si                   {x4 := not x4;     }
             les  di,[x0]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],si
  end;
end;


{---------------------------------------------------------------------------}
procedure RND3(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,ax                {x4 := x0;       }
    db $66;  or   ax,dx                {x0 := x0 or  x3;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  and  bx,si                {x1 := x1 and x4;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  and  dx,ax                {x3 := x3 and x0;}
    db $66;  or   si,bx                {x4 := x4 or  x1;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  xor  ax,bx                {x0 := x0 xor x1;}
    db $66;  and  si,ax                {x4 := x4 and x0;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  or   bx,ax                {x1 := x1 or  x0;}
    db $66;  xor  bx,cx                {x1 := x1 xor x2;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  mov  cx,bx                {x2 := x1;       }
    db $66;  or   bx,dx                {x1 := x1 or  x3;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],si
  end;
end;


{---------------------------------------------------------------------------}
procedure RND4(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  not  dx                   {x3 :=    not x3;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  mov  si,bx                {x4 :=        x1;}
    db $66;  and  bx,dx                {x1 := x1 and x3;}
    db $66;  xor  bx,cx                {x1 := x1 xor x2;}
    db $66;  xor  si,dx                {x4 := x4 xor x3;}
    db $66;  xor  ax,si                {x0 := x0 xor x4;}
    db $66;  and  cx,si                {x2 := x2 and x4;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  and  ax,bx                {x0 := x0 and x1;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  or   si,bx                {x4 := x4 or  x1;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
    db $66;  or   ax,dx                {x0 := x0 or  x3;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  and  cx,dx                {x2 := x2 and x3;}
    db $66;  not  ax                   {x0 :=    not x0;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
             les  di,[x2]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],si
             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND5(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  xor  ax,bx                {x0 := x0 xor x1;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  not  dx                   {x3 :=    not x3;}
    db $66;  mov  si,bx                {x4 :=        x1;}
    db $66;  and  bx,ax                {x1 := x1 and x0;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  bx,cx                {x1 := x1 xor x2;}
    db $66;  or   cx,si                {x2 := x2 or  x4;}
    db $66;  xor  si,dx                {x4 := x4 xor x3;}
    db $66;  and  dx,bx                {x3 := x3 and x1;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  and  ax,dx                {x0 := x0 and x3;}
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  xor  ax,si                {x0 := x0 xor x4;}
    db $66;  or   si,dx                {x4 := x4 or  x3;}
    db $66;  xor  cx,si                {x2 := x2 xor x4;}
             les  di,[x3]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND6(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  mov  si,dx                {x4 :=        x3;}
    db $66;  and  dx,ax                {x3 := x3 and x0;}
    db $66;  xor  ax,si                {x0 := x0 xor x4;}
    db $66;  xor  dx,cx                {x3 := x3 xor x2;}
    db $66;  or   cx,si                {x2 := x2 or  x4;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  or   ax,bx                {x0 := x0 or  x1;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
    db $66;  or   ax,dx                {x0 := x0 or  x3;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  xor  si,dx                {x4 := x4 xor x3;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
    db $66;  not  dx                   {x3 :=    not x3;}
    db $66;  and  cx,si                {x2 := x2 and x4;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
             les  di,[x3]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],si
             les  di,[x1]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x0]
    db $66;  mov  word ptr es:[di],ax
  end;
end;


{---------------------------------------------------------------------------}
procedure RND7(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,bx                {x4 :=        x1;}
    db $66;  or   bx,cx                {x1 := x1 or  x2;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  or   dx,si                {x3 := x3 or  x4;}
    db $66;  and  dx,ax                {x3 := x3 and x0;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  or   bx,si                {x1 := x1 or  x4;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
    db $66;  or   ax,si                {x0 := x0 or  x4;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  xor  bx,si                {x1 := x1 xor x4;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  and  bx,ax                {x1 := x1 and x0;}
    db $66;  xor  bx,si                {x1 := x1 xor x4;}
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  or   cx,ax                {x2 := x2 or  x0;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
             les  di,[x2]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],si
  end;
end;


{---------------------------------------------------------------------------}
procedure Transform(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]

    db $66;  rol  ax,13                {x0 := RotL(x0, 13);            }
    db $66;  rol  cx,3                 {x2 := RotL(x2,  3);            }
    db $66;  xor  bx,ax                {x1 := x1 xor x0 xor x2;        }
    db $66;  xor  bx,cx
    db $66;  xor  dx,cx                {x3 := x3 xor x2 xor (x0 shl 3);}
    db $66;  mov  si,ax
    db $66;  shl  si,3
    db $66;  xor  dx,si
    db $66;  rol  bx,1                 {x1 := RotL(x1, 1);             }
    db $66;  rol  dx,7                 {x3 := RotL(x3, 7);             }
    db $66;  xor  ax,bx                {x0 := x0 xor x1 xor x3;        }
    db $66;  xor  ax,dx
    db $66;  xor  cx,dx                {x2 := x2 xor x3 xor (x1 shl 7);}
    db $66;  mov  si,bx
    db $66;  shl  si,7
    db $66;  xor  cx,si
    db $66;  rol  ax,5                 {x0 := RotL(x0,  5);            }
    db $66;  rol  cx,22                {x2 := RotL(x2, 22);            }

             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x0]
    db $66;  mov  word ptr es:[di],ax
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND0(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  mov  si,bx                {x4 :=        x1;}
    db $66;  or   bx,ax                {x1 := x1 or  x0;}
    db $66;  not  si                   {x4 :=    not x4;}
    db $66;  xor  bx,cx                {x1 := x1 xor x2;}
    db $66;  or   cx,si                {x2 := x2 or  x4;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  ax,si                {x0 := x0 xor x4;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  and  ax,dx                {x0 := x0 and x3;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
    db $66;  or   ax,bx                {x0 := x0 or  x1;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  and  cx,dx                {x2 := x2 and x3;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
             les  di,[x2]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],si
             les  di,[x0]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND1(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,bx                {x4 :=        x1;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  and  dx,bx                {x3 := x3 and x1;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  or   ax,bx                {x0 := x0 or  x1;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  ax,si                {x0 := x0 xor x4;}
    db $66;  or   ax,cx                {x0 := x0 or  x2;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  ax,bx                {x0 := x0 xor x1;}
    db $66;  or   bx,dx                {x1 := x1 or  x3;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
    db $66;  not  si                   {x4 :=    not x4;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  or   bx,ax                {x1 := x1 or  x0;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
    db $66;  or   bx,si                {x1 := x1 or  x4;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
             les  di,[x1]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],si
             les  di,[x2]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],cx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND2(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  mov  si,dx                {x4 :=        x3;}
    db $66;  and  dx,cx                {x3 := x3 and x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  or   bx,cx                {x1 := x1 or  x2;}
    db $66;  xor  bx,si                {x1 := x1 xor x4;}
    db $66;  and  si,dx                {x4 := x4 and x3;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  and  si,ax                {x4 := x4 and x0;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  and  cx,bx                {x2 := x2 and x1;}
    db $66;  or   cx,ax                {x2 := x2 or  x0;}
    db $66;  not  dx                   {x3 :=    not x3;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  and  ax,bx                {x0 := x0 and x1;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],si
             les  di,[x2]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND3(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,cx                {x4 :=        x2;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  and  si,cx                {x4 := x4 and x2;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
    db $66;  and  ax,bx                {x0 := x0 and x1;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  or   dx,si                {x3 := x3 or  x4;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  xor  bx,si                {x1 := x1 xor x4;}
    db $66;  and  dx,cx                {x3 := x3 and x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
    db $66;  or   bx,cx                {x1 := x1 or  x2;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  xor  bx,si                {x1 := x1 xor x4;}
    db $66;  xor  ax,bx                {x0 := x0 xor x1;}
             les  di,[x0]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x1]
    db $66;  mov  word ptr es:[di],bx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND4(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,cx                {x4 :=        x2;}
    db $66;  and  cx,dx                {x2 := x2 and x3;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  or   bx,dx                {x1 := x1 or  x3;}
    db $66;  and  bx,ax                {x1 := x1 and x0;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  and  bx,cx                {x1 := x1 and x2;}
    db $66;  not  ax                   {x0 :=    not x0;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  and  dx,ax                {x3 := x3 and x0;}
    db $66;  xor  dx,cx                {x3 := x3 xor x2;}
    db $66;  xor  ax,bx                {x0 := x0 xor x1;}
    db $66;  and  cx,ax                {x2 := x2 and x0;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  xor  cx,si                {x2 := x2 xor x4;}
    db $66;  or   cx,dx                {x2 := x2 or  x3;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
             les  di,[x1]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],si
             les  di,[x2]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x0]
    db $66;  mov  word ptr es:[di],ax
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND5(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  not  bx                   {x1 :=    not x1;}
    db $66;  mov  si,dx                {x4 :=        x3;}
    db $66;  xor  cx,bx                {x2 := x2 xor x1;}
    db $66;  or   dx,ax                {x3 := x3 or  x0;}
    db $66;  xor  dx,cx                {x3 := x3 xor x2;}
    db $66;  or   cx,bx                {x2 := x2 or  x1;}
    db $66;  and  cx,ax                {x2 := x2 and x0;}
    db $66;  xor  si,dx                {x4 := x4 xor x3;}
    db $66;  xor  cx,si                {x2 := x2 xor x4;}
    db $66;  or   si,ax                {x4 := x4 or  x0;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  and  bx,cx                {x1 := x1 and x2;}
    db $66;  xor  bx,dx                {x1 := x1 xor x3;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
    db $66;  and  dx,si                {x3 := x3 and x4;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  not  si                   {x4 :=    not x4;}
    db $66;  xor  dx,ax                {x3 := x3 xor x0;}
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],si
             les  di,[x3]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND6(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  mov  si,cx                {x4 :=        x2;}
    db $66;  and  cx,ax                {x2 := x2 and x0;}
    db $66;  xor  si,dx                {x4 := x4 xor x3;}
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  xor  cx,dx                {x2 := x2 xor x3;}
    db $66;  or   si,ax                {x4 := x4 or  x0;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  and  bx,dx                {x1 := x1 and x3;}
    db $66;  xor  bx,ax                {x1 := x1 xor x0;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  or   ax,cx                {x0 := x0 or  x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  xor  si,ax                {x4 := x4 xor x0;}
             les  di,[x0]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],si
             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
  end;
end;


{---------------------------------------------------------------------------}
procedure InvRND7(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]
    db $66;  mov  si,cx                {x4 :=        x2;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  and  ax,dx                {x0 := x0 and x3;}
    db $66;  or   si,dx                {x4 := x4 or  x3;}
    db $66;  not  cx                   {x2 :=    not x2;}
    db $66;  xor  dx,bx                {x3 := x3 xor x1;}
    db $66;  or   bx,ax                {x1 := x1 or  x0;}
    db $66;  xor  ax,cx                {x0 := x0 xor x2;}
    db $66;  and  cx,si                {x2 := x2 and x4;}
    db $66;  and  dx,si                {x3 := x3 and x4;}
    db $66;  xor  bx,cx                {x1 := x1 xor x2;}
    db $66;  xor  cx,ax                {x2 := x2 xor x0;}
    db $66;  or   ax,cx                {x0 := x0 or  x2;}
    db $66;  xor  si,bx                {x4 := x4 xor x1;}
    db $66;  xor  ax,dx                {x0 := x0 xor x3;}
    db $66;  xor  dx,si                {x3 := x3 xor x4;}
    db $66;  or   si,ax                {x4 := x4 or  x0;}
    db $66;  xor  dx,cx                {x3 := x3 xor x2;}
    db $66;  xor  si,cx                {x4 := x4 xor x2;}
             les  di,[x2]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],ax
             les  di,[x0]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x3]
    db $66;  mov  word ptr es:[di],si
  end;
end;




{---------------------------------------------------------------------------}
procedure InvTrans(var x0,x1,x2,x3: longint);
begin
  asm
             les  di,[x0]
    db $66;  mov  ax, word ptr es:[di]
             les  di,[x1]
    db $66;  mov  bx, word ptr es:[di]
             les  di,[x2]
    db $66;  mov  cx, word ptr es:[di]
             les  di,[x3]
    db $66;  mov  dx, word ptr es:[di]

    db $66;  ror  cx,22                {x2 := RotR(x2, 22);            }
    db $66;  ror  ax,5                 {x0 := RotR(x0,  5);            }
    db $66;  mov  si,bx                {x2 := x2 xor x3 xor (x1 shl 7);}
    db $66;  shl  si,7
    db $66;  xor  cx,dx
    db $66;  xor  cx,si
    db $66;  xor  ax,dx                {x0 := x0 xor x1 xor x3;        }
    db $66;  xor  ax,bx
    db $66;  ror  dx,7                 {x3 := RotR(x3, 7);             }
    db $66;  ror  bx,1                 {x1 := RotR(x1, 1);             }
    db $66;  mov  si,ax                {x3 := x3 xor x2 xor (x0 shl 3);}
    db $66;  shl  si,3
    db $66;  xor  dx,cx
    db $66;  xor  dx,si
    db $66;  xor  bx,cx                {x1 := x1 xor x0 xor x2;        }
    db $66;  xor  bx,ax
    db $66;  ror  cx,3                 {x2 := RotR(x2,  3);            }
    db $66;  ror  ax,13                {x0 := RotR(x0, 13);            }

             les  di,[x3]
    db $66;  mov  word ptr es:[di],dx
             les  di,[x2]
    db $66;  mov  word ptr es:[di],cx
             les  di,[x1]
    db $66;  mov  word ptr es:[di],bx
             les  di,[x0]
    db $66;  mov  word ptr es:[di],ax
  end;
end;


{$else}

{$ifdef USE_BASM32}
{---------------------------------------------------------------------------}
procedure RND0(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    xor  edx,eax                {x3 := x3 xor x0; }
    mov  esi,ebx                {x4 :=        x1; }
    and  ebx,edx                {x1 := x1 and x3; }
    xor  esi,ecx                {x4 := x4 xor x2; }
    xor  ebx,eax                {x1 := x1 xor x0; }
    or   eax,edx                {x0 := x0 or  x3; }
    xor  eax,esi                {x0 := x0 xor x4; }
    xor  esi,edx                {x4 := x4 xor x3; }
    xor  edx,ecx                {x3 := x3 xor x2; }
    or   ecx,ebx                {x2 := x2 or  x1; }
    xor  ecx,esi                {x2 := x2 xor x4; }
    not  esi                    {x4 :=    not x4; }
    or   esi,ebx                {x4 := x4 or  x1; }
    xor  ebx,edx                {x1 := x1 xor x3; }
    xor  ebx,esi                {x1 := x1 xor x4; }
    or   edx,eax                {x3 := x3 or  x0; }
    xor  ebx,edx                {x1 := x1 xor x3; }
    xor  edx,esi                {x3 := x3 xor x4; }

    mov  esi,[x0]
    mov  [esi],ebx
    mov  esi,[x1]
    mov  [esi],edx
    mov  esi,[x2]
    mov  [esi],ecx
    mov  esi,[x3]
    mov  [esi],eax
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND1(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    not  eax                    {x0 := not x0;    }
    not  ecx                    {x2 := not x2;    }
    mov  esi,eax                {x4 := x0;        }
    and  eax,ebx                {x0 := x0 and x1; }
    xor  ecx,eax                {x2 := x2 xor x0; }
    or   eax,edx                {x0 := x0 or  x3; }
    xor  edx,ecx                {x3 := x3 xor x2; }
    xor  ebx,eax                {x1 := x1 xor x0; }
    xor  eax,esi                {x0 := x0 xor x4; }
    or   esi,ebx                {x4 := x4 or  x1; }
    xor  ebx,edx                {x1 := x1 xor x3; }
    or   ecx,eax                {x2 := x2 or  x0; }
    and  ecx,esi                {x2 := x2 and x4; }
    xor  eax,ebx                {x0 := x0 xor x1; }
    and  ebx,ecx                {x1 := x1 and x2; }
    xor  ebx,eax                {x1 := x1 xor x0; }
    and  eax,ecx                {x0 := x0 and x2; }
    xor  eax,esi                {x0 := x0 xor x4; }

    mov  esi,[x0]
    mov  [esi],ecx
    mov  esi,[x1]
    mov  [esi],eax
    mov  esi,[x2]
    mov  [esi],edx
    mov  esi,[x3]
    mov  [esi],ebx
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND2(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    mov  esi,eax                {x4 := x0;         }
    and  eax,ecx                {x0 := x0 and x2;  }
    xor  eax,edx                {x0 := x0 xor x3;  }
    xor  ecx,ebx                {x2 := x2 xor x1;  }
    xor  ecx,eax                {x2 := x2 xor x0;  }
    or   edx,esi                {x3 := x3 or  x4;  }
    xor  edx,ebx                {x3 := x3 xor x1;  }
    xor  esi,ecx                {x4 := x4 xor x2;  }
    mov  ebx,edx                {x1 := x3;         }
    or   edx,esi                {x3 := x3 or  x4;  }
    xor  edx,eax                {x3 := x3 xor x0;  }
    and  eax,ebx                {x0 := x0 and x1;  }
    xor  esi,eax                {x4 := x4 xor x0;  }
    xor  ebx,edx                {x1 := x1 xor x3;  }
    xor  ebx,esi                {x1 := x1 xor x4;  }
    not  esi                    {x4 := not x4;     }

    mov  eax,esi

    mov  esi,[x0]
    mov  [esi],ecx
    mov  esi,[x1]
    mov  [esi],edx
    mov  esi,[x2]
    mov  [esi],ebx
    mov  esi,[x3]
    mov  [esi],eax
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND3(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    mov  esi,eax                {x4 := x0;       }
    or   eax,edx                {x0 := x0 or  x3;}
    xor  edx,ebx                {x3 := x3 xor x1;}
    and  ebx,esi                {x1 := x1 and x4;}
    xor  esi,ecx                {x4 := x4 xor x2;}
    xor  ecx,edx                {x2 := x2 xor x3;}
    and  edx,eax                {x3 := x3 and x0;}
    or   esi,ebx                {x4 := x4 or  x1;}
    xor  edx,esi                {x3 := x3 xor x4;}
    xor  eax,ebx                {x0 := x0 xor x1;}
    and  esi,eax                {x4 := x4 and x0;}
    xor  ebx,edx                {x1 := x1 xor x3;}
    xor  esi,ecx                {x4 := x4 xor x2;}
    or   ebx,eax                {x1 := x1 or  x0;}
    xor  ebx,ecx                {x1 := x1 xor x2;}
    xor  eax,edx                {x0 := x0 xor x3;}
    mov  ecx,ebx                {x2 := x1;       }
    or   ebx,edx                {x1 := x1 or  x3;}
    xor  ebx,eax                {x1 := x1 xor x0;}

    mov  eax,esi

    mov  esi,[x0]
    mov  [esi],ebx
    mov  esi,[x1]
    mov  [esi],ecx
    mov  esi,[x2]
    mov  [esi],edx
    mov  esi,[x3]
    mov  [esi],eax
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND4(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    xor  ebx,edx                {x1 := x1 xor x3;}
    not  edx                    {x3 :=    not x3;}
    xor  ecx,edx                {x2 := x2 xor x3;}
    xor  edx,eax                {x3 := x3 xor x0;}
    mov  esi,ebx                {x4 :=        x1;}
    and  ebx,edx                {x1 := x1 and x3;}
    xor  ebx,ecx                {x1 := x1 xor x2;}
    xor  esi,edx                {x4 := x4 xor x3;}
    xor  eax,esi                {x0 := x0 xor x4;}
    and  ecx,esi                {x2 := x2 and x4;}
    xor  ecx,eax                {x2 := x2 xor x0;}
    and  eax,ebx                {x0 := x0 and x1;}
    xor  edx,eax                {x3 := x3 xor x0;}
    or   esi,ebx                {x4 := x4 or  x1;}
    xor  esi,eax                {x4 := x4 xor x0;}
    or   eax,edx                {x0 := x0 or  x3;}
    xor  eax,ecx                {x0 := x0 xor x2;}
    and  ecx,edx                {x2 := x2 and x3;}
    not  eax                    {x0 :=    not x0;}
    xor  ecx,esi                {x2 := x2 xor x4;}

    mov  esi,[x0]
    mov  [esi],ebx
    mov  esi,[x1]
    mov  [esi],ecx
    mov  esi,[x2]
    mov  [esi],eax
    mov  esi,[x3]
    mov  [esi],edx
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND5(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    xor  eax,ebx                {x0 := x0 xor x1;}
    xor  ebx,edx                {x1 := x1 xor x3;}
    not  edx                    {x3 :=    not x3;}
    mov  esi,ebx                {x4 :=        x1;}
    and  ebx,eax                {x1 := x1 and x0;}
    xor  ecx,edx                {x2 := x2 xor x3;}
    xor  ebx,ecx                {x1 := x1 xor x2;}
    or   ecx,esi                {x2 := x2 or  x4;}
    xor  esi,edx                {x4 := x4 xor x3;}
    and  edx,ebx                {x3 := x3 and x1;}
    xor  edx,eax                {x3 := x3 xor x0;}
    xor  esi,ebx                {x4 := x4 xor x1;}
    xor  esi,ecx                {x4 := x4 xor x2;}
    xor  ecx,eax                {x2 := x2 xor x0;}
    and  eax,edx                {x0 := x0 and x3;}
    not  ecx                    {x2 :=    not x2;}
    xor  eax,esi                {x0 := x0 xor x4;}
    or   esi,edx                {x4 := x4 or  x3;}
    xor  ecx,esi                {x2 := x2 xor x4;}

    mov  esi,[x0]
    mov  [esi],ebx
    mov  esi,[x1]
    mov  [esi],edx
    mov  esi,[x2]
    mov  [esi],eax
    mov  esi,[x3]
    mov  [esi],ecx
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND6(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    not  ecx                    {x2 :=    not x2;}
    mov  esi,edx                {x4 :=        x3;}
    and  edx,eax                {x3 := x3 and x0;}
    xor  eax,esi                {x0 := x0 xor x4;}
    xor  edx,ecx                {x3 := x3 xor x2;}
    or   ecx,esi                {x2 := x2 or  x4;}
    xor  ebx,edx                {x1 := x1 xor x3;}
    xor  ecx,eax                {x2 := x2 xor x0;}
    or   eax,ebx                {x0 := x0 or  x1;}
    xor  ecx,ebx                {x2 := x2 xor x1;}
    xor  esi,eax                {x4 := x4 xor x0;}
    or   eax,edx                {x0 := x0 or  x3;}
    xor  eax,ecx                {x0 := x0 xor x2;}
    xor  esi,edx                {x4 := x4 xor x3;}
    xor  esi,eax                {x4 := x4 xor x0;}
    not  edx                    {x3 :=    not x3;}
    and  ecx,esi                {x2 := x2 and x4;}
    xor  ecx,edx                {x2 := x2 xor x3;}

    mov  edx,esi

    mov  esi,[x0]
    mov  [esi],eax
    mov  esi,[x1]
    mov  [esi],ebx
    mov  esi,[x2]
    mov  [esi],edx
    mov  esi,[x3]
    mov  [esi],ecx
    pop  esi
    pop  ebx
  end;
end;


{---------------------------------------------------------------------------}
procedure RND7(var x0,x1,x2,x3: longint);
begin
  asm
    push ebx
    push esi
    mov  esi,[x0]
    mov  eax,[esi]
    mov  esi,[x1]
    mov  ebx,[esi]
    mov  esi,[x2]
    mov  ecx,[esi]
    mov  esi,[x3]
    mov  edx,[esi]

    mov  esi,ebx                {x4 :=        x1;}
    or   ebx,ecx                {x1 := x1 or  x2;}
    xor  ebx,edx                {x1 := x1 xor x3;}
    xor  esi,ecx                {x4 := x4 xor x2;}
    xor  ecx,ebx                {x2 := x2 xor x1;}
    or   edx,esi                {x3 := x3 or  x4;}
    and  edx,eax                {x3 := x3 and x0;}
    xor  esi,ecx                {x4 := x4 xor x2;}
    xor  edx,ebx                {x3 := x3 xor x1;}
    or   ebx,esi                {x1 := x1 or  x4;}
    xor  ebx,eax                {x1 := x1 xor x0;}
    or   eax,esi                {x0 := x0 or  x4;}
    xor  eax,ecx                {x0 := x0 xor x2;}
    xor  ebx,esi                {x1 := x1 xor x4;}
    xor  ecx,ebx                {x2 := x2 xor x1;}
    and  ebx,eax                {x1 := x1 and x0;}
    xor  ebx,esi                {x1 := x1 xor x4;}
    not  ecx                    {x2 :=    not x2;}
    or   ecx,eax                {x2 := x2 or  x0;}
    xor  ecx,esi                {x2 := x2 xor x4;}

    mov  esi,[x0]
    mov  [esi],ecx
    mov  esi,[x1]
    mov  [esi],edx
    mov  esi,[x2]
    mov  [esi],ebx
    mov  esi,[x3]
    mov  [esi],eax
    pop  esi
    pop  ebx
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure RND0(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x1  xor x2;
  t02 := x0  or  x3;
  t03 := x0  xor x1;
  t04 := t02 xor t01;
  t05 := x2  or  t04;
  t06 := x0  xor x3;
  t07 := x1  or  x2;
  t08 := x3  and t05;
  t09 := t03 and t07;
  t10 := t09 xor t08;
  t11 := t09 and t10;
  t12 := x2  xor x3;
  t13 := t07 xor t11;
  t14 := x1  and t06;
  t15 := t06 xor t13;
  t16 :=     not t15;
  t17 := t16 xor t14;
  t18 := t12 xor t17;
  x0  := t16;
  x1  := t18;
  x2  := t10;
  x3  := t04;
end;


{---------------------------------------------------------------------------}
procedure RND1(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  or  x3;
  t02 := x2  xor x3;
  t03 :=     not x1;
  t04 := x0  xor x2;
  t05 := x0  or  t03;
  t06 := x3  and t04;
  t07 := t01 and t02;
  t08 := x1  or  t06;
  t09 := t02 xor t05;
  t10 := t07 xor t08;
  t11 := t01 xor t10;
  t12 := t09 xor t11;
  t13 := x1  and x3;
  t14 :=     not t10;
  t15 := t13 xor t12;
  t16 := t10 or  t15;
  t17 := t05 and t16;
  t18 := x2  xor t17;
  x0  := t18;
  x1  := t15;
  x2  := t09;
  x3  := t14;
end;


{---------------------------------------------------------------------------}
procedure RND2(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16: longint;
begin
  t01 := x0  or  x2;
  t02 := x0  xor x1;
  t03 := x3  xor t01;
  t04 := t02 xor t03;
  t05 := x2  xor t04;
  t06 := x1  xor t05;
  t07 := x1  or  t05;
  t08 := t01 and t06;
  t09 := t03 xor t07;
  t10 := t02 or  t09;
  t11 := t10 xor t08;
  t12 := x0  or  x3;
  t13 := t09 xor t11;
  t14 := x1  xor t13;
  t15 :=     not t09;
  t16 := t12 xor t14;
  x0  := t04;
  x1  := t11;
  x2  := t16;
  x3  := t15;
end;


{---------------------------------------------------------------------------}
procedure RND3(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  xor x2;
  t02 := x0  or  x3;
  t03 := x0  and x3;
  t04 := t01 and t02;
  t05 := x1  or  t03;
  t06 := x0  and x1;
  t07 := x3  xor t04;
  t08 := x2  or  t06;
  t09 := x1  xor t07;
  t10 := x3  and t05;
  t11 := t02 xor t10;
  t12 := t08 xor t09;
  t13 := x3  or  t12;
  t14 := x0  or  t07;
  t15 := x1  and t13;
  t16 := t08 xor t11;
  t17 := t14 xor t15;
  t18 := t05 xor t04;
  x0  := t17;
  x1  := t18;
  x2  := t16;
  x3  := t12;
end;


{---------------------------------------------------------------------------}
procedure RND4(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  or  x1;
  t02 := x1  or  x2;
  t03 := x0  xor t02;
  t04 := x1  xor x3;
  t05 := x3  or  t03;
  t06 := x3  and t01;
  t07 := t03 xor t06;
  t08 := t07 and t04;
  t09 := t04 and t05;
  t10 := x2  xor t06;
  t11 := x1  and x2;
  t12 := t04 xor t08;
  t13 := t11 or  t03;
  t14 := t10 xor t09;
  t15 := x0  and t05;
  t16 := t11 or  t12;
  t17 := t13 xor t08;
  t18 := t15 xor t16;
  x0  := not t14;
  x1  := t18;
  x2  := t17;
  x3  := t07;
end;


{---------------------------------------------------------------------------}
procedure RND5(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17: longint;
begin
  t01 := x1  xor x3;
  t02 := x1  or  x3;
  t03 := x0  and t01;
  t04 := x2  xor t02;
  t05 := t03 xor t04;
  t06 :=     not t05;
  t07 := x0  xor t01;
  t08 := x3  or  t06;
  t09 := x1  or  t05;
  t10 := x3  xor t08;
  t11 := x1  or  t07;
  t12 := t03 or  t06;
  t13 := t07 or  t10;
  t14 := t01 xor t11;
  t15 := t09 xor t13;
  t16 := t07 xor t08;
  t17 := t12 xor t14;
  x0  := t06;
  x1  := t16;
  x2  := t15;
  x3  := t17;
end;


{---------------------------------------------------------------------------}
procedure RND6(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19: longint;
begin
  t01 := x0  and x3;
  t02 := x1  xor x2;
  t03 := x0  xor x3;
  t04 := t01 xor t02;
  t05 := x1  or  x2;
  t06 :=     not t04;
  t07 := t03 and t05;
  t08 := x1  and t06;
  t09 := x0  or  x2;
  t10 := t07 xor t08;
  t11 := x1  or  x3;
  t12 := x2  xor t11;
  t13 := t09 xor t10;
  t14 :=     not t13;
  t15 := t06 and t03;
  t16 := t12 xor t07;
  t17 := x0  xor x1;
  t18 := t14 xor t15;
  t19 := t17 xor t18;
  x0  := t19;
  x1  := t06;
  x2  := t14;
  x3  := t16;
end;


{---------------------------------------------------------------------------}
procedure RND7(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19: longint;
begin
  t01 := x0  and x2;
  t02 :=     not x3;
  t03 := x0  and t02;
  t04 := x1  or  t01;
  t05 := x0  and x1;
  t06 := x2  xor t04;
  t07 := t03 xor t06;
  t08 := x2  or  t07;
  t09 := x3  or  t05;
  t10 := x0  xor t08;
  t11 := t04 and t07;
  t12 := t09 xor t10;
  t13 := x1  xor t12;
  t14 := t01 xor t12;
  t15 := x2  xor t05;
  t16 := t11 or  t13;
  t17 := t02 or  t14;
  t18 := t15 xor t17;
  t19 := x0  xor t16;
  x0  := t18;
  x1  := t12;
  x2  := t19;
  x3  := t07;
end;


{---------------------------------------------------------------------------}
procedure Transform(var x0,x1,x2,x3: longint);  {$ifdef HAS_INLINE} inline; {$endif}
begin
  x0 := RotL(x0, 13);
  x2 := RotL(x2,  3);
  x1 := x1 xor x0 xor x2;
  x3 := x3 xor x2 xor (x0 shl 3);
  x1 := RotL(x1, 1);
  x3 := RotL(x3, 7);
  x0 := x0 xor x1 xor x3;
  x2 := x2 xor x3 xor (x1 shl 7);
  x0 := RotL(x0,  5);
  x2 := RotL(x2, 22);
end;


{---------------------------------------------------------------------------}
procedure InvRND0(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19: longint;
begin
  t01 := x2  xor x3;
  t02 := x0  or  x1;
  t03 := x1  or  x2;
  t04 := x2  and t01;
  t05 := t02 xor t01;
  t06 := x0  or  t04;
  t07 :=     not t05;
  t08 := x1  xor x3;
  t09 := t03 and t08;
  t10 := x3  or  t07;
  t11 := t09 xor t06;
  t12 := x0  or  t05;
  t13 := t11 xor t12;
  t14 := t03 xor t10;
  t15 := x0  xor x2;
  t16 := t14 xor t13;
  t17 := t05 and t13;
  t18 := t14 or  t17;
  t19 := t15 xor t18;
  x0  := t19;
  x1  := t11;
  x2  := t07;
  x3  := t16;
end;


{---------------------------------------------------------------------------}
procedure InvRND1(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  xor x1;
  t02 := x1  or  x3;
  t03 := x0  and x2;
  t04 := x2  xor t02;
  t05 := x0  or  t04;
  t06 := t01 and t05;
  t07 := x3  or  t03;
  t08 := x1  xor t06;
  t09 := t07 xor t06;
  t10 := t04 or  t03;
  t11 := x3  and t08;
  t12 :=     not t09;
  t13 := t10 xor t11;
  t14 := x0  or  t12;
  t15 := t06 xor t13;
  t16 := t01 xor t04;
  t17 := x2  xor t15;
  t18 := t14 xor t17;
  x0  := t18;
  x1  := t13;
  x2  := t12;
  x3  := t16;
end;


{---------------------------------------------------------------------------}
procedure InvRND2(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  xor x3;
  t02 := x2  xor x3;
  t03 := x0  and x2;
  t04 := x1  or  t02;
  t05 := t01 xor t04;
  t06 := x0  or  x2;
  t07 := x3  or  t05;
  t08 :=     not x3;
  t09 := x1  and t06;
  t10 := t08 or  t03;
  t11 := x1  and t07;
  t12 := t06 and t02;
  t13 := t09 xor t10;
  t14 := t12 xor t11;
  t15 := x2  and t13;
  t16 := t05 xor t14;
  t17 := t10 xor t15;
  t18 := t16 xor t17;
  x0  := t05;
  x1  := t14;
  x2  := t18;
  x3  := t13;
end;


{---------------------------------------------------------------------------}
procedure InvRND3(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17: longint;
begin
  t01 := x2  or  x3;
  t02 := x0  or  x3;
  t03 := x2  xor t02;
  t04 := x1  xor t02;
  t05 := x0  xor x3;
  t06 := t04 and t03;
  t07 := x1  and t01;
  t08 := t05 xor t06;
  t09 := x0  xor t03;
  t10 := t07 xor t03;
  t11 := t10 or  t05;
  t12 := t09 and t11;
  t13 := x0  and t08;
  t14 := t01 xor t05;
  t15 := x1  xor t12;
  t16 := x1  or  t13;
  t17 := t14 xor t16;
  x0  := t10;
  x1  := t15;
  x2  := t08;
  x3  := t17;
end;


{---------------------------------------------------------------------------}
procedure InvRND4(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17: longint;
begin
  t01 := x1  or  x3;
  t02 := x2  or  x3;
  t03 := x0  and t01;
  t04 := x1  xor t02;
  t05 := x2  xor x3;
  t06 :=     not t03;
  t07 := x0  and t04;
  t08 := t05 xor t07;
  t09 := t08 or  t06;
  t10 := x0  xor t07;
  t11 := t01 xor t09;
  t12 := x3  xor t04;
  t13 := x2  or  t10;
  t14 := t03 xor t12;
  t15 := x0  xor t04;
  t16 := t11 xor t13;
  t17 := t15 xor t09;
  x0  := t17;
  x1  := t08;
  x2  := t16;
  x3  := t14;
end;


{---------------------------------------------------------------------------}
procedure InvRND5(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17: longint;
begin
  t01 := x0  and x3;
  t02 := x2  xor t01;
  t03 := x0  xor x3;
  t04 := x1  and t02;
  t05 := x0  and x2;
  t06 := t03 xor t04;
  t07 := x0  and t06;
  t08 := t01 xor t06;
  t09 := x1  or  t05;
  t10 :=     not x1;
  t11 := t08 xor t09;
  t12 := t10 or  t07;
  t13 := t06 or  t11;
  t14 := t02 xor t12;
  t15 := t02 xor t13;
  t16 := x1  xor x3;
  t17 := t16 xor t15;
  x0  := t06;
  x1  := t11;
  x2  := t17;
  x3  := t14;
end;


{---------------------------------------------------------------------------}
procedure InvRND6(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19: longint;
begin
  t01 := x0  xor x2;
  t02 :=     not x2;
  t03 := x1  and t01;
  t04 := x1  or  t02;
  t05 := x3  or  t03;
  t06 := x1  xor x3;
  t07 := x0  and t04;
  t08 := x0  or  t02;
  t09 := t07 xor t05;
  t10 := t06 xor t08;
  t11 :=     not t09;
  t12 := x1  and t11;
  t13 := t01 and t05;
  t14 := t01 xor t12;
  t15 := t07 xor t13;
  t16 := x3  or  t02;
  t17 := x0  xor t10;
  t18 := t17 xor t15;
  t19 := t16 xor t14;
  x0  := t11;
  x1  := t10;
  x2  := t19;
  x3  := t18;
end;


{---------------------------------------------------------------------------}
procedure InvRND7(var x0,x1,x2,x3: longint); {$ifdef HAS_INLINE} inline; {$endif}
var
  t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,t13,t14,t15,t16,t17,t18: longint;
begin
  t01 := x0  and x1;
  t02 := x0  or  x1;
  t03 := x2  or  t01;
  t04 := x3  and t02;
  t05 := t03 xor t04;
  t06 := x1  xor t04;
  t07 := x3  xor t05;
  t08 :=     not t07;
  t09 := t06 or  t08;
  t10 := x1  xor x3;
  t11 := x0  or  x3;
  t12 := x0  xor t09;
  t13 := x2  xor t06;
  t14 := x2  and t11;
  t15 := x3  or  t12;
  t16 := t01 or  t10;
  t17 := t13 xor t15;
  t18 := t14 xor t16;
  x0  := t17;
  x1  := t12;
  x2  := t18;
  x3  := t05;
end;


{---------------------------------------------------------------------------}
procedure InvTrans(var x0,x1,x2,x3: longint);   {$ifdef HAS_INLINE} inline; {$endif}
begin
  x2 := RotR(x2, 22);
  x0 := RotR(x0,  5);
  x2 := x2 xor x3 xor (x1 shl 7);
  x0 := x0 xor x1 xor x3;
  x3 := RotR(x3, 7);
  x1 := RotR(x1, 1);
  x3 := x3 xor x2 xor (x0 shl 3);
  x1 := x1 xor x0 xor x2;
  x2 := RotR(x2,  3);
  x0 := RotR(x0, 13);
end;

{$endif} {Basm32}

{$endif} {Basm16}


{---------------------------------------------------------------------------}
procedure SP_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}
begin
  with ctx do begin
    bLen :=0;
    Flag :=0;
  end;
end;


{--------------------------------------------------------------------------}
procedure SP_SetFastInit(value: boolean);
  {-set FastInit variable}
begin
  FastInit := value;
end;


{---------------------------------------------------------------------------}
function SP_GetFastInit: boolean;
  {-Returns FastInit variable}
begin
  SP_GetFastInit := FastInit;
end;


{---------------------------------------------------------------------------}
{-------------------  K e y   s e t u p ------------------------------------}
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------}
function SP_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent round key and key-dependent sbox initialisation}
var
  i,KeyWords: integer;
  t: longint;
  K0: array[0..7] of longint;
const
  phi = longint($9E3779B9);
begin
  SP_Init := 0;
  if FastInit then begin
    {Clear only the necessary context data at init. IV and buf}
    {remain uninitialized, other fields are initialized below.}
    SP_Reset(ctx);
    {$ifdef CONST}
      ctx.IncProc := nil;
    {$else}
      {TP5-6 do not like IncProc := nil;}
      fillchar(ctx.IncProc, sizeof(ctx.IncProc), 0);
    {$endif}
  end
  else fillchar(ctx, sizeof(ctx), 0);
  if (KeyBits<>128) and (KeyBits<>192) and (KeyBits<>256) then begin
    SP_Init := SP_Err_Invalid_Key_Size;
    exit;
  end;
  KeyWords := KeyBits div 32;
  with ctx do begin
    {c.f. Wei Dai's key setup [2]}
    for i:=0 to KeyWords-1 do K0[i] := TWA8(key)[i];
    if KeyWords<8 then begin
      K0[KeyWords] := 1;
      for i:=KeyWords+1 to 7 do K0[i] := 0;
    end;
    t := K0[7];
    for i:=0 to 7 do begin
      t := RotL(K0[i] xor K0[(i+3) and 7] xor K0[(i+5) and 7] xor t xor phi xor longint(i), 11);
      K0[i] := t;
      RK[i] := t;
    end;
    for i:=8 to 131 do begin
      t := RotL(RK[i-8] xor RK[i-5] xor RK[i-3] xor t xor phi xor longint(i), 11);
      RK[i] := t;
    end;
    i := 0;
    while i<128 do begin
      RND3(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND2(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND1(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND0(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND7(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND6(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND5(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
      RND4(RK[i], RK[i+1], RK[i+2], RK[i+3]); inc(i,4);
    end;
    RND3(RK[128], RK[129], RK[130], RK[131]);
  end;
end;


{---------------------------------------------------------------------------}
{---------------  E n c r y p t  /  d e c r y p t  -------------------------}
{---------------------------------------------------------------------------}

{$ifdef USE_BASM32}

{Changes compared to EddyHawk's contribution:}

{The initial Virtual Pascal version needs &frame- to run without exceptions,}
{but the Free Pascal does not compile the local asm procedures without stack}
{frames. Therefore all procedures from EddyHawk are moved to asm blocks.    }


{---------------------------------------------------------------------------}
procedure SP_Encrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
  {-encrypt one block (in ECB mode)}
var
  x0,x1,x2,x3: longint;
  j: integer;
begin
  x0 := TWA4(BI)[0];
  x1 := TWA4(BI)[1];
  x2 := TWA4(BI)[2];
  x3 := TWA4(BI)[3];

  j := 0;
  with ctx do begin
    repeat
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND0T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        xor  edx,eax                {x3 := x3 xor x0; }
        mov  esi,ebx                {x4 :=        x1; }
        and  ebx,edx                {x1 := x1 and x3; }
        xor  esi,ecx                {x4 := x4 xor x2; }
        xor  ebx,eax                {x1 := x1 xor x0; }
        or   eax,edx                {x0 := x0 or  x3; }
        xor  eax,esi                {x0 := x0 xor x4; }
        xor  esi,edx                {x4 := x4 xor x3; }
        xor  edx,ecx                {x3 := x3 xor x2; }
        or   ecx,ebx                {x2 := x2 or  x1; }
        xor  ecx,esi                {x2 := x2 xor x4; }
        not  esi                    {x4 :=    not x4; }
        or   esi,ebx                {x4 := x4 or  x1; }
        xor  ebx,edx                {x1 := x1 xor x3; }
        xor  ebx,esi                {x1 := x1 xor x4; }
        or   edx,eax                {x3 := x3 or  x0; }
        xor  ebx,edx                {x1 := x1 xor x3; }
        xor  esi,edx                {x4 := x4 xor x3; }

        mov  edx,eax                {x3 := x0;}
        mov  eax,ebx                {x0 := x1;}
        mov  ebx,esi                {x1 := x4;}
       {mov  [c],ecx}               {x2 := x2;}

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND1T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        not  eax                    {x0 := not x0;    }
        not  ecx                    {x2 := not x2;    }
        mov  esi,eax                {x4 := x0;        }
        and  eax,ebx                {x0 := x0 and x1; }
        xor  ecx,eax                {x2 := x2 xor x0; }
        or   eax,edx                {x0 := x0 or  x3; }
        xor  edx,ecx                {x3 := x3 xor x2; }
        xor  ebx,eax                {x1 := x1 xor x0; }
        xor  eax,esi                {x0 := x0 xor x4; }
        or   esi,ebx                {x4 := x4 or  x1; }
        xor  ebx,edx                {x1 := x1 xor x3; }
        or   ecx,eax                {x2 := x2 or  x0; }
        and  ecx,esi                {x2 := x2 and x4; }
        xor  eax,ebx                {x0 := x0 xor x1; }
        and  ebx,ecx                {x1 := x1 and x2; }
        xor  ebx,eax                {x1 := x1 xor x0; }
        and  eax,ecx                {x0 := x0 and x2; }
        xor  esi,eax                {x4 := x4 xor x0; }

        mov  eax,ecx                {x0 := x2; }
        mov  ecx,edx                {x2 := x3; }
        mov  edx,ebx                {x3 := x1; }
        mov  ebx,esi                {x1 := x4; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND2T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        mov  esi,eax                {x4 := x0;         }
        and  eax,ecx                {x0 := x0 and x2;  }
        xor  eax,edx                {x0 := x0 xor x3;  }
        xor  ecx,ebx                {x2 := x2 xor x1;  }
        xor  ecx,eax                {x2 := x2 xor x0;  }
        or   edx,esi                {x3 := x3 or  x4;  }
        xor  edx,ebx                {x3 := x3 xor x1;  }
        xor  esi,ecx                {x4 := x4 xor x2;  }
        mov  ebx,edx                {x1 := x3;         }
        or   edx,esi                {x3 := x3 or  x4;  }
        xor  edx,eax                {x3 := x3 xor x0;  }
        and  eax,ebx                {x0 := x0 and x1;  }
        xor  esi,eax                {x4 := x4 xor x0;  }
        xor  ebx,edx                {x1 := x1 xor x3;  }
        xor  ebx,esi                {x1 := x1 xor x4;  }
        not  esi                    {x4 := not x4;     }

        mov  eax,ecx                {x0 := x2; }
        mov  ecx,ebx                {x2 := x1; }
        mov  ebx,edx                {x1 := x3; }
        mov  edx,esi                {x3 := x4; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND3T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        mov  esi,eax                {x4 := x0;       }
        or   eax,edx                {x0 := x0 or  x3;}
        xor  edx,ebx                {x3 := x3 xor x1;}
        and  ebx,esi                {x1 := x1 and x4;}
        xor  esi,ecx                {x4 := x4 xor x2;}
        xor  ecx,edx                {x2 := x2 xor x3;}
        and  edx,eax                {x3 := x3 and x0;}
        or   esi,ebx                {x4 := x4 or  x1;}
        xor  edx,esi                {x3 := x3 xor x4;}
        xor  eax,ebx                {x0 := x0 xor x1;}
        and  esi,eax                {x4 := x4 and x0;}
        xor  ebx,edx                {x1 := x1 xor x3;}
        xor  esi,ecx                {x4 := x4 xor x2;}
        or   ebx,eax                {x1 := x1 or  x0;}
        xor  ebx,ecx                {x1 := x1 xor x2;}
        xor  eax,edx                {x0 := x0 xor x3;}
        mov  ecx,ebx                {x2 := x1;       }
        or   ebx,edx                {x1 := x1 or  x3;}
        xor  eax,ebx                {x0 := x0 xor x1;}

        mov  ebx,ecx                {x1 := x2; }
        mov  ecx,edx                {x2 := x3; }
        mov  edx,esi                {x3 := x4; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
       end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND4T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        xor  ebx,edx                {x1 := x1 xor x3;}
        not  edx                    {x3 :=    not x3;}
        xor  ecx,edx                {x2 := x2 xor x3;}
        xor  edx,eax                {x3 := x3 xor x0;}
        mov  esi,ebx                {x4 :=        x1;}
        and  ebx,edx                {x1 := x1 and x3;}
        xor  ebx,ecx                {x1 := x1 xor x2;}
        xor  esi,edx                {x4 := x4 xor x3;}
        xor  eax,esi                {x0 := x0 xor x4;}
        and  ecx,esi                {x2 := x2 and x4;}
        xor  ecx,eax                {x2 := x2 xor x0;}
        and  eax,ebx                {x0 := x0 and x1;}
        xor  edx,eax                {x3 := x3 xor x0;}
        or   esi,ebx                {x4 := x4 or  x1;}
        xor  esi,eax                {x4 := x4 xor x0;}
        or   eax,edx                {x0 := x0 or  x3;}
        xor  eax,ecx                {x0 := x0 xor x2;}
        and  ecx,edx                {x2 := x2 and x3;}
        not  eax                    {x0 :=    not x0;}
        xor  esi,ecx                {x4 := x4 xor x2;}

        mov  ecx,eax                {x2 := x0; }
        mov  eax,ebx                {x0 := x1; }
        mov  ebx,esi                {x1 := x4; }
       {mov  [d],edx}               {x3 := x3; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND5T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        xor  eax,ebx                {x0 := x0 xor x1;}
        xor  ebx,edx                {x1 := x1 xor x3;}
        not  edx                    {x3 :=    not x3;}
        mov  esi,ebx                {x4 :=        x1;}
        and  ebx,eax                {x1 := x1 and x0;}
        xor  ecx,edx                {x2 := x2 xor x3;}
        xor  ebx,ecx                {x1 := x1 xor x2;}
        or   ecx,esi                {x2 := x2 or  x4;}
        xor  esi,edx                {x4 := x4 xor x3;}
        and  edx,ebx                {x3 := x3 and x1;}
        xor  edx,eax                {x3 := x3 xor x0;}
        xor  esi,ebx                {x4 := x4 xor x1;}
        xor  esi,ecx                {x4 := x4 xor x2;}
        xor  ecx,eax                {x2 := x2 xor x0;}
        and  eax,edx                {x0 := x0 and x3;}
        not  ecx                    {x2 :=    not x2;}
        xor  eax,esi                {x0 := x0 xor x4;}
        or   esi,edx                {x4 := x4 or  x3;}
        xor  ecx,esi                {x2 := x2 xor x4;}

        mov  esi,edx                {x4 := x3; }
        mov  edx,ecx                {x3 := x2; }
        mov  ecx,eax                {x2 := x0; }
        mov  eax,ebx                {x0 := x1; }
        mov  ebx,esi                {x1 := x4; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      {RND6T}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        not  ecx                    {x2 :=    not x2;}
        mov  esi,edx                {x4 :=        x3;}
        and  edx,eax                {x3 := x3 and x0;}
        xor  eax,esi                {x0 := x0 xor x4;}
        xor  edx,ecx                {x3 := x3 xor x2;}
        or   ecx,esi                {x2 := x2 or  x4;}
        xor  ebx,edx                {x1 := x1 xor x3;}
        xor  ecx,eax                {x2 := x2 xor x0;}
        or   eax,ebx                {x0 := x0 or  x1;}
        xor  ecx,ebx                {x2 := x2 xor x1;}
        xor  esi,eax                {x4 := x4 xor x0;}
        or   eax,edx                {x0 := x0 or  x3;}
        xor  eax,ecx                {x0 := x0 xor x2;}
        xor  esi,edx                {x4 := x4 xor x3;}
        xor  esi,eax                {x4 := x4 xor x0;}
        not  edx                    {x3 :=    not x3;}
        and  ecx,esi                {x2 := x2 and x4;}
        xor  edx,ecx                {x3 := x3 xor x2;}

       {mov  edx,ecx}               {x3 := x2; }
        mov  ecx,esi                {x2 := x4; }
       {mov  [b],ebx}               {x1 := x1; }
       {mov  [a],eax}               {x0 := x0; }

        rol  eax,13                {x0 := RotL(x0, 13);            }
        rol  ecx,3                 {x2 := RotL(x2,  3);            }
        xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,ecx
        xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
        mov  esi,eax
        shl  esi,3
        xor  edx,esi
        rol  ebx,1                 {x1 := RotL(x1, 1);             }
        rol  edx,7                 {x3 := RotL(x3, 7);             }
        xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,edx
        xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
        mov  esi,ebx
        shl  esi,7
        xor  ecx,esi
        rol  eax,5                 {x0 := RotL(x0,  5);            }
        rol  ecx,22                {x2 := RotL(x2, 22);            }

        mov  [x0],eax
        mov  [x1],ebx
        mov  [x2],ecx
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      if j<128 then begin
        {RND7T}
        asm
          push ebx
          push esi
          mov  eax,[x0]
          mov  ebx,[x1]
          mov  ecx,[x2]
          mov  edx,[x3]

          mov  esi,ebx                {x4 :=        x1;}
          or   ebx,ecx                {x1 := x1 or  x2;}
          xor  ebx,edx                {x1 := x1 xor x3;}
          xor  esi,ecx                {x4 := x4 xor x2;}
          xor  ecx,ebx                {x2 := x2 xor x1;}
          or   edx,esi                {x3 := x3 or  x4;}
          and  edx,eax                {x3 := x3 and x0;}
          xor  esi,ecx                {x4 := x4 xor x2;}
          xor  edx,ebx                {x3 := x3 xor x1;}
          or   ebx,esi                {x1 := x1 or  x4;}
          xor  ebx,eax                {x1 := x1 xor x0;}
          or   eax,esi                {x0 := x0 or  x4;}
          xor  eax,ecx                {x0 := x0 xor x2;}
          xor  ebx,esi                {x1 := x1 xor x4;}
          xor  ecx,ebx                {x2 := x2 xor x1;}
          and  ebx,eax                {x1 := x1 and x0;}
          xor  ebx,esi                {x1 := x1 xor x4;}
          not  ecx                    {x2 :=    not x2;}
          or   ecx,eax                {x2 := x2 or  x0;}
          xor  esi,ecx                {x4 := x4 xor x2;}

          mov  ecx,ebx                {x2 := x1; }
          mov  ebx,edx                {x1 := x3; }
          mov  edx,eax                {x3 := x0; }
          mov  eax,esi                {x0 := x4; }

          rol  eax,13                {x0 := RotL(x0, 13);            }
          rol  ecx,3                 {x2 := RotL(x2,  3);            }
          xor  ebx,eax               {x1 := x1 xor x0 xor x2;        }
          xor  ebx,ecx
          xor  edx,ecx               {x3 := x3 xor x2 xor (x0 shl 3);}
          mov  esi,eax
          shl  esi,3
          xor  edx,esi
          rol  ebx,1                 {x1 := RotL(x1, 1);             }
          rol  edx,7                 {x3 := RotL(x3, 7);             }
          xor  eax,ebx               {x0 := x0 xor x1 xor x3;        }
          xor  eax,edx
          xor  ecx,edx               {x2 := x2 xor x3 xor (x1 shl 7);}
          mov  esi,ebx
          shl  esi,7
          xor  ecx,esi
          rol  eax,5                 {x0 := RotL(x0,  5);            }
          rol  ecx,22                {x2 := RotL(x2, 22);            }

          mov  [x0],eax
          mov  [x1],ebx
          mov  [x2],ecx
          mov  [x3],edx
          pop  esi
          pop  ebx
        end;
      end
      else begin
        {RND7c}
        asm
          push ebx
          push esi
          mov  eax,[x0]
          mov  ebx,[x1]
          mov  ecx,[x2]
          mov  edx,[x3]

          mov  esi,ebx                {x4 :=        x1;}
          or   ebx,ecx                {x1 := x1 or  x2;}
          xor  ebx,edx                {x1 := x1 xor x3;}
          xor  esi,ecx                {x4 := x4 xor x2;}
          xor  ecx,ebx                {x2 := x2 xor x1;}
          or   edx,esi                {x3 := x3 or  x4;}
          and  edx,eax                {x3 := x3 and x0;}
          xor  esi,ecx                {x4 := x4 xor x2;}
          xor  edx,ebx                {x3 := x3 xor x1;}
          or   ebx,esi                {x1 := x1 or  x4;}
          xor  ebx,eax                {x1 := x1 xor x0;}
          or   eax,esi                {x0 := x0 or  x4;}
          xor  eax,ecx                {x0 := x0 xor x2;}
          xor  ebx,esi                {x1 := x1 xor x4;}
          xor  ecx,ebx                {x2 := x2 xor x1;}
          and  ebx,eax                {x1 := x1 and x0;}
          xor  ebx,esi                {x1 := x1 xor x4;}
          not  ecx                    {x2 :=    not x2;}
          or   ecx,eax                {x2 := x2 or  x0;}
          xor  esi,ecx                {x4 := x4 xor x2;}

          mov  [x2],ebx
          mov  [x1],edx
          mov  [x3],eax
          mov  [x0],esi
          pop  esi
          pop  ebx
        end;
      end;
    until j>=128;
    TWA4(BO)[0] := x0 xor RK[j];
    TWA4(BO)[1] := x1 xor RK[j+1];
    TWA4(BO)[2] := x2 xor RK[j+2];
    TWA4(BO)[3] := x3 xor RK[j+3];
  end;
end;


{---------------------------------------------------------------------------}
procedure SP_Decrypt(var ctx: TSPContext; {$ifdef CONST} const {$else} var {$endif}  BI: TSPBlock; var BO: TSPBlock);
  {-decrypt one block (in ECB mode)}
var
  x0,x1,x2,x3: longint;
  j: integer;
begin
  j := 128;
  with ctx do begin
    repeat
      if j<128 then begin
        {InvTRND7}
        asm
          push ebx
          push esi
          mov  eax,[x0]
          mov  ebx,[x1]
          mov  ecx,[x2]
          mov  edx,[x3]

          ror  ecx,22                {x2 := RotR(x2, 22);            }
          ror  eax,5                 {x0 := RotR(x0,  5);            }
          mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
          shl  esi,7
          xor  ecx,edx
          xor  ecx,esi
          xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
          xor  eax,ebx
          ror  edx,7                 {x3 := RotR(x3, 7);             }
          ror  ebx,1                 {x1 := RotR(x1, 1);             }
          mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
          shl  esi,3
          xor  edx,ecx
          xor  edx,esi
          xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
          xor  ebx,eax
          ror  ecx,3                 {x2 := RotR(x2,  3);            }
          ror  eax,13                {x0 := RotR(x0, 13);            }

          mov  esi,ecx               {x4 :=        x2;}
          xor  ecx,eax               {x2 := x2 xor x0;}
          and  eax,edx               {x0 := x0 and x3;}
          or   esi,edx               {x4 := x4 or  x3;}
          not  ecx                   {x2 :=    not x2;}
          xor  edx,ebx               {x3 := x3 xor x1;}
          or   ebx,eax               {x1 := x1 or  x0;}
          xor  eax,ecx               {x0 := x0 xor x2;}
          and  ecx,esi               {x2 := x2 and x4;}
          and  edx,esi               {x3 := x3 and x4;}
          xor  ebx,ecx               {x1 := x1 xor x2;}
          xor  ecx,eax               {x2 := x2 xor x0;}
          or   eax,ecx               {x0 := x0 or  x2;}
          xor  esi,ebx               {x4 := x4 xor x1;}
          xor  eax,edx               {x0 := x0 xor x3;}
          xor  edx,esi               {x3 := x3 xor x4;}
          or   esi,eax               {x4 := x4 or  x0;}
          xor  edx,ecx               {x3 := x3 xor x2;}
          xor  esi,ecx               {x4 := x4 xor x2;}

          mov  [x2],ebx
          mov  [x1],eax
          mov  [x0],edx
          mov  [x3],esi
          pop  esi
          pop  ebx
        end;
      end
      else begin
        x0 := TWA4(BI)[0] xor RK[j];
        x1 := TWA4(BI)[1] xor RK[j+1];
        x2 := TWA4(BI)[2] xor RK[j+2];
        x3 := TWA4(BI)[3] xor RK[j+3];
        {InvRND7}
        asm
          push ebx
          push esi
          mov  eax,[x0]
          mov  ebx,[x1]
          mov  ecx,[x2]
          mov  edx,[x3]

          mov  esi,ecx               {x4 :=        x2;}
          xor  ecx,eax               {x2 := x2 xor x0;}
          and  eax,edx               {x0 := x0 and x3;}
          or   esi,edx               {x4 := x4 or  x3;}
          not  ecx                   {x2 :=    not x2;}
          xor  edx,ebx               {x3 := x3 xor x1;}
          or   ebx,eax               {x1 := x1 or  x0;}
          xor  eax,ecx               {x0 := x0 xor x2;}
          and  ecx,esi               {x2 := x2 and x4;}
          and  edx,esi               {x3 := x3 and x4;}
          xor  ebx,ecx               {x1 := x1 xor x2;}
          xor  ecx,eax               {x2 := x2 xor x0;}
          or   eax,ecx               {x0 := x0 or  x2;}
          xor  esi,ebx               {x4 := x4 xor x1;}
          xor  eax,edx               {x0 := x0 xor x3;}
          xor  edx,esi               {x3 := x3 xor x4;}
          or   esi,eax               {x4 := x4 or  x0;}
          xor  edx,ecx               {x3 := x3 xor x2;}
          xor  esi,ecx               {x4 := x4 xor x2;}

          mov  [x2],ebx
          mov  [x1],eax
          mov  [x0],edx
          mov  [x3],esi
          pop  esi
          pop  ebx
         end;
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND6}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        xor  eax,ecx               {x0 := x0 xor x2;}
        mov  esi,ecx               {x4 :=        x2;}
        and  ecx,eax               {x2 := x2 and x0;}
        xor  esi,edx               {x4 := x4 xor x3;}
        not  ecx                   {x2 :=    not x2;}
        xor  edx,ebx               {x3 := x3 xor x1;}
        xor  ecx,edx               {x2 := x2 xor x3;}
        or   esi,eax               {x4 := x4 or  x0;}
        xor  eax,ecx               {x0 := x0 xor x2;}
        xor  edx,esi               {x3 := x3 xor x4;}
        xor  esi,ebx               {x4 := x4 xor x1;}
        and  ebx,edx               {x1 := x1 and x3;}
        xor  ebx,eax               {x1 := x1 xor x0;}
        xor  eax,edx               {x0 := x0 xor x3;}
        or   eax,ecx               {x0 := x0 or  x2;}
        xor  edx,ebx               {x3 := x3 xor x1;}
        xor  esi,eax               {x4 := x4 xor x0;}

        mov  [x0],ebx
        mov  [x1],ecx
        mov  [x2],esi
        mov  [x3],edx
        pop  esi
        pop  ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND5}
      asm
        push ebx
        push esi
        mov  eax,[x0]
        mov  ebx,[x1]
        mov  ecx,[x2]
        mov  edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        not  ebx                   {x1 :=    not x1;}
        mov  esi,edx               {x4 :=        x3;}
        xor  ecx,ebx               {x2 := x2 xor x1;}
        or   edx,eax               {x3 := x3 or  x0;}
        xor  edx,ecx               {x3 := x3 xor x2;}
        or   ecx,ebx               {x2 := x2 or  x1;}
        and  ecx,eax               {x2 := x2 and x0;}
        xor  esi,edx               {x4 := x4 xor x3;}
        xor  ecx,esi               {x2 := x2 xor x4;}
        or   esi,eax               {x4 := x4 or  x0;}
        xor  esi,ebx               {x4 := x4 xor x1;}
        and  ebx,ecx               {x1 := x1 and x2;}
        xor  ebx,edx               {x1 := x1 xor x3;}
        xor  esi,ecx               {x4 := x4 xor x2;}
        and  edx,esi               {x3 := x3 and x4;}
        xor  esi,ebx               {x4 := x4 xor x1;}
        xor  edx,esi               {x3 := x3 xor x4;}
        not  esi                   {x4 :=    not x4;}
        xor  edx,eax               {x3 := x3 xor x0;}

        mov  [x0],ebx
        mov  [x1],esi
        mov  [x3],ecx
        mov  [x2],edx
        pop  esi
        pop  ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND4}
      asm
        push ebx
        push esi
        mov eax,[x0]
        mov ebx,[x1]
        mov ecx,[x2]
        mov edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        mov esi,ecx                {x4 :=        x2;}
        and ecx,edx                {x2 := x2 and x3;}
        xor ecx,ebx                {x2 := x2 xor x1;}
        or  ebx,edx                {x1 := x1 or  x3;}
        and ebx,eax                {x1 := x1 and x0;}
        xor esi,ecx                {x4 := x4 xor x2;}
        xor esi,ebx                {x4 := x4 xor x1;}
        and ebx,ecx                {x1 := x1 and x2;}
        not eax                    {x0 :=    not x0;}
        xor edx,esi                {x3 := x3 xor x4;}
        xor ebx,edx                {x1 := x1 xor x3;}
        and edx,eax                {x3 := x3 and x0;}
        xor edx,ecx                {x3 := x3 xor x2;}
        xor eax,ebx                {x0 := x0 xor x1;}
        and ecx,eax                {x2 := x2 and x0;}
        xor edx,eax                {x3 := x3 xor x0;}
        xor ecx,esi                {x2 := x2 xor x4;}
        or  ecx,edx                {x2 := x2 or  x3;}
        xor edx,eax                {x3 := x3 xor x0;}
        xor ecx,ebx                {x2 := x2 xor x1;}

        mov [x1],edx
        mov [x3],esi
        mov [x2],ecx
        mov [x0],eax
        pop esi
        pop ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND3}
      asm
        push ebx
        push esi
        mov eax,[x0]
        mov ebx,[x1]
        mov ecx,[x2]
        mov edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        mov esi,ecx                {x4 :=        x2;}
        xor ecx,ebx                {x2 := x2 xor x1;}
        xor eax,ecx                {x0 := x0 xor x2;}
        and esi,ecx                {x4 := x4 and x2;}
        xor esi,eax                {x4 := x4 xor x0;}
        and eax,ebx                {x0 := x0 and x1;}
        xor ebx,edx                {x1 := x1 xor x3;}
        or  edx,esi                {x3 := x3 or  x4;}
        xor ecx,edx                {x2 := x2 xor x3;}
        xor eax,edx                {x0 := x0 xor x3;}
        xor ebx,esi                {x1 := x1 xor x4;}
        and edx,ecx                {x3 := x3 and x2;}
        xor edx,ebx                {x3 := x3 xor x1;}
        xor ebx,eax                {x1 := x1 xor x0;}
        or  ebx,ecx                {x1 := x1 or  x2;}
        xor eax,edx                {x0 := x0 xor x3;}
        xor ebx,esi                {x1 := x1 xor x4;}
        xor eax,ebx                {x0 := x0 xor x1;}

        mov [x0],ecx
        mov [x2],edx
        mov [x3],eax
        mov [x1],ebx
        pop esi
        pop ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND2}
      asm
        push ebx
        push esi
        mov eax,[x0]
        mov ebx,[x1]
        mov ecx,[x2]
        mov edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        xor ecx,edx                {x2 := x2 xor x3;}
        xor edx,eax                {x3 := x3 xor x0;}
        mov esi,edx                {x4 :=        x3;}
        and edx,ecx                {x3 := x3 and x2;}
        xor edx,ebx                {x3 := x3 xor x1;}
        or  ebx,ecx                {x1 := x1 or  x2;}
        xor ebx,esi                {x1 := x1 xor x4;}
        and esi,edx                {x4 := x4 and x3;}
        xor ecx,edx                {x2 := x2 xor x3;}
        and esi,eax                {x4 := x4 and x0;}
        xor esi,ecx                {x4 := x4 xor x2;}
        and ecx,ebx                {x2 := x2 and x1;}
        or  ecx,eax                {x2 := x2 or  x0;}
        not edx                    {x3 :=    not x3;}
        xor ecx,edx                {x2 := x2 xor x3;}
        xor eax,edx                {x0 := x0 xor x3;}
        and eax,ebx                {x0 := x0 and x1;}
        xor edx,esi                {x3 := x3 xor x4;}
        xor edx,eax                {x3 := x3 xor x0;}

        mov [x0],ebx
        mov [x1],esi
        mov [x2],ecx
        mov [x3],edx
        pop esi
        pop ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND1}
      asm
        push ebx
        push esi
        mov eax,[x0]
        mov ebx,[x1]
        mov ecx,[x2]
        mov edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        mov esi,ebx                {x4 :=        x1;}
        xor ebx,edx                {x1 := x1 xor x3;}
        and edx,ebx                {x3 := x3 and x1;}
        xor esi,ecx                {x4 := x4 xor x2;}
        xor edx,eax                {x3 := x3 xor x0;}
        or  eax,ebx                {x0 := x0 or  x1;}
        xor ecx,edx                {x2 := x2 xor x3;}
        xor eax,esi                {x0 := x0 xor x4;}
        or  eax,ecx                {x0 := x0 or  x2;}
        xor ebx,edx                {x1 := x1 xor x3;}
        xor eax,ebx                {x0 := x0 xor x1;}
        or  ebx,edx                {x1 := x1 or  x3;}
        xor ebx,eax                {x1 := x1 xor x0;}
        not esi                    {x4 :=    not x4;}
        xor esi,ebx                {x4 := x4 xor x1;}
        or  ebx,eax                {x1 := x1 or  x0;}
        xor ebx,eax                {x1 := x1 xor x0;}
        or  ebx,esi                {x1 := x1 or  x4;}
        xor edx,ebx                {x3 := x3 xor x1;}

        mov [x1],eax
        mov [x0],esi
        mov [x2],edx
        mov [x3],ecx
        pop esi
        pop ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      {InvTRND0}
      asm
        push ebx
        push esi
        mov eax,[x0]
        mov ebx,[x1]
        mov ecx,[x2]
        mov edx,[x3]

        ror  ecx,22                {x2 := RotR(x2, 22);            }
        ror  eax,5                 {x0 := RotR(x0,  5);            }
        mov  esi,ebx               {x2 := x2 xor x3 xor (x1 shl 7);}
        shl  esi,7
        xor  ecx,edx
        xor  ecx,esi
        xor  eax,edx               {x0 := x0 xor x1 xor x3;        }
        xor  eax,ebx
        ror  edx,7                 {x3 := RotR(x3, 7);             }
        ror  ebx,1                 {x1 := RotR(x1, 1);             }
        mov  esi,eax               {x3 := x3 xor x2 xor (x0 shl 3);}
        shl  esi,3
        xor  edx,ecx
        xor  edx,esi
        xor  ebx,ecx               {x1 := x1 xor x0 xor x2;        }
        xor  ebx,eax
        ror  ecx,3                 {x2 := RotR(x2,  3);            }
        ror  eax,13                {x0 := RotR(x0, 13);            }

        not ecx                    {x2 :=    not x2;}
        mov esi,ebx                {x4 :=        x1;}
        or  ebx,eax                {x1 := x1 or  x0;}
        not esi                    {x4 :=    not x4;}
        xor ebx,ecx                {x1 := x1 xor x2;}
        or  ecx,esi                {x2 := x2 or  x4;}
        xor ebx,edx                {x1 := x1 xor x3;}
        xor eax,esi                {x0 := x0 xor x4;}
        xor ecx,eax                {x2 := x2 xor x0;}
        and eax,edx                {x0 := x0 and x3;}
        xor esi,eax                {x4 := x4 xor x0;}
        or  eax,ebx                {x0 := x0 or  x1;}
        xor eax,ecx                {x0 := x0 xor x2;}
        xor edx,esi                {x3 := x3 xor x4;}
        xor ecx,ebx                {x2 := x2 xor x1;}
        xor edx,eax                {x3 := x3 xor x0;}
        xor edx,ebx                {x3 := x3 xor x1;}
        and ecx,edx                {x2 := x2 and x3;}
        xor esi,ecx                {x4 := x4 xor x2;}

        mov [x2],ebx
        mov [x1],esi
        mov [x0],eax
        mov [x3],edx
        pop esi
        pop ebx
      end;
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

    until j<=0;
  end;
  TWA4(BO)[0] := x0;
  TWA4(BO)[1] := x1;
  TWA4(BO)[2] := x2;
  TWA4(BO)[3] := x3;
end;

{$else}


{---------------------------------------------------------------------------}
procedure SP_Encrypt(var ctx: TSPContext; {$ifdef CONST} const {$else} var {$endif} BI: TSPBlock; var BO: TSPBlock);
  {-encrypt one block (in ECB mode)}
var
  x0,x1,x2,x3: longint;
  j: integer;
begin
  x0 := TWA4(BI)[0];
  x1 := TWA4(BI)[1];
  x2 := TWA4(BI)[2];
  x3 := TWA4(BI)[3];

  j := 0;
  with ctx do begin
    repeat

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND0(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND1(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND2(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND3(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND4(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND5(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND6(x0,x1,x2,x3);
      Transform(x0,x1,x2,x3);

      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];
      inc(j,4);
      RND7(x0,x1,x2,x3);
      if j<128 then Transform(x0,x1,x2,x3);
    until j>=128;
    TWA4(BO)[0] := x0 xor RK[j];
    TWA4(BO)[1] := x1 xor RK[j+1];
    TWA4(BO)[2] := x2 xor RK[j+2];
    TWA4(BO)[3] := x3 xor RK[j+3];
  end;
end;



{---------------------------------------------------------------------------}
procedure SP_Decrypt(var ctx: TSPContext; {$ifdef CONST} const {$else} var {$endif}  BI: TSPBlock; var BO: TSPBlock);
  {-decrypt one block (in ECB mode)}
var
  x0,x1,x2,x3: longint;
  j: integer;
begin
  j := 128;
  with ctx do begin
    repeat
      if j=128 then begin
        x0 := TWA4(BI)[0] xor RK[j];
        x1 := TWA4(BI)[1] xor RK[j+1];
        x2 := TWA4(BI)[2] xor RK[j+2];
        x3 := TWA4(BI)[3] xor RK[j+3];
      end
      else InvTrans(x0,x1,x2,x3);
      InvRND7(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND6(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND5(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND4(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND3(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND2(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND1(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

      InvTrans(x0,x1,x2,x3);
      InvRND0(x0,x1,x2,x3);
      dec(j,4);
      x0 := x0 xor RK[j];
      x1 := x1 xor RK[j+1];
      x2 := x2 xor RK[j+2];
      x3 := x3 xor RK[j+3];

    until j<=0;
  end;
  TWA4(BO)[0] := x0;
  TWA4(BO)[1] := x1;
  TWA4(BO)[2] := x2;
  TWA4(BO)[3] := x3;
end;

{$endif}

end.
