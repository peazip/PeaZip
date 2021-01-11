unit AES_Decr;


(*************************************************************************

 DESCRIPTION     :  AES decrypt functions
                    (not needed for CFB/CTR/OFB mode)

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] http://csrc.nist.gov/fips/fips-197.pdf
                    [2] rijndael-alg-fst.c V2.0/3.0: Rijmen et al Aug1999/Dec2000

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.22     16.08.03  we          longint statt word32
 0.23     16.08.03  we          separate aes_decr
 0.24     16.08.03  we          new xor_block
 0.25     18.09.03  we          Static tables, GF routines from aes_base, D4+
 0.26     20.09.03  we          optimized round code, no more move/if
 0.27     21.09.03  we          with Flag, functions, error codes
 0.28     27.09.03  we          without GFMul and -tables
 0.29     27.09.03  we          FPC/go32v2
 0.30     28.09.03  we          reorder round loop: gain 1 transformation t->block
 0.31     28.09.03  we          merge last xorblock
 0.32     28.09.03  we          two rounds in each loop
 0.33     03.10.03  we          3-para encr/decr
 0.34     03.10.03  we          two local blocks if partial unroll
 0.35     03.10.03  we          BASM for BP7
 0.36     04.10.03  we          remove add di,4
 0.37     05.10.03  we          STD.INC, TP6
 0.38     05.10.03  we          TP5,TP5.5
 0.39     28.12.03  we          DPerm removed
 0.40     29.12.03  we          BASM16: Bugfix if seg(BO)<>ds, xorblock in asm
 0.41     29.12.03  we          Delphi/VP: Pointer version
 0.42     29.12.03  we          InvMixColumn with SBox,T5..T8, Bugfix
 0.43     29.12.03  we          InvMixColumn with TBA4 if not BIT32
 0.44     15.01.04  we          InvMixColumn inline
 0.45     16.01.04  we          MakeDecrKey as BIT32, BASM16, BIT16
 0.46     14.08.04  we          UseLongBox/Td4
 0.47     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.48     24.12.04  we          STD code and Td0..Td3 like [2], AES_DECR ifdefs
 0.49     24.12.04  we          New ifdef logic, move replacement code to inc
 0.50     24.12.04  we          TP5/5.5 with round key pointer
 0.51     24.12.04  we          Fully unrolled 32 bit in dec_full.inc
 0.52     24.12.04  we          BASM16: lea trick for 4*bx
 0.53     25.12.04  we          BIT32: rearrange loop for descending key access
 0.54     27.12.04  we          All: rearrange loop for descending key access
 0.55     04.03.05  we          FPC 1.9.8, STD.INC V1.10, StrictLong
 0.56     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 0.57     09.07.06  we          Compressed tables, code in INC files
 0.58     19.07.06  we          TCd_Diag
 0.59     21.11.08  we          Use __P2I from BTypes
 0.60     01.12.12  we          separate BIT64 include statements
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


uses AES_Type, AES_Base;

{$i aes_conf.inc}

{$ifdef AES_Diag}
{$ifdef AES_ComprTab}
var
  TCd_Diag: integer;   {offset of TCd table mod 15}
                       {should be 0 or 8 for optimal alignment}
{$endif}
{$endif}


{$ifdef CONST}

function AES_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, InvMixColumn(Key) for Decypt, error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_Decrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-decrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function AES_Init_Decr(var Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, InvMixColumn(Key) for Decypt, error if invalid key size}

procedure AES_Decrypt(var ctx: TAESContext; var BI: TAESBlock; var BO: TAESBlock);
  {-decrypt one block (in ECB mode)}

{$endif}


implementation

uses BTypes;

type
  PLong = ^longint;


{$ifdef AES_ComprTab}
  {$i dec_cdat.inc}
  {$ifndef BIT16}
    {$ifdef BIT64}
      {$i dec_cp16.inc}   {This version is faster for FPC260/Win7-64!!!}
    {$else}
      {$i dec_cp32.inc}
    {$endif}
  {$else}
    {$ifdef BASM16}
      {$i dec_ca16.inc}
    {$else}
      {$i dec_cp16.inc}
    {$endif}
  {$endif}
{$else}
  {$i dec_fdat.inc}
  {$ifndef BIT16}
    {$ifdef BIT64}
      {$i dec_fp16.inc}   {This version is faster for FPC260/Win7-64!!!}
    {$else}
      {$i dec_fp32.inc}
    {$endif}
  {$else}
    {$ifdef BASM16}
      {$i dec_fa16.inc}
    {$else}
      {$i dec_fp16.inc}
    {$endif}
  {$endif}
{$endif}



{---------------------------------------------------------------------------}
function AES_Init_Decr({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, InvMixColumn(Key) for decrypt, error if invalid key size}
begin
  AES_Init_Decr := AES_Init(Key, KeyBits, ctx);
  MakeDecrKey(ctx);
  ctx.Decrypt := 1;
end;


{$ifdef AES_ComprTab}
begin
  {$ifdef AES_Diag}
    TCd_Diag := __P2I(@TCd) and 15;
  {$endif}
  {$ifdef AES_Decr_DummyAlign}
    if TCdDummy<>0 then ;
  {$endif}
{$endif}


end.
