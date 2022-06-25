unit AES_Encr;


(*************************************************************************

 DESCRIPTION     :  AES encrypt functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] http://csrc.nist.gov/fips/fips-197.pdf
                    [2] rijndael-alg-fst.c V2.0/3.0: Rijmen et al Aug1999/Dec2000


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.22     16.08.03  we          longint statt word32
 0.23     16.08.03  we          separate aes_encr
 0.24     16.08.03  we          new xor_block
 0.25     18.09.03  we          Static tables, D4+
 0.26     20.09.03  we          optimized round code, no more move/if
 0.27     21.09.03  we          functions, error codes
 0.28     27.09.03  we          FPC/go32v2
 0.29     28.09.03  we          removed temporary s-Block
 0.30     28.09.03  we          two rounds in each loop, merge last xorblock
 0.31     03.10.03  we          3-para encr/decr
 0.32     03.10.03  we          two local blocks if partial unroll
 0.33     03.10.03  we          BASM for BP7
 0.34     04.10.03  we          remove add di,4
 0.35     05.10.03  we          STD.INC, TP6
 0.36     05.10.03  we          TP5,TP5.5
 0.37     27.12.03  we          EPerm removed
 0.38     28.12.03  we          Delphi/VP: Pointer version
                                BASM16: changed variable order
 0.39     28.12.03  we          BASM16: SBox code in asm,
                                PTR: merge SBox code with XOR RK
 0.40     29.12.03  we          BASM16: xorblock in asm, PTR: reorder
 0.41     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.42     14.08.04  we          UseLongBox/Te4
 0.43     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.44     24.12.04  we          STD code and Te0..Te3 like [2], AES_ENCR ifdefs
 0.45     24.12.04  we          New ifdef logic, move replacement code to inc
 0.46     24.12.04  we          TP5/5.5 with round key pointer
 0.47     24.12.04  we          BASM16: lea trick for 4*bx
 0.48     04.03.05  we          FPC 1.9.8, STD.INC V1.10, StrictLong
 0.49     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 0.50     09.07.06  we          Compressed tables, code in INC files
 0.51     19.07.06  we          TCe_Diag
 0.52     21.11.08  we          Use __P2I from BTypes
 0.53     01.12.12  we          separate BIT64 include statements
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


uses
  AES_Type, AES_Base;

{$i aes_conf.inc}

{$ifdef AES_Diag}
{$ifdef AES_ComprTab}
var
  TCe_Diag: integer;   {offset of TCe table mod 15}
                       {should be 0 or 8 for optimal alignment}

{$endif}
{$endif}



{$ifdef CONST}

function  AES_Init_Encr(const  Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  AES_Init_Encr(var Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}

procedure AES_Encrypt(var ctx: TAESContext; var BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}

{$endif}


implementation

uses BTypes;

{$ifdef AES_ComprTab}
  {$i enc_cdat.inc}
  {$ifndef BIT16}
    {$ifdef BIT64}
      {$i enc_cp16.inc}   {This version is faster for FPC260/Win7-64!!!}
    {$else}
      {$i enc_cp32.inc}
    {$endif}
  {$else}
    {$ifdef BASM16}
      {$i enc_ca16.inc}
    {$else}
      {$i enc_cp16.inc}
    {$endif}
  {$endif}
{$else}
  {$i enc_fdat.inc}
  {$ifndef BIT16}
    {$ifdef BIT64}
      {$i enc_fp16.inc}   {This version is faster for FPC260/Win7-64!!!}
    {$else}
      {$i enc_fp32.inc}
    {$endif}
  {$else}
    {$ifdef BASM16}
      {$i enc_fa16.inc}
    {$else}
      {$i enc_fp16.inc}
    {$endif}
  {$endif}
{$endif}


{---------------------------------------------------------------------------}
function AES_Init_Encr({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
begin
  AES_Init_Encr := AES_Init(Key, KeyBits, ctx);
end;



{$ifdef AES_ComprTab}
begin
  {$ifdef AES_Diag}
    TCe_Diag := __P2I(@TCe) and 15;
  {$endif}
  {$ifdef AES_Encr_DummyAlign}
    if TCeDummy<>0 then ;
  {$endif}
{$endif}

end.
