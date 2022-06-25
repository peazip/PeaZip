unit SHA384;

{SHA384 - 384 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA384 - 384 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - Latest specification of Secure Hash Standard:
                      http://csrc.nist.gov/publications/fips/fips180-4/fips-180-4.pdf
                    - Test vectors and intermediate values:
                      http://csrc.nist.gov/groups/ST/toolkit/documents/Examples/SHA_All.pdf

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     21.11.02  W.Ehrhardt  Initial version
 0.11     21.11.02  we          BugFix SHA384File
 3.00     01.12.03  we          Common version 3.0
 3.01     05.03.04  we          Update fips180-2 URL
 3.02     26.02.05  we          With {$ifdef StrictLong}
 3.03     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.04     17.12.05  we          Force $I- in SHA384File
 3.05     15.01.06  we          uses Hash unit and THashDesc
 3.06     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.07     22.01.06  we          Removed HSelfTest from descriptor
 3.08     11.02.06  we          Descriptor as typed const
 3.09     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.10     22.02.07  we          values for OID vector
 3.11     30.06.07  we          Use conditional define FPC_ProcVar
 3.12     03.05.08  we          Bit-API: SHA384FinalBits/Ex
 3.13     05.05.08  we          THashDesc constant with HFinalBit field
 3.14     12.11.08  we          Uses BTypes, Ptr2Inc and/or Str255/Str127
 3.15     11.03.12  we          Updated references
 3.16     16.08.15  we          Removed $ifdef DLL / stdcall
 3.17     15.05.17  we          adjust OID to new MaxOIDLen
 3.18     29.11.17  we          SHA384File - fname: string

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
  BTypes,Hash,SHA512;


procedure SHA384Init(var Context: THashContext);
  {-initialize context}

procedure SHA384Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA384Final(var Context: THashContext; var Digest: TSHA384Digest);
  {-finalize SHA384 calculation, clear context}

procedure SHA384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA384 calculation, clear context}

procedure SHA384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA384FinalBits(var Context: THashContext; var Digest: TSHA384Digest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA384SelfTest: boolean;
  {-self test for string from SHA384 document}

procedure SHA384Full(var Digest: TSHA384Digest; Msg: pointer; Len: word);
  {-SHA384 of Msg with init/update/final}

procedure SHA384FullXL(var Digest: TSHA384Digest; Msg: pointer; Len: longint);
  {-SHA384 of Msg with init/update/final}

procedure SHA384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA384Digest; var buf; bsize: word; var Err: word);
  {-SHA384 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA384_BlockLen = 128;


{2.16.840.1.101.3.4.2.2}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) sha384(2)}

const
  SHA384_OID : TOID_Vec = (2,16,840,1,101,3,4,2,2,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA384_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA384_BlockLen;
               HDigestlen: sizeof(TSHA384Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA384Init;
               HFinal    : @SHA384FinalEx;
               HUpdateXL : @SHA384UpdateXL;
             {$else}
               HInit     : SHA384Init;
               HFinal    : SHA384FinalEx;
               HUpdateXL : SHA384UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA384);
               HName     : 'SHA384';
               HPtrOID   : @SHA384_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA384FinalBitsEx;
             {$else}
               HFinalBit : SHA384FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA384_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA384Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: THashState = ($c1059ed8, $cbbb9d5d, $367cd507, $629a292a,
                     $3070dd17, $9159015a, $f70e5939, $152fecd8,
                     $ffc00b31, $67332667, $68581511, $8eb44a87,
                     $64f98fa7, $db0c2e0d, $befa4fa4, $47b5481d);
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin
  {Clear context}
  fillchar(Context,sizeof(Context),0);
  Context.Hash := SIV;
end;


{---------------------------------------------------------------------------}
procedure SHA384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA384Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA512FinalBitsEx(Context, Digest, BData, bitlen);
end;


{---------------------------------------------------------------------------}
procedure SHA384FinalBits(var Context: THashContext; var Digest: TSHA384Digest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA384 calculation, clear context}
begin
  SHA512FinalBitsEx(Context, Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA384Final(var Context: THashContext; var Digest: TSHA384Digest);
  {-finalize SHA384 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA384SelfTest: boolean;
  {-self test for string from SHA384 document}
const
  s1: string[3] = 'abc';
  s2: string[112] = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'
                   +'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';

  D1: TSHA384Digest = ($cb, $00, $75, $3f, $45, $a3, $5e, $8b,
                       $b5, $a0, $3d, $69, $9a, $c6, $50, $07,
                       $27, $2c, $32, $ab, $0e, $de, $d1, $63,
                       $1a, $8b, $60, $5a, $43, $ff, $5b, $ed,
                       $80, $86, $07, $2b, $a1, $e7, $cc, $23,
                       $58, $ba, $ec, $a1, $34, $c8, $25, $a7);

  D2: TSHA384Digest = ($09, $33, $0c, $33, $f7, $11, $47, $e8,
                       $3d, $19, $2f, $c7, $82, $cd, $1b, $47,
                       $53, $11, $1b, $17, $3b, $3b, $05, $d2,
                       $2f, $a0, $80, $86, $e3, $b0, $f7, $12,
                       $fc, $c7, $c7, $1a, $55, $7e, $2d, $b9,
                       $66, $c3, $e9, $fa, $91, $74, $60, $39);
  D3: TSHA384Digest = ($63, $4a, $a6, $30, $38, $a1, $64, $ae,
                       $6c, $7d, $48, $b3, $19, $f2, $ac, $a0,
                       $a1, $07, $90, $8e, $54, $85, $19, $20,
                       $4c, $6d, $72, $db, $ea, $c0, $fd, $c3,
                       $c9, $24, $66, $74, $f9, $8e, $8f, $d3,
                       $02, $21, $ba, $98, $6e, $73, $7d, $61);
  D4: TSHA384Digest = ($e7, $9b, $94, $65, $32, $fa, $5c, $f7,
                       $22, $33, $ae, $1c, $bb, $a8, $6e, $21,
                       $9e, $1a, $3d, $35, $49, $a3, $44, $4f,
                       $2e, $a3, $fc, $db, $ce, $0f, $ab, $58,
                       $aa, $56, $70, $ab, $d1, $98, $ba, $a8,
                       $dc, $fb, $cb, $e9, $4e, $8e, $b8, $07);

var
  Context: THashContext;
  Digest : TSHA384Digest;

  function SingleTest(s: Str127; TDig: TSHA384Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA384Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA384_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA384Init(Context);
    for i:=1 to length(s) do SHA384Update(Context,@s[i],1);
    SHA384Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA384_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  SHA384SelfTest := false;
  {1 Zero bit from NESSIE test vectors}
  SHA384Init(Context);
  SHA384FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@SHA384_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  SHA384Init(Context);
  SHA384FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@SHA384_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from SHA384 document}
  SHA384SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA384FullXL(var Digest: TSHA384Digest; Msg: pointer; Len: longint);
  {-SHA384 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA384Init(Context);
  SHA384UpdateXL(Context, Msg, Len);
  SHA384Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA384Full(var Digest: TSHA384Digest; Msg: pointer; Len: word);
  {-SHA384 of Msg with init/update/final}
begin
  SHA384FullXL(Digest, Msg, Len);
end;



{---------------------------------------------------------------------------}
procedure SHA384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA384Digest; var buf; bsize: word; var Err: word);
  {-SHA384 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA384_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA384_Desc, sizeof(SHA384_Desc), 0);
    with SHA384_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA384_BlockLen;
       HDigestlen:= sizeof(TSHA384Digest);
       HInit     := SHA384Init;
       HFinal    := SHA384FinalEx;
       HUpdateXL := SHA384UpdateXL;
       HAlgNum   := longint(_SHA384);
       HName     := 'SHA384';
       HPtrOID   := @SHA384_OID;
       HLenOID   := 9;
       HFinalBit := SHA384FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA384, @SHA384_Desc);
end.
