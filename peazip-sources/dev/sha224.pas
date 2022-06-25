unit SHA224;

{SHA224 - 224 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA224 - 224 bit Secure Hash Function

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
 0.10     02.01.04  W.Ehrhardt  Initial version
 0.11     05.03.04  we          Update fips180-2 URL
 0.12     26.02.05  we          With {$ifdef StrictLong}
 0.13     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 0.14     17.12.05  we          Force $I- in SHA224File
 0.15     15.01.06  we          uses Hash unit and THashDesc
 0.16     18.01.06  we          Descriptor fields HAlgNum, HSig
 0.17     22.01.06  we          Removed HSelfTest from descriptor
 0.18     11.02.06  we          Descriptor as typed const
 0.19     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 0.20     22.02.07  we          values for OID vector
 0.21     30.06.07  we          Use conditional define FPC_ProcVar
 0.22     02.05.08  we          Bit-API: SHA224FinalBits/Ex
 0.23     05.05.08  we          THashDesc constant with HFinalBit field
 0.24     12.11.08  we          uses BTypes and Str255
 0.25     11.03.12  we          Updated references
 0.26     16.08.15  we          Removed $ifdef DLL / stdcall
 0.27     15.05.17  we          adjust OID to new MaxOIDLen
 0.28     29.11.17  we          SHA224File - fname: string

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
  BTypes,Hash,SHA256;

procedure SHA224Init(var Context: THashContext);
  {-initialize context}

procedure SHA224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA224Final(var Context: THashContext; var Digest: TSHA224Digest);
  {-finalize SHA224 calculation, clear context}

procedure SHA224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA224 calculation, clear context}

procedure SHA224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA224FinalBits(var Context: THashContext; var Digest: TSHA224Digest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA224SelfTest: boolean;
  {-self test for string from SHA224 document}

procedure SHA224Full(var Digest: TSHA224Digest; Msg: pointer; Len: word);
  {-SHA224 of Msg with init/update/final}

procedure SHA224FullXL(var Digest: TSHA224Digest; Msg: pointer; Len: longint);
  {-SHA224 of Msg with init/update/final}

procedure SHA224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA224Digest; var buf; bsize: word; var Err: word);
  {-SHA224 of file, buf: buffer with at least bsize bytes}


implementation

const
  SHA224_BlockLen  = 64;


{2.16.840.1.101.3.4.2.4}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) sha224(4)}
const
  SHA224_OID : TOID_Vec = (2,16,840,1,101,3,4,2,4,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA224_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA224_BlockLen;
               HDigestlen: sizeof(TSHA224Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA224Init;
               HFinal    : @SHA224FinalEx;
               HUpdateXL : @SHA224UpdateXL;
             {$else}
               HInit     : SHA224Init;
               HFinal    : SHA224FinalEx;
               HUpdateXL : SHA224UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA224);
               HName     : 'SHA224';
               HPtrOID   : @SHA224_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA224FinalBitsEx;
             {$else}
               HFinalBit : SHA224FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA224_Desc: THashDesc;
{$endif}



{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA224Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: array[0..7] of longint = ($c1059ed8, $367cd507, $3070dd17, $f70e5939,
                                 $ffc00b31, $68581511, $64f98fa7, $befa4fa4);
{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
begin
  {Clear context}
  fillchar(Context,sizeof(Context),0);
  move(SIV, Context.Hash, sizeof(SIV));
end;


{---------------------------------------------------------------------------}
procedure SHA224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA256UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA224Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA256UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA224 calculation, clear context}
begin
  SHA256FinalEx(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA224Final(var Context: THashContext; var Digest: TSHA224Digest);
  {-finalize SHA224 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA256FinalEx(Context, tmp);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA256FinalBitsEx(Context, Digest, BData, bitlen);
end;


{---------------------------------------------------------------------------}
procedure SHA224FinalBits(var Context: THashContext; var Digest: TSHA224Digest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA256FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA224SelfTest: boolean;
  {-self test for string from SHA224 document}
const
  s1: string[3] = 'abc';
  s2: string[56] = 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq';

  D1: TSHA224Digest = ($23, $09, $7d, $22, $34, $05, $d8, $22,
                       $86, $42, $a4, $77, $bd, $a2, $55, $b3,
                       $2a, $ad, $bc, $e4, $bd, $a0, $b3, $f7,
                       $e3, $6c, $9d, $a7);

  D2: TSHA224Digest = ($75, $38, $8b, $16, $51, $27, $76, $cc,
                       $5d, $ba, $5d, $a1, $fd, $89, $01, $50,
                       $b0, $c6, $45, $5c, $b4, $f5, $8b, $19,
                       $52, $52, $25, $25);
  D3: TSHA224Digest = ($d3, $fe, $57, $cb, $76, $cd, $d2, $4e,
                       $9e, $b2, $3e, $7e, $15, $68, $4e, $03,
                       $9c, $75, $45, $9b, $ea, $ae, $10, $0f,
                       $89, $71, $2e, $9d);

  D4: TSHA224Digest = ($b0, $4c, $42, $3c, $90, $91, $ff, $5b,
                       $b3, $2e, $a4, $b0, $06, $3e, $98, $81,
                       $46, $33, $35, $0c, $1b, $c2, $bd, $97,
                       $4f, $77, $6f, $d2);
var
  Context: THashContext;
  Digest : TSHA224Digest;

  function SingleTest(s: Str127; TDig: TSHA224Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA224Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA224_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA224Init(Context);
    for i:=1 to length(s) do SHA224Update(Context,@s[i],1);
    SHA224Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA224_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  SHA224SelfTest := false;
  {1 Zero bit from NESSIE test vectors, currently from shatest}
  SHA224Init(Context);
  SHA224FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@SHA224_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {4 highest bits of $50, D4 calculated with program shatest from RFC 4634}
  SHA224Init(Context);
  SHA224FinalBits(Context,Digest,$50,4);
  if not HashSameDigest(@SHA224_Desc, PHashDigest(@Digest), PHashDigest(@D4)) then exit;
  {strings from SHA224 document}
  SHA224SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA224FullXL(var Digest: TSHA224Digest; Msg: pointer; Len: longint);
  {-SHA224 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA224Init(Context);
  SHA224UpdateXL(Context, Msg, Len);
  SHA224Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA224Full(var Digest: TSHA224Digest; Msg: pointer; Len: word);
  {-SHA224 of Msg with init/update/final}
begin
  SHA224FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA224Digest; var buf; bsize: word; var Err: word);
  {-SHA224 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA224_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA224_Desc, sizeof(SHA224_Desc), 0);
    with SHA224_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA224_BlockLen;
       HDigestlen:= sizeof(TSHA224Digest);
       HInit     := SHA224Init;
       HFinal    := SHA224FinalEx;
       HUpdateXL := SHA224UpdateXL;
       HAlgNum   := longint(_SHA224);
       HName     := 'SHA224';
       HPtrOID   := @SHA224_OID;
       HLenOID   := 9;
       HFinalBit := SHA224FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA224, @SHA224_Desc);
end.
