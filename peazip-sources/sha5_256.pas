unit SHA5_256;

{SHA512/256 - 256 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA512/256 - 256 bit Secure Hash Function

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
 0.10     14.02.11  W.Ehrhardt  Initial BP version using SHA384 layout
 0.11     15.02.11  we          Updated OID
 0.12     16.02.11  we          function pSHA5_256_Desc
 0.13     08.03.12  we          RegisterHash(_SHA512_256...)
 0.14     08.03.12  we          Changed HName to 'SHA512/256', updates URLs
 0.15     08.03.12  we          removed pSHA5_256_Desc
 0.16     16.08.15  we          Removed $ifdef DLL / stdcall
 0.17     15.05.17  we          adjust OID to new MaxOIDLen
 0.18     29.11.17  we          SHA5_256File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2011-2017 Wolfgang Ehrhardt

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


procedure SHA5_256Init(var Context: THashContext);
  {-initialize context}

procedure SHA5_256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA5_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA5_256Final(var Context: THashContext; var Digest: TSHA5_256Digest);
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_256FinalBits(var Context: THashContext; var Digest: TSHA5_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_256SelfTest: boolean;
  {-self test for string from SHA512/256 document}

procedure SHA5_256Full(var Digest: TSHA5_256Digest; Msg: pointer; Len: word);
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256FullXL(var Digest: TSHA5_256Digest; Msg: pointer; Len: longint);
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA5_256Digest; var buf; bsize: word; var Err: word);
  {-SHA512/256 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA5_256_BlockLen = 128;

{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.6}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA512/256(6)}

const
  SHA5_256_OID : TOID_Vec = (2,16,840,1,101,3,4,2,6,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA5_256_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA5_256_BlockLen;
               HDigestlen: sizeof(TSHA5_256Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA5_256Init;
               HFinal    : @SHA5_256FinalEx;
               HUpdateXL : @SHA5_256UpdateXL;
             {$else}
               HInit     : SHA5_256Init;
               HFinal    : SHA5_256FinalEx;
               HUpdateXL : SHA5_256UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA512_256);
               HName     : 'SHA512/256';
               HPtrOID   : @SHA5_256_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA5_256FinalBitsEx;
             {$else}
               HFinalBit : SHA5_256FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA5_256_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA5_256Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: THashState = ($fc2bf72c,$22312194,$c84c64c2,$9f555fa3,
                     $6f53b151,$2393b86b,$5940eabd,$96387719,
                     $a88effe3,$96283ee2,$53863992,$be5e1e25,
                     $2c85b8aa,$2b0199fc,$81c52ca2,$0eb72ddc);
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
procedure SHA5_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA512FinalBitsEx(Context, Digest, BData, bitlen);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256FinalBits(var Context: THashContext; var Digest: TSHA5_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA5_256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/256 calculation, clear context}
begin
  SHA512FinalBitsEx(Context, Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256Final(var Context: THashContext; var Digest: TSHA5_256Digest);
  {-finalize SHA512/256 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA5_256SelfTest: boolean;
  {-self test for string from SHA512/256 document}
const
  s1: string[3] = 'abc';
  s2: string[112] = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'
                   +'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';

  D1: TSHA5_256Digest = ($53,$04,$8e,$26,$81,$94,$1e,$f9,
                         $9b,$2e,$29,$b7,$6b,$4c,$7d,$ab,
                         $e4,$c2,$d0,$c6,$34,$fc,$6d,$46,
                         $e0,$e2,$f1,$31,$07,$e7,$af,$23);

  D2: TSHA5_256Digest = ($39,$28,$e1,$84,$fb,$86,$90,$f8,
                         $40,$da,$39,$88,$12,$1d,$31,$be,
                         $65,$cb,$9d,$3e,$f8,$3e,$e6,$14,
                         $6f,$ea,$c8,$61,$e1,$9b,$56,$3a);

var
  Context: THashContext;
  Digest : TSHA5_256Digest;

  function SingleTest(s: Str127; TDig: TSHA5_256Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA5_256Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA5_256_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA5_256Init(Context);
    for i:=1 to length(s) do SHA5_256Update(Context,@s[i],1);
    SHA5_256Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA5_256_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  {strings from SHA512/256 document}
  SHA5_256SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA5_256FullXL(var Digest: TSHA5_256Digest; Msg: pointer; Len: longint);
  {-SHA512/256 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA5_256Init(Context);
  SHA5_256UpdateXL(Context, Msg, Len);
  SHA5_256Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256Full(var Digest: TSHA5_256Digest; Msg: pointer; Len: word);
  {-SHA512/256 of Msg with init/update/final}
begin
  SHA5_256FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA5_256Digest; var buf; bsize: word; var Err: word);
  {-SHA512/256 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA5_256_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA5_256_Desc, sizeof(SHA5_256_Desc), 0);
    with SHA5_256_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA5_256_BlockLen;
       HDigestlen:= sizeof(TSHA5_256Digest);
       HInit     := SHA5_256Init;
       HFinal    := SHA5_256FinalEx;
       HUpdateXL := SHA5_256UpdateXL;
       HAlgNum   := longint(_SHA512_256);
       HName     := 'SHA512/256';
       HPtrOID   := @SHA5_256_OID;
       HLenOID   := 9;
       HFinalBit := SHA5_256FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA512_256, @SHA5_256_Desc);
end.
