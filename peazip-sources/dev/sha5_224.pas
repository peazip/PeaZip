unit SHA5_224;

{SHA512/224 - 224 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA512/224 - 224 bit Secure Hash Function

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
 0.12     16.02.11  we          function pSHA5_224_Desc
 0.13     08.03.12  we          RegisterHash(_SHA512_224...)
 0.14     08.03.12  we          Changed HName to 'SHA512/224', updates URLs
 0.15     08.03.12  we          removed pSHA5_224_Desc
 0.16     16.08.15  we          Removed $ifdef DLL / stdcall
 0.17     15.05.17  we          adjust OID to new MaxOIDLen
 0.18     29.11.17  we          SHA5_224File - fname: string

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


procedure SHA5_224Init(var Context: THashContext);
  {-initialize context}

procedure SHA5_224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA5_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA5_224Final(var Context: THashContext; var Digest: TSHA5_224Digest);
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_224FinalBits(var Context: THashContext; var Digest: TSHA5_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_224SelfTest: boolean;
  {-self test for string from SHA512/224 document}

procedure SHA5_224Full(var Digest: TSHA5_224Digest; Msg: pointer; Len: word);
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224FullXL(var Digest: TSHA5_224Digest; Msg: pointer; Len: longint);
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA5_224Digest; var buf; bsize: word; var Err: word);
  {-SHA512/224 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA5_224_BlockLen = 128;


{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.5}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA512/224(5)}

const
  SHA5_224_OID : TOID_Vec = (2,16,840,1,101,3,4,2,5,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA5_224_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA5_224_BlockLen;
               HDigestlen: sizeof(TSHA5_224Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA5_224Init;
               HFinal    : @SHA5_224FinalEx;
               HUpdateXL : @SHA5_224UpdateXL;
             {$else}
               HInit     : SHA5_224Init;
               HFinal    : SHA5_224FinalEx;
               HUpdateXL : SHA5_224UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA512_224);
               HName     : 'SHA512/224';
               HPtrOID   : @SHA5_224_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA5_224FinalBitsEx;
             {$else}
               HFinalBit : SHA5_224FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA5_224_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA5_224Init(var Context: THashContext);
  {-initialize context}
{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}
const
  SIV: THashState = ($19544da2,$8c3d37c8,$89dcd4d6,$73e19966,
                     $32ff9c82,$1dfab7ae,$582f9fcf,$679dd514,
                     $7bd44da8,$0f6d2b69,$04c48942,$77e36f73,
                     $6a1d36c8,$3f9d85a8,$91d692a1,$1112e6ad);

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
procedure SHA5_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA512UpdateXL(THashContext(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA512FinalBitsEx(Context, Digest, BData, bitlen);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224FinalBits(var Context: THashContext; var Digest: TSHA5_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA5_224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/224 calculation, clear context}
begin
  SHA512FinalBitsEx(Context, Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224Final(var Context: THashContext; var Digest: TSHA5_224Digest);
  {-finalize SHA512/224 calculation, clear context}
var
  tmp: THashDigest;
begin
  SHA512FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function SHA5_224SelfTest: boolean;
  {-self test for string from SHA512/224 document}
const
  s1: string[3] = 'abc';
  s2: string[112] = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'
                   +'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu';

  D1: TSHA5_224Digest = ($46,$34,$27,$0f,$70,$7b,$6a,$54,
                         $da,$ae,$75,$30,$46,$08,$42,$e2,
                         $0e,$37,$ed,$26,$5c,$ee,$e9,$a4,
                         $3e,$89,$24,$aa);


  D2: TSHA5_224Digest = ($23,$fe,$c5,$bb,$94,$d6,$0b,$23,
                         $30,$81,$92,$64,$0b,$0c,$45,$33,
                         $35,$d6,$64,$73,$4f,$e4,$0e,$72,
                         $68,$67,$4a,$f9);

var
  Context: THashContext;
  Digest : TSHA5_224Digest;

  function SingleTest(s: Str127; TDig: TSHA5_224Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    SHA5_224Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@SHA5_224_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    SHA5_224Init(Context);
    for i:=1 to length(s) do SHA5_224Update(Context,@s[i],1);
    SHA5_224Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@SHA5_224_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  {strings from SHA512/224 document}
  SHA5_224SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2)
end;


{---------------------------------------------------------------------------}
procedure SHA5_224FullXL(var Digest: TSHA5_224Digest; Msg: pointer; Len: longint);
  {-SHA512/224 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA5_224Init(Context);
  SHA5_224UpdateXL(Context, Msg, Len);
  SHA5_224Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224Full(var Digest: TSHA5_224Digest; Msg: pointer; Len: word);
  {-SHA512/224 of Msg with init/update/final}
begin
  SHA5_224FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA5_224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA5_224Digest; var buf; bsize: word; var Err: word);
  {-SHA512/224 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA5_224_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA5_224_Desc, sizeof(SHA5_224_Desc), 0);
    with SHA5_224_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA5_224_BlockLen;
       HDigestlen:= sizeof(TSHA5_224Digest);
       HInit     := SHA5_224Init;
       HFinal    := SHA5_224FinalEx;
       HUpdateXL := SHA5_224UpdateXL;
       HAlgNum   := longint(_SHA512_224);
       HName     := 'SHA512/224';
       HPtrOID   := @SHA5_224_OID;
       HLenOID   := 9;
       HFinalBit := SHA5_224FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA512_224, @SHA5_224_Desc);
end.
