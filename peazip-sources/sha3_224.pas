unit SHA3_224;

{SHA3-224 - 224 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA3-224 - 224 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - FIPS 202 SHA-3 Standard: 'Permutation-Based Hash and
                      Extendable-Output Functions' available from
                      http://csrc.nist.gov/publications/PubsFIPS.html or
                      http://dx.doi.org/10.6028/NIST.FIPS.202 or
                      http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
                    - Test vectors and intermediate values:
                      http://csrc.nist.gov/groups/ST/toolkit/examples.html

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     10.08.15  W.Ehrhardt  Initial BP version using SHA3-256 layout
 0.11     17.08.15  we          Updated references
 0.12     15.05.17  we          adjust OID to new MaxOIDLen
 0.13     29.11.17  we          SHA3_224File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2015-2017 Wolfgang Ehrhardt

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
  BTypes,Hash,SHA3;


procedure SHA3_224Init(var Context: THashContext);
  {-initialize context}

procedure SHA3_224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA3_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA3_224Final(var Context: THashContext; var Digest: TSHA3_224Digest);
  {-finalize SHA3-224 calculation, clear context}

procedure SHA3_224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-224 calculation, clear context}

procedure SHA3_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_224FinalBits(var Context: THashContext; var Digest: TSHA3_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_224FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (LSB format), clear context}

function  SHA3_224SelfTest: boolean;
  {-self test for string from SHA3-224 documents}

procedure SHA3_224Full(var Digest: TSHA3_224Digest; Msg: pointer; Len: word);
  {-SHA3-224 of Msg with init/update/final}

procedure SHA3_224FullXL(var Digest: TSHA3_224Digest; Msg: pointer; Len: longint);
  {-SHA3-224 of Msg with init/update/final}

procedure SHA3_224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_224Digest; var buf; bsize: word; var Err: word);
  {-SHA3-224 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA3_224_BlockLen = 144;  {Rate / 8, used only for HMAC} {FIPS202, Tab.3}

{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.8}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA3-224(7)}

const
  SHA3_224_OID : TOID_Vec = (2,16,840,1,101,3,4,2,7,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA3_224_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA3_224_BlockLen;
               HDigestlen: sizeof(TSHA3_224Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA3_224Init;
               HFinal    : @SHA3_224FinalEx;
               HUpdateXL : @SHA3_224UpdateXL;
             {$else}
               HInit     : SHA3_224Init;
               HFinal    : SHA3_224FinalEx;
               HUpdateXL : SHA3_224UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA3_224);
               HName     : 'SHA3-224';
               HPtrOID   : @SHA3_224_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA3_224FinalBitsEx;
             {$else}
               HFinalBit : SHA3_224FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA3_224_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA3_224Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context}
  SHA3_LastError := SHA3_Init(TSHA3State(Context),__SHA3_224);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_224FinalBits(var Context: THashContext; var Digest: TSHA3_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_224FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-224 calculation with bitlen bits from BData (LSB format), clear context}
begin
  SHA3_LastError := SHA3_FinalBit_LSB(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-224 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224Final(var Context: THashContext; var Digest: TSHA3_224Digest);
  {-finalize SHA3-224 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
function SHA3_224SelfTest: boolean;
  {-self test for string from SHA3-224 documents}
const
  Bl1 = 0;
  dig1: TSHA3_224Digest      = ($6B,$4E,$03,$42,$36,$67,$DB,$B7,$3B,$6E,
                                $15,$45,$4F,$0E,$B1,$AB,$D4,$59,$7F,$9A,
                                $1B,$07,$8E,$3F,$5B,$5A,$6B,$C7);
  BL2 = 5;
  msg2: array[0..0] of byte  = ($13);
  dig2: TSHA3_224Digest      = ($FF,$BA,$D5,$DA,$96,$BA,$D7,$17,$89,$33,
                                $02,$06,$DC,$67,$68,$EC,$AE,$B1,$B3,$2D,
                                $CA,$6B,$33,$01,$48,$96,$74,$AB);
  BL3 = 30;
  msg3: array[0..3] of byte  = ($53,$58,$7B,$19);
  dig3: TSHA3_224Digest      = ($D6,$66,$A5,$14,$CC,$9D,$BA,$25,$AC,$1B,
                                $A6,$9E,$D3,$93,$04,$60,$DE,$AA,$C9,$85,
                                $1B,$5F,$0B,$AA,$B0,$07,$DF,$3B);

  {https://github.com/gvanas/KeccakCodePackage, SKat len=200}
  BL4 = 200;
  msg4: array[0..24] of byte = ($aa,$fd,$c9,$24,$3d,$3d,$4a,$09,
                                $65,$58,$a3,$60,$cc,$27,$c8,$d8,
                                $62,$f0,$be,$73,$db,$5e,$88,$aa,$55);
  dig4: TSHA3_224Digest      = ($23,$60,$6d,$06,$fd,$8f,$87,$c2,
                                $20,$5a,$bb,$5f,$d0,$4c,$33,$eb,
                                $a3,$05,$09,$95,$52,$00,$56,$6a,
                                $0f,$77,$2b,$49);
var
  Context: THashContext;
  Digest : TSHA3_224Digest;

  function SingleTest(Msg: pointer; BL: word; TDig: TSHA3_224Digest): boolean;
  var
    bytes: word;
  begin
    SingleTest := false;
    SHA3_224Init(Context);
    if SHA3_LastError<>0 then exit;
    if BL=0 then SHA3_224Final(Context,Digest)
    else begin
      if BL>7 then begin
        bytes := BL shr 3;
        SHA3_224Update(Context, Msg, BL shr 3);
        if SHA3_LastError<>0 then exit;
        inc(Ptr2Inc(Msg), bytes);
      end;
      SHA3_224FinalBits_LSB(Context, Digest, pByte(Msg)^, BL and 7);
    end;
    if SHA3_LastError<>0 then exit;
    SingleTest := HashSameDigest(@SHA3_224_Desc, PHashDigest(@TDig), PHashDigest(@Digest));
  end;

begin
  SHA3_224SelfTest := SingleTest(nil, BL1, dig1)   and
                      SingleTest(@msg2, BL2, dig2) and
                      SingleTest(@msg3, BL3, dig3) and
                      SingleTest(@msg4, BL4, dig4);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224FullXL(var Digest: TSHA3_224Digest; Msg: pointer; Len: longint);
  {-SHA3-224 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA3_224Init(Context);
  if SHA3_LastError=0 then SHA3_224UpdateXL(Context, Msg, Len);
  SHA3_224Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224Full(var Digest: TSHA3_224Digest; Msg: pointer; Len: word);
  {-SHA3-224 of Msg with init/update/final}
begin
  SHA3_224FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_224Digest; var buf; bsize: word; var Err: word);
  {-SHA3-224 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA3_224_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA3_224_Desc, sizeof(SHA3_224_Desc), 0);
    with SHA3_224_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA3_224_BlockLen;
       HDigestlen:= sizeof(TSHA3_224Digest);
       HInit     := SHA3_224Init;
       HFinal    := SHA3_224FinalEx;
       HUpdateXL := SHA3_224UpdateXL;
       HAlgNum   := longint(_SHA3_224);
       HName     := 'SHA3-224';
       HPtrOID   := @SHA3_224_OID;
       HLenOID   := 9;
       HFinalBit := SHA3_224FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA3_224, @SHA3_224_Desc);
end.
