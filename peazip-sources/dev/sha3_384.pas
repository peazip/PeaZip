unit SHA3_384;

{SHA3-384 - 384 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA3-384 - 384 bit Secure Hash Function

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
 0.13     29.11.17  we          SHA3_384File - fname: string

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


procedure SHA3_384Init(var Context: THashContext);
  {-initialize context}

procedure SHA3_384Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA3_384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA3_384Final(var Context: THashContext; var Digest: TSHA3_384Digest);
  {-finalize SHA3-384 calculation, clear context}

procedure SHA3_384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-384 calculation, clear context}

procedure SHA3_384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_384FinalBits(var Context: THashContext; var Digest: TSHA3_384Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_384FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_384Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (LSB format), clear context}

function  SHA3_384SelfTest: boolean;
  {-self test for string from SHA3-384 documents}

procedure SHA3_384Full(var Digest: TSHA3_384Digest; Msg: pointer; Len: word);
  {-SHA3-384 of Msg with init/update/final}

procedure SHA3_384FullXL(var Digest: TSHA3_384Digest; Msg: pointer; Len: longint);
  {-SHA3-384 of Msg with init/update/final}

procedure SHA3_384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_384Digest; var buf; bsize: word; var Err: word);
  {-SHA3-384 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA3_384_BlockLen = 104;  {Rate / 8, used only for HMAC}  {FIPS202, Tab.3}

{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.8}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA3-384(9)}

const
  SHA3_384_OID : TOID_Vec = (2,16,840,1,101,3,4,2,9,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA3_384_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA3_384_BlockLen;
               HDigestlen: sizeof(TSHA3_384Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA3_384Init;
               HFinal    : @SHA3_384FinalEx;
               HUpdateXL : @SHA3_384UpdateXL;
             {$else}
               HInit     : SHA3_384Init;
               HFinal    : SHA3_384FinalEx;
               HUpdateXL : SHA3_384UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA3_384);
               HName     : 'SHA3-384';
               HPtrOID   : @SHA3_384_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA3_384FinalBitsEx;
             {$else}
               HFinalBit : SHA3_384FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA3_384_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA3_384Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context}
  SHA3_LastError := SHA3_Init(TSHA3State(Context),__SHA3_384);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_384FinalBits(var Context: THashContext; var Digest: TSHA3_384Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_384FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_384Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-384 calculation with bitlen bits from BData (LSB format), clear context}
begin
  SHA3_LastError := SHA3_FinalBit_LSB(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-384 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384Final(var Context: THashContext; var Digest: TSHA3_384Digest);
  {-finalize SHA3-384 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
function SHA3_384SelfTest: boolean;
  {-self test for string from SHA3-384 documents}
const
  Bl1 = 0;
  dig1: TSHA3_384Digest      = ($0C,$63,$A7,$5B,$84,$5E,$4F,$7D,
                                $01,$10,$7D,$85,$2E,$4C,$24,$85,
                                $C5,$1A,$50,$AA,$AA,$94,$FC,$61,
                                $99,$5E,$71,$BB,$EE,$98,$3A,$2A,
                                $C3,$71,$38,$31,$26,$4A,$DB,$47,
                                $FB,$6B,$D1,$E0,$58,$D5,$F0,$04);
  BL2 = 5;
  msg2: array[0..0] of byte  = ($13);
  dig2: TSHA3_384Digest      = ($73,$7C,$9B,$49,$18,$85,$E9,$BF,
                                $74,$28,$E7,$92,$74,$1A,$7B,$F8,
                                $DC,$A9,$65,$34,$71,$C3,$E1,$48,
                                $47,$3F,$2C,$23,$6B,$6A,$0A,$64,
                                $55,$EB,$1D,$CE,$9F,$77,$9B,$4B,
                                $6B,$23,$7F,$EF,$17,$1B,$1C,$64);
  BL3 = 30;
  msg3: array[0..3] of byte  = ($53,$58,$7B,$19);
  dig3: TSHA3_384Digest      = ($95,$5B,$4D,$D1,$BE,$03,$26,$1B,
                                $D7,$6F,$80,$7A,$7E,$FD,$43,$24,
                                $35,$C4,$17,$36,$28,$11,$B8,$A5,
                                $0C,$56,$4E,$7E,$E9,$58,$5E,$1A,
                                $C7,$62,$6D,$DE,$2F,$DC,$03,$0F,
                                $87,$61,$96,$EA,$26,$7F,$08,$C3);

  {https://github.com/gvanas/KeccakCodePackage, SKat len=200}
  BL4 = 200;
  msg4: array[0..24] of byte = ($aa,$fd,$c9,$24,$3d,$3d,$4a,$09,
                                $65,$58,$a3,$60,$cc,$27,$c8,$d8,
                                $62,$f0,$be,$73,$db,$5e,$88,$aa,$55);
  dig4: TSHA3_384Digest      = ($3d,$d9,$05,$4c,$10,$5c,$40,$79,
                                $8d,$f4,$5c,$fb,$58,$80,$f9,$7a,
                                $95,$36,$fa,$7b,$d1,$3f,$1d,$81,
                                $6b,$8e,$e8,$87,$fc,$ba,$fc,$10,
                                $2a,$7d,$4b,$de,$9f,$e6,$e2,$65,
                                $53,$8e,$ec,$25,$25,$b5,$0d,$89);
var
  Context: THashContext;
  Digest : TSHA3_384Digest;

  function SingleTest(Msg: pointer; BL: word; TDig: TSHA3_384Digest): boolean;
  var
    bytes: word;
  begin
    SingleTest := false;
    SHA3_384Init(Context);
    if SHA3_LastError<>0 then exit;
    if BL=0 then SHA3_384Final(Context,Digest)
    else begin
      if BL>7 then begin
        bytes := BL shr 3;
        SHA3_384Update(Context, Msg, BL shr 3);
        if SHA3_LastError<>0 then exit;
        inc(Ptr2Inc(Msg), bytes);
      end;
      SHA3_384FinalBits_LSB(Context, Digest, pByte(Msg)^, BL and 7);
    end;
    if SHA3_LastError<>0 then exit;
    SingleTest := HashSameDigest(@SHA3_384_Desc, PHashDigest(@TDig), PHashDigest(@Digest));
  end;

begin
  SHA3_384SelfTest := SingleTest(nil, BL1, dig1)   and
                      SingleTest(@msg2, BL2, dig2) and
                      SingleTest(@msg3, BL3, dig3) and
                      SingleTest(@msg4, BL4, dig4);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384FullXL(var Digest: TSHA3_384Digest; Msg: pointer; Len: longint);
  {-SHA3-384 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA3_384Init(Context);
  if SHA3_LastError=0 then SHA3_384UpdateXL(Context, Msg, Len);
  SHA3_384Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384Full(var Digest: TSHA3_384Digest; Msg: pointer; Len: word);
  {-SHA3-384 of Msg with init/update/final}
begin
  SHA3_384FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_384Digest; var buf; bsize: word; var Err: word);
  {-SHA3-384 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA3_384_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA3_384_Desc, sizeof(SHA3_384_Desc), 0);
    with SHA3_384_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA3_384_BlockLen;
       HDigestlen:= sizeof(TSHA3_384Digest);
       HInit     := SHA3_384Init;
       HFinal    := SHA3_384FinalEx;
       HUpdateXL := SHA3_384UpdateXL;
       HAlgNum   := longint(_SHA3_384);
       HName     := 'SHA3-384';
       HPtrOID   := @SHA3_384_OID;
       HLenOID   := 9;
       HFinalBit := SHA3_384FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA3_384, @SHA3_384_Desc);
end.
