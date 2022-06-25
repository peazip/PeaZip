unit SHA3_512;

{SHA3-512 - 512 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA3-512 - 512 bit Secure Hash Function

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
 0.13     29.11.17  we          SHA3_512File - fname: string

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


procedure SHA3_512Init(var Context: THashContext);
  {-initialize context}

procedure SHA3_512Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA3_512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA3_512Final(var Context: THashContext; var Digest: TSHA3_512Digest);
  {-finalize SHA3-512 calculation, clear context}

procedure SHA3_512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-512 calculation, clear context}

procedure SHA3_512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_512FinalBits(var Context: THashContext; var Digest: TSHA3_512Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_512FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_512Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (LSB format), clear context}

function  SHA3_512SelfTest: boolean;
  {-self test for string from SHA3-512 documents}

procedure SHA3_512Full(var Digest: TSHA3_512Digest; Msg: pointer; Len: word);
  {-SHA3-512 of Msg with init/update/final}

procedure SHA3_512FullXL(var Digest: TSHA3_512Digest; Msg: pointer; Len: longint);
  {-SHA3-512 of Msg with init/update/final}

procedure SHA3_512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_512Digest; var buf; bsize: word; var Err: word);
  {-SHA3-512 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA3_512_BlockLen = 72;  {Rate / 8, used only for HMAC} {FIPS202, Tab.3}

{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.8}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA3-512(10)}

const
  SHA3_512_OID : TOID_Vec = (2,16,840,1,101,3,4,2,10,-1,-1); {Len=9}

{$ifndef VER5X}
const
  SHA3_512_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA3_512_BlockLen;
               HDigestlen: sizeof(TSHA3_512Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA3_512Init;
               HFinal    : @SHA3_512FinalEx;
               HUpdateXL : @SHA3_512UpdateXL;
             {$else}
               HInit     : SHA3_512Init;
               HFinal    : SHA3_512FinalEx;
               HUpdateXL : SHA3_512UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA3_512);
               HName     : 'SHA3-512';
               HPtrOID   : @SHA3_512_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA3_512FinalBitsEx;
             {$else}
               HFinalBit : SHA3_512FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA3_512_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA3_512Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context}
  SHA3_LastError := SHA3_Init(TSHA3State(Context),__SHA3_512);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_512FinalBits(var Context: THashContext; var Digest: TSHA3_512Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_512FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_512Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-512 calculation with bitlen bits from BData (LSB format), clear context}
begin
  SHA3_LastError := SHA3_FinalBit_LSB(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-512 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512Final(var Context: THashContext; var Digest: TSHA3_512Digest);
  {-finalize SHA3-512 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
function SHA3_512SelfTest: boolean;
  {-self test for string from SHA3-512 documents}
const
  Bl1 = 0;
  dig1: TSHA3_512Digest      = ($A6,$9F,$73,$CC,$A2,$3A,$9A,$C5,
                                $C8,$B5,$67,$DC,$18,$5A,$75,$6E,
                                $97,$C9,$82,$16,$4F,$E2,$58,$59,
                                $E0,$D1,$DC,$C1,$47,$5C,$80,$A6,
                                $15,$B2,$12,$3A,$F1,$F5,$F9,$4C,
                                $11,$E3,$E9,$40,$2C,$3A,$C5,$58,
                                $F5,$00,$19,$9D,$95,$B6,$D3,$E3,
                                $01,$75,$85,$86,$28,$1D,$CD,$26);
  BL2 = 5;
  msg2: array[0..0] of byte  = ($13);
  dig2: TSHA3_512Digest      = ($A1,$3E,$01,$49,$41,$14,$C0,$98,
                                $00,$62,$2A,$70,$28,$8C,$43,$21,
                                $21,$CE,$70,$03,$9D,$75,$3C,$AD,
                                $D2,$E0,$06,$E4,$D9,$61,$CB,$27,
                                $54,$4C,$14,$81,$E5,$81,$4B,$DC,
                                $EB,$53,$BE,$67,$33,$D5,$E0,$99,
                                $79,$5E,$5E,$81,$91,$8A,$DD,$B0,
                                $58,$E2,$2A,$9F,$24,$88,$3F,$37);
  BL3 = 30;
  msg3: array[0..3] of byte  = ($53,$58,$7B,$19);
  dig3: TSHA3_512Digest      = ($98,$34,$C0,$5A,$11,$E1,$C5,$D3,
                                $DA,$9C,$74,$0E,$1C,$10,$6D,$9E,
                                $59,$0A,$0E,$53,$0B,$6F,$6A,$AA,
                                $78,$30,$52,$5D,$07,$5C,$A5,$DB,
                                $1B,$D8,$A6,$AA,$98,$1A,$28,$61,
                                $3A,$C3,$34,$93,$4A,$01,$82,$3C,
                                $D4,$5F,$45,$E4,$9B,$6D,$7E,$69,
                                $17,$F2,$F1,$67,$78,$06,$7B,$AB);

  {https://github.com/gvanas/KeccakCodePackage, SKat len=200}
  BL4 = 200;
  msg4: array[0..24] of byte = ($aa,$fd,$c9,$24,$3d,$3d,$4a,$09,
                                $65,$58,$a3,$60,$cc,$27,$c8,$d8,
                                $62,$f0,$be,$73,$db,$5e,$88,$aa,$55);
  dig4: TSHA3_512Digest      = ($62,$86,$c3,$db,$87,$d3,$b4,$5c,
                                $fd,$4d,$e8,$5a,$7a,$dd,$18,$e0,
                                $7a,$e2,$2f,$1f,$0f,$46,$75,$e1,
                                $d4,$e1,$fc,$77,$63,$37,$34,$d7,
                                $96,$28,$18,$a9,$f3,$b9,$6b,$37,
                                $fe,$77,$4f,$c2,$6d,$ea,$78,$74,
                                $85,$31,$7b,$96,$22,$27,$5f,$63,
                                $a7,$dd,$6d,$62,$d6,$50,$d3,$07);

var
  Context: THashContext;
  Digest : TSHA3_512Digest;

  function SingleTest(Msg: pointer; BL: word; TDig: TSHA3_512Digest): boolean;
  var
    bytes: word;
  begin
    SingleTest := false;
    SHA3_512Init(Context);
    if SHA3_LastError<>0 then exit;
    if BL=0 then SHA3_512Final(Context,Digest)
    else begin
      if BL>7 then begin
        bytes := BL shr 3;
        SHA3_512Update(Context, Msg, BL shr 3);
        if SHA3_LastError<>0 then exit;
        inc(Ptr2Inc(Msg), bytes);
      end;
      SHA3_512FinalBits_LSB(Context, Digest, pByte(Msg)^, BL and 7);
    end;
    if SHA3_LastError<>0 then exit;
    SingleTest := HashSameDigest(@SHA3_512_Desc, PHashDigest(@TDig), PHashDigest(@Digest));
  end;

begin
  SHA3_512SelfTest := SingleTest(nil, BL1, dig1)   and
                      SingleTest(@msg2, BL2, dig2) and
                      SingleTest(@msg3, BL3, dig3) and
                      SingleTest(@msg4, BL4, dig4);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512FullXL(var Digest: TSHA3_512Digest; Msg: pointer; Len: longint);
  {-SHA3-512 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA3_512Init(Context);
  if SHA3_LastError=0 then SHA3_512UpdateXL(Context, Msg, Len);
  SHA3_512Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512Full(var Digest: TSHA3_512Digest; Msg: pointer; Len: word);
  {-SHA3-512 of Msg with init/update/final}
begin
  SHA3_512FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_512Digest; var buf; bsize: word; var Err: word);
  {-SHA3-512 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA3_512_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA3_512_Desc, sizeof(SHA3_512_Desc), 0);
    with SHA3_512_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA3_512_BlockLen;
       HDigestlen:= sizeof(TSHA3_512Digest);
       HInit     := SHA3_512Init;
       HFinal    := SHA3_512FinalEx;
       HUpdateXL := SHA3_512UpdateXL;
       HAlgNum   := longint(_SHA3_512);
       HName     := 'SHA3-512';
       HPtrOID   := @SHA3_512_OID;
       HLenOID   := 9;
       HFinalBit := SHA3_512FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA3_512, @SHA3_512_Desc);
end.
