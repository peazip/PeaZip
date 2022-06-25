unit SHA3_256;

{SHA3-256 - 256 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  SHA3-256 - 256 bit Secure Hash Function

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
 0.10     08.08.15  W.Ehrhardt  Initial BP version using SHA5-256 layout
 0.11     09.08.15  we          SHA3_256FinalBits_LSB
 0.12     17.08.15  we          Updated references
 0.13     15.05.17  we          adjust OID to new MaxOIDLen
 0.14     29.11.17  we          SHA3_256File - fname: string

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


procedure SHA3_256Init(var Context: THashContext);
  {-initialize context}

procedure SHA3_256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA3_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA3_256Final(var Context: THashContext; var Digest: TSHA3_256Digest);
  {-finalize SHA3-256 calculation, clear context}

procedure SHA3_256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-256 calculation, clear context}

procedure SHA3_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_256FinalBits(var Context: THashContext; var Digest: TSHA3_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA3_256FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (LSB format), clear context}

function  SHA3_256SelfTest: boolean;
  {-self test for string from SHA3-256 documents}

procedure SHA3_256Full(var Digest: TSHA3_256Digest; Msg: pointer; Len: word);
  {-SHA3-256 of Msg with init/update/final}

procedure SHA3_256FullXL(var Digest: TSHA3_256Digest; Msg: pointer; Len: longint);
  {-SHA3-256 of Msg with init/update/final}

procedure SHA3_256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_256Digest; var buf; bsize: word; var Err: word);
  {-SHA3-256 of file, buf: buffer with at least bsize bytes}


implementation


const
  SHA3_256_BlockLen = 136;  {Rate / 8, used only for HMAC} {FIPS202, Tab.3}

{http://csrc.nist.gov/groups/ST/crypto_apps_infra/csor/algorithms.html}
{2.16.840.1.101.3.4.2.8}
{joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithm(4) hashAlgs(2) SHA3-256(8)}

const
  SHA3_256_OID : TOID_Vec = (2,16,840,1,101,3,4,2,8,-1,-1); {Len=9}


{$ifndef VER5X}
const
  SHA3_256_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : SHA3_256_BlockLen;
               HDigestlen: sizeof(TSHA3_256Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @SHA3_256Init;
               HFinal    : @SHA3_256FinalEx;
               HUpdateXL : @SHA3_256UpdateXL;
             {$else}
               HInit     : SHA3_256Init;
               HFinal    : SHA3_256FinalEx;
               HUpdateXL : SHA3_256UpdateXL;
             {$endif}
               HAlgNum   : longint(_SHA3_256);
               HName     : 'SHA3-256';
               HPtrOID   : @SHA3_256_OID;
               HLenOID   : 9;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @SHA3_256FinalBitsEx;
             {$else}
               HFinalBit : SHA3_256FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  SHA3_256_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure SHA3_256Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context}
  SHA3_LastError := SHA3_Init(TSHA3State(Context),__SHA3_256);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  SHA3_LastError := SHA3_UpdateXL(TSHA3State(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_256FinalBits(var Context: THashContext; var Digest: TSHA3_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (big-endian), clear context}
begin
  SHA3_LastError := SHA3_FinalBit(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_256FinalBits_LSB(var Context: THashContext; var Digest: TSHA3_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA3-256 calculation with bitlen bits from BData (LSB format), clear context}
begin
  SHA3_LastError := SHA3_FinalBit_LSB(TSHA3State(Context), BData, bitlen, @Digest[0], 8*sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure SHA3_256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA3-256 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256Final(var Context: THashContext; var Digest: TSHA3_256Digest);
  {-finalize SHA3-256 calculation, clear context}
begin
  SHA3_LastError := SHA3_FinalHash(TSHA3State(Context), @Digest[0]);
end;


{---------------------------------------------------------------------------}
function SHA3_256SelfTest: boolean;
  {-self test for string from SHA3-256 documents}
const
  Bl1 = 0;
  dig1: TSHA3_256Digest      = ($A7,$FF,$C6,$F8,$BF,$1E,$D7,$66,
                                $51,$C1,$47,$56,$A0,$61,$D6,$62,
                                $F5,$80,$FF,$4D,$E4,$3B,$49,$FA,
                                $82,$D8,$0A,$4B,$80,$F8,$43,$4A);
  BL2 = 5;
  msg2: array[0..0] of byte  = ($13);
  dig2: TSHA3_256Digest      = ($7B,$00,$47,$CF,$5A,$45,$68,$82,
                                $36,$3C,$BF,$0F,$B0,$53,$22,$CF,
                                $65,$F4,$B7,$05,$9A,$46,$36,$5E,
                                $83,$01,$32,$E3,$B5,$D9,$57,$AF);
  BL3 = 30;
  msg3: array[0..3] of byte  = ($53,$58,$7B,$19);
  dig3: TSHA3_256Digest      = ($C8,$24,$2F,$EF,$40,$9E,$5A,$E9,
                                $D1,$F1,$C8,$57,$AE,$4D,$C6,$24,
                                $B9,$2B,$19,$80,$9F,$62,$AA,$8C,
                                $07,$41,$1C,$54,$A0,$78,$B1,$D0);

  {https://github.com/gvanas/KeccakCodePackage, SKat len=200}
  BL4 = 200;
  msg4: array[0..24] of byte = ($aa,$fd,$c9,$24,$3d,$3d,$4a,$09,
                                $65,$58,$a3,$60,$cc,$27,$c8,$d8,
                                $62,$f0,$be,$73,$db,$5e,$88,$aa,$55);
  dig4: TSHA3_256Digest      = ($c6,$4b,$ec,$f7,$b7,$5f,$c8,$85,
                                $d5,$85,$39,$24,$f2,$b7,$d3,$7a,
                                $bc,$ef,$d3,$da,$12,$6b,$b8,$17,
                                $69,$7e,$1a,$09,$15,$2b,$1e,$be);
var
  Context: THashContext;
  Digest : TSHA3_256Digest;

  function SingleTest(Msg: pointer; BL: word; TDig: TSHA3_256Digest): boolean;
  var
    bytes: word;
  begin
    SingleTest := false;
    SHA3_256Init(Context);
    if SHA3_LastError<>0 then exit;
    if BL=0 then SHA3_256Final(Context,Digest)
    else begin
      if BL>7 then begin
        bytes := BL shr 3;
        SHA3_256Update(Context, Msg, BL shr 3);
        if SHA3_LastError<>0 then exit;
        inc(Ptr2Inc(Msg), bytes);
      end;
      SHA3_256FinalBits_LSB(Context, Digest, pByte(Msg)^, BL and 7);
    end;
    if SHA3_LastError<>0 then exit;
    SingleTest := HashSameDigest(@SHA3_256_Desc, PHashDigest(@TDig), PHashDigest(@Digest));
  end;

begin
  SHA3_256SelfTest := SingleTest(nil, BL1, dig1)   and
                      SingleTest(@msg2, BL2, dig2) and
                      SingleTest(@msg3, BL3, dig3) and
                      SingleTest(@msg4, BL4, dig4);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256FullXL(var Digest: TSHA3_256Digest; Msg: pointer; Len: longint);
  {-SHA3-256 of Msg with init/update/final}
var
  Context: THashContext;
begin
  SHA3_256Init(Context);
  if SHA3_LastError=0 then SHA3_256UpdateXL(Context, Msg, Len);
  SHA3_256Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256Full(var Digest: TSHA3_256Digest; Msg: pointer; Len: word);
  {-SHA3-256 of Msg with init/update/final}
begin
  SHA3_256FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure SHA3_256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TSHA3_256Digest; var buf; bsize: word; var Err: word);
  {-SHA3-256 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @SHA3_256_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(SHA3_256_Desc, sizeof(SHA3_256_Desc), 0);
    with SHA3_256_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := SHA3_256_BlockLen;
       HDigestlen:= sizeof(TSHA3_256Digest);
       HInit     := SHA3_256Init;
       HFinal    := SHA3_256FinalEx;
       HUpdateXL := SHA3_256UpdateXL;
       HAlgNum   := longint(_SHA3_256);
       HName     := 'SHA3-256';
       HPtrOID   := @SHA3_256_OID;
       HLenOID   := 9;
       HFinalBit := SHA3_256FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_SHA3_256, @SHA3_256_Desc);
end.
