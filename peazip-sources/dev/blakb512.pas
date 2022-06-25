unit Blakb512;

{Blake2b-512 - 512 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  Blake2b-512 - 512 bit Secure Hash Function

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - Saarinen et al: The BLAKE2 Cryptographic Hash and Message
                      Authentication  Code (MAC); https://tools.ietf.org/html/rfc7693
                    - Aumasson et al: BLAKE2: simpler, smaller, fast as MD5;
                      https://blake2.net/blake2.pdf
                    - Official reference code: https://github.com/BLAKE2/BLAKE2


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     07.10.17  W.Ehrhardt  Initial version using Blaks256 layout
 0.11     03.11.17  we          Missing functions for hash integration, non-INT64 compatible
 0.12     24.11.17  we          RegisterHash for all compilers
 0.13     29.11.17  we          Blakb512File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2017 Wolfgang Ehrhardt

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
  BTypes,Hash,Blake2b;


procedure Blakb512Init(var Context: THashContext);
  {-initialize context}

procedure Blakb512Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure Blakb512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure Blakb512Final(var Context: THashContext; var Digest: TBlake2B_512Digest);
  {-finalize Blake2B-512 calculation, clear context}

function  Blakb512SelfTest: boolean;
  {-self test for Blake2B-512}

procedure Blakb512Full(var Digest: TBlake2B_512Digest; Msg: pointer; Len: word);
  {-Blake2B-512 of Msg with init/update/final}

procedure Blakb512FullXL(var Digest: TBlake2B_512Digest; Msg: pointer; Len: longint);
  {-Blake2B-512 of Msg with init/update/final}

procedure Blakb512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2b-512 calculation, clear context}

procedure Blakb512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2B_512Digest; var buf; bsize: word; var Err: word);
  {-Blake2B-512 of file, buf: buffer with at least bsize bytes}

implementation


{$ifdef BIT16}
  {$F+}
{$endif}


const
  Blakb512_OID : TOID_Vec = (1,3,6,1,4,1,1722,12,2,1,16);  {Len=11}

{---------------------------------------------------------------------------}
procedure Blakb512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize calculation with bitlen bits from BData, clear context}
begin
  {Just ignore the final bits}
  Blakb512FinalEx(Context, Digest);
end;

{$ifndef VER5x}

const
  Blakb512_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : BLAKE2B_BlockLen;
               HDigestlen: sizeof(TBlake2B_512Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @Blakb512Init;
               HFinal    : @Blakb512FinalEx;
               HUpdateXL : @Blakb512UpdateXL;
             {$else}
               HInit     : Blakb512Init;
               HFinal    : Blakb512FinalEx;
               HUpdateXL : Blakb512UpdateXL;
             {$endif}
               HAlgNum   : longint(_Blake2B_512);
               HName     : 'Blake2B-512';
               HPtrOID   : @Blakb512_OID;
               HLenOID   : 11;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @Blakb512FinalBitsEx;
             {$else}
               HFinalBit : Blakb512FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  Blakb512_Desc: THashDesc;
{$endif}

{---------------------------------------------------------------------------}
procedure Blakb512Init(var Context: THashContext);
  {-initialize context}
var
  err: integer;
begin
  {Clear context}
  err := blake2b_Init(Context,nil,0,sizeof(TBlake2B_512Digest));
  if err<>0 then begin
    {$ifdef debug}
      {This should not happen, because and error can only be returned}
      {if key/digest lengths are out of range, 0 and 32 are OK}
      writeln('Blakb512Init error = ',err);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
procedure Blakb512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  blake2b_update(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blakb512Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  blake2b_update(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blakb512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2b-512 calculation, clear context}
var
  b2sd: TBlake2bDigest absolute Digest;
begin
  fillchar(Digest, sizeof(Digest), 0);
  Blake2b_Final(Context, b2sd);
end;


{---------------------------------------------------------------------------}
procedure Blakb512Final(var Context: THashContext; var Digest: TBlake2B_512Digest);
  {-finalize Blake2B-512 calculation, clear context}
var
  b2bd: TBlake2bDigest;
begin
  fillchar(b2bd, sizeof(b2bd), 0);
  Blake2b_Final(Context, b2bd);
  move(b2bd, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function Blakb512SelfTest: boolean;
  {-self test for Blake2B-512}
const
  abc : array[0..2] of char8 = 'abc';
  digabc: TBlake2B_512Digest = (
           $ba,$80,$a5,$3f,$98,$1c,$4d,$0d,$6a,$27,$97,$b6,$9f,$12,$f6,$e9,
           $4c,$21,$2f,$14,$68,$5a,$c4,$b7,$4b,$12,$bb,$6f,$db,$ff,$a2,$d1,
           $7d,$87,$c5,$39,$2a,$ab,$79,$2d,$c2,$52,$d5,$de,$45,$33,$cc,$95,
           $18,$d3,$8a,$a8,$db,$f1,$92,$5a,$b9,$23,$86,$ed,$d4,$00,$99,$23);
  dignil: TBlake2B_512Digest = (
           $78,$6a,$02,$f7,$42,$01,$59,$03,$c6,$c6,$fd,$85,$25,$52,$d2,$72,
           $91,$2f,$47,$40,$e1,$58,$47,$61,$8a,$86,$e2,$17,$f7,$1f,$54,$19,
           $d2,$5e,$10,$31,$af,$ee,$58,$53,$13,$89,$64,$44,$93,$4e,$b0,$4b,
           $90,$3a,$68,$5b,$14,$48,$b7,$55,$d5,$6f,$70,$1a,$fe,$9b,$e2,$ce);

var
  Digest: TBlake2B_512Digest;
begin
  Blakb512SelfTest := false;
  if not blake2b_selftest then exit;
  {from libtomcrypt V1.18 \src\hashes\blake2b.c}
  Blakb512Full(Digest, nil, 0);
  if not HashSameDigest(@Blakb512_Desc, PHashDigest(@Digest), PHashDigest(@dignil)) then exit;
  Blakb512Full(Digest, @abc, sizeof(abc));
  Blakb512SelfTest := HashSameDigest(@Blakb512_Desc, PHashDigest(@Digest), PHashDigest(@digabc));
end;


{---------------------------------------------------------------------------}
procedure Blakb512FullXL(var Digest: TBlake2B_512Digest; Msg: pointer; Len: longint);
  {-Blake2B-512 of Msg with init/update/final}
var
  Context: THashContext;
begin
  Blakb512Init(Context);
  Blakb512UpdateXL(Context, Msg, Len);
  Blakb512Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure Blakb512Full(var Digest: TBlake2B_512Digest; Msg: pointer; Len: word);
  {-Blake2B-512 of Msg with init/update/final}
begin
  Blakb512FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blakb512File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2B_512Digest; var buf; bsize: word; var Err: word);
  {-Blake2B-512 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @Blakb512_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(Blakb512_Desc, sizeof(Blakb512_Desc), 0);
    with Blakb512_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := BLAKE2B_BlockLen;
       HDigestlen:= sizeof(TBlake2B_512Digest);
       HInit     := Blakb512Init;
       HFinal    := Blakb512FinalEx;
       HUpdateXL := Blakb512UpdateXL;
       HAlgNum   := longint(_Blake2B_512);
       HName     := 'Blake2B-512';
       HPtrOID   := @Blakb512_OID;
       HLenOID   := 11;
       HFinalBit := Blakb512FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_Blake2B_512, @Blakb512_Desc);
end.
