unit Blakb384;

{Blake2b-384 - 384 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  Blake2b-384 - 384 bit Secure Hash Function

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

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
 0.13     24.11.17  we          RegisterHash for all compilers
 0.14     29.11.17  we          Blakb384File - fname: string

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

procedure Blakb384Init(var Context: THashContext);
  {-initialize context}

procedure Blakb384Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure Blakb384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure Blakb384Final(var Context: THashContext; var Digest: TBlake2B_384Digest);
  {-finalize Blake2B-384 calculation, clear context}

procedure Blakb384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2B-384 calculation, clear context}

function  Blakb384SelfTest: boolean;
  {-self test for Blake2B-384}

procedure Blakb384Full(var Digest: TBlake2B_384Digest; Msg: pointer; Len: word);
  {-Blake2B-384 of Msg with init/update/final}

procedure Blakb384FullXL(var Digest: TBlake2B_384Digest; Msg: pointer; Len: longint);
  {-Blake2B-384 of Msg with init/update/final}

procedure Blakb384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2B_384Digest; var buf; bsize: word; var Err: word);
  {-Blake2B-384 of file, buf: buffer with at least bsize bytes}

implementation


const
  Blakb384_OID : TOID_Vec = (1,3,6,1,4,1,1722,12,2,1,12);  {Len=11}


{$ifdef BIT16}
  {$F+}
{$endif}

{---------------------------------------------------------------------------}
procedure Blakb384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize calculation with bitlen bits from BData, clear context}
begin
  {Just ignore the final bits}
  Blakb384FinalEx(Context, Digest);
end;

{$ifndef VER5X}
const
  Blakb384_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : BLAKE2B_BlockLen;
               HDigestlen: sizeof(TBlake2B_384Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @Blakb384Init;
               HFinal    : @Blakb384FinalEx;
               HUpdateXL : @Blakb384UpdateXL;
             {$else}
               HInit     : Blakb384Init;
               HFinal    : Blakb384FinalEx;
               HUpdateXL : Blakb384UpdateXL;
             {$endif}
               HAlgNum   : longint(_Blake2B_384);
               HName     : 'Blake2B-384';
               HPtrOID   : @Blakb384_OID;
               HLenOID   : 11;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @Blakb384FinalBitsEx;
             {$else}
               HFinalBit : Blakb384FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );

{$else}
var
  Blakb384_Desc: THashDesc;
{$endif}


{---------------------------------------------------------------------------}
procedure Blakb384Init(var Context: THashContext);
  {-initialize context}
var
  err: integer;
begin
  {Clear context}
  err := blake2b_Init(Context,nil,0,sizeof(TBlake2B_384Digest));
  if err<>0 then begin
    {$ifdef debug}
      {This should not happen, because and error can only be returned}
      {if key/digest lengths are out of range, 0 and 32 are OK}
      writeln('Blakb384Init error = ',err);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
procedure Blakb384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  blake2b_update(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blakb384Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  blake2b_update(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blakb384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2B-384 calculation, clear context}
var
  b2sd: TBlake2BDigest absolute Digest;
begin
  fillchar(Digest, sizeof(Digest), 0);
  Blake2b_Final(Context, b2sd);
end;


{---------------------------------------------------------------------------}
procedure Blakb384Final(var Context: THashContext; var Digest: TBlake2B_384Digest);
  {-finalize Blake2B-384 calculation, clear context}
var
  b2bd: TBlake2BDigest;
begin
  fillchar(b2bd, sizeof(b2bd), 0);
  Blake2b_Final(Context, b2bd);
  move(b2bd, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function Blakb384SelfTest: boolean;
  {-self test for Blake2B-384}
const
  abc : array[0..2] of char8 = 'abc';
  digabc: TBlake2B_384Digest = (
           $6f,$56,$a8,$2c,$8e,$7e,$f5,$26,$df,$e1,$82,$eb,$52,$12,$f7,$db,
           $9d,$f1,$31,$7e,$57,$81,$5d,$bd,$a4,$60,$83,$fc,$30,$f5,$4e,$e6,
           $c6,$6b,$a8,$3b,$e6,$4b,$30,$2d,$7c,$ba,$6c,$e1,$5b,$b5,$56,$f4);
  dignil: TBlake2B_384Digest = (
           $b3,$28,$11,$42,$33,$77,$f5,$2d,$78,$62,$28,$6e,$e1,$a7,$2e,$e5,
           $40,$52,$43,$80,$fd,$a1,$72,$4a,$6f,$25,$d7,$97,$8c,$6f,$d3,$24,
           $4a,$6c,$af,$04,$98,$81,$26,$73,$c5,$e0,$5e,$f5,$83,$82,$51,$00);
var
  Digest: TBlake2B_384Digest;
begin
  Blakb384SelfTest := false;
  if not blake2b_selftest then exit;
  {from libtomcrypt V1.18 \src\hashes\blake2b.c}
  Blakb384Full(Digest, nil, 0);
  if not HashSameDigest(@Blakb384_Desc, PHashDigest(@Digest), PHashDigest(@dignil)) then exit;
  Blakb384Full(Digest, @abc, sizeof(abc));
  Blakb384SelfTest := HashSameDigest(@Blakb384_Desc, PHashDigest(@Digest), PHashDigest(@digabc));
end;


{---------------------------------------------------------------------------}
procedure Blakb384FullXL(var Digest: TBlake2B_384Digest; Msg: pointer; Len: longint);
  {-Blake2B-384 of Msg with init/update/final}
var
  Context: THashContext;
begin
  Blakb384Init(Context);
  Blakb384UpdateXL(Context, Msg, Len);
  Blakb384Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure Blakb384Full(var Digest: TBlake2B_384Digest; Msg: pointer; Len: word);
  {-Blake2B-384 of Msg with init/update/final}
begin
  Blakb384FullXL(Digest, Msg, Len);
end;



{---------------------------------------------------------------------------}
procedure Blakb384File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2B_384Digest; var buf; bsize: word; var Err: word);
  {-Blake2B-384 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @Blakb384_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;

begin
  {$ifdef VER5X}
    fillchar(Blakb384_Desc, sizeof(Blakb384_Desc), 0);
    with Blakb384_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := BLAKE2B_BlockLen;
       HDigestlen:= sizeof(TBlake2B_384Digest);
       HInit     := Blakb384Init;
       HFinal    := Blakb384FinalEx;
       HUpdateXL := Blakb384UpdateXL;
       HAlgNum   := longint(_Blake2B_384);
       HName     := 'Blake2B-384';
       HPtrOID   := @Blakb384_OID;
       HLenOID   := 11;
       HFinalBit := Blakb384FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_Blake2B_384, @Blakb384_Desc);
end.
