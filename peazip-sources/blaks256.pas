unit Blaks256;

{Blake2s-256 - 256 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  Blake2s-256 - 256 bit Secure Hash Function

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
 0.10     15.05.17  W.Ehrhardt  Initial BP version using SHA3-256 layout
 0.11     16.05.17  we          Avoid warning in Blaks256Init, references, OID
 0.12     17.05.17  we          Two additional specific tests
 0.13     29.11.17  we          Blaks256File - fname: string

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
  BTypes,Hash,Blake2s;


procedure Blaks256Init(var Context: THashContext);
  {-initialize context}

procedure Blaks256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure Blaks256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure Blaks256Final(var Context: THashContext; var Digest: TBlake2S_256Digest);
  {-finalize Blake2S-256 calculation, clear context}

procedure Blaks256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2S-256 calculation, clear context}

function  Blaks256SelfTest: boolean;
  {-self test for Blake2S-256}

procedure Blaks256Full(var Digest: TBlake2S_256Digest; Msg: pointer; Len: word);
  {-Blake2S-256 of Msg with init/update/final}

procedure Blaks256FullXL(var Digest: TBlake2S_256Digest; Msg: pointer; Len: longint);
  {-Blake2S-256 of Msg with init/update/final}

procedure Blaks256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2S_256Digest; var buf; bsize: word; var Err: word);
  {-Blake2S-256 of file, buf: buffer with at least bsize bytes}


implementation


{$ifdef BIT16}
  {$F+}
{$endif}

{---------------------------------------------------------------------------}
procedure Blaks256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize calculation with bitlen bits from BData, clear context}
begin
  {Just ignore the final bits}
  Blaks256FinalEx(Context, Digest);
end;


const
  Blaks256_OID : TOID_Vec = (1,3,6,1,4,1,1722,12,2,2,8);  {Len=11}


{$ifndef VER5X}
const
  Blaks256_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : BLAKE2S_BlockLen;
               HDigestlen: sizeof(TBlake2S_256Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @Blaks256Init;
               HFinal    : @Blaks256FinalEx;
               HUpdateXL : @Blaks256UpdateXL;
             {$else}
               HInit     : Blaks256Init;
               HFinal    : Blaks256FinalEx;
               HUpdateXL : Blaks256UpdateXL;
             {$endif}
               HAlgNum   : longint(_Blake2S_256);
               HName     : 'Blake2S-256';
               HPtrOID   : @Blaks256_OID;
               HLenOID   : 11;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @Blaks256FinalBitsEx;
             {$else}
               HFinalBit : Blaks256FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  Blaks256_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure Blaks256Init(var Context: THashContext);
  {-initialize context}
var
  err: integer;
begin
  {Clear context}
  err := blake2s_Init(blake2s_ctx(Context),nil,0,sizeof(TBlake2S_256Digest));
  if err<>0 then begin
    {$ifdef debug}
      {This should not happen, because and error can only be returned}
      {if key/digest lengths are out of range, 0 and 32 are OK}
      writeln('Blaks256Init error = ',err);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
procedure Blaks256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  blake2s_update(blake2s_ctx(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks256Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  blake2s_update(blake2s_ctx(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2S-256 calculation, clear context}
var
  b2sd: TBlake2SDigest absolute Digest;
begin
  fillchar(Digest, sizeof(Digest), 0);
  Blake2s_Final(blake2s_ctx(Context), b2sd);
end;


{---------------------------------------------------------------------------}
procedure Blaks256Final(var Context: THashContext; var Digest: TBlake2S_256Digest);
  {-finalize Blake2S-256 calculation, clear context}
var
  b2sd: TBlake2SDigest;
begin
  fillchar(b2sd, sizeof(b2sd), 0);
  Blake2s_Final(blake2s_ctx(Context), b2sd);
  move(b2sd, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function Blaks256SelfTest: boolean;
  {-self test for Blake2S-256}
const
  abc : array[0..2] of char8 = 'abc';
  digabc: TBlake2S_256Digest = (
            $50,$8C,$5E,$8C,$32,$7C,$14,$E2,$E1,$A7,$2B,$A3,$4E,$EB,$45,$2F,
            $37,$45,$8B,$20,$9E,$D6,$3A,$29,$4D,$99,$9B,$4C,$86,$67,$59,$82);
  dignil: TBlake2S_256Digest = (
            $69,$21,$7a,$30,$79,$90,$80,$94,$e1,$11,$21,$d0,$42,$35,$4a,$7c,
            $1f,$55,$b6,$48,$2c,$a1,$a5,$1e,$1b,$25,$0d,$fd,$1e,$d0,$ee,$f9);
var
  Digest: TBlake2S_256Digest;
begin
  Blaks256SelfTest := false;
  if not blake2s_selftest then exit;
  {from libtomcrypt-develop\src\hashes\blake2s.c}
  Blaks256Full(Digest, nil, 0);
  if not HashSameDigest(@Blaks256_Desc, PHashDigest(@Digest), PHashDigest(@dignil)) then exit;
  Blaks256Full(Digest, @abc, sizeof(abc));
  Blaks256SelfTest := HashSameDigest(@Blaks256_Desc, PHashDigest(@Digest), PHashDigest(@digabc));
end;


{---------------------------------------------------------------------------}
procedure Blaks256FullXL(var Digest: TBlake2S_256Digest; Msg: pointer; Len: longint);
  {-Blake2S-256 of Msg with init/update/final}
var
  Context: THashContext;
begin
  Blaks256Init(Context);
  Blaks256UpdateXL(Context, Msg, Len);
  Blaks256Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure Blaks256Full(var Digest: TBlake2S_256Digest; Msg: pointer; Len: word);
  {-Blake2S-256 of Msg with init/update/final}
begin
  Blaks256FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks256File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2S_256Digest; var buf; bsize: word; var Err: word);
  {-Blake2S-256 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @Blaks256_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(Blaks256_Desc, sizeof(Blaks256_Desc), 0);
    with Blaks256_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := BLAKE2S_BlockLen;
       HDigestlen:= sizeof(TBlake2S_256Digest);
       HInit     := Blaks256Init;
       HFinal    := Blaks256FinalEx;
       HUpdateXL := Blaks256UpdateXL;
       HAlgNum   := longint(_Blake2S_256);
       HName     := 'Blake2S-256';
       HPtrOID   := @Blaks256_OID;
       HLenOID   := 11;
       HFinalBit := Blaks256FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_Blake2S_256, @Blaks256_Desc);
end.
