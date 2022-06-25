unit Blaks224;

{Blake2s-224 - 224 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  Blake2s-224 - 224 bit Secure Hash Function

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
 0.10     17.05.17  W.Ehrhardt  Initial BP version using Blaks256 layout
 0.11     29.11.17  we          Blaks224File - fname: string

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


procedure Blaks224Init(var Context: THashContext);
  {-initialize context}

procedure Blaks224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure Blaks224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure Blaks224Final(var Context: THashContext; var Digest: TBlake2S_224Digest);
  {-finalize Blake2S-224 calculation, clear context}

procedure Blaks224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2S-224 calculation, clear context}

function  Blaks224SelfTest: boolean;
  {-self test for Blake2S-224}

procedure Blaks224Full(var Digest: TBlake2S_224Digest; Msg: pointer; Len: word);
  {-Blake2S-224 of Msg with init/update/final}

procedure Blaks224FullXL(var Digest: TBlake2S_224Digest; Msg: pointer; Len: longint);
  {-Blake2S-224 of Msg with init/update/final}

procedure Blaks224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2S_224Digest; var buf; bsize: word; var Err: word);
  {-Blake2S-224 of file, buf: buffer with at least bsize bytes}


implementation


{$ifdef BIT16}
  {$F+}
{$endif}

{---------------------------------------------------------------------------}
procedure Blaks224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize calculation with bitlen bits from BData, clear context}
begin
  {Just ignore the final bits}
  Blaks224FinalEx(Context, Digest);
end;


const
  Blaks224_OID : TOID_Vec = (1,3,6,1,4,1,1722,12,2,2,7);  {Len=11}


{$ifndef VER5X}
const
  Blaks224_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : BLAKE2S_BlockLen;
               HDigestlen: sizeof(TBlake2S_224Digest);
             {$ifdef FPC_ProcVar}
               HInit     : @Blaks224Init;
               HFinal    : @Blaks224FinalEx;
               HUpdateXL : @Blaks224UpdateXL;
             {$else}
               HInit     : Blaks224Init;
               HFinal    : Blaks224FinalEx;
               HUpdateXL : Blaks224UpdateXL;
             {$endif}
               HAlgNum   : longint(_Blake2S_224);
               HName     : 'Blake2S-224';
               HPtrOID   : @Blaks224_OID;
               HLenOID   : 11;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @Blaks224FinalBitsEx;
             {$else}
               HFinalBit : Blaks224FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  Blaks224_Desc: THashDesc;
{$endif}


{$ifdef BIT16}
  {$F-}
{$endif}


{---------------------------------------------------------------------------}
procedure Blaks224Init(var Context: THashContext);
  {-initialize context}
var
  err: integer;
begin
  {Clear context}
  err := blake2s_Init(blake2s_ctx(Context),nil,0,sizeof(TBlake2S_224Digest));
  if err<>0 then begin
    {$ifdef debug}
      {This should not happen, because and error can only be returned}
      {if key/digest lengths are out of range, 0 and 28 are OK}
      writeln('Blaks224Init error = ',err);
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
procedure Blaks224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  blake2s_update(blake2s_ctx(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks224Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  blake2s_update(blake2s_ctx(Context), Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Blake2S-224 calculation, clear context}
var
  b2sd: TBlake2SDigest absolute Digest;
begin
  fillchar(Digest, sizeof(Digest), 0);
  Blake2s_Final(blake2s_ctx(Context), b2sd);
end;


{---------------------------------------------------------------------------}
procedure Blaks224Final(var Context: THashContext; var Digest: TBlake2S_224Digest);
  {-finalize Blake2S-224 calculation, clear context}
var
  b2sd: TBlake2SDigest;
begin
  fillchar(b2sd, sizeof(b2sd), 0);
  Blake2s_Final(blake2s_ctx(Context), b2sd);
  move(b2sd, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function Blaks224SelfTest: boolean;
  {-self test for Blake2S-224}
const
  abc : array[0..2] of char8 = 'abc';
  digabc: TBlake2S_224Digest = (
            $0b,$03,$3f,$c2,$26,$df,$7a,$bd,$e2,$9f,$67,$a0,$5d,$3d,
            $c6,$2c,$f2,$71,$ef,$3d,$fe,$a4,$d3,$87,$40,$7f,$bd,$55);
  dignil: TBlake2S_224Digest = (
            $1f,$a1,$29,$1e,$65,$24,$8b,$37,$b3,$43,$34,$75,$b2,$a0,
            $dd,$63,$d5,$4a,$11,$ec,$c4,$e3,$e0,$34,$e7,$bc,$1e,$f4);
var
  Digest: TBlake2S_224Digest;
begin
  Blaks224SelfTest := false;
  if not blake2s_selftest then exit;
  {from libtomcrypt-develop\src\hashes\blake2s.c}
  Blaks224Full(Digest, nil, 0);
  if not HashSameDigest(@Blaks224_Desc, PHashDigest(@Digest), PHashDigest(@dignil)) then exit;
  Blaks224Full(Digest, @abc, sizeof(abc));
  Blaks224SelfTest := HashSameDigest(@Blaks224_Desc, PHashDigest(@Digest), PHashDigest(@digabc));
end;


{---------------------------------------------------------------------------}
procedure Blaks224FullXL(var Digest: TBlake2S_224Digest; Msg: pointer; Len: longint);
  {-Blake2S-224 of Msg with init/update/final}
var
  Context: THashContext;
begin
  Blaks224Init(Context);
  Blaks224UpdateXL(Context, Msg, Len);
  Blaks224Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure Blaks224Full(var Digest: TBlake2S_224Digest; Msg: pointer; Len: word);
  {-Blake2S-224 of Msg with init/update/final}
begin
  Blaks224FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Blaks224File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TBlake2S_224Digest; var buf; bsize: word; var Err: word);
  {-Blake2S-224 of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @Blaks224_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(Blaks224_Desc, sizeof(Blaks224_Desc), 0);
    with Blaks224_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := BLAKE2S_BlockLen;
       HDigestlen:= sizeof(TBlake2S_224Digest);
       HInit     := Blaks224Init;
       HFinal    := Blaks224FinalEx;
       HUpdateXL := Blaks224UpdateXL;
       HAlgNum   := longint(_Blake2S_224);
       HName     := 'Blake2S-224';
       HPtrOID   := @Blaks224_OID;
       HLenOID   := 11;
       HFinalBit := Blaks224FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_Blake2S_224, @Blaks224_Desc);
end.
