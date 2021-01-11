unit AES_CTR;

(*************************************************************************

 DESCRIPTION   : AES CTR mode functions
                 Because of buffering en/decrypting is associative
                 User can supply a custom increment function

 REQUIREMENTS  : TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA : ---

 MEMORY USAGE  : ---

 DISPLAY MODE  : ---

 REFERENCES    : [3] http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
                 [1] http://csrc.nist.gov/fips/fips-197.pdf

 REMARKS       : - If a predefined or user-supplied INCProc is used, it must
                   be set before using AES_CTR_Seek.
                 - AES_CTR_Seek may be time-consuming for user-defined
                   INCProcs, because this function is called many times.
                   See AES_CTR_Seek how to provide user-supplied short-cuts.

 WARNING       : - CTR mode demands that the same key / initial CTR pair is
                   never reused for encryption. This requirement is especially
                   important for the CTR_Seek function. If different data is
                   written to the same position there will be leakage of
                   information about the plaintexts. Therefore CTR_Seek should
                   normally be used for random reads only.
                 - Default IncProc changed to IncMSBFull in V0.30, for old
                   defaults call AES_SetIncProc(AES_IncMSBPart,.) after AES_CTR_Init
                   or (less flexible) set DefaultIncMSBPart := true

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.08.03  we          initial version
 0.20     15.09.03  we          use IncProc, with IncLSB, IncMSB
 0.21     20.09.03  we          fixed obscure FPC @ bug
 0.22     21.09.03  we          functions, error codes
 0.23     27.09.03  we          FPC/go32v2
 0.24     03.10.03  we          3-para encr/decr
 0.25     05.10.03  we          STD.INC, TP5-6
 0.26     05.10.03  we          SetIncProc, Init without IncP
 0.27     05.10.03  we          Bugfix for FPC: @ and IncProc
 0.28     01.01.04  we          Handle full blocks first
 0.30     11.06.04  we          4 IncProcs, default IncMSBFull
 0.31     12.06.04  we          uses BLKSIZE constant
 0.32     12.06.04  we          check for nil pointers
 0.33     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.34     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.35     01.12.04  we          AES_ prefix for increment routines
 0.36     09.07.06  we          Checked: D9-D10
 0.37     23.06.07  we          Use conditional define FPC_ProcVar
 0.38     21.06.08  we          Make IncProcs work with FPC -dDebug
 0.39     16.11.08  we          Use Ptr2Inc, pByte from BTypes
 0.40     19.06.10  we          Initial version of AES_CTR_Seek
 0.41     20.06.10  we          AES_CTR_Seek: calculate IV if IncProc is known
 0.42     20.06.10  we          AES_CTR_Seek64
 0.43     21.06.10  we          AES_CTR_Seek: Fix loop for user-defined IncProcs
 0.44     27.07.10  we          Longint ILen in AES_CTR_En/Decrypt
 0.45     31.07.10  we          AES_CTR_Seek source moved to aes_seek.inc
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2010 Wolfgang Ehrhardt

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

interface


uses
  BTypes, AES_Type, AES_Base, AES_Encr;


const
  DefaultIncMSBPart: boolean = false;  {if true use AES_IncMSBPart as default}


{$ifdef CONST}
function  AES_CTR_Init(const Key; KeyBits: word; const CTR: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if inv. key size, encrypt CTR}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function  AES_CTR_Init(var Key; KeyBits: word; var CTR: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if inv. key size, encrypt CTR}
{$endif}


{$ifndef DLL}
function  AES_CTR_Seek({$ifdef CONST}const{$else}var{$endif} iCTR: TAESBlock;
                       SOL, SOH: longint; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in AES_CTR_Init.}
{$ifdef HAS_INT64}
function AES_CTR_Seek64(const iCTR: TAESBlock; SO: int64; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in AES_CTR_Init.}
{$endif}
{$endif}


function  AES_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}
  {$ifdef DLL} stdcall; {$endif}

function  AES_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}
  {$ifdef DLL} stdcall; {$endif}

function  AES_SetIncProc(IncP: TIncProc; var ctx: TAESContext): integer;
  {-Set user supplied IncCTR proc}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_IncMSBFull(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[0]}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_IncLSBFull(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[15]}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_IncMSBPart(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[8]}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_IncLSBPart(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[7]}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
procedure AES_IncMSBPart(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[8]}
var
  j: integer;
begin
  for j:=15 downto 8 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure AES_IncLSBPart(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[7]}
var
  j: integer;
begin
  for j:=0 to 7 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure AES_IncMSBFull(var CTR: TAESBlock);
  {-Increment CTR[15]..CTR[0]}
var
  j: integer;
begin
  for j:=15 downto 0 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure AES_IncLSBFull(var CTR: TAESBlock);
  {-Increment CTR[0]..CTR[15]}
var
  j: integer;
begin
  for j:=0 to 15 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function AES_SetIncProc(IncP: TIncProc; var ctx: TAESContext): integer;
  {-Set user supplied IncCTR proc}
begin
  AES_SetIncProc := AES_Err_MultipleIncProcs;
  with ctx do begin
    {$ifdef FPC_ProcVar}
      if IncProc=nil then begin
        IncProc := IncP;
        AES_SetIncProc := 0;
      end;
    {$else}
      if @IncProc=nil then begin
        IncProc := IncP;
        AES_SetIncProc := 0;
      end;
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
{$ifdef CONST}
function AES_CTR_Init(const Key; KeyBits: word; const CTR: TAESBlock; var ctx: TAESContext): integer;
{$else}
function AES_CTR_Init(var Key; KeyBits: word; var CTR: TAESBlock; var ctx: TAESContext): integer;
{$endif}
  {-AES key expansion, error if inv. key size, encrypt CTR}
var
  err: integer;
begin
  {AES key expansion, error if inv. key size}
  err := AES_Init_Encr(Key, KeyBits, ctx);
  if (err=0) and DefaultIncMSBPart then begin
    {$ifdef FPC_ProcVar}
      err := AES_SetIncProc(@AES_IncMSBPart, ctx);
    {$else}
      err := AES_SetIncProc(AES_IncMSBPart, ctx);
    {$endif}
  end;
  if err=0 then begin
    ctx.IV := CTR;
    {encrypt CTR}
    AES_Encrypt(ctx, CTR, ctx.buf);
  end;
  AES_CTR_Init := err;
end;


{---------------------------------------------------------------------------}
function AES_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}
begin
  AES_CTR_Encrypt := 0;

  if ctx.Decrypt<>0 then begin
    AES_CTR_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CTR_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CTR_Encrypt := AES_Err_NIL_Pointer; {nil pointer to block with nonzero length}
      exit;
    end;
  end;

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=AESBLKSIZE do with ctx do begin
      {Cipher text = plain text xor encr(CTR), cf. [3] 6.5}
      AES_XorBlock(PAESBlock(ptp)^, buf, PAESBlock(ctp)^);
      inc(Ptr2Inc(ptp), AESBLKSIZE);
      inc(Ptr2Inc(ctp), AESBLKSIZE);
      dec(ILen, AESBLKSIZE);
      {use AES_IncMSBFull if IncProc=nil}
      {$ifdef FPC_ProcVar}
        if IncProc=nil then AES_IncMSBFull(IV) else IncProc(IV);
      {$else}
        if @IncProc=nil then AES_IncMSBFull(IV) else IncProc(IV);
      {$endif}
      AES_Encrypt(ctx, IV, buf);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Refill buffer with encrypted CTR}
    if bLen>=AESBLKSIZE then begin
      {use AES_IncMSBFull if IncProc=nil}
      {$ifdef FPC_ProcVar}
        if IncProc=nil then AES_IncMSBFull(IV) else IncProc(IV);
      {$else}
        if @IncProc=nil then AES_IncMSBFull(IV) else IncProc(IV);
      {$endif}
      AES_Encrypt(ctx, IV, buf);
      bLen := 0;
    end;
    {Cipher text = plain text xor encr(CTR), cf. [3] 6.5}
    pByte(ctp)^ := buf[bLen] xor pByte(ptp)^;
    inc(bLen);
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;


{---------------------------------------------------------------------------}
function AES_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}
begin
  {Decrypt = encrypt for CTR mode}
  AES_CTR_Decrypt := AES_CTR_Encrypt(ctp, ptp, ILen, ctx);
end;


{$ifndef DLL}
  {$i aes_seek.inc}
{$endif}


end.
