unit SP_CTR;

(*************************************************************************

 DESCRIPTION   : Serpent CTR mode functions
                 Because of buffering en/decrypting is associative
                 User can supply a custom increment function

 REQUIREMENTS  : TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA : ---

 MEMORY USAGE  : ---

 DISPLAY MODE  : ---

 REFERENCES    : B.Schneier, Applied Cryptography, 2nd ed., ch. 9.9

 REMARKS       : - If a predefined or user-supplied INCProc is used, it must
                   be set before using SP_CTR_Seek.
                 - SP_CTR_Seek may be time-consuming for user-defined
                   INCProcs, because this function is called many times.
                   See SP_CTR_Seek how to provide user-supplied short-cuts.

 WARNING       : - CTR mode demands that the same key / initial CTR pair is
                   never reused for encryption. This requirement is especially
                   important for the CTR_Seek function. If different data is
                   written to the same position there will be leakage of
                   information about the plaintexts. Therefore CTR_Seek should
                   normally be used for random reads only.

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_CTR
 0.11     22.06.08  we          Make IncProcs work with FPC -dDebug
 0.12     23.11.08  we          Uses BTypes
 0.13     01.08.10  we          Longint ILen in SP_CTR_En/Decrypt
 0.14     01.08.10  we          SP_CTR_Seek, SP_CTR_Seek64 via sp_seek.inc
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2008-2010 Wolfgang Ehrhardt

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
  BTypes, SP_Base;


{$ifdef CONST}

function  SP_CTR_Init(const Key; KeyBits: word; const CTR: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if inv. key size, encrypt CTR}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_CTR_Reset(const CTR: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt CTR}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  SP_CTR_Init(var Key; KeyBits: word; var CTR: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if inv. key size, encrypt CTR}

procedure SP_CTR_Reset(var CTR: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt CTR}

{$endif}

{$ifndef DLL}
function  SP_CTR_Seek({$ifdef CONST}const{$else}var{$endif} iCTR: TSPBlock;
                      SOL, SOH: longint; var ctx: TSPContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in CAM_CTR_Init.}
{$ifdef HAS_INT64}
function SP_CTR_Seek64(const iCTR: TSPBlock; SO: int64; var ctx: TSPContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in SP_CTR_Init.}
{$endif}
{$endif}


function  SP_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}
  {$ifdef DLL} stdcall; {$endif}

function  SP_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}
  {$ifdef DLL} stdcall; {$endif}

function  SP_SetIncProc(IncP: TSPIncProc; var ctx: TSPContext): integer;
  {-Set user supplied IncCTR proc}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_IncMSBFull(var CTR: TSPBlock);
  {-Increment CTR[15]..CTR[0]}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_IncLSBFull(var CTR: TSPBlock);
  {-Increment CTR[0]..CTR[15]}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_IncMSBPart(var CTR: TSPBlock);
  {-Increment CTR[15]..CTR[8]}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_IncLSBPart(var CTR: TSPBlock);
  {-Increment CTR[0]..CTR[7]}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
procedure SP_IncMSBPart(var CTR: TSPBlock);
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
procedure SP_IncLSBPart(var CTR: TSPBlock);
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
procedure SP_IncMSBFull(var CTR: TSPBlock);
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
procedure SP_IncLSBFull(var CTR: TSPBlock);
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
function SP_SetIncProc(IncP: TSPIncProc; var ctx: TSPContext): integer;
  {-Set user supplied IncCTR proc}
begin
  SP_SetIncProc := SP_Err_MultipleIncProcs;
  with ctx do begin
    {$ifdef FPC_ProcVar}
      if IncProc=nil then begin
        IncProc := IncP;
        SP_SetIncProc := 0;
      end;
    {$else}
      if @IncProc=nil then begin
        IncProc := IncP;
        SP_SetIncProc := 0;
      end;
    {$endif}
  end;
end;


{---------------------------------------------------------------------------}
{$ifdef CONST}
function SP_CTR_Init(const Key; KeyBits: word; const CTR: TSPBlock; var ctx: TSPContext): integer;
{$else}
function SP_CTR_Init(var Key; KeyBits: word; var CTR: TSPBlock; var ctx: TSPContext): integer;
{$endif}
  {-Serpent key expansion, error if inv. key size, encrypt CTR}
var
  err: integer;
begin
  err := SP_Init(Key, KeyBits, ctx);
  if err=0 then begin
    ctx.IV := CTR;
    {encrypt CTR}
    SP_Encrypt(ctx, CTR, ctx.buf);
  end;
  SP_CTR_Init := err;
end;


{---------------------------------------------------------------------------}
procedure SP_CTR_Reset({$ifdef CONST}const {$else} var {$endif}  CTR: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt CTR}
begin
  SP_Reset(ctx);
  ctx.IV := CTR;
  SP_Encrypt(ctx, CTR, ctx.buf);
end;


{---------------------------------------------------------------------------}
function SP_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}
begin
  SP_CTR_Encrypt := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      SP_CTR_Encrypt := SP_Err_NIL_Pointer; {nil pointer to block with nonzero length}
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      SP_CTR_Encrypt := SP_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  if ctx.blen=0 then begin
    {Handle full blocks first}
    while ILen>=SPBLKSIZE do with ctx do begin
      {Cipher text = plain text xor encr(CTR)}
      SP_XorBlock(PSPBlock(ptp)^, buf, PSPBlock(ctp)^);
      inc(Ptr2Inc(ptp), SPBLKSIZE);
      inc(Ptr2Inc(ctp), SPBLKSIZE);
      dec(ILen, SPBLKSIZE);
      {use SP_IncMSBFull if IncProc=nil}
      {$ifdef FPC_ProcVar}
        if IncProc=nil then SP_IncMSBFull(IV) else IncProc(IV);
      {$else}
        if @IncProc=nil then SP_IncMSBFull(IV) else IncProc(IV);
      {$endif}
      SP_Encrypt(ctx, IV, buf);
    end;
  end;

  {Handle remaining bytes}
  while ILen>0 do with ctx do begin
    {Refill buffer with encrypted CTR}
    if bLen>=SPBLKSIZE then begin
      {use SP_IncMSBFull if IncProc=nil}
      {$ifdef FPC_ProcVar}
        if IncProc=nil then SP_IncMSBFull(IV) else IncProc(IV);
      {$else}
        if @IncProc=nil then SP_IncMSBFull(IV) else IncProc(IV);
      {$endif}
      SP_Encrypt(ctx, IV, buf);
      bLen := 0;
    end;
    {Cipher text = plain text xor encr(CTR)}
    pByte(ctp)^ := buf[bLen] xor pByte(ptp)^;
    inc(bLen);
    inc(Ptr2Inc(ptp));
    inc(Ptr2Inc(ctp));
    dec(ILen);
  end;
end;


{---------------------------------------------------------------------------}
function SP_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}
begin
  {Decrypt = encrypt for CTR mode}
  SP_CTR_Decrypt := SP_CTR_Encrypt(ctp, ptp, ILen, ctx);
end;


{$ifndef DLL}
  {$i sp_seek.inc}
{$endif}

end.
