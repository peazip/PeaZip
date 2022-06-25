unit AES_EAX;

(*************************************************************************

 DESCRIPTION     :  AES EAX mode functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  Stack: local EAX ctx in AES_EAX_Enc_Auth/AES_EAX_Dec_Veri

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] EAX: A Conventional Authenticated-Encryption Mode,
                        M.Bellare, P.Rogaway, D.Wagner <http://eprint.iacr.org/2003/069>
                    [2] http://csrc.nist.gov/CryptoToolkit/modes/proposedmodes/eax/eax-spec.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     11.06.04  we          initial version (BP7+)
 0.11     12.06.04  we          uses BLKSIZE constant
 0.12     13.06.04  we          TP5/5.5/6
 0.13     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.14     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.15     09.07.06  we          Checked: D9-D10
 0.16     14.06.07  we          Type TAES_EAXContext
 0.17     20.07.08  we          All-in-one functions AES_EAX_Enc_Auth/AES_EAX_Dec_Veri
 0.18     01.08.08  we          Fix for loop in Internal_Veri
 0.19     02.08.08  we          Local ctx for AES_EAX_Enc_Auth/AES_EAX_Dec_Veri
 0.20     06.08.08  we          Suppress D4+ warning
 0.21     09.08.08  we          Check tLen in ANU_EAX_Dec_Veri
 0.22     16.11.08  we          Use Ptr2Inc, pByte from BTypes
 0.23     27.07.10  we          Longint ILen in AES_EAX_En/Decrypt
 0.25     31.08.15  we          constant time compare in Internal_Veri
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2004-2015 Wolfgang Ehrhardt

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

{Use TEAXContext for legacy AES_EAX source codes. The new}
{context type TAES_EAXContext should be used instead.}
{.$define Support_Old_AES_EAXContext_Type}


interface

uses
  BTypes, AES_Type, AES_Base, AES_CTR, AES_OMAC;


type
  TAES_EAXContext = packed record
                      HdrOMAC : TAESContext; {Hdr OMAC1  context}
                      MsgOMAC : TAESContext; {Msg OMAC1  context}
                      ctr_ctx : TAESContext; {Msg AESCTR context}
                      NonceTag: TAESBlock;   {nonce tag         }
                      tagsize : word;        {tag size (unused) }
                      flags   : word;        {ctx flags (unused)}
                    end;
    {$ifdef Support_Old_AES_EAXContext_Type}
      TEAXContext = TAES_EAXContext;
    {$endif}


{$ifdef CONST}
function AES_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TAES_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}

{$else}
function AES_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TAES_EAXContext): integer;
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}
{$endif}

function AES_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TAES_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Supply a message header. The header "grows" with each call}

function AES_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function AES_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure AES_EAX_Final(var tag: TAESBlock; var ctx: TAES_EAXContext);
  {$ifdef DLL} stdcall; {$endif}
  {-Compute EAX tag from context}

function AES_EAX_Enc_Auth(var tag: TAESBlock;                     {Tag record}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
        {$ifdef CONST}const{$else}var{$endif} nonce; nLen: word;  {nonce: address / length}
                              Hdr: pointer; hLen: word;           {header: address / length}
                              ptp: pointer; pLen: longint;        {plaintext: address / length}
                              ctp: pointer                        {ciphertext: address}
                                ): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-All-in-one call to encrypt/authenticate}

function AES_EAX_Dec_Veri(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
        {$ifdef CONST}const{$else}var{$endif} nonce; nLen: word;  {nonce: address / length}
                              Hdr: pointer; hLen: word;           {header: address / length}
                              ctp: pointer; cLen: longint;        {ciphertext: address / length}
                              ptp: pointer                        {plaintext: address}
                                ): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function AES_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TAES_EAXContext): integer;
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}
{$else}
function AES_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TAES_EAXContext): integer;
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}
{$endif}
var
  err: integer;
  t_n: TAESBlock;
begin
  fillchar(ctx, sizeof(ctx), 0);
  {Initialize OMAC context with key}
  err := AES_OMAC_Init(Key, KBits, ctx.HdrOMAC);
  if err=0 then begin
    {copy fresh context, first use MsgOMAC for nonce OMAC}
    ctx.MsgOMAC := ctx.HdrOMAC;
    fillchar(t_n, sizeof(t_n), 0);
    err := AES_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
    if err=0 then err := AES_OMAC_Update(@nonce, nLen, ctx.MsgOMAC);
    if err=0 then AES_OMAC_Final(ctx.NonceTag, ctx.MsgOMAC);
    {inititialize AES-CTR context}
    if err=0 then err := AES_CTR_Init(Key, KBits, ctx.NonceTag, ctx.ctr_ctx);
    if err=0 then begin
      {initialize msg OMAC}
      ctx.MsgOMAC := ctx.HdrOMAC;
      t_n[AESBLKSIZE-1] := 2;
      err := AES_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
      {initialize header OMAC}
      t_n[AESBLKSIZE-1] := 1;
      if err=0 then err := AES_OMAC_Update(@t_n, sizeof(t_n), ctx.HdrOMAC);
    end;
  end;
  AES_EAX_Init := err;
end;


{---------------------------------------------------------------------------}
function AES_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TAES_EAXContext): integer;
  {-Supply a message header. The header "grows" with each call}
begin
  AES_EAX_Provide_Header := AES_OMAC_Update(Hdr, hLen, ctx.HdrOMAC);
end;


{---------------------------------------------------------------------------}
function AES_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {encrypt (and check for nil pointers)}
  err := AES_CTR_Encrypt(ptp, ctp, ILen, ctx.ctr_ctx);
  if err=0 then begin
    {OMAC1 ciphertext}
    err := AES_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  end;
  AES_EAX_Encrypt := err;
end;


{---------------------------------------------------------------------------}
function AES_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {OMAC1 ciphertext}
  err := AES_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  if err=0 then begin
    {decrypt}
    err := AES_CTR_Decrypt(ctp, ptp, ILen, ctx.ctr_ctx);
  end;
  AES_EAX_Decrypt := err;
end;


{---------------------------------------------------------------------------}
procedure AES_EAX_Final(var tag: TAESBlock; var ctx: TAES_EAXContext);
  {-Compute EAX tag from context}
var
  ht: TAESBlock;
begin
  AES_OMAC1_Final(ht, ctx.HdrOMAC);
  AES_OMAC1_Final(tag, ctx.MsgOMAC);
  AES_XorBlock(tag,ht,tag);
  AES_XorBlock(tag,ctx.NonceTag,tag);
end;


{---------------------------------------------------------------------------}
function AES_EAX_Enc_Auth(var tag: TAESBlock;                     {Tag record}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
        {$ifdef CONST}const{$else}var{$endif} nonce; nLen: word;  {nonce: address / length}
                              Hdr: pointer; hLen: word;           {header: address / length}
                              ptp: pointer; pLen: longint;        {plaintext: address / length}
                              ctp: pointer                        {ciphertext: address}
                                ): integer;
  {-All-in-one call to encrypt/authenticate}
var
  err : integer;
  ILen: word;
  ctx : TAES_EAXContext;
const
  CHUNK=$8000;
begin
  {$ifdef BIT16}
    if (pLen>$FFFF) or (ofs(ptp^)+pLen>$FFFF) or (ofs(ctp^)+pLen>$FFFF)  then begin
      AES_EAX_Enc_Auth := AES_Err_EAX_Inv_Text_Length;
      exit;
    end;
  {$endif}
  if (ptp=nil) or (ctp=nil) then begin
    if pLen>0 then begin
      AES_EAX_Enc_Auth := AES_Err_NIL_Pointer;
      exit;
    end;
  end;
  err := AES_EAX_Init(Key, KBits, nonce, nLen, ctx);
  if err=0 then err := AES_EAX_Provide_Header(Hdr, hLen, ctx);
  while (err=0) and (pLen>0) do begin
    if pLen>CHUNK then ILen := CHUNK else ILen := pLen;
    err := AES_EAX_Encrypt(ptp, ctp, ILen, ctx);
    inc(Ptr2Inc(ptp), ILen);
    inc(Ptr2Inc(ctp), ILen);
    dec(pLen, ILen);
  end;
  if err=0 then AES_EAX_Final(tag, ctx);
  fillchar(ctx, sizeof(ctx), 0);
  AES_EAX_Enc_Auth := err;
end;


{---------------------------------------------------------------------------}
function Internal_Veri(var ctx: TAES_EAXContext; ptag: pointer; tLen: word;
                           ctp: pointer; cLen: longint): integer;
  {-calculate and verify tLen bytes of ptag^, performs OMAC phase of EAX}
var
  err,i: integer;
  ILen: word;
  atag: TAESBlock;
  diff: byte;
const
  CHUNK=$8000;
begin
  {internal, assumes ctx is initialized, nonce and header}
  {are processed, cLen, tLen are with in allowed ranges}
  err := 0;
  {calculate the ciphertext OMAC}
  while (err=0) and (cLen>0) do begin
    if cLen>CHUNK then ILen := CHUNK else ILen := cLen;
    err := AES_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
    inc(Ptr2Inc(ctp), ILen);
    dec(cLen, ILen);
  end;
  if (err=0) and (tLen>0) then begin
    {calculate actual tag and compare with supplied tag}
    AES_EAX_Final(atag, ctx);
    diff := 0;
    for i:=0 to pred(tLen) do begin
      diff := diff or (pByte(ptag)^ xor atag[i]);
      inc(Ptr2Inc(ptag));
    end;
    err := (((integer(diff)-1) shr 8) and 1)-1;  {0 compare, -1 otherwise}
    err := err and AES_Err_EAX_Verify_Tag;
  end;
  Internal_Veri := err;
end;



{---------------------------------------------------------------------------}
function AES_EAX_Dec_Veri(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
        {$ifdef CONST}const{$else}var{$endif} Key; KBits: word;   {key and bitlength of key}
        {$ifdef CONST}const{$else}var{$endif} nonce; nLen: word;  {nonce: address / length}
                              Hdr: pointer; hLen: word;           {header: address / length}
                              ctp: pointer; cLen: longint;        {ciphertext: address / length}
                              ptp: pointer                        {plaintext: address}
                                ): integer;
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}
var
  err : integer;
  ILen: word;
  ctx : TAES_EAXContext;
const
  CHUNK=$8000;
begin
  {$ifdef BIT16}
    if (cLen>$FFFF) or (ofs(ptp^)+cLen>$FFFF) or (ofs(ctp^)+cLen>$FFFF)  then begin
      AES_EAX_Dec_Veri := AES_Err_EAX_Inv_Text_Length;
      exit;
    end;
  {$endif}
  if (ptp=nil) or (ctp=nil) then begin
    if cLen>0 then begin
      AES_EAX_Dec_Veri := AES_Err_NIL_Pointer;
      exit;
    end;
  end;
  if tLen>AESBLKSIZE then begin
    AES_EAX_Dec_Veri := AES_Err_EAX_Inv_TAG_Length;
    exit;
  end;
  err := AES_EAX_Init(Key, KBits, nonce, nLen, ctx);
  if err=0 then err := AES_EAX_Provide_Header(Hdr, hLen, ctx);
  if err=0 then begin
    {First pass through ciphertext, calculated and compare tag}
    err := Internal_Veri(ctx, ptag, tLen, ctp, cLen);
    {if error or verification failed, decrypt loop is skipped}
    while (err=0) and (cLen>0) do begin
      if cLen>CHUNK then ILen := CHUNK else ILen := cLen;
      err := AES_CTR_Decrypt(ctp, ptp, ILen, ctx.ctr_ctx);
      inc(Ptr2Inc(ptp), ILen);
      inc(Ptr2Inc(ctp), ILen);
      dec(cLen, ILen);
    end;
  end;
  fillchar(ctx, sizeof(ctx), 0);
  AES_EAX_Dec_Veri:= err;
end;


end.
