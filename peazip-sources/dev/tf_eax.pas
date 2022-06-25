unit TF_EAX;

(*************************************************************************

 DESCRIPTION     :  Twofish EAX mode functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] EAX: A Conventional Authenticated-Encryption Mode,
                        M.Bellare, P.Rogaway, D.Wagner <http://eprint.iacr.org/2003/069>
                    [2] http://csrc.nist.gov/CryptoToolkit/modes/proposedmodes/eax/eax-spec.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.06.07  W.Ehrhardt  Initial version analog AES_EAX
 0.11     31.07.10  we          Longint ILen in TF_EAX_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2007-2010 Wolfgang Ehrhardt

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


uses TF_Base, TF_CTR, TF_OMAC;

type
  TTF_EAXContext = packed record
                      HdrOMAC : TTFContext; {Hdr OMAC1  context}
                      MsgOMAC : TTFContext; {Msg OMAC1  context}
                      ctr_ctx : TTFContext; {Msg TFCTR context }
                      NonceTag: TTFBlock;   {nonce tag         }
                      tagsize : word;       {tag size (unused) }
                      flags   : word;       {ctx flags (unused)}
                    end;


{$ifdef CONST}
function TF_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TTF_EAXContext): integer;
  {-Init hdr and msg OMACs, setp TFCTR with nonce tag}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function TF_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TTF_EAXContext): integer;
  {-Init hdr and msg OMACs, setp TFCTR with nonce tag}
{$endif}

function TF_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TTF_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Supply a message header. The header "grows" with each call}

function TF_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function TF_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure TF_EAX_Final(var tag: TTFBlock; var ctx: TTF_EAXContext);
  {$ifdef DLL} stdcall; {$endif}
  {-Compute EAX tag from context}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function TF_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TTF_EAXContext): integer;
  {-Init hdr and msg OMACs, setp TFCTR with nonce tag}
{$else}
function TF_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TTF_EAXContext): integer;
  {-Init hdr and msg OMACs, setp TFCTR with nonce tag}
{$endif}
var
  err: integer;
  t_n: TTFBlock;
begin
  fillchar(ctx, sizeof(ctx), 0);
  {Initialize OMAC context with key}
  err := TF_OMAC_Init(Key, KBits, ctx.HdrOMAC);
  if err=0 then begin
    {copy fresh context, first use MsgOMAC for nonce OMAC}
    ctx.MsgOMAC := ctx.HdrOMAC;
    fillchar(t_n, sizeof(t_n),0);
    err := TF_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
    if err=0 then err := TF_OMAC_Update(@nonce, nLen, ctx.MsgOMAC);
    if err=0 then TF_OMAC_Final(ctx.NonceTag, ctx.MsgOMAC);
    {inititialize TF-CTR context}
    if err=0 then err := TF_CTR_Init(Key, KBits, ctx.NonceTag, ctx.ctr_ctx);
    if err=0 then begin
      {initialize msg OMAC}
      ctx.MsgOMAC := ctx.HdrOMAC;
      t_n[TFBLKSIZE-1] := 2;
      err := TF_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
      {initialize header OMAC}
      t_n[TFBLKSIZE-1] := 1;
      if err=0 then err := TF_OMAC_Update(@t_n, sizeof(t_n), ctx.HdrOMAC);
    end;
  end;
  TF_EAX_Init := err;
end;



{---------------------------------------------------------------------------}
function TF_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TTF_EAXContext): integer;
  {-Supply a message header. The header "grows" with each call}
begin
  TF_EAX_Provide_Header := TF_OMAC_Update(Hdr, hLen, ctx.HdrOMAC);
end;


{---------------------------------------------------------------------------}
function TF_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {encrypt (and check for nil pointers)}
  err := TF_CTR_Encrypt(ptp, ctp, ILen, ctx.ctr_ctx);
  if err=0 then begin
    {OMAC1 ciphertext}
    err := TF_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  end;
  TF_EAX_Encrypt := err;
end;


{---------------------------------------------------------------------------}
function TF_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {OMAC1 ciphertext}
  err := TF_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  if err=0 then begin
    {decrypt}
    err := TF_CTR_Decrypt(ctp, ptp, ILen, ctx.ctr_ctx);
  end;
  TF_EAX_Decrypt := err;
end;


{---------------------------------------------------------------------------}
procedure TF_EAX_Final(var tag: TTFBlock; var ctx: TTF_EAXContext);
  {-Compute EAX tag from context}
var
  ht: TTFBlock;
begin
  TF_OMAC1_Final(ht, ctx.HdrOMAC);
  TF_OMAC1_Final(tag, ctx.MsgOMAC);
  TF_XorBlock(tag,ht,tag);
  TF_XorBlock(tag,ctx.NonceTag,tag);
end;



end.
