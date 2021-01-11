unit SP_EAX;

(*************************************************************************

 DESCRIPTION     :  Serpent EAX mode functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] EAX: A Conventional Authenticated-Encryption Mode,
                        M.Bellare, P.Rogaway, D.Wagner <http://eprint.iacr.org/2003/069>
                    [2] http://csrc.nist.gov/CryptoToolkit/modes/proposedmodes/eax/eax-spec.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_EAX
 0.11     01.08.10  we          Longint ILen in SP_EAX_En/Decrypt
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


uses SP_Base, SP_CTR, SP_OMAC;

type
  TSP_EAXContext = packed record
                      HdrOMAC : TSPContext; {Hdr OMAC1  context}
                      MsgOMAC : TSPContext; {Msg OMAC1  context}
                      ctr_ctx : TSPContext; {Msg SPCTR context }
                      NonceTag: TSPBlock;   {nonce tag         }
                      tagsize : word;       {tag size (unused) }
                      flags   : word;       {ctx flags (unused)}
                    end;


{$ifdef CONST}
function SP_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TSP_EAXContext): integer;
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function SP_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TSP_EAXContext): integer;
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}
{$endif}

function SP_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TSP_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Supply a message header. The header "grows" with each call}

function SP_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function SP_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {$ifdef DLL} stdcall; {$endif}
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure SP_EAX_Final(var tag: TSPBlock; var ctx: TSP_EAXContext);
  {$ifdef DLL} stdcall; {$endif}
  {-Compute EAX tag from context}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
function SP_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TSP_EAXContext): integer;
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}
{$else}
function SP_EAX_Init(var Key; KBits: word; var nonce; nLen: word; var ctx: TSP_EAXContext): integer;
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}
{$endif}
var
  err: integer;
  t_n: TSPBlock;
begin
  fillchar(ctx, sizeof(ctx), 0);
  {Initialize OMAC context with key}
  err := SP_OMAC_Init(Key, KBits, ctx.HdrOMAC);
  if err=0 then begin
    {copy fresh context, first use MsgOMAC for nonce OMAC}
    ctx.MsgOMAC := ctx.HdrOMAC;
    fillchar(t_n, sizeof(t_n),0);
    err := SP_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
    if err=0 then err := SP_OMAC_Update(@nonce, nLen, ctx.MsgOMAC);
    if err=0 then SP_OMAC_Final(ctx.NonceTag, ctx.MsgOMAC);
    {inititialize SP-CTR context}
    if err=0 then err := SP_CTR_Init(Key, KBits, ctx.NonceTag, ctx.ctr_ctx);
    if err=0 then begin
      {initialize msg OMAC}
      ctx.MsgOMAC := ctx.HdrOMAC;
      t_n[SPBLKSIZE-1] := 2;
      err := SP_OMAC_Update(@t_n, sizeof(t_n), ctx.MsgOMAC);
      {initialize header OMAC}
      t_n[SPBLKSIZE-1] := 1;
      if err=0 then err := SP_OMAC_Update(@t_n, sizeof(t_n), ctx.HdrOMAC);
    end;
  end;
  SP_EAX_Init := err;
end;



{---------------------------------------------------------------------------}
function SP_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TSP_EAXContext): integer;
  {-Supply a message header. The header "grows" with each call}
begin
  SP_EAX_Provide_Header := SP_OMAC_Update(Hdr, hLen, ctx.HdrOMAC);
end;


{---------------------------------------------------------------------------}
function SP_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {encrypt (and check for nil pointers)}
  err := SP_CTR_Encrypt(ptp, ctp, ILen, ctx.ctr_ctx);
  if err=0 then begin
    {OMAC1 ciphertext}
    err := SP_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  end;
  SP_EAX_Encrypt := err;
end;


{---------------------------------------------------------------------------}
function SP_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}
var
  err: integer;
begin
  {OMAC1 ciphertext}
  err := SP_OMAC_Update(ctp, ILen, ctx.MsgOMAC);
  if err=0 then begin
    {decrypt}
    err := SP_CTR_Decrypt(ctp, ptp, ILen, ctx.ctr_ctx);
  end;
  SP_EAX_Decrypt := err;
end;


{---------------------------------------------------------------------------}
procedure SP_EAX_Final(var tag: TSPBlock; var ctx: TSP_EAXContext);
  {-Compute EAX tag from context}
var
  ht: TSPBlock;
begin
  SP_OMAC1_Final(ht, ctx.HdrOMAC);
  SP_OMAC1_Final(tag, ctx.MsgOMAC);
  SP_XorBlock(tag,ht,tag);
  SP_XorBlock(tag,ctx.NonceTag,tag);
end;



end.
