unit fcrypts;


(*************************************************************************

 DESCRIPTION     :  File crypt/authenticate unit

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  Make sure to change hdr.salt for subsequent calls to FCA_XXX_init

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.03.16  G.Tani      Based on W.Ehrhardt fcrypta 0.44 modified to use Serpent
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2003-2008 Wolfgang Ehrhardt

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
  BTypes,
  {$ifdef USEDLL}
     {$ifdef VirtualPascal}
       CH_INTV, SP_INTV;
     {$else}
       CH_INTF, SP_INTF;
     {$endif}
  {$else}
    Hash, HMAC, SHA1, KDF, SP_base, SP_CTR, SP_EAX;
  {$endif}


const
  C_FCS_Sig = $FC;

type
  TFCSSalt  = array[0..2] of longint;         {96 Bit salt}
  TFCSHdr   = packed record
                FCSsig: byte;                 {Sig  $FC}
                Flags : byte;                 {High $A0, Bit0: 1=EAX, 0:HMAC-CTR}
                Salt  : TFCSSalt;
                PW_Ver: word;
              end;


type
  TFCS_HMAC_Context  = record
                         SP_ctx  : TspContext;   {crypt context}
                         hmac_ctx : THMAC_Context;  {auth  context}
                       end;

type
  TFCS_AuthBlock = array[0..15] of byte;

const
  KeyIterations  : word = 1000;          {Iterations in KeyDeriv}


function FCS_HMAC_init(var cx: TFCS_HMAC_Context; pPW: pointer; pLen: word; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCS_HMAC_initS(var cx: TFCS_HMAC_Context; sPW: Str255; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCS_HMAC_encrypt(var cx: TFCS_HMAC_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}

function FCS_HMAC_decrypt(var cx: TFCS_HMAC_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}

procedure FCS_HMAC_final(var cx: TFCS_HMAC_Context; var auth: TFCS_AuthBlock);
  {-return final HMAC-SHA1-128 digest}


function FCS_EAX_init(var cx: TSP_EAXContext; pPW: pointer; pLen: word; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCS_EAX_initS(var cx: TSP_EAXContext; sPW: Str255; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCS_EAX_encrypt(var cx: TSP_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}

function FCS_EAX_decrypt(var cx: TSP_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}

procedure FCS_EAX_final(var cx: TSP_EAXContext; var auth: TFCS_AuthBlock);
  {-return final EAX tag}


implementation


type
  TXKey = packed record                       {eXtended key for PBKDF}
            ak: packed array[0..15] of byte;  {SP  128 bit key      }
            hk: packed array[0..15] of byte;  {HMAC key / EAX nonce  }
            pv: word;                         {password verifier     }
          end;


{---------------------------------------------------------------------------}
function FCS_HMAC_init(var cx: TFCS_HMAC_Context; pPW: pointer; pLen: word; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TXKey;
  CTR : TSPBlock;
  psha: PHashDesc;
  Err : integer;
begin
  {CTR=0, random/uniqness from hdr.salt}
  fillchar(CTR, sizeof(CTR), 0);

  {derive the SP, HMAC keys and pw verifier}
  psha := FindHash_by_ID(_SHA1);
  Err  := pbkdf2(psha, pPW, pLen, @hdr.salt, sizeof(TFCSSalt), KeyIterations, XKey, sizeof(XKey));

  {init SP CTR mode with ak}
  if Err=0 then Err := SP_CTR_Init(XKey.ak, 8*sizeof(XKey.ak), CTR, cx.SP_ctx);

  {exit if any error}
  FCS_HMAC_init := Err;
  if Err<>0 then exit;

  {initialise HMAC with hk, here psha is valid}
  hmac_init(cx.hmac_ctx, psha, @XKey.hk, sizeof(XKey.hk));

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCSSig := C_FCS_Sig;
  hdr.Flags  := $A0;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCS_HMAC_initS(var cx: TFCS_HMAC_Context; sPW: Str255; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCS_HMAC_initS := FCS_HMAC_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCS_HMAC_encrypt(var cx: TFCS_HMAC_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}
begin
  FCS_HMAC_encrypt := SP_CTR_Encrypt(@data, @data, dLen, cx.SP_ctx);
  hmac_update(cx.hmac_ctx, @data, dLen);
end;


{---------------------------------------------------------------------------}
function FCS_HMAC_decrypt(var cx: TFCS_HMAC_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}
begin
  hmac_update(cx.hmac_ctx, @data, dLen);
  FCS_HMAC_decrypt := SP_CTR_Encrypt(@data, @data, dLen, cx.SP_ctx);
end;


{---------------------------------------------------------------------------}
procedure FCS_HMAC_final(var cx: TFCS_HMAC_Context; var auth: TFCS_AuthBlock);
  {-return final HMAC-SHA1-128 digest}
var
  mac: THashDigest;
begin
  hmac_final(cx.hmac_ctx,mac);
  move(mac, auth, sizeof(auth));
end;


{---------------------------------------------------------------------------}
function FCS_EAX_init(var cx: TSP_eaxContext; pPW: pointer; pLen: word; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TXKey;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_SHA1), pPW, pLen, @hdr.salt, sizeof(TFCSSalt), KeyIterations, XKey, sizeof(XKey));

  {init SP EAX mode with ak/hk}
  if Err=0 then Err := SP_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCS_EAX_init := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCSSig := C_FCS_Sig;
  hdr.Flags  := $A1;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCS_EAX_initS(var cx: TSP_EAXContext; sPW: Str255; var hdr: TFCSHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCS_EAX_initS := FCS_EAX_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCS_EAX_encrypt(var cx: TSP_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}
begin
  FCS_EAX_encrypt := SP_EAX_Encrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
function FCS_EAX_decrypt(var cx: TSP_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}
begin
  FCS_EAX_decrypt := SP_EAX_decrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
procedure FCS_EAX_final(var cx: TSP_EAXContext; var auth: TFCS_AuthBlock);
  {-return final EAX tag}
begin
  SP_EAX_Final(TSPBlock(auth), cx);
end;


end.
