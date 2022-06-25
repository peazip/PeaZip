unit fcrypta;


(*************************************************************************

 DESCRIPTION     :  File crypt/authenticate unit

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  (http://fp.gladman.plus.com/cryptography_technology/fileencrypt/fileencrypt.htm)

 REMARK          :  Make sure to change hdr.salt for subsequent calls to FCA_XXX_init

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     06.12.03  W.Ehrhardt  Initial vers. inspired from Gladman's fileenc
 0.20     06.12.03  we          TXKey type
 0.30     07.12.02  we          TFCSig, changed other length, 32 byte overhead
 0.31     07.12.02  we          comments, burn XKey
 0.32     07.12.02  we          use functions and return error codes
 0.33     07.12.02  we          fcrypt_init and fcrypt_initS
 0.34     08.12.02  we          removed TFCSig, changed TSalt type
 0.35     21.12.02  we          typos corrected
 0.36     12.04.04  we          Delphi 7
 0.37     07.07.04  we          DLL support
 0.38     08.07.04  we          EAX support, new name, header record
 0.39     10.07.04  we          TFCA_string
 0.40     22.01.06  we          uses new Hash, HMAC units
 0.41     23.01.06  we          uses PB_KDF
 0.42     14.06.07  we          Type TAES_EAXContext
 0.43     15.07.08  we          Unit KDF/pbkdf2
 0.44     22.11.08  we          TFCA_string replaced by Str255 from BTypes
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
       CH_INTV, AES_INTV;
     {$else}
       CH_INTF, AES_INTF;
     {$endif}
  {$else}
    Hash, HMAC, SHA1, KDF, AES_Type, AES_CTR, AES_EAX;
  {$endif}


const
  C_FCA_Sig = $FC;

type
  TFCASalt  = array[0..2] of longint;         {96 Bit salt}
  TFCAHdr   = packed record
                FCAsig: byte;                 {Sig  $FC}
                Flags : byte;                 {High $A0, Bit0: 1=EAX, 0:HMAC-CTR}
                Salt  : TFCASalt;
                PW_Ver: word;
              end;


type
  TFCA_HMAC_Context  = record
                         aes_ctx  : TAESContext;   {crypt context}
                         hmac_ctx : THMAC_Context;  {auth  context}
                       end;

type
  TFCA_AuthBlock = array[0..15] of byte;

const
  KeyIterations  : word = 1000;          {Iterations in KeyDeriv}


function FCA_HMAC_init(var cx: TFCA_HMAC_Context; pPW: pointer; pLen: word; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCA_HMAC_initS(var cx: TFCA_HMAC_Context; sPW: Str255; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCA_HMAC_encrypt(var cx: TFCA_HMAC_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}

function FCA_HMAC_decrypt(var cx: TFCA_HMAC_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}

procedure FCA_HMAC_final(var cx: TFCA_HMAC_Context; var auth: TFCA_AuthBlock);
  {-return final HMAC-SHA1-128 digest}


function FCA_EAX_init(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCA_EAX_initS(var cx: TAES_EAXContext; sPW: Str255; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCA_EAX_encrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}

function FCA_EAX_decrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}

procedure FCA_EAX_final(var cx: TAES_EAXContext; var auth: TFCA_AuthBlock);
  {-return final EAX tag}


implementation


type
  TXKey = packed record                       {eXtended key for PBKDF}
            ak: packed array[0..15] of byte;  {AES  128 bit key      }
            hk: packed array[0..15] of byte;  {HMAC key / EAX nonce  }
            pv: word;                         {password verifier     }
          end;


{---------------------------------------------------------------------------}
function FCA_HMAC_init(var cx: TFCA_HMAC_Context; pPW: pointer; pLen: word; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TXKey;
  CTR : TAESBlock;
  psha: PHashDesc;
  Err : integer;
begin
  {CTR=0, random/uniqness from hdr.salt}
  fillchar(CTR, sizeof(CTR), 0);

  {derive the AES, HMAC keys and pw verifier}
  psha := FindHash_by_ID(_SHA1);
  Err  := pbkdf2(psha, pPW, pLen, @hdr.salt, sizeof(TFCASalt), KeyIterations, XKey, sizeof(XKey));

  {init AES CTR mode with ak}
  if Err=0 then Err := AES_CTR_Init(XKey.ak, 8*sizeof(XKey.ak), CTR, cx.aes_ctx);

  {exit if any error}
  FCA_HMAC_init := Err;
  if Err<>0 then exit;

  {initialise HMAC with hk, here psha is valid}
  hmac_init(cx.hmac_ctx, psha, @XKey.hk, sizeof(XKey.hk));

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCASig := C_FCA_Sig;
  hdr.Flags  := $A0;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC_initS(var cx: TFCA_HMAC_Context; sPW: Str255; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCA_HMAC_initS := FCA_HMAC_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC_encrypt(var cx: TFCA_HMAC_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}
begin
  FCA_HMAC_encrypt := AES_CTR_Encrypt(@data, @data, dLen, cx.aes_ctx);
  hmac_update(cx.hmac_ctx, @data, dLen);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC_decrypt(var cx: TFCA_HMAC_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}
begin
  hmac_update(cx.hmac_ctx, @data, dLen);
  FCA_HMAC_decrypt := AES_CTR_Encrypt(@data, @data, dLen, cx.aes_ctx);
end;


{---------------------------------------------------------------------------}
procedure FCA_HMAC_final(var cx: TFCA_HMAC_Context; var auth: TFCA_AuthBlock);
  {-return final HMAC-SHA1-128 digest}
var
  mac: THashDigest;
begin
  hmac_final(cx.hmac_ctx,mac);
  move(mac, auth, sizeof(auth));
end;


{---------------------------------------------------------------------------}
function FCA_EAX_init(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TXKey;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_SHA1), pPW, pLen, @hdr.salt, sizeof(TFCASalt), KeyIterations, XKey, sizeof(XKey));

  {init AES EAX mode with ak/hk}
  if Err=0 then Err := AES_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCA_EAX_init := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCASig := C_FCA_Sig;
  hdr.Flags  := $A1;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCA_EAX_initS(var cx: TAES_EAXContext; sPW: Str255; var hdr: TFCAHdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCA_EAX_initS := FCA_EAX_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCA_EAX_encrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}
begin
  FCA_EAX_encrypt := AES_EAX_Encrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
function FCA_EAX_decrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}
begin
  FCA_EAX_decrypt := AES_EAX_decrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
procedure FCA_EAX_final(var cx: TAES_EAXContext; var auth: TFCA_AuthBlock);
  {-return final EAX tag}
begin
  AES_EAX_Final(TAESBlock(auth), cx);
end;


end.
