unit fctf256;


(*************************************************************************

 DESCRIPTION     :  File crypt/authenticate unit using Twofish 256

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     10.03.16  G.Tani      Based on W.Ehrhardt fcaes256 0.16 modified to use Twofish 256
 0.11     31.08.20  G.Tani      FCF_EAX256_initP for slower pbkdf2 key schedule with more iterations (algorithm and number of iterations different from other *P functions), and off-line password tag verification
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2003-2008 Wolfgang Ehrhardt, Giorgio Tani

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
       CH_INTV, TF_INTV;
     {$else}
       CH_INTF, TF_INTF;
     {$endif}
  {$else}
    Hash, HMAC, Whirl512, KDF, TF_base, TF_CTR, TF_EAX;
  {$endif}



const
  C_FCF_Sig = $FC;
  KeyIterations : word = 1000;                {Iterations in KeyDeriv}

type
  TFCF256Salt  = array[0..2] of longint;      {96 Bit salt}
  TFCF256Hdr   = packed record                {same header format used in Fcrypt}
                                              {a plus key size bit in Flag for 256 bit keys (bit 2)}
                   FCFsig: byte;              {Sig  $FC}
                   Flags : byte;              {High $A0; Bit2: 0=128bit (as in Fcrypta)}
                                              {1=256bit; Bit1: compression; Bit0: 1=EAX, 0=HMAC-CTR}
                   Salt  : TFCF256Salt;
                   PW_Ver: word;
                 end;

  TFCF256_AuthBlock = array[0..15] of byte;


type
  TFCF_HMAC256_Context  = record
                            TF_ctx  : TTFContext;    {crypt context}
                            hmac_ctx : THMAC_Context;  {auth  context}
                          end;


function FCF_EAX256_init(var cx: TTF_EAXContext; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
function FCF_EAX256_initP(var cx: TTF_EAXContext; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCF_EAX256_initS(var cx: TTF_EAXContext; sPW: Str255; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCF_EAX256_encrypt(var cx: TTF_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}

function FCF_EAX256_decrypt(var cx: TTF_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}

procedure FCF_EAX256_final(var cx: TTF_EAXContext; var auth: TFCF256_AuthBlock);
  {-return final EAX tag}


function FCF_HMAC256_init(var cx: TFCF_HMAC256_Context; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCF_HMAC256_initS(var cx: TFCF_HMAC256_Context; sPW: Str255; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCF_HMAC256_encrypt(var cx: TFCF_HMAC256_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}

function FCF_HMAC256_decrypt(var cx: TFCF_HMAC256_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}

procedure FCF_HMAC256_final(var cx: TFCF_HMAC256_Context; var auth: TFCF256_AuthBlock);
  {-return final HMAC-Whirlpool-128 digest}


implementation



type
  TX256Key = packed record                       {eXtended key for PBKDF}
               ak: packed array[0..31] of byte;  {TF  256 bit key      }
               hk: packed array[0..31] of byte;  {HMAC key / EAX nonce  }
               pv: word;                         {password verifier     }
             end;


{---------------------------------------------------------------------------}
function FCF_HMAC256_init(var cx: TFCF_HMAC256_Context; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  CTR : TTFBlock;
  pwph: PHashDesc;
  Err : integer;
begin
  {CTR=0, random/uniqness from hdr.salt}
  fillchar(CTR, sizeof(CTR), 0);

  {derive the TF, HMAC keys and pw verifier}
  pwph := FindHash_by_ID(_Whirlpool);
  Err  := pbkdf2(pwph, pPW, pLen, @hdr.salt, sizeof(TFCF256Salt), KeyIterations, XKey, sizeof(XKey));

  {init TF CTR mode with ak}
  if Err=0 then Err := TF_CTR_Init(XKey.ak, 8*sizeof(XKey.ak), CTR, cx.TF_ctx);

  {exit if any error}
  FCF_HMAC256_init := Err;
  if Err<>0 then exit;

  {initialise HMAC with hk, here pwph is valid}
  hmac_init(cx.hmac_ctx, pwph, @XKey.hk, sizeof(XKey.hk));

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCFSig := C_FCF_Sig;
  hdr.Flags  := $A4;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCF_HMAC256_initS(var cx: TFCF_HMAC256_Context; sPW: Str255; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCF_HMAC256_initS := FCF_HMAC256_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCF_HMAC256_encrypt(var cx: TFCF_HMAC256_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}
begin
  FCF_HMAC256_encrypt := TF_CTR_Encrypt(@data, @data, dLen, cx.TF_ctx);
  hmac_update(cx.hmac_ctx, @data, dLen);
end;


{---------------------------------------------------------------------------}
function FCF_HMAC256_decrypt(var cx: TFCF_HMAC256_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}
begin
  hmac_update(cx.hmac_ctx, @data, dLen);
  FCF_HMAC256_decrypt := TF_CTR_Encrypt(@data, @data, dLen, cx.TF_ctx);
end;


{---------------------------------------------------------------------------}
procedure FCF_HMAC256_final(var cx: TFCF_HMAC256_Context; var auth: TFCF256_AuthBlock);
  {-return final HMAC-Whirlpool-128 digest}
var
  mac: THashDigest;
begin
  hmac_final(cx.hmac_ctx,mac);
  move(mac, auth, sizeof(auth));
end;


{---------------------------------------------------------------------------}
function FCF_EAX256_init(var cx: TTF_EAXContext; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_Whirlpool), pPW, pLen, @hdr.salt, sizeof(TFCF256Salt), KeyIterations, XKey, sizeof(XKey));

  {init TF EAX mode with ak/hk}
  if Err=0 then Err := TF_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCF_EAX256_init := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCFSig := C_FCF_Sig;
  hdr.Flags  := $A5;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;

function FCF_EAX256_initP(var cx: TTF_EAXContext; pPW: pointer; pLen: word; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_SHA512), pPW, pLen, @hdr.salt, sizeof(TFCF256Salt), 50000, XKey, sizeof(XKey));

  {init TF EAX mode with ak/hk}
  if Err=0 then Err := TF_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCF_EAX256_initP := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCFSig := C_FCF_Sig;
  hdr.Flags  := $A5;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;

{---------------------------------------------------------------------------}
function FCF_EAX256_initS(var cx: TTF_EAXContext; sPW: Str255; var hdr: TFCF256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCF_EAX256_initS := FCF_EAX256_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCF_EAX256_encrypt(var cx: TTF_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}
begin
  FCF_EAX256_encrypt := TF_EAX_Encrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
function FCF_EAX256_decrypt(var cx: TTF_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}
begin
  FCF_EAX256_decrypt := TF_EAX_decrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
procedure FCF_EAX256_final(var cx: TTF_EAXContext; var auth: TFCF256_AuthBlock);
  {-return final EAX tag}
begin
  TF_EAX_Final(TTFBlock(auth), cx);
end;


end.
