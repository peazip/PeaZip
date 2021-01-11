unit fcaes256;


(*************************************************************************

 DESCRIPTION     :  File crypt/authenticate unit using AES256

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  (http://fp.gladman.plus.com/cryptography_technology/fileencrypt/fileencrypt.htm)

 REMARK          :

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     04.08.06  G.Tani      Initial version based on Wolfgang Ehrhardt's Fcrypta 0.41:
                                HMAC mode removed (the unit is focused on EAX mode);
                                KDF2 now uses Whirlpool instead of SHA1 as primitive;
                                functions changed to use 256 bit key;
                                the header format is the same of fcrypta (plus bit2 in Flags for key size)
                                and the digest is still a single AES block (128 bit of size).
 0.11     04.08.06  W.Ehrhardt  Portable code (line length < 128, unit name)
 0.12     05.08.06  we          Reintroduced HMAC (Whirlpool code)
 0.13     13.08.06  we          Changed unit name and description
 0.14     14.06.07  we          Type TAES_EAXContext
 0.15     15.07.08  we          Unit KDF/pbkdf2
 0.16     22.11.08  we          TFCA256_string replaced by Str255 from BTypes
 0.17     31.08.20  G.Tani      FCA_EAX256_initP for slower pbkdf2 key schedule with more iterations (algorithm and number of iterations different from other *P functions), and off-line password tag verification
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
       CH_INTV, AES_INTV;
     {$else}
       CH_INTF, AES_INTF;
     {$endif}
  {$else}
    Hash, HMAC, Whirl512, KDF, AES_Type, AES_CTR, AES_EAX;
  {$endif}



const
  C_FCA_Sig = $FC;
  KeyIterations : word = 1000;                {Iterations in KeyDeriv}

type
  TFCA256Salt  = array[0..2] of longint;      {96 Bit salt}
  TFCA256Hdr   = packed record                {same header format used in Fcrypt}
                                              {a plus key size bit in Flag for 256 bit keys (bit 2)}
                   FCAsig: byte;              {Sig  $FC}
                   Flags : byte;              {High $A0; Bit2: 0=128bit (as in Fcrypta)}
                                              {1=256bit; Bit1: compression; Bit0: 1=EAX, 0=HMAC-CTR}
                   Salt  : TFCA256Salt;
                   PW_Ver: word;
                 end;

  TFCA256_AuthBlock = array[0..15] of byte;


type
  TFCA_HMAC256_Context  = record
                            aes_ctx  : TAESContext;    {crypt context}
                            hmac_ctx : THMAC_Context;  {auth  context}
                          end;


function FCA_EAX256_init(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;
function FCA_EAX256_initP(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;//50x iterations
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCA_EAX256_initS(var cx: TAES_EAXContext; sPW: Str255; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCA_EAX256_encrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}

function FCA_EAX256_decrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}

procedure FCA_EAX256_final(var cx: TAES_EAXContext; var auth: TFCA256_AuthBlock);
  {-return final EAX tag}


function FCA_HMAC256_init(var cx: TFCA_HMAC256_Context; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}

function FCA_HMAC256_initS(var cx: TFCA_HMAC256_Context; sPW: Str255; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}

function FCA_HMAC256_encrypt(var cx: TFCA_HMAC256_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}

function FCA_HMAC256_decrypt(var cx: TFCA_HMAC256_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}

procedure FCA_HMAC256_final(var cx: TFCA_HMAC256_Context; var auth: TFCA256_AuthBlock);
  {-return final HMAC-Whirlpool-128 digest}


implementation



type
  TX256Key = packed record                       {eXtended key for PBKDF}
               ak: packed array[0..31] of byte;  {AES  256 bit key      }
               hk: packed array[0..31] of byte;  {HMAC key / EAX nonce  }
               pv: word;                         {password verifier     }
             end;


{---------------------------------------------------------------------------}
function FCA_HMAC256_init(var cx: TFCA_HMAC256_Context; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  CTR : TAESBlock;
  pwph: PHashDesc;
  Err : integer;
begin
  {CTR=0, random/uniqness from hdr.salt}
  fillchar(CTR, sizeof(CTR), 0);

  {derive the AES, HMAC keys and pw verifier}
  pwph := FindHash_by_ID(_Whirlpool);
  Err  := pbkdf2(pwph, pPW, pLen, @hdr.salt, sizeof(TFCA256Salt), KeyIterations, XKey, sizeof(XKey));

  {init AES CTR mode with ak}
  if Err=0 then Err := AES_CTR_Init(XKey.ak, 8*sizeof(XKey.ak), CTR, cx.aes_ctx);

  {exit if any error}
  FCA_HMAC256_init := Err;
  if Err<>0 then exit;

  {initialise HMAC with hk, here pwph is valid}
  hmac_init(cx.hmac_ctx, pwph, @XKey.hk, sizeof(XKey.hk));

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCASig := C_FCA_Sig;
  hdr.Flags  := $A4;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC256_initS(var cx: TFCA_HMAC256_Context; sPW: Str255; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCA_HMAC256_initS := FCA_HMAC256_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC256_encrypt(var cx: TFCA_HMAC256_Context; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update HMAC}
begin
  FCA_HMAC256_encrypt := AES_CTR_Encrypt(@data, @data, dLen, cx.aes_ctx);
  hmac_update(cx.hmac_ctx, @data, dLen);
end;


{---------------------------------------------------------------------------}
function FCA_HMAC256_decrypt(var cx: TFCA_HMAC256_Context; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update HMAC}
begin
  hmac_update(cx.hmac_ctx, @data, dLen);
  FCA_HMAC256_decrypt := AES_CTR_Encrypt(@data, @data, dLen, cx.aes_ctx);
end;


{---------------------------------------------------------------------------}
procedure FCA_HMAC256_final(var cx: TFCA_HMAC256_Context; var auth: TFCA256_AuthBlock);
  {-return final HMAC-Whirlpool-128 digest}
var
  mac: THashDigest;
begin
  hmac_final(cx.hmac_ctx,mac);
  move(mac, auth, sizeof(auth));
end;


{---------------------------------------------------------------------------}
function FCA_EAX256_init(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_Whirlpool), pPW, pLen, @hdr.salt, sizeof(TFCA256Salt), KeyIterations, XKey, sizeof(XKey));

  {init AES EAX mode with ak/hk}
  if Err=0 then Err := AES_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCA_EAX256_init := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCASig := C_FCA_Sig;
  hdr.Flags  := $A5;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;

function FCA_EAX256_initP(var cx: TAES_EAXContext; pPW: pointer; pLen: word; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password pointer pPW and hdr.salt}
var
  XKey: TX256Key;
  Err : integer;
begin

  {derive the EAX key / nonce and pw verifier}
  Err := pbkdf2(FindHash_by_ID(_Whirlpool), pPW, pLen, @hdr.salt, sizeof(TFCA256Salt), 25000, XKey, sizeof(XKey));

  {init AES EAX mode with ak/hk}
  if Err=0 then Err := AES_EAX_Init(XKey.ak, 8*sizeof(XKey.ak), xkey.hk, sizeof(XKey.hk), cx);;

  {exit if any error}
  FCA_EAX256_initP := Err;
  if Err<>0 then exit;

  {return pw verifier}
  hdr.PW_Ver := XKey.pv;
  hdr.FCASig := C_FCA_Sig;
  hdr.Flags  := $A5;

  {burn XKey}
  fillchar(XKey, sizeof(XKey),0);
end;

{---------------------------------------------------------------------------}
function FCA_EAX256_initS(var cx: TAES_EAXContext; sPW: Str255; var hdr: TFCA256Hdr): integer;
  {-Initialize crypt context using password string sPW and hdr.salt}
begin
  FCA_EAX256_initS := FCA_EAX256_init(cx, @sPW[1], length(sPW), hdr);
end;


{---------------------------------------------------------------------------}
function FCA_EAX256_encrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-encyrypt a block of data in place and update EAX}
begin
  FCA_EAX256_encrypt := AES_EAX_Encrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
function FCA_EAX256_decrypt(var cx: TAES_EAXContext; var data; dLen: word): integer;
  {-decyrypt a block of data in place and update EAX}
begin
  FCA_EAX256_decrypt := AES_EAX_decrypt(@data, @data, dLen, cx);
end;


{---------------------------------------------------------------------------}
procedure FCA_EAX256_final(var cx: TAES_EAXContext; var auth: TFCA256_AuthBlock);
  {-return final EAX tag}
begin
  AES_EAX_Final(TAESBlock(auth), cx);
end;


end.
