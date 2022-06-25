unit kdf;

{(Password Based) Key Derivation Functions}


interface

(*************************************************************************

 DESCRIPTION     :  (Password Based) Key Derivation Functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://tools.ietf.org/html/rfc2898
                    http://tools.ietf.org/html/rfc3211       [includes test vectors]
                    http://tools.ietf.org/html/rfc5869       [includes test vectors]
                    http://www.di-mgt.com.au/cryptoKDFs.html [includes test vectors]

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     17.01.06  W.Ehrhardt  Initial version based on keyderiv 1.29
 0.11     17.01.06  we          error codes
 0.12     23.01.06  we          new names: unit pb_kdf, functions kdf2/s
 0.13     22.06.08  we          Make IncMSB work with FPC -dDebug
 0.14     12.07.08  we          Rename old kdf2 to pbkdf2, unit kdf
 0.15     12.07.08  we          pbkdf1
 0.16     12.07.08  we          kdf1, kdf2, kdf3, mgf1
 0.17     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 0.18     25.04.09  we          updated RFC URL(s)
 0.19     19.08.10  we          kdf_err_nil_input
 0.20     20.08.10  we          hkdf/hkdfs
 0.21     15.08.14  we          pbkdf2 functions with longint sLen, dkLen
 0.22     16.08.15  we          Removed $ifdef DLL / stdcall
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006-2015 Wolfgang Ehrhardt

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
  BTypes,Hash,HMAC;

const
  kdf_err_nil_pointer   = $0001;  {phash descriptor is nil}
  kdf_err_digestlen     = $0002;  {digest length from descriptor is zero}
  kdf_err_invalid_dKLen = $0003;  {dKLen greater than hash digest length}
  kdf_err_nil_input     = $0004;  {input nil pointer and non-zero length}


function kdf1(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf2(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf3(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}


function mgf1(phash: PHashDesc; pSeed: pointer; sLen: word; var Mask; mLen: word): integer;
  {-Derive Mask from seed, hash function from phash, Mask Generation Function 1 for PKCS #1}


function pbkdf1(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password pPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf1s(phash: PHashDesc; sPW: Str255; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password string sPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}

function pbkdf2s(phash: PHashDesc; sPW: Str255; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}


function hkdf(phash: PHashDesc;              {Descriptor of the Hash to use}
              pIKM: pointer; L_IKM: word;    {input key material: addr/length}
              salt: pointer; L_salt: word;   {optional salt; can be nil: see below }
              info: pointer; L_info: word;   {optional context/application specific information}
              var DK; dkLen: word): integer; {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}

function hkdfs(phash: PHashDesc; sIKM: Str255; {Hash; input key material as string}
               salt: pointer; L_salt: word;    {optional salt; can be nil: see below }
               info: pointer; L_info: word;    {optional context/application specific information}
               var DK; dkLen: word): integer;  {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}


implementation


{helper type}
type
  TMSBA = array[0..3] of byte;


{---------------------------------------------------------------------------}
procedure IncMSB(var CTR: TMSBA);
  {-Increment CTR[3]..CTR[0], i.e. 32 Bit MSB counter}
var
  j: integer;
begin
  for j:=3 downto 0 do begin
    if CTR[j]=$FF then CTR[j] := 0
    else begin
      inc(CTR[j]);
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function pbkdf1(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password pPW using 8 byte salt and iteration count C, uses hash function from phash}
var
  ctx: THashContext;
  Digest: THashDigest;
  hlen: word;
begin
  if (phash=nil) or (phash^.HSig<>C_HashSig) then begin
    pbkdf1 := kdf_err_nil_pointer;
    exit;
  end;
  hLen := phash^.HDigestLen;
  if hLen=0 then begin
    pbkdf1 := kdf_err_digestlen;
    exit;
  end;
  if hLen<dKLen then begin
    pbkdf1 := kdf_err_invalid_dKLen;
    exit;
  end;
  if ((pPW=nil) and (pLen>0)) or (salt=nil) then begin
    pbkdf1 := kdf_err_nil_input;
    exit;
  end;

  pbkdf1 := 0;
  {calculate T_1 = hash(PW || salt)}
  with phash^ do begin
    HInit(ctx);
    HUpdateXL(ctx, pPW, pLen);
    HUpdateXL(ctx, salt, 8);
    HFinal(ctx, Digest);
  end;
  while C>1 do begin
    HashFullXL(phash, Digest, @Digest, hlen);
    dec(C);
  end;
  move(Digest, DK, dKLen);
end;


{---------------------------------------------------------------------------}
function pbkdf1s(phash: PHashDesc; sPW: Str255; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password string sPW using 8 byte salt and iteration count C, uses hash function from phash}
begin
  pbkdf1s := pbkdf1(phash, @sPW[1], length(sPW), salt, C, DK, dkLen);
end;


{---------------------------------------------------------------------------}
function pbkdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}
var
  k, hLen: word;
  i, j, lk: longint;
  pDK: pByte;   {pointer to DK  }
  ii: TMSBA;    {i in big endian}
  u, ucum: THashDigest;
  ctx: THMAC_Context;
begin
  if phash=nil then begin
    pbkdf2 := kdf_err_nil_pointer;
    exit;
  end;
  if phash^.HDigestLen=0 then begin
    pbkdf2 := kdf_err_digestlen;
    exit;
  end;
  if ((pPW=nil) and (pLen>0)) or ((salt=nil) and (sLen>0)) then begin
    pbkdf2 := kdf_err_nil_input;
    exit;
  end;

  pbkdf2 := 0;
  hLen := phash^.HDigestLen;
  lk := 0;
  pDK := pByte(@DK);
  fillchar(ii, sizeof(ii), 0);
  for i:=1 to 1 + pred(dkLen) div hLen do begin
    IncMSB(ii);
    fillchar(ucum, sizeof(ucum),0);
    for j:=1 to C do begin
      hmac_init(ctx, phash, pPW, pLen);
      if j=1 then begin
        {U_1 = PRF(pPW, salt || ii)}
        hmac_updateXL(ctx, salt, sLen);
        hmac_updateXL(ctx, @ii, 4);
      end
      else begin
        {U_i = PRF(pPW, U_(i-1)}
        hmac_updateXL(ctx, @u, hLen);
      end;
      hmac_final(ctx, u);
      {update cumulative XOR U_i}
      for k:=0 to hLen-1 do ucum[k] := ucum[k] xor u[k];
    end;
    {T_i = F(P,S,C,l) = ucum}
    for k:=0 to hLen-1 do begin
      {concat T_i}
      if lk<dkLen then begin
        pDK^ := ucum[k];
        inc(lk);
        inc(Ptr2Inc(pDK));
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function pbkdf2s(phash: PHashDesc; sPW: Str255; salt: pointer; sLen, C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}
begin
  pbkdf2s := pbkdf2(phash, @sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;


{---------------------------------------------------------------------------}
function kdfx(phash: PHashDesc; x: byte; Z, pOtherInfo: pointer; zLen, oLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}
  { Internal function for all kdf1, kdf2, kdf3, mgf1}
var
  ctr: TMSBA;    {i in big endian}
  ctx: THashContext;
  pDK: pByte;   {pointer to DK  }
  Digest: THashDigest;
  i, k, lk, hLen: word;
begin

  if (phash=nil) or (phash^.HSig<>C_HashSig) then begin
    kdfx := kdf_err_nil_pointer;
    exit;
  end;
  hLen := phash^.HDigestLen;
  if hLen=0 then begin
    kdfx := kdf_err_digestlen;
    exit;
  end;
  if ((Z=nil) and (zLen>0)) or ((pOtherInfo=nil) and (oLen>0)) then begin
    kdfx := kdf_err_nil_input;
    exit;
  end;

  kdfx := 0;
  fillchar(ctr, sizeof(ctr), 0);
  if x=2 then IncMSB(ctr);
  lk := 0;
  pDK := pByte(@DK);
  for i:=1 to 1 + pred(dkLen) div hLen do begin
    if x=3 then begin
      {Hash(ctr || Z || [OtherInfo])} {x=3}
      phash^.HInit(ctx);
      phash^.HUpdateXL(ctx, @ctr, sizeof(ctr));
      phash^.HUpdateXL(ctx, Z, zLen);
    end
    else begin
      {Hash(Z || ctr || [OtherInfo])} {x=1,2}
      phash^.HInit(ctx);
      phash^.HUpdateXL(ctx, Z, zLen);
      phash^.HUpdateXL(ctx, @ctr, sizeof(ctr));
    end;
    if oLen<>0 then phash^.HUpdateXL(ctx, pOtherInfo, oLen);
    phash^.HFinal(ctx, Digest);
    for k:=0 to hLen-1 do begin
      {store T_i}
      if lk<dkLen then begin
        pDK^ := Digest[k];
        inc(lk);
        inc(Ptr2Inc(pDK));
      end
      else exit;
    end;
    IncMSB(ctr);
  end;
end;


{---------------------------------------------------------------------------}
function kdf1(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}
begin
  kdf1 := kdfx(phash, 1, Z, pOtherInfo, zLen, oiLen, DK, dkLen);
end;


{---------------------------------------------------------------------------}
function kdf2(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}
begin
  kdf2 := kdfx(phash, 2, Z, pOtherInfo, zLen, oiLen, DK, dkLen);
end;


{---------------------------------------------------------------------------}
function kdf3(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}
begin
  kdf3 := kdfx(phash, 3, Z, pOtherInfo, zLen, oiLen, DK, dkLen);
end;


{---------------------------------------------------------------------------}
function mgf1(phash: PHashDesc; pSeed: pointer; sLen: word; var Mask; mLen: word): integer;
  {-Derive Mask from seed, hash function from phash, Mask Generation Function 1 for PKCS #1}
begin
  mgf1 := kdfx(phash, 1, pSeed, nil, sLen, 0, Mask, mLen);
end;


{---------------------------------------------------------------------------}
function hkdf(phash: PHashDesc;              {Descriptor of the Hash to use}
              pIKM: pointer; L_IKM: word;    {input key material: addr/length}
              salt: pointer; L_salt: word;   {optional salt; can be nil: see below }
              info: pointer; L_info: word;   {optional context/application specific information}
              var DK; dkLen: word): integer; {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}
var
  PRK,TI: THashDigest;
  ctx: THMAC_Context;
  i,k,hLen,lt,lk: word;
  ctr: byte;
  pDK: pByte;
begin

  {Ref: (IETF) rfc5869 - H. Krawczyk, P. Eronen, May 2010}
  {HMAC-based Extract-and-Expand Key Derivation Function (HKDF)}

  {Check input parameters}
  if (phash=nil) or (phash^.HSig<>C_HashSig) then begin
    hkdf := kdf_err_nil_pointer;
    exit;
  end;
  if ((pIKM=nil) and (L_IKM>0)) or ((info=nil) and (L_info>0)) then begin
    hkdf := kdf_err_nil_input;
    exit;
  end;

  hLen := phash^.HDigestLen;
  if hLen=0 then begin
    hkdf := kdf_err_digestlen;
    exit;
  end;

  {Stage 1: Extract}
  hkdf := 0;
  {if salt=nil then hLen binary zeros are used}
  if salt=nil then begin
    fillchar(TI, sizeof(TI), 0);
    hmac_init(ctx, phash, @TI, hLen);
  end
  else hmac_init(ctx, phash, salt, L_salt);
  hmac_update(ctx, pIKM, L_IKM);
  hmac_final(ctx, PRK);

  {Stage 2: Expand}
  lt  := 0;
  lk  := 0;
  ctr := 1;
  pDK := pByte(@DK);
  for i:=1 to 1 + pred(dkLen) div hLen do begin
    {calculate T_i from T_(i-1), info, and ctr}
    hmac_init(ctx, phash, @PRK, hLen);
    hmac_update(ctx, @TI, lt);
    hmac_update(ctx, info, L_info);
    hmac_update(ctx, @ctr, 1);
    hmac_final(ctx, TI);
    for k:=0 to hLen-1 do begin
      {store T_i}
      if lk<dkLen then begin
        pDK^ := TI[k];
        inc(lk);
        inc(Ptr2Inc(pDK));
      end
      else exit;
    end;
    lt := hLen;
    inc(ctr);
  end;
end;


{---------------------------------------------------------------------------}
function hkdfs(phash: PHashDesc; sIKM: Str255; {Hash; input key material as string}
               salt: pointer; L_salt: word;    {optional salt; can be nil: see below }
               info: pointer; L_info: word;    {optional context/application specific information}
               var DK; dkLen: word): integer;  {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}
begin
  hkdfs := hkdf(phash,@sIKM[1],length(sIKM),salt,L_salt,info,L_info,DK,dkLen);
end;


end.
