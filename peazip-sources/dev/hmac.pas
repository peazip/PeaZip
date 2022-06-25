unit HMAC;

{General HMAC unit}


interface

(*************************************************************************

 DESCRIPTION     :  General HMAC (hash message authentication) unit

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - HMAC: Keyed-Hashing for Message Authentication
                      (http://tools.ietf.org/html/rfc2104)
                    - The Keyed-Hash Message Authentication Code (HMAC)
                      http://csrc.nist.gov/publications/fips/fips198/fips-198a.pdf
                    - US Secure Hash Algorithms (SHA and HMAC-SHA)
                      (http://tools.ietf.org/html/rfc4634)

 REMARKS         :  Trailing bits in SHA3-LSB format must be converted to MSB

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.01.06  W.Ehrhardt  Initial version based on HMACWHIR
 0.11     07.05.08  we          hmac_final_bits
 0.12     12.11.08  we          Uses BTypes, THMAC_string replaced by Str255
 0.13     25.04.09  we          updated RFC URL(s)

 0.14     08.08.15  we          type of hmacbuf changed to THMacBuffer
 0.15     16.08.15  we          Removed $ifdef DLL / stdcall

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

uses
  BTypes,hash;

type
  THMAC_Context = record
                    hashctx: THashContext;
                    hmacbuf: THMacBuffer;
                    phashd : PHashDesc;
                  end;

procedure hmac_init(var ctx: THMAC_Context; phash: PHashDesc; key: pointer; klen: word);
  {-initialize HMAC context with hash descr phash^ and key}

procedure hmac_inits(var ctx: THMAC_Context; phash: PHashDesc; skey: Str255);
  {-initialize HMAC context with hash descr phash^ and skey}

procedure hmac_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_final(var ctx: THMAC_Context; var mac: THashDigest);
  {-end data input, calculate HMAC digest}

procedure hmac_final_bits(var ctx: THMAC_Context; var mac: THashDigest; BData: byte; bitlen: integer);
  {-end data input with bitlen bits (MSB format) from BData, calculate HMAC digest}


implementation


{---------------------------------------------------------------------------}
procedure hmac_init(var ctx: THMAC_Context; phash: PHashDesc; key: pointer; klen: word);
  {-initialize HMAC context with hash descr phash^ and key}
var
  i,lk,bl: word;
  kb: THashDigest;
begin
  fillchar(ctx, sizeof(ctx),0);
  if phash<>nil then with ctx do begin
    phashd := phash;
    lk := klen;
    bl := phash^.HBlockLen;
    if lk > bl then begin
      {Hash if key length > block length}
      HashFullXL(phash, kb, key, lk);
      lk := phash^.HDigestLen;
      move(kb, hmacbuf, lk);
    end
    else move(key^, hmacbuf, lk);
    {XOR with ipad}
    for i:=0 to bl-1 do hmacbuf[i] := hmacbuf[i] xor $36;
    {start inner hash}
    phash^.HInit(hashctx);
    phash^.HUpdateXL(hashctx, @hmacbuf, bl);
  end;
end;


{---------------------------------------------------------------------------}
procedure hmac_inits(var ctx: THMAC_Context; phash: PHashDesc; skey: Str255);
  {-initialize HMAC context with hash descr phash^ and skey}
begin
  if phash<>nil then hmac_init(ctx, phash, @skey[1], length(skey));
end;


{---------------------------------------------------------------------------}
procedure hmac_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
begin
  with ctx do begin
    if phashd<>nil then phashd^.HUpdateXL(hashctx, data, dlen);
  end;
end;


{---------------------------------------------------------------------------}
procedure hmac_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
begin
  with ctx do begin
    if phashd<>nil then phashd^.HUpdateXL(hashctx, data, dlen);
  end;
end;


{---------------------------------------------------------------------------}
procedure hmac_final(var ctx: THMAC_Context; var mac: THashDigest);
  {-end data input, calculate HMAC digest}
var
  i: integer;
  bl: word;
begin
  with ctx do if phashd<>nil then begin
    bl := phashd^.HBlockLen;
    {complete inner hash}
    phashd^.HFinal(hashctx, mac);
    {remove ipad from buf, XOR opad}
    for i:=0 to bl-1 do hmacbuf[i] := hmacbuf[i] xor ($36 xor $5c);
    {outer hash}
    phashd^.HInit(hashctx);
    phashd^.HUpdateXL(hashctx, @hmacbuf, bl);
    phashd^.HUpdateXL(hashctx, @mac, phashd^.HDigestLen);
    phashd^.HFinal(hashctx, mac);
  end;
end;


{---------------------------------------------------------------------------}
procedure hmac_final_bits(var ctx: THMAC_Context; var mac: THashDigest; BData: byte; bitlen: integer);
  {-end data input with bitlen bits (MSB format) from BData, calculate HMAC digest}
var
  i: integer;
  bl: word;
begin
  with ctx do if phashd<>nil then begin
    bl := phashd^.HBlockLen;
    {complete inner hash}
    phashd^.HFinalBit(hashctx, mac, BData, bitlen);
    {remove ipad from buf, XOR opad}
    for i:=0 to bl-1 do hmacbuf[i] := hmacbuf[i] xor ($36 xor $5c);
    {outer hash}
    phashd^.HInit(hashctx);
    phashd^.HUpdateXL(hashctx, @hmacbuf, bl);
    phashd^.HUpdateXL(hashctx, @mac, phashd^.HDigestLen);
    phashd^.HFinal(hashctx, mac);
  end;
end;

end.
