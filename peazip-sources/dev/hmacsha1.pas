unit HMACSHA1;

{HMAC SHA1 - message authentication with SHA1, obsolete: use HMAC unit!}


interface

(*************************************************************************

 DESCRIPTION     :  HMAC SHA1 - message authentication with SHA1 hash

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 2202 (http://tools.ietf.org/html/rfc2202)
                 :  http://csrc.nist.gov/publications/fips/fips198/fips-198a.pdf

 REMARKS         :  "Truncate" from RFC not implemented, mac: full digest

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     03.08.03  W.Ehrhardt  Initial version
 1.01     03.08.03  we          With SHA1Full
 2.10     29.08.03  we          common version, XL versions for Win32
 2.20     05.10.03  we          STD.INC
 2.40     10.10.03  we          common version, english comments
 3.00     01.12.03  we          Common version 3.0
 3.01     07.07.04  we          THMACSHA1_string, stdcall for DLL
 3.02     17.01.06  we          Obsolete/legacy, shell for HMAC unit
 3.03     12.11.08  we          uses BTypes, Str255
 3.04     25.04.09  we          updated RFC URL(s)
 3.05     16.08.15  we          Removed $ifdef DLL / stdcall
************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2015 Wolfgang Ehrhardt

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
  BTypes,Hash,HMAC,SHA1;

procedure hmac_sha1_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_sha1_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_sha1_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_sha1_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_sha1_final(var ctx: THMAC_Context; var mac: TSHA1Digest);
  {-end data input, calculate HMAC digest}

implementation


{---------------------------------------------------------------------------}
procedure hmac_sha1_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}
var
  phash: PHashDesc;
begin
  phash := FindHash_by_ID(_SHA1);
  hmac_init(ctx, phash, key, klen);
end;


{---------------------------------------------------------------------------}
procedure hmac_sha1_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}
begin
  hmac_sha1_init(ctx, @skey[1], length(skey));
end;


{---------------------------------------------------------------------------}
procedure hmac_sha1_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_sha1_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_sha1_final(var ctx: THMAC_Context; var mac: TSHA1Digest);
  {-end data input, calculate HMAC digest}
var
  d: THashDigest;
begin
  hmac_final(ctx, d);
  move(d, mac, sizeof(mac));
end;

end.

