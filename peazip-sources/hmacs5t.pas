unit HMACS5T;

{HMAC SHA512/224 and SHA512/256 message authentication}


interface

(*************************************************************************

 DESCRIPTION     :  HMAC SHA512/224 and SHA512/256 message authentication

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :

 REMARKS         :

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.02.11  W.Ehrhardt  Initial version
 0.11     10.03.12  we          Adjust Hash names
 0.12     16.08.15  we          Removed $ifdef DLL / stdcall
************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2011-2015 Wolfgang Ehrhardt

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
  BTypes,Hash,HMAC,SHA5_224,SHA5_256;


procedure hmac_SHA5_224_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_SHA5_224_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_SHA5_224_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA5_224_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA5_224_final(var ctx: THMAC_Context; var mac: TSHA5_224Digest);
  {-end data input, calculate HMAC digest}


procedure hmac_SHA5_256_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_SHA5_256_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_SHA5_256_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA5_256_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA5_256_final(var ctx: THMAC_Context; var mac: TSHA5_256Digest);
  {-end data input, calculate HMAC digest}


implementation


{---------------------------------------------------------------------------}
procedure hmac_SHA5_224_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}
begin
  hmac_init(ctx, pSHA5_224_Desc, key, klen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_224_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}
begin
  hmac_SHA5_224_init(ctx, @skey[1], length(skey));
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_224_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_224_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_224_final(var ctx: THMAC_Context; var mac: TSHA5_224Digest);
  {-end data input, calculate HMAC digest}
var
  d: THashDigest;
begin
  hmac_final(ctx, d);
  move(d, mac, sizeof(mac));
end;



{---------------------------------------------------------------------------}
procedure hmac_SHA5_256_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}
begin
  hmac_init(ctx, pSHA5_256_Desc, key, klen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_256_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}
begin
  hmac_SHA5_256_init(ctx, @skey[1], length(skey));
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_256_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_256_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_SHA5_256_final(var ctx: THMAC_Context; var mac: TSHA5_256Digest);
  {-end data input, calculate HMAC digest}
var
  d: THashDigest;
begin
  hmac_final(ctx, d);
  move(d, mac, sizeof(mac));
end;

end.

