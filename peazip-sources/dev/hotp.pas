unit HOTP;

{HMAC-SHA1 based One-Time Password Algorithm}


interface

(*************************************************************************

 DESCRIPTION     :  HMAC-SHA1 based One-Time Password Algorithm

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D12, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  RFC 4226 (http://tools.ietf.org/html/rfc4226)
                    D. M'Raihi et al: HOTP: An HMAC-Based One-Time Password Algorithm

 REMARKS         :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.03.10  W.Ehrhardt  Initial version
 0.11     16.03.10  we          empty result if error (SHA1 not registered)
 0.12     16.03.10  we          Version for T5-6
 0.13     17.03.10  we          string version hotp_generate_otps
 0.14     17.03.10  we          set/inc count procedures
 0.15     17.03.10  we          hotp_selftest
 0.16     20.03.10  we          code cleanup
************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2010 Wolfgang Ehrhardt

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

const
  MinDigits  = 6;   {Minimum value for codeDigits}
  MaxDigits  = 9;   {Maximum value for codeDigits}

type
  THOTPCount = packed array[0..7] of byte;  {64 bit MSB HOTP counter}


function hotp_generate_otp(psecret: pointer; slen: word;
                           {$ifdef CONST}const{$endif} movingFactor: THOTPCount;
                           codeDigits, truncOffset: integer): str255;
  {-Return an OTP string of codeDigits decimal digits, empty if error.    }
  { psecret,slen: pointer to shared secret and length of secret in bytes  }
  { movingFactor: the counter, time, etc. that changes on a per use basis.}
  { codeDigits  : the number of decimal digits in the result string       }
  { truncOffset : the offset into the HMAC result to start truncation. If }
  {               not in [0..15], then dynamic truncation will be used.   }

function hotp_generate_otps({$ifdef CONST}const{$endif} secret: str255;
                            {$ifdef CONST}const{$endif} movingFactor: THOTPCount;
                            codeDigits, truncOffset: integer): str255;
  {-Return an OTP string of codeDigits decimal digits, empty if error.    }
  { secret      : the shared secret as (ANSI) string                      }
  { movingFactor: the counter, time, etc. that changes on a per use basis.}
  { codeDigits  : the number of decimal digits in the result string       }
  { truncOffset : the offset into the HMAC result to start truncation. If }
  {               not in [0..15], then dynamic truncation will be used.   }


procedure hotp_set_count32(var cnt: THOTPCount; chi, clo: longint);
  {-Transform the two 32 bit LSB integers to a 64 bit MSB counter}

{$ifdef HAS_INT64}
procedure hotp_set_count64(var cnt: THOTPCount; cint64: int64);
  {-Transform the 64 bit LSB integer to a 64 bit MSB counter}
{$endif}

procedure hotp_inc_count(var cnt: THOTPCount);
  {-Increment MSB counter cnt mod 2^64}

function  hotp_selftest: boolean;
  {-Simple selftest of (most) hotp functions}


implementation


type
  TBA4 = packed array[0..3] of byte;

{---------------------------------------------------------------------------}
function hotp_generate_otp(psecret: pointer; slen: word;
                           {$ifdef CONST}const{$endif} movingFactor: THOTPCount;
                           codeDigits, truncOffset: integer): str255;
  {-Return an OTP string of codeDigits decimal digits, empty if error.    }
  { psecret,slen: pointer to shared secret and length of secret in bytes  }
  { movingFactor: the counter, time, etc. that changes on a per use basis.}
  { codeDigits  : the number of decimal digits in the result string       }
  { truncOffset : the offset into the HMAC result to start truncation. If }
  {               not in [0..15], then dynamic truncation will be used.   }
var
  ctx: THMAC_Context;
  mac: THashDigest;
  phash: PHashDesc;
  i,off: integer;
  bincode,dc: longint;
  res: string[12];
const
  pow10: array[MinDigits..MaxDigits] of longint = (1000000,10000000,100000000,1000000000);
  dig10: array[0..9] of char8 = '0123456789';
begin
  res := '';
  phash := FindHash_by_ID(_SHA1);
  {result will be empty if phash=nil}
  if phash<>nil then begin
    {Calculate HMAC(Secret, movingFactor)}
    hmac_init(ctx, phash, psecret, slen);
    hmac_updateXL(ctx, @movingFactor, sizeof(movingFactor));
    hmac_final(ctx, mac);
    {Truncation (with LSB -> MSB conversion)}
    off := truncOffset;
    if (off<0) or (off>15) then off := mac[19] and 15; {dynamic truncation}
    for i:=0 to 3 do TBA4(bincode)[3-i] := mac[off+i];
    {encode truncated HMAC}
    if codeDigits<MinDigits then codeDigits := MinDigits;
    if codeDigits>MaxDigits then codeDigits := MaxDigits;
    dc := (bincode and MaxLongint) mod pow10[codeDigits];
    for i:=1 to codeDigits do begin
      res  := dig10[dc mod 10] + res;
      dc := dc div 10;
    end;
  end;
  hotp_generate_otp := res;
end;


{---------------------------------------------------------------------------}
function hotp_generate_otps({$ifdef CONST}const{$endif} secret: str255;
                            {$ifdef CONST}const{$endif} movingFactor: THOTPCount;
                            codeDigits, truncOffset: integer): str255;
  {-Return an OTP string of codeDigits decimal digits, empty if error.    }
  { secret      : the shared secret as (ANSI) string                      }
  { movingFactor: the counter, time, etc. that changes on a per use basis.}
  { codeDigits  : the number of decimal digits in the result string       }
  { truncOffset : the offset into the HMAC result to start truncation. If }
  {               not in [0..15], then dynamic truncation will be used.   }

begin
  hotp_generate_otps := hotp_generate_otp(@secret[1], length(secret), movingFactor, codeDigits, truncOffset);
end;


{---------------------------------------------------------------------------}
procedure hotp_set_count32(var cnt: THOTPCount; chi, clo: longint);
  {-Transform the two 32 bit LSB integers to a 64 bit MSB counter}
begin
  cnt[0] := TBA4(chi)[3];
  cnt[1] := TBA4(chi)[2];
  cnt[2] := TBA4(chi)[1];
  cnt[3] := TBA4(chi)[0];
  cnt[4] := TBA4(clo)[3];
  cnt[5] := TBA4(clo)[2];
  cnt[6] := TBA4(clo)[1];
  cnt[7] := TBA4(clo)[0];
end;


{$ifdef HAS_INT64}
{---------------------------------------------------------------------------}
procedure hotp_set_count64(var cnt: THOTPCount; cint64: int64);
  {-Transform the 64 bit LSB integer to a 64 bit MSB counter}
var
  cl64: THOTPCount absolute cint64;
begin
  cnt[0] := cl64[7];
  cnt[1] := cl64[6];
  cnt[2] := cl64[5];
  cnt[3] := cl64[4];
  cnt[4] := cl64[3];
  cnt[5] := cl64[2];
  cnt[6] := cl64[1];
  cnt[7] := cl64[0];
end;
{$endif}


{---------------------------------------------------------------------------}
procedure hotp_inc_count(var cnt: THOTPCount);
  {-Increment MSB counter cnt mod 2^64}
var
  j: integer;
begin
  for j:=7 downto 0 do begin
    if cnt[j]=$FF then cnt[j] := 0
    else begin
      inc(cnt[j]);
      exit;
    end;
  end;
end;


(*
from RFC 4226, Appendix D - HOTP Algorithm: Test Values}

The following test data uses the ASCII string
"12345678901234567890" for the secret:

Secret = 0x3132333435363738393031323334353637383930

Table 1 details for each count, the intermediate HMAC value.

Count    Hexadecimal HMAC-SHA-1(secret, count)
0        cc93cf18508d94934c64b65d8ba7667fb7cde4b0
1        75a48a19d4cbe100644e8ac1397eea747a2d33ab
2        0bacb7fa082fef30782211938bc1c5e70416ff44
3        66c28227d03a2d5529262ff016a1e6ef76557ece
4        a904c900a64b35909874b33e61c5938a8e15ed1c
5        a37e783d7b7233c083d4f62926c7a25f238d0316
6        bc9cd28561042c83f219324d3c607256c03272ae
7        a4fb960c0bc06e1eabb804e5b397cdc4b45596fa
8        1b3c89f65e6c9e883012052823443f048b4332db
9        1637409809a679dc698207310c8c7fc07290d9e5

                  Truncated
Count    Hexadecimal    Decimal        HOTP
0        4c93cf18       1284755224     755224
1        41397eea       1094287082     287082
2         82fef30        137359152     359152
3        66ef7655       1726969429     969429
4        61c5938a       1640338314     338314
5        33c083d4        868254676     254676
6        7256c032       1918287922     287922
7         4e5b397         82162583     162583
8        2823443f        673399871     399871
9        2679dc69        645520489     520489
*)

{---------------------------------------------------------------------------}
function hotp_selftest: boolean;
  {-Simple selftest of (most) hotp functions}
const
  secret: string[20] = '12345678901234567890';
var
  count : THOTPCount;
begin
  hotp_selftest := false;

  {Data from RFC 4226, Appendix D - HOTP Algorithm: Test Values}
  fillchar(count, sizeof(count),0);
  hotp_set_count32(count, -1, -1);
  hotp_inc_count(count);
  if hotp_generate_otp(@secret[1], length(secret), count, 6, -1) <> '755224' then exit;
  hotp_inc_count(count);
  if hotp_generate_otps(secret, count, 6, $b) <> '287082' then exit;
  {$ifdef HAS_INT64}
     hotp_set_count64(count, 5);
     if hotp_generate_otps(secret, count, 6, -1) <> '254676' then exit;
  {$endif}
  hotp_set_count32(count, 0, 7);
  if hotp_generate_otps(secret, count, 6, -1) <> '162583' then exit;

  {Data from Simon Josefsson HOTP Toolkit, http://www.nongnu.org/hotp-toolkit/}
  hotp_set_count32(count, 0, 4);
  if hotp_generate_otps(secret, count, 7, -1) <> '0338314' then exit;
  hotp_set_count32(count, 0, 19);
  if hotp_generate_otps(secret, count, 8, -1) <> '21578337' then exit;

  hotp_selftest := true;
end;

end.
