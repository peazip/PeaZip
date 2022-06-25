unit scrypt;

{scrypt key derivation functions}

interface

{$i std.inc}

uses
  BTypes, memh, Hash, kdf;

(*************************************************************************

 DESCRIPTION     :  scrypt key derivation functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REMARKS         :  - assumes little-endian (checked for FPC)
                    - very restricted for 16-bit because all buffer sizes must be < 64KB

 REFERENCES      :  - Colin Percival, Stronger Key Derivation via Sequential Memory-Hard
                      Functions, http://www.tarsnap.com/scrypt/scrypt.pdf
                    - Source code from http://www.tarsnap.com/scrypt/scrypt-1.1.6.tgz
                    - Specification and test vectors from http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     13.08.14  W.Ehrhardt  Initial BP7, test case salsa20/8 using salsa unit
 0.11     14.08.14  we          Test case salsa20/8 without salsa unit
 0.12     14.08.14  we          blockmix_salsa8
 0.13     14.08.14  we          smix
 0.14     14.08.14  we          pbkfd2_hmac_sha256
 0.15     14.08.14  we          scrypt_kdf
 0.16     15.08.14  we          Support for other compilers
 0.17     15.08.14  we          Removed restriction on r*p (longint sLen, dkLen in pbkdf2)
 0.18     15.08.14  we          String versions scrypt_kdfs, scrypt_kdfss
 0.19     15.08.14  we          Allow pPW=nil or salt=nil
 0.20     15.08.14  we          Simply parameter checks, comments
 0.21     16.08.14  we          Separate unit
 0.22     16.08.14  we          More parameter checks
 0.23     26.08.15  we          Faster (reordered) salsa20/8
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2014-2015 Wolfgang Ehrhardt

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

const
  sc_kdf_err_mem        = -1;  {Error from malloc}
  sc_kdf_err_inv_nrp    = -2;  {Invalid N,r,p. Note N must be a power of 2}
  sc_kdf_err_64KB       = -3;  {16-bit malloc with more than 64 KB}
  sc_kdf_err_big_endian = -4;  {(FPC) compiling with big-endian}


function scrypt_kdf(pPW: pointer; pLen: word; salt: pointer; sLen,N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW and salt using scrypt with parameters N,r,p}

function scrypt_kdfs(sPW: Str255; salt: pointer; sLen,N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password sPW and salt using scrypt with parameters N,r,p}

function scrypt_kdfss(sPW, salt: Str255; N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password sPW and salt using scrypt with parameters N,r,p}


implementation

uses
  sha256; {Register SHA256 for HMAC-SHA256}

type
  TLA16 = array[0..15] of longint;
  TBA64 = array[0..63] of byte;


{---------------------------------------------------------------------------}
procedure salsa20_8(var B: TLA16);
  {-Apply the salsa20/8 core to the provided block B}
var
  i: integer;
  y: longint;
  x: TLA16;
begin
  {This is the PurePascal version from my salsa20 unit}
  x := B;
{$ifdef OldOrder}
  for i:=0 to 3 do begin
    y := x[ 0] + x[12]; x[ 4] := x[ 4] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 4] + x[ 0]; x[ 8] := x[ 8] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 8] + x[ 4]; x[12] := x[12] xor ((y shl 13) or (y shr (32-13)));
    y := x[12] + x[ 8]; x[ 0] := x[ 0] xor ((y shl 18) or (y shr (32-18)));

    y := x[ 5] + x[ 1]; x[ 9] := x[ 9] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 9] + x[ 5]; x[13] := x[13] xor ((y shl 09) or (y shr (32-09)));
    y := x[13] + x[ 9]; x[ 1] := x[ 1] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 1] + x[13]; x[ 5] := x[ 5] xor ((y shl 18) or (y shr (32-18)));

    y := x[10] + x[ 6]; x[14] := x[14] xor ((y shl 07) or (y shr (32-07)));
    y := x[14] + x[10]; x[ 2] := x[ 2] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 2] + x[14]; x[ 6] := x[ 6] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 6] + x[ 2]; x[10] := x[10] xor ((y shl 18) or (y shr (32-18)));

    y := x[15] + x[11]; x[ 3] := x[ 3] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 3] + x[15]; x[ 7] := x[ 7] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 7] + x[ 3]; x[11] := x[11] xor ((y shl 13) or (y shr (32-13)));
    y := x[11] + x[ 7]; x[15] := x[15] xor ((y shl 18) or (y shr (32-18)));

    y := x[ 0] + x[ 3]; x[ 1] := x[ 1] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 1] + x[ 0]; x[ 2] := x[ 2] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 2] + x[ 1]; x[ 3] := x[ 3] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 3] + x[ 2]; x[ 0] := x[ 0] xor ((y shl 18) or (y shr (32-18)));

    y := x[ 5] + x[ 4]; x[ 6] := x[ 6] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 6] + x[ 5]; x[ 7] := x[ 7] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 7] + x[ 6]; x[ 4] := x[ 4] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 4] + x[ 7]; x[ 5] := x[ 5] xor ((y shl 18) or (y shr (32-18)));

    y := x[10] + x[ 9]; x[11] := x[11] xor ((y shl 07) or (y shr (32-07)));
    y := x[11] + x[10]; x[ 8] := x[ 8] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 8] + x[11]; x[ 9] := x[ 9] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 9] + x[ 8]; x[10] := x[10] xor ((y shl 18) or (y shr (32-18)));

    y := x[15] + x[14]; x[12] := x[12] xor ((y shl 07) or (y shr (32-07)));
    y := x[12] + x[15]; x[13] := x[13] xor ((y shl 09) or (y shr (32-09)));
    y := x[13] + x[12]; x[14] := x[14] xor ((y shl 13) or (y shr (32-13)));
    y := x[14] + x[13]; x[15] := x[15] xor ((y shl 18) or (y shr (32-18)));
  end;
{$else}
  for i:=0 to 3 do begin
    y := x[ 0] + x[12]; x[ 4] := x[ 4] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 5] + x[ 1]; x[ 9] := x[ 9] xor ((y shl 07) or (y shr (32-07)));
    y := x[10] + x[ 6]; x[14] := x[14] xor ((y shl 07) or (y shr (32-07)));
    y := x[15] + x[11]; x[ 3] := x[ 3] xor ((y shl 07) or (y shr (32-07)));

    y := x[ 4] + x[ 0]; x[ 8] := x[ 8] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 9] + x[ 5]; x[13] := x[13] xor ((y shl 09) or (y shr (32-09)));
    y := x[14] + x[10]; x[ 2] := x[ 2] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 3] + x[15]; x[ 7] := x[ 7] xor ((y shl 09) or (y shr (32-09)));

    y := x[ 8] + x[ 4]; x[12] := x[12] xor ((y shl 13) or (y shr (32-13)));
    y := x[13] + x[ 9]; x[ 1] := x[ 1] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 2] + x[14]; x[ 6] := x[ 6] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 7] + x[ 3]; x[11] := x[11] xor ((y shl 13) or (y shr (32-13)));

    y := x[12] + x[ 8]; x[ 0] := x[ 0] xor ((y shl 18) or (y shr (32-18)));
    y := x[ 1] + x[13]; x[ 5] := x[ 5] xor ((y shl 18) or (y shr (32-18)));
    y := x[ 6] + x[ 2]; x[10] := x[10] xor ((y shl 18) or (y shr (32-18)));
    y := x[11] + x[ 7]; x[15] := x[15] xor ((y shl 18) or (y shr (32-18)));

    y := x[ 0] + x[ 3]; x[ 1] := x[ 1] xor ((y shl 07) or (y shr (32-07)));
    y := x[ 5] + x[ 4]; x[ 6] := x[ 6] xor ((y shl 07) or (y shr (32-07)));
    y := x[10] + x[ 9]; x[11] := x[11] xor ((y shl 07) or (y shr (32-07)));
    y := x[15] + x[14]; x[12] := x[12] xor ((y shl 07) or (y shr (32-07)));

    y := x[ 1] + x[ 0]; x[ 2] := x[ 2] xor ((y shl 09) or (y shr (32-09)));
    y := x[ 6] + x[ 5]; x[ 7] := x[ 7] xor ((y shl 09) or (y shr (32-09)));
    y := x[11] + x[10]; x[ 8] := x[ 8] xor ((y shl 09) or (y shr (32-09)));
    y := x[12] + x[15]; x[13] := x[13] xor ((y shl 09) or (y shr (32-09)));

    y := x[ 2] + x[ 1]; x[ 3] := x[ 3] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 7] + x[ 6]; x[ 4] := x[ 4] xor ((y shl 13) or (y shr (32-13)));
    y := x[ 8] + x[11]; x[ 9] := x[ 9] xor ((y shl 13) or (y shr (32-13)));
    y := x[13] + x[12]; x[14] := x[14] xor ((y shl 13) or (y shr (32-13)));

    y := x[ 3] + x[ 2]; x[ 0] := x[ 0] xor ((y shl 18) or (y shr (32-18)));
    y := x[ 4] + x[ 7]; x[ 5] := x[ 5] xor ((y shl 18) or (y shr (32-18)));
    y := x[ 9] + x[ 8]; x[10] := x[10] xor ((y shl 18) or (y shr (32-18)));
    y := x[14] + x[13]; x[15] := x[15] xor ((y shl 18) or (y shr (32-18)));
  end;
{$endif}
  for i:=0 to 15 do B[i] := x[i] + B[i]
end;


{---------------------------------------------------------------------------}
procedure xorblock(dest, src: pByte; len: longint);
  {-xor block dest := dest xor src}
begin
  while len > 0 do begin
    dest^ := dest^ xor src^;
    inc(Ptr2Inc(dest));
    inc(Ptr2Inc(src));
    dec(len);
  end;
end;


{---------------------------------------------------------------------------}
function scrypt_kdfs(sPW: Str255; salt: pointer; sLen,N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password sPW and salt using scrypt with parameters N,r,p}
begin
  scrypt_kdfs := scrypt_kdf(@sPW[1], length(sPw), salt,sLen,N,r,p,DK,dkLen);
end;


{---------------------------------------------------------------------------}
function scrypt_kdfss(sPW, salt: Str255; N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password sPW and salt using scrypt with parameters N,r,p}
begin
  scrypt_kdfss := scrypt_kdf(@sPW[1],length(sPw),@salt[1],length(salt),N,r,p,DK,dkLen);
end;


{The following scrypt Pascal functions are based on Colin Percival's C}
{function crypto_scrypt-ref.c distributed with the BSD-style license: }

(*-
 * Copyright 2009 Colin Percival
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * This file was originally written by Colin Percival as part of the Tarsnap
 * online backup system.
 *)


{---------------------------------------------------------------------------}
procedure blockmix_salsa8(B, Y: pByte; r: longint);
  {-Compute B = BlockMix_(salsa20/8, r)(B).  The input B must be 128*r}
  { bytes in length; the temporary space Y must also be the same size.}
var
  i: longint;
  pb,py: pByte;
  X: TBA64;
begin
  (* Parameters:
     r       Block size parameter.
     B[0],  ..., B[2*r-1]   input vector of 2*r 64-byte blocks
     B'[0], ..., B'[2*r-1] output vector of 2*r 64-byte blocks

   * Algorithm:
   1. X = B[2*r-1]
   2. for i = 0 to 2*r-1 do
        T = X xor B[i]
        X = Salsa(T)
        Y[i] = X
      end for
   3. B' = (Y[0], Y[2], ..., Y[2 * r - 2],
            Y[1], Y[3], ..., Y[2 * r - 1])
  *)
  {Step 1}
  pb := B;
  inc(Ptr2Inc(pb), (2*r-1)*64);
  move(pb^, X, 64);
  pb := B;
  py := Y;
  {Steps 2}
  for i:= 0 to 2*r - 1 do begin
    xorblock(pByte(@X), pByte(pb), 64);
    inc(Ptr2Inc(pb), 64);
    salsa20_8(TLA16(X));
    move(X, py^, 64);
    inc(Ptr2Inc(py), 64);
  end;
  {Step 3}
  pb := B;
  py := Y;
  for i:=0 to r-1 do begin
    move(py^, pb^, 64);
    inc(Ptr2Inc(pb), 64);
    inc(Ptr2Inc(py), 128);
  end;
  py := Y;
  inc(Ptr2Inc(py), 64);
  for i:=0 to r-1 do begin
    move(py^, pb^, 64);
    inc(Ptr2Inc(pb), 64);
    inc(Ptr2Inc(py), 128);
  end;
end;


{---------------------------------------------------------------------------}
procedure smix(B: pByte; r,N: longint; V, XY: pByte);
  {-Compute B = SMix_r(B, N).  The input B must be 128*r bytes in length; }
  { the temporary storage V must be 128*r*N bytes in length; the temporary}
  { storage XY must be 256*r bytes in length. N must be a power of 2.     }
var
  i,j,r128: longint;
  px,py,pv,pj: pByte;
begin
  (* Algorithm scryptROMix
     Input:  r   Block size parameter.
             B   Input octet vector of length 128 * r octets.
             N   CPU/Memory cost parameter, must be larger than 1,
     Output: B'  Output octet vector of length 128 * r octets.
  *)
  {WE: Note that the reference performs the salsa steps as: Convert to LE,}
  {salsa compress, convert from LE. Skipped here assuming little-endian.  }
  r128 := 128*r;
  pv := V;
  px := XY;
  py := XY;
  inc(Ptr2Inc(py), r128);
  move(B^,px^,r128);
  for i:=0 to N-1 do begin
    move(px^, pv^, r128);
    inc(Ptr2Inc(pv), r128);
    blockmix_salsa8(px, py, r);
  end;
  pj := XY;
  inc(Ptr2Inc(pj), (2*r - 1)*64);
  for i:=0 to N-1 do begin
    {The next line is the function Integerify(X) mod N, i.e. the remainder}
    {of dividing the multi-precision little-endian integer B[2*r-1] by N. }
    {Because we assume little-endian and N must be a power of two, this   }
    {reduces to a simple AND operation!}
    j := pLongint(pj)^ and (N-1);
    {X = blockmix(V[j] xor X)}
    pv := V;
    inc(Ptr2Inc(pv), j*r128);
    xorblock(px, pv, r128);
    blockmix_salsa8(px, py, r);
  end;
  move(px^, B^, r128);
end;


{---------------------------------------------------------------------------}
function pbkfd2_hmac_sha256(pPW: pointer; pLen: word; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW using salt and iteration count C using (hmac-)sha256}
var
  phash: PHashDesc;
begin
  {Note: pbkdf2 will return error indicator phash=nil if _SHA256 is not found!}
  phash := FindHash_by_ID(_SHA256);
  pbkfd2_hmac_sha256 := pbkdf2(phash,pPW,pLen,salt,sLen,C,DK,dkLen);
end;


{---------------------------------------------------------------------------}
function scrypt_kdf(pPW: pointer; pLen: word; salt: pointer; sLen,N,r,p: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW and salt using scrypt with parameters N,r,p}
var
  pB,pV,pXY,pw: pByte;
  sB,sV,sXY,i: longint;
  err: integer;
begin
  {$ifdef ENDIAN_BIG}
    scrypt_kdf := sc_kdf_err_big_endian;
    exit;
  {$endif}

  {Check parameter values and if N is a power of two}
  i := MaxLongint div 128;
  if (r<1) or (r > i div 2)
     or (p<1) or (p > i div r)
     or (N<2) or (N and (N-1) <> 0) or (N > i div r) then
  begin
    scrypt_kdf := sc_kdf_err_inv_nrp;
    exit;
  end;

  {Compute and store sizes, needed for releasing memory}
  {sB = 128*r*p,  sXY = 256*r, sV = 128*r*N}
  i   := 128*r;  {128 <= i < $40000000}
  sB  := p*i;
  sXY := 2*i;
  sV  := N*i;

  {Simple sanity checks for possible remaining overflows}
  if (sV<i) or (sXY<i) or (sB<i) then begin
    scrypt_kdf := sc_kdf_err_inv_nrp;
    exit;
  end;
  {$ifdef BIT16}
    if (dkLen>$FF00) or (sLen>$FF00) or (sB>$FF00) or (sXY>$FF00) or (sV>$FF00) then begin
      scrypt_kdf := sc_kdf_err_64KB;
      exit;
    end;
  {$endif}

  pB  := malloc(sB);
  pV  := malloc(sV);
  pXY := malloc(sXY);
  if (pB<>nil) and (pV<>nil) and (pXY<>nil) then begin
    err := pbkfd2_hmac_sha256(pPW, pLen, salt, sLen, 1, pB^, sB);
    if err=0 then begin
      pw := pB;
      for i:=0 to p-1 do begin
        smix(pw, r, N, pV, pXY);
        inc(Ptr2Inc(pw), r*128);
      end;
      err := pbkfd2_hmac_sha256(pPW, pLen, pB, sB, 1, DK, dKlen);
    end;
    scrypt_kdf := err;
  end
  else scrypt_kdf := sc_kdf_err_mem;
  mfree(pB,sB);
  mfree(pV,sV);
  mfree(pXY,sXY);
end;

end.
