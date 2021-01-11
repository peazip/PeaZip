unit AES_ECB;

(*************************************************************************

 DESCRIPTION     :  AES ECB functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [3] http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
                    [1] http://csrc.nist.gov/fips/fips-197.pdf
                    [4] Cipher text stealing: Schneier, Applied Cryptography 2.ed, ch.9.1


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     21.09.03  we          initial version a la CBC
 0.11     27.09.03  we          FPC/go32v2
 0.12     03.10.03  we          3-para encr/decr
 0.13     05.10.03  we          STD.INC, TP5-6
 0.14     12.06.04  we          uses BLKSIZE constant
 0.15     12.06.04  we          check for nil pointers
 0.16     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.17     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.18     01.12.04  we          No more processing after short block
 0.19     09.07.06  we          Checked: D9-D10
 0.20     15.11.08  we          Use Ptr2Inc from BTypes
 0.21     27.07.10  we          Longint ILen in AES_ECB_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2010 Wolfgang Ehrhardt

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
  BTypes, AES_Type, AES_Base, AES_Encr, AES_Decr;


{$ifdef CONST}
function AES_ECB_Init_Encr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}
function AES_ECB_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}
{$else}
function AES_ECB_Init_Encr(var Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function AES_ECB_Init_Decr(var Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
{$endif}

function AES_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}

function AES_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
function AES_ECB_Init_Encr({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
begin
  {-AES key expansion, error if invalid key size}
  AES_ECB_Init_Encr := AES_Init_Encr(Key, KeyBits, ctx);
end;


{---------------------------------------------------------------------------}
function AES_ECB_Init_Decr({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size}
begin
  {-AES key expansion, error if invalid key size}
  AES_ECB_Init_Decr := AES_Init_Decr(Key, KeyBits, ctx);
end;


{---------------------------------------------------------------------------}
function AES_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TAESBlock;
begin

  AES_ECB_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.Decrypt<>0 then begin
    AES_ECB_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_ECB_Encrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_ECB_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_ECB_Encrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    AES_ECB_Encrypt := AES_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      AES_Encrypt(ctx, PAESBlock(ptp)^, PAESBlock(ctp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      AES_Encrypt(ctx, PAESBlock(ptp)^, buf);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      tmp := buf;
      move(PAESBlock(ptp)^, tmp, m);
      AES_Encrypt(ctx, tmp, PAESBlock(ctp)^);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      move(buf,PAESBlock(ctp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function AES_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TAESBlock;
begin

  AES_ECB_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.Decrypt=0 then begin
    AES_ECB_Decrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_ECB_Decrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_ECB_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_ECB_Decrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    AES_ECB_Decrypt := AES_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      AES_Decrypt(ctx, PAESBlock(ctp)^, PAESBlock(ptp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      AES_Decrypt(ctx, PAESBlock(ctp)^, buf);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      tmp := buf;
      move(PAESBlock(ctp)^, tmp, m);
      AES_Decrypt(ctx, tmp, PAESBlock(ptp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      move(buf,PAESBlock(ptp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;

end.
