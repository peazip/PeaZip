unit AES_CBC;

(*************************************************************************

 DESCRIPTION     :  AES CBC functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [3] http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf
                    [1] http://csrc.nist.gov/fips/fips-197.pdf
                    [4] Cipher text stealing: Schneier, Applied Cryptography 2.ed, ch.9.3


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20.09.03  we          initial version
 0.20     21.09.03  we          Cipher text stealing
 0.21     21.09.03  we          with Flag, functions, error codes
 0.22     27.09.03  we          FPC/go32v2
 0.23     03.10.03  we          3-para encr/decr
 0.24     03.10.03  we          Fix overwrite source bug for decrypt
 0.25     05.10.03  we          STD.INC, TP5-6
 0.26     12.06.04  we          uses BLKSIZE constant
 0.27     12.06.04  we          check for nil pointers
 0.28     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 0.29     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.30     01.12.04  we          No more processing after short block
 0.31     09.07.06  we          Checked: D9-D10
 0.34     16.11.08  we          Use Ptr2Inc from BTypes
 0.35     27.07.10  we          Longint ILen in AES_CBC_En/Decrypt
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

function AES_CBC_Init_Encr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}

function AES_CBC_Init_Decr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function AES_CBC_Init_Encr(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

function AES_CBC_Init_Decr(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
  {-AES key expansion, error if invalid key size, encrypt IV}

{$endif}


function AES_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}
  {$ifdef DLL} stdcall; {$endif}

function AES_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
  function AES_CBC_Init_Encr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
{$else}
  function AES_CBC_Init_Encr(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
{$endif}
  {-AES key expansion, error if invalid key size, encrypt IV}
begin
  {-AES key expansion, error if invalid key size}
  AES_CBC_Init_Encr := AES_Init_Encr(Key, KeyBits, ctx);
  ctx.IV := IV;
end;


{---------------------------------------------------------------------------}
{$ifdef CONST}
function AES_CBC_Init_Decr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
{$else}
function AES_CBC_Init_Decr(var Key; KeyBits: word; var IV: TAESBlock; var ctx: TAESContext): integer;
{$endif}
  {-AES key expansion, error if invalid key size, encrypt IV}
begin
  {-AES key expansion, error if invalid key size}
  AES_CBC_Init_Decr := AES_Init_Decr(Key, KeyBits, ctx);
  ctx.IV := IV;
end;


{---------------------------------------------------------------------------}
function AES_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}
var
  i,n: longint;
  m: word;
begin

  AES_CBC_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.Decrypt<>0 then begin
    AES_CBC_Encrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CBC_Encrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CBC_Encrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_CBC_Encrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    AES_CBC_Encrypt := AES_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      {ct[i] = encr(ct[i-1] xor pt[i]), cf. [3] 6.2}
      AES_XorBlock(PAESBlock(ptp)^, IV, IV);
      AES_Encrypt(ctx, IV, IV);
      PAESBlock(ctp)^ := IV;
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      AES_XorBlock(PAESBlock(ptp)^, IV, IV);
      AES_Encrypt(ctx, IV, IV);
      buf := IV;
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      for i:=0 to m-1 do IV[i] := IV[i] xor PAESBlock(ptp)^[i];
      AES_Encrypt(ctx, IV, PAESBlock(ctp)^);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      move(buf,PAESBlock(ctp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function AES_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}
var
  i,n: longint;
  m: word;
  tmp: TAESBlock;
begin

  AES_CBC_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if ctx.Decrypt=0 then begin
    AES_CBC_Decrypt := AES_Err_Invalid_Mode;
    exit;
  end;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      AES_CBC_Decrypt := AES_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      AES_CBC_Decrypt := AES_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div AESBLKSIZE; {Full blocks}
  m := ILen mod AESBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      AES_CBC_Decrypt := AES_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    AES_CBC_Decrypt := AES_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      {pt[i] = decr(ct[i]) xor ct[i-1]), cf. [3] 6.2}
      buf := IV;
      IV  := PAESBlock(ctp)^;
      AES_Decrypt(ctx, IV, PAESBlock(ptp)^);
      AES_XorBlock(PAESBlock(ptp)^, buf, PAESBlock(ptp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing, L=ILen (Schneier's n)}
      buf := IV;                       {C(L-2)}
      AES_Decrypt(ctx, PAESBlock(ctp)^, IV);
      inc(Ptr2Inc(ctp),AESBLKSIZE);
      fillchar(tmp,sizeof(tmp),0);
      move(PAESBlock(ctp)^,tmp,m);     {c[L]|0}
      AES_XorBlock(tmp,IV,IV);
      tmp := IV;
      move(PAESBlock(ctp)^,tmp,m);     {c[L]| C'}
      AES_Decrypt(ctx,tmp,tmp);
      AES_XorBlock(tmp, buf, PAESBlock(ptp)^);
      inc(Ptr2Inc(ptp),AESBLKSIZE);
      move(IV,PAESBlock(ptp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;

end.
