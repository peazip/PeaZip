unit SP_CBC;

(*************************************************************************

 DESCRIPTION     :  Serpent CBC functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  B.Schneier, Applied Cryptography, 2nd ed., ch. 9.3


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_CBC
 0.11     23.11.08  we          Uses BTypes
 0.12     01.08.10  we          Longint ILen in SP_CBC_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2008-2010 Wolfgang Ehrhardt

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
  BTypes, SP_Base;

{$ifdef CONST}

function  SP_CBC_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size, save IV}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_CBC_Reset(const IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, save IV}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  SP_CBC_Init(var Key; KeyBits: word; var IV: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size, save IV}

procedure SP_CBC_Reset(var IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, save IV}

{$endif}


function  SP_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}
  {$ifdef DLL} stdcall; {$endif}

function  SP_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
{$ifdef CONST}
  function SP_CBC_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
{$else}
  function SP_CBC_Init(var Key; KeyBits: word; var IV: TSPBlock; var ctx: TSPContext): integer;
{$endif}
  {-Serpent key expansion, error if invalid key size, encrypt IV}
begin
  SP_CBC_Init := SP_Init(Key, KeyBits, ctx);
  ctx.IV := IV;
end;


{---------------------------------------------------------------------------}
procedure SP_CBC_Reset({$ifdef CONST}const {$else} var {$endif} IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, save IV}
begin
  SP_Reset(ctx);
  ctx.IV := IV;
end;


{---------------------------------------------------------------------------}
function SP_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}
var
  i,n: longint;
  m: word;
begin

  SP_CBC_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      SP_CBC_Encrypt := SP_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      SP_CBC_Encrypt := SP_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div SPBLKSIZE; {Full blocks}
  m := ILen mod SPBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      SP_CBC_Encrypt := SP_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    SP_CBC_Encrypt := SP_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      {ct[i] = encr(ct[i-1] xor pt[i])}
      SP_xorblock(PSPBlock(ptp)^, IV, IV);
      SP_Encrypt(ctx, IV, IV);
      PSPBlock(ctp)^ := IV;
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      SP_xorblock(PSPBlock(ptp)^, IV, IV);
      SP_Encrypt(ctx, IV, IV);
      buf := IV;
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      for i:=0 to m-1 do IV[i] := IV[i] xor PSPBlock(ptp)^[i];
      SP_Encrypt(ctx, IV, PSPBlock(ctp)^);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
      move(buf,PSPBlock(ctp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;

end;


{---------------------------------------------------------------------------}
function SP_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}
var
  i,n: longint;
  m: word;
  tmp: TSPBlock;
begin

  SP_CBC_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      SP_CBC_Decrypt := SP_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      SP_CBC_Decrypt := SP_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div SPBLKSIZE; {Full blocks}
  m := ILen mod SPBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      SP_CBC_Decrypt := SP_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    SP_CBC_Decrypt := SP_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      {pt[i] = decr(ct[i]) xor ct[i-1])}
      buf := IV;
      IV  := PSPBlock(ctp)^;
      SP_Decrypt(ctx, IV, PSPBlock(ptp)^);
      SP_xorblock(PSPBlock(ptp)^, buf, PSPBlock(ptp)^);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing, L=ILen (Schneier's n)}
      buf := IV;                       {C(L-2)}
      SP_Decrypt(ctx, PSPBlock(ctp)^, IV);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
      fillchar(tmp,sizeof(tmp),0);
      move(PSPBlock(ctp)^,tmp,m);     {c[L]|0}
      SP_xorblock(tmp,IV,IV);
      tmp := IV;
      move(PSPBlock(ctp)^,tmp,m);     {c[L]| C'}
      SP_Decrypt(ctx,tmp,tmp);
      SP_xorblock(tmp, buf, PSPBlock(ptp)^);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      move(IV,PSPBlock(ptp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;

end;

end.
