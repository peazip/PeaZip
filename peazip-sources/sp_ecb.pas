unit SP_ECB;

(*************************************************************************

 DESCRIPTION     :  Serpent ECB functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  B.Schneier, Applied Cryptography, 2nd ed., ch. 9.1


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_ECB
 0.11     23.11.08  we          Uses BTypes
 0.12     01.08.10  we          Longint ILen in SP_ECB_En/Decrypt
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


function  SP_ECB_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

procedure SP_ECB_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}
  {$ifdef DLL} stdcall; {$endif}

function  SP_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}

function  SP_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
procedure SP_ECB_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}
begin
  SP_Reset(ctx);
end;


{---------------------------------------------------------------------------}
function SP_ECB_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size}
begin
  SP_ECB_Init := SP_Init(Key, KeyBits, ctx);
end;


{---------------------------------------------------------------------------}
function SP_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TSPBlock;
begin

  SP_ECB_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      SP_ECB_Encrypt := SP_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      SP_ECB_Encrypt := SP_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div SPBLKSIZE; {Full blocks}
  m := ILen mod SPBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      SP_ECB_Encrypt := SP_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    SP_ECB_Encrypt := SP_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      SP_Encrypt(ctx, PSPBlock(ptp)^, PSPBlock(ctp)^);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      SP_Encrypt(ctx, PSPBlock(ptp)^, buf);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      tmp := buf;
      move(PSPBlock(ptp)^, tmp, m);
      SP_Encrypt(ctx, tmp, PSPBlock(ctp)^);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
      move(buf,PSPBlock(ctp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function SP_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TSPBlock;
begin

  SP_ECB_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      SP_ECB_Decrypt := SP_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      SP_ECB_Decrypt := SP_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div SPBLKSIZE; {Full blocks}
  m := ILen mod SPBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      SP_ECB_Decrypt := SP_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    SP_ECB_Decrypt := SP_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      SP_Decrypt(ctx, PSPBlock(ctp)^, PSPBlock(ptp)^);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      SP_Decrypt(ctx, PSPBlock(ctp)^, buf);
      inc(Ptr2Inc(ctp),SPBLKSIZE);
      tmp := buf;
      move(PSPBlock(ctp)^, tmp, m);
      SP_Decrypt(ctx, tmp, PSPBlock(ptp)^);
      inc(Ptr2Inc(ptp),SPBLKSIZE);
      move(buf,PSPBlock(ptp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;

end.
