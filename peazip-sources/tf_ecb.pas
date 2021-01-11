unit TF_ECB;

(*************************************************************************

 DESCRIPTION     :  Twofish ECB functions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  B.Schneier, Applied Cryptography, 2nd ed., ch. 9.1


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     29.05.06  W.Ehrhardt  Initial version analog BF_ECB
 0.11     15.11.08  we          Use Ptr2Inc from BTypes
 0.12     31.07.10  we          Longint ILen in TF_ECB_En/Decrypt
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006-2010 Wolfgang Ehrhardt

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
  BTypes, TF_Base;


function  TF_ECB_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TTFContext): integer;
  {-TF key expansion, error if invalid key size}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_ECB_Reset(var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag}
  {$ifdef DLL} stdcall; {$endif}

function  TF_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}

function  TF_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
procedure TF_ECB_Reset(var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag}
begin
  TF_Reset(ctx);
end;


{---------------------------------------------------------------------------}
function TF_ECB_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TTFContext): integer;
  {-TF key expansion, error if invalid key size}
begin
  TF_ECB_Init := TF_Init(Key, KeyBits, ctx);
end;



{---------------------------------------------------------------------------}
function TF_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TTFBlock;
begin

  TF_ECB_Encrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      TF_ECB_Encrypt := TF_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      TF_ECB_Encrypt := TF_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div TFBLKSIZE; {Full blocks}
  m := ILen mod TFBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      TF_ECB_Encrypt := TF_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    TF_ECB_Encrypt := TF_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      TF_Encrypt(ctx, PTFBlock(ptp)^, PTFBlock(ctp)^);
      inc(Ptr2Inc(ptp),TFBLKSIZE);
      inc(Ptr2Inc(ctp),TFBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      TF_Encrypt(ctx, PTFBlock(ptp)^, buf);
      inc(Ptr2Inc(ptp),TFBLKSIZE);
      tmp := buf;
      move(PTFBlock(ptp)^, tmp, m);
      TF_Encrypt(ctx, tmp, PTFBlock(ctp)^);
      inc(Ptr2Inc(ctp),TFBLKSIZE);
      move(buf,PTFBlock(ctp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;


{---------------------------------------------------------------------------}
function TF_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}
var
  i,n: longint;
  m: word;
  tmp: TTFBlock;
begin

  TF_ECB_Decrypt := 0;
  if ILen<0 then ILen := 0;

  if (ptp=nil) or (ctp=nil) then begin
    if ILen>0 then begin
      TF_ECB_Decrypt := TF_Err_NIL_Pointer;
      exit;
    end;
  end;

  {$ifdef BIT16}
    if (ofs(ptp^)+ILen>$FFFF) or (ofs(ctp^)+ILen>$FFFF) then begin
      TF_ECB_Decrypt := TF_Err_Invalid_16Bit_Length;
      exit;
    end;
  {$endif}

  n := ILen div TFBLKSIZE; {Full blocks}
  m := ILen mod TFBLKSIZE; {Remaining bytes in short block}
  if m<>0 then begin
    if n=0 then begin
      TF_ECB_Decrypt := TF_Err_Invalid_Length;
      exit;
    end;
    dec(n);           {CTS: special treatment of last TWO blocks}
  end;

  {Short block must be last, no more processing allowed}
  if ctx.Flag and 1 <> 0 then begin
    TF_ECB_Decrypt := TF_Err_Data_After_Short_Block;
    exit;
  end;

  with ctx do begin
    for i:=1 to n do begin
      TF_Decrypt(ctx, PTFBlock(ctp)^, PTFBlock(ptp)^);
      inc(Ptr2Inc(ptp),TFBLKSIZE);
      inc(Ptr2Inc(ctp),TFBLKSIZE);
    end;
    if m<>0 then begin
      {Cipher text stealing}
      TF_Decrypt(ctx, PTFBlock(ctp)^, buf);
      inc(Ptr2Inc(ctp),TFBLKSIZE);
      tmp := buf;
      move(PTFBlock(ctp)^, tmp, m);
      TF_Decrypt(ctx, tmp, PTFBlock(ptp)^);
      inc(Ptr2Inc(ptp),TFBLKSIZE);
      move(buf,PTFBlock(ptp)^,m);
      {Set short block flag}
      Flag := Flag or 1;
    end;
  end;
end;

end.
