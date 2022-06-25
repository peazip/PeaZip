unit AES_CMAC;

(*************************************************************************

 DESCRIPTION   : AES CMAC routines

 REQUIREMENTS  : TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA : ---

 MEMORY USAGE  : ---

 DISPLAY MODE  : ---

 REFERENCES    : [1] NIST Special Publication 800-38B, Recommendation for Block
                     Cipher Modes of Operation: The CMAC Mode for Authentication
                     http://csrc.nist.gov/publications/nistpubs/800-38B/SP_800-38B.pdf
                 [2] OMAC page: http://www.nuee.nagoya-u.ac.jp/labs/tiwata/omac/omac.html
                 [3] T.Iwata and K.Kurosawa. OMAC: One-Key CBC MAC - Addendum
                     http://csrc.nist.gov/CryptoToolkit/modes/proposedmodes/omac/omac-ad.pdf


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version, wrapper for OMAC
 0.11     09.07.06  we          Calls to AES_OMAC_UpdateXL, AES_OMACx_Final
 0.12     28.07.10  we          AES_CMAC_Update with ILen: longint, XL Version with $define OLD_XL_Version
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
  AES_Type, AES_OMAC;

function  AES_CMAC_Init({$ifdef CONST} const Key {$else} var Key {$endif}; KeyBits: word; var ctx: TAESContext): integer;
  {-CMAC init: AES key expansion, error if inv. key size}
  {$ifdef DLL} stdcall; {$endif}

function AES_CMAC_Update(data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}
  {$ifdef DLL} stdcall; {$endif}

procedure AES_CMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate CMAC=OMAC1 tag}
  {$ifdef DLL} stdcall; {$endif}

{$ifdef OLD_XL_Version}
function  AES_CMAC_UpdateXL (data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}
{$endif}


implementation


{---------------------------------------------------------------------------}
function AES_CMAC_Init({$ifdef CONST} const Key {$else} var Key {$endif}; KeyBits: word; var ctx: TAESContext): integer;
  {-CMAC init: AES key expansion, error if inv. key size}
begin
  AES_CMAC_Init := AES_OMAC_Init(Key, KeyBits, ctx);
end;


{$ifdef OLD_XL_Version}
{---------------------------------------------------------------------------}
function AES_CMAC_UpdateXL (data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}
begin
  AES_CMAC_UpdateXL := AES_OMAC_Update(data, ILen, ctx);
end;
{$endif}


{---------------------------------------------------------------------------}
function AES_CMAC_Update(data: pointer; ILen: longint; var ctx: TAESContext): integer;
  {-CMAC data input, may be called more than once}
begin
  AES_CMAC_Update := AES_OMAC_Update(data, ILen, ctx);;
end;


{---------------------------------------------------------------------------}
procedure AES_CMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
  {-end data input, calculate CMAC=OMAC1 tag}
begin
  AES_OMACx_Final(false, tag, ctx);
end;


end.

