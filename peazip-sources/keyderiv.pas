unit KeyDeriv;

{Key Derivation Function with SHAxx and Whirlpool, obsolete: use pb_kdf!}


interface

(*************************************************************************

 DESCRIPTION     :  RFC 2898: Password Based Key Derivation Function 2

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://www.faqs.org/rfcs/rfc2898.html
                    http://www.faqs.org/rfcs/rfc3211.html  [includes test vectors]

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     09.03.03  W.Ehrhardt  Initial version (BP7 port of Gladman code)
 1.10     14.08.03  we          Use HMACSHA1 vers 1.01
 1.11     15.08.03  we          Complete rewrite close to RFC PBKDF2,
                                only one ctx (no need to optimize speed!)
 1.12     15.08.03  we          TP 5.5, TP6.0
 1.13     27.09.03  we          FPC/go32v2
 1.21     05.10.03  we          with STD.INC, TP5
 1.22     04.12.03  we          comments, version for strings
 1.23     12.04.04  we          Delphi 7
 1.24     07.07.04  we          PBKDF2S with THMACSHA1_string, stdcall for DLL
 1.25     04.01.05  we          with HMAC256, IncMSB, TKD_String
 1.26     04.01.05  we          Counter C now longint (on user request)
 1.27     04.01.05  we          with HMAC512
 1.28     05.05.05  we          $ifndef SHA1ONLY to avoid SHA256/512 overhead
 1.29     11.12.05  we          Whirlpool
 1.30     17.01.06  we          Obsolete/legacy: shell for pb_kdf; HaltOnError
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2006 Wolfgang Ehrhardt

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
  Hash,HMAC,pb_kdf,
  {$ifndef SHA1ONLY}
    SHA256, SHA512, Whirl512,
  {$endif}
  SHA1;


type
  TKD_String = string[255];

const
  HaltOnError : boolean = true;  {RunError(255) on error return from kdf2}


procedure PBKDF2(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA1}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2S(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA}
  {$ifdef DLL} stdcall; {$endif}

{$ifndef SHA1ONLY}
procedure PBKDF2_256(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA256}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2S_256(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA256}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2_512(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA512}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2S_512(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA512}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2_Whirl(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMAC_Whirl}
  {$ifdef DLL} stdcall; {$endif}

procedure PBKDF2S_Whirl(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMAC-Whirl}
  {$ifdef DLL} stdcall; {$endif}
{$endif}



implementation


{---------------------------------------------------------------------------}
procedure PBKDF2(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA}
begin
  if kdf2(FindHash_by_ID(_SHA1),pPW,pLen,salt,sLen,C,DK,dkLen)<>0 then begin
    if HaltOnError then RunError(254);
  end;
end;


{---------------------------------------------------------------------------}
procedure PBKDF2S(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA}
begin
  PBKDF2(@sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;


{$ifndef SHA1ONLY}

{---------------------------------------------------------------------------}
procedure PBKDF2_256(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA256}
begin
  if kdf2(FindHash_by_ID(_SHA256),pPW,pLen,salt,sLen,C,DK,dkLen)<>0 then begin
    if HaltOnError then RunError(254);
  end;
end;


{---------------------------------------------------------------------------}
procedure PBKDF2S_256(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA256}
begin
  PBKDF2_256(@sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;

{---------------------------------------------------------------------------}
procedure PBKDF2_512(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMACSHA512}
begin
  if kdf2(FindHash_by_ID(_SHA512),pPW,pLen,salt,sLen,C,DK,dkLen)<>0 then begin
    if HaltOnError then RunError(254);
  end;
end;


{---------------------------------------------------------------------------}
procedure PBKDF2S_512(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMACSHA512}
begin
  PBKDF2_512(@sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;


{---------------------------------------------------------------------------}
procedure PBKDF2_Whirl(pPW: pointer; pLen: word; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password pPW using salt and iteration count C, uses HMAC_Whirl}
begin
  if kdf2(FindHash_by_ID(_Whirlpool),pPW,pLen,salt,sLen,C,DK,dkLen)<>0 then begin
    if HaltOnError then RunError(254);
  end;
end;


{---------------------------------------------------------------------------}
procedure PBKDF2S_Whirl(sPW: TKD_String; salt: pointer; sLen: word; C: longint; var DK; dkLen: word);
  {-Derive key DK from password string sPW using salt and iteration count C, uses HMAC-Whirl}
begin
  PBKDF2_Whirl(@sPW[1], length(sPW), salt, sLen, C, DK, dkLen);
end;

{$endif}

end.
