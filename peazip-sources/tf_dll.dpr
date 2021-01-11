library TF_DLL;

{$ifndef DLL}
  error('compile with $define DLL');
  end.
{$endif}


(*************************************************************************

 DESCRIPTION     :  DLL for Twofish

 REQUIREMENTS    :  D2-D7/D9-D10/D12, compile with $define DLL

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     30.05.06  W.Ehrhardt  Initial version analog BF_DLL
 0.11     04.06.06  we          Incl version info, updated TF_DLL_Version
 0.12     16.06.07  we          TF_OMAC, TF_EAX; exports TF_Reset
 0.13     16.07.09  we          TF_DLL_Version returns PAnsiChar
 0.14     31.07.10  we          TF_CTR_Seek, longint ILen, TF_OMAC_UpdateXL removed
 0.15     01.08.10  we          Removed TF_CTR_Seek (handled in interface unit)
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

uses
  tf_base, tf_ctr, tf_cfb, tf_ofb, tf_cbc, tf_ecb, tf_omac, tf_eax;

{$R *.RES}


{---------------------------------------------------------------------------}
function TF_DLL_Version: PAnsiChar; stdcall;
  {-Return DLL version as PAnsiChar}
begin
  Result := '0.15';
end;


exports  TF_DLL_Version        name 'TF_DLL_Version';
exports  TF_XorBlock           name 'TF_XorBlock';
exports  TF_Init               name 'TF_Init';
exports  TF_Encrypt            name 'TF_Encrypt';
exports  TF_Decrypt            name 'TF_Decrypt';
exports  TF_Reset              name 'TF_Reset';
exports  TF_SetFastInit        name 'TF_SetFastInit';
exports  TF_GetFastInit        name 'TF_GetFastInit';

exports  TF_ECB_Init           name 'TF_ECB_Init';
exports  TF_ECB_Reset          name 'TF_ECB_Reset';
exports  TF_ECB_Encrypt        name 'TF_ECB_Encrypt';
exports  TF_ECB_Decrypt        name 'TF_ECB_Decrypt';

exports  TF_CBC_Init           name 'TF_CBC_Init';
exports  TF_CBC_Reset          name 'TF_CBC_Reset';
exports  TF_CBC_Encrypt        name 'TF_CBC_Encrypt';
exports  TF_CBC_Decrypt        name 'TF_CBC_Decrypt';

exports  TF_CFB_Init           name 'TF_CFB_Init';
exports  TF_CFB_Reset          name 'TF_CFB_Reset';
exports  TF_CFB_Encrypt        name 'TF_CFB_Encrypt';
exports  TF_CFB_Decrypt        name 'TF_CFB_Decrypt';

exports  TF_OFB_Init           name 'TF_OFB_Init';
exports  TF_OFB_Reset          name 'TF_OFB_Reset';
exports  TF_OFB_Encrypt        name 'TF_OFB_Encrypt';
exports  TF_OFB_Decrypt        name 'TF_OFB_Decrypt';

exports  TF_CTR_Init           name 'TF_CTR_Init';
exports  TF_CTR_Reset          name 'TF_CTR_Reset';
exports  TF_CTR_Encrypt        name 'TF_CTR_Encrypt';
exports  TF_CTR_Decrypt        name 'TF_CTR_Decrypt';
exports  TF_SetIncProc         name 'TF_SetIncProc';
exports  TF_IncMSBFull         name 'TF_IncMSBFull';
exports  TF_IncLSBFull         name 'TF_IncLSBFull';
exports  TF_IncMSBPart         name 'TF_IncMSBPart';
exports  TF_IncLSBPart         name 'TF_IncLSBPart';


exports  TF_OMAC_Init          name 'TF_OMAC_Init';
exports  TF_OMAC_Update        name 'TF_OMAC_Update';
exports  TF_OMAC_Final         name 'TF_OMAC_Final';
exports  TF_OMAC1_Final        name 'TF_OMAC1_Final';
exports  TF_OMAC2_Final        name 'TF_OMAC2_Final';

exports  TF_EAX_Init           name 'TF_EAX_Init';
exports  TF_EAX_Provide_Header name 'TF_EAX_Provide_Header';
exports  TF_EAX_Encrypt        name 'TF_EAX_Encrypt';
exports  TF_EAX_Decrypt        name 'TF_EAX_Decrypt';
exports  TF_EAX_Final          name 'TF_EAX_Final';


end.

