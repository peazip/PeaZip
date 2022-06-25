library SP_DLL;

{$ifndef DLL}
  error('compile with $define DLL');
  end.
{$endif}


(*************************************************************************

 DESCRIPTION     :  DLL for Serpent

 REQUIREMENTS    :  D2-D7/D9-D10/D12, compile with $define DLL

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_DLL
 0.11     16.07.09  we          SP_DLL_Version returns PAnsiChar
 0.12     01.08.10  we          SP_CTR_Seek, longint ILen, SP_OMAC_UpdateXL removed
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

uses
  SP_base, SP_ctr, SP_cfb, SP_ofb, SP_cbc, SP_ecb, SP_omac, SP_eax;

{$R *.RES}


{---------------------------------------------------------------------------}
function SP_DLL_Version: PAnsiChar; stdcall;
  {-Return DLL version as PAnsiChar}
begin
  Result := '0.12';
end;


exports  SP_DLL_Version        name 'SP_DLL_Version';
exports  SP_XorBlock           name 'SP_XorBlock';
exports  SP_Init               name 'SP_Init';
exports  SP_Encrypt            name 'SP_Encrypt';
exports  SP_Decrypt            name 'SP_Decrypt';
exports  SP_Reset              name 'SP_Reset';
exports  SP_SetFastInit        name 'SP_SetFastInit';
exports  SP_GetFastInit        name 'SP_GetFastInit';

exports  SP_ECB_Init           name 'SP_ECB_Init';
exports  SP_ECB_Reset          name 'SP_ECB_Reset';
exports  SP_ECB_Encrypt        name 'SP_ECB_Encrypt';
exports  SP_ECB_Decrypt        name 'SP_ECB_Decrypt';

exports  SP_CBC_Init           name 'SP_CBC_Init';
exports  SP_CBC_Reset          name 'SP_CBC_Reset';
exports  SP_CBC_Encrypt        name 'SP_CBC_Encrypt';
exports  SP_CBC_Decrypt        name 'SP_CBC_Decrypt';

exports  SP_CFB_Init           name 'SP_CFB_Init';
exports  SP_CFB_Reset          name 'SP_CFB_Reset';
exports  SP_CFB_Encrypt        name 'SP_CFB_Encrypt';
exports  SP_CFB_Decrypt        name 'SP_CFB_Decrypt';

exports  SP_OFB_Init           name 'SP_OFB_Init';
exports  SP_OFB_Reset          name 'SP_OFB_Reset';
exports  SP_OFB_Encrypt        name 'SP_OFB_Encrypt';
exports  SP_OFB_Decrypt        name 'SP_OFB_Decrypt';

exports  SP_CTR_Init           name 'SP_CTR_Init';
exports  SP_CTR_Reset          name 'SP_CTR_Reset';
exports  SP_CTR_Encrypt        name 'SP_CTR_Encrypt';
exports  SP_CTR_Decrypt        name 'SP_CTR_Decrypt';
exports  SP_SetIncProc         name 'SP_SetIncProc';
exports  SP_IncMSBFull         name 'SP_IncMSBFull';
exports  SP_IncLSBFull         name 'SP_IncLSBFull';
exports  SP_IncMSBPart         name 'SP_IncMSBPart';
exports  SP_IncLSBPart         name 'SP_IncLSBPart';

exports  SP_OMAC_Init          name 'SP_OMAC_Init';
exports  SP_OMAC_Update        name 'SP_OMAC_Update';
exports  SP_OMAC_Final         name 'SP_OMAC_Final';
exports  SP_OMAC1_Final        name 'SP_OMAC1_Final';
exports  SP_OMAC2_Final        name 'SP_OMAC2_Final';

exports  SP_EAX_Init           name 'SP_EAX_Init';
exports  SP_EAX_Provide_Header name 'SP_EAX_Provide_Header';
exports  SP_EAX_Encrypt        name 'SP_EAX_Encrypt';
exports  SP_EAX_Decrypt        name 'SP_EAX_Decrypt';
exports  SP_EAX_Final          name 'SP_EAX_Final';

end.

