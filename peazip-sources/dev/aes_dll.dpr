library AES_DLL;

{$ifndef DLL}
  error('compile with $define DLL');
  end.
{$endif}


(*************************************************************************

 DESCRIPTION     :  DLL for AES

 REQUIREMENTS    :  D2-D7/D9-D10/D12, compile with $define DLL

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REMARK          :  AES_CTR_Seek/64 will be supplied by interface unit

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.07.04  W.Ehrhardt  Initial version
 0.11     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.12     01.12.04  we          AES_ prefix for CTR increment routines
 0.13     24.12.04  we          AES_Get/SetFastInit
 0.14     09.07.06  we          Checked: D9-D10
 0.15     09.07.06  we          Added CMAC, updated OMAC
 0.16     16.06.07  we          AES_CPRF128
 0.17     29.09.07  we          AES_XTS
 0.18     25.12.07  we          AES_CFB8
 0.19     20.07.08  we          All-in-one functions AES_EAX_Enc_Auth/AES_EAX_Dec_Veri
 0.20     21.05.09  we          AES_CCM
 0.21     06.07.09  we          AES_DLL_Version returns PAnsiChar
 0.22     22.06.10  we          AES_CTR_Seek
 0.23     27.07.10  we          Longint ILen in AES_xxx_En/Decrypt
 0.24     28.07.10  we          Removed OMAC/CMAC XL versions
 0.25     31.07.10  we          Removed AES_CTR_Seek (handled in interface unit)
 0.26     27.09.10  we          AES_GCM
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2004-2010 Wolfgang Ehrhardt

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
  aes_type, aes_base, aes_encr, aes_decr, aes_cfb8,
  aes_ctr,  aes_cfb,  aes_ofb,  aes_cbc,  aes_ecb,
  aes_omac, aes_cmac, aes_eax,  aes_cprf, aes_xts,
  aes_ccm,  aes_gcm;

{$R *.RES}


{---------------------------------------------------------------------------}
function AES_DLL_Version: PAnsiChar; stdcall;
  {-Return DLL version as PAnsiChar}
begin
  Result := '0.26';
end;


exports  AES_DLL_Version        name 'AES_DLL_Version';
exports  AES_XorBlock           name 'AES_XorBlock';
exports  AES_SetFastInit        name 'AES_SetFastInit';
exports  AES_GetFastInit        name 'AES_GetFastInit';
exports  AES_Init               name 'AES_Init';

exports  AES_Init_Encr          name 'AES_Init_Encr';
exports  AES_Encrypt            name 'AES_Encrypt';

exports  AES_ECB_Init_Encr      name 'AES_ECB_Init_Encr';
exports  AES_ECB_Init_Decr      name 'AES_ECB_Init_Decr';
exports  AES_ECB_Encrypt        name 'AES_ECB_Encrypt';
exports  AES_ECB_Decrypt        name 'AES_ECB_Decrypt';

exports  AES_Init_Decr          name 'AES_Init_Decr';
exports  AES_Decrypt            name 'AES_Decrypt';

exports  AES_CBC_Init_Encr      name 'AES_CBC_Init_Encr';
exports  AES_CBC_Init_Decr      name 'AES_CBC_Init_Decr';
exports  AES_CBC_Encrypt        name 'AES_CBC_Encrypt';
exports  AES_CBC_Decrypt        name 'AES_CBC_Decrypt';

exports  AES_CFB_Init           name 'AES_CFB_Init';
exports  AES_CFB_Encrypt        name 'AES_CFB_Encrypt';
exports  AES_CFB_Decrypt        name 'AES_CFB_Decrypt';

exports  AES_CFB8_Init          name 'AES_CFB8_Init';
exports  AES_CFB8_Encrypt       name 'AES_CFB8_Encrypt';
exports  AES_CFB8_Decrypt       name 'AES_CFB8_Decrypt';

exports  AES_OFB_Init           name 'AES_OFB_Init';
exports  AES_OFB_Encrypt        name 'AES_OFB_Encrypt';
exports  AES_OFB_Decrypt        name 'AES_OFB_Decrypt';

exports  AES_CTR_Init           name 'AES_CTR_Init';
exports  AES_CTR_Encrypt        name 'AES_CTR_Encrypt';
exports  AES_CTR_Decrypt        name 'AES_CTR_Decrypt';
exports  AES_SetIncProc         name 'AES_SetIncProc';
exports  AES_IncMSBFull         name 'AES_IncMSBFull';
exports  AES_IncLSBFull         name 'AES_IncLSBFull';
exports  AES_IncMSBPart         name 'AES_IncMSBPart';
exports  AES_IncLSBPart         name 'AES_IncLSBPart';

exports  AES_OMAC_Init          name 'AES_OMAC_Init';
exports  AES_OMAC_Update        name 'AES_OMAC_Update';
exports  AES_OMAC_Final         name 'AES_OMAC_Final';
exports  AES_OMAC1_Final        name 'AES_OMAC1_Final';
exports  AES_OMAC2_Final        name 'AES_OMAC2_Final';
exports  AES_OMACx_Final        name 'AES_OMACx_Final';

exports  AES_CMAC_Init          name 'AES_CMAC_Init';
exports  AES_CMAC_Update        name 'AES_CMAC_Update';
exports  AES_CMAC_Final         name 'AES_CMAC_Final';

exports  AES_EAX_Init           name 'AES_EAX_Init';
exports  AES_EAX_Provide_Header name 'AES_EAX_Provide_Header';
exports  AES_EAX_Encrypt        name 'AES_EAX_Encrypt';
exports  AES_EAX_Decrypt        name 'AES_EAX_Decrypt';
exports  AES_EAX_Final          name 'AES_EAX_Final';
exports  AES_EAX_Enc_Auth       name 'AES_EAX_Enc_Auth';
exports  AES_EAX_Dec_Veri       name 'AES_EAX_Dec_Veri';

exports  AES_CPRF128            name 'AES_CPRF128';
exports  AES_CPRF128_selftest   name 'AES_CPRF128_selftest';

exports  AES_XTS_Init_Encr      name 'AES_XTS_Init_Encr';
exports  AES_XTS_Encrypt        name 'AES_XTS_Encrypt';
exports  AES_XTS_Init_Decr      name 'AES_XTS_Init_Decr';
exports  AES_XTS_Decrypt        name 'AES_XTS_Decrypt';

exports  AES_CCM_Dec_Veri       name 'AES_CCM_Dec_Veri';
exports  AES_CCM_Dec_VeriEX     name 'AES_CCM_Dec_VeriEX';
exports  AES_CCM_Enc_Auth       name 'AES_CCM_Enc_Auth';
exports  AES_CCM_Enc_AuthEx     name 'AES_CCM_Enc_AuthEx';

exports  AES_GCM_Init           name 'AES_GCM_Init';
exports  AES_GCM_Reset_IV       name 'AES_GCM_Reset_IV';
exports  AES_GCM_Encrypt        name 'AES_GCM_Encrypt';
exports  AES_GCM_Decrypt        name 'AES_GCM_Decrypt';
exports  AES_GCM_Add_AAD        name 'AES_GCM_Add_AAD';
exports  AES_GCM_Final          name 'AES_GCM_Final';
exports  AES_GCM_Enc_Auth       name 'AES_GCM_Enc_Auth';
exports  AES_GCM_Dec_Veri       name 'AES_GCM_Dec_Veri';

end.

