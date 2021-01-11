library CH_DLL;

{DLL for CRC,Hash,HMAC,kdf}

{$ifndef DLL}
  error('compile with $define DLL');
  end.
{$endif}


(*************************************************************************

 DESCRIPTION     :  DLL for CRC,Hash,HMAC,kdf

 REQUIREMENTS    :  D2-D7/D9-D10/D12/D17-D18, compile with $define DLL

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     16.03.02  W.Ehrhardt  Initial version
 2.00     26.07.03  we          common vers., longint for word32
 2.10     29.08.03  we          XL versions for Win32
 2.20     13.09.03  we          with Adler32, CRC64
 2.50     13.09.03  we          SHA256/384/512
 3.00     01.12.03  we          Common version 3.0
 3.10     02.01.04  we          SHA224
 3.20     14.04.04  we          D7, removed $ifdef DLL
 3.21     07.07.04  we          HMACs, KeyDeriv
 3.22     04.01.05  we          HMACSHA256/512, PBKDF2_256/512
 3.23     22.12.05  we          Whirlpool functions
 3.24     08.01.06  we          SHA1Compress removed
 3.25     17.01.06  we          hash, hmac, pb_kdf
 3.26     31.01.06  we          RIPEMD-160
 3.27     11.02.06  we          ClearHashDesc
 3.28     03.04.06  we          CRC24
 3.29     20.02.07  we          MD4, ED2K
 3.30     30.06.07  we          FCRC32
 3.31     07.05.08  we          FinalBits/Ex
 3.32     07.05.08  we          HashSameDigest
 3.33     12.07.08  we          crcmodel procedures, removed keyderive
 3.34     12.07.08  we          kdf with kdf1/2/3, mgf1, pbkdf1(2
 3.35     12.11.08  we          BTypes, Ptr2Inc and/or Str255/Str127
 3.36     04.06.09  we          New version number (changed crcmodel unit)
 3.37     06.07.09  we          CH_DLL_Version returns PAnsiChar
 3.38     06.09.09  we          cm_combine
 3.39     20.08.10  we          hkdf, hkdfs
 3.40     17.12.10  we          CRC_Sick
 3.41     08.03.12  we          SHA512/224 and SHA512/256
 3.42     15.08.14  we          pbkdf2 functions with longint sLen, dkLen

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2014 Wolfgang Ehrhardt

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
  BTypes, CRC16, CRC24, CRC32, FCRC32, CRC64, Adler32, MD5, RMD160, MD4, ED2K, SHA1,
  SHA224, SHA256, SHA384, SHA512, SHA5_224, SHA5_256, Whirl512,
  HMAC_MD5, HMACSHA1, HMACSHA2, HMACSHA5, HMACWhir,
  Hash, HMAC, kdf, crcmodel, crc_sick;

{$R *.RES}


{---------------------------------------------------------------------------}
function CH_DLL_Version: PAnsiChar; stdcall;
  {-Return DLL version as PAnsiChar}
begin
  Result := '3.42';
end;


{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

exports CRC16Init            name 'CRC16Init';
exports CRC16Update          name 'CRC16Update';
exports CRC16Final           name 'CRC16Final';
exports CRC16SelfTest        name 'CRC16SelfTest';
exports CRC16Full            name 'CRC16Full';
exports CRC16File            name 'CRC16File';
exports CRC16UpdateXL        name 'CRC16UpdateXL';
exports CRC16FullXL          name 'CRC16FullXL';

exports CRC24Init            name 'CRC24Init';
exports CRC24Update          name 'CRC24Update';
exports CRC24Final           name 'CRC24Final';
exports CRC24SelfTest        name 'CRC24SelfTest';
exports CRC24Full            name 'CRC24Full';
exports CRC24File            name 'CRC24File';
exports CRC24UpdateXL        name 'CRC24UpdateXL';
exports CRC24FullXL          name 'CRC24FullXL';
exports Long2PGP             name 'Long2PGP';

exports CRC32Init            name 'CRC32Init';
exports CRC32Update          name 'CRC32Update';
exports CRC32Final           name 'CRC32Final';
exports CRC32SelfTest        name 'CRC32SelfTest';
exports CRC32Full            name 'CRC32Full';
exports CRC32File            name 'CRC32File';
exports CRC32UpdateXL        name 'CRC32UpdateXL';
exports CRC32FullXL          name 'CRC32FullXL';

exports FCRC32Init           name 'FCRC32Init';
exports FCRC32Update         name 'FCRC32Update';
exports FCRC32Final          name 'FCRC32Final';
exports FCRC32SelfTest       name 'FCRC32SelfTest';
exports FCRC32Full           name 'FCRC32Full';
exports FCRC32File           name 'FCRC32File';
exports FCRC32UpdateXL       name 'FCRC32UpdateXL';
exports FCRC32FullXL         name 'FCRC32FullXL';

exports Adler32Init          name 'Adler32Init';
exports Adler32Update        name 'Adler32Update';
exports Adler32Final         name 'Adler32Final';
exports Adler32SelfTest      name 'Adler32SelfTest';
exports Adler32Full          name 'Adler32Full';
exports Adler32File          name 'Adler32File';
exports Adler32UpdateXL      name 'Adler32UpdateXL';
exports Adler32FullXL        name 'Adler32FullXL';

exports CRC64Init            name 'CRC64Init';
exports CRC64Update          name 'CRC64Update';
exports CRC64Final           name 'CRC64Final';
exports CRC64SelfTest        name 'CRC64SelfTest';
exports CRC64Full            name 'CRC64Full';
exports CRC64File            name 'CRC64File';
exports CRC64UpdateXL        name 'CRC64UpdateXL';
exports CRC64FullXL          name 'CRC64FullXL';

exports ED2K_Init            name 'ED2K_Init';
exports ED2K_Update          name 'ED2K_Update';
exports ED2K_Final           name 'ED2K_Final';
exports ED2K_SelfTest        name 'ED2K_SelfTest';
exports ED2K_Full            name 'ED2K_Full';
exports ED2K_File            name 'ED2K_File';
exports ED2K_UpdateXL        name 'ED2K_UpdateXL';
exports ED2K_FullXL          name 'ED2K_FullXL';

exports MD4Init              name 'MD4Init';
exports MD4Update            name 'MD4Update';
exports MD4Final             name 'MD4Final';
exports MD4FinalEx           name 'MD4FinalEx';
exports MD4FinalBits         name 'MD4FinalBits';
exports MD4FinalBitsEx       name 'MD4FinalBitsEx';
exports MD4SelfTest          name 'MD4SelfTest';
exports MD4Full              name 'MD4Full';
exports MD4File              name 'MD4File';
exports MD4UpdateXL          name 'MD4UpdateXL';
exports MD4FullXL            name 'MD4FullXL';

exports MD5Init              name 'MD5Init';
exports MD5Update            name 'MD5Update';
exports MD5Final             name 'MD5Final';
exports MD5FinalEx           name 'MD5FinalEx';
exports MD5FinalBits         name 'MD5FinalBits';
exports MD5FinalBitsEx       name 'MD5FinalBitsEx';
exports MD5SelfTest          name 'MD5SelfTest';
exports MD5Full              name 'MD5Full';
exports MD5File              name 'MD5File';
exports MD5UpdateXL          name 'MD5UpdateXL';
exports MD5FullXL            name 'MD5FullXL';

exports RMD160Init           name 'RMD160Init';
exports RMD160Update         name 'RMD160Update';
exports RMD160Final          name 'RMD160Final';
exports RMD160FinalEx        name 'RMD160FinalEx';
exports RMD160FinalBits      name 'RMD160FinalBits';
exports RMD160FinalBitsEx    name 'RMD160FinalBitsEx';
exports RMD160SelfTest       name 'RMD160SelfTest';
exports RMD160Full           name 'RMD160Full';
exports RMD160File           name 'RMD160File';
exports RMD160UpdateXL       name 'RMD160UpdateXL';
exports RMD160FullXL         name 'RMD160FullXL';

exports SHA1Init             name 'SHA1Init';
exports SHA1Update           name 'SHA1Update';
exports SHA1Final            name 'SHA1Final';
exports SHA1FinalEx          name 'SHA1FinalEx';
exports SHA1FinalBits        name 'SHA1FinalBits';
exports SHA1FinalBitsEx      name 'SHA1FinalBitsEx';
exports SHA1SelfTest         name 'SHA1SelfTest';
exports SHA1Full             name 'SHA1Full';
exports SHA1File             name 'SHA1File';
exports SHA1UpdateXL         name 'SHA1UpdateXL';
exports SHA1FullXL           name 'SHA1FullXL';

exports SHA224Init           name 'SHA224Init';
exports SHA224Update         name 'SHA224Update';
exports SHA224Final          name 'SHA224Final';
exports SHA224FinalEx        name 'SHA224FinalEx';
exports SHA224FinalBits      name 'SHA224FinalBits';
exports SHA224FinalBitsEx    name 'SHA224FinalBitsEx';
exports SHA224SelfTest       name 'SHA224SelfTest';
exports SHA224Full           name 'SHA224Full';
exports SHA224File           name 'SHA224File';
exports SHA224UpdateXL       name 'SHA224UpdateXL';
exports SHA224FullXL         name 'SHA224FullXL';

exports SHA256Init           name 'SHA256Init';
exports SHA256Update         name 'SHA256Update';
exports SHA256Final          name 'SHA256Final';
exports SHA256FinalEx        name 'SHA256FinalEx';
exports SHA256FinalBits      name 'SHA256FinalBits';
exports SHA256FinalBitsEx    name 'SHA256FinalBitsEx';
exports SHA256SelfTest       name 'SHA256SelfTest';
exports SHA256Full           name 'SHA256Full';
exports SHA256File           name 'SHA256File';
exports SHA256UpdateXL       name 'SHA256UpdateXL';
exports SHA256FullXL         name 'SHA256FullXL';

exports SHA384Init           name 'SHA384Init';
exports SHA384Update         name 'SHA384Update';
exports SHA384Final          name 'SHA384Final';
exports SHA384FinalEx        name 'SHA384FinalEx';
exports SHA384FinalBits      name 'SHA384FinalBits';
exports SHA384FinalBitsEx    name 'SHA384FinalBitsEx';
exports SHA384SelfTest       name 'SHA384SelfTest';
exports SHA384Full           name 'SHA384Full';
exports SHA384File           name 'SHA384File';
exports SHA384UpdateXL       name 'SHA384UpdateXL';
exports SHA384FullXL         name 'SHA384FullXL';

exports SHA512Init           name 'SHA512Init';
exports SHA512Update         name 'SHA512Update';
exports SHA512Final          name 'SHA512Final';
exports SHA512FinalEx        name 'SHA512FinalEx';
exports SHA512FinalBits      name 'SHA512FinalBits';
exports SHA512FinalBitsEx    name 'SHA512FinalBitsEx';
exports SHA512SelfTest       name 'SHA512SelfTest';
exports SHA512Full           name 'SHA512Full';
exports SHA512File           name 'SHA512File';
exports SHA512UpdateXL       name 'SHA512UpdateXL';
exports SHA512FullXL         name 'SHA512FullXL';

exports SHA5_224Init         name 'SHA5_224Init';
exports SHA5_224Update       name 'SHA5_224Update';
exports SHA5_224Final        name 'SHA5_224Final';
exports SHA5_224FinalEx      name 'SHA5_224FinalEx';
exports SHA5_224FinalBits    name 'SHA5_224FinalBits';
exports SHA5_224FinalBitsEx  name 'SHA5_224FinalBitsEx';
exports SHA5_224SelfTest     name 'SHA5_224SelfTest';
exports SHA5_224Full         name 'SHA5_224Full';
exports SHA5_224File         name 'SHA5_224File';
exports SHA5_224UpdateXL     name 'SHA5_224UpdateXL';
exports SHA5_224FullXL       name 'SHA5_224FullXL';

exports SHA5_256Init         name 'SHA5_256Init';
exports SHA5_256Update       name 'SHA5_256Update';
exports SHA5_256Final        name 'SHA5_256Final';
exports SHA5_256FinalEx      name 'SHA5_256FinalEx';
exports SHA5_256FinalBits    name 'SHA5_256FinalBits';
exports SHA5_256FinalBitsEx  name 'SHA5_256FinalBitsEx';
exports SHA5_256SelfTest     name 'SHA5_256SelfTest';
exports SHA5_256Full         name 'SHA5_256Full';
exports SHA5_256File         name 'SHA5_256File';
exports SHA5_256UpdateXL     name 'SHA5_256UpdateXL';
exports SHA5_256FullXL       name 'SHA5_256FullXL';

exports Whirl_Init           name 'Whirl_Init';
exports Whirl_Update         name 'Whirl_Update';
exports Whirl_Final          name 'Whirl_Final';
exports Whirl_FinalEx        name 'Whirl_FinalEx';
exports Whirl_FinalBits      name 'Whirl_FinalBits';
exports Whirl_FinalBitsEx    name 'Whirl_FinalBitsEx';
exports Whirl_SelfTest       name 'Whirl_SelfTest';
exports Whirl_Full           name 'Whirl_Full';
exports Whirl_File           name 'Whirl_File';
exports Whirl_UpdateXL       name 'Whirl_UpdateXL';
exports Whirl_FullXL         name 'Whirl_FullXL';

exports hmac_MD5_init        name 'hmac_MD5_init';
exports hmac_MD5_inits       name 'hmac_MD5_inits';
exports hmac_MD5_update      name 'hmac_MD5_update';
exports hmac_MD5_final       name 'hmac_MD5_final';
exports hmac_MD5_updateXL    name 'hmac_MD5_updateXL';

exports hmac_sha1_init       name 'hmac_sha1_init';
exports hmac_sha1_inits      name 'hmac_sha1_inits';
exports hmac_sha1_update     name 'hmac_sha1_update';
exports hmac_sha1_final      name 'hmac_sha1_final';
exports hmac_sha1_updateXL   name 'hmac_sha1_updateXL';

exports hmac_SHA256_init     name 'hmac_SHA256_init';
exports hmac_SHA256_inits    name 'hmac_SHA256_inits';
exports hmac_SHA256_update   name 'hmac_SHA256_update';
exports hmac_SHA256_final    name 'hmac_SHA256_final';
exports hmac_SHA256_updateXL name 'hmac_SHA256_updateXL';

exports hmac_SHA512_init     name 'hmac_SHA512_init';
exports hmac_SHA512_inits    name 'hmac_SHA512_inits';
exports hmac_SHA512_update   name 'hmac_SHA512_update';
exports hmac_SHA512_final    name 'hmac_SHA512_final';
exports hmac_SHA512_updateXL name 'hmac_SHA512_updateXL';

exports hmac_Whirl_init      name 'hmac_Whirl_init';
exports hmac_Whirl_inits     name 'hmac_Whirl_inits';
exports hmac_Whirl_update    name 'hmac_Whirl_update';
exports hmac_Whirl_final     name 'hmac_Whirl_final';
exports hmac_Whirl_updateXL  name 'hmac_Whirl_updateXL';

exports RegisterHash         name 'RegisterHash';
exports FindHash_by_ID       name 'FindHash_by_ID';
exports FindHash_by_Name     name 'FindHash_by_Name';
exports HashFile             name 'HashFile';
exports HashUpdate           name 'HashUpdate';
exports HashFullXL           name 'HashFullXL';
exports HashFull             name 'HashFull';
exports HashSameDigest       name 'HashSameDigest';

exports hmac_init            name 'hmac_init';
exports hmac_inits           name 'hmac_inits';
exports hmac_update          name 'hmac_update';
exports hmac_updateXL        name 'hmac_updateXL';
exports hmac_final           name 'hmac_final';
exports hmac_final_bits      name 'hmac_final_bits';

exports kdf1                 name 'kdf1';
exports kdf2                 name 'kdf2';
exports kdf3                 name 'kdf3';
exports mgf1                 name 'mgf1';
exports pbkdf1               name 'pbkdf1';
exports pbkdf1s              name 'pbkdf1s';
exports pbkdf2               name 'pbkdf2';
exports pbkdf2s              name 'pbkdf2s';
exports hkdf                 name 'hkdf';
exports hkdfs                name 'hkdfs';

exports cm_CalcTab           name 'cm_CalcTab';
exports cm_Create            name 'cm_Create';
exports cm_Init              name 'cm_Init';
exports cm_Update            name 'cm_Update';
exports cm_Final             name 'cm_Final';
exports cm_SelfTest          name 'cm_SelfTest';
exports cm_Full              name 'cm_Full';
exports cm_File              name 'cm_File';
exports cm_next              name 'cm_next';
exports cm_reflect           name 'cm_reflect';
exports cm_combine           name 'cm_combine';

exports CRC_Sick_Init        name 'CRC_Sick_Init';
exports CRC_Sick_Update      name 'CRC_Sick_Update';
exports CRC_Sick_Final       name 'CRC_Sick_Final';
exports CRC_Sick_SelfTest    name 'CRC_Sick_SelfTest';
exports CRC_Sick_Full        name 'CRC_Sick_Full';
exports CRC_Sick_File        name 'CRC_Sick_File';
exports CRC_Sick_UpdateXL    name 'CRC_Sick_UpdateXL';
exports CRC_Sick_FullXL      name 'CRC_Sick_FullXL';

exports CH_DLL_Version       name 'CH_DLL_Version';

end.
