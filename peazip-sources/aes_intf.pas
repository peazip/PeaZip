unit AES_INTF;

(*************************************************************************

 DESCRIPTION     :  Interface unit for AES_DLL

 REQUIREMENTS    :  D2-D7/D9-D10/D12, FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.07.04  W.Ehrhardt  Initial version
 0.11     30.11.04  we          AES_XorBlock, AESBLKSIZE
 0.12     01.12.04  we          AES_Err_Data_After_Short_Block
 0.23     01.12.04  we          AES_ prefix for CTR increment routines
 0.24     24.12.04  we          AES_Get/SetFastInit
 0.25     09.07.06  we          CMAC, updated OMAC, checked: D9-D10
 0.26     14.06.07  we          Type TAES_EAXContext
 0.27     16.06.07  we          AES_CPRF128
 0.28     29.09.07  we          AES_XTS
 0.29     25.12.07  we          AES_CFB8
 0.30     20.07.08  we          All-in-one functions AES_EAX_Enc_Auth/AES_EAX_Dec_Veri
 0.31     02.08.08  we          Removed ctx parameter in AES_EAX_Enc_Auth/AES_EAX_Dec_Veri
 0.32     21.05.09  we          AES_CCM
 0.33     05.07.09  we          external 'aes_dll.dll'
 0.34     06.07.09  we          AES_DLL_Version returns PAnsiChar
 0.35     22.06.10  we          AES_CTR_Seek, AES_CTR_Seek64
 0.36     27.07.10  we          Longint ILen, AES_Err_Invalid_16Bit_Length
 0.37     28.07.10  we          Removed OMAC/CMAC XL versions
 0.38     31.07.10  we          AES_CTR_Seek via aes_seek.inc
 0.38     27.09.10  we          AES_GCM
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

const
  AES_Err_Invalid_Key_Size       = -1;  {Key size <> 128, 192, or 256 Bits}
  AES_Err_Invalid_Mode           = -2;  {Encr/Decr with Init for Decr/Encr}
  AES_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  AES_Err_Data_After_Short_Block = -4;  {Short block must be last}
  AES_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting    }
  AES_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}
  AES_Err_EAX_Inv_Text_Length    = -7;  {More than 64K text length in EAX all-in-one for 16 Bit}
  AES_Err_EAX_Inv_TAG_Length     = -8;  {EAX all-in-one tag length not 0..16}
  AES_Err_EAX_Verify_Tag         = -9;  {EAX all-in-one tag does not compare}
  AES_Err_CCM_Hdr_length         = -10; {CCM header length >= $FF00}
  AES_Err_CCM_Nonce_length       = -11; {CCM nonce length < 7 or > 13}
  AES_Err_CCM_Tag_length         = -12; {CCM tag length not in [4,6,8,19,12,14,16]}
  AES_Err_CCM_Verify_Tag         = -13; {Computed CCM tag does not compare}
  AES_Err_CCM_Text_length        = -14; {16 bit plain/cipher text length to large}
  AES_Err_CTR_SeekOffset         = -15; {Negative offset in AES_CTR_Seek}
  AES_Err_GCM_Verify_Tag         = -17; {GCM all-in-one tag does not compare}
  AES_Err_GCM_Auth_After_Final   = -18; {Auth after final or multiple finals}
  AES_Err_Invalid_16Bit_Length   = -20; {BaseAddr + length > $FFFF for 16 bit code}

const
  AESMaxRounds = 14;

type
  TAESBlock   = packed array[0..15] of byte;
  PAESBlock   = ^TAESBlock;
  TKeyArray   = packed array[0..AESMaxRounds] of TAESBlock;
  TIncProc    = procedure(var CTR: TAESBlock); {$ifdef USEDLL} stdcall; {$endif}
                 {user supplied IncCTR proc}
  TAESContext = packed record
                  RK      : TKeyArray;  {Key (encr. or decr.)   }
                  IV      : TAESBlock;  {IV or CTR              }
                  buf     : TAESBlock;  {Work buffer            }
                  bLen    : word;       {Bytes used in buf      }
                  Rounds  : word;       {Number of rounds       }
                  KeyBits : word;       {Number of bits in key  }
                  Decrypt : byte;       {<>0 if decrypting key  }
                  Flag    : byte;       {Bit 1: Short block     }
                  IncProc : TIncProc;   {Increment proc CTR-Mode}
                end;

const
  AESBLKSIZE = sizeof(TAESBlock);

type
  TAES_EAXContext = packed record
                      HdrOMAC : TAESContext; {Hdr OMAC1  context}
                      MsgOMAC : TAESContext; {Msg OMAC1  context}
                      ctr_ctx : TAESContext; {Msg AESCTR context}
                      NonceTag: TAESBlock;   {nonce tag         }
                      tagsize : word;        {tag size (unused) }
                      flags   : word;        {ctx flags (unused)}
                    end;


type
  TAES_XTSContext = packed record
                      main : TAESContext; {Main  context}
                      tweak: TAESContext; {Tweak context}
                    end;


type
  TGCM_Tab4K = array[0..255] of TAESBlock;   {64 KB gf_mul_h table  }

type
  TBit64 = packed array[0..1] of longint;    {64 bit counter        }

type
  TAES_GCMContext = packed record
                      actx    : TAESContext; {Basic AES context     }
                      aad_ghv : TAESBlock;   {ghash value AAD       }
                      txt_ghv : TAESBlock;   {ghash value ciphertext}
                      ghash_h : TAESBlock;   {ghash H value         }
                      gf_t4k  : TGCM_Tab4K;  {gf_mul_h table        }
                      aad_cnt : TBit64;      {processed AAD bytes   }
                      atx_cnt : TBit64;      {authent. text bytes   }
                      y0_val  : longint;     {initial 32-bit ctr val}
                    end;



function  AES_DLL_Version: PAnsiChar;
stdcall;  external 'aes_dll.dll' name 'AES_DLL_Version';
  {-Return DLL version as PAnsiChar}



procedure AES_XorBlock(const B1, B2: TAESBlock; var B3: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_XorBlock';
  {-xor two blocks, result in third}

function  AES_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_Init';
  {-AES key expansion, error if invalid key size}

procedure AES_SetFastInit(value: boolean);
stdcall;  external 'aes_dll.dll' name 'AES_SetFastInit';
  {-set FastInit variable}

function  AES_GetFastInit: boolean;
stdcall;  external 'aes_dll.dll' name 'AES_GetFastInit';
  {-Returns FastInit variable}



function  AES_Init_Encr(const  Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_Init_Encr';
  {-AES key expansion, error if invalid key size}

procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_Encrypt';
  {-encrypt one block, not checked: key must be encryption key}



function  AES_ECB_Init_Encr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_ECB_Init_Encr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_ECB_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_ECB_Init_Decr';
  {-AES key expansion, error if invalid key size, encrypt IV}



function  AES_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_ECB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  AES_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_ECB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  AES_Init_Decr(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_Init_Decr';
  {-AES key expansion, InvMixColumn(Key) for Decypt, error if invalid key size}

procedure AES_Decrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_Decrypt';
  {-decrypt one block (in ECB mode)}



function  AES_CBC_Init_Encr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CBC_Init_Encr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Init_Decr(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CBC_Init_Decr';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CBC_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  AES_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CBC_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  AES_CFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB_Init';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  AES_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}



function  AES_CFB8_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB8_Init';
  {-AES key expansion, error if invalid key size, store IV}

function  AES_CFB8_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB8_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB8 mode}

function  AES_CFB8_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CFB8_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB8 mode}



function  AES_OFB_Init(const Key; KeyBits: word; const IV: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_OFB_Init';
  {-AES key expansion, error if invalid key size, encrypt IV}

function  AES_OFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_OFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  AES_OFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_OFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  AES_CTR_Init(const Key; KeyBits: word; const CTR: TAESBlock; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CTR_Init';
  {-AES key expansion, error if inv. key size, encrypt CTR}

function  AES_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CTR_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  AES_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CTR_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  AES_CTR_Seek(const iCTR: TAESBlock; SOL, SOH: longint; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in AES_CTR_Init.}

{$ifdef HAS_INT64}
function AES_CTR_Seek64(const iCTR: TAESBlock; SO: int64; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in AES_CTR_Init.}
{$endif}

function  AES_SetIncProc(IncP: TIncProc; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_SetIncProc';
  {-Set user supplied IncCTR proc}

procedure AES_IncMSBFull(var CTR: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_IncMSBFull';
  {-Increment CTR[15]..CTR[0]}

procedure AES_IncLSBFull(var CTR: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_IncLSBFull';
  {-Increment CTR[0]..CTR[15]}

procedure AES_IncMSBPart(var CTR: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_IncMSBPart';
  {-Increment CTR[15]..CTR[8]}

procedure AES_IncLSBPart(var CTR: TAESBlock);
stdcall;  external 'aes_dll.dll' name 'AES_IncLSBPart';
  {-Increment CTR[0]..CTR[7]}



function  AES_OMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_OMAC_Init';
  {-OMAC init: AES key expansion, error if inv. key size}

function  AES_OMAC_Update(data: pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_OMAC_Update';
  {-OMAC data input, may be called more than once}

procedure AES_OMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'aes_dll.dll' name 'AES_OMAC_Final';
  {-end data input, calculate OMAC=OMAC1 tag}

procedure AES_OMAC1_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'aes_dll.dll' name 'AES_OMAC1_Final';
  {-end data input, calculate OMAC1 tag}

procedure AES_OMAC2_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'aes_dll.dll' name 'AES_OMAC2_Final';
  {-end data input, calculate OMAC2 tag}

procedure AES_OMACx_Final(OMAC2: boolean; var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'aes_dll.dll' name 'AES_OMACx_Final';
  {-end data input, calculate OMAC tag}



function  AES_CMAC_Init(const Key; KeyBits: word; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CMAC_Init';
  {-CMAC init: AES key expansion, error if inv. key size}

function  AES_CMAC_Update(data: pointer; ILen: longint; var ctx: TAESContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CMAC_Update';
  {-CMAC data input, may be called more than once}

procedure AES_CMAC_Final(var tag: TAESBlock; var ctx: TAESContext);
stdcall;  external 'aes_dll.dll' name 'AES_CMAC_Final';
  {-end data input, calculate CMAC=OMAC1 tag}



function  AES_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Init';
  {-Init hdr and msg OMACs, setp AESCTR with nonce tag}

function  AES_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TAES_EAXContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Provide_Header';
  {-Supply a message header. The header "grows" with each call}

function  AES_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function  AES_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_EAXContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Decrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure AES_EAX_Final(var tag: TAESBlock; var ctx: TAES_EAXContext);
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Final';
  {-Compute EAX tag from context}

function  AES_EAX_Enc_Auth(var tag: TAESBlock;               {Tag record}
                         const Key; KBits: word;             {key and bitlength of key}
                       const nonce; nLen: word;              {nonce: address / length}
                               Hdr: pointer; hLen: word;     {header: address / length}
                               ptp: pointer; pLen: longint;  {plaintext: address / length}
                               ctp: pointer                  {ciphertext: address}
                                 ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Enc_Auth';
  {-All-in-one call to encrypt/authenticate}

function  AES_EAX_Dec_Veri(   ptag: pointer; tLen : word;    {Tag: address / length (0..16)}
                         const Key; KBits: word;             {key and bitlength of key}
                       const nonce; nLen : word;             {nonce: address / length}
                               Hdr: pointer; hLen: word;     {header: address / length}
                               ctp: pointer; cLen: longint;  {ciphertext: address / length}
                               ptp: pointer                  {plaintext: address}
                                 ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_EAX_Dec_Veri';
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}



function AES_CPRF128(const Key; KeyBytes: word; msg: pointer; msglen: longint; var PRV: TAESBlock): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CPRF128';
  {Calculate variable-length key AES CMAC Pseudo-Random Function-128 for msg}
  {returns AES_OMAC error and 128-bit pseudo-random value PRV}

function AES_CPRF128_selftest: boolean;
stdcall;  external 'aes_dll.dll' name 'AES_CPRF128_selftest';
  {-Selftest with RFC 4615 test vectors}



function AES_XTS_Init_Encr(const K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_XTS_Init_Encr';
  {-Init XTS encrypt context (key expansion), error if invalid key size}

function AES_XTS_Encrypt(ptp, ctp: Pointer; ILen: longint; const twk: TAESBlock; var ctx: TAES_XTSContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_XTS_Encrypt';
  {-Encrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}

function AES_XTS_Init_Decr(const K1,K2; KBits: word; var ctx: TAES_XTSContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_XTS_Init_Decr';
  {-Init XTS decrypt context (key expansion), error if invalid key size}

function AES_XTS_Decrypt(ctp, ptp: Pointer; ILen: longint; const twk: TAESBlock; var ctx: TAES_XTSContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_XTS_Decrypt';
  {-Decrypt data unit of ILen bytes from ptp^ to ctp^ in XTS mode, twk: tweak of data unit}



function AES_CCM_Enc_AuthEx(var ctx: TAESContext;
                            var tag: TAESBlock; tLen : word;  {Tag & length in [4,6,8,19,12,14,16]}
                          const nonce;        nLen: word;     {nonce: address / length}
                                hdr: pointer; hLen: word;     {header: address / length}
                                ptp: pointer; pLen: longint;  {plaintext: address / length}
                                ctp: pointer                  {ciphertext: address}
                                  ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CCM_Enc_AuthEx';
  {-CCM packet encrypt/authenticate without key setup}

function AES_CCM_Enc_Auth(var tag: TAESBlock; tLen : word;  {Tag & length in [4,6,8,19,12,14,16]}
                      const   Key; KBytes: word;            {key and byte length of key}
                      const nonce; nLen: word;              {nonce: address / length}
                              hdr: pointer; hLen: word;     {header: address / length}
                              ptp: pointer; pLen: longint;  {plaintext: address / length}
                              ctp: pointer                  {ciphertext: address}
                                ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CCM_Enc_Auth';
  {-All-in-one call for CCM packet encrypt/authenticate}

function AES_CCM_Dec_VeriEX(var ctx: TAESContext;
                               ptag: pointer; tLen : word;    {Tag & length in [4,6,8,19,12,14,16]}
                        const nonce; nLen: word;              {nonce: address / length}
                                hdr: pointer; hLen: word;     {header: address / length}
                                ctp: pointer; cLen: longint;  {ciphertext: address / length}
                                ptp: pointer                  {plaintext: address}
                                  ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CCM_Dec_VeriEX';
  {-CCM packet decrypt/verify without key setup. If ptag^ verification fails, ptp^ is zero-filled!}

function AES_CCM_Dec_Veri(   ptag: pointer; tLen : word;    {Tag & length in [4,6,8,19,12,14,16]}
                      const   Key; KBytes: word;            {key and byte length of key}
                      const nonce; nLen: word;              {nonce: address / length}
                              hdr: pointer; hLen: word;     {header: address / length}
                              ctp: pointer; cLen: longint;  {ciphertext: address / length}
                              ptp: pointer                  {plaintext: address}
                                ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_CCM_Dec_Veri';
  {-All-in-one CCM packet decrypt/verify. If ptag^ verification fails, ptp^ is zero-filled!}



function AES_GCM_Init(const Key; KeyBits: word; var ctx: TAES_GCMContext): integer;
  {-Init context, calculate key-dependent GF(2^128) element H=E(K,0) and mul tables}
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Init';

function AES_GCM_Reset_IV(pIV: pointer; IV_len: word; var ctx: TAES_GCMContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Reset_IV';
  {-Reset: keep key but start new encryption with given IV}

function AES_GCM_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update auth data}

function AES_GCM_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TAES_GCMContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode, update auth data}

function AES_GCM_Add_AAD(pAAD: pointer; aLen: longint; var ctx: TAES_GCMContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Add_AAD';
  {-Add additional authenticated data (will not be encrypted)}

function AES_GCM_Final(var tag: TAESBlock; var ctx: TAES_GCMContext): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Final';
  {-Compute GCM tag from context}

function AES_GCM_Enc_Auth(var tag: TAESBlock;                     {Tag record}
                        const Key; KBits: word;                   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ptp: pointer; pLen: longint;        {plaintext: address / length}
                              ctp: pointer;                       {ciphertext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Enc_Auth';
  {-All-in-one call to encrypt/authenticate}

function AES_GCM_Dec_Veri(   ptag: pointer; tLen: word;           {Tag: address / length (0..16)}
                        const Key; KBits: word;                   {key and bitlength of key}
                              pIV: pointer; IV_len: word;         {IV: address / length}
                             pAAD: pointer; aLen: word;           {AAD: address / length}
                              ctp: pointer; cLen: longint;        {ciphertext: address / length}
                              ptp: pointer;                       {plaintext: address}
                          var ctx: TAES_GCMContext                {context, will be cleared}
                                ): integer;
stdcall;  external 'aes_dll.dll' name 'AES_GCM_Dec_Veri';
  {-All-in-one call to decrypt/verify. Decryption is done only if ptag^ is verified}


implementation

{$i aes_seek.inc}

end.
