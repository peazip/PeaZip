unit SP_INTF;

(*************************************************************************

 DESCRIPTION     :  Interface unit for SP_DLL

 REQUIREMENTS    :  D2-D7/D9-D10/D12, FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_INTF
 0.11     16.07.09  we          SP_DLL_Version returns PAnsiChar, external 'SP_DLL.DLL'
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


interface

{$i std.inc}

const
  SP_Err_Invalid_Key_Size       = -1;  {Key size in bits not 128, 192, or 256}
  SP_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  SP_Err_Data_After_Short_Block = -4;  {Short block must be last}
  SP_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting}
  SP_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}

  SP_Err_CTR_SeekOffset         = -15; {Negative offset in SP_CTR_Seek}
  SP_Err_Invalid_16Bit_Length   = -20; {Pointer + Offset > $FFFF for 16 bit code}

type
  TSPRndKey  = packed array[0..131] of longint;
  TSPBlock   = packed array[0..15]  of byte;
  PSPBlock   = ^TSPBlock;

type
  TSPIncProc = procedure(var CTR: TSPBlock);   {user supplied IncCTR proc}
                {$ifdef USEDLL} stdcall; {$endif}
type
  TSPContext = packed record
                 IV      : TSPBlock;   {IV or CTR              }
                 buf     : TSPBlock;   {Work buffer            }
                 bLen    : word;       {Bytes used in buf      }
                 Flag    : word;       {Bit 1: Short block     }
                 IncProc : TSPIncProc; {Increment proc CTR-Mode}
                 RK      : TSPRndKey;  {Round keys             }
               end;

const
  SPBLKSIZE  = sizeof(TSPBlock);       {Serpent block size}

type
  TSP_EAXContext = packed record
                      HdrOMAC : TSPContext; {Hdr OMAC1  context}
                      MsgOMAC : TSPContext; {Msg OMAC1  context}
                      ctr_ctx : TSPContext; {Msg SPCTR context }
                      NonceTag: TSPBlock;   {nonce tag         }
                      tagsize : word;       {tag size (unused) }
                      flags   : word;       {ctx flags (unused)}
                    end;

function  SP_DLL_Version: PAnsiChar;
stdcall; external 'SP_DLL.DLL' name 'SP_DLL_Version';
  {-Return DLL version as PAnsiChar}


function  SP_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_Init';
  {-Serpent round key and key-dependent sbox initialisation}

procedure SP_Encrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_Encrypt';
  {-encrypt one block (in ECB mode)}

procedure SP_Decrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_Decrypt';
  {-decrypt one block (in ECB mode)}

procedure SP_XorBlock(const B1, B2: TSPBlock; var B3: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_XorBlock';
  {-xor two blocks, result in third}

procedure SP_Reset(var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_Reset';
  {-Clears ctx fields bLen and Flag}

procedure SP_SetFastInit(value: boolean);
stdcall; external 'SP_DLL.DLL' name 'SP_SetFastInit';
  {-set FastInit variable}

function  SP_GetFastInit: boolean;
stdcall; external 'SP_DLL.DLL' name 'SP_GetFastInit';
  {-Returns FastInit variable}



function  SP_CBC_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CBC_Init';
  {-Serpent key expansion, error if invalid key size, save IV}

procedure SP_CBC_Reset(const IV: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_CBC_Reset';
  {-Clears ctx fields bLen and Flag, save IV}

function  SP_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CBC_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  SP_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CBC_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  SP_CFB_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CFB_Init';
  {-Serpent key expansion, error if invalid key size, encrypt IV}

procedure SP_CFB_Reset(const IV: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_CFB_Reset';
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  SP_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  SP_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}



function  SP_CTR_Init(const Key; KeyBits: word; const CTR: TSPBlock; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CTR_Init';
  {-Serpent key expansion, error if inv. key size, encrypt CTR}

procedure SP_CTR_Reset(const CTR: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_CTR_Reset';
  {-Clears ctx fields bLen and Flag, encrypt CTR}

function  SP_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CTR_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  SP_CTR_Seek(const iCTR: TSPBlock; SOL, SOH: longint; var ctx: TSPContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in SP_CTR_Init.}

{$ifdef HAS_INT64}
function SP_CTR_Seek64(const iCTR: TSPBlock; SO: int64; var ctx: TSPContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in SP_CTR_Init.}
{$endif}

function  SP_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_CTR_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  SP_SetIncProc(IncP: TSPIncProc; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_SetIncProc';
  {-Set user supplied IncCTR proc}

procedure SP_IncMSBFull(var CTR: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_IncMSBFull';
  {-Increment CTR[15]..CTR[0]}

procedure SP_IncLSBFull(var CTR: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_IncLSBFull';
  {-Increment CTR[0]..CTR[15]}

procedure SP_IncMSBPart(var CTR: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_IncMSBPart';
  {-Increment CTR[15]..CTR[8]}

procedure SP_IncLSBPart(var CTR: TSPBlock);
stdcall; external 'SP_DLL.DLL' name 'SP_IncLSBPart';
  {-Increment CTR[0]..CTR[7]}



function  SP_ECB_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_ECB_Init';
  {-Serpent key expansion, error if invalid key size}

procedure SP_ECB_Reset(var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_ECB_Reset';
  {-Clears ctx fields bLen and Flag}

function  SP_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_ECB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  SP_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_ECB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  SP_OFB_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_OFB_Init';
  {-Serpent key expansion, error if invalid key size, encrypt IV}

procedure SP_OFB_Reset(const IV: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_OFB_Reset';
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  SP_OFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_OFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  SP_OFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_OFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  SP_OMAC_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_OMAC_Init';
  {-OMAC init: Serpent key expansion, error if inv. key size}

function  SP_OMAC_Update(data: pointer; ILen: longint; var ctx: TSPContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_OMAC_Update';
  {-OMAC data input, may be called more than once}

procedure SP_OMAC_Final(var tag: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_OMAC_Final';
  {-end data input, calculate OMAC=OMAC1 tag}

procedure SP_OMAC1_Final(var tag: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_OMAC1_Final';
  {-end data input, calculate OMAC1 tag}

procedure SP_OMAC2_Final(var tag: TSPBlock; var ctx: TSPContext);
stdcall; external 'SP_DLL.DLL' name 'SP_OMAC2_Final';
  {-end data input, calculate OMAC2 tag}


function SP_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TSP_EAXContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_EAX_Init';
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}

function SP_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TSP_EAXContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_EAX_Provide_Header';
  {-Supply a message header. The header "grows" with each call}

function SP_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_EAX_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function SP_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
stdcall; external 'SP_DLL.DLL' name 'SP_EAX_Decrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure SP_EAX_Final(var tag: TSPBlock; var ctx: TSP_EAXContext);
stdcall; external 'SP_DLL.DLL' name 'SP_EAX_Final';
  {-Compute EAX tag from context}

implementation

{$i sp_seek.inc}

end.
