unit TF_INTF;

(*************************************************************************

 DESCRIPTION     :  Interface unit for TF_DLL

 REQUIREMENTS    :  D2-D7/D9-D10/D12, FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     30.05.06  W.Ehrhardt  Initial version analog BF_INTF
 0.11     16.06.07  we          TF_Reset interfaced; TF_OMAC, TF_EAX
 0.12     16.07.09  we          TF_DLL_Version returns PAnsiChar, external 'TF_DLL.DLL'
 0.13     31.07.10  we          TF_CTR_Seek, longint ILen, TF_OMAC_UpdateXL removed
 0.14     01.08.10  we          TF_CTR_Seek via tf_seek.inc
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


const
  TF_Err_Invalid_Key_Size       = -1;  {Key size in bytes <1 or >56}
  TF_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  TF_Err_Data_After_Short_Block = -4;  {Short block must be last}
  TF_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting}
  TF_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}

  TF_Err_CTR_SeekOffset         = -15; {Negative offset in TF_CTR_Seek}
  TF_Err_Invalid_16Bit_Length   = -20; {Pointer + Offset > $FFFF for 16 bit code}

type
  TTFRndKey  = packed array[0..39]  of longint;
  TTFSBox    = packed array[0..255] of longint;
  TTFSBoxArr = packed array[0..3]   of TTFSbox;
  TTFBlock   = packed array[0..15]  of byte;
  PTFBlock   = ^TTFBlock;

type
  TTFIncProc = procedure(var CTR: TTFBlock);   {user supplied IncCTR proc}
                {$ifdef USEDLL} stdcall; {$endif}
type
  TTFContext = packed record
                 IV      : TTFBlock;   {IV or CTR              }
                 buf     : TTFBlock;   {Work buffer            }
                 bLen    : word;       {Bytes used in buf      }
                 Flag    : word;       {Bit 1: Short block     }
                 IncProc : TTFIncProc; {Increment proc CTR-Mode}
                 RK      : TTFRndKey;
                 S0,S1,
                 S2,S3   : TTFSBox;
               end;

const
  TFBLKSIZE  = sizeof(TTFBlock);

type
  TTF_EAXContext = packed record
                      HdrOMAC : TTFContext; {Hdr OMAC1  context}
                      MsgOMAC : TTFContext; {Msg OMAC1  context}
                      ctr_ctx : TTFContext; {Msg TFCTR context }
                      NonceTag: TTFBlock;   {nonce tag         }
                      tagsize : word;       {tag size (unused) }
                      flags   : word;       {ctx flags (unused)}
                    end;

function  TF_DLL_Version: PAnsiChar;
stdcall; external 'TF_DLL.DLL' name 'TF_DLL_Version';
  {-Return DLL version as PAnsiChar}


function  TF_Init(const Key; KeyBits: word; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_Init';
  {-Twofish round key and key-dependent sbox initialisation}

procedure TF_Encrypt(var ctx: TTFContext; const BI: TTFBlock; var BO: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_Encrypt';
  {-encrypt one block (in ECB mode)}

procedure TF_Decrypt(var ctx: TTFContext; const BI: TTFBlock; var BO: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_Decrypt';
  {-decrypt one block (in ECB mode)}

procedure TF_XorBlock(const B1, B2: TTFBlock; var B3: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_XorBlock';
  {-xor two blocks, result in third}

procedure TF_Reset(var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_Reset';
  {-Clears ctx fields bLen and Flag}

procedure TF_SetFastInit(value: boolean);
stdcall; external 'TF_DLL.DLL' name 'TF_SetFastInit';
  {-set FastInit variable}

function  TF_GetFastInit: boolean;
stdcall; external 'TF_DLL.DLL' name 'TF_GetFastInit';
  {-Returns FastInit variable}



function  TF_CBC_Init(const Key; KeyBits: word; const IV: TTFBlock; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CBC_Init';
  {-TF key expansion, error if invalid key size, save IV}

procedure TF_CBC_Reset(const IV: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_CBC_Reset';
  {-Clears ctx fields bLen and Flag, save IV}

function  TF_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CBC_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  TF_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CBC_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  TF_CFB_Init(const Key; KeyBits: word; const IV: TTFBlock; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CFB_Init';
  {-TF key expansion, error if invalid key size, encrypt IV}

procedure TF_CFB_Reset(const IV: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_CFB_Reset';
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  TF_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  TF_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}



function  TF_CTR_Init(const Key; KeyBits: word; const CTR: TTFBlock; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CTR_Init';
  {-TF key expansion, error if inv. key size, encrypt CTR}

procedure TF_CTR_Reset(const CTR: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_CTR_Reset';
  {-Clears ctx fields bLen and Flag, encrypt CTR}

function  TF_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CTR_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  TF_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_CTR_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  TF_CTR_Seek(const iCTR: TTFBlock; SOL, SOH: longint; var ctx: TTFContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in TF_CTR_Init.}

{$ifdef HAS_INT64}
function TF_CTR_Seek64(const iCTR: TTFBlock; SO: int64; var ctx: TTFContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in TF_CTR_Init.}
{$endif}

function  TF_SetIncProc(IncP: TTFIncProc; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_SetIncProc';
  {-Set user supplied IncCTR proc}

procedure TF_IncMSBFull(var CTR: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_IncMSBFull';
  {-Increment CTR[15]..CTR[0]}

procedure TF_IncLSBFull(var CTR: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_IncLSBFull';
  {-Increment CTR[0]..CTR[15]}

procedure TF_IncMSBPart(var CTR: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_IncMSBPart';
  {-Increment CTR[15]..CTR[8]}

procedure TF_IncLSBPart(var CTR: TTFBlock);
stdcall; external 'TF_DLL.DLL' name 'TF_IncLSBPart';
  {-Increment CTR[0]..CTR[7]}



function  TF_ECB_Init(const Key; KeyBits: word; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_ECB_Init';
  {-TF key expansion, error if invalid key size}

procedure TF_ECB_Reset(var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_ECB_Reset';
  {-Clears ctx fields bLen and Flag}

function  TF_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_ECB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  TF_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_ECB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  TF_OFB_Init(const Key; KeyBits: word; const IV: TTFBlock; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_OFB_Init';
  {-TF key expansion, error if invalid key size, encrypt IV}

procedure TF_OFB_Reset(const IV: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_OFB_Reset';
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  TF_OFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_OFB_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  TF_OFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_OFB_Decrypt';
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  TF_OMAC_Init(const Key; KeyBits: word; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_OMAC_Init';
  {-OMAC init: TF key expansion, error if inv. key size}

function  TF_OMAC_Update(data: pointer; ILen: longint; var ctx: TTFContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_OMAC_Update';
  {-OMAC data input, may be called more than once}

procedure TF_OMAC_Final(var tag: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_OMAC_Final';
  {-end data input, calculate OMAC=OMAC1 tag}

procedure TF_OMAC1_Final(var tag: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_OMAC1_Final';
  {-end data input, calculate OMAC1 tag}

procedure TF_OMAC2_Final(var tag: TTFBlock; var ctx: TTFContext);
stdcall; external 'TF_DLL.DLL' name 'TF_OMAC2_Final';
  {-end data input, calculate OMAC2 tag}


function TF_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TTF_EAXContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_EAX_Init';
  {-Init hdr and msg OMACs, setp TFCTR with nonce tag}

function TF_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TTF_EAXContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_EAX_Provide_Header';
  {-Supply a message header. The header "grows" with each call}

function TF_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_EAX_Encrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function TF_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TTF_EAXContext): integer;
stdcall; external 'TF_DLL.DLL' name 'TF_EAX_Decrypt';
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure TF_EAX_Final(var tag: TTFBlock; var ctx: TTF_EAXContext);
stdcall; external 'TF_DLL.DLL' name 'TF_EAX_Final';
  {-Compute EAX tag from context}

implementation

{$i tf_seek.inc}

end.
