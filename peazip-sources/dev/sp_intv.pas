unit SP_INTV;


{$ifdef VirtualPascal}
  {$stdcall+}
{$else}
  Error('Interface unit for VirtualPascal');
{$endif}


(*************************************************************************

 DESCRIPTION     :  Interface unit for SP_DLL

 REQUIREMENTS    :  VirtualPascal

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.04.08  W.Ehrhardt  Initial version analog TF_INTF
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


interface


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
  {-Return DLL version as PAnsiChar}


function  SP_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent round key and key-dependent sbox initialisation}

procedure SP_Encrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
  {-encrypt one block (in ECB mode)}

procedure SP_Decrypt(var ctx: TSPContext; const BI: TSPBlock; var BO: TSPBlock);
  {-decrypt one block (in ECB mode)}

procedure SP_XorBlock(const B1, B2: TSPBlock; var B3: TSPBlock);
  {-xor two blocks, result in third}

procedure SP_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}

procedure SP_SetFastInit(value: boolean);
  {-set FastInit variable}

function  SP_GetFastInit: boolean;
  {-Returns FastInit variable}

function  SP_CBC_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size, save IV}

procedure SP_CBC_Reset(const IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, save IV}

function  SP_CBC_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CBC mode}

function  SP_CBC_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CBC mode}



function  SP_CFB_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size, encrypt IV}

procedure SP_CFB_Reset(const IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  SP_CFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CFB128 mode}

function  SP_CFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CFB128 mode}



function  SP_CTR_Init(const Key; KeyBits: word; const CTR: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if inv. key size, encrypt CTR}

procedure SP_CTR_Reset(const CTR: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt CTR}

function  SP_CTR_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode}

function  SP_CTR_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in CTR mode}

function  SP_CTR_Seek(const iCTR: TSPBlock; SOL, SOH: longint; var ctx: TSPContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in SP_CTR_Init.}

function  SP_SetIncProc(IncP: TSPIncProc; var ctx: TSPContext): integer;
  {-Set user supplied IncCTR proc}

procedure SP_IncMSBFull(var CTR: TSPBlock);
  {-Increment CTR[15]..CTR[0]}

procedure SP_IncLSBFull(var CTR: TSPBlock);
  {-Increment CTR[0]..CTR[15]}

procedure SP_IncMSBPart(var CTR: TSPBlock);
  {-Increment CTR[15]..CTR[8]}

procedure SP_IncLSBPart(var CTR: TSPBlock);
  {-Increment CTR[0]..CTR[7]}



function  SP_ECB_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size}

procedure SP_ECB_Reset(var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag}

function  SP_ECB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in ECB mode}

function  SP_ECB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in ECB mode}



function  SP_OFB_Init(const Key; KeyBits: word; const IV: TSPBlock; var ctx: TSPContext): integer;
  {-Serpent key expansion, error if invalid key size, encrypt IV}

procedure SP_OFB_Reset(const IV: TSPBlock; var ctx: TSPContext);
  {-Clears ctx fields bLen and Flag, encrypt IV}

function  SP_OFB_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in OFB mode}

function  SP_OFB_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSPContext): integer;
  {-Decrypt ILen bytes from ctp^ to ptp^ in OFB mode}



function  SP_OMAC_Init(const Key; KeyBits: word; var ctx: TSPContext): integer;
  {-OMAC init: Serpent key expansion, error if inv. key size}

function  SP_OMAC_Update(data: pointer; ILen: longint; var ctx: TSPContext): integer;
  {-OMAC data input, may be called more than once}

procedure SP_OMAC_Final(var tag: TSPBlock; var ctx: TSPContext);
  {-end data input, calculate OMAC=OMAC1 tag}

procedure SP_OMAC1_Final(var tag: TSPBlock; var ctx: TSPContext);
  {-end data input, calculate OMAC1 tag}

procedure SP_OMAC2_Final(var tag: TSPBlock; var ctx: TSPContext);
  {-end data input, calculate OMAC2 tag}



function SP_EAX_Init(const Key; KBits: word; const nonce; nLen: word; var ctx: TSP_EAXContext): integer;
  {-Init hdr and msg OMACs, setup SPCTR with nonce tag}

function SP_EAX_Provide_Header(Hdr: pointer; hLen: word; var ctx: TSP_EAXContext): integer;
  {-Supply a message header. The header "grows" with each call}

function SP_EAX_Encrypt(ptp, ctp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

function SP_EAX_Decrypt(ctp, ptp: Pointer; ILen: longint; var ctx: TSP_EAXContext): integer;
  {-Encrypt ILen bytes from ptp^ to ctp^ in CTR mode, update OMACs}

procedure SP_EAX_Final(var tag: TSPBlock; var ctx: TSP_EAXContext);
  {-Compute EAX tag from context}



implementation



function  SP_DLL_Version; external 'SP_DLL' name 'SP_DLL_Version';
function  SP_Init;        external 'SP_DLL' name 'SP_Init';
procedure SP_Encrypt;     external 'SP_DLL' name 'SP_Encrypt';
procedure SP_Decrypt;     external 'SP_DLL' name 'SP_Decrypt';
procedure SP_XorBlock;    external 'SP_DLL' name 'SP_XorBlock';
procedure SP_Reset;       external 'SP_DLL' name 'SP_Reset';
procedure SP_SetFastInit; external 'SP_DLL' name 'SP_SetFastInit';
function  SP_GetFastInit; external 'SP_DLL' name 'SP_GetFastInit';

function  SP_CBC_Init;    external 'SP_DLL' name 'SP_CBC_Init';
procedure SP_CBC_Reset;   external 'SP_DLL' name 'SP_CBC_Reset';
function  SP_CBC_Encrypt; external 'SP_DLL' name 'SP_CBC_Encrypt';
function  SP_CBC_Decrypt; external 'SP_DLL' name 'SP_CBC_Decrypt';

function  SP_CFB_Init;    external 'SP_DLL' name 'SP_CFB_Init';
procedure SP_CFB_Reset;   external 'SP_DLL' name 'SP_CFB_Reset';
function  SP_CFB_Encrypt; external 'SP_DLL' name 'SP_CFB_Encrypt';
function  SP_CFB_Decrypt; external 'SP_DLL' name 'SP_CFB_Decrypt';

function  SP_CTR_Init;    external 'SP_DLL' name 'SP_CTR_Init';
procedure SP_CTR_Reset;   external 'SP_DLL' name 'SP_CTR_Reset';
function  SP_CTR_Encrypt; external 'SP_DLL' name 'SP_CTR_Encrypt';
function  SP_CTR_Decrypt; external 'SP_DLL' name 'SP_CTR_Decrypt';
function  SP_SetIncProc;  external 'SP_DLL' name 'SP_SetIncProc';
procedure SP_IncMSBFull;  external 'SP_DLL' name 'SP_IncMSBFull';
procedure SP_IncLSBFull;  external 'SP_DLL' name 'SP_IncLSBFull';
procedure SP_IncMSBPart;  external 'SP_DLL' name 'SP_IncMSBPart';
procedure SP_IncLSBPart;  external 'SP_DLL' name 'SP_IncLSBPart';

function  SP_ECB_Init;    external 'SP_DLL' name 'SP_ECB_Init';
procedure SP_ECB_Reset;   external 'SP_DLL' name 'SP_ECB_Reset';
function  SP_ECB_Encrypt; external 'SP_DLL' name 'SP_ECB_Encrypt';
function  SP_ECB_Decrypt; external 'SP_DLL' name 'SP_ECB_Decrypt';

function  SP_OFB_Init;    external 'SP_DLL' name 'SP_OFB_Init';
procedure SP_OFB_Reset;   external 'SP_DLL' name 'SP_OFB_Reset';
function  SP_OFB_Encrypt; external 'SP_DLL' name 'SP_OFB_Encrypt';
function  SP_OFB_Decrypt; external 'SP_DLL' name 'SP_OFB_Decrypt';

function  SP_OMAC_Init;      external 'SP_DLL' name 'SP_OMAC_Init';
function  SP_OMAC_Update;    external 'SP_DLL' name 'SP_OMAC_Update';
procedure SP_OMAC_Final;     external 'SP_DLL' name 'SP_OMAC_Final';
procedure SP_OMAC1_Final;    external 'SP_DLL' name 'SP_OMAC1_Final';
procedure SP_OMAC2_Final;    external 'SP_DLL' name 'SP_OMAC2_Final';

function  SP_EAX_Init;            external 'SP_DLL' name 'SP_EAX_Init';
function  SP_EAX_Encrypt;         external 'SP_DLL' name 'SP_EAX_Encrypt';
function  SP_EAX_Decrypt;         external 'SP_DLL' name 'SP_EAX_Decrypt';
procedure SP_EAX_Final;           external 'SP_DLL' name 'SP_EAX_Final';
function  SP_EAX_Provide_Header;  external 'SP_DLL' name 'SP_EAX_Provide_Header';


{$define CONST}
{$i sp_seek.inc}

end.
