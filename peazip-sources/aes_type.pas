unit AES_Type;

(*************************************************************************

 DESCRIPTION     :  AES type definitions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     16.08.03  we          Separate unit from AESCrypt
 1.10     15.09.03  we          with IncProc
 1.20     21.09.03  we          with Flag, error codes
 1.21     05.10.03  we          with STD.INC
 1.23     05.10.03  we          with AES_Err_MultipleIncProcs
 1.24     12.06.04  we          with AES_Err_NIL_Pointer, const BLKSIZE
 1.25     02.07.04  we          {$ifdef DLL} stdcall; {$endif}
 1.26     29.11.04  we          FastInit
 1.27     30.11.04  we          AES_XorBlock, AESBLKSIZE
 1.28     01.12.04  we          AES_Err_Data_After_Short_Block
 1.29     09.07.06  we          Checked: D9-D10
 1.30     20.07.08  we          Error codes for EAX all-in-one function results
 1.31     21.05.09  we          CCM error codes
 1.32     20.06.10  we          CTR_Seek error code
 1.33     27.07.10  we          AES_Err_Invalid_16Bit_Length
 1.34     27.09.10  we          GCM error codes
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


interface


const
  AESMaxRounds = 14;

const
  AES_Err_Invalid_Key_Size       = -1;  {Key size <> 128, 192, or 256 Bits}
  AES_Err_Invalid_Mode           = -2;  {Encr/Decr with Init for Decr/Encr}
  AES_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  AES_Err_Data_After_Short_Block = -4;  {Short block must be last         }
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

type
  TAESBlock   = packed array[0..15] of byte;
  PAESBlock   = ^TAESBlock;
  TKeyArray   = packed array[0..AESMaxRounds] of TAESBlock;

  TIncProc    = procedure(var CTR: TAESBlock);   {user supplied IncCTR proc}
                {$ifdef DLL} stdcall; {$endif}

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
  AESBLKSIZE  = sizeof(TAESBlock);


implementation

end.
