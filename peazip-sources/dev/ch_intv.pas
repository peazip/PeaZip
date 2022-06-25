unit CH_IntV;

{VP interface unit for CH_DLL}

interface

{Warning: This unit must be compiled with the same HugeString}
{=======  option $H+/- that is used for compiling the CH_DLL!}

{$ifdef VirtualPascal}
  {&stdcall+}
{$else}
  Error('Interface unit for VirtualPascal');
{$endif}


(*************************************************************************

 DESCRIPTION     :  Interface unit for CH_DLL

 REQUIREMENTS    :  VirtualPascal

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.07.04  W.Ehrhardt  Initial version
 0.11     07.07.04  we          keyderiv routines
 0.12     04.01.05  we          HMACSHA256/512, PBKDF2_256/512
 0.13     22.12.05  we          Whirlpool V3.0 functions
 0.14     08.01.06  we          SHA1Compress removed
 0.15     17.01.06  we          hash, hmac, pb_kdf
 0.16     18.01.06  we          Descriptor fields HAlgNum, HSig
 0.17     31.01.06  we          RIPEMD-160
 0.19     11.02.06  we          Fields: HDSize, HVersion, HPtrOID, HLenOID
 0.20     03.05.06  we          CRC24
 0.21     07.08.06  we          const fname: shortstring
 0.22     21.02.07  we          MD4, ED2K
 0.23     22.02.07  we          POID_Vec=^TOID_Vec, typed HPtrOID
 0.24     30.06.07  we          FCRC32
 0.25     04.10.07  we          THashContext.Index now longint
 0.26     07.05.08  we          [Hash]FinalBits/Ex, HashSameDigest
 0.27     12.07.08  we          crcmodel, removed keyderive routines
 0.28     12.07.08  we          kdf with kdf1/2/3, mgf1, pbkdf1/2
 0.29     16.11.08  we          uses BTypes and Str255
 0.30     06.07.09  we          CH_DLL_Version returns PAnsiChar
 0.31     06.09.09  we          cm_combine
 0.32     20.08.10  we          hkdf, hkdfs
 0.33     17.12.10  we          CRC_Sick
 0.34     08.03.12  we          SHA512/224 and SHA512/256
 0.35     15.08.14  we          pbkdf2 functions with longint sLen, dkLen
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
  BTypes;


function CH_DLL_Version: PAnsiChar; stdcall;
  {-return DLL version as PAnsiChar}


{---------------------------------------------------------------------------}
procedure CRC16Init(var CRC: word);
  {-initialize context}

procedure CRC16Update(var CRC: word; Msg: pointer; Len: word);
  {-update CRC16 with Msg data}

procedure CRC16Final(var CRC: word);
  {-CRC16: finalize calculation (dummy)}

function  CRC16SelfTest: boolean;
  {-Self test for CRC16}

procedure CRC16Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC16 of Msg with init/update/final}

procedure CRC16File(const fname: Str255; var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC16 of file, buf: buffer with at least bsize bytes}

procedure CRC16UpdateXL(var CRC: word; Msg: pointer; Len: longint);
  {-update CRC16 with Msg data}

procedure CRC16FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC16 of Msg with init/update/final}


{---------------------------------------------------------------------------}
type
  TPGPDigest = array[0..2] of byte;  {OpenPGP 3 byte MSB first CRC24 digest}


procedure Long2PGP(CRC: longint; var PGPCRC: TPGPDigest);
  {-convert longint CRC24 to OpenPGP MSB first format}

procedure CRC24Init(var CRC: longint);
  {-initialize context}

procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC24 with Msg data}

procedure CRC24Final(var CRC: longint);
  {-CRC24: finalize calculation}

function  CRC24SelfTest: boolean;
  {-Self test for CRC24}

procedure CRC24Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC24 of Msg with init/update/final}

procedure CRC24File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC24 of file, buf: buffer with at least bsize bytes}

procedure CRC24UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC24 with Msg data}

procedure CRC24FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC24 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure CRC32Init(var CRC: longint);
  {-initialize context}

procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC32 with Msg data}

procedure CRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}

function  CRC32SelfTest: boolean;
  {-Self test for CRC32}

procedure CRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}

procedure CRC32File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}

procedure CRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC32 with Msg data}

procedure CRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure FCRC32Init(var CRC: longint);
  {-initialize context}

procedure FCRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC with Msg data}

procedure FCRC32Final(var CRC: longint);
  {-CRC32 finalize calculation}

function  FCRC32SelfTest: boolean;
  {-Self test for FCRC32}

procedure FCRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}

procedure FCRC32File({$ifdef CONST} const {$endif} fname: Str255;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}

procedure FCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC with Msg data}

procedure FCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC of Msg with init/update/final}
  {$ifdef DLL} stdcall; {$endif}


{---------------------------------------------------------------------------}
procedure Adler32Init(var CRC: longint);
  {-initialize context}

procedure Adler32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update Adler32 with Msg data}

procedure Adler32Final(var CRC: longint);
  {-Adler32: finalize calculation}

function  Adler32SelfTest: boolean;
  {-Self test for Adler32}

procedure Adler32Full(var CRC: longint; Msg: pointer; Len: word);
  {-Adler32 of Msg with init/update/final}

procedure Adler32File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
  {-Adler32 of file, buf: buffer with at least bsize bytes}

procedure Adler32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update Adler32 with Msg data}

procedure Adler32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-Adler32 of Msg with init/update/final}


{---------------------------------------------------------------------------}
type
  TCRC64 = packed record
             lo32, hi32: longint;
           end;

procedure CRC64Init(var CRC: TCRC64);
  {-CRC64 initialization}

procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}

procedure CRC64Final(var CRC: TCRC64);
  {-CRC64: finalize calculation, CRC wird geloescht}

function  CRC64SelfTest: boolean;
  {-Self test for CRC64}

procedure CRC64Full(var CRC: TCRC64; Msg: pointer; Len: word);
  {-CRC64 of Msg with init/update/final}

procedure CRC64File(const fname: Str255; var CRC: TCRC64; var buf; bsize: word; var Err: word);
  {-CRC64 of file, buf: buffer with at least bsize bytes}

procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
  {-update CRC64 with Msg data}

procedure CRC64FullXL(var CRC: TCRC64; Msg: pointer; Len: longint);
  {-CRC64 of Msg with init/update/final}


{---------------------------------------------------------------------------}
type
  THashAlgorithm = (_MD4,_MD5,_RIPEMD160,_SHA1,_SHA224,_SHA256,
                    _SHA384,_SHA512,_Whirlpool,_SHA512_224,_SHA512_256); {Supported hash algorithms}

const
  _RMD160  = _RIPEMD160;     {Alias}

const
  MaxBlockLen  = 128;         {Max. block length (buffer size), multiple of 4}
  MaxDigestLen = 64;          {Max. length of hash digest}
  MaxStateLen  = 16;          {Max. size of internal state}
  MaxOIDLen    = 9;           {Current max. OID length}
  C_HashSig    = $3D7A;       {Signature for Hash descriptor}
  C_HashVers   = $00010005;   {Version of Hash definitions}
  C_MinHash    = _MD4;        {Lowest  hash in THashAlgorithm}
  C_MaxHash    = _SHA512_256; {Highest hash in THashAlgorithm}

type
  THashState   = packed array[0..MaxStateLen-1] of longint;         {Internal state}
  THashBuffer  = packed array[0..MaxBlockLen-1] of byte;            {hash buffer block}
  THashDigest  = packed array[0..MaxDigestLen-1] of byte;           {hash digest}
  PHashDigest  = ^THashDigest;                                      {pointer to hash digest}
  THashBuf32   = packed array[0..MaxBlockLen  div 4 -1] of longint; {type cast helper}
  THashDig32   = packed array[0..MaxDigestLen div 4 -1] of longint; {type cast helper}


type
  THashContext = packed record
                   Hash  : THashState;             {Working hash}
                   MLen  : packed array[0..3] of longint; {max 128 bit msg length}
                   Buffer: THashBuffer;            {Block buffer}
                   Index : longint;                {Index in buffer}
                 end;

type
  TMD4Digest       = packed array[0..15] of byte;  {MD4    digest   }
  TMD5Digest       = packed array[0..15] of byte;  {MD5    digest   }
  TRMD160Digest    = packed array[0..19] of byte;  {RMD160 digest   }
  TSHA1Digest      = packed array[0..19] of byte;  {SHA1   digest   }
  TSHA224Digest    = packed array[0..27] of byte;  {SHA224 digest   }
  TSHA256Digest    = packed array[0..31] of byte;  {SHA256 digest   }
  TSHA384Digest    = packed array[0..47] of byte;  {SHA384 digest   }
  TSHA512Digest    = packed array[0..63] of byte;  {SHA512 digest   }
  TSHA5_224Digest  = packed array[0..27] of byte;  {SHA512/224 digest}
  TSHA5_256Digest  = packed array[0..31] of byte;  {SHA512/256 digest}
  TWhirlDigest     = packed array[0..63] of byte;  {Whirlpool digest}

type
  HashInitProc     = procedure(var Context: THashContext);
                      {-initialize context}
                      {$ifdef DLL} stdcall; {$endif}

  HashUpdateXLProc = procedure(var Context: THashContext; Msg: pointer; Len: longint);
                      {-update context with Msg data}
                      {$ifdef DLL} stdcall; {$endif}

  HashFinalProc    = procedure(var Context: THashContext; var Digest: THashDigest);
                      {-finalize calculation, clear context}
                      {$ifdef DLL} stdcall; {$endif}

  HashFinalBitProc = procedure(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
                      {-finalize calculation with bitlen bits from BData, clear context}
                      {$ifdef DLL} stdcall; {$endif}

type
  TOID_Vec  = packed array[1..MaxOIDLen] of longint; {OID vector}
  POID_Vec  = ^TOID_Vec;                             {ptr to OID vector}

type
  THashName = string[19];                      {Hash algo name type }
  PHashDesc = ^THashDesc;                      {Ptr to descriptor   }
  THashDesc = packed record
                HSig      : word;              {Signature=C_HashSig }
                HDSize    : word;              {sizeof(THashDesc)   }
                HDVersion : longint;           {THashDesc Version   }
                HBlockLen : word;              {Blocklength of hash }
                HDigestlen: word;              {Digestlength of hash}
                HInit     : HashInitProc;      {Init  procedure     }
                HFinal    : HashFinalProc;     {Final procedure     }
                HUpdateXL : HashUpdateXLProc;  {Update procedure    }
                HAlgNum   : longint;           {Algo ID, longint avoids problems with enum size/DLL}
                HName     : THashName;         {Name of hash algo   }
                HPtrOID   : POID_Vec;          {Pointer to OID vec  }
                HLenOID   : word;              {Length of OID vec   }
                HFill     : word;
                HFinalBit : HashFinalBitProc;  {Bit-API Final proc  }
                HReserved : packed array[0..19] of byte;
              end;


procedure RegisterHash(AlgId: THashAlgorithm; PHash: PHashDesc);
  {-Register algorithm with AlgID and Hash descriptor PHash^}

function  FindHash_by_ID(AlgoID: THashAlgorithm): PHashDesc;
  {-Return PHashDesc of AlgoID, nil if not found/registered}

function  FindHash_by_Name(AlgoName: THashName): PHashDesc;
  {-Return PHashDesc of Algo with AlgoName, nil if not found/registered}


procedure HashFile(const fname: Str255; PHash: PHashDesc; var Digest: THashDigest; var buf; bsize: word; var Err: word);
  {-Calulate hash digest of file, buf: buffer with at least bsize bytes}

procedure HashUpdate(PHash: PHashDesc; var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure HashFullXL(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: longint);
  {-Calulate hash digest of Msg with init/update/final}

procedure HashFull(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: word);
  {-Calulate hash digest of Msg with init/update/final}

function  HashSameDigest(PHash: PHashDesc; PD1, PD2: PHashDigest): boolean;
  {-Return true if same digests, using HDigestlen of PHash}


{---------------------------------------------------------------------------}
type
  TED2KResult  = packed record
                   eDonkey: TMD4Digest;  {eDonkey method hash}
                     eMule: TMD4Digest;  {eMule   method hash}
                    differ: boolean;     {eDonkey and eMule differ}
                 end;

type
  TED2KContext = packed record
                   total_ctx: THashContext;  {outer total context}
                   chunk_ctx: THashContext;  {inner chunk context}
                   lastdig  : TMD4Digest;    {hash of completed inner chunk}
                   cbytecnt : longint;       {byte count within chunk}
                   chunkcnt : longint;       {chunk count}
                 end;


procedure ED2K_Init(var Context: TED2KContext);
  {-initialize context}

procedure ED2K_Update(var Context: TED2KContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure ED2K_UpdateXL(var Context: TED2KContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure ED2K_Final(var Context: TED2KContext; var ResRec: TED2KResult);
  {-finalize eDonkey/eMule hash calculation, clear context}

function  ED2K_SelfTest: boolean;
  {-eDonkey hash self test for two small strings}

procedure ED2K_Full(var ResRec: TED2KResult; Msg: pointer; Len: word);
  {-eDonkey/eMule hash of Msg with init/update/final}

procedure ED2K_FullXL(var ResRec: TED2KResult; Msg: pointer; Len: longint);
  {-eDonkey/eMule hash of Msg with init/update/final}

procedure ED2K_File(const fname: Str255; var ResRec: TED2KResult; var buf; bsize: word; var Err: word);
  {-eDonkey/eMule hash of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure MD4Init(var Context: THashContext);
  {-initialize context}

procedure MD4Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure MD4UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure MD4Final(var Context: THashContext; var Digest: TMD4Digest);
  {-finalize MD4 calculation, clear context}

procedure MD4FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD4 calculation, clear context}

procedure MD4FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD4FinalBits(var Context: THashContext; var Digest: TMD4Digest; BData: byte; bitlen: integer);
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

function  MD4SelfTest: boolean;
  {-self test for string from MD4 document}

procedure MD4Full(var Digest: TMD4Digest; Msg: pointer; Len: word);
  {-MD4 of Msg with init/update/final}

procedure MD4FullXL(var Digest: TMD4Digest; Msg: pointer; Len: longint);
  {-MD4 of Msg with init/update/final}

procedure MD4File(const fname: Str255; var Digest: TMD4Digest; var buf; bsize: word; var Err: word);
  {-MD4 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure MD5Init(var Context: THashContext);
  {-initialize context}

procedure MD5Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure MD5UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure MD5Final(var Context: THashContext; var Digest: TMD5Digest);
  {-finalize MD5 calculation, clear context}

procedure MD5FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize MD5 calculation, clear context}

procedure MD5FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD5FinalBits(var Context: THashContext; var Digest: TMD5Digest; BData: byte; bitlen: integer);
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

function  MD5SelfTest: boolean;
  {-self test for string from MD5 document}

procedure MD5Full(var Digest: TMD5Digest; Msg: pointer; Len: word);
  {-MD5 of Msg with init/update/final}

procedure MD5FullXL(var Digest: TMD5Digest; Msg: pointer; Len: longint);
  {-MD5 of Msg with init/update/final}

procedure MD5File(const fname: Str255; var Digest: TMD5Digest; var buf; bsize: word; var Err: word);
  {-MD5 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure RMD160Init(var Context: THashContext);
  {-initialize context}

procedure RMD160Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure RMD160UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure RMD160Final(var Context: THashContext; var Digest: TRMD160Digest);
  {-finalize RMD160 calculation, clear context}

procedure RMD160FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize RMD160 calculation, clear context}

procedure RMD160FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

procedure RMD160FinalBits(var Context: THashContext; var Digest: TRMD160Digest; BData: byte; bitlen: integer);
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

function  RMD160SelfTest: boolean;
  {-self test for string from RMD160 document}

procedure RMD160Full(var Digest: TRMD160Digest; Msg: pointer; Len: word);
  {-RMD160 of Msg with init/update/final}

procedure RMD160FullXL(var Digest: TRMD160Digest; Msg: pointer; Len: longint);
  {-RMD160 of Msg with init/update/final}

procedure RMD160File(const fname: Str255; var Digest: TRMD160Digest; var buf; bsize: word; var Err: word);
  {-RMD160 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA1Init(var Context: THashContext);
  {-initialize context}

procedure SHA1Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA1UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA1Final(var Context: THashContext; var Digest: TSHA1Digest);
  {-finalize SHA1 calculation, clear context}

procedure SHA1FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA1 calculation, clear context}

procedure SHA1FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA1FinalBits(var Context: THashContext; var Digest: TSHA1Digest; BData: byte; bitlen: integer);
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA1SelfTest: boolean;
  {-self test SHA1: compare with known value}

procedure SHA1Full(var Digest: TSHA1Digest; Msg: pointer; Len: word);
  {-SHA1 of Msg with init/update/final}

procedure SHA1FullXL(var Digest: TSHA1Digest; Msg: pointer; Len: longint);
  {-SHA1 of Msg with init/update/final}

procedure SHA1File(const fname: Str255; var Digest: TSHA1Digest; var buf; bsize: word; var Err: word);
  {-SHA1 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA224Init(var Context: THashContext);
  {-initialize context}

procedure SHA224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA224Final(var Context: THashContext; var Digest: TSHA224Digest);
  {-finalize SHA224 calculation, clear context}

procedure SHA224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA224 calculation, clear context}

procedure SHA224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA224FinalBits(var Context: THashContext; var Digest: TSHA224Digest; BData: byte; bitlen: integer);
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA224SelfTest: boolean;
  {-self test for string from SHA224 document}

procedure SHA224Full(var Digest: TSHA224Digest; Msg: pointer; Len: word);
  {-SHA224 of Msg with init/update/final}

procedure SHA224FullXL(var Digest: TSHA224Digest; Msg: pointer; Len: longint);
  {-SHA224 of Msg with init/update/final}

procedure SHA224File(const fname: Str255; var Digest: TSHA224Digest; var buf; bsize: word; var Err: word);
  {-SHA224 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA256Init(var Context: THashContext);
  {-initialize context}

procedure SHA256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA256Final(var Context: THashContext; var Digest: TSHA256Digest);
  {-finalize SHA256 calculation, clear context}

procedure SHA256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA256 calculation, clear context}

procedure SHA256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA256FinalBits(var Context: THashContext; var Digest: TSHA256Digest; BData: byte; bitlen: integer);
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA256SelfTest: boolean;
  {-self test for string from SHA256 document}

procedure SHA256Full(var Digest: TSHA256Digest; Msg: pointer; Len: word);
  {-SHA256 of Msg with init/update/final}

procedure SHA256FullXL(var Digest: TSHA256Digest; Msg: pointer; Len: longint);
  {-SHA256 of Msg with init/update/final}

procedure SHA256File(const fname: Str255; var Digest: TSHA256Digest; var buf; bsize: word; var Err: word);
  {-SHA256 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA384Init(var Context: THashContext);
  {-initialize context}

procedure SHA384Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA384Final(var Context: THashContext; var Digest: TSHA384Digest);
  {-finalize SHA384 calculation, clear context}

procedure SHA384FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA384 calculation, clear context}

procedure SHA384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA384FinalBits(var Context: THashContext; var Digest: TSHA384Digest; BData: byte; bitlen: integer);
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA384SelfTest: boolean;
  {-self test for string from SHA384 document}

procedure SHA384Full(var Digest: TSHA384Digest; Msg: pointer; Len: word);
  {-SHA384 of Msg with init/update/final}

procedure SHA384FullXL(var Digest: TSHA384Digest; Msg: pointer; Len: longint);
  {-SHA384 of Msg with init/update/final}

procedure SHA384File(const fname: Str255; var Digest: TSHA384Digest; var buf; bsize: word; var Err: word);
  {-SHA384 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA512Init(var Context: THashContext);
  {-initialize context}

procedure SHA512Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA512Final(var Context: THashContext; var Digest: TSHA512Digest);
  {-finalize SHA512 calculation, clear context}

procedure SHA512FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512 calculation, clear context}

procedure SHA512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA512FinalBits(var Context: THashContext; var Digest: TSHA512Digest; BData: byte; bitlen: integer);
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA512SelfTest: boolean;
  {-self test for string from SHA512 document}

procedure SHA512Full(var Digest: TSHA512Digest; Msg: pointer; Len: word);
  {-SHA512 of Msg with init/update/final}

procedure SHA512FullXL(var Digest: TSHA512Digest; Msg: pointer; Len: longint);
  {-SHA512 of Msg with init/update/final}

procedure SHA512File(const fname: Str255; var Digest: TSHA512Digest; var buf; bsize: word; var Err: word);
  {-SHA512 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA5_224Init(var Context: THashContext);
  {-initialize context}

procedure SHA5_224Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA5_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA5_224Final(var Context: THashContext; var Digest: TSHA5_224Digest);
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_224FinalBits(var Context: THashContext; var Digest: TSHA5_224Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_224SelfTest: boolean;
  {-self test for string from SHA512/224 document}

procedure SHA5_224Full(var Digest: TSHA5_224Digest; Msg: pointer; Len: word);
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224FullXL(var Digest: TSHA5_224Digest; Msg: pointer; Len: longint);
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224File(const fname: Str255; var Digest: TSHA5_224Digest; var buf; bsize: word; var Err: word);
  {-SHA512/224 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA5_256Init(var Context: THashContext);
  {-initialize context}

procedure SHA5_256Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure SHA5_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure SHA5_256Final(var Context: THashContext; var Digest: TSHA5_256Digest);
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_256FinalBits(var Context: THashContext; var Digest: TSHA5_256Digest; BData: byte; bitlen: integer);
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_256SelfTest: boolean;
  {-self test for string from SHA512/256 document}

procedure SHA5_256Full(var Digest: TSHA5_256Digest; Msg: pointer; Len: word);
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256FullXL(var Digest: TSHA5_256Digest; Msg: pointer; Len: longint);
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256File(const fname: Str255; var Digest: TSHA5_256Digest; var buf; bsize: word; var Err: word);
  {-SHA512/256 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure Whirl_Init(var Context: THashContext);
  {-initialize context}

procedure Whirl_Update(var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure Whirl_UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure Whirl_Final(var Context: THashContext; var Digest: TWhirlDigest);
  {-finalize Whirlpool calculation, clear context}

procedure Whirl_FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Whirlpool calculation, clear context}

procedure Whirl_FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}

procedure Whirl_FinalBits(var Context: THashContext; var Digest: TWhirlDigest; BData: byte; bitlen: integer);
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}

function  Whirl_SelfTest: boolean;
  {-self test for strings from Whirlpool distribution}

procedure Whirl_Full(var Digest: TWhirlDigest; Msg: pointer; Len: word);
  {-Whirlpool hash-code of Msg with init/update/final}

procedure Whirl_FullXL(var Digest: TWhirlDigest; Msg: pointer; Len: longint);
  {-Whirlpool hash-code of Msg with init/update/final}

procedure Whirl_File(const fname: Str255; var Digest: TWhirlDigest; var buf; bsize: word; var Err: word);
  {-Whirlpool hash-code of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
type
  THMAC_Context = record
                    hashctx: THashContext;
                    hmacbuf: THashBuffer;
                    phashd : PHashDesc;
                  end;

procedure hmac_init(var ctx: THMAC_Context; phash: PHashDesc; key: pointer; klen: word);
  {-initialize HMAC context with hash descr phash^ and key}

procedure hmac_inits(var ctx: THMAC_Context; phash: PHashDesc; skey: Str255);
  {-initialize HMAC context with hash descr phash^ and skey}

procedure hmac_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_final(var ctx: THMAC_Context; var mac: THashDigest);
  {-end data input, calculate HMAC digest}

procedure hmac_final_bits(var ctx: THMAC_Context; var mac: THashDigest; BData: byte; bitlen: integer);
  {-end data input with bitlen bits from BData, calculate HMAC digest}


{---------------------------------------------------------------------------}
procedure hmac_MD5_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_MD5_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_MD5_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_MD5_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_MD5_final(var ctx: THMAC_Context; var mac: TMD5Digest);
  {-end data input, calculate HMAC digest}


{---------------------------------------------------------------------------}
procedure hmac_sha1_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_sha1_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_sha1_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_sha1_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_sha1_final(var ctx: THMAC_Context; var mac: TSHA1Digest);
  {-end data input, calculate HMAC digest}


{---------------------------------------------------------------------------}
procedure hmac_SHA256_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_SHA256_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_SHA256_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA256_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA256_final(var ctx: THMAC_Context; var mac: TSHA256Digest);
  {-end data input, calculate HMAC digest}


{---------------------------------------------------------------------------}
procedure hmac_SHA512_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_SHA512_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_SHA512_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA512_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_SHA512_final(var ctx: THMAC_Context; var mac: TSHA512Digest);
  {-end data input, calculate HMAC digest}


{---------------------------------------------------------------------------}
procedure hmac_Whirl_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}

procedure hmac_Whirl_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}

procedure hmac_Whirl_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}

procedure hmac_Whirl_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}

procedure hmac_Whirl_final(var ctx: THMAC_Context; var mac: TWhirlDigest);
  {-end data input, calculate HMAC digest}


{---------------------------------------------------------------------------}
const
  kdf_err_nil_pointer   = $0001;  {phash descriptor is nil}
  kdf_err_digestlen     = $0002;  {digest length from descriptor is zero}
  kdf_err_invalid_dKLen = $0003;  {dKLen greater than hash digest length}
  kdf_err_nil_input     = $0004;  {input nil pointer and non-zero length}

function kdf1(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf2(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf3(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function mgf1(phash: PHashDesc; pSeed: pointer; sLen: word; var Mask; mLen: word): integer;
  {-Derive Mask from seed, hash function from phash, Mask Generation Function 1 for PKCS #1}

function pbkdf1(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password pPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf1s(phash: PHashDesc; sPW: Str255; salt: pointer; C: longint; var DK; dkLen: word): integer;
  {-Derive key DK from password string sPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}

function pbkdf2s(phash: PHashDesc; sPW: Str255; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}

function hkdf(phash: PHashDesc;              {Descriptor of the Hash to use}
              pIKM: pointer; L_IKM: word;    {input key material: addr/length}
              salt: pointer; L_salt: word;   {optional salt; can be nil: see below }
              info: pointer; L_info: word;   {optional context/application specific information}
              var DK; dkLen: word): integer; {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}

function hkdfs(phash: PHashDesc; sIKM: Str255; {Hash; input key material as string}
               salt: pointer; L_salt: word;    {optional salt; can be nil: see below }
               info: pointer; L_info: word;    {optional context/application specific information}
               var DK; dkLen: word): integer;  {output key material: addr/length}
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}


{---------------------------------------------------------------------------}
type
  TCRCParam  = packed record
                 poly  : longint;    {CRC polynomial, top bit omitted}
                 init  : longint;    {Initial value of crc register}
                 xorout: longint;    {final xormask for crc register}
                 check : longint;    {CRC value for '123456789'}
                 width : word;       {width of algorithm, deg(poly)-1}
                 refin : boolean;    {reflect input bytes before processing}
                 refout: boolean;    {reflect reg before xor with xorout}
                 name  : string[19]; {name of the CRC algorithm}
               end;
type
  PCRC32Tab  = ^TCRC32Tab;             {Pointer to CRC table}
  TCRC32Tab  = array[byte] of longint; {CRC table type}

type
  TCRC_ctx   = packed record
                 reg   : longint;    {CRC state register}
                 poly  : longint;    {CRC polynomial, top bit omitted}
                 init  : longint;    {Initial value of crc register}
                 xorout: longint;    {final xormask for crc register}
                 check : longint;    {CRC value for '123456789'}
                 wmask : longint;    {mask with lowest width bits set}
                 ptab  : PCRC32Tab;  {pointer to table, may be nil}
                 width : word;       {width of algorithm, deg(poly)-1}
                 shift : word;       {shift value for table processing}
                 refin : boolean;    {reflect input bytes before processing}
                 refout: boolean;    {reflect reg before xor with xorout}
                 name  : string[19]; {name of the CRC algorithm}
               end;


procedure cm_CalcTab(const CRCPara: TCRCParam; var Tab: TCRC32Tab);
  {-Calculate CRC table from CRCPara, does nothing if CRCPara.width<8}

procedure cm_Create(const CRCPara: TCRCParam; ptab: PCRC32Tab; var ctx: TCRC_ctx);
  {-Create crc context from CRCPara, ptab may be nil}

procedure cm_Init(var ctx: TCRC_ctx);
  {-initialize context}

procedure cm_Update(var ctx: TCRC_ctx; Msg: pointer; Len: word);
  {-update ctx with Msg data}

procedure cm_Final(var ctx: TCRC_ctx; var CRC: longint);
  {-finalize calculation and return CRC}

function  cm_SelfTest(const CRCPara: TCRCParam) : boolean;
  {-Self test for CRCPara (no table version)}

procedure cm_Full(var ctx: TCRC_ctx; var CRC: longint; Msg: pointer; Len: word);
  {-process Msg with init/update/final using ctx}

procedure cm_File(const fname: Str255; var ctx: TCRC_ctx; var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC of file, buf: buffer with at least bsize bytes}

procedure cm_next(var ctx: TCRC_ctx; b: byte);
  {-update ctx with data byte b}

function  cm_reflect(v: longint; b: integer): longint;
  {-returns the reflected lowest b bits of v}

function  cm_combine(const para: TCRCParam; crc1, crc2: longint; len2: longint): longint;
  {-combine two CRCs calculated with para, i.e. if crc1 = CRC(m1) and}
  { crc2 = CRC(m2) then cm_combine = CRC(m1||m2); len2 = length(m2).}


{---------------------------------------------------------------------------}
type
  TSickCTX = packed record
               crc: word; {must be swapped for final result}
               prv: word; {temporary result for previous byte}
             end;

procedure CRC_Sick_Init(var ctx: TSickCTX);
  {-initialize context}

procedure CRC_Sick_Update(var ctx: TSickCTX; Msg: pointer; Len: word);
  {-update CRC_Sick with Msg data}

function CRC_Sick_Final(var ctx: TSickCTX): word;
  {-CRC_Sick: finalize calculation (dummy)}

function  CRC_Sick_SelfTest: boolean;
  {-Self test for CRC_Sick with '123456789' and 'abcdefghijklmnopqrstuvwxyz'}

procedure CRC_Sick_Full(var CRC: word; Msg: pointer; Len: word);
  {-CRC_Sick of Msg with init/update/final}

procedure CRC_Sick_File(const fname: Str255; var CRC: word; var buf; bsize: word; var Err: word);
  {-CRC_Sick of file, buf: buffer with at least bsize bytes}


procedure CRC_Sick_UpdateXL(var ctx: TSickCTX; Msg: pointer; Len: longint);
  {-update CRC_Sick with Msg data}

procedure CRC_Sick_FullXL(var CRC: word; Msg: pointer; Len: longint);
  {-CRC of Msg with init/update/final}


implementation


procedure CRC16Init;            external 'CH_DLL' name 'CRC16Init';
procedure CRC16Update;          external 'CH_DLL' name 'CRC16Update';
procedure CRC16Final;           external 'CH_DLL' name 'CRC16Final';
function  CRC16SelfTest;        external 'CH_DLL' name 'CRC16SelfTest';
procedure CRC16Full;            external 'CH_DLL' name 'CRC16Full';
procedure CRC16File;            external 'CH_DLL' name 'CRC16File';
procedure CRC16UpdateXL;        external 'CH_DLL' name 'CRC16UpdateXL';
procedure CRC16FullXL;          external 'CH_DLL' name 'CRC16FullXL';

procedure CRC24Init;            external 'CH_DLL' name 'CRC24Init';
procedure CRC24Update;          external 'CH_DLL' name 'CRC24Update';
procedure CRC24Final;           external 'CH_DLL' name 'CRC24Final';
function  CRC24SelfTest;        external 'CH_DLL' name 'CRC24SelfTest';
procedure CRC24Full;            external 'CH_DLL' name 'CRC24Full';
procedure CRC24File;            external 'CH_DLL' name 'CRC24File';
procedure CRC24UpdateXL;        external 'CH_DLL' name 'CRC24UpdateXL';
procedure CRC24FullXL;          external 'CH_DLL' name 'CRC24FullXL';
procedure Long2PGP;             external 'CH_DLL' name 'Long2PGP';

procedure CRC32Init;            external 'CH_DLL' name 'CRC32Init';
procedure CRC32Update;          external 'CH_DLL' name 'CRC32Update';
procedure CRC32Final;           external 'CH_DLL' name 'CRC32Final';
function  CRC32SelfTest;        external 'CH_DLL' name 'CRC32SelfTest';
procedure CRC32Full;            external 'CH_DLL' name 'CRC32Full';
procedure CRC32File;            external 'CH_DLL' name 'CRC32File';
procedure CRC32UpdateXL;        external 'CH_DLL' name 'CRC32UpdateXL';
procedure CRC32FullXL;          external 'CH_DLL' name 'CRC32FullXL';

procedure FCRC32Init;           external 'CH_DLL' name 'FCRC32Init';
procedure FCRC32Update;         external 'CH_DLL' name 'FCRC32Update';
procedure FCRC32Final;          external 'CH_DLL' name 'FCRC32Final';
function  FCRC32SelfTest;       external 'CH_DLL' name 'FCRC32SelfTest';
procedure FCRC32Full;           external 'CH_DLL' name 'FCRC32Full';
procedure FCRC32File;           external 'CH_DLL' name 'FCRC32File';
procedure FCRC32UpdateXL;       external 'CH_DLL' name 'FCRC32UpdateXL';
procedure FCRC32FullXL;         external 'CH_DLL' name 'FCRC32FullXL';

procedure Adler32Init;          external 'CH_DLL' name 'Adler32Init';
procedure Adler32Update;        external 'CH_DLL' name 'Adler32Update';
procedure Adler32Final;         external 'CH_DLL' name 'Adler32Final';
function  Adler32SelfTest;      external 'CH_DLL' name 'Adler32SelfTest';
procedure Adler32Full;          external 'CH_DLL' name 'Adler32Full';
procedure Adler32File;          external 'CH_DLL' name 'Adler32File';
procedure Adler32UpdateXL;      external 'CH_DLL' name 'Adler32UpdateXL';
procedure Adler32FullXL;        external 'CH_DLL' name 'Adler32FullXL';

procedure CRC64Init;            external 'CH_DLL' name 'CRC64Init';
procedure CRC64Update;          external 'CH_DLL' name 'CRC64Update';
procedure CRC64Final;           external 'CH_DLL' name 'CRC64Final';
function  CRC64SelfTest;        external 'CH_DLL' name 'CRC64SelfTest';
procedure CRC64Full;            external 'CH_DLL' name 'CRC64Full';
procedure CRC64File;            external 'CH_DLL' name 'CRC64File';
procedure CRC64UpdateXL;        external 'CH_DLL' name 'CRC64UpdateXL';
procedure CRC64FullXL;          external 'CH_DLL' name 'CRC64FullXL';

procedure InitHashDesc;         external 'CH_DLL' name 'InitHashDesc';
procedure RegisterHash;         external 'CH_DLL' name 'RegisterHash';
function  FindHash_by_ID;       external 'CH_DLL' name 'FindHash_by_ID';
function  FindHash_by_Name;     external 'CH_DLL' name 'FindHash_by_Name';
procedure HashFile;             external 'CH_DLL' name 'HashFile';
procedure HashUpdate;           external 'CH_DLL' name 'HashUpdate';
procedure HashFullXL;           external 'CH_DLL' name 'HashFullXL';
procedure HashFull;             external 'CH_DLL' name 'HashFull';
function  HashSameDigest;       external 'CH_DLL' name 'HashSameDigest';

procedure hmac_init;            external 'CH_DLL' name 'hmac_init';
procedure hmac_inits;           external 'CH_DLL' name 'hmac_inits';
procedure hmac_update;          external 'CH_DLL' name 'hmac_update';
procedure hmac_updateXL;        external 'CH_DLL' name 'hmac_updateXL';
procedure hmac_final;           external 'CH_DLL' name 'hmac_final';
procedure hmac_final_bits;      external 'CH_DLL' name 'hmac_final_bits';

function  kdf1;                 external 'CH_DLL' name 'kdf1';
function  kdf2;                 external 'CH_DLL' name 'kdf2';
function  kdf3;                 external 'CH_DLL' name 'kdf3';
function  mgf1;                 external 'CH_DLL' name 'mgf1';
function  pbkdf1;               external 'CH_DLL' name 'pbkdf1';
function  pbkdf1s;              external 'CH_DLL' name 'pbkdf1s';
function  pbkdf2;               external 'CH_DLL' name 'pbkdf2';
function  pbkdf2s;              external 'CH_DLL' name 'pbkdf2s';
function  hkdf;                 external 'CH_DLL' name 'hkdf';
function  hkdfs;                external 'CH_DLL' name 'hkdfs';

procedure ED2K_Init;            external 'CH_DLL' name 'ED2K_Init';
procedure ED2K_Update;          external 'CH_DLL' name 'ED2K_Update';
procedure ED2K_UpdateXL;        external 'CH_DLL' name 'ED2K_UpdateXL';
procedure ED2K_Final;           external 'CH_DLL' name 'ED2K_Final';
function  ED2K_SelfTest;        external 'CH_DLL' name 'ED2K_SelfTest';
procedure ED2K_Full;            external 'CH_DLL' name 'ED2K_Full';
procedure ED2K_FullXL;          external 'CH_DLL' name 'ED2K_FullXL';
procedure ED2K_File;            external 'CH_DLL' name 'ED2K_File';

procedure MD4Init;              external 'CH_DLL' name 'MD4Init';
procedure MD4Update;            external 'CH_DLL' name 'MD4Update';
procedure MD4Final;             external 'CH_DLL' name 'MD4Final';
procedure MD4FinalEx;           external 'CH_DLL' name 'MD4FinalEx';
procedure MD4FinalBits;         external 'CH_DLL' name 'MD4FinalBits';
procedure MD4FinalBitsEx;       external 'CH_DLL' name 'MD4FinalBitsEx';
function  MD4SelfTest;          external 'CH_DLL' name 'MD4SelfTest';
procedure MD4Full;              external 'CH_DLL' name 'MD4Full';
procedure MD4File;              external 'CH_DLL' name 'MD4File';
procedure MD4UpdateXL;          external 'CH_DLL' name 'MD4UpdateXL';
procedure MD4FullXL;            external 'CH_DLL' name 'MD4FullXL';

procedure MD5Init;              external 'CH_DLL' name 'MD5Init';
procedure MD5Update;            external 'CH_DLL' name 'MD5Update';
procedure MD5Final;             external 'CH_DLL' name 'MD5Final';
procedure MD5FinalEx;           external 'CH_DLL' name 'MD5FinalEx';
procedure MD5FinalBits;         external 'CH_DLL' name 'MD5FinalBits';
procedure MD5FinalBitsEx;       external 'CH_DLL' name 'MD5FinalBitsEx';
function  MD5SelfTest;          external 'CH_DLL' name 'MD5SelfTest';
procedure MD5Full;              external 'CH_DLL' name 'MD5Full';
procedure MD5File;              external 'CH_DLL' name 'MD5File';
procedure MD5UpdateXL;          external 'CH_DLL' name 'MD5UpdateXL';
procedure MD5FullXL;            external 'CH_DLL' name 'MD5FullXL';

procedure RMD160Init;           external 'CH_DLL' name 'RMD160Init';
procedure RMD160Update;         external 'CH_DLL' name 'RMD160Update';
procedure RMD160Final;          external 'CH_DLL' name 'RMD160Final';
procedure RMD160FinalEx;        external 'CH_DLL' name 'RMD160FinalEx';
procedure RMD160FinalBits;      external 'CH_DLL' name 'RMD160FinalBits';
procedure RMD160FinalBitsEx;    external 'CH_DLL' name 'RMD160FinalBitsEx';
function  RMD160SelfTest;       external 'CH_DLL' name 'RMD160SelfTest';
procedure RMD160Full;           external 'CH_DLL' name 'RMD160Full';
procedure RMD160File;           external 'CH_DLL' name 'RMD160File';
procedure RMD160UpdateXL;       external 'CH_DLL' name 'RMD160UpdateXL';
procedure RMD160FullXL;         external 'CH_DLL' name 'RMD160FullXL';

procedure SHA1Init;             external 'CH_DLL' name 'SHA1Init';
procedure SHA1Update;           external 'CH_DLL' name 'SHA1Update';
procedure SHA1Final;            external 'CH_DLL' name 'SHA1Final';
procedure SHA1FinalEx;          external 'CH_DLL' name 'SHA1FinalEx';
procedure SHA1FinalBits;        external 'CH_DLL' name 'SHA1FinalBits';
procedure SHA1FinalBitsEx;      external 'CH_DLL' name 'SHA1FinalBitsEx';
function  SHA1SelfTest;         external 'CH_DLL' name 'SHA1SelfTest';
procedure SHA1Full;             external 'CH_DLL' name 'SHA1Full';
procedure SHA1File;             external 'CH_DLL' name 'SHA1File';
procedure SHA1UpdateXL;         external 'CH_DLL' name 'SHA1UpdateXL';
procedure SHA1FullXL;           external 'CH_DLL' name 'SHA1FullXL';

procedure SHA224Init;           external 'CH_DLL' name 'SHA224Init';
procedure SHA224Update;         external 'CH_DLL' name 'SHA224Update';
procedure SHA224Final;          external 'CH_DLL' name 'SHA224Final';
procedure SHA224FinalEx;        external 'CH_DLL' name 'SHA224FinalEx';
procedure SHA224FinalBits;      external 'CH_DLL' name 'SHA224FinalBits';
procedure SHA224FinalBitsEx;    external 'CH_DLL' name 'SHA224FinalBitsEx';
function  SHA224SelfTest;       external 'CH_DLL' name 'SHA224SelfTest';
procedure SHA224Full;           external 'CH_DLL' name 'SHA224Full';
procedure SHA224File;           external 'CH_DLL' name 'SHA224File';
procedure SHA224UpdateXL;       external 'CH_DLL' name 'SHA224UpdateXL';
procedure SHA224FullXL;         external 'CH_DLL' name 'SHA224FullXL';

procedure SHA256Init;           external 'CH_DLL' name 'SHA256Init';
procedure SHA256Update;         external 'CH_DLL' name 'SHA256Update';
procedure SHA256Final;          external 'CH_DLL' name 'SHA256Final';
procedure SHA256FinalEx;        external 'CH_DLL' name 'SHA256FinalEx';
procedure SHA256FinalBits;      external 'CH_DLL' name 'SHA256FinalBits';
procedure SHA256FinalBitsEx;    external 'CH_DLL' name 'SHA256FinalBitsEx';
function  SHA256SelfTest;       external 'CH_DLL' name 'SHA256SelfTest';
procedure SHA256Full;           external 'CH_DLL' name 'SHA256Full';
procedure SHA256File;           external 'CH_DLL' name 'SHA256File';
procedure SHA256UpdateXL;       external 'CH_DLL' name 'SHA256UpdateXL';
procedure SHA256FullXL;         external 'CH_DLL' name 'SHA256FullXL';

procedure SHA384Init;           external 'CH_DLL' name 'SHA384Init';
procedure SHA384Update;         external 'CH_DLL' name 'SHA384Update';
procedure SHA384Final;          external 'CH_DLL' name 'SHA384Final';
procedure SHA384FinalEx;        external 'CH_DLL' name 'SHA384FinalEx';
procedure SHA384FinalBits;      external 'CH_DLL' name 'SHA384FinalBits';
procedure SHA384FinalBitsEx;    external 'CH_DLL' name 'SHA384FinalBitsEx';
function  SHA384SelfTest;       external 'CH_DLL' name 'SHA384SelfTest';
procedure SHA384Full;           external 'CH_DLL' name 'SHA384Full';
procedure SHA384File;           external 'CH_DLL' name 'SHA384File';
procedure SHA384UpdateXL;       external 'CH_DLL' name 'SHA384UpdateXL';
procedure SHA384FullXL;         external 'CH_DLL' name 'SHA384FullXL';

procedure SHA512Init;           external 'CH_DLL' name 'SHA512Init';
procedure SHA512Update;         external 'CH_DLL' name 'SHA512Update';
procedure SHA512Final;          external 'CH_DLL' name 'SHA512Final';
procedure SHA512FinalEx;        external 'CH_DLL' name 'SHA512FinalEx';
procedure SHA512FinalBits;      external 'CH_DLL' name 'SHA512FinalBits';
procedure SHA512FinalBitsEx;    external 'CH_DLL' name 'SHA512FinalBitsEx';
function  SHA512SelfTest;       external 'CH_DLL' name 'SHA512SelfTest';
procedure SHA512Full;           external 'CH_DLL' name 'SHA512Full';
procedure SHA512File;           external 'CH_DLL' name 'SHA512File';
procedure SHA512UpdateXL;       external 'CH_DLL' name 'SHA512UpdateXL';
procedure SHA512FullXL;         external 'CH_DLL' name 'SHA512FullXL';

procedure SHA5_224Init;         external 'CH_DLL' name 'SHA5_224Init';
procedure SHA5_224Update;       external 'CH_DLL' name 'SHA5_224Update';
procedure SHA5_224Final;        external 'CH_DLL' name 'SHA5_224Final';
procedure SHA5_224FinalEx;      external 'CH_DLL' name 'SHA5_224FinalEx';
procedure SHA5_224FinalBits;    external 'CH_DLL' name 'SHA5_224FinalBits';
procedure SHA5_224FinalBitsEx;  external 'CH_DLL' name 'SHA5_224FinalBitsEx';
function  SHA5_224SelfTest;     external 'CH_DLL' name 'SHA5_224SelfTest';
procedure SHA5_224Full;         external 'CH_DLL' name 'SHA5_224Full';
procedure SHA5_224File;         external 'CH_DLL' name 'SHA5_224File';
procedure SHA5_224UpdateXL;     external 'CH_DLL' name 'SHA5_224UpdateXL';
procedure SHA5_224FullXL;       external 'CH_DLL' name 'SHA5_224FullXL';

procedure SHA5_256Init;         external 'CH_DLL' name 'SHA5_256Init';
procedure SHA5_256Update;       external 'CH_DLL' name 'SHA5_256Update';
procedure SHA5_256Final;        external 'CH_DLL' name 'SHA5_256Final';
procedure SHA5_256FinalEx;      external 'CH_DLL' name 'SHA5_256FinalEx';
procedure SHA5_256FinalBits;    external 'CH_DLL' name 'SHA5_256FinalBits';
procedure SHA5_256FinalBitsEx;  external 'CH_DLL' name 'SHA5_256FinalBitsEx';
function  SHA5_256SelfTest;     external 'CH_DLL' name 'SHA5_256SelfTest';
procedure SHA5_256Full;         external 'CH_DLL' name 'SHA5_256Full';
procedure SHA5_256File;         external 'CH_DLL' name 'SHA5_256File';
procedure SHA5_256UpdateXL;     external 'CH_DLL' name 'SHA5_256UpdateXL';
procedure SHA5_256FullXL;       external 'CH_DLL' name 'SHA5_256FullXL';

procedure Whirl_Init;           external 'CH_DLL' name 'Whirl_Init';
procedure Whirl_Update;         external 'CH_DLL' name 'Whirl_Update';
procedure Whirl_Final;          external 'CH_DLL' name 'Whirl_Final';
procedure Whirl_FinalEx;        external 'CH_DLL' name 'Whirl_FinalEx';
procedure Whirl_FinalBits;      external 'CH_DLL' name 'Whirl_FinalBits';
procedure Whirl_FinalBitsEx;    external 'CH_DLL' name 'Whirl_FinalBitsEx';
function  Whirl_SelfTest;       external 'CH_DLL' name 'Whirl_SelfTest';
procedure Whirl_Full;           external 'CH_DLL' name 'Whirl_Full';
procedure Whirl_File;           external 'CH_DLL' name 'Whirl_File';
procedure Whirl_UpdateXL;       external 'CH_DLL' name 'Whirl_UpdateXL';
procedure Whirl_FullXL;         external 'CH_DLL' name 'Whirl_FullXL';

procedure hmac_MD5_init;        external 'CH_DLL' name 'hmac_MD5_init';
procedure hmac_MD5_inits;       external 'CH_DLL' name 'hmac_MD5_inits';
procedure hmac_MD5_update;      external 'CH_DLL' name 'hmac_MD5_update';
procedure hmac_MD5_final;       external 'CH_DLL' name 'hmac_MD5_final';
procedure hmac_MD5_updateXL;    external 'CH_DLL' name 'hmac_MD5_updateXL';

procedure hmac_sha1_init;       external 'CH_DLL' name 'hmac_sha1_init';
procedure hmac_sha1_inits;      external 'CH_DLL' name 'hmac_sha1_inits';
procedure hmac_sha1_update;     external 'CH_DLL' name 'hmac_sha1_update';
procedure hmac_sha1_final;      external 'CH_DLL' name 'hmac_sha1_final';
procedure hmac_sha1_updateXL;   external 'CH_DLL' name 'hmac_sha1_updateXL';

procedure hmac_SHA256_init;     external 'CH_DLL' name 'hmac_SHA256_init';
procedure hmac_SHA256_inits;    external 'CH_DLL' name 'hmac_SHA256_inits';
procedure hmac_SHA256_update;   external 'CH_DLL' name 'hmac_SHA256_update';
procedure hmac_SHA256_final;    external 'CH_DLL' name 'hmac_SHA256_final';
procedure hmac_SHA256_updateXL; external 'CH_DLL' name 'hmac_SHA256_updateXL';

procedure hmac_SHA512_init;     external 'CH_DLL' name 'hmac_SHA512_init';
procedure hmac_SHA512_inits;    external 'CH_DLL' name 'hmac_SHA512_inits';
procedure hmac_SHA512_update;   external 'CH_DLL' name 'hmac_SHA512_update';
procedure hmac_SHA512_final;    external 'CH_DLL' name 'hmac_SHA512_final';
procedure hmac_SHA512_updateXL; external 'CH_DLL' name 'hmac_SHA512_updateXL';

procedure hmac_Whirl_init;      external 'CH_DLL' name 'hmac_Whirl_init';
procedure hmac_Whirl_inits;     external 'CH_DLL' name 'hmac_Whirl_inits';
procedure hmac_Whirl_update;    external 'CH_DLL' name 'hmac_Whirl_update';
procedure hmac_Whirl_final;     external 'CH_DLL' name 'hmac_Whirl_final';
procedure hmac_Whirl_updateXL;  external 'CH_DLL' name 'hmac_Whirl_updateXL';

procedure cm_CalcTab;           external 'CH_DLL' name 'cm_CalcTab';
procedure cm_Create;            external 'CH_DLL' name 'cm_Create';
procedure cm_Init;              external 'CH_DLL' name 'cm_Init';
procedure cm_Update;            external 'CH_DLL' name 'cm_Update';
procedure cm_Final;             external 'CH_DLL' name 'cm_Final';
function  cm_SelfTest;          external 'CH_DLL' name 'cm_SelfTest';
procedure cm_Full;              external 'CH_DLL' name 'cm_Full';
procedure cm_File;              external 'CH_DLL' name 'cm_File';
procedure cm_next;              external 'CH_DLL' name 'cm_next';
function  cm_reflect;           external 'CH_DLL' name 'cm_reflect';
function  cm_combine;           external 'CH_DLL' name 'cm_combine';


procedure CRC_Sick_Init;        external 'CH_DLL' name 'CRC_Sick_Init';
procedure CRC_Sick_Update;      external 'CH_DLL' name 'CRC_Sick_Update';
function  CRC_Sick_Final;       external 'CH_DLL' name 'CRC_Sick_Final';
function  CRC_Sick_SelfTest;    external 'CH_DLL' name 'CRC_Sick_SelfTest';
procedure CRC_Sick_Full;        external 'CH_DLL' name 'CRC_Sick_Full';
procedure CRC_Sick_File;        external 'CH_DLL' name 'CRC_Sick_File';
procedure CRC_Sick_UpdateXL;    external 'CH_DLL' name 'CRC_Sick_UpdateXL';
procedure CRC_Sick_FullXL;      external 'CH_DLL' name 'CRC_Sick_FullXL';


function  CH_DLL_Version;       external 'CH_DLL' name 'CH_DLL_Version';

end.
