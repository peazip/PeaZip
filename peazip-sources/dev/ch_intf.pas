unit CH_Intf;

{Interface unit for CH_DLL}

interface

(*************************************************************************

 DESCRIPTION     :  Interface unit for CH_DLL

 REQUIREMENTS    :  D2-D7/D9/D10/D12/D17-D18, FPC 2+

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     18.03.02  W.Ehrhardt  Initial version
 2.00     26.07.03  we          common vers., longint for word32
 2.10     29.08.03  we          XL versions for Win32
 2.20     29.08.03  we          With Adler32, CRC64
 2.40     10.10.03  we          common version, english comments
 2.50     24.11.03  we          with SHA384/512
 3.00     01.12.03  we          Common version 3.0
 3.10     02.01.04  we          SHA224
 3.11     07.07.04  we          New unit name, HMACs, keyderiv
 3.12     04.01.05  we          HMACSHA256/512, PBKDF2_256/512
 3.13     11.12.05  we          Whirlpool functions
 3.14     22.12.05  we          Whirlpool V3.0 only
 3.15     08.01.06  we          SHA1Compress removed
 3.16     17.01.06  we          hash, hmac, pb_kdf
 3.17     18.01.06  we          Descriptor fields HAlgNum, HSig
 3.18     31.01.06  we          RIPEMD-160
 3.19     11.02.06  we          Fields: HDSize, HVersion, HPtrOID, HLenOID
 3.20     02.04.06  we          CRC24
 3.21     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.22     20.02.07  we          MD4, ED2K
 3.23     22.02.07  we          POID_Vec=^TOID_Vec, typed HPtrOID
 3.24     30.06.07  we          FCRC32
 3.25     04.10.07  we          THashContext.Index now longint
 3.26     07.05.08  we          [Hash]FinalBits/Ex, HashSameDigest
 3.27     12.07.08  we          crcmodel, removed keyderive routines
 3.28     12.07.08  we          kdf with kdf1/2/3, mgf1, pbkdf1/2
 3.29     16.11.08  we          uses BTypes and Str255
 3.30     05.07.09  we          external 'ch_dll.dll'
 3.31     06.07.09  we          CH_DLL_Version returns PAnsiChar
 3.32     06.09.09  we          cm_combine
 3.32     20.08.10  we          hkdf, hkdfs
 3.33     17.12.10  we          CRC_Sick
 3.34     08.03.12  we          SHA512/224 and SHA512/256
 3.35     15.08.14  we          pbkdf2 functions with longint sLen, dkLen
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


function CH_DLL_Version: PAnsiChar;
stdcall; external 'ch_dll.dll' name 'CH_DLL_Version';
  {-return DLL version as PAnsiChar}


{---------------------------------------------------------------------------}
procedure CRC16Init(var CRC: word);
stdcall;  external 'ch_dll.dll' name 'CRC16Init';
  {-initialize context}

procedure CRC16Update(var CRC: word; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC16Update';
  {-update CRC16 with Msg data}

procedure CRC16Final(var CRC: word);
stdcall;  external 'ch_dll.dll' name 'CRC16Final';
  {-CRC16: finalize calculation (dummy)}

function  CRC16SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'CRC16SelfTest';
  {-Self test for CRC16}

procedure CRC16Full(var CRC: word; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC16Full';
  {-CRC16 of Msg with init/update/final}

procedure CRC16File(const fname: Str255; var CRC: word; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'CRC16File';
  {-CRC16 of file, buf: buffer with at least bsize bytes}

procedure CRC16UpdateXL(var CRC: word; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC16UpdateXL';
  {-update CRC16 with Msg data}

procedure CRC16FullXL(var CRC: word; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC16FullXL';
  {-CRC16 of Msg with init/update/final}


{---------------------------------------------------------------------------}
type
  TPGPDigest = array[0..2] of byte;  {OpenPGP 3 byte MSB first CRC24 digest}

procedure Long2PGP(CRC: longint; var PGPCRC: TPGPDigest);
stdcall;  external 'ch_dll.dll' name 'Long2PGP';
  {-convert longint CRC24 to OpenPGP MSB first format}

procedure CRC24Init(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'CRC24Init';
  {-initialize context}

procedure CRC24Update(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC24Update';
  {-update CRC24 with Msg data}

procedure CRC24Final(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'CRC24Final';
  {-CRC24: finalize calculation}

function  CRC24SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'CRC24SelfTest';
  {-Self test for CRC24}

procedure CRC24Full(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC24Full';
  {-CRC24 of Msg with init/update/final}

procedure CRC24File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'CRC24File';
  {-CRC24 of file, buf: buffer with at least bsize bytes}

procedure CRC24UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC24UpdateXL';
  {-update CRC24 with Msg data}

procedure CRC24FullXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC24FullXL';
  {-CRC24 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure CRC32Init(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'CRC32Init';
  {-initialize context}

procedure CRC32Update(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC32Update';
  {-update CRC32 with Msg data}

procedure CRC32Final(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'CRC32Final';
  {-CRC32: finalize calculation}

function  CRC32SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'CRC32SelfTest';
  {-Self test for CRC32}

procedure CRC32Full(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC32Full';
  {-CRC32 of Msg with init/update/final}

procedure CRC32File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'CRC32File';
  {-CRC32 of file, buf: buffer with at least bsize bytes}

procedure CRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC32UpdateXL';
  {-update CRC32 with Msg data}

procedure CRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC32FullXL';
  {-CRC32 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure FCRC32Init(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'FCRC32Init';
  {-initialize context}

procedure FCRC32Update(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'FCRC32Update';
  {-update CRC with Msg data}

procedure FCRC32Final(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'FCRC32Final';
  {-FCRC32: finalize calculation}

function  FCRC32SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'FCRC32SelfTest';
  {-Self test for FCRC32}

procedure FCRC32Full(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'FCRC32Full';
  {-CRC32 of Msg with init/update/final}

procedure FCRC32File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'FCRC32File';
  {-CRC32 of file, buf: buffer with at least bsize bytes}

procedure FCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'FCRC32UpdateXL';
  {-update CRC with Msg data}

procedure FCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'FCRC32FullXL';
  {-CRC32 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure Adler32Init(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'Adler32Init';
  {-initialize context}

procedure Adler32Update(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'Adler32Update';
  {-update Adler32 with Msg data}

procedure Adler32Final(var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'Adler32Final';
  {-Adler32: finalize calculation}

function  Adler32SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'Adler32SelfTest';
  {-Self test for Adler32}

procedure Adler32Full(var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'Adler32Full';
  {-Adler32 of Msg with init/update/final}

procedure Adler32File(const fname: Str255; var CRC: longint; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'Adler32File';
  {-Adler32 of file, buf: buffer with at least bsize bytes}

procedure Adler32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'Adler32UpdateXL';
  {-update Adler32 with Msg data}

procedure Adler32FullXL(var CRC: longint; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'Adler32FullXL';
  {-Adler32 of Msg with init/update/final}


{---------------------------------------------------------------------------}
type
  TCRC64 = packed record
             lo32, hi32: longint;
           end;

procedure CRC64Init(var CRC: TCRC64);
stdcall;  external 'ch_dll.dll' name 'CRC64Init';
  {-CRC64 initialization}

procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC64Update';
  {-update CRC64 with Msg data}

procedure CRC64Final(var CRC: TCRC64);
stdcall;  external 'ch_dll.dll' name 'CRC64Final';
  {-CRC64: finalize calculation, CRC wird geloescht}

function  CRC64SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'CRC64SelfTest';
  {-Self test for CRC64}

procedure CRC64Full(var CRC: TCRC64; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC64Full';
  {-CRC64 of Msg with init/update/final}

procedure CRC64File(const fname: Str255; var CRC: TCRC64; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'CRC64File';
  {-CRC64 of file, buf: buffer with at least bsize bytes}

procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC64UpdateXL';
  {-update CRC64 with Msg data}

procedure CRC64FullXL(var CRC: TCRC64; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC64FullXL';
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
stdcall;  external 'ch_dll.dll' name 'RegisterHash';
  {-Registers algorithm with AlgID and Hash descriptor PHash^}

function  FindHash_by_ID(AlgoID: THashAlgorithm): PHashDesc;
stdcall;  external 'ch_dll.dll' name 'FindHash_by_ID';
  {-Return PHashDesc of AlgoID, nil if not found/registered}

function  FindHash_by_Name(AlgoName: THashName): PHashDesc;
stdcall;  external 'ch_dll.dll' name 'FindHash_by_Name';
  {-Return PHashDesc of Algo with AlgoName, nil if not found/registered}

procedure HashFile(const fname: Str255; PHash: PHashDesc; var Digest: THashDigest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'HashFile';
  {-Calulate hash digest of file, buf: buffer with at least bsize bytes}

procedure HashUpdate(PHash: PHashDesc; var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'HashUpdate';
  {-update context with Msg data}

procedure HashFullXL(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'HashFullXL';
  {-Calulate hash digest of Msg with init/update/final}

procedure HashFull(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'HashFull';
  {-Calulate hash digest of Msg with init/update/final}

function  HashSameDigest(PHash: PHashDesc; PD1, PD2: PHashDigest): boolean;
stdcall;  external 'ch_dll.dll' name 'HashSameDigest';
  {-Return true if same digests, using HDigestlen of PHash}


{---------------------------------------------------------------------------}
type
  THMAC_Context = record
                    hashctx: THashContext;
                    hmacbuf: THashBuffer;
                    phashd : PHashDesc;
                  end;

procedure hmac_init(var ctx: THMAC_Context; phash: PHashDesc; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_init';
  {-initialize HMAC context with hash descr phash^ and key}

procedure hmac_inits(var ctx: THMAC_Context; phash: PHashDesc; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_inits';
  {-initialize HMAC context with hash descr phash^ and skey}

procedure hmac_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_update';
  {-HMAC data input, may be called more than once}

procedure hmac_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_updateXL';
  {-HMAC data input, may be called more than once}

procedure hmac_final(var ctx: THMAC_Context; var mac: THashDigest);
stdcall;  external 'ch_dll.dll' name 'hmac_final';
  {-end data input, calculate HMAC digest}

procedure hmac_final_bits(var ctx: THMAC_Context; var mac: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'hmac_final_bits';
  {-end data input with bitlen bits from BData, calculate HMAC digest}


{---------------------------------------------------------------------------}
const
  kdf_err_nil_pointer   = $0001;  {phash descriptor is nil}
  kdf_err_digestlen     = $0002;  {digest length from descriptor is zero}
  kdf_err_invalid_dKLen = $0003;  {dKLen greater than hash digest length}
  kdf_err_nil_input     = $0004;  {input nil pointer and non-zero length}


function kdf1(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'kdf1';
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf2(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'kdf2';
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function kdf3(phash: PHashDesc; Z: pointer; zLen: word; pOtherInfo: pointer; oiLen: word; var DK; dkLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'kdf3';
  {-Derive key DK from shared secret Z using optional OtherInfo, hash function from phash}

function mgf1(phash: PHashDesc; pSeed: pointer; sLen: word; var Mask; mLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'mgf1';
  {-Derive Mask from seed, hash function from phash, Mask Generation Function 1 for PKCS #1}

function pbkdf1(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; C: longint; var DK; dkLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'pbkdf1';
  {-Derive key DK from password pPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf1s(phash: PHashDesc; sPW: Str255; salt: pointer; C: longint; var DK; dkLen: word): integer;
stdcall;  external 'ch_dll.dll' name 'pbkdf1s';
  {-Derive key DK from password string sPW using 8 byte salt and iteration count C, uses hash function from phash}

function pbkdf2(phash: PHashDesc; pPW: pointer; pLen: word; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
stdcall;  external 'ch_dll.dll' name 'pbkdf2';
  {-Derive key DK from password pPW using salt and iteration count C, uses hash function from phash}

function pbkdf2s(phash: PHashDesc; sPW: Str255; salt: pointer; sLen,C: longint; var DK; dkLen: longint): integer;
stdcall;  external 'ch_dll.dll' name 'pbkdf2s';
  {-Derive key DK from password string sPW using salt and iteration count C, uses hash function from phash}

function hkdf(phash: PHashDesc;              {Descriptor of the Hash to use}
              pIKM: pointer; L_IKM: word;    {input key material: addr/length}
              salt: pointer; L_salt: word;   {optional salt; can be nil: see below }
              info: pointer; L_info: word;   {optional context/application specific information}
              var DK; dkLen: word): integer; {output key material: addr/length}
stdcall;  external 'ch_dll.dll' name 'hkdf';
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}

function hkdfs(phash: PHashDesc; sIKM: Str255; {Hash; input key material as string}
               salt: pointer; L_salt: word;    {optional salt; can be nil: see below }
               info: pointer; L_info: word;    {optional context/application specific information}
               var DK; dkLen: word): integer;  {output key material: addr/length}
stdcall;  external 'ch_dll.dll' name 'hkdfs';
  {-Derive key DK from input key material and salt/info, uses hash function from phash}
  { If salt=nil then phash^.HDigestLen binary zeros will be used as salt.}


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
stdcall; external 'ch_dll.dll' name 'ED2K_Init';
  {-initialize context}

procedure ED2K_Update(var Context: TED2KContext; Msg: pointer; Len: word);
stdcall; external 'ch_dll.dll' name 'ED2K_Update';
  {-update context with Msg data}

procedure ED2K_UpdateXL(var Context: TED2KContext; Msg: pointer; Len: longint);
stdcall; external 'ch_dll.dll' name 'ED2K_UpdateXL';
  {-update context with Msg data}

procedure ED2K_Final(var Context: TED2KContext; var ResRec: TED2KResult);
stdcall; external 'ch_dll.dll' name 'ED2K_Final';
  {-finalize eDonkey hash calculation, clear context}

function  ED2K_SelfTest: boolean;
stdcall; external 'ch_dll.dll' name 'ED2K_SelfTest';
  {-eDonkey hash self test for two small strings}

procedure ED2K_Full(var ResRec: TED2KResult; Msg: pointer; Len: word);
stdcall; external 'ch_dll.dll' name 'ED2K_Full';
  {-eDonkey hash-code of Msg with init/update/final}

procedure ED2K_FullXL(var ResRec: TED2KResult; Msg: pointer; Len: longint);
stdcall; external 'ch_dll.dll' name 'ED2K_FullXL';
  {-eDonkey hash-code of Msg with init/update/final}

procedure ED2K_File(const fname: Str255; var ResRec: TED2KResult; var buf; bsize: word; var Err: word);
stdcall; external 'ch_dll.dll' name 'ED2K_File';
  {-eDonkey hash-code of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure MD4Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'MD4Init';
  {-initialize context}

procedure MD4Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'MD4Update';
  {-update context with Msg data}

procedure MD4Final(var Context: THashContext; var Digest: TMD4Digest);
stdcall;  external 'ch_dll.dll' name 'MD4Final';
  {-finalize MD4 calculation, clear context}

procedure MD4FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'MD4FinalBitsEx';
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD4FinalBits(var Context: THashContext; var Digest: TMD4Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'MD4FinalBits';
  {-finalize MD4 calculation with bitlen bits from BData (big-endian), clear context}

function  MD4SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'MD4SelfTest';
  {-self test for string from MD4 document}

procedure MD4Full(var Digest: TMD4Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'MD4Full';
  {-MD4 of Msg with init/update/final}

procedure MD4File(const fname: Str255; var Digest: TMD4Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'MD4File';
  {-MD4 of file, buf: buffer with at least bsize bytes}

procedure MD4UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'MD4UpdateXL';
  {-update context with Msg data}

procedure MD4FullXL(var Digest: TMD4Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'MD4FullXL';
  {-MD4 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure MD5Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'MD5Init';
  {-initialize context}

procedure MD5Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'MD5Update';
  {-update context with Msg data}

procedure MD5Final(var Context: THashContext; var Digest: TMD5Digest);
stdcall;  external 'ch_dll.dll' name 'MD5Final';
  {-finalize MD5 calculation, clear context}

procedure MD5FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'MD5FinalBitsEx';
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

procedure MD5FinalBits(var Context: THashContext; var Digest: TMD5Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'MD5FinalBits';
  {-finalize MD5 calculation with bitlen bits from BData (big-endian), clear context}

function  MD5SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'MD5SelfTest';
  {-self test for string from MD5 document}

procedure MD5Full(var Digest: TMD5Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'MD5Full';
  {-MD5 of Msg with init/update/final}

procedure MD5File(const fname: Str255; var Digest: TMD5Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'MD5File';
  {-MD5 of file, buf: buffer with at least bsize bytes}

procedure MD5UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'MD5UpdateXL';
  {-update context with Msg data}

procedure MD5FullXL(var Digest: TMD5Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'MD5FullXL';
  {-MD5 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure RMD160Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'RMD160Init';
  {-initialize context}

procedure RMD160Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'RMD160Update';
  {-update context with Msg data}

procedure RMD160Final(var Context: THashContext; var Digest: TRMD160Digest);
stdcall;  external 'ch_dll.dll' name 'RMD160Final';
  {-finalize RMD160 calculation, clear context}

procedure RMD160FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'RMD160FinalBitsEx';
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

procedure RMD160FinalBits(var Context: THashContext; var Digest: TRMD160Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'RMD160FinalBits';
  {-finalize RMD160 calculation with bitlen bits from BData (big-endian), clear context}

function  RMD160SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'RMD160SelfTest';
  {-self test for string from RMD160 document}

procedure RMD160Full(var Digest: TRMD160Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'RMD160Full';
  {-RMD160 of Msg with init/update/final}

procedure RMD160File(const fname: Str255; var Digest: TRMD160Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'RMD160File';
  {-RMD160 of file, buf: buffer with at least bsize bytes}

procedure RMD160UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'RMD160UpdateXL';
  {-update context with Msg data}

procedure RMD160FullXL(var Digest: TRMD160Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'RMD160FullXL';
  {-RMD160 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure SHA1Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA1Init';
  {-initialize context}

procedure SHA1Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA1Update';
  {-update context with Msg data}

procedure SHA1Final(var Context: THashContext; var Digest: TSHA1Digest);
stdcall;  external 'ch_dll.dll' name 'SHA1Final';
  {-finalize SHA1 calculation, clear context}

procedure SHA1FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA1FinalBitsEx';
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA1FinalBits(var Context: THashContext; var Digest: TSHA1Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA1FinalBits';
  {-finalize SHA1 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA1SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA1SelfTest';
  {-self test SHA1: compare with known value}

procedure SHA1Full(var Digest: TSHA1Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA1Full';
  {-SHA1 of Msg with init/update/final}

procedure SHA1File(const fname: Str255; var Digest: TSHA1Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA1File';
  {-SHA1 of file, buf: buffer with at least bsize bytes}

procedure SHA1UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA1UpdateXL';
  {-update context with Msg data}

procedure SHA1FullXL(var Digest: TSHA1Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA1FullXL';
  {-SHA1 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure SHA224Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA224Init';
  {-initialize context}

procedure SHA224Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA224Update';
  {-update context with Msg data}

procedure SHA224Final(var Context: THashContext; var Digest: TSHA224Digest);
stdcall;  external 'ch_dll.dll' name 'SHA224Final';
  {-finalize SHA224 calculation, clear context}

procedure SHA224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA224FinalBitsEx';
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA224FinalBits(var Context: THashContext; var Digest: TSHA224Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA224FinalBits';
  {-finalize SHA224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA224SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA224SelfTest';
  {-self test for string from SHA224 document}

procedure SHA224Full(var Digest: TSHA224Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA224Full';
  {-SHA224 of Msg with init/update/final}

procedure SHA224File(const fname: Str255; var Digest: TSHA224Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA224File';
  {-SHA224 of file, buf: buffer with at least bsize bytes}

procedure SHA224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA224UpdateXL';
  {-update context with Msg data}

procedure SHA224FullXL(var Digest: TSHA224Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA224FullXL';
  {-SHA224 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure SHA256Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA256Init';
  {-initialize context}

procedure SHA256Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA256Update';
  {-update context with Msg data}

procedure SHA256Final(var Context: THashContext; var Digest: TSHA256Digest);
stdcall;  external 'ch_dll.dll' name 'SHA256Final';
  {-finalize SHA256 calculation, clear context}

procedure SHA256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA256FinalBitsEx';
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA256FinalBits(var Context: THashContext; var Digest: TSHA256Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA256FinalBits';
  {-finalize SHA256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA256SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA256SelfTest';
  {-self test for string from SHA224 document}

procedure SHA256Full(var Digest: TSHA256Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA256Full';
  {-SHA256 of Msg with init/update/final}

procedure SHA256File(const fname: Str255; var Digest: TSHA256Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA256File';
  {-SHA256 of file, buf: buffer with at least bsize bytes}

procedure SHA256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA256UpdateXL';
  {-update context with Msg data}

procedure SHA256FullXL(var Digest: TSHA256Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA256FullXL';
  {-SHA256 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure SHA384Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA384Init';
  {-initialize context}

procedure SHA384Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA384Update';
  {-update context with Msg data}

procedure SHA384Final(var Context: THashContext; var Digest: TSHA384Digest);
stdcall;  external 'ch_dll.dll' name 'SHA384Final';
  {-finalize SHA384 calculation, clear context}

procedure SHA384FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA384FinalBitsEx';
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA384FinalBits(var Context: THashContext; var Digest: TSHA384Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA384FinalBits';
  {-finalize SHA384 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA384SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA384SelfTest';
  {-self test for string from SHA384 document}

procedure SHA384Full(var Digest: TSHA384Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA384Full';
  {-SHA384 of Msg with init/update/final}

procedure SHA384File(const fname: Str255; var Digest: TSHA384Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA384File';
  {-SHA384 of file, buf: buffer with at least bsize bytes}

procedure SHA384UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA384UpdateXL';
  {-update context with Msg data}

procedure SHA384FullXL(var Digest: TSHA384Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA384FullXL';
  {-SHA384 of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure SHA512Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA512Init';
  {-initialize context}

procedure SHA512Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA512Update';
  {-update context with Msg data}

procedure SHA512Final(var Context: THashContext; var Digest: TSHA512Digest);
stdcall;  external 'ch_dll.dll' name 'SHA512Final';
  {-finalize SHA512 calculation, clear context}

procedure SHA512FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA512FinalBitsEx';
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA512FinalBits(var Context: THashContext; var Digest: TSHA512Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA512FinalBits';
  {-finalize SHA512 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA512SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA512SelfTest';
  {-self test for string from SHA512 document}

procedure SHA512Full(var Digest: TSHA512Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA512Full';
  {-SHA512 of Msg with init/update/final}

procedure SHA512File(const fname: Str255; var Digest: TSHA512Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA512File';
  {-SHA512 of file, buf: buffer with at least bsize bytes}

procedure SHA512UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA512UpdateXL';
  {-update context with Msg data}

procedure SHA512FullXL(var Digest: TSHA512Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA512FullXL';
  {-SHA512 of Msg with init/update/final}

{---------------------------------------------------------------------------}
procedure SHA5_224Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA5_224Init';
  {-initialize context}

procedure SHA5_224Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_224Update';
  {-update context with Msg data}

procedure SHA5_224UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA5_224UpdateXL';
  {-update context with Msg data}

procedure SHA5_224Final(var Context: THashContext; var Digest: TSHA5_224Digest);
stdcall;  external 'ch_dll.dll' name 'SHA5_224Final';
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalEx(var Context: THashContext; var Digest: THashDigest);
stdcall;  external 'ch_dll.dll' name 'SHA5_224FinalEx';
  {-finalize SHA512/224 calculation, clear context}

procedure SHA5_224FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA5_224FinalBitsEx';
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_224FinalBits(var Context: THashContext; var Digest: TSHA5_224Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA5_224FinalBits';
  {-finalize SHA512/224 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_224SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA5_224SelfTest';
  {-self test for string from SHA512/224 document}

procedure SHA5_224Full(var Digest: TSHA5_224Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_224Full';
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224FullXL(var Digest: TSHA5_224Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA5_224FullXL';
  {-SHA512/224 of Msg with init/update/final}

procedure SHA5_224File(const fname: Str255; var Digest: TSHA5_224Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_224File';
  {-SHA512/224 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure SHA5_256Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'SHA5_256Init';
  {-initialize context}

procedure SHA5_256Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_256Update';
  {-update context with Msg data}

procedure SHA5_256UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA5_256UpdateXL';
  {-update context with Msg data}

procedure SHA5_256Final(var Context: THashContext; var Digest: TSHA5_256Digest);
stdcall;  external 'ch_dll.dll' name 'SHA5_256Final';
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalEx(var Context: THashContext; var Digest: THashDigest);
stdcall;  external 'ch_dll.dll' name 'SHA5_256FinalEx';
  {-finalize SHA512/256 calculation, clear context}

procedure SHA5_256FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA5_256FinalBitsEx';
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

procedure SHA5_256FinalBits(var Context: THashContext; var Digest: TSHA5_256Digest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'SHA5_256FinalBits';
  {-finalize SHA512/256 calculation with bitlen bits from BData (big-endian), clear context}

function  SHA5_256SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'SHA5_256SelfTest';
  {-self test for string from SHA512/256 document}

procedure SHA5_256Full(var Digest: TSHA5_256Digest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_256Full';
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256FullXL(var Digest: TSHA5_256Digest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'SHA5_256FullXL';
  {-SHA512/256 of Msg with init/update/final}

procedure SHA5_256File(const fname: Str255; var Digest: TSHA5_256Digest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'SHA5_256File';
  {-SHA512/256 of file, buf: buffer with at least bsize bytes}


{---------------------------------------------------------------------------}
procedure Whirl_Init(var Context: THashContext);
stdcall;  external 'ch_dll.dll' name 'Whirl_Init';
  {-initialize context}

procedure Whirl_Update(var Context: THashContext; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'Whirl_Update';
  {-update context with Msg data}

procedure Whirl_Final(var Context: THashContext; var Digest: TWhirlDigest);
stdcall;  external 'ch_dll.dll' name 'Whirl_Final';
  {-finalize Whirlpool calculation, clear context}

procedure Whirl_FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'Whirl_FinalBitsEx';
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}

procedure Whirl_FinalBits(var Context: THashContext; var Digest: TWhirlDigest; BData: byte; bitlen: integer);
stdcall;  external 'ch_dll.dll' name 'Whirl_FinalBits';
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}

function  Whirl_SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'Whirl_SelfTest';
  {-self test for strings from Whirlpool distribution}

procedure Whirl_Full(var Digest: TWhirlDigest; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'Whirl_Full';
  {-Whirlpool hash-code of Msg with init/update/final}

procedure Whirl_File(const fname: Str255; var Digest: TWhirlDigest; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'Whirl_File';
  {-Whirlpool hash-code of file, buf: buffer with at least bsize bytes}

procedure Whirl_UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'Whirl_UpdateXL';
  {-update context with Msg data}

procedure Whirl_FullXL(var Digest: TWhirlDigest; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'Whirl_FullXL';
  {-Whirlpool hash-code of Msg with init/update/final}


{---------------------------------------------------------------------------}
procedure hmac_MD5_init(var ctx: THMAC_Context; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_MD5_init';
  {-initialize HMAC context with key}

procedure hmac_MD5_inits(var ctx: THMAC_Context; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_MD5_inits';
  {-initialize HMAC context with skey}

procedure hmac_MD5_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_MD5_update';
  {-HMAC data input, may be called more than once}

procedure hmac_MD5_final(var ctx: THMAC_Context; var mac: TMD5Digest);
stdcall;  external 'ch_dll.dll' name 'hmac_MD5_final';
  {-end data input, calculate HMAC digest}

procedure hmac_MD5_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_MD5_updateXL';
  {-HMAC data input, may be called more than once}


{---------------------------------------------------------------------------}
procedure hmac_sha1_init(var ctx: THMAC_Context; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_sha1_init';
  {-initialize HMAC context with key}

procedure hmac_sha1_inits(var ctx: THMAC_Context; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_sha1_inits';
  {-initialize HMAC context with skey}

procedure hmac_sha1_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_sha1_update';
  {-HMAC data input, may be called more than once}

procedure hmac_sha1_final(var ctx: THMAC_Context; var mac: TSHA1Digest);
stdcall;  external 'ch_dll.dll' name 'hmac_sha1_final';
  {-end data input, calculate HMAC digest}

procedure hmac_sha1_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_sha1_updateXL';
  {-HMAC data input, may be called more than once}


{---------------------------------------------------------------------------}
procedure hmac_SHA256_init(var ctx: THMAC_Context; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA256_init';
  {-initialize HMAC context with key}

procedure hmac_SHA256_inits(var ctx: THMAC_Context; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA256_inits';
  {-initialize HMAC context with skey}

procedure hmac_SHA256_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA256_update';
  {-HMAC data input, may be called more than once}

procedure hmac_SHA256_final(var ctx: THMAC_Context; var mac: TSHA256Digest);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA256_final';
  {-end data input, calculate HMAC digest}

procedure hmac_SHA256_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA256_updateXL';
  {-HMAC data input, may be called more than once}


{---------------------------------------------------------------------------}
procedure hmac_SHA512_init(var ctx: THMAC_Context; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA512_init';
  {-initialize HMAC context with key}

procedure hmac_SHA512_inits(var ctx: THMAC_Context; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA512_inits';
  {-initialize HMAC context with skey}

procedure hmac_SHA512_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA512_update';
  {-HMAC data input, may be called more than once}

procedure hmac_SHA512_final(var ctx: THMAC_Context; var mac: TSHA512Digest);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA512_final';
  {-end data input, calculate HMAC digest}

procedure hmac_SHA512_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_SHA512_updateXL';
  {-HMAC data input, may be called more than once}


{---------------------------------------------------------------------------}
procedure hmac_Whirl_init(var ctx: THMAC_Context; key: pointer; klen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_Whirl_init';
  {-initialize HMAC context with key}

procedure hmac_Whirl_inits(var ctx: THMAC_Context; skey: Str255);
stdcall;  external 'ch_dll.dll' name 'hmac_Whirl_inits';
  {-initialize HMAC context with skey}

procedure hmac_Whirl_update(var ctx: THMAC_Context; data: pointer; dlen: word);
stdcall;  external 'ch_dll.dll' name 'hmac_Whirl_update';
  {-HMAC data input, may be called more than once}

procedure hmac_Whirl_final(var ctx: THMAC_Context; var mac: TWhirlDigest);
stdcall;  external 'ch_dll.dll' name 'hmac_Whirl_final';
  {-end data input, calculate HMAC digest}

procedure hmac_Whirl_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
stdcall;  external 'ch_dll.dll' name 'hmac_Whirl_updateXL';
  {-HMAC data input, may be called more than once}


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
stdcall;  external 'ch_dll.dll' name 'cm_CalcTab';
  {-Calculate CRC table from CRCPara, does nothing if CRCPara.width<8}

procedure cm_Create(const CRCPara: TCRCParam; ptab: PCRC32Tab; var ctx: TCRC_ctx);
stdcall;  external 'ch_dll.dll' name 'cm_Create';
  {-Create crc context from CRCPara, ptab may be nil}

procedure cm_Init(var ctx: TCRC_ctx);
stdcall;  external 'ch_dll.dll' name 'cm_Init';
  {-initialize context}

procedure cm_Update(var ctx: TCRC_ctx; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'cm_Update';
  {-update ctx with Msg data}

procedure cm_Final(var ctx: TCRC_ctx; var CRC: longint);
stdcall;  external 'ch_dll.dll' name 'cm_Final';
  {-finalize calculation and return CRC}

function  cm_SelfTest(const CRCPara: TCRCParam) : boolean;
stdcall;  external 'ch_dll.dll' name 'cm_SelfTest';
  {-Self test for CRCPara (no table version)}

procedure cm_Full(var ctx: TCRC_ctx; var CRC: longint; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'cm_Full';
  {-process Msg with init/update/final using ctx}

procedure cm_File(const fname: Str255; var ctx: TCRC_ctx; var CRC: longint; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'cm_File';
  {-CRC of file, buf: buffer with at least bsize bytes}

procedure cm_next(var ctx: TCRC_ctx; b: byte);
stdcall;  external 'ch_dll.dll' name 'cm_next';
  {-update ctx with data byte b}

function  cm_reflect(v: longint; b: integer): longint;
stdcall;  external 'ch_dll.dll' name 'cm_reflect';
  {-returns the reflected lowest b bits of v}

function  cm_combine(const para: TCRCParam; crc1, crc2: longint; len2: longint): longint;
stdcall;  external 'ch_dll.dll' name 'cm_combine';
  {-combine two CRCs calculated with para, i.e. if crc1 = CRC(m1) and}
  { crc2 = CRC(m2) then cm_combine = CRC(m1||m2); len2 = length(m2).}


{---------------------------------------------------------------------------}
type
  TSickCTX = packed record
               crc: word; {must be swapped for final result}
               prv: word; {temporary result for previous byte}
             end;

procedure CRC_Sick_Init(var ctx: TSickCTX);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_Init';
  {-initialize context}

procedure CRC_Sick_Update(var ctx: TSickCTX; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_Update';
  {-update CRC_Sick with Msg data}

function  CRC_Sick_Final(var ctx: TSickCTX): word;
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_Final';
  {-CRC_Sick: finalize calculation (dummy)}

function  CRC_Sick_SelfTest: boolean;
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_SelfTest';
  {-Self test for CRC_Sick with '123456789' and 'abcdefghijklmnopqrstuvwxyz'}

procedure CRC_Sick_Full(var CRC: word; Msg: pointer; Len: word);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_Full';
  {-CRC_Sick of Msg with init/update/final}

procedure CRC_Sick_File(const fname: Str255; var CRC: word; var buf; bsize: word; var Err: word);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_File';
  {-CRC_Sick of file, buf: buffer with at least bsize bytes}

procedure CRC_Sick_UpdateXL(var ctx: TSickCTX; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_UpdateXL';
  {-update CRC_Sick with Msg data}

procedure CRC_Sick_FullXL(var CRC: word; Msg: pointer; Len: longint);
stdcall;  external 'ch_dll.dll' name 'CRC_Sick_FullXL';
  {-CRC of Msg with init/update/final}


implementation

end.
