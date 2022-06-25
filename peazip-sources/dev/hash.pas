unit Hash;

{General Hash Unit: This unit defines the common types, functions, and
procedures. Via Hash descriptors and corresponding pointers, algorithms
can be searched by name or by ID. More important: all supported algorithms
can be used in the HMAC and KDF constructions.}


interface

(*************************************************************************

 DESCRIPTION     :  General hash unit: defines Algo IDs, digest types, etc

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  THashContext does not directly map the structure of the
                    context for SHA3 algorithms, a typecast with TSHA3State
                    from unit SHA3 should be used to access the fields.


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.01.06  W.Ehrhardt  Initial version
 0.11     15.01.06  we          FindHash_by_ID, $ifdef DLL: stdcall
 0.12     16.01.06  we          FindHash_by_Name
 0.13     18.01.06  we          Descriptor fields HAlgNum, HSig
 0.14     22.01.06  we          Removed HSelfTest from descriptor
 0.14     31.01.06  we          RIPEMD-160, C_MinHash, C_MaxHash
 0.15     11.02.06  we          Fields: HDSize, HVersion, HPtrOID, HLenOID
 0.16     02.08.06  we          Packed arrays
 0.17     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 0.18     07.08.06  we          C_HashVers = $00010002
 0.19     10.02.07  we          HashFile: no eof, XL and filemode via $ifdef
 0.20     18.02.07  we          MD4, C_HashVers = $00010003
 0.21     22.02.07  we          POID_Vec=^TOID_Vec, typed HPtrOID
 0.22     24.02.07  we          added some checks for HSig=C_HashSig
 0.23     04.10.07  we          THashContext.Index now longint
 0.24     02.05.08  we          type PHashDigest, function HashSameDigest
 0.25     04.05.08  we          BitAPI_Mask, BitAPI_PBit
 0.26     05.05.08  we          Descriptor with HFinalBit, C_HashVers=$00010004
 0.27     20.05.08  we          RMD160 as alias for RIPEMD160
 0.28     12.11.08  we          uses BTypes and Str255
 0.29     19.07.09  we          D12 fix: assign with typecast string(fname)
 0.30     08.03.12  we          SHA512/224 and SHA512/256, C_HashVers=$00010005
 0.31     10.03.12  we          HashFile: {$ifndef BIT16} instead of {$ifdef WIN32}

 0.32     08.08.18  we          New enlarged padded context, _SHA3_224 .. _SHA3_512
 0.33     08.08.18  we          THMacBuffer, assert HASHCTXSIZE
 0.34     16.08.15  we          Removed $ifdef DLL / stdcall

 0.35     15.05.17  we          Changes for Blake2s
 0.36     16.05.17  we          MaxOIDLen = 11 and MaxC_HashVers = $00020002

 0.37     03.11.17  we          TBlake2B_384/512Digest

 0.38     29.11.17  we          HashFile - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2006-2017 Wolfgang Ehrhardt

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

uses
  BTypes;

type
  THashAlgorithm = (_MD4, _MD5, _RIPEMD160, _SHA1,
                    _SHA224, _SHA256, _SHA384, _SHA512,
                    _Whirlpool, _SHA512_224, _SHA512_256,
                    _SHA3_224, _SHA3_256, _SHA3_384, _SHA3_512,
                    _Blake2S_224, _Blake2S_256,
                    _Blake2B_384, _Blake2B_512); {Supported hash algorithms}

const
  _RMD160  = _RIPEMD160;      {Alias}

const
  MaxBlockLen  = 128;         {Max. block length (buffer size), multiple of 4}
  MaxDigestLen = 64;          {Max. length of hash digest}
  MaxStateLen  = 16;          {Max. size of internal state}
  MaxOIDLen    = 11;          {Current max. OID length}
  C_HashSig    = $3D7A;       {Signature for Hash descriptor}
  C_HashVers   = $00020002;   {Version of Hash definitions}
  C_MinHash    = _MD4;        {Lowest  hash in THashAlgorithm}
  C_MaxHash    = _Blake2B_512;{Highest hash in THashAlgorithm}

type
  THashState   = packed array[0..MaxStateLen-1] of longint;         {Internal state}
  THashBuffer  = packed array[0..MaxBlockLen-1] of byte;            {hash buffer block}
  THashDigest  = packed array[0..MaxDigestLen-1] of byte;           {hash digest}
  PHashDigest  = ^THashDigest;                                      {pointer to hash digest}
  THashBuf32   = packed array[0..MaxBlockLen  div 4 -1] of longint; {type cast helper}
  THashDig32   = packed array[0..MaxDigestLen div 4 -1] of longint; {type cast helper}
  THMacBuffer  = packed array[0..143] of byte;                      {hmac buffer block}

const
  HASHCTXSIZE  = 448;  {Common size of enlarged padded old context}
                       {and new padded SHA3/SHAKE/Keccak context  }

type
  THashContext = packed record
                   Hash  : THashState;             {Working hash}
                   MLen  : packed array[0..3] of longint; {max 128 bit msg length}
                   Buffer: THashBuffer;            {Block buffer}
                   Index : longint;                {Index in buffer}
                   Fill2 : packed array[213..HASHCTXSIZE] of byte;
                 end;

type
  TMD4Digest       = packed array[0..15] of byte;  {MD4    digest    }
  TMD5Digest       = packed array[0..15] of byte;  {MD5    digest    }
  TRMD160Digest    = packed array[0..19] of byte;  {RMD160 digest    }
  TSHA1Digest      = packed array[0..19] of byte;  {SHA1   digest    }
  TSHA224Digest    = packed array[0..27] of byte;  {SHA224 digest    }
  TSHA256Digest    = packed array[0..31] of byte;  {SHA256 digest    }
  TSHA384Digest    = packed array[0..47] of byte;  {SHA384 digest    }
  TSHA512Digest    = packed array[0..63] of byte;  {SHA512 digest    }
  TSHA5_224Digest  = packed array[0..27] of byte;  {SHA512/224 digest}
  TSHA5_256Digest  = packed array[0..31] of byte;  {SHA512/256 digest}
  TWhirlDigest     = packed array[0..63] of byte;  {Whirlpool digest }
  TSHA3_224Digest  = packed array[0..27] of byte;  {SHA3_224 digest  }
  TSHA3_256Digest  = packed array[0..31] of byte;  {SHA3_256 digest  }
  TSHA3_384Digest  = packed array[0..47] of byte;  {SHA3_384 digest  }
  TSHA3_512Digest  = packed array[0..63] of byte;  {SHA3_512 digest  }
  TBlake2S_224Digest = packed array[0..27] of byte;  {Blake2S digest }
  TBlake2S_256Digest = packed array[0..31] of byte;  {Blake2S digest }
  TBlake2B_384Digest = packed array[0..47] of byte;  {Blake2B-384 digest}
  TBlake2B_512Digest = packed array[0..63] of byte;  {Blake2B-512 digest}


type
  HashInitProc     = procedure(var Context: THashContext);
                      {-initialize context}

  HashUpdateXLProc = procedure(var Context: THashContext; Msg: pointer; Len: longint);
                      {-update context with Msg data}

  HashFinalProc    = procedure(var Context: THashContext; var Digest: THashDigest);
                      {-finalize calculation, clear context}

  HashFinalBitProc = procedure(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
                      {-finalize calculation with bitlen bits from BData, clear context}

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
                HBlockLen : word;              {Blocklength of hash, rate div 8 for SHA3}
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


const
  BitAPI_Mask: array[0..7] of byte = ($00,$80,$C0,$E0,$F0,$F8,$FC,$FE);
  BitAPI_PBit: array[0..7] of byte = ($80,$40,$20,$10,$08,$04,$02,$01);

procedure RegisterHash(AlgId: THashAlgorithm; PHash: PHashDesc);
  {-Register algorithm with AlgID and Hash descriptor PHash^}

function  FindHash_by_ID(AlgoID: THashAlgorithm): PHashDesc;
  {-Return PHashDesc of AlgoID, nil if not found/registered}

function  FindHash_by_Name(AlgoName: THashName): PHashDesc;
  {-Return PHashDesc of Algo with AlgoName, nil if not found/registered}

procedure HashFile({$ifdef CONST} const {$endif} fname: string; PHash: PHashDesc;
                    var Digest: THashDigest; var buf; bsize: word; var Err: word);
  {-Calculate hash digest of file, buf: buffer with at least bsize bytes}

procedure HashUpdate(PHash: PHashDesc; var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure HashFullXL(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: longint);
  {-Calulate hash digest of Msg with init/update/final}

procedure HashFull(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: word);
  {-Calulate hash digest of Msg with init/update/final}

function  HashSameDigest(PHash: PHashDesc; PD1, PD2: PHashDigest): boolean;
  {-Return true if same digests, using HDigestlen of PHash}


implementation


var
  PHashVec : array[THashAlgorithm] of PHashDesc;
             {Hash descriptor pointers of all defined hash algorithms}

{---------------------------------------------------------------------------}
procedure RegisterHash(AlgId: THashAlgorithm; PHash: PHashDesc);
  {-Register algorithm with AlgID and Hash descriptor PHash^}
begin
  if (PHash<>nil) and
     (PHash^.HAlgNum=longint(AlgId)) and
     (PHash^.HSig=C_HashSig) and
     (PHash^.HDVersion=C_HashVers) and
     (PHash^.HDSize=sizeof(THashDesc)) then PHashVec[AlgId] := PHash;
end;


{---------------------------------------------------------------------------}
function FindHash_by_ID(AlgoID: THashAlgorithm): PHashDesc;
  {-Return PHashDesc of AlgoID, nil if not found/registered}
var
  p: PHashDesc;
  A: longint;
begin
  A := longint(AlgoID);
  FindHash_by_ID := nil;
  if (A>=ord(C_MinHash)) and (A<=ord(C_MaxHash)) then begin
    p := PHashVec[AlgoID];
    if (p<>nil) and (p^.HSig=C_HashSig) and (p^.HAlgNum=A) then FindHash_by_ID := p;
  end;
end;


{---------------------------------------------------------------------------}
function  FindHash_by_Name(AlgoName: THashName): PHashDesc;
  {-Return PHashDesc of Algo with AlgoName, nil if not found/registered}
var
  algo : THashAlgorithm;
  phash: PHashDesc;

  function StrUpcase(s: THashName): THashName;
    {-Upcase for strings}
  var
    i: integer;
  begin
    for i:=1 to length(s) do s[i] := upcase(s[i]);
    StrUpcase := s;
  end;

begin
  AlgoName := StrUpcase(Algoname);
  {Transform RMD160 alias to standard name}
  if AlgoName='RMD160' then AlgoName:='RIPEMD160';
  FindHash_by_Name := nil;
  for algo := C_MinHash to C_MaxHash do begin
    phash := PHashVec[algo];
    if (phash<>nil) and (AlgoName=StrUpcase(phash^.HName))
      and (phash^.HSig=C_HashSig) and (phash^.HAlgNum=longint(algo))
    then begin
      FindHash_by_Name := phash;
      exit;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure HashUpdate(PHash: PHashDesc; var Context: THashContext; Msg: pointer; Len: word);
  {-update context with Msg data}
begin
  if PHash<>nil then with PHash^ do begin
    if HSig=C_HashSig then HUpdateXL(Context, Msg, Len);
  end;
end;


{---------------------------------------------------------------------------}
procedure HashFullXL(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: longint);
  {-Calulate hash digest of Msg with init/update/final}
var
  Context: THashContext;
begin
  if PHash<>nil then with PHash^ do begin
    if HSig=C_HashSig then begin
      HInit(Context);
      HUpdateXL(Context, Msg, Len);
      HFinal(Context, Digest);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure HashFull(PHash: PHashDesc; var Digest: THashDigest; Msg: pointer; Len: word);
  {-Calulate hash digest of Msg with init/update/final}
begin
  {test PHash<>nil in HashFullXL}
  HashFullXL(PHash, Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
function HashSameDigest(PHash: PHashDesc; PD1, PD2: PHashDigest): boolean;
  {-Return true if same digests, using HDigestlen of PHash}
var
  i: integer;
begin
  HashSameDigest := false;
  if PHash<>nil then with PHash^ do begin
    if (HSig=C_HashSig) and (HDigestlen>0) then begin
      for i:=0 to pred(HDigestlen) do begin
        if PD1^[i]<>PD2^[i] then exit;
      end;
      HashSameDigest := true;
    end;
  end;
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure HashFile({$ifdef CONST} const {$endif} fname: string; PHash: PHashDesc;
                    var Digest: THashDigest; var buf; bsize: word; var Err: word);
  {-Calculate hash digest of file, buf: buffer with at least bsize bytes}
var
  {$ifdef VirtualPascal}
    fms: word;
  {$else}
    fms: byte;
  {$endif}
  {$ifndef BIT16}
    L: longint;
  {$else}
    L: word;
  {$endif}
var
  Context: THashContext;
  f: file;
begin
  if (PHash=nil) or (Phash^.HSig<>C_HashSig) then begin
    Err := 204; {Invalid pointer}
    exit;
  end;
  fms := FileMode;
  {$ifdef VirtualPascal}
    FileMode := $40; {open_access_ReadOnly or open_share_DenyNone;}
  {$else}
    FileMode := 0;
  {$endif}
  system.assign(f,{$ifdef D12Plus} string {$endif} (fname));
  system.reset(f,1);
  Err := IOResult;
  FileMode := fms;
  if Err<>0 then exit;
  with PHash^ do begin
    HInit(Context);
    L := bsize;
    while (Err=0) and (L=bsize) do begin
      system.blockread(f,buf,bsize,L);
      Err := IOResult;
      HUpdateXL(Context, @buf, L);
    end;
    system.close(f);
    if IOResult=0 then {};
    HFinal(Context, Digest);
  end;
end;


begin
{$ifdef HAS_ASSERT}
  assert(sizeof(THashContext)=HASHCTXSIZE , '** Invalid sizeof(THashContext)');
{$else}
  if sizeof(THashContext)<>HASHCTXSIZE then RunError(227);
{$endif}
  {Paranoia: initialize all descriptor pointers to nil (should}
  {be done by compiler/linker because array is in global data)}
  fillchar(PHashVec,sizeof(PHashVec),0);
end.
