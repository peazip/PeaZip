unit Whirl512;

{Whirlpool - 512 bit Secure Hash Function}


interface

(*************************************************************************

 DESCRIPTION     :  Whirlpool - 512 bit Secure Hash Function

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  about 8KB tables in data segment

 DISPLAY MODE    :  ---

 REMARKS         :  Message bit length is limited to "only" 2^128

 REFERENCES      :  [1] Whirlpool.c Version 3.0 (2003.03.12) by
                        Paulo S.L.M. Barreto / Vincent Rijmen available in
                        http://www.larc.usp.br/~pbarreto/whirlpool.zip

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     08.12.05  W.Ehrhardt  Initial BP7 using SHA512 layout
 0.11     08.12.05  we          ISO_WHIRLPOOL: switch between V3.0 and ISO-Draft(=V2.1)
 0.12     08.12.05  we          TP5-6
 0.13     09.12.05  we          Tables C0...C7
 0.14     09.12.05  we          Tables C0L, C0H, ... C7H,C7H
 0.15     09.12.05  we          Tables CxL as absolute aliases
 0.16     09.12.05  we          Transform now global proc
 0.17     09.12.05  we          Byte access in 16 Bit Transform
 0.18     09.12.05  we          Tables: CxH -> Cx, CxL -> Cy (y=x+4 mod 8)
 0.19     09.12.05  we          BIT16: local copy in Transform, 20% faster
 0.20     10.12.05  we          const ISO_Whirl, BASM16
 0.21     10.12.05  we          BASM16: combine L[2x], L[2x+1] calculations
 0.22     11.12.05  we          BASM16: XorBlock
 0.23     11.12.05  we          Whirl_is_ISO, Whirl_UpdateXL interfaced if WIN32
 0.24     11.12.05  we          Force $I- in Whirl_File
 0.25     17.12.05  we          $ifdef DOUBLE_ROUNDS
 0.26     22.12.05  we          Keep only Final V3.0, no INC for tables
 0.27     15.01.06  we          uses Hash unit and THashDesc
 0.28     15.01.06  we          THState8
 0.29     18.01.06  we          Descriptor fields HAlgNum, HSig
 0.30     22.01.06  we          Removed HSelfTest from descriptor
 0.31     11.02.06  we          Descriptor as typed const
 0.32     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 0.33     21.01.07  we          Bugfix for message bit lengths >= 2^32 (reported by Nicola Lugato)
 0.34     22.02.07  we          values for OID vector
 0.35     30.06.07  we          Use conditional define FPC_ProcVar
 0.36     04.10.07  we          FPC: {$asmmode intel}
 0.37     03.05.08  we          Bit-API: Whirl_FinalBits/Ex
 0.38     05.05.08  we          THashDesc constant with HFinalBit field
 0.39     07.05.08  we          Selftest; 1 Zero bit from nessie-test-vectors.txt
 0.40     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 0.41     18.12.10  we          Updated link
 0.42     26.12.12  we          D17 and PurePascal
 0.43     27.12.12  we          Removed symbols DOUBLE_ROUNDS and USE_SHR
 0.44     16.08.15  we          Removed $ifdef DLL / stdcall
 0.45     15.05.17  we          adjust OID to new MaxOIDLen
 0.46     29.11.17  we          Whirl_File - fname: string
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2005-2017 Wolfgang Ehrhardt

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

{$ifdef BIT64}
  {$ifndef PurePascal}
    {$define PurePascal}
  {$endif}
{$endif}

{$ifdef PurePascal}
  {$define UseInt64}
{$else}
  {$ifdef D4Plus}
    {$define UseInt64}
  {$endif}
  {$ifdef FPC}
    {$define UseInt64}
  {$endif}
{$endif}


uses
  BTypes,Hash;


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

procedure Whirl_File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TWhirlDigest; var buf; bsize: word; var Err: word);
  {-Whirlpool hash-code of file, buf: buffer with at least bsize bytes}


implementation

{$ifdef BIT16}
  {$F-}
{$endif}

const
  Whirl_BlockLen = 64;

type
  TWhirlTab  = array[0..255] of longint;
  THState8   = array[0..sizeof(THashState)-1] of byte;
  PHashState = ^THashState;

{1.0.10118.3.0.55}
{iso(1) standard(0) hash-functions(10118) part3(3) algorithm(0) whirlpool(55)}
const
  Whirl_OID : TOID_Vec = (1,0,10118,3,0,55,-1,-1,-1,-1,-1); {Len=6}


{$ifndef VER5X}
const
  Whirl_Desc: THashDesc = (
               HSig      : C_HashSig;
               HDSize    : sizeof(THashDesc);
               HDVersion : C_HashVers;
               HBlockLen : Whirl_BlockLen;
               HDigestlen: sizeof(TWhirlDigest);
             {$ifdef FPC_ProcVar}
               HInit     : @Whirl_Init;
               HFinal    : @Whirl_FinalEx;
               HUpdateXL : @Whirl_UpdateXL;
             {$else}
               HInit     : Whirl_Init;
               HFinal    : Whirl_FinalEx;
               HUpdateXL : Whirl_UpdateXL;
             {$endif}
               HAlgNum   : longint(_Whirlpool);
               HName     : 'Whirlpool';
               HPtrOID   : @Whirl_OID;
               HLenOID   : 6;
               HFill     : 0;
             {$ifdef FPC_ProcVar}
               HFinalBit : @Whirl_FinalBitsEx;
             {$else}
               HFinalBit : Whirl_FinalBitsEx;
             {$endif}
               HReserved : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            );
{$else}
var
  Whirl_Desc: THashDesc;
{$endif}


{Though Whirlpool is endianness-neutral, the encryption tables are listed}
{in Little-Endian format, which is adopted throughout this implementation}

{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

const
  {Round constants in Little-Endian format high, low, high, low, ...}
  RC: array[0..19] of longint = (
        $e8c62318, $4f01b887, $f5d2a636, $52916f79, $8e9bbc60, $357b0ca3, $c2d7e01d,
        $57fe4b2e, $e5377715, $da4af09f, $0a29c958, $856ba0b1, $f4105dbd, $67053ecb,
        $8b4127e4, $d8957da7, $667ceefb, $9e4717dd, $07bf2dca, $33835aad);

  {Whirlpool Version 3.0 encryption tables (highest 32 bits)}
  C0: TWhirlTab = (
        $18601818, $238c2323, $c63fc6c6, $e887e8e8, $87268787, $b8dab8b8, $01040101, $4f214f4f,
        $36d83636, $a6a2a6a6, $d26fd2d2, $f5f3f5f5, $79f97979, $6fa16f6f, $917e9191, $52555252,
        $609d6060, $bccabcbc, $9b569b9b, $8e028e8e, $a3b6a3a3, $0c300c0c, $7bf17b7b, $35d43535,
        $1d741d1d, $e0a7e0e0, $d77bd7d7, $c22fc2c2, $2eb82e2e, $4b314b4b, $fedffefe, $57415757,
        $15541515, $77c17777, $37dc3737, $e5b3e5e5, $9f469f9f, $f0e7f0f0, $4a354a4a, $da4fdada,
        $587d5858, $c903c9c9, $29a42929, $0a280a0a, $b1feb1b1, $a0baa0a0, $6bb16b6b, $852e8585,
        $bdcebdbd, $5d695d5d, $10401010, $f4f7f4f4, $cb0bcbcb, $3ef83e3e, $05140505, $67816767,
        $e4b7e4e4, $279c2727, $41194141, $8b168b8b, $a7a6a7a7, $7de97d7d, $956e9595, $d847d8d8,
        $fbcbfbfb, $ee9feeee, $7ced7c7c, $66856666, $dd53dddd, $175c1717, $47014747, $9e429e9e,
        $ca0fcaca, $2db42d2d, $bfc6bfbf, $071c0707, $ad8eadad, $5a755a5a, $83368383, $33cc3333,
        $63916363, $02080202, $aa92aaaa, $71d97171, $c807c8c8, $19641919, $49394949, $d943d9d9,
        $f2eff2f2, $e3abe3e3, $5b715b5b, $881a8888, $9a529a9a, $26982626, $32c83232, $b0fab0b0,
        $e983e9e9, $0f3c0f0f, $d573d5d5, $803a8080, $bec2bebe, $cd13cdcd, $34d03434, $483d4848,
        $ffdbffff, $7af57a7a, $907a9090, $5f615f5f, $20802020, $68bd6868, $1a681a1a, $ae82aeae,
        $b4eab4b4, $544d5454, $93769393, $22882222, $648d6464, $f1e3f1f1, $73d17373, $12481212,
        $401d4040, $08200808, $c32bc3c3, $ec97ecec, $db4bdbdb, $a1bea1a1, $8d0e8d8d, $3df43d3d,
        $97669797, $00000000, $cf1bcfcf, $2bac2b2b, $76c57676, $82328282, $d67fd6d6, $1b6c1b1b,
        $b5eeb5b5, $af86afaf, $6ab56a6a, $505d5050, $45094545, $f3ebf3f3, $30c03030, $ef9befef,
        $3ffc3f3f, $55495555, $a2b2a2a2, $ea8feaea, $65896565, $bad2baba, $2fbc2f2f, $c027c0c0,
        $de5fdede, $1c701c1c, $fdd3fdfd, $4d294d4d, $92729292, $75c97575, $06180606, $8a128a8a,
        $b2f2b2b2, $e6bfe6e6, $0e380e0e, $1f7c1f1f, $62956262, $d477d4d4, $a89aa8a8, $96629696,
        $f9c3f9f9, $c533c5c5, $25942525, $59795959, $842a8484, $72d57272, $39e43939, $4c2d4c4c,
        $5e655e5e, $78fd7878, $38e03838, $8c0a8c8c, $d163d1d1, $a5aea5a5, $e2afe2e2, $61996161,
        $b3f6b3b3, $21842121, $9c4a9c9c, $1e781e1e, $43114343, $c73bc7c7, $fcd7fcfc, $04100404,
        $51595151, $995e9999, $6da96d6d, $0d340d0d, $facffafa, $df5bdfdf, $7ee57e7e, $24902424,
        $3bec3b3b, $ab96abab, $ce1fcece, $11441111, $8f068f8f, $4e254e4e, $b7e6b7b7, $eb8bebeb,
        $3cf03c3c, $813e8181, $946a9494, $f7fbf7f7, $b9deb9b9, $134c1313, $2cb02c2c, $d36bd3d3,
        $e7bbe7e7, $6ea56e6e, $c437c4c4, $030c0303, $56455656, $440d4444, $7fe17f7f, $a99ea9a9,
        $2aa82a2a, $bbd6bbbb, $c123c1c1, $53515353, $dc57dcdc, $0b2c0b0b, $9d4e9d9d, $6cad6c6c,
        $31c43131, $74cd7474, $f6fff6f6, $46054646, $ac8aacac, $891e8989, $14501414, $e1a3e1e1,
        $16581616, $3ae83a3a, $69b96969, $09240909, $70dd7070, $b6e2b6b6, $d067d0d0, $ed93eded,
        $cc17cccc, $42154242, $985a9898, $a4aaa4a4, $28a02828, $5c6d5c5c, $f8c7f8f8, $86228686) ;

  C1: TWhirlTab = (
        $601818d8, $8c232326, $3fc6c6b8, $87e8e8fb, $268787cb, $dab8b811, $04010109, $214f4f0d,
        $d836369b, $a2a6a6ff, $6fd2d20c, $f3f5f50e, $f9797996, $a16f6f30, $7e91916d, $555252f8,
        $9d606047, $cabcbc35, $569b9b37, $028e8e8a, $b6a3a3d2, $300c0c6c, $f17b7b84, $d4353580,
        $741d1df5, $a7e0e0b3, $7bd7d721, $2fc2c29c, $b82e2e43, $314b4b29, $dffefe5d, $415757d5,
        $541515bd, $c17777e8, $dc373792, $b3e5e59e, $469f9f13, $e7f0f023, $354a4a20, $4fdada44,
        $7d5858a2, $03c9c9cf, $a429297c, $280a0a5a, $feb1b150, $baa0a0c9, $b16b6b14, $2e8585d9,
        $cebdbd3c, $695d5d8f, $40101090, $f7f4f407, $0bcbcbdd, $f83e3ed3, $1405052d, $81676778,
        $b7e4e497, $9c272702, $19414173, $168b8ba7, $a6a7a7f6, $e97d7db2, $6e959549, $47d8d856,
        $cbfbfb70, $9feeeecd, $ed7c7cbb, $85666671, $53dddd7b, $5c1717af, $01474745, $429e9e1a,
        $0fcacad4, $b42d2d58, $c6bfbf2e, $1c07073f, $8eadadac, $755a5ab0, $368383ef, $cc3333b6,
        $9163635c, $08020212, $92aaaa93, $d97171de, $07c8c8c6, $641919d1, $3949493b, $43d9d95f,
        $eff2f231, $abe3e3a8, $715b5bb9, $1a8888bc, $529a9a3e, $9826260b, $c83232bf, $fab0b059,
        $83e9e9f2, $3c0f0f77, $73d5d533, $3a8080f4, $c2bebe27, $13cdcdeb, $d0343489, $3d484832,
        $dbffff54, $f57a7a8d, $7a909064, $615f5f9d, $8020203d, $bd68680f, $681a1aca, $82aeaeb7,
        $eab4b47d, $4d5454ce, $7693937f, $8822222f, $8d646463, $e3f1f12a, $d17373cc, $48121282,
        $1d40407a, $20080848, $2bc3c395, $97ececdf, $4bdbdb4d, $bea1a1c0, $0e8d8d91, $f43d3dc8,
        $6697975b, $00000000, $1bcfcff9, $ac2b2b6e, $c57676e1, $328282e6, $7fd6d628, $6c1b1bc3,
        $eeb5b574, $86afafbe, $b56a6a1d, $5d5050ea, $09454557, $ebf3f338, $c03030ad, $9befefc4,
        $fc3f3fda, $495555c7, $b2a2a2db, $8feaeae9, $8965656a, $d2baba03, $bc2f2f4a, $27c0c08e,
        $5fdede60, $701c1cfc, $d3fdfd46, $294d4d1f, $72929276, $c97575fa, $18060636, $128a8aae,
        $f2b2b24b, $bfe6e685, $380e0e7e, $7c1f1fe7, $95626255, $77d4d43a, $9aa8a881, $62969652,
        $c3f9f962, $33c5c5a3, $94252510, $795959ab, $2a8484d0, $d57272c5, $e43939ec, $2d4c4c16,
        $655e5e94, $fd78789f, $e03838e5, $0a8c8c98, $63d1d117, $aea5a5e4, $afe2e2a1, $9961614e,
        $f6b3b342, $84212134, $4a9c9c08, $781e1eee, $11434361, $3bc7c7b1, $d7fcfc4f, $10040424,
        $595151e3, $5e999925, $a96d6d22, $340d0d65, $cffafa79, $5bdfdf69, $e57e7ea9, $90242419,
        $ec3b3bfe, $96abab9a, $1fcecef0, $44111199, $068f8f83, $254e4e04, $e6b7b766, $8bebebe0,
        $f03c3cc1, $3e8181fd, $6a949440, $fbf7f71c, $deb9b918, $4c13138b, $b02c2c51, $6bd3d305,
        $bbe7e78c, $a56e6e39, $37c4c4aa, $0c03031b, $455656dc, $0d44445e, $e17f7fa0, $9ea9a988,
        $a82a2a67, $d6bbbb0a, $23c1c187, $515353f1, $57dcdc72, $2c0b0b53, $4e9d9d01, $ad6c6c2b,
        $c43131a4, $cd7474f3, $fff6f615, $0546464c, $8aacaca5, $1e8989b5, $501414b4, $a3e1e1ba,
        $581616a6, $e83a3af7, $b9696906, $24090941, $dd7070d7, $e2b6b66f, $67d0d01e, $93ededd6,
        $17cccce2, $15424268, $5a98982c, $aaa4a4ed, $a0282875, $6d5c5c86, $c7f8f86b, $228686c2);

  C2: TWhirlTab = (
        $1818d830, $23232646, $c6c6b891, $e8e8fbcd, $8787cb13, $b8b8116d, $01010902, $4f4f0d9e,
        $36369b6c, $a6a6ff51, $d2d20cb9, $f5f50ef7, $797996f2, $6f6f30de, $91916d3f, $5252f8a4,
        $606047c0, $bcbc3565, $9b9b372b, $8e8e8a01, $a3a3d25b, $0c0c6c18, $7b7b84f6, $3535806a,
        $1d1df53a, $e0e0b3dd, $d7d721b3, $c2c29c99, $2e2e435c, $4b4b2996, $fefe5de1, $5757d5ae,
        $1515bd2a, $7777e8ee, $3737926e, $e5e59ed7, $9f9f1323, $f0f023fd, $4a4a2094, $dada44a9,
        $5858a2b0, $c9c9cf8f, $29297c52, $0a0a5a14, $b1b1507f, $a0a0c95d, $6b6b14d6, $8585d917,
        $bdbd3c67, $5d5d8fba, $10109020, $f4f407f5, $cbcbdd8b, $3e3ed37c, $05052d0a, $676778ce,
        $e4e497d5, $2727024e, $41417382, $8b8ba70b, $a7a7f653, $7d7db2fa, $95954937, $d8d856ad,
        $fbfb70eb, $eeeecdc1, $7c7cbbf8, $666671cc, $dddd7ba7, $1717af2e, $4747458e, $9e9e1a21,
        $cacad489, $2d2d585a, $bfbf2e63, $07073f0e, $adadac47, $5a5ab0b4, $8383ef1b, $3333b666,
        $63635cc6, $02021204, $aaaa9349, $7171dee2, $c8c8c68d, $1919d132, $49493b92, $d9d95faf,
        $f2f231f9, $e3e3a8db, $5b5bb9b6, $8888bc0d, $9a9a3e29, $26260b4c, $3232bf64, $b0b0597d,
        $e9e9f2cf, $0f0f771e, $d5d533b7, $8080f41d, $bebe2761, $cdcdeb87, $34348968, $48483290,
        $ffff54e3, $7a7a8df4, $9090643d, $5f5f9dbe, $20203d40, $68680fd0, $1a1aca34, $aeaeb741,
        $b4b47d75, $5454cea8, $93937f3b, $22222f44, $646463c8, $f1f12aff, $7373cce6, $12128224,
        $40407a80, $08084810, $c3c3959b, $ececdfc5, $dbdb4dab, $a1a1c05f, $8d8d9107, $3d3dc87a,
        $97975b33, $00000000, $cfcff983, $2b2b6e56, $7676e1ec, $8282e619, $d6d628b1, $1b1bc336,
        $b5b57477, $afafbe43, $6a6a1dd4, $5050eaa0, $4545578a, $f3f338fb, $3030ad60, $efefc4c3,
        $3f3fda7e, $5555c7aa, $a2a2db59, $eaeae9c9, $65656aca, $baba0369, $2f2f4a5e, $c0c08e9d,
        $dede60a1, $1c1cfc38, $fdfd46e7, $4d4d1f9a, $92927639, $7575faea, $0606360c, $8a8aae09,
        $b2b24b79, $e6e685d1, $0e0e7e1c, $1f1fe73e, $626255c4, $d4d43ab5, $a8a8814d, $96965231,
        $f9f962ef, $c5c5a397, $2525104a, $5959abb2, $8484d015, $7272c5e4, $3939ec72, $4c4c1698,
        $5e5e94bc, $78789ff0, $3838e570, $8c8c9805, $d1d117bf, $a5a5e457, $e2e2a1d9, $61614ec2,
        $b3b3427b, $21213442, $9c9c0825, $1e1eee3c, $43436186, $c7c7b193, $fcfc4fe5, $04042408,
        $5151e3a2, $9999252f, $6d6d22da, $0d0d651a, $fafa79e9, $dfdf69a3, $7e7ea9fc, $24241948,
        $3b3bfe76, $abab9a4b, $cecef081, $11119922, $8f8f8303, $4e4e049c, $b7b76673, $ebebe0cb,
        $3c3cc178, $8181fd1f, $94944035, $f7f71cf3, $b9b9186f, $13138b26, $2c2c5158, $d3d305bb,
        $e7e78cd3, $6e6e39dc, $c4c4aa95, $03031b06, $5656dcac, $44445e88, $7f7fa0fe, $a9a9884f,
        $2a2a6754, $bbbb0a6b, $c1c1879f, $5353f1a6, $dcdc72a5, $0b0b5316, $9d9d0127, $6c6c2bd8,
        $3131a462, $7474f3e8, $f6f615f1, $46464c8c, $acaca545, $8989b50f, $1414b428, $e1e1badf,
        $1616a62c, $3a3af774, $696906d2, $09094112, $7070d7e0, $b6b66f71, $d0d01ebd, $ededd6c7,
        $cccce285, $42426884, $98982c2d, $a4a4ed55, $28287550, $5c5c86b8, $f8f86bed, $8686c211);

  C3: TWhirlTab = (
        $18d83078, $232646af, $c6b891f9, $e8fbcd6f, $87cb13a1, $b8116d62, $01090205, $4f0d9e6e,
        $369b6cee, $a6ff5104, $d20cb9bd, $f50ef706, $7996f280, $6f30dece, $916d3fef, $52f8a407,
        $6047c0fd, $bc356576, $9b372bcd, $8e8a018c, $a3d25b15, $0c6c183c, $7b84f68a, $35806ae1,
        $1df53a69, $e0b3dd47, $d721b3ac, $c29c99ed, $2e435c96, $4b29967a, $fe5de121, $57d5ae16,
        $15bd2a41, $77e8eeb6, $37926eeb, $e59ed756, $9f1323d9, $f023fd17, $4a20947f, $da44a995,
        $58a2b025, $c9cf8fca, $297c528d, $0a5a1422, $b1507f4f, $a0c95d1a, $6b14d6da, $85d917ab,
        $bd3c6773, $5d8fba34, $10902050, $f407f503, $cbdd8bc0, $3ed37cc6, $052d0a11, $6778cee6,
        $e497d553, $27024ebb, $41738258, $8ba70b9d, $a7f65301, $7db2fa94, $954937fb, $d856ad9f,
        $fb70eb30, $eecdc171, $7cbbf891, $6671cce3, $dd7ba78e, $17af2e4b, $47458e46, $9e1a21dc,
        $cad489c5, $2d585a99, $bf2e6379, $073f0e1b, $adac4723, $5ab0b42f, $83ef1bb5, $33b666ff,
        $635cc6f2, $0212040a, $aa934938, $71dee2a8, $c8c68dcf, $19d1327d, $493b9270, $d95faf9a,
        $f231f91d, $e3a8db48, $5bb9b62a, $88bc0d92, $9a3e29c8, $260b4cbe, $32bf64fa, $b0597d4a,
        $e9f2cf6a, $0f771e33, $d533b7a6, $80f41dba, $be27617c, $cdeb87de, $348968e4, $48329075,
        $ff54e324, $7a8df48f, $90643dea, $5f9dbe3e, $203d40a0, $680fd0d5, $1aca3472, $aeb7412c,
        $b47d755e, $54cea819, $937f3be5, $222f44aa, $6463c8e9, $f12aff12, $73cce6a2, $1282245a,
        $407a805d, $08481028, $c3959be8, $ecdfc57b, $db4dab90, $a1c05f1f, $8d910783, $3dc87ac9,
        $975b33f1, $00000000, $cff983d4, $2b6e5687, $76e1ecb3, $82e619b0, $d628b1a9, $1bc33677,
        $b574775b, $afbe4329, $6a1dd4df, $50eaa00d, $45578a4c, $f338fb18, $30ad60f0, $efc4c374,
        $3fda7ec3, $55c7aa1c, $a2db5910, $eae9c965, $656acaec, $ba036968, $2f4a5e93, $c08e9de7,
        $de60a181, $1cfc386c, $fd46e72e, $4d1f9a64, $927639e0, $75faeabc, $06360c1e, $8aae0998,
        $b24b7940, $e685d159, $0e7e1c36, $1fe73e63, $6255c4f7, $d43ab5a3, $a8814d32, $965231f4,
        $f962ef3a, $c5a397f6, $25104ab1, $59abb220, $84d015ae, $72c5e4a7, $39ec72dd, $4c169861,
        $5e94bc3b, $789ff085, $38e570d8, $8c980586, $d117bfb2, $a5e4570b, $e2a1d94d, $614ec2f8,
        $b3427b45, $213442a5, $9c0825d6, $1eee3c66, $43618652, $c7b193fc, $fc4fe52b, $04240814,
        $51e3a208, $99252fc7, $6d22dac4, $0d651a39, $fa79e935, $df69a384, $7ea9fc9b, $241948b4,
        $3bfe76d7, $ab9a4b3d, $cef081d1, $11992255, $8f830389, $4e049c6b, $b7667351, $ebe0cb60,
        $3cc178cc, $81fd1fbf, $944035fe, $f71cf30c, $b9186f67, $138b265f, $2c51589c, $d305bbb8,
        $e78cd35c, $6e39dccb, $c4aa95f3, $031b060f, $56dcac13, $445e8849, $7fa0fe9e, $a9884f37,
        $2a675482, $bb0a6b6d, $c1879fe2, $53f1a602, $dc72a58b, $0b531627, $9d0127d3, $6c2bd8c1,
        $31a462f5, $74f3e8b9, $f615f109, $464c8c43, $aca54526, $89b50f97, $14b42844, $e1badf42,
        $16a62c4e, $3af774d2, $6906d2d0, $0941122d, $70d7e0ad, $b66f7154, $d01ebdb7, $edd6c77e,
        $cce285db, $42688457, $982c2dc2, $a4ed550e, $28755088, $5c86b831, $f86bed3f, $86c211a4);

  C4: TWhirlTab = (
        $d83078c0, $2646af05, $b891f97e, $fbcd6f13, $cb13a14c, $116d62a9, $09020508, $0d9e6e42,
        $9b6ceead, $ff510459, $0cb9bdde, $0ef706fb, $96f280ef, $30dece5f, $6d3feffc, $f8a407aa,
        $47c0fd27, $35657689, $372bcdac, $8a018c04, $d25b1571, $6c183c60, $84f68aff, $806ae1b5,
        $f53a69e8, $b3dd4753, $21b3acf6, $9c99ed5e, $435c966d, $29967a62, $5de121a3, $d5ae1682,
        $bd2a41a8, $e8eeb69f, $926eeba5, $9ed7567b, $1323d98c, $23fd17d3, $20947f6a, $44a9959e,
        $a2b025fa, $cf8fca06, $7c528d55, $5a142250, $507f4fe1, $c95d1a69, $14d6da7f, $d917ab5c,
        $3c677381, $8fba34d2, $90205080, $07f503f3, $dd8bc016, $d37cc6ed, $2d0a1128, $78cee61f,
        $97d55373, $024ebb25, $73825832, $a70b9d2c, $f6530151, $b2fa94cf, $4937fbdc, $56ad9f8e,
        $70eb308b, $cdc17123, $bbf891c7, $71cce317, $7ba78ea6, $af2e4bb8, $458e4602, $1a21dc84,
        $d489c51e, $585a9975, $2e637991, $3f0e1b38, $ac472301, $b0b42fea, $ef1bb56c, $b666ff85,
        $5cc6f23f, $12040a10, $93493839, $dee2a8af, $c68dcf0e, $d1327dc8, $3b927072, $5faf9a86,
        $31f91dc3, $a8db484b, $b9b62ae2, $bc0d9234, $3e29c8a4, $0b4cbe2d, $bf64fa8d, $597d4ae9,
        $f2cf6a1b, $771e3378, $33b7a6e6, $f41dba74, $27617c99, $eb87de26, $8968e4bd, $3290757a,
        $54e324ab, $8df48ff7, $643deaf4, $9dbe3ec2, $3d40a01d, $0fd0d567, $ca3472d0, $b7412c19,
        $7d755ec9, $cea8199a, $7f3be5ec, $2f44aa0d, $63c8e907, $2aff12db, $cce6a2bf, $82245a90,
        $7a805d3a, $48102840, $959be856, $dfc57b33, $4dab9096, $c05f1f61, $9107831c, $c87ac9f5,
        $5b33f1cc, $00000000, $f983d436, $6e568745, $e1ecb397, $e619b064, $28b1a9fe, $c33677d8,
        $74775bc1, $be432911, $1dd4df77, $eaa00dba, $578a4c12, $38fb18cb, $ad60f09d, $c4c3742b,
        $da7ec3e5, $c7aa1c92, $db591079, $e9c96503, $6acaec0f, $036968b9, $4a5e9365, $8e9de74e,
        $60a181be, $fc386ce0, $46e72ebb, $1f9a6452, $7639e0e4, $faeabc8f, $360c1e30, $ae099824,
        $4b7940f9, $85d15963, $7e1c3670, $e73e63f8, $55c4f737, $3ab5a3ee, $814d3229, $5231f4c4,
        $62ef3a9b, $a397f666, $104ab135, $abb220f2, $d015ae54, $c5e4a7b7, $ec72ddd5, $1698615a,
        $94bc3bca, $9ff085e7, $e570d8dd, $98058614, $17bfb2c6, $e4570b41, $a1d94d43, $4ec2f82f,
        $427b45f1, $3442a515, $0825d694, $ee3c66f0, $61865222, $b193fc76, $4fe52bb3, $24081420,
        $e3a208b2, $252fc7bc, $22dac44f, $651a3968, $79e93583, $69a384b6, $a9fc9bd7, $1948b43d,
        $fe76d7c5, $9a4b3d31, $f081d13e, $99225588, $8303890c, $049c6b4a, $667351d1, $e0cb600b,
        $c178ccfd, $fd1fbf7c, $4035fed4, $1cf30ceb, $186f67a1, $8b265f98, $51589c7d, $05bbb8d6,
        $8cd35c6b, $39dccb57, $aa95f36e, $1b060f18, $dcac138a, $5e88491a, $a0fe9edf, $884f3721,
        $6754824d, $0a6b6db1, $879fe246, $f1a602a2, $72a58bae, $53162758, $0127d39c, $2bd8c147,
        $a462f595, $f3e8b987, $15f109e3, $4c8c430a, $a5452609, $b50f973c, $b42844a0, $badf425b,
        $a62c4eb0, $f774d2cd, $06d2d06f, $41122d48, $d7e0ada7, $6f7154d9, $1ebdb7ce, $d6c77e3b,
        $e285db2e, $6884572a, $2c2dc2b4, $ed550e49, $7550885d, $86b831da, $6bed3f93, $c211a444);

  C5: TWhirlTab = (
        $3078c018, $46af0523, $91f97ec6, $cd6f13e8, $13a14c87, $6d62a9b8, $02050801, $9e6e424f,
        $6ceead36, $510459a6, $b9bdded2, $f706fbf5, $f280ef79, $dece5f6f, $3feffc91, $a407aa52,
        $c0fd2760, $657689bc, $2bcdac9b, $018c048e, $5b1571a3, $183c600c, $f68aff7b, $6ae1b535,
        $3a69e81d, $dd4753e0, $b3acf6d7, $99ed5ec2, $5c966d2e, $967a624b, $e121a3fe, $ae168257,
        $2a41a815, $eeb69f77, $6eeba537, $d7567be5, $23d98c9f, $fd17d3f0, $947f6a4a, $a9959eda,
        $b025fa58, $8fca06c9, $528d5529, $1422500a, $7f4fe1b1, $5d1a69a0, $d6da7f6b, $17ab5c85,
        $677381bd, $ba34d25d, $20508010, $f503f3f4, $8bc016cb, $7cc6ed3e, $0a112805, $cee61f67,
        $d55373e4, $4ebb2527, $82583241, $0b9d2c8b, $530151a7, $fa94cf7d, $37fbdc95, $ad9f8ed8,
        $eb308bfb, $c17123ee, $f891c77c, $cce31766, $a78ea6dd, $2e4bb817, $8e460247, $21dc849e,
        $89c51eca, $5a99752d, $637991bf, $0e1b3807, $472301ad, $b42fea5a, $1bb56c83, $66ff8533,
        $c6f23f63, $040a1002, $493839aa, $e2a8af71, $8dcf0ec8, $327dc819, $92707249, $af9a86d9,
        $f91dc3f2, $db484be3, $b62ae25b, $0d923488, $29c8a49a, $4cbe2d26, $64fa8d32, $7d4ae9b0,
        $cf6a1be9, $1e33780f, $b7a6e6d5, $1dba7480, $617c99be, $87de26cd, $68e4bd34, $90757a48,
        $e324abff, $f48ff77a, $3deaf490, $be3ec25f, $40a01d20, $d0d56768, $3472d01a, $412c19ae,
        $755ec9b4, $a8199a54, $3be5ec93, $44aa0d22, $c8e90764, $ff12dbf1, $e6a2bf73, $245a9012,
        $805d3a40, $10284008, $9be856c3, $c57b33ec, $ab9096db, $5f1f61a1, $07831c8d, $7ac9f53d,
        $33f1cc97, $00000000, $83d436cf, $5687452b, $ecb39776, $19b06482, $b1a9fed6, $3677d81b,
        $775bc1b5, $432911af, $d4df776a, $a00dba50, $8a4c1245, $fb18cbf3, $60f09d30, $c3742bef,
        $7ec3e53f, $aa1c9255, $591079a2, $c96503ea, $caec0f65, $6968b9ba, $5e93652f, $9de74ec0,
        $a181bede, $386ce01c, $e72ebbfd, $9a64524d, $39e0e492, $eabc8f75, $0c1e3006, $0998248a,
        $7940f9b2, $d15963e6, $1c36700e, $3e63f81f, $c4f73762, $b5a3eed4, $4d3229a8, $31f4c496,
        $ef3a9bf9, $97f666c5, $4ab13525, $b220f259, $15ae5484, $e4a7b772, $72ddd539, $98615a4c,
        $bc3bca5e, $f085e778, $70d8dd38, $0586148c, $bfb2c6d1, $570b41a5, $d94d43e2, $c2f82f61,
        $7b45f1b3, $42a51521, $25d6949c, $3c66f01e, $86522243, $93fc76c7, $e52bb3fc, $08142004,
        $a208b251, $2fc7bc99, $dac44f6d, $1a39680d, $e93583fa, $a384b6df, $fc9bd77e, $48b43d24,
        $76d7c53b, $4b3d31ab, $81d13ece, $22558811, $03890c8f, $9c6b4a4e, $7351d1b7, $cb600beb,
        $78ccfd3c, $1fbf7c81, $35fed494, $f30cebf7, $6f67a1b9, $265f9813, $589c7d2c, $bbb8d6d3,
        $d35c6be7, $dccb576e, $95f36ec4, $060f1803, $ac138a56, $88491a44, $fe9edf7f, $4f3721a9,
        $54824d2a, $6b6db1bb, $9fe246c1, $a602a253, $a58baedc, $1627580b, $27d39c9d, $d8c1476c,
        $62f59531, $e8b98774, $f109e3f6, $8c430a46, $452609ac, $0f973c89, $2844a014, $df425be1,
        $2c4eb016, $74d2cd3a, $d2d06f69, $122d4809, $e0ada770, $7154d9b6, $bdb7ced0, $c77e3bed,
        $85db2ecc, $84572a42, $2dc2b498, $550e49a4, $50885d28, $b831da5c, $ed3f93f8, $11a44486);

  C6: TWhirlTab = (
        $78c01860, $af05238c, $f97ec63f, $6f13e887, $a14c8726, $62a9b8da, $05080104, $6e424f21,
        $eead36d8, $0459a6a2, $bdded26f, $06fbf5f3, $80ef79f9, $ce5f6fa1, $effc917e, $07aa5255,
        $fd27609d, $7689bcca, $cdac9b56, $8c048e02, $1571a3b6, $3c600c30, $8aff7bf1, $e1b535d4,
        $69e81d74, $4753e0a7, $acf6d77b, $ed5ec22f, $966d2eb8, $7a624b31, $21a3fedf, $16825741,
        $41a81554, $b69f77c1, $eba537dc, $567be5b3, $d98c9f46, $17d3f0e7, $7f6a4a35, $959eda4f,
        $25fa587d, $ca06c903, $8d5529a4, $22500a28, $4fe1b1fe, $1a69a0ba, $da7f6bb1, $ab5c852e,
        $7381bdce, $34d25d69, $50801040, $03f3f4f7, $c016cb0b, $c6ed3ef8, $11280514, $e61f6781,
        $5373e4b7, $bb25279c, $58324119, $9d2c8b16, $0151a7a6, $94cf7de9, $fbdc956e, $9f8ed847,
        $308bfbcb, $7123ee9f, $91c77ced, $e3176685, $8ea6dd53, $4bb8175c, $46024701, $dc849e42,
        $c51eca0f, $99752db4, $7991bfc6, $1b38071c, $2301ad8e, $2fea5a75, $b56c8336, $ff8533cc,
        $f23f6391, $0a100208, $3839aa92, $a8af71d9, $cf0ec807, $7dc81964, $70724939, $9a86d943,
        $1dc3f2ef, $484be3ab, $2ae25b71, $9234881a, $c8a49a52, $be2d2698, $fa8d32c8, $4ae9b0fa,
        $6a1be983, $33780f3c, $a6e6d573, $ba74803a, $7c99bec2, $de26cd13, $e4bd34d0, $757a483d,
        $24abffdb, $8ff77af5, $eaf4907a, $3ec25f61, $a01d2080, $d56768bd, $72d01a68, $2c19ae82,
        $5ec9b4ea, $199a544d, $e5ec9376, $aa0d2288, $e907648d, $12dbf1e3, $a2bf73d1, $5a901248,
        $5d3a401d, $28400820, $e856c32b, $7b33ec97, $9096db4b, $1f61a1be, $831c8d0e, $c9f53df4,
        $f1cc9766, $00000000, $d436cf1b, $87452bac, $b39776c5, $b0648232, $a9fed67f, $77d81b6c,
        $5bc1b5ee, $2911af86, $df776ab5, $0dba505d, $4c124509, $18cbf3eb, $f09d30c0, $742bef9b,
        $c3e53ffc, $1c925549, $1079a2b2, $6503ea8f, $ec0f6589, $68b9bad2, $93652fbc, $e74ec027,
        $81bede5f, $6ce01c70, $2ebbfdd3, $64524d29, $e0e49272, $bc8f75c9, $1e300618, $98248a12,
        $40f9b2f2, $5963e6bf, $36700e38, $63f81f7c, $f7376295, $a3eed477, $3229a89a, $f4c49662,
        $3a9bf9c3, $f666c533, $b1352594, $20f25979, $ae54842a, $a7b772d5, $ddd539e4, $615a4c2d,
        $3bca5e65, $85e778fd, $d8dd38e0, $86148c0a, $b2c6d163, $0b41a5ae, $4d43e2af, $f82f6199,
        $45f1b3f6, $a5152184, $d6949c4a, $66f01e78, $52224311, $fc76c73b, $2bb3fcd7, $14200410,
        $08b25159, $c7bc995e, $c44f6da9, $39680d34, $3583facf, $84b6df5b, $9bd77ee5, $b43d2490,
        $d7c53bec, $3d31ab96, $d13ece1f, $55881144, $890c8f06, $6b4a4e25, $51d1b7e6, $600beb8b,
        $ccfd3cf0, $bf7c813e, $fed4946a, $0cebf7fb, $67a1b9de, $5f98134c, $9c7d2cb0, $b8d6d36b,
        $5c6be7bb, $cb576ea5, $f36ec437, $0f18030c, $138a5645, $491a440d, $9edf7fe1, $3721a99e,
        $824d2aa8, $6db1bbd6, $e246c123, $02a25351, $8baedc57, $27580b2c, $d39c9d4e, $c1476cad,
        $f59531c4, $b98774cd, $09e3f6ff, $430a4605, $2609ac8a, $973c891e, $44a01450, $425be1a3,
        $4eb01658, $d2cd3ae8, $d06f69b9, $2d480924, $ada770dd, $54d9b6e2, $b7ced067, $7e3bed93,
        $db2ecc17, $572a4215, $c2b4985a, $0e49a4aa, $885d28a0, $31da5c6d, $3f93f8c7, $a4448622);

  C7: TWhirlTab = (
        $c0186018, $05238c23, $7ec63fc6, $13e887e8, $4c872687, $a9b8dab8, $08010401, $424f214f,
        $ad36d836, $59a6a2a6, $ded26fd2, $fbf5f3f5, $ef79f979, $5f6fa16f, $fc917e91, $aa525552,
        $27609d60, $89bccabc, $ac9b569b, $048e028e, $71a3b6a3, $600c300c, $ff7bf17b, $b535d435,
        $e81d741d, $53e0a7e0, $f6d77bd7, $5ec22fc2, $6d2eb82e, $624b314b, $a3fedffe, $82574157,
        $a8155415, $9f77c177, $a537dc37, $7be5b3e5, $8c9f469f, $d3f0e7f0, $6a4a354a, $9eda4fda,
        $fa587d58, $06c903c9, $5529a429, $500a280a, $e1b1feb1, $69a0baa0, $7f6bb16b, $5c852e85,
        $81bdcebd, $d25d695d, $80104010, $f3f4f7f4, $16cb0bcb, $ed3ef83e, $28051405, $1f678167,
        $73e4b7e4, $25279c27, $32411941, $2c8b168b, $51a7a6a7, $cf7de97d, $dc956e95, $8ed847d8,
        $8bfbcbfb, $23ee9fee, $c77ced7c, $17668566, $a6dd53dd, $b8175c17, $02470147, $849e429e,
        $1eca0fca, $752db42d, $91bfc6bf, $38071c07, $01ad8ead, $ea5a755a, $6c833683, $8533cc33,
        $3f639163, $10020802, $39aa92aa, $af71d971, $0ec807c8, $c8196419, $72493949, $86d943d9,
        $c3f2eff2, $4be3abe3, $e25b715b, $34881a88, $a49a529a, $2d269826, $8d32c832, $e9b0fab0,
        $1be983e9, $780f3c0f, $e6d573d5, $74803a80, $99bec2be, $26cd13cd, $bd34d034, $7a483d48,
        $abffdbff, $f77af57a, $f4907a90, $c25f615f, $1d208020, $6768bd68, $d01a681a, $19ae82ae,
        $c9b4eab4, $9a544d54, $ec937693, $0d228822, $07648d64, $dbf1e3f1, $bf73d173, $90124812,
        $3a401d40, $40082008, $56c32bc3, $33ec97ec, $96db4bdb, $61a1bea1, $1c8d0e8d, $f53df43d,
        $cc976697, $00000000, $36cf1bcf, $452bac2b, $9776c576, $64823282, $fed67fd6, $d81b6c1b,
        $c1b5eeb5, $11af86af, $776ab56a, $ba505d50, $12450945, $cbf3ebf3, $9d30c030, $2bef9bef,
        $e53ffc3f, $92554955, $79a2b2a2, $03ea8fea, $0f658965, $b9bad2ba, $652fbc2f, $4ec027c0,
        $bede5fde, $e01c701c, $bbfdd3fd, $524d294d, $e4927292, $8f75c975, $30061806, $248a128a,
        $f9b2f2b2, $63e6bfe6, $700e380e, $f81f7c1f, $37629562, $eed477d4, $29a89aa8, $c4966296,
        $9bf9c3f9, $66c533c5, $35259425, $f2597959, $54842a84, $b772d572, $d539e439, $5a4c2d4c,
        $ca5e655e, $e778fd78, $dd38e038, $148c0a8c, $c6d163d1, $41a5aea5, $43e2afe2, $2f619961,
        $f1b3f6b3, $15218421, $949c4a9c, $f01e781e, $22431143, $76c73bc7, $b3fcd7fc, $20041004,
        $b2515951, $bc995e99, $4f6da96d, $680d340d, $83facffa, $b6df5bdf, $d77ee57e, $3d249024,
        $c53bec3b, $31ab96ab, $3ece1fce, $88114411, $0c8f068f, $4a4e254e, $d1b7e6b7, $0beb8beb,
        $fd3cf03c, $7c813e81, $d4946a94, $ebf7fbf7, $a1b9deb9, $98134c13, $7d2cb02c, $d6d36bd3,
        $6be7bbe7, $576ea56e, $6ec437c4, $18030c03, $8a564556, $1a440d44, $df7fe17f, $21a99ea9,
        $4d2aa82a, $b1bbd6bb, $46c123c1, $a2535153, $aedc57dc, $580b2c0b, $9c9d4e9d, $476cad6c,
        $9531c431, $8774cd74, $e3f6fff6, $0a460546, $09ac8aac, $3c891e89, $a0145014, $5be1a3e1,
        $b0165816, $cd3ae83a, $6f69b969, $48092409, $a770dd70, $d9b6e2b6, $ced067d0, $3bed93ed,
        $2ecc17cc, $2a421542, $b4985a98, $49a4aaa4, $5d28a028, $da5c6d5c, $93f8c7f8, $44862286);


{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}


{Internal types for type casting}
type
  TW64  = packed record
            L,H: longint;
          end;


{$ifndef BIT16}

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  function RB(A: longint): longint;  {$ifdef HAS_INLINE} inline; {$endif}
    {-reverse byte order in longint}
  begin
    RB := ((A and $FF) shl 24) or ((A and $FF00) shl 8) or ((A and $FF0000) shr 8) or ((A and longint($FF000000)) shr 24);
  end;
{$else}
  {---------------------------------------------------------------------------}
  function RB(A: longint): longint; assembler;
    {-reverse byte order in longint}
  asm
    {$ifdef LoadArgs}
      mov eax,[A]
    {$endif}
      xchg al,ah
      rol  eax,16
      xchg al,ah
  end;
  {$ifndef UseInt64}
    {---------------------------------------------------------------------------}
    procedure Inc64(var Z: TW64; const X: TW64); assembler;
      {-Inc a 64 bit integer}
    asm
      {$ifdef LoadArgs}
        mov eax,[Z]
        mov edx,[X]
      {$endif}
      mov  ecx,    [edx]
      add  [eax],  ecx
      mov  ecx,    [edx+4]
      adc  [eax+4],ecx
    end;
  {$endif}
{$endif}

{$else}

{*** 16 bit ***}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

(*** TP 5/5.5 ***)

{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/              {pop    ax   }
  $5A/              {pop    dx   }
  $86/$C6/          {xchg   dh,al}
  $86/$E2);         {xchg   dl,ah}


{---------------------------------------------------------------------------}
procedure Inc64(var Z: TW64; var X: TW64);
  {-Inc a 64 bit integer}
inline(
  $8C/$DF/      {mov   di,ds     }
  $5E/          {pop   si        }
  $1F/          {pop   ds        }
  $8B/$04/      {mov   ax,[si]   }
  $8B/$5C/$02/  {mov   bx,[si+02]}
  $8B/$4C/$04/  {mov   cx,[si+04]}
  $8B/$54/$06/  {mov   dx,[si+06]}
  $5E/          {pop   si        }
  $1F/          {pop   ds        }
  $01/$04/      {add   [si],ax   }
  $11/$5C/$02/  {adc   [si+02],bx}
  $11/$4C/$04/  {adc   [si+04],cx}
  $11/$54/$06/  {adc   [si+06],dx}
  $8E/$DF);     {mov   ds,di     }


{$else}


(**** TP 6/7/Delphi1 for 386+ *****)


{---------------------------------------------------------------------------}
function RB(A: longint): longint;
  {-reverse byte order in longint}
inline(
  $58/              {pop    ax   }
  $5A/              {pop    dx   }
  $86/$C6/          {xchg   dh,al}
  $86/$E2);         {xchg   dl,ah}


{---------------------------------------------------------------------------}
procedure Inc64(var Z: TW64; {$ifdef CONST} const {$else} var {$endif} X: TW64);
  {-Inc a 64 bit integer}
inline(
  $8C/$D9/             {mov  cx,ds      }
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$8B/$07/         {mov  eax,[bx]   }
  $66/$8B/$57/$04/     {mov  edx,[bx+04]}
  $5B/                 {pop  bx         }
  $1F/                 {pop  ds         }
  $66/$03/$07/         {add  eax,[bx]   }
  $66/$13/$57/$04/     {adc  edx,[bx+04]}
  $66/$89/$07/         {mov  [bx],eax   }
  $66/$89/$57/$04/     {mov  [bx+04],edx}
  $8E/$D9);            {mov  ds,cx      }


{$endif BASM16}

{$endif BIT16}


{---------------------------------------------------------------------------}
procedure XorBlock({$ifdef CONST} const {$else} var {$endif} b1,b2: THashState; var b3: THashState);
  {-xor two Whirlpool hash blocks}
begin
{$ifdef BASM16}
  asm
             mov  di,ds
             lds  si,[b1]
     db $66; mov  ax,[si+00]
     db $66; mov  bx,[si+04]
     db $66; mov  cx,[si+08]
     db $66; mov  dx,[si+12]
             lds  si,[b2]
     db $66; xor  ax,[si+00]
     db $66; xor  bx,[si+04]
     db $66; xor  cx,[si+08]
     db $66; xor  dx,[si+12]
             lds  si,[b3]
     db $66; mov  [si+00],ax
     db $66; mov  [si+04],bx
     db $66; mov  [si+08],cx
     db $66; mov  [si+12],dx

             lds  si,[b1]
     db $66; mov  ax,[si+16]
     db $66; mov  bx,[si+20]
     db $66; mov  cx,[si+24]
     db $66; mov  dx,[si+28]
             lds  si,[b2]
     db $66; xor  ax,[si+16]
     db $66; xor  bx,[si+20]
     db $66; xor  cx,[si+24]
     db $66; xor  dx,[si+28]
             lds  si,[b3]
     db $66; mov  [si+16],ax
     db $66; mov  [si+20],bx
     db $66; mov  [si+24],cx
     db $66; mov  [si+28],dx

             lds  si,[b1]
     db $66; mov  ax,[si+32]
     db $66; mov  bx,[si+36]
     db $66; mov  cx,[si+40]
     db $66; mov  dx,[si+44]
             lds  si,[b2]
     db $66; xor  ax,[si+32]
     db $66; xor  bx,[si+36]
     db $66; xor  cx,[si+40]
     db $66; xor  dx,[si+44]
             lds  si,[b3]
     db $66; mov  [si+32],ax
     db $66; mov  [si+36],bx
     db $66; mov  [si+40],cx
     db $66; mov  [si+44],dx

             lds  si,[b1]
     db $66; mov  ax,[si+48]
     db $66; mov  bx,[si+52]
     db $66; mov  cx,[si+56]
     db $66; mov  dx,[si+60]
             lds  si,[b2]
     db $66; xor  ax,[si+48]
     db $66; xor  bx,[si+52]
     db $66; xor  cx,[si+56]
     db $66; xor  dx,[si+60]
             lds  si,[b3]
     db $66; mov  [si+48],ax
     db $66; mov  [si+52],bx
     db $66; mov  [si+56],cx
     db $66; mov  [si+60],dx
             mov  ds,di
  end;
{$else}
  b3[00] := b1[00] xor b2[00];
  b3[01] := b1[01] xor b2[01];
  b3[02] := b1[02] xor b2[02];
  b3[03] := b1[03] xor b2[03];
  b3[04] := b1[04] xor b2[04];
  b3[05] := b1[05] xor b2[05];
  b3[06] := b1[06] xor b2[06];
  b3[07] := b1[07] xor b2[07];
  b3[08] := b1[08] xor b2[08];
  b3[09] := b1[09] xor b2[09];
  b3[10] := b1[10] xor b2[10];
  b3[11] := b1[11] xor b2[11];
  b3[12] := b1[12] xor b2[12];
  b3[13] := b1[13] xor b2[13];
  b3[14] := b1[14] xor b2[14];
  b3[15] := b1[15] xor b2[15];
{$endif}
end;


{$ifdef BASM16}

{---------------------------------------------------------------------------}
procedure Transform({$ifdef CONST} const {$else} var {$endif} KK: THashState; var L: THashState);
  {-perform transformation L = theta(pi(gamma(KK)))}
var
  K: THState8;
begin
  asm
    {dest = K}
    mov dx, ds
    mov ax, ss
    mov es, ax
    lea di, [K]
    {src = KK}
    mov dx, ds
    lds si, [KK]
    {move words, movsd is slower!}
    mov cx, 32
    cld
    rep movsw
    {restore ds}
    mov ds,dx
    {es:di -> L}
    les di, [L]

    {L[00]:= C0[K[00]] xor C1[K[57]] xor C2[K[50]] xor C3[K[43]] xor
             C4[K[36]] xor C5[K[29]] xor C6[K[22]] xor C7[K[15]]}
    {L[01]:= C4[K[00]] xor C5[K[57]] xor C6[K[50]] xor C7[K[43]] xor
             C0[K[36]] xor C1[K[29]] xor C2[K[22]] xor C3[K[15]]}
             sub bh, bh
             mov bl, byte ptr K[00]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[57]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[50]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[43]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[36]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[29]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[22]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[15]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[02]:= C0[K[08]] xor C1[K[01]] xor C2[K[58]] xor C3[K[51]] xor
             C4[K[44]] xor C5[K[37]] xor C6[K[30]] xor C7[K[23]]}
    {L[03]:= C4[K[08]] xor C5[K[01]] xor C6[K[58]] xor C7[K[51]] xor
             C0[K[44]] xor C1[K[37]] xor C2[K[30]] xor C3[K[23]]}
             sub bh, bh
             mov bl, byte ptr K[08]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[01]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[58]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[51]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[44]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[37]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[30]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[23]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[04]:= C0[K[16]] xor C1[K[09]] xor C2[K[02]] xor C3[K[59]] xor
             C4[K[52]] xor C5[K[45]] xor C6[K[38]] xor C7[K[31]]}
    {L[05]:= C4[K[16]] xor C5[K[09]] xor C6[K[02]] xor C7[K[59]] xor
             C0[K[52]] xor C1[K[45]] xor C2[K[38]] xor C3[K[31]]}
             sub bh, bh
             mov bl, byte ptr K[16]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[09]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[02]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[59]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[52]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[45]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[38]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[31]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[06]:= C0[K[24]] xor C1[K[17]] xor C2[K[10]] xor C3[K[03]] xor
             C4[K[60]] xor C5[K[53]] xor C6[K[46]] xor C7[K[39]]}
    {L[07]:= C4[K[24]] xor C5[K[17]] xor C6[K[10]] xor C7[K[03]] xor
             C0[K[60]] xor C1[K[53]] xor C2[K[46]] xor C3[K[39]]}
             sub bh, bh
             mov bl, byte ptr K[24]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[17]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[10]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[03]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[60]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[53]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[46]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[39]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[08]:= C0[K[32]] xor C1[K[25]] xor C2[K[18]] xor C3[K[11]] xor
             C4[K[04]] xor C5[K[61]] xor C6[K[54]] xor C7[K[47]]}
    {L[09]:= C4[K[32]] xor C5[K[25]] xor C6[K[18]] xor C7[K[11]] xor
             C0[K[04]] xor C1[K[61]] xor C2[K[54]] xor C3[K[47]]}
             sub bh, bh
             mov bl, byte ptr K[32]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[25]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[18]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[11]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[04]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[61]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[54]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[47]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[10]:= C0[K[40]] xor C1[K[33]] xor C2[K[26]] xor C3[K[19]] xor
             C4[K[12]] xor C5[K[05]] xor C6[K[62]] xor C7[K[55]]}
    {L[11]:= C4[K[40]] xor C5[K[33]] xor C6[K[26]] xor C7[K[19]] xor
             C0[K[12]] xor C1[K[05]] xor C2[K[62]] xor C3[K[55]]}
             sub bh, bh
             mov bl, byte ptr K[40]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[33]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[26]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[19]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[12]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[05]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[62]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[55]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[12]:= C0[K[48]] xor C1[K[41]] xor C2[K[34]] xor C3[K[27]] xor
             C4[K[20]] xor C5[K[13]] xor C6[K[06]] xor C7[K[63]]}
    {L[13]:= C4[K[48]] xor C5[K[41]] xor C6[K[34]] xor C7[K[27]] xor
             C0[K[20]] xor C1[K[13]] xor C2[K[06]] xor C3[K[63]]}
             sub bh, bh
             mov bl, byte ptr K[48]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[41]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[34]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[27]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[20]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[13]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[06]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[63]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
             add di,8

    {L[14]:= C0[K[56]] xor C1[K[49]] xor C2[K[42]] xor C3[K[35]] xor
             C4[K[28]] xor C5[K[21]] xor C6[K[14]] xor C7[K[07]]}
    {L[15]:= C4[K[56]] xor C5[K[49]] xor C6[K[42]] xor C7[K[35]] xor
             C0[K[28]] xor C1[K[21]] xor C2[K[14]] xor C3[K[07]]}
             sub bh, bh
             mov bl, byte ptr K[56]
             shl bx, 2
    db $66;  mov ax, word ptr C0[bx]
    db $66;  mov dx, word ptr C4[bx]
             sub bh, bh
             mov bl, byte ptr K[49]
             shl bx, 2
    db $66;  xor ax, word ptr C1[bx]
    db $66;  xor dx, word ptr C5[bx]
             sub bh, bh
             mov bl, byte ptr K[42]
             shl bx, 2
    db $66;  xor ax, word ptr C2[bx]
    db $66;  xor dx, word ptr C6[bx]
             sub bh, bh
             mov bl, byte ptr K[35]
             shl bx, 2
    db $66;  xor ax, word ptr C3[bx]
    db $66;  xor dx, word ptr C7[bx]
             sub bh, bh
             mov bl, byte ptr K[28]
             shl bx, 2
    db $66;  xor ax, word ptr C4[bx]
    db $66;  xor dx, word ptr C0[bx]
             sub bh, bh
             mov bl, byte ptr K[21]
             shl bx, 2
    db $66;  xor ax, word ptr C5[bx]
    db $66;  xor dx, word ptr C1[bx]
             sub bh, bh
             mov bl, byte ptr K[14]
             shl bx, 2
    db $66;  xor ax, word ptr C6[bx]
    db $66;  xor dx, word ptr C2[bx]
             sub bh, bh
             mov bl, byte ptr K[07]
             shl bx, 2
    db $66;  xor ax, word ptr C7[bx]
    db $66;  xor dx, word ptr C3[bx]
    db $66;  mov es:[di],ax
    db $66;  mov es:[di+4],dx
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure Transform({$ifdef CONST} const {$else} var {$endif} KK: THashState; var L: THashState);
  {-perform transformation L = theta(pi(gamma(KK)))}
var
  K: THState8 {$ifndef BIT16} absolute KK {$endif};
begin
  {$ifdef BIT16}
    {For 16 bit compilers copy KK to stack segment: about 20% faster.}
    {For 32 bit compilers with flat address space copying slows down!}
    K := THState8(KK);
  {$endif}
  L[00] := C0[K[00]] xor C1[K[57]] xor C2[K[50]] xor C3[K[43]] xor C4[K[36]] xor C5[K[29]] xor C6[K[22]] xor C7[K[15]];
  L[02] := C0[K[08]] xor C1[K[01]] xor C2[K[58]] xor C3[K[51]] xor C4[K[44]] xor C5[K[37]] xor C6[K[30]] xor C7[K[23]];
  L[04] := C0[K[16]] xor C1[K[09]] xor C2[K[02]] xor C3[K[59]] xor C4[K[52]] xor C5[K[45]] xor C6[K[38]] xor C7[K[31]];
  L[06] := C0[K[24]] xor C1[K[17]] xor C2[K[10]] xor C3[K[03]] xor C4[K[60]] xor C5[K[53]] xor C6[K[46]] xor C7[K[39]];
  L[08] := C0[K[32]] xor C1[K[25]] xor C2[K[18]] xor C3[K[11]] xor C4[K[04]] xor C5[K[61]] xor C6[K[54]] xor C7[K[47]];
  L[10] := C0[K[40]] xor C1[K[33]] xor C2[K[26]] xor C3[K[19]] xor C4[K[12]] xor C5[K[05]] xor C6[K[62]] xor C7[K[55]];
  L[12] := C0[K[48]] xor C1[K[41]] xor C2[K[34]] xor C3[K[27]] xor C4[K[20]] xor C5[K[13]] xor C6[K[06]] xor C7[K[63]];
  L[14] := C0[K[56]] xor C1[K[49]] xor C2[K[42]] xor C3[K[35]] xor C4[K[28]] xor C5[K[21]] xor C6[K[14]] xor C7[K[07]];
  {Use special properties of circulant matrices: the lower 32 bit}
  {tables Cx(low) are the tables Cy(high) with y = x + 4 (mod 8) }
  L[01] := C4[K[00]] xor C5[K[57]] xor C6[K[50]] xor C7[K[43]] xor C0[K[36]] xor C1[K[29]] xor C2[K[22]] xor C3[K[15]];
  L[03] := C4[K[08]] xor C5[K[01]] xor C6[K[58]] xor C7[K[51]] xor C0[K[44]] xor C1[K[37]] xor C2[K[30]] xor C3[K[23]];
  L[05] := C4[K[16]] xor C5[K[09]] xor C6[K[02]] xor C7[K[59]] xor C0[K[52]] xor C1[K[45]] xor C2[K[38]] xor C3[K[31]];
  L[07] := C4[K[24]] xor C5[K[17]] xor C6[K[10]] xor C7[K[03]] xor C0[K[60]] xor C1[K[53]] xor C2[K[46]] xor C3[K[39]];
  L[09] := C4[K[32]] xor C5[K[25]] xor C6[K[18]] xor C7[K[11]] xor C0[K[04]] xor C1[K[61]] xor C2[K[54]] xor C3[K[47]];
  L[11] := C4[K[40]] xor C5[K[33]] xor C6[K[26]] xor C7[K[19]] xor C0[K[12]] xor C1[K[05]] xor C2[K[62]] xor C3[K[55]];
  L[13] := C4[K[48]] xor C5[K[41]] xor C6[K[34]] xor C7[K[27]] xor C0[K[20]] xor C1[K[13]] xor C2[K[06]] xor C3[K[63]];
  L[15] := C4[K[56]] xor C5[K[49]] xor C6[K[42]] xor C7[K[35]] xor C0[K[28]] xor C1[K[21]] xor C2[K[14]] xor C3[K[07]];
end;
{$endif BASM16}


{---------------------------------------------------------------------------}
procedure Whirl_Compress(var Data: THashContext);
  {-The core Whirlpool compression function, using double rounds}
var
  r,r4: integer;
  L,K: THashState; {even/odd round keys}
  S: THashState;   {cipher state}
begin
  {compute and apply K^0 to the cipher state}
  K := Data.Hash;
  XorBlock(Data.Hash, PHashState(@Data.Buffer)^, S);
  for r:=0 to 4 do begin
    {compression is done in five double rounds}
    r4 := r shl 2;
    {compute next round key (=L)}
    Transform(K,L);
    L[0] := L[0] xor RC[r4  ];
    L[1] := L[1] xor RC[r4+1];
    {apply the round transformation}
    Transform(S,K);
    XorBlock(L, K, S);
    {compute next round key (=K)}
    Transform(L,K);
    K[0] := K[0] xor RC[r4+2];
    K[1] := K[1] xor RC[r4+3];
    {apply the round transformation}
    Transform(S,L);
    XorBlock(K, L, S);
  end;
  {apply the Miyaguchi-Preneel compression function}
  XorBlock(S, PHashState(@Data.Buffer)^, S);
  XorBlock(S, Data.Hash, Data.Hash);
end;


{$ifdef UseInt64}
{---------------------------------------------------------------------------}
procedure UpdateLen(var Context: THashContext; Len: longint; IsByteLen: boolean);
  {-Update 128 bit message bit length, Len = byte length if IsByteLen}
var
  t0,t1: TW64;
  i: integer;
begin
  if IsByteLen then int64(t0) := 8*int64(Len)
  else int64(t0) := int64(Len);
  t1.L := Context.MLen[0];
  t1.H := 0;
  Inc(int64(t0),int64(t1));
  Context.MLen[0] := t0.L;
  for i:=1 to 3 do begin
    if t0.H=0 then exit;
    t1.L := Context.MLen[i];
    t0.L := t0.H;
    t0.H := 0;
    Inc(int64(t0),int64(t1));
    Context.MLen[i] := t0.L;
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure UpdateLen(var Context: THashContext; Len: longint; IsByteLen: boolean);
  {-Update 128 bit message bit length, Len = byte length if IsByteLen}
var
  t0,t1: TW64;
  i: integer;
begin
  if IsByteLen then begin
    {Calculate bit length increment = 8*Len}
    if Len<=$0FFFFFFF then begin
      {safe to multiply without overflow}
      t0.L := 8*Len;
      t0.H := 0;
    end
    else begin
      t0.L := Len;
      t0.H := 0;
      Inc64(t0,t0);
      Inc64(t0,t0);
      Inc64(t0,t0);
    end;
  end
  else begin
    t0.L := Len;
    t0.H := 0;
  end;
  {Update 128 bit length}
  t1.L := Context.MLen[0];
  t1.H := 0;
  Inc64(t0,t1);
  Context.MLen[0] := t0.L;
  for i:=1 to 3 do begin
    {propagate carry into higher bits}
    if t0.H=0 then exit;
    t1.L := Context.MLen[i];
    t0.L := t0.H;
    t0.H := 0;
    Inc64(t0,t1);
    Context.MLen[i] := t0.L;
  end;
end;
{$endif}


{---------------------------------------------------------------------------}
procedure Whirl_Init(var Context: THashContext);
  {-initialize context}
begin
  {Clear context, buffer=0!, InitIV=0!}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure Whirl_UpdateXL(var Context: THashContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
begin
  {Update message bit length}
  UpdateLen(Context, Len, true);
  while Len > 0 do begin
    {fill block with msg data}
    Context.Buffer[Context.Index]:= pByte(Msg)^;
    inc(Ptr2Inc(Msg));
    inc(Context.Index);
    dec(Len);
    if Context.Index=Whirl_BlockLen then begin
      {If 512 bit transferred, compress a block}
      Context.Index:= 0;
      Whirl_Compress(Context);
      while Len>=Whirl_BlockLen do begin
        move(Msg^,Context.Buffer,Whirl_BlockLen);
        Whirl_Compress(Context);
        inc(Ptr2Inc(Msg),Whirl_BlockLen);
        dec(Len,Whirl_BlockLen);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure Whirl_Update(var Context: THashContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  Whirl_UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Whirl_FinalBitsEx(var Context: THashContext; var Digest: THashDigest; BData: byte; bitlen: integer);
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}
var
  i: integer;
begin
  {Message padding}
  {append bits from BData and a single '1' bit}
  if (bitlen>0) and (bitlen<=7) then begin
    Context.Buffer[Context.Index]:= (BData and BitAPI_Mask[bitlen]) or BitAPI_PBit[bitlen];
    UpdateLen(Context, bitlen, false);
  end
  else Context.Buffer[Context.Index]:= $80;

  for i:=Context.Index+1 to pred(sizeof(Context.Buffer)) do Context.Buffer[i] := 0;
  {2. Compress if more than 256 bits}
  if Context.Index>=Whirl_BlockLen-32 then begin
    Whirl_Compress(Context);
    fillchar(Context.Buffer,sizeof(Context.Buffer),0);
  end;
  {Write 128 bit msg length into the last bits of the last block}
  {(in big endian format) and do a final compress}
  THashBuf32(Context.Buffer)[12]:= RB(Context.MLen[3]);
  THashBuf32(Context.Buffer)[13]:= RB(Context.MLen[2]);
  THashBuf32(Context.Buffer)[14]:= RB(Context.MLen[1]);
  THashBuf32(Context.Buffer)[15]:= RB(Context.MLen[0]);
  Whirl_Compress(Context);
  {Hash -> Digest (note: Hash is little endian format)}
  move(Context.Hash, Digest, sizeof(Digest));
  {Clear context}
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
procedure Whirl_FinalBits(var Context: THashContext; var Digest: TWhirlDigest; BData: byte; bitlen: integer);
  {-finalize Whirlpool calculation with bitlen bits from BData (big-endian), clear context}
var
  tmp: THashDigest;
begin
  Whirl_FinalBitsEx(Context, tmp, BData, bitlen);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
procedure Whirl_FinalEx(var Context: THashContext; var Digest: THashDigest);
  {-finalize Whirlpool calculation, clear context}
begin
  Whirl_FinalBitsEx(Context,Digest,0,0);
end;


{---------------------------------------------------------------------------}
procedure Whirl_Final(var Context: THashContext; var Digest: TWhirlDigest);
  {-finalize Whirlpool calculation, clear context}
var
  tmp: THashDigest;
begin
  Whirl_FinalBitsEx(Context, tmp, 0, 0);
  move(tmp, Digest, sizeof(Digest));
end;


{---------------------------------------------------------------------------}
function Whirl_SelfTest: boolean;
  {-self test for strings from Whirlpool distribution}
const
  sex3: string[03] = 'abc';
  sex7: string[80] = '12345678901234567890123456789012345678901234567890123456789012345678901234567890';

const
  {Whirlpool Version 3 test vectors} {abc'}
  DEX3: TWhirlDigest = ($4e,$24,$48,$a4,$c6,$f4,$86,$bb,
                        $16,$b6,$56,$2c,$73,$b4,$02,$0b,
                        $f3,$04,$3e,$3a,$73,$1b,$ce,$72,
                        $1a,$e1,$b3,$03,$d9,$7e,$6d,$4c,
                        $71,$81,$ee,$bd,$b6,$c5,$7e,$27,
                        $7d,$0e,$34,$95,$71,$14,$cb,$d6,
                        $c7,$97,$fc,$9d,$95,$d8,$b5,$82,
                        $d2,$25,$29,$20,$76,$d4,$ee,$f5);

  {'12345678901234567890123456789012345678901234567890123456789012345678901234567890'}
  DEX7: TWhirlDigest = ($46,$6e,$f1,$8b,$ab,$b0,$15,$4d,
                        $25,$b9,$d3,$8a,$64,$14,$f5,$c0,
                        $87,$84,$37,$2b,$cc,$b2,$04,$d6,
                        $54,$9c,$4a,$fa,$db,$60,$14,$29,
                        $4d,$5b,$d8,$df,$2a,$6c,$44,$e5,
                        $38,$cd,$04,$7b,$26,$81,$a5,$1a,
                        $2c,$60,$48,$1e,$88,$c5,$a2,$0b,
                        $2c,$2a,$80,$cf,$3a,$9a,$08,$3b);

  {nessie-test-vectors.txt, L=1}
  D3: TWhirlDigest   = ($e3,$84,$d5,$40,$e0,$bd,$fd,$28,
                        $c8,$52,$91,$77,$34,$3b,$31,$18,
                        $3f,$b4,$0c,$20,$f9,$60,$b0,$bc,
                        $dc,$e0,$51,$3a,$38,$2f,$96,$a3,
                        $83,$20,$99,$eb,$b6,$aa,$bd,$b7,
                        $1b,$0e,$a2,$e3,$01,$77,$f6,$98,
                        $ea,$70,$3d,$e5,$1f,$93,$cf,$3c,
                        $fe,$a6,$d3,$17,$1b,$95,$53,$83);
var
  Context: THashContext;
  Digest : TWhirlDigest;

  function SingleTest(s: Str127; TDig: TWhirlDigest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    i: integer;
  begin
    SingleTest := false;
    {1. Hash complete string}
    Whirl_Full(Digest, @s[1],length(s));
    {Compare with known value}
    if not HashSameDigest(@Whirl_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    {2. one update call for all chars}
    Whirl_Init(Context);
    for i:=1 to length(s) do Whirl_Update(Context,@s[i],1);
    Whirl_Final(Context,Digest);
    {Compare with known value}
    if not HashSameDigest(@Whirl_Desc, PHashDigest(@Digest), PHashDigest(@TDig)) then exit;
    SingleTest := true;
  end;

begin
  Whirl_SelfTest := false;
  {1 Zero bit from nessie-test-vectors.txt}
  Whirl_Init(Context);
  Whirl_FinalBits(Context,Digest,0,1);
  if not HashSameDigest(@Whirl_Desc, PHashDigest(@Digest), PHashDigest(@D3)) then exit;
  {example xx strings from iso-test-vectors.txt}
  Whirl_SelfTest := SingleTest(sex3, DEX3) and SingleTest(sex7, DEX7)
end;


{---------------------------------------------------------------------------}
procedure Whirl_FullXL(var Digest: TWhirlDigest; Msg: pointer; Len: longint);
  {-Whirlpool hash-code of Msg with init/update/final}
var
  Context: THashContext;
begin
  Whirl_Init(Context);
  Whirl_UpdateXL(Context, Msg, Len);
  Whirl_Final(Context, Digest);
end;


{---------------------------------------------------------------------------}
procedure Whirl_Full(var Digest: TWhirlDigest; Msg: pointer; Len: word);
  {-Whirlpool hash-code of Msg with init/update/final}
begin
  Whirl_FullXL(Digest, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure Whirl_File({$ifdef CONST} const {$endif} fname: string;
                     var Digest: TWhirlDigest; var buf; bsize: word; var Err: word);
  {-Whirlpool hash-code of file, buf: buffer with at least bsize bytes}
var
  tmp: THashDigest;
begin
  HashFile(fname, @Whirl_Desc, tmp, buf, bsize, Err);
  move(tmp, Digest, sizeof(Digest));
end;


begin
  {$ifdef VER5X}
    fillchar(Whirl_Desc, sizeof(Whirl_Desc), 0);
    with Whirl_Desc do begin
       HSig      := C_HashSig;
       HDSize    := sizeof(THashDesc);
       HDVersion := C_HashVers;
       HBlockLen := Whirl_BlockLen;
       HDigestlen:= sizeof(TWhirlDigest);
       HInit     := Whirl_Init;
       HFinal    := Whirl_FinalEx;
       HUpdateXL := Whirl_UpdateXL;
       HAlgNum   := longint(_Whirlpool);
       HName     := 'Whirlpool';
       HPtrOID   := @Whirl_OID;
       HLenOID   := 6;
       HFinalBit := Whirl_FinalBitsEx;
    end;
  {$endif}
  RegisterHash(_Whirlpool, @Whirl_Desc);
end.
