unit CRC64;

{64 bit CRC with table, (ECMA-182 DLT1 format, PostgreSQL)}


interface

(*************************************************************************

 DESCRIPTION     :  64 bit CRC with table

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://www.ecma-international.org/publications/files/ecma-st/
                    Ecma-182.pdf: DLT1 spec  or  Ecma-231.pdf: DLT4 spec

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     31.08.03  W.Ehrhardt  Initial version based on bCRC64 layout
 2.10     31.08.03  we          Common vers., XL versions for Win32
 2.11     13.09.03  we          with {$ifdef DLL} stdcall ..
 2.20     27.09.03  we          FPC/go32v2
 2.30     05.10.03  we          STD.INC, TP5.0
 2.40     10.10.03  we          common version, english comments
 2.41     26.10.03  we          fix VP Basm quirk, use [ebx*scale]
 3.00     01.12.03  we          Common version 3.0
 3.01     25.12.03  we          swap instructions in main loop
 3.02     26.02.05  we          With {$ifdef StrictLong}
 3.03     05.05.05  we          $R- for StrictLong, D9: errors if $R+ even if warnings off
 3.04     17.12.05  we          Force $I- in CRC64File
 3.05     07.08.06  we          $ifdef BIT32: (const fname: shortstring...)
 3.06     10.02.07  we          CRC54File: no eof, XL and filemode via $ifdef
 3.07     29.06.07  we          BASM16: align helpers
 3.08     04.10.07  we          FPC: {$asmmode intel}
 3.09     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 3.10     19.07.09  we          D12 fix: assign with typecast string(fname)
 3.11     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 3.12     26.12.12  we          D17 and PurePascal
 3.13     15.08.15  we          64-bit shifts and table if $ifdef BIT64
 3.14     16.08.15  we          Removed $ifdef DLL / stdcall
 3.15     29.11.17  we          CRC64File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2002-2017 Wolfgang Ehrhardt

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

uses BTypes;

type
  TCRC64 = packed record
             lo32, hi32: longint;
           end;

procedure CRC64Init(var CRC: TCRC64);
  {-CRC64 initialization}

procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}

procedure CRC64Final(var CRC: TCRC64);
  {-CRC64: finalize calculation}

function  CRC64SelfTest: boolean;
  {-Self test for CRC64}

procedure CRC64Full(var CRC: TCRC64; Msg: pointer; Len: word);
  {-CRC64 of Msg with init/update/final}

procedure CRC64File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: TCRC64; var buf; bsize: word; var Err: word);
  {-CRC64 of file, buf: buffer with at least bsize bytes}

{$ifndef BIT16}
procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
  {-update CRC64 with Msg data}

procedure CRC64FullXL(var CRC: TCRC64; Msg: pointer; Len: longint);
  {-CRC64 of Msg with init/update/final}
{$endif}



implementation

const
  Mask64 : TCRC64  = (lo32:-1; hi32:-1);


{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}


(*************************************************************************
T_CTab64 - CRC64 table calculation     (c) 2002-2004 W.Ehrhardt

Calculate CRC64 tables for polynomial:

x^64 + x^62 + x^57 + x^55 + x^54 + x^53 + x^52 + x^47 + x^46 + x^45 +
x^40 + x^39 + x^38 + x^37 + x^35 + x^33 + x^32 + x^31 + x^29 + x^27 +
x^24 + x^23 + x^22 + x^21 + x^19 + x^17 + x^13 + x^12 + x^10 + x^9  +
x^7  + x^4  + x^1  + 1

const
  PolyLo = $A9EA3693;
  PolyHi = $42F0E1EB;
*************************************************************************)


{$ifdef BASM16}
  {$i ALIGN.INC}
{$endif}


{$ifdef BIT64}
type
  TW64    = packed record
              L,H: longint;
            end;

{Use the 32-bit record construct for uint64 constant because}
{Delphi 2 does not compile even though code is not used and }
{some FPC do not know uint64 constants                      }

const
  Tab64 : packed array[0..255] of TW64 = (
    (L:$00000000; H:$00000000), (L:$a9ea3693; H:$42f0e1eb), (L:$53d46d26; H:$85e1c3d7), (L:$fa3e5bb5; H:$c711223c),
    (L:$0e42ecdf; H:$49336645), (L:$a7a8da4c; H:$0bc387ae), (L:$5d9681f9; H:$ccd2a592), (L:$f47cb76a; H:$8e224479),
    (L:$1c85d9be; H:$9266cc8a), (L:$b56fef2d; H:$d0962d61), (L:$4f51b498; H:$17870f5d), (L:$e6bb820b; H:$5577eeb6),
    (L:$12c73561; H:$db55aacf), (L:$bb2d03f2; H:$99a54b24), (L:$41135847; H:$5eb46918), (L:$e8f96ed4; H:$1c4488f3),
    (L:$90e185ef; H:$663d78ff), (L:$390bb37c; H:$24cd9914), (L:$c335e8c9; H:$e3dcbb28), (L:$6adfde5a; H:$a12c5ac3),
    (L:$9ea36930; H:$2f0e1eba), (L:$37495fa3; H:$6dfeff51), (L:$cd770416; H:$aaefdd6d), (L:$649d3285; H:$e81f3c86),
    (L:$8c645c51; H:$f45bb475), (L:$258e6ac2; H:$b6ab559e), (L:$dfb03177; H:$71ba77a2), (L:$765a07e4; H:$334a9649),
    (L:$8226b08e; H:$bd68d230), (L:$2bcc861d; H:$ff9833db), (L:$d1f2dda8; H:$388911e7), (L:$7818eb3b; H:$7a79f00c),
    (L:$21c30bde; H:$cc7af1ff), (L:$88293d4d; H:$8e8a1014), (L:$721766f8; H:$499b3228), (L:$dbfd506b; H:$0b6bd3c3),
    (L:$2f81e701; H:$854997ba), (L:$866bd192; H:$c7b97651), (L:$7c558a27; H:$00a8546d), (L:$d5bfbcb4; H:$4258b586),
    (L:$3d46d260; H:$5e1c3d75), (L:$94ace4f3; H:$1cecdc9e), (L:$6e92bf46; H:$dbfdfea2), (L:$c77889d5; H:$990d1f49),
    (L:$33043ebf; H:$172f5b30), (L:$9aee082c; H:$55dfbadb), (L:$60d05399; H:$92ce98e7), (L:$c93a650a; H:$d03e790c),
    (L:$b1228e31; H:$aa478900), (L:$18c8b8a2; H:$e8b768eb), (L:$e2f6e317; H:$2fa64ad7), (L:$4b1cd584; H:$6d56ab3c),
    (L:$bf6062ee; H:$e374ef45), (L:$168a547d; H:$a1840eae), (L:$ecb40fc8; H:$66952c92), (L:$455e395b; H:$2465cd79),
    (L:$ada7578f; H:$3821458a), (L:$044d611c; H:$7ad1a461), (L:$fe733aa9; H:$bdc0865d), (L:$57990c3a; H:$ff3067b6),
    (L:$a3e5bb50; H:$711223cf), (L:$0a0f8dc3; H:$33e2c224), (L:$f031d676; H:$f4f3e018), (L:$59dbe0e5; H:$b60301f3),
    (L:$ea6c212f; H:$da050215), (L:$438617bc; H:$98f5e3fe), (L:$b9b84c09; H:$5fe4c1c2), (L:$10527a9a; H:$1d142029),
    (L:$e42ecdf0; H:$93366450), (L:$4dc4fb63; H:$d1c685bb), (L:$b7faa0d6; H:$16d7a787), (L:$1e109645; H:$5427466c),
    (L:$f6e9f891; H:$4863ce9f), (L:$5f03ce02; H:$0a932f74), (L:$a53d95b7; H:$cd820d48), (L:$0cd7a324; H:$8f72eca3),
    (L:$f8ab144e; H:$0150a8da), (L:$514122dd; H:$43a04931), (L:$ab7f7968; H:$84b16b0d), (L:$02954ffb; H:$c6418ae6),
    (L:$7a8da4c0; H:$bc387aea), (L:$d3679253; H:$fec89b01), (L:$2959c9e6; H:$39d9b93d), (L:$80b3ff75; H:$7b2958d6),
    (L:$74cf481f; H:$f50b1caf), (L:$dd257e8c; H:$b7fbfd44), (L:$271b2539; H:$70eadf78), (L:$8ef113aa; H:$321a3e93),
    (L:$66087d7e; H:$2e5eb660), (L:$cfe24bed; H:$6cae578b), (L:$35dc1058; H:$abbf75b7), (L:$9c3626cb; H:$e94f945c),
    (L:$684a91a1; H:$676dd025), (L:$c1a0a732; H:$259d31ce), (L:$3b9efc87; H:$e28c13f2), (L:$9274ca14; H:$a07cf219),
    (L:$cbaf2af1; H:$167ff3ea), (L:$62451c62; H:$548f1201), (L:$987b47d7; H:$939e303d), (L:$31917144; H:$d16ed1d6),
    (L:$c5edc62e; H:$5f4c95af), (L:$6c07f0bd; H:$1dbc7444), (L:$9639ab08; H:$daad5678), (L:$3fd39d9b; H:$985db793),
    (L:$d72af34f; H:$84193f60), (L:$7ec0c5dc; H:$c6e9de8b), (L:$84fe9e69; H:$01f8fcb7), (L:$2d14a8fa; H:$43081d5c),
    (L:$d9681f90; H:$cd2a5925), (L:$70822903; H:$8fdab8ce), (L:$8abc72b6; H:$48cb9af2), (L:$23564425; H:$0a3b7b19),
    (L:$5b4eaf1e; H:$70428b15), (L:$f2a4998d; H:$32b26afe), (L:$089ac238; H:$f5a348c2), (L:$a170f4ab; H:$b753a929),
    (L:$550c43c1; H:$3971ed50), (L:$fce67552; H:$7b810cbb), (L:$06d82ee7; H:$bc902e87), (L:$af321874; H:$fe60cf6c),
    (L:$47cb76a0; H:$e224479f), (L:$ee214033; H:$a0d4a674), (L:$141f1b86; H:$67c58448), (L:$bdf52d15; H:$253565a3),
    (L:$49899a7f; H:$ab1721da), (L:$e063acec; H:$e9e7c031), (L:$1a5df759; H:$2ef6e20d), (L:$b3b7c1ca; H:$6c0603e6),
    (L:$7d3274cd; H:$f6fae5c0), (L:$d4d8425e; H:$b40a042b), (L:$2ee619eb; H:$731b2617), (L:$870c2f78; H:$31ebc7fc),
    (L:$73709812; H:$bfc98385), (L:$da9aae81; H:$fd39626e), (L:$20a4f534; H:$3a284052), (L:$894ec3a7; H:$78d8a1b9),
    (L:$61b7ad73; H:$649c294a), (L:$c85d9be0; H:$266cc8a1), (L:$3263c055; H:$e17dea9d), (L:$9b89f6c6; H:$a38d0b76),
    (L:$6ff541ac; H:$2daf4f0f), (L:$c61f773f; H:$6f5faee4), (L:$3c212c8a; H:$a84e8cd8), (L:$95cb1a19; H:$eabe6d33),
    (L:$edd3f122; H:$90c79d3f), (L:$4439c7b1; H:$d2377cd4), (L:$be079c04; H:$15265ee8), (L:$17edaa97; H:$57d6bf03),
    (L:$e3911dfd; H:$d9f4fb7a), (L:$4a7b2b6e; H:$9b041a91), (L:$b04570db; H:$5c1538ad), (L:$19af4648; H:$1ee5d946),
    (L:$f156289c; H:$02a151b5), (L:$58bc1e0f; H:$4051b05e), (L:$a28245ba; H:$87409262), (L:$0b687329; H:$c5b07389),
    (L:$ff14c443; H:$4b9237f0), (L:$56fef2d0; H:$0962d61b), (L:$acc0a965; H:$ce73f427), (L:$052a9ff6; H:$8c8315cc),
    (L:$5cf17f13; H:$3a80143f), (L:$f51b4980; H:$7870f5d4), (L:$0f251235; H:$bf61d7e8), (L:$a6cf24a6; H:$fd913603),
    (L:$52b393cc; H:$73b3727a), (L:$fb59a55f; H:$31439391), (L:$0167feea; H:$f652b1ad), (L:$a88dc879; H:$b4a25046),
    (L:$4074a6ad; H:$a8e6d8b5), (L:$e99e903e; H:$ea16395e), (L:$13a0cb8b; H:$2d071b62), (L:$ba4afd18; H:$6ff7fa89),
    (L:$4e364a72; H:$e1d5bef0), (L:$e7dc7ce1; H:$a3255f1b), (L:$1de22754; H:$64347d27), (L:$b40811c7; H:$26c49ccc),
    (L:$cc10fafc; H:$5cbd6cc0), (L:$65facc6f; H:$1e4d8d2b), (L:$9fc497da; H:$d95caf17), (L:$362ea149; H:$9bac4efc),
    (L:$c2521623; H:$158e0a85), (L:$6bb820b0; H:$577eeb6e), (L:$91867b05; H:$906fc952), (L:$386c4d96; H:$d29f28b9),
    (L:$d0952342; H:$cedba04a), (L:$797f15d1; H:$8c2b41a1), (L:$83414e64; H:$4b3a639d), (L:$2aab78f7; H:$09ca8276),
    (L:$ded7cf9d; H:$87e8c60f), (L:$773df90e; H:$c51827e4), (L:$8d03a2bb; H:$020905d8), (L:$24e99428; H:$40f9e433),
    (L:$975e55e2; H:$2cffe7d5), (L:$3eb46371; H:$6e0f063e), (L:$c48a38c4; H:$a91e2402), (L:$6d600e57; H:$ebeec5e9),
    (L:$991cb93d; H:$65cc8190), (L:$30f68fae; H:$273c607b), (L:$cac8d41b; H:$e02d4247), (L:$6322e288; H:$a2dda3ac),
    (L:$8bdb8c5c; H:$be992b5f), (L:$2231bacf; H:$fc69cab4), (L:$d80fe17a; H:$3b78e888), (L:$71e5d7e9; H:$79880963),
    (L:$85996083; H:$f7aa4d1a), (L:$2c735610; H:$b55aacf1), (L:$d64d0da5; H:$724b8ecd), (L:$7fa73b36; H:$30bb6f26),
    (L:$07bfd00d; H:$4ac29f2a), (L:$ae55e69e; H:$08327ec1), (L:$546bbd2b; H:$cf235cfd), (L:$fd818bb8; H:$8dd3bd16),
    (L:$09fd3cd2; H:$03f1f96f), (L:$a0170a41; H:$41011884), (L:$5a2951f4; H:$86103ab8), (L:$f3c36767; H:$c4e0db53),
    (L:$1b3a09b3; H:$d8a453a0), (L:$b2d03f20; H:$9a54b24b), (L:$48ee6495; H:$5d459077), (L:$e1045206; H:$1fb5719c),
    (L:$1578e56c; H:$919735e5), (L:$bc92d3ff; H:$d367d40e), (L:$46ac884a; H:$1476f632), (L:$ef46bed9; H:$568617d9),
    (L:$b69d5e3c; H:$e085162a), (L:$1f7768af; H:$a275f7c1), (L:$e549331a; H:$6564d5fd), (L:$4ca30589; H:$27943416),
    (L:$b8dfb2e3; H:$a9b6706f), (L:$11358470; H:$eb469184), (L:$eb0bdfc5; H:$2c57b3b8), (L:$42e1e956; H:$6ea75253),
    (L:$aa188782; H:$72e3daa0), (L:$03f2b111; H:$30133b4b), (L:$f9cceaa4; H:$f7021977), (L:$5026dc37; H:$b5f2f89c),
    (L:$a45a6b5d; H:$3bd0bce5), (L:$0db05dce; H:$79205d0e), (L:$f78e067b; H:$be317f32), (L:$5e6430e8; H:$fcc19ed9),
    (L:$267cdbd3; H:$86b86ed5), (L:$8f96ed40; H:$c4488f3e), (L:$75a8b6f5; H:$0359ad02), (L:$dc428066; H:$41a94ce9),
    (L:$283e370c; H:$cf8b0890), (L:$81d4019f; H:$8d7be97b), (L:$7bea5a2a; H:$4a6acb47), (L:$d2006cb9; H:$089a2aac),
    (L:$3af9026d; H:$14dea25f), (L:$931334fe; H:$562e43b4), (L:$692d6f4b; H:$913f6188), (L:$c0c759d8; H:$d3cf8063),
    (L:$34bbeeb2; H:$5dedc41a), (L:$9d51d821; H:$1f1d25f1), (L:$676f8394; H:$d80c07cd), (L:$ce85b507; H:$9afce626));
{$else}

const

  {$ifdef BASM16}
    {$ifdef A4_CRC64}
      AlignDummy_CRC64: word = 0;
    {$endif}
  {$endif}

  Tab64lo : array[0..255] of longint = (
    $00000000,$a9ea3693,$53d46d26,$fa3e5bb5,$0e42ecdf,$a7a8da4c,$5d9681f9,$f47cb76a,
    $1c85d9be,$b56fef2d,$4f51b498,$e6bb820b,$12c73561,$bb2d03f2,$41135847,$e8f96ed4,
    $90e185ef,$390bb37c,$c335e8c9,$6adfde5a,$9ea36930,$37495fa3,$cd770416,$649d3285,
    $8c645c51,$258e6ac2,$dfb03177,$765a07e4,$8226b08e,$2bcc861d,$d1f2dda8,$7818eb3b,
    $21c30bde,$88293d4d,$721766f8,$dbfd506b,$2f81e701,$866bd192,$7c558a27,$d5bfbcb4,
    $3d46d260,$94ace4f3,$6e92bf46,$c77889d5,$33043ebf,$9aee082c,$60d05399,$c93a650a,
    $b1228e31,$18c8b8a2,$e2f6e317,$4b1cd584,$bf6062ee,$168a547d,$ecb40fc8,$455e395b,
    $ada7578f,$044d611c,$fe733aa9,$57990c3a,$a3e5bb50,$0a0f8dc3,$f031d676,$59dbe0e5,
    $ea6c212f,$438617bc,$b9b84c09,$10527a9a,$e42ecdf0,$4dc4fb63,$b7faa0d6,$1e109645,
    $f6e9f891,$5f03ce02,$a53d95b7,$0cd7a324,$f8ab144e,$514122dd,$ab7f7968,$02954ffb,
    $7a8da4c0,$d3679253,$2959c9e6,$80b3ff75,$74cf481f,$dd257e8c,$271b2539,$8ef113aa,
    $66087d7e,$cfe24bed,$35dc1058,$9c3626cb,$684a91a1,$c1a0a732,$3b9efc87,$9274ca14,
    $cbaf2af1,$62451c62,$987b47d7,$31917144,$c5edc62e,$6c07f0bd,$9639ab08,$3fd39d9b,
    $d72af34f,$7ec0c5dc,$84fe9e69,$2d14a8fa,$d9681f90,$70822903,$8abc72b6,$23564425,
    $5b4eaf1e,$f2a4998d,$089ac238,$a170f4ab,$550c43c1,$fce67552,$06d82ee7,$af321874,
    $47cb76a0,$ee214033,$141f1b86,$bdf52d15,$49899a7f,$e063acec,$1a5df759,$b3b7c1ca,
    $7d3274cd,$d4d8425e,$2ee619eb,$870c2f78,$73709812,$da9aae81,$20a4f534,$894ec3a7,
    $61b7ad73,$c85d9be0,$3263c055,$9b89f6c6,$6ff541ac,$c61f773f,$3c212c8a,$95cb1a19,
    $edd3f122,$4439c7b1,$be079c04,$17edaa97,$e3911dfd,$4a7b2b6e,$b04570db,$19af4648,
    $f156289c,$58bc1e0f,$a28245ba,$0b687329,$ff14c443,$56fef2d0,$acc0a965,$052a9ff6,
    $5cf17f13,$f51b4980,$0f251235,$a6cf24a6,$52b393cc,$fb59a55f,$0167feea,$a88dc879,
    $4074a6ad,$e99e903e,$13a0cb8b,$ba4afd18,$4e364a72,$e7dc7ce1,$1de22754,$b40811c7,
    $cc10fafc,$65facc6f,$9fc497da,$362ea149,$c2521623,$6bb820b0,$91867b05,$386c4d96,
    $d0952342,$797f15d1,$83414e64,$2aab78f7,$ded7cf9d,$773df90e,$8d03a2bb,$24e99428,
    $975e55e2,$3eb46371,$c48a38c4,$6d600e57,$991cb93d,$30f68fae,$cac8d41b,$6322e288,
    $8bdb8c5c,$2231bacf,$d80fe17a,$71e5d7e9,$85996083,$2c735610,$d64d0da5,$7fa73b36,
    $07bfd00d,$ae55e69e,$546bbd2b,$fd818bb8,$09fd3cd2,$a0170a41,$5a2951f4,$f3c36767,
    $1b3a09b3,$b2d03f20,$48ee6495,$e1045206,$1578e56c,$bc92d3ff,$46ac884a,$ef46bed9,
    $b69d5e3c,$1f7768af,$e549331a,$4ca30589,$b8dfb2e3,$11358470,$eb0bdfc5,$42e1e956,
    $aa188782,$03f2b111,$f9cceaa4,$5026dc37,$a45a6b5d,$0db05dce,$f78e067b,$5e6430e8,
    $267cdbd3,$8f96ed40,$75a8b6f5,$dc428066,$283e370c,$81d4019f,$7bea5a2a,$d2006cb9,
    $3af9026d,$931334fe,$692d6f4b,$c0c759d8,$34bbeeb2,$9d51d821,$676f8394,$ce85b507);


  Tab64hi : array[0..255] of longint = (
    $00000000,$42f0e1eb,$85e1c3d7,$c711223c,$49336645,$0bc387ae,$ccd2a592,$8e224479,
    $9266cc8a,$d0962d61,$17870f5d,$5577eeb6,$db55aacf,$99a54b24,$5eb46918,$1c4488f3,
    $663d78ff,$24cd9914,$e3dcbb28,$a12c5ac3,$2f0e1eba,$6dfeff51,$aaefdd6d,$e81f3c86,
    $f45bb475,$b6ab559e,$71ba77a2,$334a9649,$bd68d230,$ff9833db,$388911e7,$7a79f00c,
    $cc7af1ff,$8e8a1014,$499b3228,$0b6bd3c3,$854997ba,$c7b97651,$00a8546d,$4258b586,
    $5e1c3d75,$1cecdc9e,$dbfdfea2,$990d1f49,$172f5b30,$55dfbadb,$92ce98e7,$d03e790c,
    $aa478900,$e8b768eb,$2fa64ad7,$6d56ab3c,$e374ef45,$a1840eae,$66952c92,$2465cd79,
    $3821458a,$7ad1a461,$bdc0865d,$ff3067b6,$711223cf,$33e2c224,$f4f3e018,$b60301f3,
    $da050215,$98f5e3fe,$5fe4c1c2,$1d142029,$93366450,$d1c685bb,$16d7a787,$5427466c,
    $4863ce9f,$0a932f74,$cd820d48,$8f72eca3,$0150a8da,$43a04931,$84b16b0d,$c6418ae6,
    $bc387aea,$fec89b01,$39d9b93d,$7b2958d6,$f50b1caf,$b7fbfd44,$70eadf78,$321a3e93,
    $2e5eb660,$6cae578b,$abbf75b7,$e94f945c,$676dd025,$259d31ce,$e28c13f2,$a07cf219,
    $167ff3ea,$548f1201,$939e303d,$d16ed1d6,$5f4c95af,$1dbc7444,$daad5678,$985db793,
    $84193f60,$c6e9de8b,$01f8fcb7,$43081d5c,$cd2a5925,$8fdab8ce,$48cb9af2,$0a3b7b19,
    $70428b15,$32b26afe,$f5a348c2,$b753a929,$3971ed50,$7b810cbb,$bc902e87,$fe60cf6c,
    $e224479f,$a0d4a674,$67c58448,$253565a3,$ab1721da,$e9e7c031,$2ef6e20d,$6c0603e6,
    $f6fae5c0,$b40a042b,$731b2617,$31ebc7fc,$bfc98385,$fd39626e,$3a284052,$78d8a1b9,
    $649c294a,$266cc8a1,$e17dea9d,$a38d0b76,$2daf4f0f,$6f5faee4,$a84e8cd8,$eabe6d33,
    $90c79d3f,$d2377cd4,$15265ee8,$57d6bf03,$d9f4fb7a,$9b041a91,$5c1538ad,$1ee5d946,
    $02a151b5,$4051b05e,$87409262,$c5b07389,$4b9237f0,$0962d61b,$ce73f427,$8c8315cc,
    $3a80143f,$7870f5d4,$bf61d7e8,$fd913603,$73b3727a,$31439391,$f652b1ad,$b4a25046,
    $a8e6d8b5,$ea16395e,$2d071b62,$6ff7fa89,$e1d5bef0,$a3255f1b,$64347d27,$26c49ccc,
    $5cbd6cc0,$1e4d8d2b,$d95caf17,$9bac4efc,$158e0a85,$577eeb6e,$906fc952,$d29f28b9,
    $cedba04a,$8c2b41a1,$4b3a639d,$09ca8276,$87e8c60f,$c51827e4,$020905d8,$40f9e433,
    $2cffe7d5,$6e0f063e,$a91e2402,$ebeec5e9,$65cc8190,$273c607b,$e02d4247,$a2dda3ac,
    $be992b5f,$fc69cab4,$3b78e888,$79880963,$f7aa4d1a,$b55aacf1,$724b8ecd,$30bb6f26,
    $4ac29f2a,$08327ec1,$cf235cfd,$8dd3bd16,$03f1f96f,$41011884,$86103ab8,$c4e0db53,
    $d8a453a0,$9a54b24b,$5d459077,$1fb5719c,$919735e5,$d367d40e,$1476f632,$568617d9,
    $e085162a,$a275f7c1,$6564d5fd,$27943416,$a9b6706f,$eb469184,$2c57b3b8,$6ea75253,
    $72e3daa0,$30133b4b,$f7021977,$b5f2f89c,$3bd0bce5,$79205d0e,$be317f32,$fcc19ed9,
    $86b86ed5,$c4488f3e,$0359ad02,$41a94ce9,$cf8b0890,$8d7be97b,$4a6acb47,$089a2aac,
    $14dea25f,$562e43b4,$913f6188,$d3cf8063,$5dedc41a,$1f1d25f1,$d80c07cd,$9afce626);


{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}
{$endif} {BIT64}



{$ifndef BIT16}

(**** 32+ Bit Delphi2+/FPC/VP *****)

{$ifdef PurePascal}
  {$ifdef BIT64}
    {---------------------------------------------------------------------------}
    procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
      {-update CRC64 with Msg data}
    var
      i: longint;
      lmp: pByte;
      c64: uint64;
    begin
      c64 := uint64(CRC);
      lmp := pByte(msg);
      for i:=1 to Len do begin
        c64 := uint64(Tab64[(c64 shr 56) xor lmp^]) xor (c64 shl 8);
        inc(lmp);
      end;
      uint64(CRC) := c64;
    end;
  {$else}
    {---------------------------------------------------------------------------}
    procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
      {-update CRC64 with Msg data}
    var
      i,it: longint;
      clo,chi: longint;
    type
      BR = packed record
             b0,b1,b2,b3: byte;
           end;
    begin
      clo := CRC.lo32;
      chi := CRC.hi32;
      for i:=1 to Len do begin
        {c64 := Tab64[(c64 shr 56) xor Msg^] xor (c64 shl 8)}
        it := BR(chi).b3 xor PByte(Msg)^;   {index in tables}
        chi := chi shl 8;
        BR(chi).b0 := BR(clo).b3;
        chi := chi xor Tab64Hi[it];
        clo := (clo shl 8) xor Tab64Lo[it];
        inc(Ptr2Inc(Msg));
      end;
      CRC.lo32 := clo;
      CRC.hi32 := chi;
    end;
  {$endif}

{$else}
  {---------------------------------------------------------------------------}
  procedure CRC64UpdateXL(var CRC: TCRC64; Msg: pointer; Len: longint);
    {-update CRC64 with Msg data}
  begin
    asm
         push  ebx
         push  esi
         mov   ecx,[Len]
         jecxz @@4

         mov   eax,[CRC]
         mov   edx,[eax+4]
         mov   eax,[eax]
         mov   esi,[Msg]

        {c64 := Tab64[(c64 shr 56) xor Msg^] xor (c64 shl 8)}

    @@1: mov   ebx,edx
         shr   ebx,24
         xor   bl, byte ptr [esi]
         inc   esi
         shld  edx,eax,8
         shl   eax,8
         xor   edx,dword ptr Tab64Hi[ebx*4]
         xor   eax,dword ptr Tab64Lo[ebx*4]
         dec   ecx
         jnz   @@1

         mov   ebx,[CRC]
         mov   [ebx],eax
         mov   [ebx+4],edx
    @@4: pop   esi
         pop   ebx
    end;
  end;
{$endif}




{---------------------------------------------------------------------------}
procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}
begin
  CRC64UpdateXL(CRC, Msg, Len);
end;


{$else}


(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word);
  {-update CRC64 with Msg data}
var
  i,it: word;
  clo,chi: longint;
type
  BR = packed record
         b0,b1,b2,b3: byte;
       end;
begin
  clo := CRC.lo32;
  chi := CRC.hi32;
  for i:=1 to Len do begin
    {c64 := Tab64[(c64 shr 56) xor Msg^] xor (c64 shl 8)}
    it := BR(chi).b3 xor pByte(Msg)^;   {index in tables}
    chi := chi shl 8;
    BR(chi).b0 := BR(clo).b3;
    chi := chi xor Tab64Hi[it];
    clo := (clo shl 8) xor Tab64Lo[it];
    inc(Ptr2Inc(Msg));
  end;
  CRC.lo32 := clo;
  CRC.hi32 := chi;
end;

{$else}

{TP 6/7/Delphi1}

{---------------------------------------------------------------------------}
procedure CRC64Update(var CRC: TCRC64; Msg: pointer; Len: word); assembler;
  {-update CRC64 with Msg data}
asm
               les     si,[CRC]
       db $66; mov     ax,es:[si]
       db $66; mov     dx,es:[si+4]
               les     si,[Msg]
               mov     cx,[len]
               or      cx,cx
               jz      @@4

       {c64 := Tab64[(c64 shr 56) xor Msg^] xor (c64 shl 8)}

  @@1: db $66; mov     bx,dx
       db $66; shr     bx,24
               xor     bl,es:[si]           {= (c64 shr 56) xor Msg^}
               inc     si
               shl     bx,2                 {index in tables}
       db $66, $0F,$A4,$C2,$08              {shld edx,eax,8}
       db $66; shl     ax,8
       db $66; xor     dx,word ptr Tab64Hi[bx]
       db $66; xor     ax,word ptr Tab64Lo[bx]
               dec     cx
               jnz     @@1

               les     si,CRC               {store result}
       db $66; mov     es:[si],ax
       db $66; mov     es:[si+4],dx

  @@4:
end;


{$endif BASM16}
{$endif BIT16}


{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure CRC64FullXL(var CRC: TCRC64; Msg: pointer; Len: longint);
  {-CRC64 of Msg with init/update/final}
begin
  CRC64Init(CRC);
  CRC64UpdateXL(CRC, Msg, Len);
  CRC64Final(CRC);
end;
{$endif}


{---------------------------------------------------------------------------}
procedure CRC64Init(var CRC: TCRC64);
  {-CRC64 initialization}
begin
  CRC := Mask64;
end;


{---------------------------------------------------------------------------}
procedure CRC64Final(var CRC: TCRC64);
  {-CRC64: finalize calculation}
begin
  CRC.lo32 := CRC.lo32 xor Mask64.lo32;
  CRC.hi32 := CRC.hi32 xor Mask64.hi32;
end;


{---------------------------------------------------------------------------}
function  CRC64SelfTest: boolean;
  {-Self test for CRC64}
const
  s: string[20] = '123456789';
  CHKhi: longint = longint($62EC59E3);
  CHKlo: longint = longint($F1A4F00A);
var
  CRC: TCRC64;
begin
  CRC64Init(CRC);
  CRC64Update(CRC, @s[1], length(s));
  CRC64Final(CRC);
  CRC64SelfTest := (CRC.lo32=CHKlo) and (CRC.hi32=CHKhi);
end;


{---------------------------------------------------------------------------}
procedure CRC64Full(var CRC: TCRC64; Msg: pointer; Len: word);
  {-CRC64 of Msg with init/update/final}
begin
  CRC64Init(CRC);
  CRC64Update(CRC, Msg, Len);
  CRC64Final(CRC);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure CRC64File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: TCRC64; var buf; bsize: word; var Err: word);
  {-CRC64 of file, buf: buffer with at least bsize bytes}
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
  f: file;
begin
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
  CRC64Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      CRC64UpdateXL(CRC, @buf, L);
    {$else}
      CRC64Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  CRC64Final(CRC);
end;

{$ifdef DumpAlign}
begin
  writeln('Align  CRC64: ',ofs(Tab64lo) and 3:2, ofs(Tab64hi) and 3:2);
{$endif}

end.
