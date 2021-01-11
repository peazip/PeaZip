unit FCRC32;

{32 Bit CRC, fast 4KB table version, polynomial (used in zip and others)}


interface

(*************************************************************************

 DESCRIPTION     :  32 Bit CRC

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] zlib 1.2.x CRC32 routines (http://www.gzip.org/zlib/)
                    [2] http://www.intel.com/technology/comms/perfnet/download/CRC_generators.pdf

 REMARKS         :  - A note for FPC users: The current implementation is for
                      little-endian processors only! Users interested in
                      big-endian endian code should send an e-mail.
                    - intel papers ([2] and two others) from mid. 2005 do NOT
                      mention the zlib 'slicing by 4' routines from Oct/Nov 2003.
                      For a discussion see e.g. http://www.c10n.info/archives/383/

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     27.06.07  W.Ehrhardt  Initial pure single Pascal version
 0.11     27.06.07  we          BIT16 version with local array[0..3] of byte
 0.12     28.06.07  we          BASM16 version
 0.13     28.06.07  we          Leading/trailing single byte processing
 0.14     28.06.07  we          BASM32 version
 0.15     29.06.07  we          BASM16: align helpers
 0.16     30.06.07  we          PByte/PLong local for TP5x
 0.17     06.07.07  we          {$ifdef ENDIAN_BIG}, remarks and references
 0.18     07.07.07  we          Bugfix BIT16 leading single byte processing
 0.19     04.10.07  we          FPC: {$asmmode intel}
 0.20     06.09.08  we          Fix for FPC222: dest for jecxz out of range
 0.21     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255
 0.22     19.07.09  we          D12 fix: assign with typecast string(fname)
 0.23     08.03.12  we          {$ifndef BIT16} instead of {$ifdef WIN32}
 0.24     26.12.12  we          D17 and PurePascal
 0.25     16.08.15  we          Removed $ifdef DLL / stdcall
 0.26     29.11.17  we          FCRC32File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2007-2017 Wolfgang Ehrhardt

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

uses
  BTypes;

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

procedure FCRC32File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}


{$ifndef BIT16}
procedure FCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
  {-update CRC with Msg data}

procedure FCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
{$endif}


implementation


const
  Mask32 = longint($FFFFFFFF);

{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9 errors!}
{$endif}

{$ifdef BASM16}
  {$i ALIGN.INC}
{$endif}

const
  {$ifdef BASM16}
    {$ifdef A4_FCRC32}
      AlignDummy_FCRC32: word = 0;
    {$endif}
  {$endif}
  CTab0: array[0..255] of longint = (
    $00000000,$77073096,$ee0e612c,$990951ba,$076dc419,$706af48f,$e963a535,$9e6495a3,
    $0edb8832,$79dcb8a4,$e0d5e91e,$97d2d988,$09b64c2b,$7eb17cbd,$e7b82d07,$90bf1d91,
    $1db71064,$6ab020f2,$f3b97148,$84be41de,$1adad47d,$6ddde4eb,$f4d4b551,$83d385c7,
    $136c9856,$646ba8c0,$fd62f97a,$8a65c9ec,$14015c4f,$63066cd9,$fa0f3d63,$8d080df5,
    $3b6e20c8,$4c69105e,$d56041e4,$a2677172,$3c03e4d1,$4b04d447,$d20d85fd,$a50ab56b,
    $35b5a8fa,$42b2986c,$dbbbc9d6,$acbcf940,$32d86ce3,$45df5c75,$dcd60dcf,$abd13d59,
    $26d930ac,$51de003a,$c8d75180,$bfd06116,$21b4f4b5,$56b3c423,$cfba9599,$b8bda50f,
    $2802b89e,$5f058808,$c60cd9b2,$b10be924,$2f6f7c87,$58684c11,$c1611dab,$b6662d3d,
    $76dc4190,$01db7106,$98d220bc,$efd5102a,$71b18589,$06b6b51f,$9fbfe4a5,$e8b8d433,
    $7807c9a2,$0f00f934,$9609a88e,$e10e9818,$7f6a0dbb,$086d3d2d,$91646c97,$e6635c01,
    $6b6b51f4,$1c6c6162,$856530d8,$f262004e,$6c0695ed,$1b01a57b,$8208f4c1,$f50fc457,
    $65b0d9c6,$12b7e950,$8bbeb8ea,$fcb9887c,$62dd1ddf,$15da2d49,$8cd37cf3,$fbd44c65,
    $4db26158,$3ab551ce,$a3bc0074,$d4bb30e2,$4adfa541,$3dd895d7,$a4d1c46d,$d3d6f4fb,
    $4369e96a,$346ed9fc,$ad678846,$da60b8d0,$44042d73,$33031de5,$aa0a4c5f,$dd0d7cc9,
    $5005713c,$270241aa,$be0b1010,$c90c2086,$5768b525,$206f85b3,$b966d409,$ce61e49f,
    $5edef90e,$29d9c998,$b0d09822,$c7d7a8b4,$59b33d17,$2eb40d81,$b7bd5c3b,$c0ba6cad,
    $edb88320,$9abfb3b6,$03b6e20c,$74b1d29a,$ead54739,$9dd277af,$04db2615,$73dc1683,
    $e3630b12,$94643b84,$0d6d6a3e,$7a6a5aa8,$e40ecf0b,$9309ff9d,$0a00ae27,$7d079eb1,
    $f00f9344,$8708a3d2,$1e01f268,$6906c2fe,$f762575d,$806567cb,$196c3671,$6e6b06e7,
    $fed41b76,$89d32be0,$10da7a5a,$67dd4acc,$f9b9df6f,$8ebeeff9,$17b7be43,$60b08ed5,
    $d6d6a3e8,$a1d1937e,$38d8c2c4,$4fdff252,$d1bb67f1,$a6bc5767,$3fb506dd,$48b2364b,
    $d80d2bda,$af0a1b4c,$36034af6,$41047a60,$df60efc3,$a867df55,$316e8eef,$4669be79,
    $cb61b38c,$bc66831a,$256fd2a0,$5268e236,$cc0c7795,$bb0b4703,$220216b9,$5505262f,
    $c5ba3bbe,$b2bd0b28,$2bb45a92,$5cb36a04,$c2d7ffa7,$b5d0cf31,$2cd99e8b,$5bdeae1d,
    $9b64c2b0,$ec63f226,$756aa39c,$026d930a,$9c0906a9,$eb0e363f,$72076785,$05005713,
    $95bf4a82,$e2b87a14,$7bb12bae,$0cb61b38,$92d28e9b,$e5d5be0d,$7cdcefb7,$0bdbdf21,
    $86d3d2d4,$f1d4e242,$68ddb3f8,$1fda836e,$81be16cd,$f6b9265b,$6fb077e1,$18b74777,
    $88085ae6,$ff0f6a70,$66063bca,$11010b5c,$8f659eff,$f862ae69,$616bffd3,$166ccf45,
    $a00ae278,$d70dd2ee,$4e048354,$3903b3c2,$a7672661,$d06016f7,$4969474d,$3e6e77db,
    $aed16a4a,$d9d65adc,$40df0b66,$37d83bf0,$a9bcae53,$debb9ec5,$47b2cf7f,$30b5ffe9,
    $bdbdf21c,$cabac28a,$53b39330,$24b4a3a6,$bad03605,$cdd70693,$54de5729,$23d967bf,
    $b3667a2e,$c4614ab8,$5d681b02,$2a6f2b94,$b40bbe37,$c30c8ea1,$5a05df1b,$2d02ef8d);

  CTab1: array[0..255] of longint = (
    $00000000,$191b3141,$32366282,$2b2d53c3,$646cc504,$7d77f445,$565aa786,$4f4196c7,
    $c8d98a08,$d1c2bb49,$faefe88a,$e3f4d9cb,$acb54f0c,$b5ae7e4d,$9e832d8e,$87981ccf,
    $4ac21251,$53d92310,$78f470d3,$61ef4192,$2eaed755,$37b5e614,$1c98b5d7,$05838496,
    $821b9859,$9b00a918,$b02dfadb,$a936cb9a,$e6775d5d,$ff6c6c1c,$d4413fdf,$cd5a0e9e,
    $958424a2,$8c9f15e3,$a7b24620,$bea97761,$f1e8e1a6,$e8f3d0e7,$c3de8324,$dac5b265,
    $5d5daeaa,$44469feb,$6f6bcc28,$7670fd69,$39316bae,$202a5aef,$0b07092c,$121c386d,
    $df4636f3,$c65d07b2,$ed705471,$f46b6530,$bb2af3f7,$a231c2b6,$891c9175,$9007a034,
    $179fbcfb,$0e848dba,$25a9de79,$3cb2ef38,$73f379ff,$6ae848be,$41c51b7d,$58de2a3c,
    $f0794f05,$e9627e44,$c24f2d87,$db541cc6,$94158a01,$8d0ebb40,$a623e883,$bf38d9c2,
    $38a0c50d,$21bbf44c,$0a96a78f,$138d96ce,$5ccc0009,$45d73148,$6efa628b,$77e153ca,
    $babb5d54,$a3a06c15,$888d3fd6,$91960e97,$ded79850,$c7cca911,$ece1fad2,$f5facb93,
    $7262d75c,$6b79e61d,$4054b5de,$594f849f,$160e1258,$0f152319,$243870da,$3d23419b,
    $65fd6ba7,$7ce65ae6,$57cb0925,$4ed03864,$0191aea3,$188a9fe2,$33a7cc21,$2abcfd60,
    $ad24e1af,$b43fd0ee,$9f12832d,$8609b26c,$c94824ab,$d05315ea,$fb7e4629,$e2657768,
    $2f3f79f6,$362448b7,$1d091b74,$04122a35,$4b53bcf2,$52488db3,$7965de70,$607eef31,
    $e7e6f3fe,$fefdc2bf,$d5d0917c,$cccba03d,$838a36fa,$9a9107bb,$b1bc5478,$a8a76539,
    $3b83984b,$2298a90a,$09b5fac9,$10aecb88,$5fef5d4f,$46f46c0e,$6dd93fcd,$74c20e8c,
    $f35a1243,$ea412302,$c16c70c1,$d8774180,$9736d747,$8e2de606,$a500b5c5,$bc1b8484,
    $71418a1a,$685abb5b,$4377e898,$5a6cd9d9,$152d4f1e,$0c367e5f,$271b2d9c,$3e001cdd,
    $b9980012,$a0833153,$8bae6290,$92b553d1,$ddf4c516,$c4eff457,$efc2a794,$f6d996d5,
    $ae07bce9,$b71c8da8,$9c31de6b,$852aef2a,$ca6b79ed,$d37048ac,$f85d1b6f,$e1462a2e,
    $66de36e1,$7fc507a0,$54e85463,$4df36522,$02b2f3e5,$1ba9c2a4,$30849167,$299fa026,
    $e4c5aeb8,$fdde9ff9,$d6f3cc3a,$cfe8fd7b,$80a96bbc,$99b25afd,$b29f093e,$ab84387f,
    $2c1c24b0,$350715f1,$1e2a4632,$07317773,$4870e1b4,$516bd0f5,$7a468336,$635db277,
    $cbfad74e,$d2e1e60f,$f9ccb5cc,$e0d7848d,$af96124a,$b68d230b,$9da070c8,$84bb4189,
    $03235d46,$1a386c07,$31153fc4,$280e0e85,$674f9842,$7e54a903,$5579fac0,$4c62cb81,
    $8138c51f,$9823f45e,$b30ea79d,$aa1596dc,$e554001b,$fc4f315a,$d7626299,$ce7953d8,
    $49e14f17,$50fa7e56,$7bd72d95,$62cc1cd4,$2d8d8a13,$3496bb52,$1fbbe891,$06a0d9d0,
    $5e7ef3ec,$4765c2ad,$6c48916e,$7553a02f,$3a1236e8,$230907a9,$0824546a,$113f652b,
    $96a779e4,$8fbc48a5,$a4911b66,$bd8a2a27,$f2cbbce0,$ebd08da1,$c0fdde62,$d9e6ef23,
    $14bce1bd,$0da7d0fc,$268a833f,$3f91b27e,$70d024b9,$69cb15f8,$42e6463b,$5bfd777a,
    $dc656bb5,$c57e5af4,$ee530937,$f7483876,$b809aeb1,$a1129ff0,$8a3fcc33,$9324fd72);

  CTab2: array[0..255] of longint = (
    $00000000,$01c26a37,$0384d46e,$0246be59,$0709a8dc,$06cbc2eb,$048d7cb2,$054f1685,
    $0e1351b8,$0fd13b8f,$0d9785d6,$0c55efe1,$091af964,$08d89353,$0a9e2d0a,$0b5c473d,
    $1c26a370,$1de4c947,$1fa2771e,$1e601d29,$1b2f0bac,$1aed619b,$18abdfc2,$1969b5f5,
    $1235f2c8,$13f798ff,$11b126a6,$10734c91,$153c5a14,$14fe3023,$16b88e7a,$177ae44d,
    $384d46e0,$398f2cd7,$3bc9928e,$3a0bf8b9,$3f44ee3c,$3e86840b,$3cc03a52,$3d025065,
    $365e1758,$379c7d6f,$35dac336,$3418a901,$3157bf84,$3095d5b3,$32d36bea,$331101dd,
    $246be590,$25a98fa7,$27ef31fe,$262d5bc9,$23624d4c,$22a0277b,$20e69922,$2124f315,
    $2a78b428,$2bbade1f,$29fc6046,$283e0a71,$2d711cf4,$2cb376c3,$2ef5c89a,$2f37a2ad,
    $709a8dc0,$7158e7f7,$731e59ae,$72dc3399,$7793251c,$76514f2b,$7417f172,$75d59b45,
    $7e89dc78,$7f4bb64f,$7d0d0816,$7ccf6221,$798074a4,$78421e93,$7a04a0ca,$7bc6cafd,
    $6cbc2eb0,$6d7e4487,$6f38fade,$6efa90e9,$6bb5866c,$6a77ec5b,$68315202,$69f33835,
    $62af7f08,$636d153f,$612bab66,$60e9c151,$65a6d7d4,$6464bde3,$662203ba,$67e0698d,
    $48d7cb20,$4915a117,$4b531f4e,$4a917579,$4fde63fc,$4e1c09cb,$4c5ab792,$4d98dda5,
    $46c49a98,$4706f0af,$45404ef6,$448224c1,$41cd3244,$400f5873,$4249e62a,$438b8c1d,
    $54f16850,$55330267,$5775bc3e,$56b7d609,$53f8c08c,$523aaabb,$507c14e2,$51be7ed5,
    $5ae239e8,$5b2053df,$5966ed86,$58a487b1,$5deb9134,$5c29fb03,$5e6f455a,$5fad2f6d,
    $e1351b80,$e0f771b7,$e2b1cfee,$e373a5d9,$e63cb35c,$e7fed96b,$e5b86732,$e47a0d05,
    $ef264a38,$eee4200f,$eca29e56,$ed60f461,$e82fe2e4,$e9ed88d3,$ebab368a,$ea695cbd,
    $fd13b8f0,$fcd1d2c7,$fe976c9e,$ff5506a9,$fa1a102c,$fbd87a1b,$f99ec442,$f85cae75,
    $f300e948,$f2c2837f,$f0843d26,$f1465711,$f4094194,$f5cb2ba3,$f78d95fa,$f64fffcd,
    $d9785d60,$d8ba3757,$dafc890e,$db3ee339,$de71f5bc,$dfb39f8b,$ddf521d2,$dc374be5,
    $d76b0cd8,$d6a966ef,$d4efd8b6,$d52db281,$d062a404,$d1a0ce33,$d3e6706a,$d2241a5d,
    $c55efe10,$c49c9427,$c6da2a7e,$c7184049,$c25756cc,$c3953cfb,$c1d382a2,$c011e895,
    $cb4dafa8,$ca8fc59f,$c8c97bc6,$c90b11f1,$cc440774,$cd866d43,$cfc0d31a,$ce02b92d,
    $91af9640,$906dfc77,$922b422e,$93e92819,$96a63e9c,$976454ab,$9522eaf2,$94e080c5,
    $9fbcc7f8,$9e7eadcf,$9c381396,$9dfa79a1,$98b56f24,$99770513,$9b31bb4a,$9af3d17d,
    $8d893530,$8c4b5f07,$8e0de15e,$8fcf8b69,$8a809dec,$8b42f7db,$89044982,$88c623b5,
    $839a6488,$82580ebf,$801eb0e6,$81dcdad1,$8493cc54,$8551a663,$8717183a,$86d5720d,
    $a9e2d0a0,$a820ba97,$aa6604ce,$aba46ef9,$aeeb787c,$af29124b,$ad6fac12,$acadc625,
    $a7f18118,$a633eb2f,$a4755576,$a5b73f41,$a0f829c4,$a13a43f3,$a37cfdaa,$a2be979d,
    $b5c473d0,$b40619e7,$b640a7be,$b782cd89,$b2cddb0c,$b30fb13b,$b1490f62,$b08b6555,
    $bbd72268,$ba15485f,$b853f606,$b9919c31,$bcde8ab4,$bd1ce083,$bf5a5eda,$be9834ed);

  CTab3: array[0..255] of longint = (
    $00000000,$b8bc6765,$aa09c88b,$12b5afee,$8f629757,$37def032,$256b5fdc,$9dd738b9,
    $c5b428ef,$7d084f8a,$6fbde064,$d7018701,$4ad6bfb8,$f26ad8dd,$e0df7733,$58631056,
    $5019579f,$e8a530fa,$fa109f14,$42acf871,$df7bc0c8,$67c7a7ad,$75720843,$cdce6f26,
    $95ad7f70,$2d111815,$3fa4b7fb,$8718d09e,$1acfe827,$a2738f42,$b0c620ac,$087a47c9,
    $a032af3e,$188ec85b,$0a3b67b5,$b28700d0,$2f503869,$97ec5f0c,$8559f0e2,$3de59787,
    $658687d1,$dd3ae0b4,$cf8f4f5a,$7733283f,$eae41086,$525877e3,$40edd80d,$f851bf68,
    $f02bf8a1,$48979fc4,$5a22302a,$e29e574f,$7f496ff6,$c7f50893,$d540a77d,$6dfcc018,
    $359fd04e,$8d23b72b,$9f9618c5,$272a7fa0,$bafd4719,$0241207c,$10f48f92,$a848e8f7,
    $9b14583d,$23a83f58,$311d90b6,$89a1f7d3,$1476cf6a,$accaa80f,$be7f07e1,$06c36084,
    $5ea070d2,$e61c17b7,$f4a9b859,$4c15df3c,$d1c2e785,$697e80e0,$7bcb2f0e,$c377486b,
    $cb0d0fa2,$73b168c7,$6104c729,$d9b8a04c,$446f98f5,$fcd3ff90,$ee66507e,$56da371b,
    $0eb9274d,$b6054028,$a4b0efc6,$1c0c88a3,$81dbb01a,$3967d77f,$2bd27891,$936e1ff4,
    $3b26f703,$839a9066,$912f3f88,$299358ed,$b4446054,$0cf80731,$1e4da8df,$a6f1cfba,
    $fe92dfec,$462eb889,$549b1767,$ec277002,$71f048bb,$c94c2fde,$dbf98030,$6345e755,
    $6b3fa09c,$d383c7f9,$c1366817,$798a0f72,$e45d37cb,$5ce150ae,$4e54ff40,$f6e89825,
    $ae8b8873,$1637ef16,$048240f8,$bc3e279d,$21e91f24,$99557841,$8be0d7af,$335cb0ca,
    $ed59b63b,$55e5d15e,$47507eb0,$ffec19d5,$623b216c,$da874609,$c832e9e7,$708e8e82,
    $28ed9ed4,$9051f9b1,$82e4565f,$3a58313a,$a78f0983,$1f336ee6,$0d86c108,$b53aa66d,
    $bd40e1a4,$05fc86c1,$1749292f,$aff54e4a,$322276f3,$8a9e1196,$982bbe78,$2097d91d,
    $78f4c94b,$c048ae2e,$d2fd01c0,$6a4166a5,$f7965e1c,$4f2a3979,$5d9f9697,$e523f1f2,
    $4d6b1905,$f5d77e60,$e762d18e,$5fdeb6eb,$c2098e52,$7ab5e937,$680046d9,$d0bc21bc,
    $88df31ea,$3063568f,$22d6f961,$9a6a9e04,$07bda6bd,$bf01c1d8,$adb46e36,$15080953,
    $1d724e9a,$a5ce29ff,$b77b8611,$0fc7e174,$9210d9cd,$2aacbea8,$38191146,$80a57623,
    $d8c66675,$607a0110,$72cfaefe,$ca73c99b,$57a4f122,$ef189647,$fdad39a9,$45115ecc,
    $764dee06,$cef18963,$dc44268d,$64f841e8,$f92f7951,$41931e34,$5326b1da,$eb9ad6bf,
    $b3f9c6e9,$0b45a18c,$19f00e62,$a14c6907,$3c9b51be,$842736db,$96929935,$2e2efe50,
    $2654b999,$9ee8defc,$8c5d7112,$34e11677,$a9362ece,$118a49ab,$033fe645,$bb838120,
    $e3e09176,$5b5cf613,$49e959fd,$f1553e98,$6c820621,$d43e6144,$c68bceaa,$7e37a9cf,
    $d67f4138,$6ec3265d,$7c7689b3,$c4caeed6,$591dd66f,$e1a1b10a,$f3141ee4,$4ba87981,
    $13cb69d7,$ab770eb2,$b9c2a15c,$017ec639,$9ca9fe80,$241599e5,$36a0360b,$8e1c516e,
    $866616a7,$3eda71c2,$2c6fde2c,$94d3b949,$090481f0,$b1b8e695,$a30d497b,$1bb12e1e,
    $43d23e48,$fb6e592d,$e9dbf6c3,$516791a6,$ccb0a91f,$740cce7a,$66b96194,$de0506f1);

{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}

{$ifdef ENDIAN_BIG}
const
  CTab0 = 'Little-endian only, see Remark';
{$endif}


{$ifndef BIT16}

(**** 32+ Bit Delphi2+/FPC/VP *****)

{$ifdef PurePascal}
  {---------------------------------------------------------------------------}
  procedure FCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC with Msg data}
  var
    i,t: longint;
    ta: packed array[0..3] of byte absolute t;
  begin
    {Process bytewise until Msg is on dword boundary}
    while (Len>0) and (__P2I(Msg) and 3 <> 0) do begin
      CRC := CTab0[byte(CRC) xor pByte(Msg)^] xor (CRC shr 8);
      inc(Ptr2Inc(Msg));
      dec(Len);
    end;
    {Process remaining complete dwords}
    while Len>3 do begin
      t := CRC xor pLongint(Msg)^;
      inc(Ptr2Inc(Msg),4);
      dec(Len,4);
      CRC := CTab3[ta[0]] xor CTab2[ta[1]] xor CTab1[ta[2]] xor CTab0[ta[3]];
    end;
    {Process remaining bytes}
    for i:=1 to Len do begin
      CRC := CTab0[byte(CRC) xor pByte(Msg)^] xor (CRC shr 8);
      inc(Ptr2Inc(Msg));
    end;
  end;
{$else}
  {---------------------------------------------------------------------------}
  procedure FCRC32UpdateXL(var CRC: longint; Msg: pointer; Len: longint);
    {-update CRC with Msg data}
  begin
    asm
         push  ebx
         push  edi
         mov   ecx,[Len]
         mov   eax,[CRC]
         mov   eax,[eax]
         mov   edi,[Msg]

         or    ecx,ecx      {Fix for FPC222: dest for jecxz out of range}
         jnz   @@1
         jmp   @@9

         {Process bytewise until edi Msg pointer is on dword boundary}
    @@1: test  edi,3
         jz    @@2
         movzx ebx,byte ptr [edi]
         inc   edi
         xor   bl,al
         shr   eax,8
         xor   eax,dword ptr CTab0[ebx*4]
         dec   ecx
         jnz   @@1

    @@2: cmp   ecx,3
         jbe   @@4

         {Process remaining complete dwords}
         {Save old ecx and let new ecx count dwords}
         push  ecx
         shr   ecx,2
    @@3: mov   edx,[edi]
         xor   edx,eax
         add   edi,4
         movzx ebx,dl
         mov   eax,dword ptr CTab3[ebx*4]
         shr   edx,8
         movzx ebx,dl
         xor   eax,dword ptr CTab2[ebx*4]
         shr   edx,8
         movzx ebx,dl
         xor   eax,dword ptr CTab1[ebx*4]
         shr   edx,8
         movzx ebx,dl
         xor   eax,dword ptr CTab0[ebx*4]
         dec   ecx
         jnz   @@3
         {Get number of remaining bytes}
         pop   ecx
         and   ecx,3

         {Process remaining bytes}
    @@4: jecxz @@6
    @@5: movzx ebx,byte ptr [edi]
         inc   edi
         xor   bl,al
         shr   eax,8
         xor   eax,dword ptr CTab0[ebx*4]
         dec   ecx
         jnz   @@5

         {Store result CRC and restore registers}
    @@6: mov   edx,[CRC]
         mov   [edx],eax
    @@9: pop   edi
         pop   ebx
    end;
  end;
{$endif}


{---------------------------------------------------------------------------}
procedure FCRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC with Msg data}
begin
  FCRC32UpdateXL(CRC, Msg, Len);
end;


{$else}

(**** TP5-7/Delphi1 for 386+ *****)

{$ifndef BASM16}

{---------------------------------------------------------------------------}
procedure FCRC32Update(var CRC: longint; Msg: pointer; Len: word);
  {-update CRC with Msg data}
var
  i: word;
  t: longint;
  ta: packed array[0..3] of byte absolute t;
begin
  {Process bytewise until Msg is on dword boundary}
  while (Len>0) and (longint(Msg) and 3 <> 0) do begin
    CRC := CTab0[byte(CRC) xor pByte(Msg)^] xor (CRC shr 8);
    inc(Ptr2Inc(Msg));
    dec(Len);
  end;
  {Process remaining complete dwords}
  while Len>3 do begin
    t := CRC xor pLongint(Msg)^;
    inc(Ptr2Inc(Msg),4);
    dec(Len,4);
    CRC := CTab3[ta[0]] xor CTab2[ta[1]] xor CTab1[ta[2]] xor CTab0[ta[3]];
  end;
  {Process remaining bytes}
  for i:=1 to Len do begin
    CRC := CTab0[byte(CRC) xor pByte(Msg)^] xor (CRC shr 8);
    inc(Ptr2Inc(Msg));
  end;
end;

{$else}


{---------------------------------------------------------------------------}
procedure FCRC32Update(var CRC: longint; Msg: pointer; Len: word); assembler;
  {-update CRC with Msg data}
asm
             {ax: crc; bx,si: addr; cx: Len; di: Msg; dx: temp}
             mov   cx,[len]
             les   si,[CRC]
     db $66; mov   ax,es:[si]
     db $66; sub   bx,bx      {clear ebx}
             les   di,[Msg]

             or    cx,cx
             jnz   @@1
             jmp   @@9
   {Process bytewise until Msg is on dword boundary}
   @@1:      test  di,3
             jz    @@2
     db $66; mov   dx,ax
             xor   dx,es:[di]
             inc   di
             mov   bl,dl
     db $66; shr   ax,8
     db $66,$67,$8D,$34,$5B;
     db $66; xor   ax,word ptr CTab0[bx+si]
             dec   cx
             jnz   @@1
   {Process remaining complete dwords}
   @@2:      cmp   cx,3
             jbe   @@3

             push  cx
             shr   cx,2
   @@l:
     db $66; mov   dx,es:[di]
     db $66; xor   dx,ax
             add   di,4
             mov   bl,dl
    db $66,$67,$8D,$34,$5B;
    db $66;  mov   ax,word ptr CTab3[bx+si]
             mov   bl,dh
    db $66;  shr   dx,16
    db $66,$67,$8D,$34,$5B;
    db $66;  xor   ax,word ptr CTab2[bx+si]
             mov   bl,dl
    db $66,$67,$8D,$34,$5B;
    db $66;  xor   ax,word ptr CTab1[bx+si]
             mov   bl,dh
    db $66,$67,$8D,$34,$5B;
    db $66;  xor   ax,word ptr CTab0[bx+si]
             dec   cx
             jnz   @@l
             pop   cx
             and   cx,3

   {Process remaining bytes}
   @@3:      jcxz  @@5
   @@4:
     db $66; mov   dx,ax
             xor   dx,es:[di]
             inc   di
             mov   bl,dl
     db $66; shr   ax,8
     db $66,$67,$8D,$34,$5B;
     db $66; xor   ax,word ptr CTab0[bx+si]
             dec   cx
             jnz   @@4

   @@5:      les   si,CRC
     db $66; mov   es:[si],ax
  @@9:
end;

{$endif BASM16}
{$endif BIT16}



{$ifndef BIT16}
{---------------------------------------------------------------------------}
procedure FCRC32FullXL(var CRC: longint; Msg: pointer; Len: longint);
  {-CRC32 of Msg with init/update/final}
begin
  FCRC32Init(CRC);
  FCRC32UpdateXL(CRC, Msg, Len);
  FCRC32Final(CRC);
end;
{$endif}



{---------------------------------------------------------------------------}
procedure FCRC32Init(var CRC: longint);
  {-CRC initialization}
begin
  CRC := Mask32;
end;


{---------------------------------------------------------------------------}
procedure FCRC32Final(var CRC: longint);
  {-CRC32: finalize calculation}
begin
  CRC := CRC xor Mask32;
end;


{---------------------------------------------------------------------------}
procedure FCRC32Full(var CRC: longint; Msg: pointer; Len: word);
  {-CRC32 of Msg with init/update/final}
begin
  FCRC32Init(CRC);
  FCRC32Update(CRC, Msg, Len);
  FCRC32Final(CRC);
end;


{---------------------------------------------------------------------------}
function  FCRC32SelfTest: boolean;
  {-self test for FCRC32}
const
  s: string[17] = '0123456789abcdefg';
  Check = longint($BE6CBE90);
var
  i: integer;
  CRC, CRCF: longint;
begin
  FCRC32Full(CRCF, @s[1], length(s));
  FCRC32Init(CRC);
  for i:=1 to length(s) do FCRC32Update(CRC, @s[i], 1);
  FCRC32Final(CRC);
  FCRC32SelfTest := (CRC=Check) and (CRCF=Check);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure FCRC32File({$ifdef CONST} const {$endif} fname: string;
                    var CRC: longint; var buf; bsize: word; var Err: word);
  {-CRC32 of file, buf: buffer with at least bsize bytes}
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
  FCRC32Init(CRC);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    {$ifndef BIT16}
      FCRC32UpdateXL(CRC, @buf, L);
    {$else}
      FCRC32Update(CRC, @buf, L);
    {$endif}
  end;
  system.close(f);
  if IOResult=0 then;
  FCRC32Final(CRC);
end;

{$ifdef DumpAlign}
begin
  writeln('Align FCRC32: ',ofs(CTab0) and 3:2, ofs(CTab1) and 3:2, ofs(CTab2) and 3:2, ofs(CTab3) and 3:2);
{$endif}

end.
