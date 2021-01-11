unit TF_Base;

(*************************************************************************

 DESCRIPTION   :  Twofish basic routines

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12/D17, FPC, VP

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  4.5 KB static data

 DISPLAY MODE  :  ---

 REFERENCES    :  [1] Schneier et al, Twofish: A 128-Bit Block Cipher
                      http://www.schneier.com/paper-twofish-paper.pdf
                  [2] Source code and test vectors available from the Twofish
                      page http://www.schneier.com/twofish.html
                  [3] Wei Dai's public domain twofish.cpp in
                      Crypto++ Library 5.2.1 from http://www.cryptopp.com

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.01     27.05.06  W.Ehrhardt  Initial BP7 key init
 0.02     27.05.06  we          Bug fix 192 bit key
 0.03     27.05.06  we          Encrypt, bug fix rs_mul, other compilers
 0.04     28.05.06  we          Decrypt
 0.05     28.05.06  we          Removed r in rs_mul
 0.06     28.05.06  we          Encr: Rearranged PHT/Rotate, strict ascending RK access
 0.07     28.05.06  we          Encr: BIT32 with shr and mask
 0.08     28.05.06  we          Decrypt routines
 0.09     28.05.06  we          BASM16: RotL1, RotR1
 0.10     28.05.06  we          Inline: RotL1, RotR1 for 16 bit
 0.11     28.05.06  we          Space reduction for round key calculation
 0.12     28.05.06  we          Add some comments
 0.13     28.05.06  we          Tableless Reed-Solomon
 0.14     28.05.06  we          Removed r from rs_mul
 0.15     28.05.06  we          BIT32 rs_mul like Crypto++
 0.16     28.05.06  we          Code cleaning, comments, tables in .pas
 0.17     04.06.06  we          Removed unused types, some more comments
 0.18     05.06.06  we          BASM16: encrypt with asm
 0.19     05.06.06  we          BASM16: decrypt with asm
 0.20     10.06.06  we          BASM16 encrypt: some improvements
 0.21     10.06.06  we          BIT16: improved Pseudo Hadamard and rotation
 0.22     16.06.07  we          TF_Reset stdcall
 0.23     06.08.09  we          Locally trurn off range checks for byte shifts (for debug)
 0.24     31.07.10  we          TF_Err_CTR_SeekOffset, TF_Err_Invalid_16Bit_Length
 0.25     02.07.12  we          64-bit adjustments
 0.26     25.12.12  we          {$J+} if needed

 **************************************************************************)



(*-------------------------------------------------------------------------
 (C) Copyright 2006-2012 Wolfgang Ehrhardt

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

{$i std.inc}


interface

const
  TF_Err_Invalid_Key_Size       = -1;  {Key size in bits not 128, 192, or 256}
  TF_Err_Invalid_Length         = -3;  {No full block for cipher stealing}
  TF_Err_Data_After_Short_Block = -4;  {Short block must be last}
  TF_Err_MultipleIncProcs       = -5;  {More than one IncProc Setting}
  TF_Err_NIL_Pointer            = -6;  {nil pointer to block with nonzero length}

  TF_Err_CTR_SeekOffset         = -15; {Negative offset in TF_CTR_Seek}
  TF_Err_Invalid_16Bit_Length   = -20; {Pointer + Offset > $FFFF for 16 bit code}

type
  TTFRndKey  = packed array[0..39]  of longint;
  TTFSBox    = packed array[0..255] of longint;
  TTFBlock   = packed array[0..15]  of byte;
  PTFBlock   = ^TTFBlock;

type
  TTFIncProc = procedure(var CTR: TTFBlock);   {user supplied IncCTR proc}
                {$ifdef DLL} stdcall; {$endif}
type
  TTFContext = packed record
                 IV      : TTFBlock;   {IV or CTR              }
                 buf     : TTFBlock;   {Work buffer            }
                 bLen    : word;       {Bytes used in buf      }
                 Flag    : word;       {Bit 1: Short block     }
                 IncProc : TTFIncProc; {Increment proc CTR-Mode}
                 RK      : TTFRndKey;  {Round keys             }
                 S0,S1,                {Key dependent SBoxes   }
                 S2,S3   : TTFSBox;    {DO NOT CHANGE SEQUENCE!}
               end;

const
  TFBLKSIZE  = sizeof(TTFBlock);       {Twofish block size}


{$ifdef CONST}

function  TF_Init(const Key; KeyBits: word; var ctx: TTFContext): integer;
  {-Twofish round key and key-dependent sbox initialisation}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_Encrypt(var ctx: TTFContext; const BI: TTFBlock; var BO: TTFBlock);
  {-encrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_Decrypt(var ctx: TTFContext; const BI: TTFBlock; var BO: TTFBlock);
  {-decrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_XorBlock(const B1, B2: TTFBlock; var B3: TTFBlock);
  {-xor two blocks, result in third}
  {$ifdef DLL} stdcall; {$endif}

{$else}

function  TF_Init(var Key; KeyBits: word; var ctx: TTFContext): integer;
  {-Twofish round key and key-dependent sbox initialisation}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_Encrypt(var ctx: TTFContext; var BI: TTFBlock; var BO: TTFBlock);
  {-encrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_Decrypt(var ctx: TTFContext; var BI: TTFBlock; var BO: TTFBlock);
  {-decrypt one block (in ECB mode)}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_XorBlock(var B1, B2: TTFBlock; var B3: TTFBlock);
  {-xor two blocks, result in third}

{$endif}

procedure TF_Reset(var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag}
  {$ifdef DLL} stdcall; {$endif}

procedure TF_SetFastInit(value: boolean);
  {-set FastInit variable}
  {$ifdef DLL} stdcall; {$endif}

function  TF_GetFastInit: boolean;
  {-Returns FastInit variable}
  {$ifdef DLL} stdcall; {$endif}


implementation

{$ifdef D4Plus}
var
{$else}
{$ifdef J_OPT} {$J+} {$endif}
const
{$endif}
  FastInit : boolean = true;    {Clear only necessary context data at init}
                                {IV and buf remain uninitialized}

type
  TWA4 = packed array[0..3] of longint;  {TF block as array of longint}
  TWA8 = packed array[0..7] of longint;  {longint type cast for key}


{Static tables}

const
  q0: array[0..255] of byte = (
    $A9,$67,$B3,$E8,$04,$FD,$A3,$76,$9A,$92,$80,$78,$E4,$DD,$D1,$38,
    $0D,$C6,$35,$98,$18,$F7,$EC,$6C,$43,$75,$37,$26,$FA,$13,$94,$48,
    $F2,$D0,$8B,$30,$84,$54,$DF,$23,$19,$5B,$3D,$59,$F3,$AE,$A2,$82,
    $63,$01,$83,$2E,$D9,$51,$9B,$7C,$A6,$EB,$A5,$BE,$16,$0C,$E3,$61,
    $C0,$8C,$3A,$F5,$73,$2C,$25,$0B,$BB,$4E,$89,$6B,$53,$6A,$B4,$F1,
    $E1,$E6,$BD,$45,$E2,$F4,$B6,$66,$CC,$95,$03,$56,$D4,$1C,$1E,$D7,
    $FB,$C3,$8E,$B5,$E9,$CF,$BF,$BA,$EA,$77,$39,$AF,$33,$C9,$62,$71,
    $81,$79,$09,$AD,$24,$CD,$F9,$D8,$E5,$C5,$B9,$4D,$44,$08,$86,$E7,
    $A1,$1D,$AA,$ED,$06,$70,$B2,$D2,$41,$7B,$A0,$11,$31,$C2,$27,$90,
    $20,$F6,$60,$FF,$96,$5C,$B1,$AB,$9E,$9C,$52,$1B,$5F,$93,$0A,$EF,
    $91,$85,$49,$EE,$2D,$4F,$8F,$3B,$47,$87,$6D,$46,$D6,$3E,$69,$64,
    $2A,$CE,$CB,$2F,$FC,$97,$05,$7A,$AC,$7F,$D5,$1A,$4B,$0E,$A7,$5A,
    $28,$14,$3F,$29,$88,$3C,$4C,$02,$B8,$DA,$B0,$17,$55,$1F,$8A,$7D,
    $57,$C7,$8D,$74,$B7,$C4,$9F,$72,$7E,$15,$22,$12,$58,$07,$99,$34,
    $6E,$50,$DE,$68,$65,$BC,$DB,$F8,$C8,$A8,$2B,$40,$DC,$FE,$32,$A4,
    $CA,$10,$21,$F0,$D3,$5D,$0F,$00,$6F,$9D,$36,$42,$4A,$5E,$C1,$E0);

const
  q1: array[0..255] of byte = (
    $75,$F3,$C6,$F4,$DB,$7B,$FB,$C8,$4A,$D3,$E6,$6B,$45,$7D,$E8,$4B,
    $D6,$32,$D8,$FD,$37,$71,$F1,$E1,$30,$0F,$F8,$1B,$87,$FA,$06,$3F,
    $5E,$BA,$AE,$5B,$8A,$00,$BC,$9D,$6D,$C1,$B1,$0E,$80,$5D,$D2,$D5,
    $A0,$84,$07,$14,$B5,$90,$2C,$A3,$B2,$73,$4C,$54,$92,$74,$36,$51,
    $38,$B0,$BD,$5A,$FC,$60,$62,$96,$6C,$42,$F7,$10,$7C,$28,$27,$8C,
    $13,$95,$9C,$C7,$24,$46,$3B,$70,$CA,$E3,$85,$CB,$11,$D0,$93,$B8,
    $A6,$83,$20,$FF,$9F,$77,$C3,$CC,$03,$6F,$08,$BF,$40,$E7,$2B,$E2,
    $79,$0C,$AA,$82,$41,$3A,$EA,$B9,$E4,$9A,$A4,$97,$7E,$DA,$7A,$17,
    $66,$94,$A1,$1D,$3D,$F0,$DE,$B3,$0B,$72,$A7,$1C,$EF,$D1,$53,$3E,
    $8F,$33,$26,$5F,$EC,$76,$2A,$49,$81,$88,$EE,$21,$C4,$1A,$EB,$D9,
    $C5,$39,$99,$CD,$AD,$31,$8B,$01,$18,$23,$DD,$1F,$4E,$2D,$F9,$48,
    $4F,$F2,$65,$8E,$78,$5C,$58,$19,$8D,$E5,$98,$57,$67,$7F,$05,$64,
    $AF,$63,$B6,$FE,$F5,$B7,$3C,$A5,$CE,$E9,$68,$44,$E0,$4D,$43,$69,
    $29,$2E,$AC,$15,$59,$A8,$0A,$9E,$6E,$47,$DF,$34,$35,$6A,$CF,$DC,
    $22,$C9,$C0,$9B,$89,$D4,$ED,$AB,$12,$A2,$0D,$52,$BB,$02,$2F,$A9,
    $D7,$61,$1E,$B4,$50,$04,$F6,$C2,$16,$25,$86,$56,$55,$09,$BE,$91);

{$ifdef StrictLong}
  {$warnings off}
  {$R-} {avoid D9+ errors!}
{$endif}

const
  mds0: array[0..255] of longint = (
    $BCBC3275,$ECEC21F3,$202043C6,$B3B3C9F4,$DADA03DB,$02028B7B,$E2E22BFB,$9E9EFAC8,
    $C9C9EC4A,$D4D409D3,$18186BE6,$1E1E9F6B,$98980E45,$B2B2387D,$A6A6D2E8,$2626B74B,
    $3C3C57D6,$93938A32,$8282EED8,$525298FD,$7B7BD437,$BBBB3771,$5B5B97F1,$474783E1,
    $24243C30,$5151E20F,$BABAC6F8,$4A4AF31B,$BFBF4887,$0D0D70FA,$B0B0B306,$7575DE3F,
    $D2D2FD5E,$7D7D20BA,$666631AE,$3A3AA35B,$59591C8A,$00000000,$CDCD93BC,$1A1AE09D,
    $AEAE2C6D,$7F7FABC1,$2B2BC7B1,$BEBEB90E,$E0E0A080,$8A8A105D,$3B3B52D2,$6464BAD5,
    $D8D888A0,$E7E7A584,$5F5FE807,$1B1B1114,$2C2CC2B5,$FCFCB490,$3131272C,$808065A3,
    $73732AB2,$0C0C8173,$79795F4C,$6B6B4154,$4B4B0292,$53536974,$94948F36,$83831F51,
    $2A2A3638,$C4C49CB0,$2222C8BD,$D5D5F85A,$BDBDC3FC,$48487860,$FFFFCE62,$4C4C0796,
    $4141776C,$C7C7E642,$EBEB24F7,$1C1C1410,$5D5D637C,$36362228,$6767C027,$E9E9AF8C,
    $4444F913,$1414EA95,$F5F5BB9C,$CFCF18C7,$3F3F2D24,$C0C0E346,$7272DB3B,$54546C70,
    $29294CCA,$F0F035E3,$0808FE85,$C6C617CB,$F3F34F11,$8C8CE4D0,$A4A45993,$CACA96B8,
    $68683BA6,$B8B84D83,$38382820,$E5E52EFF,$ADAD569F,$0B0B8477,$C8C81DC3,$9999FFCC,
    $5858ED03,$19199A6F,$0E0E0A08,$95957EBF,$70705040,$F7F730E7,$6E6ECF2B,$1F1F6EE2,
    $B5B53D79,$09090F0C,$616134AA,$57571682,$9F9F0B41,$9D9D803A,$111164EA,$2525CDB9,
    $AFAFDDE4,$4545089A,$DFDF8DA4,$A3A35C97,$EAEAD57E,$353558DA,$EDEDD07A,$4343FC17,
    $F8F8CB66,$FBFBB194,$3737D3A1,$FAFA401D,$C2C2683D,$B4B4CCF0,$32325DDE,$9C9C71B3,
    $5656E70B,$E3E3DA72,$878760A7,$15151B1C,$F9F93AEF,$6363BFD1,$3434A953,$9A9A853E,
    $B1B1428F,$7C7CD133,$88889B26,$3D3DA65F,$A1A1D7EC,$E4E4DF76,$8181942A,$91910149,
    $0F0FFB81,$EEEEAA88,$161661EE,$D7D77321,$9797F5C4,$A5A5A81A,$FEFE3FEB,$6D6DB5D9,
    $7878AEC5,$C5C56D39,$1D1DE599,$7676A4CD,$3E3EDCAD,$CBCB6731,$B6B6478B,$EFEF5B01,
    $12121E18,$6060C523,$6A6AB0DD,$4D4DF61F,$CECEE94E,$DEDE7C2D,$55559DF9,$7E7E5A48,
    $2121B24F,$03037AF2,$A0A02665,$5E5E198E,$5A5A6678,$65654B5C,$62624E58,$FDFD4519,
    $0606F48D,$404086E5,$F2F2BE98,$3333AC57,$17179067,$05058E7F,$E8E85E05,$4F4F7D64,
    $89896AAF,$10109563,$74742FB6,$0A0A75FE,$5C5C92F5,$9B9B74B7,$2D2D333C,$3030D6A5,
    $2E2E49CE,$494989E9,$46467268,$77775544,$A8A8D8E0,$9696044D,$2828BD43,$A9A92969,
    $D9D97929,$8686912E,$D1D187AC,$F4F44A15,$8D8D1559,$D6D682A8,$B9B9BC0A,$42420D9E,
    $F6F6C16E,$2F2FB847,$DDDD06DF,$23233934,$CCCC6235,$F1F1C46A,$C1C112CF,$8585EBDC,
    $8F8F9E22,$7171A1C9,$9090F0C0,$AAAA539B,$0101F189,$8B8BE1D4,$4E4E8CED,$8E8E6FAB,
    $ABABA212,$6F6F3EA2,$E6E6540D,$DBDBF252,$92927BBB,$B7B7B602,$6969CA2F,$3939D9A9,
    $D3D30CD7,$A7A72361,$A2A2AD1E,$C3C399B4,$6C6C4450,$07070504,$04047FF6,$272746C2,
    $ACACA716,$D0D07625,$50501386,$DCDCF756,$84841A55,$E1E15109,$7A7A25BE,$1313EF91);

const
  mds1: array[0..255] of longint = (
    $A9D93939,$67901717,$B3719C9C,$E8D2A6A6,$04050707,$FD985252,$A3658080,$76DFE4E4,
    $9A084545,$92024B4B,$80A0E0E0,$78665A5A,$E4DDAFAF,$DDB06A6A,$D1BF6363,$38362A2A,
    $0D54E6E6,$C6432020,$3562CCCC,$98BEF2F2,$181E1212,$F724EBEB,$ECD7A1A1,$6C774141,
    $43BD2828,$7532BCBC,$37D47B7B,$269B8888,$FA700D0D,$13F94444,$94B1FBFB,$485A7E7E,
    $F27A0303,$D0E48C8C,$8B47B6B6,$303C2424,$84A5E7E7,$54416B6B,$DF06DDDD,$23C56060,
    $1945FDFD,$5BA33A3A,$3D68C2C2,$59158D8D,$F321ECEC,$AE316666,$A23E6F6F,$82165757,
    $63951010,$015BEFEF,$834DB8B8,$2E918686,$D9B56D6D,$511F8383,$9B53AAAA,$7C635D5D,
    $A63B6868,$EB3FFEFE,$A5D63030,$BE257A7A,$16A7ACAC,$0C0F0909,$E335F0F0,$6123A7A7,
    $C0F09090,$8CAFE9E9,$3A809D9D,$F5925C5C,$73810C0C,$2C273131,$2576D0D0,$0BE75656,
    $BB7B9292,$4EE9CECE,$89F10101,$6B9F1E1E,$53A93434,$6AC4F1F1,$B499C3C3,$F1975B5B,
    $E1834747,$E66B1818,$BDC82222,$450E9898,$E26E1F1F,$F4C9B3B3,$B62F7474,$66CBF8F8,
    $CCFF9999,$95EA1414,$03ED5858,$56F7DCDC,$D4E18B8B,$1C1B1515,$1EADA2A2,$D70CD3D3,
    $FB2BE2E2,$C31DC8C8,$8E195E5E,$B5C22C2C,$E9894949,$CF12C1C1,$BF7E9595,$BA207D7D,
    $EA641111,$77840B0B,$396DC5C5,$AF6A8989,$33D17C7C,$C9A17171,$62CEFFFF,$7137BBBB,
    $81FB0F0F,$793DB5B5,$0951E1E1,$ADDC3E3E,$242D3F3F,$CDA47676,$F99D5555,$D8EE8282,
    $E5864040,$C5AE7878,$B9CD2525,$4D049696,$44557777,$080A0E0E,$86135050,$E730F7F7,
    $A1D33737,$1D40FAFA,$AA346161,$ED8C4E4E,$06B3B0B0,$706C5454,$B22A7373,$D2523B3B,
    $410B9F9F,$7B8B0202,$A088D8D8,$114FF3F3,$3167CBCB,$C2462727,$27C06767,$90B4FCFC,
    $20283838,$F67F0404,$60784848,$FF2EE5E5,$96074C4C,$5C4B6565,$B1C72B2B,$AB6F8E8E,
    $9E0D4242,$9CBBF5F5,$52F2DBDB,$1BF34A4A,$5FA63D3D,$9359A4A4,$0ABCB9B9,$EF3AF9F9,
    $91EF1313,$85FE0808,$49019191,$EE611616,$2D7CDEDE,$4FB22121,$8F42B1B1,$3BDB7272,
    $47B82F2F,$8748BFBF,$6D2CAEAE,$46E3C0C0,$D6573C3C,$3E859A9A,$6929A9A9,$647D4F4F,
    $2A948181,$CE492E2E,$CB17C6C6,$2FCA6969,$FCC3BDBD,$975CA3A3,$055EE8E8,$7AD0EDED,
    $AC87D1D1,$7F8E0505,$D5BA6464,$1AA8A5A5,$4BB72626,$0EB9BEBE,$A7608787,$5AF8D5D5,
    $28223636,$14111B1B,$3FDE7575,$2979D9D9,$88AAEEEE,$3C332D2D,$4C5F7979,$02B6B7B7,
    $B896CACA,$DA583535,$B09CC4C4,$17FC4343,$551A8484,$1FF64D4D,$8A1C5959,$7D38B2B2,
    $57AC3333,$C718CFCF,$8DF40606,$74695353,$B7749B9B,$C4F59797,$9F56ADAD,$72DAE3E3,
    $7ED5EAEA,$154AF4F4,$229E8F8F,$12A2ABAB,$584E6262,$07E85F5F,$99E51D1D,$34392323,
    $6EC1F6F6,$50446C6C,$DE5D3232,$68724646,$6526A0A0,$BC93CDCD,$DB03DADA,$F8C6BABA,
    $C8FA9E9E,$A882D6D6,$2BCF6E6E,$40507070,$DCEB8585,$FE750A0A,$328A9393,$A48DDFDF,
    $CA4C2929,$10141C1C,$2173D7D7,$F0CCB4B4,$D309D4D4,$5D108A8A,$0FE25151,$00000000,
    $6F9A1919,$9DE01A1A,$368F9494,$42E6C7C7,$4AECC9C9,$5EFDD2D2,$C1AB7F7F,$E0D8A8A8);

const
  mds2: array[0..255] of longint = (
    $BC75BC32,$ECF3EC21,$20C62043,$B3F4B3C9,$DADBDA03,$027B028B,$E2FBE22B,$9EC89EFA,
    $C94AC9EC,$D4D3D409,$18E6186B,$1E6B1E9F,$9845980E,$B27DB238,$A6E8A6D2,$264B26B7,
    $3CD63C57,$9332938A,$82D882EE,$52FD5298,$7B377BD4,$BB71BB37,$5BF15B97,$47E14783,
    $2430243C,$510F51E2,$BAF8BAC6,$4A1B4AF3,$BF87BF48,$0DFA0D70,$B006B0B3,$753F75DE,
    $D25ED2FD,$7DBA7D20,$66AE6631,$3A5B3AA3,$598A591C,$00000000,$CDBCCD93,$1A9D1AE0,
    $AE6DAE2C,$7FC17FAB,$2BB12BC7,$BE0EBEB9,$E080E0A0,$8A5D8A10,$3BD23B52,$64D564BA,
    $D8A0D888,$E784E7A5,$5F075FE8,$1B141B11,$2CB52CC2,$FC90FCB4,$312C3127,$80A38065,
    $73B2732A,$0C730C81,$794C795F,$6B546B41,$4B924B02,$53745369,$9436948F,$8351831F,
    $2A382A36,$C4B0C49C,$22BD22C8,$D55AD5F8,$BDFCBDC3,$48604878,$FF62FFCE,$4C964C07,
    $416C4177,$C742C7E6,$EBF7EB24,$1C101C14,$5D7C5D63,$36283622,$672767C0,$E98CE9AF,
    $441344F9,$149514EA,$F59CF5BB,$CFC7CF18,$3F243F2D,$C046C0E3,$723B72DB,$5470546C,
    $29CA294C,$F0E3F035,$088508FE,$C6CBC617,$F311F34F,$8CD08CE4,$A493A459,$CAB8CA96,
    $68A6683B,$B883B84D,$38203828,$E5FFE52E,$AD9FAD56,$0B770B84,$C8C3C81D,$99CC99FF,
    $580358ED,$196F199A,$0E080E0A,$95BF957E,$70407050,$F7E7F730,$6E2B6ECF,$1FE21F6E,
    $B579B53D,$090C090F,$61AA6134,$57825716,$9F419F0B,$9D3A9D80,$11EA1164,$25B925CD,
    $AFE4AFDD,$459A4508,$DFA4DF8D,$A397A35C,$EA7EEAD5,$35DA3558,$ED7AEDD0,$431743FC,
    $F866F8CB,$FB94FBB1,$37A137D3,$FA1DFA40,$C23DC268,$B4F0B4CC,$32DE325D,$9CB39C71,
    $560B56E7,$E372E3DA,$87A78760,$151C151B,$F9EFF93A,$63D163BF,$345334A9,$9A3E9A85,
    $B18FB142,$7C337CD1,$8826889B,$3D5F3DA6,$A1ECA1D7,$E476E4DF,$812A8194,$91499101,
    $0F810FFB,$EE88EEAA,$16EE1661,$D721D773,$97C497F5,$A51AA5A8,$FEEBFE3F,$6DD96DB5,
    $78C578AE,$C539C56D,$1D991DE5,$76CD76A4,$3EAD3EDC,$CB31CB67,$B68BB647,$EF01EF5B,
    $1218121E,$602360C5,$6ADD6AB0,$4D1F4DF6,$CE4ECEE9,$DE2DDE7C,$55F9559D,$7E487E5A,
    $214F21B2,$03F2037A,$A065A026,$5E8E5E19,$5A785A66,$655C654B,$6258624E,$FD19FD45,
    $068D06F4,$40E54086,$F298F2BE,$335733AC,$17671790,$057F058E,$E805E85E,$4F644F7D,
    $89AF896A,$10631095,$74B6742F,$0AFE0A75,$5CF55C92,$9BB79B74,$2D3C2D33,$30A530D6,
    $2ECE2E49,$49E94989,$46684672,$77447755,$A8E0A8D8,$964D9604,$284328BD,$A969A929,
    $D929D979,$862E8691,$D1ACD187,$F415F44A,$8D598D15,$D6A8D682,$B90AB9BC,$429E420D,
    $F66EF6C1,$2F472FB8,$DDDFDD06,$23342339,$CC35CC62,$F16AF1C4,$C1CFC112,$85DC85EB,
    $8F228F9E,$71C971A1,$90C090F0,$AA9BAA53,$018901F1,$8BD48BE1,$4EED4E8C,$8EAB8E6F,
    $AB12ABA2,$6FA26F3E,$E60DE654,$DB52DBF2,$92BB927B,$B702B7B6,$692F69CA,$39A939D9,
    $D3D7D30C,$A761A723,$A21EA2AD,$C3B4C399,$6C506C44,$07040705,$04F6047F,$27C22746,
    $AC16ACA7,$D025D076,$50865013,$DC56DCF7,$8455841A,$E109E151,$7ABE7A25,$139113EF);

const
  mds3: array[0..255] of longint = (
    $D939A9D9,$90176790,$719CB371,$D2A6E8D2,$05070405,$9852FD98,$6580A365,$DFE476DF,
    $08459A08,$024B9202,$A0E080A0,$665A7866,$DDAFE4DD,$B06ADDB0,$BF63D1BF,$362A3836,
    $54E60D54,$4320C643,$62CC3562,$BEF298BE,$1E12181E,$24EBF724,$D7A1ECD7,$77416C77,
    $BD2843BD,$32BC7532,$D47B37D4,$9B88269B,$700DFA70,$F94413F9,$B1FB94B1,$5A7E485A,
    $7A03F27A,$E48CD0E4,$47B68B47,$3C24303C,$A5E784A5,$416B5441,$06DDDF06,$C56023C5,
    $45FD1945,$A33A5BA3,$68C23D68,$158D5915,$21ECF321,$3166AE31,$3E6FA23E,$16578216,
    $95106395,$5BEF015B,$4DB8834D,$91862E91,$B56DD9B5,$1F83511F,$53AA9B53,$635D7C63,
    $3B68A63B,$3FFEEB3F,$D630A5D6,$257ABE25,$A7AC16A7,$0F090C0F,$35F0E335,$23A76123,
    $F090C0F0,$AFE98CAF,$809D3A80,$925CF592,$810C7381,$27312C27,$76D02576,$E7560BE7,
    $7B92BB7B,$E9CE4EE9,$F10189F1,$9F1E6B9F,$A93453A9,$C4F16AC4,$99C3B499,$975BF197,
    $8347E183,$6B18E66B,$C822BDC8,$0E98450E,$6E1FE26E,$C9B3F4C9,$2F74B62F,$CBF866CB,
    $FF99CCFF,$EA1495EA,$ED5803ED,$F7DC56F7,$E18BD4E1,$1B151C1B,$ADA21EAD,$0CD3D70C,
    $2BE2FB2B,$1DC8C31D,$195E8E19,$C22CB5C2,$8949E989,$12C1CF12,$7E95BF7E,$207DBA20,
    $6411EA64,$840B7784,$6DC5396D,$6A89AF6A,$D17C33D1,$A171C9A1,$CEFF62CE,$37BB7137,
    $FB0F81FB,$3DB5793D,$51E10951,$DC3EADDC,$2D3F242D,$A476CDA4,$9D55F99D,$EE82D8EE,
    $8640E586,$AE78C5AE,$CD25B9CD,$04964D04,$55774455,$0A0E080A,$13508613,$30F7E730,
    $D337A1D3,$40FA1D40,$3461AA34,$8C4EED8C,$B3B006B3,$6C54706C,$2A73B22A,$523BD252,
    $0B9F410B,$8B027B8B,$88D8A088,$4FF3114F,$67CB3167,$4627C246,$C06727C0,$B4FC90B4,
    $28382028,$7F04F67F,$78486078,$2EE5FF2E,$074C9607,$4B655C4B,$C72BB1C7,$6F8EAB6F,
    $0D429E0D,$BBF59CBB,$F2DB52F2,$F34A1BF3,$A63D5FA6,$59A49359,$BCB90ABC,$3AF9EF3A,
    $EF1391EF,$FE0885FE,$01914901,$6116EE61,$7CDE2D7C,$B2214FB2,$42B18F42,$DB723BDB,
    $B82F47B8,$48BF8748,$2CAE6D2C,$E3C046E3,$573CD657,$859A3E85,$29A96929,$7D4F647D,
    $94812A94,$492ECE49,$17C6CB17,$CA692FCA,$C3BDFCC3,$5CA3975C,$5EE8055E,$D0ED7AD0,
    $87D1AC87,$8E057F8E,$BA64D5BA,$A8A51AA8,$B7264BB7,$B9BE0EB9,$6087A760,$F8D55AF8,
    $22362822,$111B1411,$DE753FDE,$79D92979,$AAEE88AA,$332D3C33,$5F794C5F,$B6B702B6,
    $96CAB896,$5835DA58,$9CC4B09C,$FC4317FC,$1A84551A,$F64D1FF6,$1C598A1C,$38B27D38,
    $AC3357AC,$18CFC718,$F4068DF4,$69537469,$749BB774,$F597C4F5,$56AD9F56,$DAE372DA,
    $D5EA7ED5,$4AF4154A,$9E8F229E,$A2AB12A2,$4E62584E,$E85F07E8,$E51D99E5,$39233439,
    $C1F66EC1,$446C5044,$5D32DE5D,$72466872,$26A06526,$93CDBC93,$03DADB03,$C6BAF8C6,
    $FA9EC8FA,$82D6A882,$CF6E2BCF,$50704050,$EB85DCEB,$750AFE75,$8A93328A,$8DDFA48D,
    $4C29CA4C,$141C1014,$73D72173,$CCB4F0CC,$09D4D309,$108A5D10,$E2510FE2,$00000000,
    $9A196F9A,$E01A9DE0,$8F94368F,$E6C742E6,$ECC94AEC,$FDD25EFD,$AB7FC1AB,$D8A8E0D8);


{$ifdef StrictLong}
  {$warnings on}
  {$ifdef RangeChecks_on}
    {$R+}
  {$endif}
{$endif}




{$ifdef BIT16}
{$ifndef BASM16}
{---------------------------------------------------------------------------}
function RotL1(x: longint): longint;
  {-Rotate left 1}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $2B/$C9/      { sub  cx,cx}
  $D1/$D0/      { rcl  ax,1 }
  $D1/$D2/      { rcl  dx,1 }
  $13/$C1);     { adc  ax,cx}

{---------------------------------------------------------------------------}
function RotR1(x: longint): longint;
  {-Rotate right 1}
inline(
  $58/          { pop  ax   }
  $5A/          { pop  dx   }
  $8B/$CA/      { mov  cx,dx}
  $D1/$E9/      { shr  cx,1 }
  $D1/$D8/      { rcr  ax,1 }
  $D1/$DA);     { rcr  dx,1 }

{---------------------------------------------------------------------------}
function RotL(x: longint; c: byte): longint;
 {-Rotate left c bits, room for optimization}
 { Currently not used, needed with c=8,9 in round key calculation}
begin
  RotL := (x shl c) or (x shr (32-c));
end;

{$endif}
{$endif}


{---------------------------------------------------------------------------}
procedure TF_Reset(var ctx: TTFContext);
  {-Clears ctx fields bLen and Flag}
begin
  with ctx do begin
    bLen :=0;
    Flag :=0;
  end;
end;


{$ifdef BASM16}
{---------------------------------------------------------------------------}
procedure TF_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TTFBlock; var B3: TTFBlock);
  {-xor two blocks, result in third}
begin
  asm
             mov   di,ds
             lds   si,[B1]
    db $66;  mov   ax,[si]
    db $66;  mov   bx,[si+4]
    db $66;  mov   cx,[si+8]
    db $66;  mov   dx,[si+12]
             lds   si,[B2]
    db $66;  xor   ax,[si]
    db $66;  xor   bx,[si+4]
    db $66;  xor   cx,[si+8]
    db $66;  xor   dx,[si+12]
             lds   si,[B3]
    db $66;  mov   [si],ax
    db $66;  mov   [si+4],bx
    db $66;  mov   [si+8],cx
    db $66;  mov   [si+12],dx
             mov   ds,di
  end;
end;

{$else}

{---------------------------------------------------------------------------}
procedure TF_XorBlock({$ifdef CONST} const {$else} var {$endif} B1, B2: TTFBlock; var B3: TTFBlock);
  {-xor two blocks, result in third}
var
  a1: TWA4 absolute B1;
  a2: TWA4 absolute B2;
  a3: TWA4 absolute B3;
begin
  a3[0] := a1[0] xor a2[0];
  a3[1] := a1[1] xor a2[1];
  a3[2] := a1[2] xor a2[2];
  a3[3] := a1[3] xor a2[3];
end;

{$endif BASM16}


{--------------------------------------------------------------------------}
procedure TF_SetFastInit(value: boolean);
  {-set FastInit variable}
begin
  FastInit := value;
end;


{---------------------------------------------------------------------------}
function TF_GetFastInit: boolean;
  {-Returns FastInit variable}
begin
  TF_GetFastInit := FastInit;
end;



{---------------------------------------------------------------------------}
{---------------  E n c r y p t  /  d e c r y p t  -------------------------}
{---------------------------------------------------------------------------}


{$ifndef BIT16}

{---------------------------------------------------------------------------}
procedure TF_Encrypt(var ctx: TTFContext; const BI: TTFBlock; var BO: TTFBlock);
  {-encrypt one block (in ECB mode)}
var
  X,Y: longint;
  T: TTFBlock;
  W: TWA4 absolute T;
  i,j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK)^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    for j:=0 to 7 do begin
      i := 4*j;
      {first part of double round}
      X := S0[W[0]        and $FF] xor S1[W[0] shr 8 and $FF] xor S2[W[0] shr 16 and $FF] xor S3[W[0] shr 24 and $FF];
      Y := S0[W[1] shr 24 and $FF] xor S1[W[1]       and $FF] xor S2[W[1] shr  8 and $FF] xor S3[W[1] shr 16 and $FF];
      {Pseudo Hadamard and rotation}
      W[3]:= (W[3] shl 1) or (W[3] shr 31);
      W[2]:=  W[2] xor (X +   Y + RK[i+8]);
      W[3]:=  W[3] xor (X + 2*Y + RK[i+9]);
      W[2]:= (W[2] shr 1) or (W[2] shl 31);
      {second part of double round}
      X := S0[W[2]        and $FF] xor S1[W[2] shr 8 and $FF] xor S2[W[2] shr 16 and $FF] xor S3[W[2] shr 24 and $FF];
      Y := S0[W[3] shr 24 and $FF] xor S1[W[3]       and $FF] xor S2[W[3] shr  8 and $FF] xor S3[W[3] shr 16 and $FF];
      {Pseudo Hadamard and rotation}
      W[1]:= (W[1] shl 1) or (W[1] shr 31);
      W[0]:=  W[0] xor (X +   Y + RK[i+10]);
      W[1]:=  W[1] xor (X + 2*Y + RK[i+11]);
      W[0]:= (W[0] shr 1) or (W[0] shl 31);
    end;
    {Store with final swap and output whitening}
    TWA4(BO)[0] := W[2] xor RK[4];
    TWA4(BO)[1] := W[3] xor RK[5];
    TWA4(BO)[2] := W[0] xor RK[6];
    TWA4(BO)[3] := W[1] xor RK[7];
  end;
end;


{---------------------------------------------------------------------------}
procedure TF_Decrypt(var ctx: TTFContext; {$ifdef CONST} const {$else} var {$endif}  BI: TTFBlock; var BO: TTFBlock);
  {-decrypt one block (in ECB mode)}
var
  X,Y: longint;
  T: TTFBlock;
  W: TWA4 absolute T;
  i,j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK[4])^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    for j:=0 to 7 do begin
      i := 4*j;
      {first part of double round}
      X := S0[W[0]        and $FF] xor S1[W[0] shr 8 and $FF] xor S2[W[0] shr 16 and $FF] xor S3[W[0] shr 24 and $FF];
      Y := S0[W[1] shr 24 and $FF] xor S1[W[1]       and $FF] xor S2[W[1] shr  8 and $FF] xor S3[W[1] shr 16 and $FF];
      {Pseudo Hadamard and rotation}
      W[2]:= (W[2] shl 1) or (W[2] shr 31);
      W[3]:=  W[3] xor (X + 2*Y + RK[39-i]);
      W[2]:=  W[2] xor (X +   Y + RK[38-i]);
      W[3]:= (W[3] shr 1) or (W[3] shl 31);
      {second part of double round}
      X := S0[W[2]        and $FF] xor S1[W[2] shr 8 and $FF] xor S2[W[2] shr 16 and $FF] xor S3[W[2] shr 24 and $FF];
      Y := S0[W[3] shr 24 and $FF] xor S1[W[3]       and $FF] xor S2[W[3] shr  8 and $FF] xor S3[W[3] shr 16 and $FF];
      {Pseudo Hadamard and rotation}
      W[0]:= (W[0] shl 1) or (W[0] shr 31);
      W[1]:=  W[1] xor (X + 2*Y + RK[37-i]);
      W[0]:=  W[0] xor (X +   Y + RK[36-i]);
      W[1]:= (W[1] shr 1) or (W[1] shl 31);
    end;
    {Store with final swap and output whitening}
    TWA4(BO)[0] := TWA4(T)[2] xor RK[0];
    TWA4(BO)[1] := TWA4(T)[3] xor RK[1];
    TWA4(BO)[2] := TWA4(T)[0] xor RK[2];
    TWA4(BO)[3] := TWA4(T)[1] xor RK[3];
  end;
end;


{$else}

{$ifdef BASM16}
{---------------------------------------------------------------------------}
procedure TF_Encrypt(var ctx: TTFContext; {$ifdef CONST} const {$else} var {$endif}  BI: TTFBlock; var BO: TTFBlock);
  {-encrypt one block (in ECB mode)}
var
  T: TTFBlock;
  W: TWA4 absolute T;
  j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK)^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    asm
              mov   [j],8
              push  ds                        {save ds}
              lds   si,[ctx]
              lea   si,TTFContext[si].S0      {ds:si -> ctx.S0}
              les   di,[ctx]
              lea   di,TTFContext[di].RK+8*4  {es:di -> ctx.RK[8]}

@@1:  {first part of double round}
      {X := S0[T[00]] xor S1[T[01]] xor S2[T[02]] xor S3[T[03]];}
              mov   bl,byte ptr T[0]
              sub   bh,bh
              shl   bx,2
      db $66; mov   cx,[si+bx]
              mov   bl,byte ptr T[1]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$400]
              mov   bl,byte ptr T[2]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$800]
              mov   bl,byte ptr T[3]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$C00]
      {Y := S0[T[07]] xor S1[T[04]] xor S2[T[05]] xor S3[T[06]];}
              mov   bl,byte ptr T[7]
              sub   bh,bh
              shl   bx,2
      db $66; mov   dx,[si+bx]
              mov   bl,byte ptr T[4]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$400]
              mov   bl,byte ptr T[5]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$800]
              mov   bl,byte ptr T[6]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$C00]

      {Pseudo Hadamard and rotation}
      {W[2]:= W[2] xor (X +   Y + RK[i+8]);}
      db $66; mov   ax,es:[di]
              add   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; xor   ax,word ptr T[8]
      {W[2]:= RotR1(W[2]);}
      db $66; ror   ax,1
      db $66; mov   word ptr T[8],ax

      {W[3]:= RotL1(W[3]);}
      {W[3]:= W[3] xor (X + 2*Y + RK[i+9]);}
      db $66; mov   ax,es:[di]
              add   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; add   ax,dx
      db $66; mov   bx,word ptr T[12]
      db $66; rol   bx,1
      db $66; xor   ax,bx
      db $66; mov   word ptr T[12],ax

      {second part of double round}
      {X := S0[T[08]] xor S1[T[09]] xor S2[T[10]] xor S3[T[11]];}
              mov   bl,byte ptr T[8]
              sub   bh,bh
              shl   bx,2
      db $66; mov   cx,[si+bx]
              mov   bl,byte ptr T[9]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$400]
              mov   bl,byte ptr T[10]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$800]
              mov   bl,byte ptr T[11]
              sub   bh,bh
              shl   bx,2
      db $66;  xor  cx,[si+bx+$C00]
      {Y := S0[T[15]] xor S1[T[12]] xor S2[T[13]] xor S3[T[14]];}
              mov   bl,byte ptr T[15]
              sub   bh,bh
              shl   bx,2
      db $66; mov   dx,[si+bx]
              mov   bl,byte ptr T[12]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$400]
              mov   bl,byte ptr T[13]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$800]
              mov   bl,byte ptr T[14]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$C00]

      {Pseudo Hadamard and rotation}
      {W[0]:=  W[0] xor (X +   Y + RK[i+10]);}
      db $66; mov   ax,es:[di]
              add   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; xor   ax,word ptr T[0]
      {W[0]:= RotR1(W[0]);}
      db $66; ror   ax,1
      db $66; mov   word ptr T[0],ax

      {W[1]:= RotL1(W[1]);}
      {W[1]:= W[1] xor (X + 2*Y + RK[i+11]);}
      db $66; mov   ax,es:[di]
              add   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; add   ax,dx
      db $66; mov   bx,word ptr T[4]
      db $66; rol   bx,1
      db $66; xor   ax,bx
      db $66; mov   word ptr T[4],ax

              dec   [j]
              jnz   @@1

              pop   ds
    end;
    {Store with final swap and output whitening}
    TWA4(BO)[0] := W[2] xor RK[4];
    TWA4(BO)[1] := W[3] xor RK[5];
    TWA4(BO)[2] := W[0] xor RK[6];
    TWA4(BO)[3] := W[1] xor RK[7];
  end;
end;


{---------------------------------------------------------------------------}
procedure TF_Decrypt(var ctx: TTFContext; {$ifdef CONST} const {$else} var {$endif}  BI: TTFBlock; var BO: TTFBlock);
  {-decrypt one block (in ECB mode)}
var
  T: TTFBlock;
  W: TWA4 absolute T;
  j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK[4])^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    asm
              mov   [j],8
              push  ds                        {save ds}
              lds   si,[ctx]
              lea   si,TTFContext[si].S0      {ds:si -> ctx.S0}
              les   di,[ctx]
              lea   di,TTFContext[di].RK+39*4 {es:di -> ctx.RK[39]}
@@1:  {first part of double round}
      {X := S0[T[00]] xor S1[T[01]] xor S2[T[02]] xor S3[T[03]];}
              mov   bl,byte ptr T[0]
              sub   bh,bh
              shl   bx,2
      db $66; mov   cx,[si+bx]
              mov   bl,byte ptr T[1]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$400]
              mov   bl,byte ptr T[2]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$800]
              mov   bl,byte ptr T[3]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$C00]
      {Y := S0[T[07]] xor S1[T[04]] xor S2[T[05]] xor S3[T[06]];}
              mov   bl,byte ptr T[7]
              sub   bh,bh
              shl   bx,2
      db $66; mov   dx,[si+bx]
              mov   bl,byte ptr T[4]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$400]
              mov   bl,byte ptr T[5]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$800]
              mov   bl,byte ptr T[6]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$C00]
      {Pseudo Hadamard and rotation}
      {W[2]:= RotL1(W[2])}
      db $66; rol   word ptr T[8],1
      {W[3]:=  W[3] xor (X + 2*Y + RK[39-i]);}
      db $66; mov   ax,es:[di]
              sub   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; add   ax,dx
      db $66; xor   word ptr T[12],ax
      {W[2]:=  W[2] xor (X +   Y + RK[38-i]);}
      db $66; mov   ax,es:[di]
              sub   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; xor   word ptr T[8],ax
      {W[3]:= RotR1(W[3]);}
      db $66; ror   word ptr T[12],1

      {second part of double round}
      {X := S0[T[08]] xor S1[T[09]] xor S2[T[10]] xor S3[T[11]];}
              mov   bl,byte ptr T[8]
              sub   bh,bh
              shl   bx,2
      db $66; mov   cx,[si+bx]
              mov   bl,byte ptr T[9]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$400]
              mov   bl,byte ptr T[10]
              sub   bh,bh
              shl   bx,2
      db $66; xor   cx,[si+bx+$800]
              mov   bl,byte ptr T[11]
              sub   bh,bh
              shl   bx,2
      db $66;  xor  cx,[si+bx+$C00]
      {Y := S0[T[15]] xor S1[T[12]] xor S2[T[13]] xor S3[T[14]];}
              mov   bl,byte ptr T[15]
              sub   bh,bh
              shl   bx,2
      db $66; mov   dx,[si+bx]
              mov   bl,byte ptr T[12]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$400]
              mov   bl,byte ptr T[13]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$800]
              mov   bl,byte ptr T[14]
              sub   bh,bh
              shl   bx,2
      db $66; xor   dx,[si+bx+$C00]
      {Pseudo Hadamard and rotation}
      {W[0]:= RotL1(W[0]);}
      db $66; rol   word ptr T[0],1
      {W[1]:=  W[1] xor (X + 2*Y + RK[37-i]);}
      db $66; mov   ax,es:[di]
              sub   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; add   ax,dx
      db $66; xor   word ptr T[4],ax
      {W[0]:=  W[0] xor (X +   Y + RK[36-i]);}
      db $66; mov   ax,es:[di]
              sub   di,4
      db $66; add   ax,cx
      db $66; add   ax,dx
      db $66; xor   word ptr T[0],ax
      {W[1]:= RotR1(W[1]);}
      db $66; ror   word ptr T[4],1

              dec   [j]
              jnz   @@1

              pop   ds
    end;

    {Store with final swap and output whitening}
    TWA4(BO)[0] := W[2] xor RK[0];
    TWA4(BO)[1] := W[3] xor RK[1];
    TWA4(BO)[2] := W[0] xor RK[2];
    TWA4(BO)[3] := W[1] xor RK[3];
  end;
end;


{$else}

{---------------------------------------------------------------------------}
procedure TF_Encrypt(var ctx: TTFContext; {$ifdef CONST} const {$else} var {$endif}  BI: TTFBlock; var BO: TTFBlock);
  {-encrypt one block (in ECB mode)}
var
  X,Y: longint;
  T: TTFBlock;
  W: TWA4 absolute T;
  i,j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK)^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    for j:=0 to 7 do begin
      i := 4*j;
      {first part of double round}
      X := S0[T[00]] xor S1[T[01]] xor S2[T[02]] xor S3[T[03]];
      Y := S0[T[07]] xor S1[T[04]] xor S2[T[05]] xor S3[T[06]];
      {Pseudo Hadamard and rotation}
      W[2]:= RotR1(W[2]  xor (X +   Y + RK[i+8]));
      W[3]:= RotL1(W[3]) xor (X + 2*Y + RK[i+9]);
      {second part of double round}
      X := S0[T[08]] xor S1[T[09]] xor S2[T[10]] xor S3[T[11]];
      Y := S0[T[15]] xor S1[T[12]] xor S2[T[13]] xor S3[T[14]];
      {Pseudo Hadamard and rotation}
      W[0]:= RotR1(W[0]  xor (X +   Y + RK[i+10]));
      W[1]:= RotL1(W[1]) xor (X + 2*Y + RK[i+11]);
    end;
    {Store with final swap and output whitening}
    TWA4(BO)[0] := W[2] xor RK[4];
    TWA4(BO)[1] := W[3] xor RK[5];
    TWA4(BO)[2] := W[0] xor RK[6];
    TWA4(BO)[3] := W[1] xor RK[7];
  end;
end;



{---------------------------------------------------------------------------}
procedure TF_Decrypt(var ctx: TTFContext; {$ifdef CONST} const {$else} var {$endif}  BI: TTFBlock; var BO: TTFBlock);
  {-decrypt one block (in ECB mode)}
var
  X,Y: longint;
  T: TTFBlock;
  W: TWA4 absolute T;
  i,j: integer;
begin
  with ctx do begin
    {Get local copy with input whitening}
    TF_XorBlock(BI,PTFBlock(@RK[4])^,T);
    {perform eight double rounds, this avoids swapping needed for 16 single rounds}
    for j:=0 to 7 do begin
      i := 4*j;
      {first part of double round}
      X := S0[T[00]] xor S1[T[01]] xor S2[T[02]] xor S3[T[03]];
      Y := S0[T[07]] xor S1[T[04]] xor S2[T[05]] xor S3[T[06]];
      {Pseudo Hadamard and rotation}
      W[3]:= RotR1(W[3]  xor (X + 2*Y + RK[39-i]));
      W[2]:= RotL1(W[2]) xor (X +   Y + RK[38-i]);
      {second part of double round}
      X := S0[T[08]] xor S1[T[09]] xor S2[T[10]] xor S3[T[11]];
      Y := S0[T[15]] xor S1[T[12]] xor S2[T[13]] xor S3[T[14]];
      {Pseudo Hadamard and rotation}
      W[1]:= RotR1(W[1]  xor (X + 2*Y + RK[37-i]));
      W[0]:= RotL1(W[0]) xor (X +   Y + RK[36-i]);
    end;
    {Store with final swap and output whitening}
    TWA4(BO)[0] := W[2] xor RK[0];
    TWA4(BO)[1] := W[3] xor RK[1];
    TWA4(BO)[2] := W[0] xor RK[2];
    TWA4(BO)[3] := W[1] xor RK[3];
  end;
end;

{$endif}

{$endif}



{---------------------------------------------------------------------------}
{-------------------  K e y   s e t u p ------------------------------------}
{---------------------------------------------------------------------------}


{Turn off range checking for byte shifts}
{$ifopt R+} {$define SetRPlus} {$else} {$undef SetRPlus} {$endif}
{$R-}

{$ifndef BIT16}

{Reed-Solomon calculation adapted from Wei Dai's Crypto++}

{---------------------------------------------------------------------------}
function rs_mod(c: longint): longint;
  {-compute c*x^4 mod (x^4 + (a+1/a)*x^3 + a*x^2 + (a+1/a)*x + 1) over GF(256)}
var
  c1,c2: longint;
begin
  if c and $80 = 0 then c2 := c shl 1 else c2 := (c shl 1) xor $14d;
  if odd(c) then c1 := c2 xor (c shr 1) xor ($14d shr 1)
  else c1 := c2 xor (c shr 1);
  rs_mod := c or (c1 shl 8) or (c2 shl 16) or (c1 shl 24);
end;


{---------------------------------------------------------------------------}
function rs_mul(a,b: longint): longint;
  {-compute RS(12,8) code with the above polynomial as generator}
  { this is equivalent to multiplying by the RS matrix}
var
  i: integer;
begin
  for i:=0 to 7 do begin
    b := rs_mod(b shr 24) xor (b shl 8) xor (a shr 24);
    a := a shl 8;
  end;
  rs_mul := b;
end;

{$else}

{---------------------------------------------------------------------------}
procedure rs_mod(var x: longint);
  {-Special 16 bit version of the above function}
var
  c,c1,c2: byte;
  y: array[0..3] of byte absolute x;
begin
  c := y[3];
  if c and $80 = 0 then c2 := c shl 1
  else begin
    {c2 IS a byte because then $100 bit would be xored to zero with $14D}
    c2 := (c shl 1) xor $4d;
  end;
  if odd(c) then c1 := (c shr 1) xor $A6 xor c2   {Note: $A6 = $14D shr 1}
  else c1 := (c shr 1) xor c2;
  y[3] := y[2] xor c1;
  y[2] := y[1] xor c2;
  y[1] := y[0] xor c1;
  y[0] := c;
end;


{---------------------------------------------------------------------------}
function rs_mul(a, b: longint): longint;
  {-compute RS(12,8) code with the above polynomial as generator}
  { this is equivalent to multiplying by the RS matrix, 16 bit special}
var
  i: integer;
begin
  for i:=0 to 3 do rs_mod(b);
  b := b xor a;
  for i:=0 to 3 do rs_mod(b);
  rs_mul := b;
end;

{$endif}

{$ifdef SetRPlus}
  {$R+}
{$endif}


{---------------------------------------------------------------------------}
function TF_Init({$ifdef CONST} const {$else} var {$endif} Key; KeyBits: word; var ctx: TTFContext): integer;
  {-Twofish round key and key-dependent sbox initialisation}
var
  ka: array[0..31] of byte absolute key;
  i,j: integer;
  X,Y: longint;
  S: array[0..15] of byte;
  y0,y1,y2,y3,x0,x1,x2,x3: byte;
begin
  TF_Init := 0;

  if FastInit then begin
    {Clear only the necessary context data at init. IV and buf}
    {remain uninitialized, other fields are initialized below.}
    TF_Reset(ctx);
    {$ifdef CONST}
      ctx.IncProc := nil;
    {$else}
      {TP5-6 do not like IncProc := nil;}
      fillchar(ctx.IncProc, sizeof(ctx.IncProc), 0);
    {$endif}
  end
  else fillchar(ctx, sizeof(ctx), 0);

  if (KeyBits<>128) and (KeyBits<>192) and (KeyBits<>256) then begin
    TF_Init := TF_Err_Invalid_Key_Size;
    exit;
  end;

  {Reed-Solomon multiplication}
  for i:=0 to pred(KeyBits div 64) do begin
    TWA4(S)[i] := rs_mul(TWA8(key)[2*i], TWA8(key)[2*i+1]);
  end;

  with ctx do begin
    {calculate key-dependent sboxes}
    if KeyBits=128 then begin
      for j:=0 to 255 do begin
        S0[j] := mds0[q0[q0[j] xor S[0]] xor S[4]];
        S1[j] := mds1[q0[q1[j] xor S[1]] xor S[5]];
        S2[j] := mds2[q1[q0[j] xor S[2]] xor S[6]];
        S3[j] := mds3[q1[q1[j] xor S[3]] xor S[7]];
      end;
    end
    else if KeyBits=192 then begin
      for j:=0 to 255 do begin
        S0[j] := mds0[q0[q0[q1[j] xor S[0]] xor S[4]] xor S[ 8]];
        S1[j] := mds1[q0[q1[q1[j] xor S[1]] xor S[5]] xor S[ 9]];
        S2[j] := mds2[q1[q0[q0[j] xor S[2]] xor S[6]] xor S[10]];
        S3[j] := mds3[q1[q1[q0[j] xor S[3]] xor S[7]] xor S[11]];
      end;
    end
    else begin {KeyBits=256}
      for j:=0 to 255 do begin
        S0[j] := mds0[q0[q0[q1[q1[j] xor S[0]] xor S[4]] xor S[ 8]] xor S[12]];
        S1[j] := mds1[q0[q1[q1[q0[j] xor S[1]] xor S[5]] xor S[ 9]] xor S[13]];
        S2[j] := mds2[q1[q0[q0[q0[j] xor S[2]] xor S[6]] xor S[10]] xor S[14]];
        S3[j] := mds3[q1[q1[q0[q1[j] xor S[3]] xor S[7]] xor S[11]] xor S[15]];
      end;
    end;
    {calculate round keys, see [1], Section 4.3.2: The Function h}
    j := 0;
    while j<40 do begin
      i := j+1;
      if KeyBits=128 then begin
        x0 := q0[j] xor ka[ 8];
        x1 := q1[j] xor ka[ 9];
        x2 := q0[j] xor ka[10];
        x3 := q1[j] xor ka[11];
        y0 := q0[i] xor ka[12];
        y1 := q1[i] xor ka[13];
        y2 := q0[i] xor ka[14];
        y3 := q1[i] xor ka[15];
      end
      else if KeyBits=192 then begin
        x0 := q0[q1[j] xor ka[16]] xor ka[ 8];
        x1 := q1[q1[j] xor ka[17]] xor ka[ 9];
        x2 := q0[q0[j] xor ka[18]] xor ka[10];
        x3 := q1[q0[j] xor ka[19]] xor ka[11];
        y0 := q0[q1[i] xor ka[20]] xor ka[12];
        y1 := q1[q1[i] xor ka[21]] xor ka[13];
        y2 := q0[q0[i] xor ka[22]] xor ka[14];
        y3 := q1[q0[i] xor ka[23]] xor ka[15];
      end
      else begin
        x0 := q0[q1[q1[j] xor ka[24]] xor ka[16]] xor ka[ 8];
        x1 := q1[q1[q0[j] xor ka[25]] xor ka[17]] xor ka[ 9];
        x2 := q0[q0[q0[j] xor ka[26]] xor ka[18]] xor ka[10];
        x3 := q1[q0[q1[j] xor ka[27]] xor ka[19]] xor ka[11];
        y0 := q0[q1[q1[i] xor ka[28]] xor ka[20]] xor ka[12];
        y1 := q1[q1[q0[i] xor ka[29]] xor ka[21]] xor ka[13];
        y2 := q0[q0[q0[i] xor ka[30]] xor ka[22]] xor ka[14];
        y3 := q1[q0[q1[i] xor ka[31]] xor ka[23]] xor ka[15];
      end;
      X := mds0[q0[x0] xor ka[0]] xor mds1[q0[x1] xor ka[1]] xor mds2[q1[x2] xor ka[2]] xor mds3[q1[x3] xor ka[3]];
      Y := mds0[q0[y0] xor ka[4]] xor mds1[q0[y1] xor ka[5]] xor mds2[q1[y2] xor ka[6]] xor mds3[q1[y3] xor ka[7]];
      {$ifdef BASM16}
        asm  db $66; rol word ptr Y,8  end;
      {$else}
        Y := (Y shl 8) or (Y shr 24);
      {$endif}
      inc(X,Y);
      inc(Y,X);
      RK[j] := X;
      {$ifdef BASM16}
        asm  db $66; rol word ptr Y,9  end;
        RK[i] := Y;
      {$else}
        RK[i] := (Y shl 9) or (Y shr 23);
      {$endif}
      inc(j,2);
    end;
  end;
end;



end.
