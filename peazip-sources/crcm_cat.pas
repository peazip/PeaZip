unit crcm_cat;

{Catalogue of "Rocksoft^tm Model" parameters}


interface


(*************************************************************************

 DESCRIPTION     :  Catalogue of "Rocksoft^tm Model" parameters

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  [1] Ross Williams' public domain C sources crcmodel.c, crcmodel.h
                        in "A Painless Guide to CRC Error Detection Algorithms"
                        http://www.ross.net/crc/download/crc_v3.txt
                    [2] Greg Cook's Catalogue of Parameterised CRC Algorithms
                        http://reveng.sourceforge.net/crc-catalogue/
                    [3] Thomas Pircher's PYCRC
                        http://www.tty1.net/pycrc/crc-models.html
                    [4] Danjel McGougan's Universal_crc
                        http://mcgougan.se/universal_crc/


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     11.07.08  W.Ehrhardt  From crcmodel: CRC32_Zip, CRC16_CCITT, CRC24_PGP
 0.11     11.07.08  we          Add Greg Cook's catalogue
 0.12     11.07.08  we          Changed CRC8_ICODE.check to $7E, calculated with [4]
 0.13     12.07.08  we          Additional parameters from pycrc catalogue
 0.14     12.07.08  we          Added alias descriptions
 0.15     01.12.08  we          8 new parameter records from [2]
 0.16     25.04.09  we          Added CRC-10, CRC-16/CCITT from [2]
 0.17     04.06.09  we          Added CRC-5/ITU, CRC-32/Q  from [2]
 0.18     01.10.09  we          CRC-8/ITU, CRC-8/MAXIM, CRC-16/MAXIM, CRC-16/T10-DIF from [2]
 0.19     13.03.10  we          Added ten new parameter records from [2]
 0.20     09.12.13  we          Added 12 new parameter records from [2], split CRC6_DARC/A
 0.21     16.08.15  we          Removed $ifdef DLL / stdcall
 0.22     04.05.17  we          Added 31 new parameter records from [2]
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2008-2017 Wolfgang Ehrhardt

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
 crcmodel;

{---------------------------------------------------------------------------}
{---------------------------------------------------------------------------}

const
         CRC3_GSM: TCRCParam = (poly   : $3;         {V0.22, [2]}
                                init   : $0;
                                xorout : $7;
                                check  : $4;
                                width  : 3;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-3/GSM');

const
        CRC3_ROHC: TCRCParam = (poly   : $3;         {V0.19, [2]}
                                init   : $7;
                                xorout : $0;
                                check  : $6;
                                width  : 3;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-3/ROHC');

const
  CRC4_INTERLAKEN: TCRCParam = (poly   : $3;         {V0.22, [2]}
                                init   : $f;
                                xorout : $f;
                                check  : $b;
                                width  : 4;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-4/INTERLAKEN');

const
         CRC4_ITU: TCRCParam = (poly   : $3;         {V0.15, [2]}
                                init   : $0;
                                xorout : $0;
                                check  : $7;
                                width  : 4;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-4/ITU');

const
         CRC5_EPC: TCRCParam = (poly   : $09;        {V0.17, [2]}
                                Init   : $09;
                                XorOut : 00;
                                Check  : 00;
                                Width  : 5;
                                RefIn  : False;
                                RefOut : False;
                                name   : 'CRC-5/EPC');

const
         CRC5_ITU: TCRCParam = (poly   : $15;        {V0.15, [2]}
                                init   : $0;
                                xorout : $0;
                                check  : $07;
                                width  : 5;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-5/ITU');

const
         CRC5_USB: TCRCParam = (poly   : $05;
                                init   : $1F;
                                xorout : $1F;
                                check  : $19;
                                width  : 5;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-5/USB');

const
         CRC6_ITU: TCRCParam = (poly   : $03;        {V0.15, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $06;
                                width  : 6;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-6/ITU');

const
   CRC6_CDMA2000A: TCRCParam = (poly   : $27;        {V0.20, [2]}
                                init   : $3f;
                                xorout : $00;
                                check  : $0d;
                                width  : 6;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-6/CDMA2000-A');

const
   CRC6_CDMA2000B: TCRCParam = (poly   : $07;        {V0.20, [2]}
                                init   : $3f;
                                xorout : $00;
                                check  : $3b;
                                width  : 6;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-6/CDMA2000-B');

const
        CRC6_DARC: TCRCParam = (poly   : $19;        {V0.20, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $26;
                                width  : 6;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-6/DARC');

const
       CRC6_DARCA: TCRCParam = (poly   : $19;        {V0.20, renamed to DARC-A}
                                init   : $00;
                                xorout : $00;
                                check  : $19;
                                width  : 6;
                                refin  : true;
                                refout : false;
                                name   : 'CRC-6/DARC-A');
const
         CRC6_GSM: TCRCParam = (poly   : $2f;        {V0.22, [2]}
                                init   : $00;
                                xorout : $3f;
                                check  : $13;
                                width  : 6;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-6/GSM');


const
             CRC7: TCRCParam = (poly   : $09;        {V0.15, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $75;
                                width  : 7;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-7');

const
        CRC7_ROHC: TCRCParam = (poly   : $4F;        {V0.19, [2]}
                                init   : $7F;
                                xorout : $0;
                                check  : $53;
                                width  : 7;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-7/ROHC');
const
        CRC7_UMTS: TCRCParam = (poly   : $45;        {V0.22, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $61;
                                width  : 7;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-7/UMTS');
const
             CRC8: TCRCParam = (poly   : $07;
                                init   : $00;
                                xorout : $00;
                                check  : $F4;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8');

const
     CRC8_AUTOSAR: TCRCParam = (poly   : $2f;        {V0.22, [2]}
                                init   : $ff;
                                xorout : $ff;
                                check  : $df;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/AUTOSAR');

const
    CRC8_CDMA2000: TCRCParam = (poly   : $9b;        {V0.20, [2]}
                                init   : $ff;
                                xorout : $00;
                                check  : $da;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/CDMA2000');

const
        CRC8_DARC: TCRCParam = (poly   : $39;        {V0.19, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $15;
                                width  : 8;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-8/DARC');
const
      CRC8_DVB_S2: TCRCParam = (poly   : $d5;        {V0.22, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $bc;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/DVB-S2');



const
         CRC8_EBU: TCRCParam = (poly   : $1d;        {V0.20, [2]}
                                init   : $ff;
                                xorout : $00;
                                check  : $97;
                                width  : 8;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-8/EBU');
const
       CRC8_GSM_A: TCRCParam = (poly   : $1d;        {V0.22, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $37;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/GSM-A');
const
       CRC8_GSM_B: TCRCParam = (poly   : $49;        {V0.22, [2]}
                                init   : $00;
                                xorout : $ff;
                                check  : $94;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/GSM-B');

const
       CRC8_ICODE: TCRCParam = (poly   : $1D;
                                init   : $FD;
                                xorout : $00;
                                check  : $7E;        {calculated with [4]}
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/I-CODE');

const
         CRC8_ITU: TCRCParam = (poly   : $07;        {V0.18, [2]}
                                init   : $00;
                                xorout : $55;
                                check  : $A1;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/ITU');

const
         CRC8_LTE: TCRCParam = (poly   : $9b;        {V0.22, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $ea;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/LTE');
const
       CRC8_MAXIM: TCRCParam = (poly   : $31;        {V0.18, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $A1;
                                width  : 8;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-8/MAXIM');
                               {Alias  : 'CRC-8/Dallas-1-Wire'}   {Ref: [3]}
const
  CRC8_OPENSAFETY: TCRCParam = (poly   : $2f;        {V0.22, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $3e;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/OPENSAFETY');


const
        CRC8_ROHC: TCRCParam = (poly   : $07;        {V0.19, [2]}
                                init   : $FF;
                                xorout : $0;
                                check  : $D0;
                                width  : 8;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-8/ROHC');
const
   CRC8_SAE_J1850: TCRCParam = (poly   : $1d;        {V0.22, [2]}
                                init   : $ff;
                                xorout : $ff;
                                check  : $4b;
                                width  : 8;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-8/SAE-J1850');

const
       CRC8_WCDMA: TCRCParam = (poly   : $9B;        {V0.19, [2]}
                                init   : $00;
                                xorout : $00;
                                check  : $25;
                                width  : 8;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-8/WCDMA');

const
            CRC10: TCRCParam = (poly   : $233;       {V0.16, [2]}
                                init   : $000;
                                xorout : $000;
                                check  : $199;
                                width  : 10;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-10');

const
   CRC10_CDMA2000: TCRCParam = (poly   : $3d9;       {V0.22, [2]}
                                init   : $3ff;
                                xorout : $000;
                                check  : $233;
                                width  : 10;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-10/CDMA2000');
const
        CRC10_GSM: TCRCParam = (poly   : $175;       {V0.22, [2]}
                                init   : $000;
                                xorout : $3ff;
                                check  : $12a;
                                width  : 10;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-10/GSM');

const
            CRC11: TCRCParam = (poly   : $385;
                                init   : $01A;
                                xorout : $000;
                                check  : $5A3;
                                width  : 11;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-11');

const
       CRC11_UMTS: TCRCParam = (poly   : $307;       {V0.22, [2]}
                                init   : $000;
                                xorout : $000;
                                check  : $061;
                                width  : 11;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-11/UMTS');
const
            CRC12: TCRCParam = (poly   : $80F;       {V0.19, [2]}
                                init   : $000;
                                xorout : $000;
                                check  : $DAF;
                                width  : 12;
                                refin  : false;
                                refout : true;
                                name   : 'CRC-12');

const
   CRC12_CDMA2000: TCRCParam = (poly   : $f13;       {V0.20, [2]}
                                init   : $fff;
                                xorout : $000;
                                check  : $d4d;
                                width  : 12;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-12/CDMA2000');

const
       CRC12_DECT: TCRCParam = (poly   : $80f;       {V0.20, [2]}
                                init   : $000;
                                xorout : $000;
                                check  : $f5b;
                                width  : 12;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-12/DECT');
const
        CRC12_GSM: TCRCParam = (poly   : $d31;       {V0.22, [2]}
                                init   : $000;
                                xorout : $fff;
                                check  : $b34;
                                width  : 12;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-12/GSM');
const
       CRC12_UMTS: TCRCParam = (poly   : $80f;       {V0.22, [2]}
                                init   : $000;
                                xorout : $000;
                                check  : $daf;
                                width  : 12;
                                refin  : false;
                                refout : true;
                                name   : 'CRC-12/UMTS');
const
        CRC13_BBC: TCRCParam = (poly   : $1cf5;      {V0.20, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $04fa;
                                width  : 13;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-13/BBC');

const
       CRC14_DARC: TCRCParam = (poly   : $0805;      {V0.19, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $082D;
                                width  : 14;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-14/DARC');
const
        CRC14_GSM: TCRCParam = (poly   : $202d;      {V0.22, [2]}
                                init   : $0000;
                                xorout : $3fff;
                                check  : $30ae;
                                width  : 14;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-14/GSM');

const
            CRC15: TCRCParam = (poly   : $4599;
                                init   : $0000;
                                xorout : $0000;
                                check  : $059E;
                                width  : 15;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-15');

const
    CRC15_MPT1327: TCRCParam = (poly   : $6815;      {V0.20, [2]}
                                init   : $0000;
                                xorout : $0001;
                                check  : $2566;
                                width  : 15;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-15/MPT1327');

const
       CRC16_ARC:  TCRCParam = (poly   : $8005;
                                init   : $0000;
                                xorout : $0000;
                                check  : $BB3D;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/ARC');
                               {alias  : 'CRC-16/LHA'}

const
       CRC16_ATOM: TCRCParam = (poly   : $002D;
                                init   : $0000;
                                xorout : $0000;
                                check  : $4287;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/ATOM');

const
          CRC16_A: TCRCParam = (poly   : $1021;      {V0.22, [2]}
                                init   : $c6c6;
                                xorout : $0000;
                                check  : $bf05;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-A');
const
  CRC16_AUG2_CITT: TCRCParam = (poly   : $1021;
                                init   : $84C0;
                                xorout : $0000;
                                check  : $19CF;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/AUG-2-CCITT');
                               {alias  : 'CRC-16/SPI-FUJITSU'}
                               {alias  : 'CRC-16/AUG-2-CITT')}

const
   CRC16_AUG_CITT: TCRCParam = (poly   : $1021;
                                init   : $1D0F;
                                xorout : $0000;
                                check  : $E5CC;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/AUG-CCITT');
                               {alias  : 'CRC-16/AUG-CITT')}

const
    CRC16_BT_CHIP: TCRCParam = (poly   : $1021;
                                init   : $FFFF;
                                xorout : $0000;
                                check  : $89F6;
                                width  : 16;
                                refin  : true;
                                refout : false;
                                name   : 'CRC-16/BT-CHIP');

const
    CRC16_BUYPASS: TCRCParam = (poly   : $8005;
                                init   : $0000;
                                xorout : $0000;
                                check  : $FEE8;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/BUYPASS');

const
   CRC16_CDMA2000: TCRCParam = (poly   : $c867;      {V0.20, [2]}
                                init   : $ffff;
                                xorout : $0000;
                                check  : $4c06;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/CDMA2000');

const
       CRC16_CITT: TCRCParam = (poly   : $1021;
                                init   : $FFFF;
                                xorout : $0000;
                                check  : $29B1;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/CCITT');       {V0.16}
                               {alias  : 'CRC-16/CCITT-FALSE'}
const
        CRC16_CMS: TCRCParam = (poly   : $8005;      {V0.22, [2]}
                                init   : $ffff;
                                xorout : $0000;
                                check  : $aee7;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/CMS');
const
      CRC16_DECTX: TCRCParam = (poly   : $0589;      {V0.20, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $007f;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/DECT-X');

const
     CRC16_DDS110: TCRCParam = (poly   : $8005;      {V0.19, [2]}
                                init   : $800D;
                                xorout : $0000;
                                check  : $9ECF;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/DDS-110');

const
        CRC16_DNP: TCRCParam = (poly   : $3D65;
                                init   : $0000;
                                xorout : $FFFF;
                                check  : $EA82;
                                Width  : 16;
                                RefIn  : true;
                                RefOut : true;
                                Name   : 'CRC-16/DNP');

const
   CRC16_EN_13757: TCRCParam = (poly   : $3D65;      {V0.15, [2]}
                                init   : $0000;
                                xorout : $FFFF;
                                check  : $C2B7;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/EN-13757');

const
    CRC16_GENIBUS: TCRCParam = (poly   : $1021;      {V0.22, [2]}
                                init   : $ffff;
                                xorout : $ffff;
                                check  : $d64e;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/GENIBUS');
const
        CRC16_GSM: TCRCParam = (poly   : $1021;      {V0.22, [2]}
                                init   : $0000;
                                xorout : $ffff;
                                check  : $ce3c;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/GSM');
const
      CRC16_ICODE: TCRCParam = (poly   : $1021;
                                init   : $FFFF;
                                xorout : $FFFF;
                                check  : $D64E;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/I-CODE');
                               {alias  :  CRC-16/GENIBUS}

const
     CRC16_KERMIT: TCRCParam = (poly   : $1021;
                                init   : $0000;
                                xorout : $0000;
                                check  : $2189;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/KERMIT');
                               {alias  : 'CRC-16/CCITT-TRUE'}
const
     CRC16_LJ1200: TCRCParam = (poly   : $6f63;      {V0.22, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $bdf4;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/LJ1200');

const
      CRC16_MAXIM: TCRCParam = (poly   : $8005;     {V0.18, [2]}
                                init   : $0000;
                                xorout : $FFFF;
                                check  : $44C2;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/MAXIM');
const
    CRC16_MCRF4XX: TCRCParam = (poly   : $1021;
                                init   : $FFFF;
                                xorout : $0000;
                                check  : $6F91;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/MCRF4XX');

const
     CRC16_MODBUS: TCRCParam = (poly   : $8005;
                                init   : $FFFF;
                                xorout : $0000;
                                check  : $4B37;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/MODBUS');

const
CRC16_OPENSAFETY_A: TCRCParam= (poly   : $5935;      {V0.22, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $5d38;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/OPENSAFETY-A');
const
CRC16_OPENSAFETY_B: TCRCParam= (poly   : $755b;      {V0.22, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $20fe;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/OPENSAFETY-B');

const
   CRC16_PROFIBUS: TCRCParam = (poly   : $1dcf;      {V0.22, [2]}
                                init   : $ffff;
                                xorout : $ffff;
                                check  : $a819;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/PROFIBUS');
const
          CRC16_R: TCRCParam = (poly   : $0589;      {Ref: [3]}
                                init   : $0000;
                                xorout : $0001;
                                check  : $007E;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/R');

const
     CRC16_RIELLO: TCRCParam = (poly   : $1021;      {V0.16, [2]}
                                init   : $B2AA;
                                xorout : $0000;
                                check  : $63D0;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/RIELLO');

const
    CRC16_T10_DIF: TCRCParam = (poly   : $8BB7;      {V0.18, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $D0DB;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/T10-DIF');

const
   CRC16_TELEDISK: TCRCParam = (poly   : $A097;      {V0.19, [2]}
                                init   : $0000;
                                xorout : $0000;
                                check  : $0FB3;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/TELEDISK');

const
   CRC16_TMS37157: TCRCParam = (poly   : $1021;      {V0.20, [2]}
                                init   : $89ec;
                                xorout : $0000;
                                check  : $26b1;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/TMS37157');

const
        CRC16_USB: TCRCParam = (poly   : $8005;
                                init   : $FFFF;
                                xorout : $FFFF;
                                check  : $B4C8;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/USB');
const
        CRC16_X25: TCRCParam = (poly   : $1021;
                                init   : $FFFF;
                                xorout : $FFFF;
                                check  : $906E;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/X-25');
                               {alias  : 'CRC-16/IBM-SDLC'}
                               {alias  : 'CRC-16/ISO-HDLC'}

const
    CRC16_XKERMIT: TCRCParam = (poly   : $8408;
                                init   : $0000;
                                xorout : $0000;
                                check  : $0C73;
                                width  : 16;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-16/X-KERMIT');
                               {alias  : 'X-XMODEM'}

const
     CRC16_ZMODEM: TCRCParam = (poly   : $1021;
                                init   : $0000;
                                xorout : $0000;
                                check  : $31C3;
                                width  : 16;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-16/ZMODEM');
                               {alias  : 'XMODEM'}
                               {alias  : 'CRC-16/ACORN'}
const
        CRC24_BLE: TCRCParam = (poly   : $00065b;    {V0.22, [2]}
                                init   : $555555;
                                xorout : $000000;
                                check  : $c25a56;
                                width  : 24;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-24/BLE');

const
 CRC24_INTERLAKEN: TCRCParam = (poly   : $328b63;    {V0.22, [2]}
                                init   : $ffffff;
                                xorout : $ffffff;
                                check  : $b4f3e6;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-24/INTERLAKEN');

const
      CRC24_LTE_B: TCRCParam = (poly   : $800063;    {V0.22, [2]}
                                init   : $000000;
                                xorout : $000000;
                                check  : $23ef52;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-24/LTE-B');

const
      CRC24_LTE_A: TCRCParam = (poly   : $864cfb;    {V0.22, [2]}
                                init   : $000000;
                                xorout : $000000;
                                check  : $cde703;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-24/LTE-A');

const
       CRC24_PGP:  TCRCParam = (poly   : $864CFB;
                                init   : $B704CE;
                                xorout : 0;
                                check  : $21CF02;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC24/PGP');
                               {alias  : 'CRC-24'}
                               {alias  : 'CRC-24/OPENPGP'}

const
   CRC24_FLEXRAYA: TCRCParam = (poly   : $5D6DCB;
                                init   : $FEDCBA;
                                xorout : $000000;
                                check  : $7979BD;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-24/FLEXRAY-A');

const
   CRC24_FLEXRAYB: TCRCParam = (poly   : $5D6DCB;
                                init   : $ABCDEF;
                                xorout : $000000;
                                check  : $1F23B8;
                                width  : 24;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-24/FLEXRAY-B');
const
       CRC30_CDMA: TCRCParam = (poly   : $2030b9c7;  {V0.22, [2]}
                                init   : $3fffffff;
                                xorout : $3fffffff;
                                check  : $04c34abf;
                                width  : 30;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-30/CDMA');
const
    CRC31_PHILIPS: TCRCParam = (poly   : $04c11db7;  {V0.20, [2]}
                                init   : $7fffffff;
                                xorout : $7fffffff;
                                check  : $0ce9e46c;
                                width  : 31;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-31/PHILIPS');

{Note: longint typecasts avoid some compiler warnings/errors}
const
    CRC32_AUTOSAR: TCRCParam = (poly   : longint($f4acfb13);  {V0.22, [2]}
                                init   : longint($ffffffff);
                                xorout : longint($ffffffff);
                                check  : $1697d06a;
                                width  : 32;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-32/AUTOSAR');

const
        CRC32_Zip: TCRCParam = (poly   : longint($04C11DB7);
                                init   : longint($FFFFFFFF);
                                xorout : longint($FFFFFFFF);
                                check  : longint($CBF43926);
                                width  : 32;
                                refin  : true;
                                refout : true;
                                name   : 'CRC32/Zip');
                               {alias  : 'CRC-32'}
                               {alias  : 'CRC-32/ADCCP'}
                               {alias  : 'PKZIP'}

const
      CRC32_BZIP2: TCRCParam = (poly   : longint($04C11DB7);  {V0.15, [2]}
                                init   : longint($FFFFFFFF);
                                xorout : longint($FFFFFFFF);
                                check  : longint($FC891918);
                                width  : 32;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-32/BZIP2');

const
          CRC32_C: TCRCParam = (poly   : longint($1EDC6F41);
                                init   : longint($FFFFFFFF);
                                xorout : longint($FFFFFFFF);
                                check  : longint($E3069283);
                                width  : 32;
                                refin  : true;
                                refout : true;
                                name   : 'CRC32/C');
                               {alias  : 'CRC-32/ISCSI'}
                               {alias  : 'CRC-32/CASTAGNOLI'}

const
          CRC32_D: TCRCParam = (poly   : longint($A833982B);  {V0.15, [2]}
                                init   : longint($FFFFFFFF);
                                xorout : longint($FFFFFFFF);
                                check  : longint($87315576);
                                width  : 32;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-32/D');

const
     CRC32_JAMCRC: TCRCParam = (poly   : longint($04C11DB7);
                                init   : longint($FFFFFFFF);
                                xorout : longint($00000000);
                                check  : longint($340BC6D9);
                                width  : 32;
                                refin  : true;
                                refout : true;
                                name   : 'CRC-32/JAMCRC');

const
      CRC32_MPEG2: TCRCParam = (poly   : longint($04C11DB7);  {V0.15, [2]}
                                init   : longint($FFFFFFFF);
                                xorout : longint($00000000);
                                check  : longint($0376E6E7);
                                width  : 32;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-32/MPEG-2');

const
      CRC32_POSIX: TCRCParam = (poly   : longint($04C11DB7);
                                init   : longint($00000000);
                                xorout : longint($FFFFFFFF);
                                check  : longint($765E7680);
                                width  : 32;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-32/POSIX');
                               {alias  : 'CKSUM'}

const
          CRC32_Q: TCRCParam = (poly   : longint($814141AB);  {V0.17, [2]}
                                Init   : longint($00000000);
                                XorOut : longint($00000000);
                                Check  : longint($3010BF7F);
                                Width  : 32;
                                RefIn  : False;
                                RefOut : False;
                                Name   : 'CRC-32/Q');

const
       CRC32_XFER: TCRCParam = (poly   : longint($000000AF);
                                init   : longint($00000000);
                                xorout : longint($00000000);
                                check  : longint($BD0BE338);
                                width  : 32;
                                refin  : false;
                                refout : false;
                                name   : 'CRC-32/XFER');


implementation

end.
