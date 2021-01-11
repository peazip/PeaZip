(*************************************************************************

 DESCRIPTION     :  GUI demo for crcmodel/crcm_cat

 REQUIREMENTS    :  D3-D7/D9-D10

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODUS   :  ---

 REFERENCES      :  ---

 REMARK          :  For Delphi2 ignore/remove all unsupported properties

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     18.08.08  W.Ehrhardt  Initial version
 0.11     20.08.08  we          Clear Lab_InvHex for empty HEX input, add comments
 0.12     06.09.08  we          Version display, finalize for web upload
 0.13     24.09.08  we          Max input length 1000, ansistring
 **************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2008 Wolfgang Ehrhardt

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

{***************************************************************
 *
 * Unit Name: tcrc16u
 * Purpose  : Calculate and display CRC16 values for crcm_cat algorithms
 * Author   : W.Ehrhardt
 * History  : 1.00  Initinal version
 *          : 1.01  Clear Lab_InvHex for empty HEX input, add comments
 *
 ****************************************************************}

unit tcrc16u;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;         {HEX/string input control}
    CB_Format: TComboBox; {HEX/string input format control}
    GroupBox1: TGroupBox;
    LN1: TLabel;          {LNx: labels for parax.name}
    LN2: TLabel;          {LVx: labels for crc value with parax}
    LN3: TLabel;
    LN4: TLabel;
    LN5: TLabel;
    LN6: TLabel;
    LN7: TLabel;
    LN8: TLabel;
    LN9: TLabel;
    LV1: TLabel;
    LV2: TLabel;
    LV3: TLabel;
    LV4: TLabel;
    LV5: TLabel;
    LV6: TLabel;
    LV7: TLabel;
    LV8: TLabel;
    LV9: TLabel;
    LN10: TLabel;
    LN11: TLabel;
    LN12: TLabel;
    LN13: TLabel;
    LN14: TLabel;
    LN15: TLabel;
    LN16: TLabel;
    LN17: TLabel;
    LN18: TLabel;
    LN19: TLabel;
    LV10: TLabel;
    LN20: TLabel;
    LV11: TLabel;
    LV12: TLabel;
    LV13: TLabel;
    LV14: TLabel;
    LV15: TLabel;
    LV16: TLabel;
    LV17: TLabel;
    LV18: TLabel;
    LV19: TLabel;
    LV20: TLabel;
    Lab_InvHex: TLabel;
    Lab_Hdr: TLabel;
    Lab_Foot: TLabel;

    procedure FormCreate(Sender: TObject);
      {-One-time initialization}
    procedure FormShow(Sender: TObject);
      {-(Re)calculate and display CRCs if form is shown}
    procedure Edit1Change(Sender: TObject);
      {-(Re)calculate and display CRCs if input changed}
    procedure CB_FormatChange(Sender: TObject);
      {-(Re)calculate and display CRCs if input format changed}
  private
    { Private declarations }
    buf  : array[0..255] of byte;
    blen : word;
  public
    { Public declarations }
    procedure RecalcAll;
      {-Calculate and display CRCs for all crcm_cat 16 bit algorithms}
    procedure CheckAndCalc;
      {-Check input, display warning if inv. Hex, calculate and display all CRCs}
  end;

var
  Form1: TForm1;

implementation

uses
  mem_util, crcmodel, crcm_cat;

{$R *.DFM}


{---------------------------------------------------------------------------}
procedure Recalc1(const para: TCRCParam; const TN, TV: TLabel);
  {-Calculate of blen bytes of buf with CRC defined by parameter set para}
  { Display para.name on at label TN and CRC at label TV}
var
  CRC: longint;
  ctx: TCRC_ctx;
begin
  cm_Create(para,nil,ctx);
  cm_Full(ctx,CRC,@Form1.buf,Form1.blen);
  TN.Caption := para.name;
  TV.Caption := '$'+IntToHex(CRC,4);
end;


{---------------------------------------------------------------------------}
procedure TForm1.RecalcAll;
  {-Calculate and display CRCs for all crcm_cat 16 bit algorithms}
begin
  Recalc1(      CRC16_ARC , LN1  , LV1  );
  Recalc1(     CRC16_ATOM , LN2  , LV2  );
  Recalc1(CRC16_AUG2_CITT , LN3  , LV3  );
  Recalc1( CRC16_AUG_CITT , LN4  , LV4  );
  Recalc1(  CRC16_BT_CHIP , LN5  , LV5  );
  Recalc1(  CRC16_BUYPASS , LN6  , LV6  );
  Recalc1(     CRC16_CITT , LN7  , LV7  );
  Recalc1(      CRC16_DNP , LN8  , LV8  );
  Recalc1(    CRC16_ICODE , LN9  , LV9  );
  Recalc1(  CRC16_MCRF4XX , LN10 , LV10 );
  Recalc1(      CRC16_USB , LN11 , LV11 );
  Recalc1(   CRC16_KERMIT , LN12 , LV12 );
  Recalc1(   CRC16_MODBUS , LN13 , LV13 );
  Recalc1(        CRC16_R , LN14 , LV14 );
  Recalc1(      CRC16_X25 , LN15 , LV15 );
  Recalc1(  CRC16_XKERMIT , LN16 , LV16 );
  Recalc1(   CRC16_ZMODEM , LN17 , LV17 );
end;

{---------------------------------------------------------------------------}
procedure TForm1.CheckAndCalc;
  {-Check input, display warning if inv. Hex, calculate and display all CRCs}
var
  s: ansistring;
  i: integer;
  HOK: boolean;
begin
  s := Edit1.Text;
  if length(s)>sizeof(buf) then SetLength(s,sizeof(buf));
  blen := length(s);
  if CB_Format.Itemindex=1 then begin
    {string input, copy char to buf bytes}
    for i:=1 to blen do buf[i-1] := byte(s[i]);
  end
  else begin
    {Hex input, first check for invalid chars}
    HOK := true;
    for i:=1 to blen do begin
      if pos(upcase(s[i]),'0123456789ABCDEF')=0 then begin
        HOK := false;
        break;
      end;
    end;
    if HOK then begin
      Lab_InvHex.Visible := false;
      Lab_InvHex.Caption := '';
    end
    else begin
      Lab_InvHex.Visible := true;
      Lab_InvHex.Caption := 'Invalid HEX char(s)';
    end;
    {Convert hex string to memory at buf, stops at first invalid}
    Hex2Mem(s, @buf, sizeof(buf),blen);
  end;
  {Calculate and display all CRCs of buf}
  RecalcAll;
end;

{---------------------------------------------------------------------------}
procedure TForm1.FormCreate(Sender: TObject);
  {-One-time initialization}
begin
  CB_Format.Itemindex := 1;
  Lab_InvHex.Caption  := '';
end;

{---------------------------------------------------------------------------}
procedure TForm1.FormShow(Sender: TObject);
  {-(Re)calculate and display CRCs if form is shown}
begin
  RecalcAll;
end;

{---------------------------------------------------------------------------}
procedure TForm1.Edit1Change(Sender: TObject);
  {-(Re)calculate and display CRCs if input changed}
begin
  if Edit1.Modified then CheckAndCalc;
end;

{---------------------------------------------------------------------------}
procedure TForm1.CB_FormatChange(Sender: TObject);
  {-(Re)calculate and display CRCs if input format changed}
begin
  CheckAndCalc;
end;

end.
