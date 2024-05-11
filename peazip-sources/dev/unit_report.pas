unit Unit_report;
{
 DESCRIPTION     :  Unit providing GUI for display reports in two string grids
                    and four label

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060908  G.Tani      Initial version
 0.11     20060920  G.Tani      removed *_VER; P_RELEASE constant in pea_utils
                                is used to keep track of release level;
                                for porting the application please refer to notes
                                in unit Peach.
 0.12     20060927  G.Tani      changed Win32 transparence code to be compatible
                                with all Win32 versions (no longer needed separate
                                builds);
 0.12b    20070328  G.Tani      Minor visual updates for better integration with
                                PeaZip 1.6 look and feel
 0.13     20070503  G.Tani      Updated look and feel
 0.14     20070802  G.Tani      Accepts new PeaZip theming
 0.15     20070924  G.Tani      Updated according to PeaZip theming improvements
 0.16     20071130  G.Tani      Minor cleanup
 0.17     20080314  G.Tani      Transparency made available for Win64
 0.18     20080707  G.Tani      Updated to work with utf8 LCL
 0.19     20080826  G.Tani      Ask path for saving reports, default is desktop (or current path if desktop is not found)
 0.20     20081026  G.Tani      Autosized/customisable GUI's items height; various graphic updates
                                Form_report that can now close the application if it is the only form needing to be shown
 0.21     20081118  G.Tani      appdata fixed for Windows users with names containing extended characters
                                filemode set to 0 before all reset file operations to avoid possible lock situations (i.e. concurrent instances)
 0.22     20091103  G.Tani      New icons
 0.23     20101105  G.Tani      Updated look and feel
 0.24     20200414  G.Tani      New function to save crc/hash value(s) to file
 0.25     20210502  G.Tani      Batch and hidden *_report modes now save report to output path without requiring user interaction
 0.26     20231206  G.Tani      Updated theming

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com
The program is released under GNU LGPL http://www.gnu.org/licenses/lgpl.txt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  Grids, StdCtrls, ExtCtrls, ComCtrls,
  ansiutf8_utils, list_utils, pea_utils, img_utils, Menus;

type

  { TForm_report }

  TForm_report = class(TForm)
    Button2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelCase: TLabel;
    LabelTitleREP1: TLabel;
    LabelSave: TLabel;
    LabelSaveTxt: TLabel;
    LabelSave2: TLabel;
    LabelSaveTxt1: TLabel;
    LabelTitleREP2: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Notebook1: TPageControl;
    InputT: TTabSheet;
    OutputT: TTabSheet;
    Panelsp0: TPanel;
    PanelTitleREPTabAlign: TPanel;
    PanelTitleREP: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Shapelinkrep1: TShape;
    Shapelinkrep2: TShape;
    ShapeTitleREPb1: TShape;
    ShapeTitleREPb2: TShape;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelCaseClick(Sender: TObject);
    procedure LabelSaveTxt1Click(Sender: TObject);
    procedure LabelSaveTxtClick(Sender: TObject);
    procedure LabelTitleREP1Click(Sender: TObject);
    procedure LabelTitleREP1MouseEnter(Sender: TObject);
    procedure LabelTitleREP1MouseLeave(Sender: TObject);
    procedure LabelTitleREP2Click(Sender: TObject);
    procedure LabelTitleREP2MouseEnter(Sender: TObject);
    procedure LabelTitleREP2MouseLeave(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure StringGrid1HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid2HeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure save_report(s,reptype,modparam,out_path:ansistring);
procedure clicklabel_rep(var a: TLabel; var b:TShape);
  
var
  Form_report: TForm_report;
   t:text;
   //theming
   conf:text;
   opacity,grid1index,grid2index,alttabstyle,highlighttabs:integer;
   confpath,csvsep:ansistring;
   grid1switch,grid2switch:boolean;
   executable_path,dummy,color1,color2,color3,color4,color5:string;
   Binfo,Bloadlayout:TBitmap;
   activelabel_rep:TLabel;
   tablowcol,tabpencol,tabbrushcol,tabbrushhighcol:TColor;
   
implementation

///rep
procedure exitlabel_rep(var a: TLabel; var b:TShape);
begin
if activelabel_rep=a then exit;
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   b.Brush.Color:=tabbrushcol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end
else
   begin
   b.Brush.Color:=tabpencol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end;
if (highlighttabs=1) or (highlighttabs=4) or (highlighttabs=5) then a.Font.Color:=clDefault else a.Font.Color:=pGray;
end;

procedure deselectlabels_rep;
begin
with Form_report do
begin
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   exitlabel_rep(LabelTitleREP1,ShapeTitleREPb1);
   exitlabel_rep(LabelTitleREP2,ShapeTitleREPb2);
   end
else
   begin
   exitlabel_rep(LabelTitleREP1,ShapeLinkREP1);
   exitlabel_rep(LabelTitleREP2,ShapeLinkREP2);
   end;
end;
end;

procedure save_report_clip;
var
   x,y:dword;
begin
Form_report.Memo1.lines.BeginUpdate;;
Form_report.Memo1.Clear;
if Form_report.Caption<>'Hex preview' then
begin
for x:=1 to Form_report.StringGrid1.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid1.ColCount-1 do
      if Form_report.StringGrid1.ColWidths[y]>0 then
      if ((Form_report.StringGrid1.Cells[y,0]<>'File header') and (Form_report.StringGrid1.Cells[y,0]<>'End of file')) then
      if Form_report.StringGrid1.Cells[y,x]<>'' then
         Form_report.Memo1.Append(Form_report.StringGrid1.Cells[y,0]+': '+Form_report.StringGrid1.Cells[y,x])
      else
         Form_report.Memo1.Append(Form_report.StringGrid1.Cells[y,0]+': -');
   Form_report.Memo1.Append('');
   end;
if Form_report.StringGrid2.Cells[0,0]<>'' then
for x:=1 to Form_report.StringGrid2.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid2.ColCount-1 do
      if Form_report.StringGrid2.ColWidths[y]>0 then
      if Form_report.StringGrid2.Cells[y,x]<>'' then
         Form_report.Memo1.Append(Form_report.StringGrid2.Cells[y,0]+': '+Form_report.StringGrid2.Cells[y,x])
      else
         Form_report.Memo1.Append(Form_report.StringGrid2.Cells[y,0]+': - ');
   Form_report.Memo1.Append('');
   end;
end;
Form_report.Memo1.Append(Form_report.Label1.Caption);
Form_report.Memo1.Append(Form_report.Label2.Caption);
Form_report.Memo1.Append(Form_report.Label3.Caption);
Form_report.Memo1.Append(Form_report.Label4.Caption);
Form_report.Memo1.lines.EndUpdate;
Form_report.Memo1.SelStart:=0;
Form_report.Memo1.SelLength:=0;
end;

procedure setpanel_rep(i:integer);
begin
case i of
   1: begin
   Form_report.Notebook1.visible:=true;
   Form_report.Memo1.visible:=false;
   end;
   2: begin
   Form_report.Notebook1.visible:=false;
   Form_report.Memo1.visible:=true;
   save_report_clip;
   end;
end;
end;

procedure setlabelpanel_rep(var a: Tlabel);
begin
with Form_report do
begin
if a = LabelTitleREP1 then setpanel_rep(1);
if a = LabelTitleREP2 then setpanel_rep(2);
end;
end;

procedure clicklabel_rep(var a: TLabel; var b:TShape);
begin
activelabel_rep:=a;
deselectlabels_rep;
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   b.Brush.Color:=StringToColor(color2);
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end
else
   begin
   b.Brush.Color:=tablowcol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end;
if ((alttabstyle=1) or (alttabstyle=4)) and ((highlighttabs=1) or (highlighttabs=4) or (highlighttabs=5)) then a.Font.Color:=ptextaccent else a.Font.Color:=clDefault;
setlabelpanel_rep(a);
end;

procedure enterlabel_rep(var a: TLabel; var b:TShape);
begin
if activelabel_rep=a then exit;
b.Brush.Color:=tabbrushhighcol;
b.Pen.Color:=tabpencol;
b.Pen.Style:=psSolid;
a.Font.Color:=clDefault;
end;

///

function wingetdesk(var dp:ansistring):integer;
{$IFDEF MSWINDOWS}
var
  pidl: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
wingetdesk:=-1;
{$IFDEF MSWINDOWS}
try
   if Succeeded(ShGetSpecialFolderLocation(Form_report.Handle,0,pidl)) then //0 is CSIDL_DESKTOP numerical value
      if ShGetPathfromIDList(pidl, Buf ) then
         begin
         dp:=(Buf);
         CoTaskMemFree(pidl);
         wingetdesk:=0;
         end
      else CoTaskMemFree(pidl);
except
end;
{$ENDIF}
end;

procedure save_report(s,reptype,modparam,out_path:ansistring);
var
x,y:dword;
field_delim:string;
p:ansistring;
begin
if reptype='txt' then field_delim:=chr($09)
else field_delim:=csvsep;

if upcase(modparam)='INTERACTIVE_REPORT' then //interactive
   begin
   {$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
   if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
   s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+s+'.'+reptype;
   Form_report.SaveDialog1.FileName:=p+s;
   if directoryexists(p) then Form_report.SaveDialog1.InitialDir:=p;
   if Form_report.SaveDialog1.Execute then s:=Form_report.SaveDialog1.FileName
   else s:='';
   end
else //batch or hidden, non interactive
   begin
   p:=out_path;
   if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
   s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+s+'.'+reptype;
   end;

if s<>'' then
begin
assignfile(t,s);
rewrite(t);
write_header(t);
if Form_report.Caption<>'Hex preview' then
begin
for x:=0 to Form_report.StringGrid1.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid1.ColCount-1 do
      if Form_report.StringGrid1.ColWidths[y]>0 then
      if ((Form_report.StringGrid1.Cells[y,0]<>'File header') and (Form_report.StringGrid1.Cells[y,0]<>'End of file')) then
      write(t,Form_report.StringGrid1.Cells[y,x]+field_delim);
   writeln(t);
   end;
for x:=0 to Form_report.StringGrid2.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid2.ColCount-1 do
      if Form_report.StringGrid2.ColWidths[y]>0 then
      write(t,Form_report.StringGrid2.Cells[y,x]+field_delim);
   writeln(t);
   end;
end;
writeln(t,Form_report.Label1.Caption);
writeln(t,Form_report.Label2.Caption);
writeln(t,Form_report.Label3.Caption);
writeln(t,Form_report.Label4.Caption);
closefile(t);
end;
end;

procedure save_hashfn;
var
x,y:dword;
fname,p:ansistring;
begin
if Form_report.StringGrid1.Cells[0,Form_report.StringGrid1.Row]='* Digest *' then exit;
fname:=Form_report.StringGrid1.Cells[Form_report.StringGrid1.Col,0]+'.txt';
assignfile(t,fname);
rewrite(t);
//write_header(t);
for x:=1 to Form_report.StringGrid1.RowCount-1 do
   begin
   if Form_report.StringGrid1.Cells[0,x]='* Digest *' then break;
   write(t,Form_report.StringGrid1.Cells[Form_report.StringGrid1.Col,x]+'  '+Form_report.StringGrid1.Cells[1,x]+char($0A));
   end;
closefile(t);
end;

{ TForm_report }

procedure conditional_stop;
begin
if Form_report.Caption='List' then Application.Terminate;
if Form_report.Caption='Info' then Application.Terminate;
if Form_report.Caption='Compare' then Application.Terminate;
if Form_report.Caption='Checksum and hash' then Application.Terminate;
if Form_report.Caption='Analyze' then Application.Terminate;
if Form_report.Caption='Environment variables' then Application.Terminate;
if Form_report.Caption='Hex preview' then Application.Terminate;
end;

procedure TForm_report.Button2Click(Sender: TObject);
begin
Form_report.Visible:=false;
conditional_stop;
end;

procedure TForm_report.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
conditional_stop;
end;

procedure TForm_report.FormCreate(Sender: TObject);
begin
grid1index:=0;
grid2index:=0;
grid1switch:=true;
grid2switch:=true;
end;

procedure TForm_report.FormShow(Sender: TObject);
begin
Form_report.PanelTitleREPTabAlign.Width:=Form_report.ShapeTitleREPb1.Width+
Form_report.ShapeTitleREPb2.Width+
Form_report.LabelTitleREP1.BorderSpacing.Left+Form_report.LabelTitleREP1.BorderSpacing.Left+
Form_report.LabelTitleREP2.BorderSpacing.Left;
if alttabstyle<=2 then
   Form_report.PanelTitleREPTabAlign.AnchorSideLeft.Side:=asrleft
else
   Form_report.PanelTitleREPTabAlign.AnchorSideLeft.Side:=asrCenter;
if (alttabstyle=1) or (alttabstyle=4) then
   begin
   Form_report.LabelTitleREP1.AnchorSideTop.Control:=Form_report.PanelTitleREP;
   Form_report.ShapeTitleREPb1.visible:=false;
   Form_report.ShapeLinkREP1.visible:=true;
   Form_report.LabelTitleREP2.AnchorSideTop.Control:=Form_report.PanelTitleREP;
   Form_report.ShapeTitleREPb2.visible:=false;
   Form_report.ShapeLinkREP2.visible:=true;
   end
else
   begin
   Form_report.LabelTitleREP1.AnchorSideTop.Control:=Form_report.ShapeTitleREPb1;
   Form_report.ShapeTitleREPb1.visible:=true;
   Form_report.ShapeLinkREP1.visible:=false;
   Form_report.LabelTitleREP2.AnchorSideTop.Control:=Form_report.ShapeTitleREPb2;
   Form_report.ShapeTitleREPb2.visible:=true;
   Form_report.ShapeLinkREP2.visible:=false;
   end;

   case alttabstyle of
      0,3:
      begin
      Form_report.ShapeTitleREPb1.Shape:=stRoundRect;
      Form_report.ShapeTitleREPb2.Shape:=stRoundRect;
      Form_report.LabelTitleREP1.BorderSpacing.Left:=6;
      Form_report.LabelTitleREP2.BorderSpacing.Left:=6;
      end
      else
      begin
      Form_report.ShapeTitleREPb1.Shape:=stRectangle;
      Form_report.ShapeTitleREPb2.Shape:=stRectangle;
      Form_report.LabelTitleREP1.BorderSpacing.Left:=0;
      Form_report.LabelTitleREP2.BorderSpacing.Left:=0;
      end;
      end;

end;

procedure TForm_report.LabelCaseClick(Sender: TObject);
var
   irow,icol:integer;
   orig_activelabel_rep:TLabel;
begin
orig_activelabel_rep:=activelabel_rep;
if LabelCase.Caption='[CASE]' then
   begin
   LabelCase.Caption:='[case]';
   if Form_report.StringGrid1.RowCount<2 then exit;
   if Form_report.StringGrid1.ColCount<24 then exit;
   for irow:=1 to Form_report.StringGrid1.RowCount-1 do
      for icol:=7 to 24 do Form_report.StringGrid1.Cells[icol,irow]:=upcase(Form_report.StringGrid1.Cells[icol,irow]);
   end
else
   begin
   LabelCase.Caption:='[CASE]';
   if Form_report.StringGrid1.RowCount<2 then exit;
   if Form_report.StringGrid1.ColCount<24 then exit;
   for irow:=1 to Form_report.StringGrid1.RowCount-1 do
      for icol:=7 to 24 do Form_report.StringGrid1.Cells[icol,irow]:=lowercase(Form_report.StringGrid1.Cells[icol,irow]);
   end;
clicklabel_rep(LabelTitleREP2,ShapeTitleREPb2);
if orig_activelabel_rep=LabelTitleREP1 then clicklabel_rep(LabelTitleREP1,ShapeTitleREPb1);
end;

procedure TForm_report.LabelSaveTxt1Click(Sender: TObject);
begin
save_report(Form_report.Caption,'csv','INTERACTIVE_REPORT','');
end;

procedure TForm_report.LabelSaveTxtClick(Sender: TObject);
begin
save_report(Form_report.Caption,'txt','INTERACTIVE_REPORT','');
end;

procedure TForm_report.LabelTitleREP1Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_rep(LabelTitleREP1,ShapeTitleREPb1) else clicklabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TForm_report.LabelTitleREP1MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_rep(LabelTitleREP1,ShapeTitleREPb1) else enterlabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TForm_report.LabelTitleREP1MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_rep(LabelTitleREP1,ShapeTitleREPb1) else exitlabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TForm_report.LabelTitleREP2Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_rep(LabelTitleREP2,ShapeTitleREPb2) else clicklabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TForm_report.LabelTitleREP2MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_rep(LabelTitleREP2,ShapeTitleREPb2) else enterlabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TForm_report.LabelTitleREP2MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_rep(LabelTitleREP2,ShapeTitleREPb2) else exitlabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TForm_report.MenuItem1Click(Sender: TObject);
var
   s,fname:AnsiString;
begin
if StringGrid1.Row>0 then
   if (StringGrid1.Col>7) and (StringGrid1.Col<25) then
      begin
      s:=StringGrid1.Cells[StringGrid1.Col,StringGrid1.Row];
      if StringGrid1.Cells[0,StringGrid1.Row]='* Digest *' then exit;
      fname:=StringGrid1.Cells[0,StringGrid1.Row]+'.'+StringGrid1.Cells[StringGrid1.Col,0]+'.txt';
      assignfile(t,fname);
      rewrite(t);
      write(t,s);
      closefile(t);
      end;
end;

procedure TForm_report.MenuItem2Click(Sender: TObject);
var
   s,fname:AnsiString;
   y:integer;
begin
if StringGrid1.Row>0 then
   begin
   if StringGrid1.Cells[0,StringGrid1.Row]='* Digest *' then exit;
   fname:=StringGrid1.Cells[0,StringGrid1.Row]+'.info.txt';
   assignfile(t,fname);
   rewrite(t);
   write_header(t);
   writeln(t,'Name: '+StringGrid1.Cells[1,StringGrid1.Row]);
   writeln(t,'Size: '+StringGrid1.Cells[3,StringGrid1.Row]+' ('+StringGrid1.Cells[4,StringGrid1.Row]+' Bytes)');
   writeln(t,'Modified: '+StringGrid1.Cells[5,StringGrid1.Row]);
   writeln(t,'Attributes: '+StringGrid1.Cells[6,StringGrid1.Row]);
   for y:=8 to 24 do
      if StringGrid1.ColWidths[y]>0 then
         writeln(t,StringGrid1.Cells[y,0]+': '+StringGrid1.Cells[y,StringGrid1.Row]);
   closefile(t);
   end;
end;

procedure TForm_report.MenuItem3Click(Sender: TObject);
begin
save_hashfn;
end;

procedure TForm_report.StringGrid1HeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var i:integer;
begin
if grid1index=index then grid1switch:=not(grid1switch);
if grid1switch=true then StringGrid1.SortOrder:=soAscending else StringGrid1.SortOrder:=soDescending;
i:=index;
if (Form_report.Caption='Checksum and hash') and ((i=3) or (i=4)) then i:=25;
if (Form_report.Caption='Checksum and hash') and (i=29) then i:=30;
StringGrid1.SortColRow(true,i);
grid1index:=Index;
end;

procedure crcmenuenable(en:boolean);
begin
Form_report.MenuItem3.Enabled:=en;
Form_report.MenuItem1.Enabled:=en;
Form_report.MenuItem2.Enabled:=en;
end;

procedure TForm_report.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var col,row:integer;
begin
StringGrid1.MouseToCell(X,Y,col,row);
StringGrid1.Col:=col;
if (StringGrid1.Col>7) and (StringGrid1.Col<25) then
   begin
   crcmenuenable(true);
   MenuItem1.Caption:='Save '+StringGrid1.Cells[StringGrid1.Col,0]+' value of this file';
   MenuItem3.Caption:='Save '+StringGrid1.Cells[StringGrid1.Col,0]+' values and file names';
   end
else
   begin
   crcmenuenable(true);
   MenuItem1.Caption:='Save selected CRC or hash value of this file';
   MenuItem3.Caption:='Save selected CRC or hash values and file names';
   MenuItem1.Enabled:=false;
   MenuItem3.Enabled:=false;
   end;
if StringGrid1.Cells[0,StringGrid1.Row]='* Digest *' then crcmenuenable(false);
end;

procedure TForm_report.StringGrid2HeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var i:integer;
begin
if grid2index=index then grid2switch:=not(grid2switch);
if grid2switch=true then StringGrid2.SortOrder:=soAscending else StringGrid2.SortOrder:=soDescending;
i:=index;
StringGrid2.SortColRow(true,i);
grid2index:=Index;
end;

function wingetappdata(var s:ansistring):integer;
{$IFDEF MSWINDOWS}
var
  pidl: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
wingetappdata:=-1;
{$IFDEF MSWINDOWS}
try
   if Succeeded(ShGetSpecialFolderLocation(Form_report.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
      if ShGetPathfromIDList(pidl, Buf ) then
         begin
         s:=(Buf)+'\PeaZip\';
         CoTaskMemFree(pidl);
         wingetappdata:=0;
         end
      else CoTaskMemFree(pidl);
except
end;
{$ENDIF}
end;

initialization
  {$I unit_report.lrs}

end.

