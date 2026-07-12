unit UnitReport; //Form for reporting results of completed task, with options to save logs
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
                                FormReport that can now close the application if it is the only form needing to be shown
 0.21     20081118  G.Tani      appdata fixed for Windows users with names containing extended characters
                                filemode set to 0 before all reset file operations to avoid possible lock situations (i.e. concurrent instances)
 0.22     20091103  G.Tani      New icons
 0.23     20101105  G.Tani      Updated look and feel
 0.24     20200414  G.Tani      New function to save crc/hash value(s) to file
 0.25     20210502  G.Tani      Batch and hidden *_report modes now save report to output path without requiring user interaction
 0.26     20231216  G.Tani      Updated theming
 0.27     20251014  G.Tani      New text preview
 0.28     20251224  G.Tani      Improved text preview, options are now persistent
 0.29     20260521  G.Tani      Rationalized use of text

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
  Grids, StdCtrls, ExtCtrls, ComCtrls, LazUTF8,
  list_utils, pea_utils, img_utils, Menus, ActnList;

type

  { TFormReport }

  TFormReport = class(TForm)
    peabold: TAction;
    FontSet: TMenuItem;
    FontBold: TMenuItem;
    Separator6: TMenuItem;
    peamonotypefont: TAction;
    peacasesearch: TAction;
    FontDialogMemo: TFontDialog;
    FontMonotype: TMenuItem;
    peawordwrap: TAction;
    fontzoomminusn: TAction;
    fontzoomminus: TAction;
    fonzoomplusn: TAction;
    fonzoomplus: TAction;
    fontzoomin: TAction;
    fontzoomreset: TAction;
    fontzoomout: TAction;
    FZoomIn: TMenuItem;
    FZoomReset: TMenuItem;
    FZoomOut: TMenuItem;
    MenuItemOpenPath: TMenuItem;
    Separator4: TMenuItem;
    peafind: TAction;
    encutf7: TMenuItem;
    peafindprev: TAction;
    Separator3: TMenuItem;
    enccasesensitive: TMenuItem;
    peafindnext: TAction;
    ActionListFind: TActionList;
    ButtonFindText: TButton;
    ButtonClose: TBitBtn;
    EditFindText: TEdit;
    LabelEncoding: TLabel;
    LabelReport1: TLabel;
    LabelReport2: TLabel;
    LabelReport3: TLabel;
    LabelReport4: TLabel;
    LabelCase: TLabel;
    LabelSave1: TLabel;
    LabelSave3: TLabel;
    LabelSaveCsv1: TLabel;
    LabelSaveTxt: TLabel;
    LabelTitleREP1: TLabel;
    LabelSave: TLabel;
    LabelSaveTsv: TLabel;
    LabelSave2: TLabel;
    LabelSaveCsv: TLabel;
    LabelTitleREP2: TLabel;
    MemoReport: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    encansi: TMenuItem;
    encascii: TMenuItem;
    encutf8: TMenuItem;
    encunicodele: TMenuItem;
    encunicodebe: TMenuItem;
    encwwrap: TMenuItem;
    encdefault: TMenuItem;
    PopupMenuEncoding: TPopupMenu;
    Separator1: TMenuItem;
    NotebookReport: TPageControl;
    InputT: TTabSheet;
    OutputT: TTabSheet;
    PanelSpReport: TPanel;
    PanelTitleREPTabAlign: TPanel;
    PanelTitleREP: TPanel;
    PopupMenu1: TPopupMenu;
    SaveDialogReport: TSaveDialog;
    Separator2: TMenuItem;
    Separator5: TMenuItem;
    Shapelinkrep1: TShape;
    Shapelinkrep2: TShape;
    ShapeTitleREPb1: TShape;
    ShapeTitleREPb2: TShape;
    StringGridInput: TStringGrid;
    StringGridOutput: TStringGrid;
    procedure FontBoldClick(Sender: TObject);
    procedure FontMonotypeClick(Sender: TObject);
    procedure fontzoomminusExecute(Sender: TObject);
    procedure fontzoomminusnExecute(Sender: TObject);
    procedure fonzoomplusExecute(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonFindTextClick(Sender: TObject);
    procedure EditFindTextChange(Sender: TObject);
    procedure EditFindTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure encansiClick(Sender: TObject);
    procedure encasciiClick(Sender: TObject);
    procedure enccasesensitiveClick(Sender: TObject);
    procedure encdefaultClick(Sender: TObject);
    procedure encunicodebeClick(Sender: TObject);
    procedure encunicodeleClick(Sender: TObject);
    procedure encutf7Click(Sender: TObject);
    procedure encutf8Click(Sender: TObject);
    procedure encwwrapClick(Sender: TObject);
    procedure fontzoominExecute(Sender: TObject);
    procedure fontzoomoutExecute(Sender: TObject);
    procedure fontzoomresetExecute(Sender: TObject);
    procedure fonzoomplusnExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelCaseClick(Sender: TObject);
    procedure LabelEncodingClick(Sender: TObject);
    procedure LabelSaveCsv1Click(Sender: TObject);
    procedure LabelSaveCsvClick(Sender: TObject);
    procedure LabelSaveTsvClick(Sender: TObject);
    procedure LabelSaveTxtClick(Sender: TObject);
    procedure LabelTitleREP1Click(Sender: TObject);
    procedure LabelTitleREP1MouseEnter(Sender: TObject);
    procedure LabelTitleREP1MouseLeave(Sender: TObject);
    procedure LabelTitleREP2Click(Sender: TObject);
    procedure LabelTitleREP2MouseEnter(Sender: TObject);
    procedure LabelTitleREP2MouseLeave(Sender: TObject);
    procedure MemoReportClick(Sender: TObject);
    procedure MemoReportKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure FZoomInClick(Sender: TObject);
    procedure FZoomResetClick(Sender: TObject);
    procedure FZoomOutClick(Sender: TObject);
    procedure FontSetClick(Sender: TObject);
    procedure MenuItemOpenPathClick(Sender: TObject);
    procedure peaboldExecute(Sender: TObject);
    procedure peacasesearchExecute(Sender: TObject);
    procedure peafindExecute(Sender: TObject);
    procedure peafindnextExecute(Sender: TObject);
    procedure peafindprevExecute(Sender: TObject);
    procedure peamonotypefontExecute(Sender: TObject);
    procedure peawordwrapExecute(Sender: TObject);
    procedure StringGridInputHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGridInputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridOutputHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure save_report(s,reptype,repopt,modparam,out_path:ansistring);
procedure clicklabel_rep(var a: TLabel; var b:TShape);
procedure save_hashfn;
function peatextstats:integer;
procedure memowordwrap;
procedure setpeacasesensitivesearch;
procedure setpeamonotypefont;
procedure setpeabold;
procedure fontzoomset;
  
var
  FormReport: TFormReport;
   t:text;
   //theming
   conftextpreview:text;
   opacity,grid1index,grid2index,alttabstyle,highlighttabs,psearchpos,texstatsresult,textzoom:integer;
   desk_env:byte;
   confpath,intextfile,sizeastr,textwrap,textbold,textcase,textmonotype,textmfont:ansistring;
   grid1switch,grid2switch,needclose,noreportdetails,peacasesensitive,isutf8withbom,
   isutf16BEwithbom,isutf16LEwithbom,isutf7withbom:boolean;
   executable_path,dummy,color1,color2,color3,color4,color5:string;
   Binfo,Bloadlayout:TBitmap;
   activelabel_rep:TLabel;
   tablowcol,tabpencol,tabbrushcol,tabbrushhighcol:TColor;
   intextfileenc:TEncoding;
   peaprevpos:array of integer;
   //text
   txt_textpreview,txt_hexpreview,txt_fileheader,txt_eof,txt_digest,txt_list,
   txt_info,txt_compare,txt_ch,txt_analyze,txt_ev,txt_sys,txt_encerr,txt_saveto,
   txt_l,txt_w,txt_c,txt_lowcase,txt_upcase,txt_name,txt_size,txt_modified,
   txt_attributes,txt_save,txt_search,txt_sh,txt_thisfile,txt_values,
   txt_savefilehash,txt_savehashnames:ansistring;
   
implementation

///text
procedure set_report_text;
begin
txt_textpreview:='Text preview';
txt_hexpreview:='Hex preview';
txt_fileheader:='File header';
txt_eof:='End of file';
txt_digest:='* Digest *';
txt_list:='List';
txt_info:='Info';
txt_compare:='Compare';
txt_ch:='Checksum and hash';
txt_analyze:='Analyze';
txt_ev:='Environment variables';
txt_sys:='System default';
txt_encerr:='Try different encoding';
txt_saveto:='Save to';
txt_l:='Lines';
txt_w:='Words';
txt_c:='Chars';
txt_lowcase:='case';
txt_upcase:=UpCase(txt_lowcase);
txt_name:='Name';
txt_size:='Size';
txt_modified:='Modified';
txt_attributes:='Attributes';
txt_save:='Save';
txt_search:='Search';
txt_sh:='Search selected CRC/hash';
txt_thisfile:='of this file';
txt_values:='values and file names';
txt_savefilehash:='Save selected CRC/hash of this file';
txt_savehashnames:='Save selected CRC/hash values and file names';
end;

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
with FormReport do
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
FormReport.MemoReport.lines.BeginUpdate;;
FormReport.MemoReport.Clear;
FormReport.StringGridInput.BeginUpdate;
FormReport.StringGridOutput.BeginUpdate;
if noreportdetails=false then
begin
for x:=1 to FormReport.StringGridInput.RowCount-1 do
   begin
   for y:=0 to FormReport.StringGridInput.ColCount-1 do
      if FormReport.StringGridInput.ColWidths[y]>0 then
      if ((FormReport.StringGridInput.Cells[y,0]<>txt_fileheader) and (FormReport.StringGridInput.Cells[y,0]<>txt_eof)) then
      if FormReport.StringGridInput.Cells[y,x]<>'' then
         FormReport.MemoReport.Append(FormReport.StringGridInput.Cells[y,0]+': '+FormReport.StringGridInput.Cells[y,x])
      else
         FormReport.MemoReport.Append(FormReport.StringGridInput.Cells[y,0]+': -');
   FormReport.MemoReport.Append('');
   end;
Application.ProcessMessages;
if FormReport.StringGridOutput.Cells[0,0]<>'' then
for x:=1 to FormReport.StringGridOutput.RowCount-1 do
   begin
   for y:=0 to FormReport.StringGridOutput.ColCount-1 do
      if FormReport.StringGridOutput.ColWidths[y]>0 then
      if FormReport.StringGridOutput.Cells[y,x]<>'' then
         FormReport.MemoReport.Append(FormReport.StringGridOutput.Cells[y,0]+': '+FormReport.StringGridOutput.Cells[y,x])
      else
         FormReport.MemoReport.Append(FormReport.StringGridOutput.Cells[y,0]+': - ');
   FormReport.MemoReport.Append('');
   end;
end;
FormReport.MemoReport.Append(FormReport.LabelReport1.Caption);
FormReport.MemoReport.Append(FormReport.LabelReport2.Caption);
FormReport.MemoReport.Append(FormReport.LabelReport3.Caption);
FormReport.MemoReport.Append(FormReport.LabelReport4.Caption);
FormReport.MemoReport.lines.EndUpdate;
FormReport.StringGridInput.EndUpdate;
FormReport.StringGridOutput.EndUpdate;
FormReport.MemoReport.SelStart:=0;
FormReport.MemoReport.SelLength:=0;
end;

procedure setpanel_rep(i:integer);
begin
case i of
   1: begin
   FormReport.NotebookReport.visible:=true;
   FormReport.MemoReport.visible:=false;
   end;
   2: begin
   FormReport.NotebookReport.visible:=false;
   FormReport.MemoReport.visible:=true;
   save_report_clip;
   end;
end;
end;

procedure setlabelpanel_rep(var a: Tlabel);
begin
with FormReport do
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
   if Succeeded(ShGetSpecialFolderLocation(FormReport.Handle,0,pidl)) then //0 is CSIDL_DESKTOP numerical value
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

procedure save_report(s,reptype,repopt,modparam,out_path:ansistring);
var
x,y:dword;
field_delim:string;
p:ansistring;
begin
if (reptype='txt') or (reptype='tsv') then field_delim:=chr($09)
else field_delim:=repopt;

if upcase(modparam)='INTERACTIVE_REPORT' then //interactive
   begin
   {$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
   if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
   s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+s+'.'+reptype;
   FormReport.SaveDialogReport.FileName:=s;
   FormReport.SaveDialogReport.InitialDir:=p;
   if directoryexists(p) then FormReport.SaveDialogReport.InitialDir:=p;
   if FormReport.SaveDialogReport.Execute then s:=FormReport.SaveDialogReport.FileName
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
if FormReport.LabelReport1.Caption=txt_textpreview then
   begin
   FormReport.MemoReport.Lines.SaveToFile(s,intextfileenc);
   exit;
   end;
assignfile(t,s);
rewrite(t);
write_header(t);
if FormReport.Caption<>txt_hexpreview then
begin
for x:=0 to FormReport.StringGridInput.RowCount-1 do
   begin
   for y:=0 to FormReport.StringGridInput.ColCount-1 do
      if FormReport.StringGridInput.ColWidths[y]>0 then
      if ((FormReport.StringGridInput.Cells[y,0]<>txt_fileheader) and (FormReport.StringGridInput.Cells[y,0]<>txt_eof)) then
      write(t,FormReport.StringGridInput.Cells[y,x]+field_delim);
   writeln(t);
   end;
for x:=0 to FormReport.StringGridOutput.RowCount-1 do
   begin
   for y:=0 to FormReport.StringGridOutput.ColCount-1 do
      if FormReport.StringGridOutput.ColWidths[y]>0 then
      write(t,FormReport.StringGridOutput.Cells[y,x]+field_delim);
   writeln(t);
   end;
end;
writeln(t,FormReport.LabelReport1.Caption);
writeln(t,FormReport.LabelReport2.Caption);
writeln(t,FormReport.LabelReport3.Caption);
writeln(t,FormReport.LabelReport4.Caption);
closefile(t);
end;
end;

procedure save_hashfn;
var
x,hcol:dword;
fname,p:ansistring;
begin
if FormReport.StringGridInput.Cells[0,FormReport.StringGridInput.Row]=txt_digest then exit;
hcol:=FormReport.StringGridInput.Col;
fname:=FormReport.StringGridInput.Cells[hcol,0]+'.txt';
if pathistmp(GetCurrentDir)=true then //switch to desktop if file is in PeaZip's temp path (i.e. using the function on a preview of archived item)
   begin
   {$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
   if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
   fname:=p+extractfilename(fname);
   end;
assignfile(t,fname);
rewrite(t);
//write_header(t);
for x:=1 to FormReport.StringGridInput.RowCount-1 do
   begin
   if FormReport.StringGridInput.Cells[0,x]=txt_digest then break;
   if FormReport.StringGridInput.Cells[hcol,x]<>'' then
      if (hcol>8) and (hcol<13) then
         write(t,FormReport.StringGridInput.Cells[hcol,x]+' '+FormReport.StringGridInput.Cells[4,x]+' '+FormReport.StringGridInput.Cells[1,x]+char($0A))
      else
         write(t,FormReport.StringGridInput.Cells[hcol,x]+'  '+FormReport.StringGridInput.Cells[1,x]+char($0A));
   end;
closefile(t);
end;

{ TFormReport }

procedure saveconf;
begin
try
assignfile(conftextpreview,(confpath+'textpreview.txt'));
rewrite(conftextpreview);
writeln(conftextpreview,'['+txt_textpreview+']');
writeln(conftextpreview,textwrap);
writeln(conftextpreview,textcase);
writeln(conftextpreview,textbold);
writeln(conftextpreview,textmonotype);
writeln(conftextpreview,textmfont);
writeln(conftextpreview,inttostr(textzoom));
CloseFile(conftextpreview);
except
CloseFile(conftextpreview);
end;
end;

procedure conditional_stop;
begin
if FormReport.Caption=txt_list then Application.Terminate;
if FormReport.Caption=txt_info then Application.Terminate;
if FormReport.Caption=txt_compare then Application.Terminate;
if FormReport.Caption=txt_ch then Application.Terminate;
if FormReport.Caption=txt_analyze then Application.Terminate;
if FormReport.Caption=txt_ev then Application.Terminate;
if FormReport.LabelReport1.Caption=txt_hexpreview then Application.Terminate;
if FormReport.LabelReport1.Caption=txt_textpreview then Application.Terminate;
if FormReport.Caption='MoTW' then Application.Terminate;
if needclose=true then
   begin
   saveconf;
   Application.Terminate;
   end;
end;

procedure TFormReport.ButtonCloseClick(Sender: TObject);
begin
FormReport.Visible:=false;
conditional_stop;
end;

procedure peafindtext;
var
   i:integer;
begin
if FormReport.EditFindText.Text='' then exit;
if peacasesensitive=true then i:=utf8Pos(FormReport.EditFindText.Text,FormReport.MemoReport.Text,psearchpos)
else i:=utf8Pos(upcase(FormReport.EditFindText.Text),upcase(FormReport.MemoReport.Text),psearchpos);
if i>0 then
   begin
   FormReport.MemoReport.SelStart:=i-1;
   FormReport.MemoReport.SelLength:=UTF8Length(FormReport.EditFindText.Text);
   psearchpos:=FormReport.MemoReport.SelStart+FormReport.MemoReport.SelLength+1;
   try
      FormReport.MemoReport.SetFocus;
   except
   end;
   end
else
   begin //retry once from text start (in case search miss is due to reaching the end on text)
   if peacasesensitive=true then i:=utf8Pos(FormReport.EditFindText.Text,FormReport.MemoReport.Text,1)
   else i:=utf8Pos(upcase(FormReport.EditFindText.Text),upcase(FormReport.MemoReport.Text),1);
   if i>0 then
      begin
      FormReport.MemoReport.SelStart:=i-1;
      FormReport.MemoReport.SelLength:=UTF8Length(FormReport.EditFindText.Text);
      psearchpos:=FormReport.MemoReport.SelStart+FormReport.MemoReport.SelLength+1;
      try
         FormReport.MemoReport.SetFocus;
      except
      end;
      end
   else
      begin
      FormReport.MemoReport.SelStart:=0;
      FormReport.MemoReport.SelLength:=0;
      psearchpos:=1;
      try
         FormReport.MemoReport.SetFocus;
      except
      end;
      end;
   end;
if i<>0 then
   begin
   if length(peaprevpos)>0 then
      if i=peaprevpos[high(peaprevpos)] then exit;
   SetLength(peaprevpos,length(peaprevpos)+1);
   peaprevpos[high(peaprevpos)]:=i;
   end;
peatextstats;
end;

procedure peafindprevtext;
var
   i:integer;
begin
if length(peaprevpos)<2 then
   begin
   FormReport.MemoReport.SelStart:=0;
   FormReport.MemoReport.SelLength:=0;
   try
      FormReport.MemoReport.SetFocus;
   except
   end;
   exit;
   end;
SetLength(peaprevpos,length(peaprevpos)-1);
i:=peaprevpos[high(peaprevpos)];
FormReport.MemoReport.SelStart:=i-1;
FormReport.MemoReport.SelLength:=UTF8Length(FormReport.EditFindText.Text);
try
   FormReport.MemoReport.SetFocus;
except
end;
psearchpos:=FormReport.MemoReport.SelStart+FormReport.MemoReport.SelLength;
end;

procedure TFormReport.ButtonFindTextClick(Sender: TObject);
begin
peafindtext;
end;

procedure TFormReport.EditFindTextChange(Sender: TObject);
begin
SetLength(peaprevpos,0);
end;

procedure TFormReport.EditFindTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=13 then peafindtext;
end;

function decodetextencoding:ansistring;
begin
if FormReport.encansi.Checked=true then result:='ANSI';
if FormReport.encascii.Checked=true then result:='ASCII';
if FormReport.encdefault.Checked=true then result:=txt_sys;
if FormReport.encunicodebe.Checked=true then result:='Unicode-BE';
if FormReport.encunicodele.Checked=true then result:='Unicode-LE';
if FormReport.encutf7.Checked=true then result:='UTF-7';
if FormReport.encutf8.Checked=true then result:='UTF-8';
if (result='UTF-8') and (isutf8withbom=true) then result:='UTF-8/BOM';
if (result='UTF-7') and (isutf7withbom=true) then result:='UTF-7/BOM';
if (result='Unicode-BE') and (isutf8withbom=true) then result:='Unicode-BE/BOM';
if (result='Unicode-LE') and (isutf8withbom=true) then result:='Unicode-LE/BOM';
end;

function peatextstats:integer;
var
  sfin:TStringList;
begin
result:=-1;
if FormReport.Visible<>true then exit;
if FormReport.LabelReport1.Caption<>txt_textpreview then exit;
sfin:=TStringList.Create;
result:=peasplitstring(FormReport.MemoReport.Text,sfin);
texstatsresult:=result;
if result<0 then
   begin
   FormReport.Labelsave.Caption:=sizeastr+' ['+txt_encerr+'] '+
   ' | '+txt_saveto+' ';
   end
else
   begin
   FormReport.Labelsave.Caption:=sizeastr+' ['+
   inttostr(FormReport.MemoReport.CaretPos.Y+1)+':'+inttostr(FormReport.MemoReport.CaretPos.X+1)+'] '+
   decodetextencoding+', '+inttostr(FormReport.MemoReport.Lines.Count)+' '+txt_l+', '+
   inttostr(result)+' '+txt_w+', '+
   inttostr(FormReport.MemoReport.GetTextLen)+' '+txt_c+
   ' | '+txt_saveto+' ';
   end;
end;

procedure uncheckenc;
begin
FormReport.encansi.Checked:=false;
FormReport.encascii.Checked:=false;
FormReport.encdefault.Checked:=false;
FormReport.encunicodebe.Checked:=false;
FormReport.encunicodele.Checked:=false;
FormReport.encutf7.Checked:=false;
FormReport.encutf8.Checked:=false;
end;

procedure TFormReport.encansiClick(Sender: TObject);
begin
uncheckenc;
FormReport.encansi.Checked:=true;
try
intextfileenc:=TEncoding.ANSI;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure TFormReport.encasciiClick(Sender: TObject);
begin
uncheckenc;
FormReport.encascii.Checked:=true;
try
intextfileenc:=TEncoding.ASCII;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure setpeacasesensitivesearch;
begin
peacasesensitive:=not(peacasesensitive);
FormReport.enccasesensitive.Checked:=peacasesensitive;
if FormReport.enccasesensitive.Checked=True then textcase:='S' else textcase:='s';
SetLength(peaprevpos,0);
end;

procedure TFormReport.enccasesensitiveClick(Sender: TObject);
begin
setpeacasesensitivesearch;
end;

procedure TFormReport.encdefaultClick(Sender: TObject);
begin
uncheckenc;
FormReport.encdefault.Checked:=true;
try
intextfileenc:=TEncoding.Default;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure TFormReport.encunicodebeClick(Sender: TObject);
begin
uncheckenc;
FormReport.encunicodebe.Checked:=true;
try
intextfileenc:=TEncoding.BigEndianUnicode;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure TFormReport.encunicodeleClick(Sender: TObject);
begin
uncheckenc;
FormReport.encunicodele.Checked:=true;
try
intextfileenc:=TEncoding.Unicode;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure TFormReport.encutf7Click(Sender: TObject);
begin
uncheckenc;
FormReport.encutf7.Checked:=true;
try
intextfileenc:=TEncoding.UTF7;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure TFormReport.encutf8Click(Sender: TObject);
begin
uncheckenc;
FormReport.encutf8.Checked:=true;
try
intextfileenc:=TEncoding.UTF8;
FormReport.MemoReport.Lines.BeginUpdate;
FormReport.MemoReport.Lines.LoadFromFile(intextfile,intextfileenc);
FormReport.MemoReport.Lines.EndUpdate;
peatextstats;
except
end;
end;

procedure memowordwrap;
begin
FormReport.MemoReport.WordWrap:=not(FormReport.MemoReport.WordWrap);
FormReport.encwwrap.Checked:=FormReport.MemoReport.WordWrap;
if FormReport.encwwrap.Checked=True then textwrap:='W' else textwrap:='w';
peatextstats; //needs to recalculate text stats when word wrap is changed
end;

procedure setpeabold;
begin
FormReport.MemoReport.Font.Bold:=not(FormReport.MemoReport.Font.Bold);
FormReport.FontBold.Checked:=FormReport.MemoReport.Font.Bold;
if FormReport.FontBold.Checked=True then textbold:='B' else textbold:='b';
end;

procedure TFormReport.encwwrapClick(Sender: TObject);
begin
memowordwrap;
end;

procedure TFormReport.peawordwrapExecute(Sender: TObject);
begin
memowordwrap;
end;

procedure fontzoomset;
var
   relzoom:integer;
begin
relzoom:=textzoom-100;
if relzoom=0 then exit;
with FormReport do
begin
if relzoom>0 then //enlarge size
   begin
   if GetFontData(MemoReport.Font.Handle).Height<0 then
      MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height-2*relzoom
   else
      MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height+2*relzoom;
   end
else //decrease size
   begin
   if (GetFontData(MemoReport.Font.Handle).Height>=-2*-relzoom) and (GetFontData(MemoReport.Font.Handle).Height<=2*-relzoom) then exit;
      if GetFontData(MemoReport.Font.Handle).Height<0 then
         MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height+2*-relzoom
      else
         MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height-2*-relzoom;
   end;
end;
end;

procedure fontzoomincrease;
begin
with FormReport do
begin
if GetFontData(MemoReport.Font.Handle).Height<0 then
   MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height-2
else
   MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height+2;
end;
textzoom:=textzoom+1;
end;

procedure fontzoomdecrease;
begin
with FormReport do
begin
if (GetFontData(MemoReport.Font.Handle).Height>=-2) and (GetFontData(MemoReport.Font.Handle).Height<=2) then exit;
if GetFontData(MemoReport.Font.Handle).Height<0 then
   MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height+2
else
   MemoReport.Font.Height:=GetFontData(MemoReport.Font.Handle).Height-2;
end;
textzoom:=textzoom-1;
end;

procedure TFormReport.fontzoominExecute(Sender: TObject);
begin
fontzoomincrease;
end;

procedure TFormReport.fonzoomplusExecute(Sender: TObject);
begin
fontzoomincrease;
end;

procedure TFormReport.fonzoomplusnExecute(Sender: TObject);
begin
fontzoomincrease;
end;

procedure TFormReport.FZoomInClick(Sender: TObject);
begin
fontzoomincrease;
end;

procedure TFormReport.fontzoomminusExecute(Sender: TObject);
begin
fontzoomdecrease;
end;

procedure setpeamonotypefont;
begin
if textmonotype='m' then
   begin
   FormReport.MemoReport.Font.Name:=DefFontData.Name;
   FormReport.FontMonotype.Checked:=false;
   textmonotype:='m';
   end
else
   try
   if textmfont<>'' then
      FormReport.MemoReport.Font.Name:=textmfont
   else
      {$IFDEF MSWINDOWS}
      FormReport.MemoReport.Font.Name:='Courier New';
      {$ELSE}
      {$IFDEF DARWIN}
      FormReport.MemoReport.Font.Name:='Courier New';
      {$ELSE}
      FormReport.MemoReport.Font.Name:='DejaVu Sans Mono';
      {$ENDIF}
      {$ENDIF}
   FormReport.FontMonotype.Checked:=true;
   textmonotype:='M';
   except
   FormReport.MemoReport.Font.Name:=DefFontData.Name;
   FormReport.FontMonotype.Checked:=false;
   textmonotype:='m';
   textmfont:='';
   end;
end;

procedure changepeamonotypefont;
begin
if textmonotype='m' then
   textmonotype:='M'
else
   textmonotype:='m';
setpeamonotypefont;
end;

procedure TFormReport.FontMonotypeClick(Sender: TObject);
begin
changepeamonotypefont;
end;

procedure TFormReport.FontBoldClick(Sender: TObject);
begin
setpeabold;
end;

procedure TFormReport.fontzoomminusnExecute(Sender: TObject);
begin
fontzoomdecrease;
end;

procedure TFormReport.fontzoomoutExecute(Sender: TObject);
begin
fontzoomdecrease;
end;

procedure TFormReport.FZoomOutClick(Sender: TObject);
begin
fontzoomdecrease;
end;

procedure TFormReport.FontSetClick(Sender: TObject);
begin
if FontDialogMemo.Execute then
   begin
   //MemoReport.Font:=FontDialogMemo.Font;
   textmfont:=FontDialogMemo.Font.Name;
   textmonotype:='M';
   setpeamonotypefont;
   end;
end;

procedure TFormReport.fontzoomresetExecute(Sender: TObject);
begin
MemoReport.Font.Size:=0;
textzoom:=100;
end;

procedure TFormReport.FZoomResetClick(Sender: TObject);
begin
MemoReport.Font.Size:=0;
textzoom:=100;
end;

procedure TFormReport.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
conditional_stop;
end;

procedure TFormReport.FormCreate(Sender: TObject);
begin
grid1index:=0;
grid2index:=0;
grid1switch:=true;
grid2switch:=true;
needclose:=false;
noreportdetails:=false;
psearchpos:=1;
peacasesensitive:=false;
set_report_text;
{$IFDEF DARWIN}
//replace some shortcuts with macOS specific alternative
peafind.ShortCut:=($1000) + Ord('F');
peafindnext.ShortCut:=($1000) + Ord('G');
peafindprev.ShortCut:=($3000) + Ord('G');
{$ENDIF}
end;

procedure TFormReport.FormShow(Sender: TObject);
begin
FormReport.PanelTitleREPTabAlign.Width:=FormReport.LabelTitleREP1.Width+
FormReport.LabelTitleREP2.Width+
FormReport.LabelTitleREP1.BorderSpacing.Left*3;
if alttabstyle<=2 then
   FormReport.PanelTitleREPTabAlign.AnchorSideLeft.Side:=asrleft
else
   FormReport.PanelTitleREPTabAlign.AnchorSideLeft.Side:=asrCenter;
if (alttabstyle=1) or (alttabstyle=4) then
   begin
   FormReport.LabelTitleREP1.AnchorSideTop.Control:=FormReport.PanelTitleREP;
   FormReport.ShapeTitleREPb1.visible:=false;
   FormReport.ShapeLinkREP1.visible:=true;
   FormReport.LabelTitleREP2.AnchorSideTop.Control:=FormReport.PanelTitleREP;
   FormReport.ShapeTitleREPb2.visible:=false;
   FormReport.ShapeLinkREP2.visible:=true;
   end
else
   begin
   FormReport.LabelTitleREP1.AnchorSideTop.Control:=FormReport.ShapeTitleREPb1;
   FormReport.ShapeTitleREPb1.visible:=true;
   FormReport.ShapeLinkREP1.visible:=false;
   FormReport.LabelTitleREP2.AnchorSideTop.Control:=FormReport.ShapeTitleREPb2;
   FormReport.ShapeTitleREPb2.visible:=true;
   FormReport.ShapeLinkREP2.visible:=false;
   end;

   case alttabstyle of
      0,3:
      begin
      FormReport.ShapeTitleREPb1.Shape:=stRoundRect;
      FormReport.ShapeTitleREPb2.Shape:=stRoundRect;
      FormReport.LabelTitleREP1.BorderSpacing.Left:=6;
      FormReport.LabelTitleREP2.BorderSpacing.Left:=6;
      end
      else
      begin
      FormReport.ShapeTitleREPb1.Shape:=stRectangle;
      FormReport.ShapeTitleREPb2.Shape:=stRectangle;
      FormReport.LabelTitleREP1.BorderSpacing.Left:=0;
      FormReport.LabelTitleREP2.BorderSpacing.Left:=0;
      end;
      end;
end;

procedure TFormReport.LabelCaseClick(Sender: TObject);
var
   irow,icol:integer;
   orig_activelabel_rep:TLabel;
begin
orig_activelabel_rep:=activelabel_rep;
if LabelCase.Caption='['+txt_upcase+']' then
   begin
   LabelCase.Caption:='['+txt_lowcase+']';
   if FormReport.StringGridInput.RowCount<2 then exit;
   if FormReport.StringGridInput.ColCount<24 then exit;
   for irow:=1 to FormReport.StringGridInput.RowCount-1 do
      for icol:=7 to 24 do FormReport.StringGridInput.Cells[icol,irow]:=upcase(FormReport.StringGridInput.Cells[icol,irow]);
   end
else
   begin
   LabelCase.Caption:='['+txt_upcase+']';
   if FormReport.StringGridInput.RowCount<2 then exit;
   if FormReport.StringGridInput.ColCount<24 then exit;
   for irow:=1 to FormReport.StringGridInput.RowCount-1 do
      for icol:=7 to 24 do FormReport.StringGridInput.Cells[icol,irow]:=lowercase(FormReport.StringGridInput.Cells[icol,irow]);
   end;
clicklabel_rep(LabelTitleREP2,ShapeTitleREPb2);
if orig_activelabel_rep=LabelTitleREP1 then clicklabel_rep(LabelTitleREP1,ShapeTitleREPb1);
end;

procedure TFormReport.LabelEncodingClick(Sender: TObject);
begin
PopupMenuEncoding.PopUp;
end;

procedure TFormReport.LabelSaveCsv1Click(Sender: TObject);
begin
save_report(FormReport.Caption,'csv',';','INTERACTIVE_REPORT','');
end;

procedure TFormReport.LabelSaveCsvClick(Sender: TObject);
begin
save_report(FormReport.Caption,'csv',',','INTERACTIVE_REPORT','');
end;

procedure TFormReport.LabelSaveTsvClick(Sender: TObject);
begin
save_report(FormReport.Caption,'tsv','','INTERACTIVE_REPORT','');
end;

procedure TFormReport.LabelSaveTxtClick(Sender: TObject);
begin
save_report(FormReport.Caption,'txt','','INTERACTIVE_REPORT','');
end;

procedure TFormReport.LabelTitleREP1Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_rep(LabelTitleREP1,ShapeTitleREPb1) else clicklabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TFormReport.LabelTitleREP1MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_rep(LabelTitleREP1,ShapeTitleREPb1) else enterlabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TFormReport.LabelTitleREP1MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_rep(LabelTitleREP1,ShapeTitleREPb1) else exitlabel_rep(LabelTitleREP1,ShapeLinkREP1);
end;

procedure TFormReport.LabelTitleREP2Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_rep(LabelTitleREP2,ShapeTitleREPb2) else clicklabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TFormReport.LabelTitleREP2MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_rep(LabelTitleREP2,ShapeTitleREPb2) else enterlabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TFormReport.LabelTitleREP2MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_rep(LabelTitleREP2,ShapeTitleREPb2) else exitlabel_rep(LabelTitleREP2,ShapeLinkREP2);
end;

procedure TFormReport.MemoReportClick(Sender: TObject);
begin
peatextstats;
end;

procedure TFormReport.MemoReportKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
peatextstats;
end;

procedure TFormReport.MenuItem1Click(Sender: TObject);
var
   s,p,fname:AnsiString;
begin
if StringGridInput.Row>0 then
   if (StringGridInput.Col>7) and (StringGridInput.Col<25) then
      begin
      s:=StringGridInput.Cells[StringGridInput.Col,StringGridInput.Row];
      if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then exit;
      fname:=StringGridInput.Cells[0,StringGridInput.Row]+'.'+StringGridInput.Cells[StringGridInput.Col,0]+'.txt';
      if pathistmp(GetCurrentDir)=true then //switch to desktop if file is in PeaZip's temp path (i.e. using the function on a preview of archived item)
         begin
         {$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
         if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
         fname:=p+extractfilename(fname);
         end;
      assignfile(t,fname);
      rewrite(t);
      write(t,s);
      closefile(t);
      end;
end;

procedure TFormReport.MenuItem2Click(Sender: TObject);
var
   s,fname,p:AnsiString;
   y:integer;
begin
if StringGridInput.Row>0 then
   begin
   if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then exit;
   fname:=StringGridInput.Cells[0,StringGridInput.Row]+'.info.txt';
   if pathistmp(GetCurrentDir)=true then //switch to desktop if file is in PeaZip's temp path (i.e. using the function on a preview of archived item)
      begin
      {$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
      if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
      fname:=p+extractfilename(fname);
      end;
   assignfile(t,fname);
   rewrite(t);
   write_header(t);
   writeln(t,txt_name+': '+StringGridInput.Cells[1,StringGridInput.Row]);
   writeln(t,txt_size+': '+StringGridInput.Cells[3,StringGridInput.Row]+' ('+StringGridInput.Cells[4,StringGridInput.Row]+' B)');
   writeln(t,txt_modified+': '+StringGridInput.Cells[5,StringGridInput.Row]);
   writeln(t,txt_attributes+': '+StringGridInput.Cells[6,StringGridInput.Row]);
   for y:=8 to 24 do
      if StringGridInput.ColWidths[y]>0 then
         writeln(t,StringGridInput.Cells[y,0]+': '+StringGridInput.Cells[y,StringGridInput.Row]);
   closefile(t);
   end;
end;

procedure TFormReport.MenuItem3Click(Sender: TObject);
begin
save_hashfn;
end;

function webopen(s:ansistring):integer;
var
   w:widestring;
begin
webopen:=-1;
if s='' then exit;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
webopen:=ShellExecuteW(FormReport.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}webopen:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}webopen:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}webopen:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF OPENBSD}webopen:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF DARWIN}webopen:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

function cp_open(s:ansistring; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s<>'' then
   {$IFDEF MSWINDOWS}
   w:=utf8decode(s);
   cp_open:=ShellExecuteW(FormReport.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), 1);//all Windows from 95 and NT3.1
   if cp_open<33 then
      cp_open:=shellexecuteW(FormReport.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), 1);
   {$ENDIF}
   {$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
   {$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF OPENBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF DARWIN}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TFormReport.MenuItem4Click(Sender: TObject);
var
  s:ansistring;
begin
if StringGridInput.Row>0 then
   if (StringGridInput.Col>7) and (StringGridInput.Col<25) then
      begin
      s:=StringGridInput.Cells[StringGridInput.Col,StringGridInput.Row];
      if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then exit;
      webopen('https://www.virustotal.com/gui/file/'+s);
      end;
end;

procedure TFormReport.MenuItem5Click(Sender: TObject);
var
  s:ansistring;
begin
if StringGridInput.Row>0 then
   if (StringGridInput.Col>7) and (StringGridInput.Col<25) then
      begin
      s:=StringGridInput.Cells[StringGridInput.Col,StringGridInput.Row];
      if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then exit;
      webopen('https://www.google.com/search?q='+s);
      end;
end;

procedure TFormReport.MenuItem6Click(Sender: TObject);
var
  s:ansistring;
begin
if StringGridInput.Row>0 then
   if (StringGridInput.Col>7) and (StringGridInput.Col<25) then
      begin
      s:=StringGridInput.Cells[StringGridInput.Col,StringGridInput.Row];
      if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then exit;
      webopen('https://virusscan.jotti.org/en-EN/search/hash/'+s);
      end;
end;

procedure TFormReport.MenuItemOpenPathClick(Sender: TObject);
begin
if StringGridInput.Row>0 then
   cp_open(extractfilepath(StringGridInput.Cells[0,StringGridInput.Row]),desk_env);
end;

procedure TFormReport.peaboldExecute(Sender: TObject);
begin
setpeabold;
end;

procedure TFormReport.peacasesearchExecute(Sender: TObject);
begin
setpeacasesensitivesearch;
end;

procedure TFormReport.peafindExecute(Sender: TObject);
begin
if FormReport.Visible<>true then exit;
if FormReport.LabelReport1.Caption<>txt_textpreview then exit;
peafindtext;
end;

procedure TFormReport.peafindnextExecute(Sender: TObject);
begin
if FormReport.Visible<>true then exit;
if FormReport.LabelReport1.Caption<>txt_textpreview then exit;
peafindtext;
end;

procedure TFormReport.peafindprevExecute(Sender: TObject);
begin
if FormReport.Visible<>true then exit;
if FormReport.LabelReport1.Caption<>txt_textpreview then exit;
peafindprevtext;
end;

procedure TFormReport.peamonotypefontExecute(Sender: TObject);
begin
changepeamonotypefont;
end;

procedure TFormReport.StringGridInputHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var i:integer;
begin
if grid1index=index then grid1switch:=not(grid1switch);
if grid1switch=true then StringGridInput.SortOrder:=soAscending else StringGridInput.SortOrder:=soDescending;
i:=index;
if (FormReport.Caption=txt_ch) and ((i=3) or (i=4)) then i:=25;
if (FormReport.Caption=txt_ch) and (i=29) then i:=30;
StringGridInput.SortColRow(true,i);
grid1index:=Index;
end;

procedure crcmenuenable(en:boolean);
begin
FormReport.MenuItem4.Enabled:=en;
FormReport.MenuItem6.Enabled:=en;
FormReport.MenuItem5.Enabled:=en;
FormReport.MenuItem3.Enabled:=en;
FormReport.MenuItem1.Enabled:=en;
FormReport.MenuItem2.Enabled:=en;
end;

procedure TFormReport.StringGridInputMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var col,row:integer;
begin
StringGridInput.MouseToCell(X,Y,col,row);
StringGridInput.Col:=col;
if (StringGridInput.Col>7) and (StringGridInput.Col<25) then
   begin
   crcmenuenable(true);
   MenuItem1.Caption:=txt_save+' '+StringGridInput.Cells[StringGridInput.Col,0]+' '+txt_thisfile;
   MenuItem3.Caption:=txt_save+' '+StringGridInput.Cells[StringGridInput.Col,0]+' '+txt_values;
   MenuItem4.Caption:=txt_search+' '+StringGridInput.Cells[StringGridInput.Col,0]+' / VirusTotal';
   MenuItem6.Caption:=txt_search+' '+StringGridInput.Cells[StringGridInput.Col,0]+' / Jotti VirusScan';
   MenuItem5.Caption:=txt_search+' '+StringGridInput.Cells[StringGridInput.Col,0]+' / Google';
   end
else
   begin
   crcmenuenable(true);
   MenuItem1.Caption:=txt_savefilehash;
   MenuItem3.Caption:=txt_savehashnames;
   MenuItem4.Caption:=txt_sh+' / VirusTotal';
   MenuItem6.Caption:=txt_sh+' / Jotti VirusScan';
   MenuItem5.Caption:=txt_sh+' / Google';
   MenuItem1.Enabled:=false;
   MenuItem3.Enabled:=false;
   MenuItem4.Enabled:=false;
   MenuItem6.Enabled:=false;
   MenuItem5.Enabled:=false;
   end;
if StringGridInput.Cells[0,StringGridInput.Row]=txt_digest then crcmenuenable(false);
end;

procedure TFormReport.StringGridOutputHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var i:integer;
begin
if grid2index=index then grid2switch:=not(grid2switch);
if grid2switch=true then StringGridOutput.SortOrder:=soAscending else StringGridOutput.SortOrder:=soDescending;
i:=index;
StringGridOutput.SortColRow(true,i);
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
   if Succeeded(ShGetSpecialFolderLocation(FormReport.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
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
  {$I unitreport.lrs}

end.

