unit UnitPM; //Password manager form: save and organize passwords and notes, set master password for the manager

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  UnitDlg, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Menus, ButtonPanel, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  pea_utils,list_utils;

type

  { TFormPM }

  TFormPM = class(TForm)
    ButtonEditNamePw: TSpeedButton;
    LabelPMadd: TLabel;
    LabelPMremove: TLabel;
    ButtonEditName3: TButton;
    ButtonPanelPM: TButtonPanel;
    ctrlpm: TSpeedButton;
    EditName3: TEdit;
    EditName4: TEdit;
    EditUn7zaPW: TEdit;
    EditUn7zaPW1: TEdit;
    EditUn7zaPW2: TEdit;
    ImageInfoPM1: TImage;
    LabelTitlePM1: TLabel;
    LabelTitlePM2: TLabel;
    LableListPath1: TLabel;
    LableListPath2: TLabel;
    LableListPath3: TLabel;
    LablePMHint: TLabel;
    MemoPM: TMemo;
    MenuItemPWreset: TMenuItem;
    MenuItemPRsingle: TMenuItem;
    PopupMenuPW: TPopupMenu;
    Separator1: TMenuItem;
    MenuItemPR: TMenuItem;
    mpmspacer3: TMenuItem;
    PageControlPM: TPanel;
    PanelKF: TPanel;
    PanelPWMaster: TPanel;
    PanelTitlePMTabAlign: TPanel;
    PanelTitlePM: TPanel;
    pmexpenc: TMenuItem;
    pmexpplain: TMenuItem;
    pmexp: TMenuItem;
    mpw: TMenuItem;
    mnote: TMenuItem;
    mpmspacer2: TMenuItem;
    mpwexplore: TMenuItem;
    mpwreset: TMenuItem;
    mremove: TMenuItem;
    mpmspacer4: TMenuItem;
    mreset: TMenuItem;
    mnew: TMenuItem;
    mpmspacer1: TMenuItem;
    OpenDialogKFM: TOpenDialog;
    pmPM: TPopupMenu;
    PopupMenupwm: TPopupMenu;
    SelectDirectoryPM: TSelectDirectoryDialog;
    Shapelinkpm1: TShape;
    Shapelinkpm2: TShape;
    ShapeTitlePMb1: TShape;
    ShapeTitlePMb2: TShape;
    StringGridPM: TStringGrid;
    StringGridPM1: TStringGrid;
    TabSheetMasterPW: TPanel;
    TabSheetPWlist: TPanel;
    procedure ButtonEditNamePwClick(Sender: TObject);
    procedure LabelPMaddClick(Sender: TObject);
    procedure LabelPMremoveClick(Sender: TObject);
    procedure ButtonEditName3Click(Sender: TObject);
    procedure ctrlpmClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageInfoPM1Click(Sender: TObject);
    procedure LabelTitlePM1Click(Sender: TObject);
    procedure LabelTitlePM1MouseEnter(Sender: TObject);
    procedure LabelTitlePM1MouseLeave(Sender: TObject);
    procedure LabelTitlePM2Click(Sender: TObject);
    procedure LabelTitlePM2MouseEnter(Sender: TObject);
    procedure LabelTitlePM2MouseLeave(Sender: TObject);
    procedure LableListPath2Click(Sender: TObject);
    procedure MenuItemPRClick(Sender: TObject);
    procedure MenuItemPRsingleClick(Sender: TObject);
    procedure MenuItemPWresetClick(Sender: TObject);
    procedure mnoteClick(Sender: TObject);
    procedure mpwClick(Sender: TObject);
    procedure mnewClick(Sender: TObject);
    procedure mpwexploreClick(Sender: TObject);
    procedure mpwresetClick(Sender: TObject);
    procedure mremoveClick(Sender: TObject);
    procedure mresetClick(Sender: TObject);
    procedure pmexpencClick(Sender: TObject);
    procedure pmexpplainClick(Sender: TObject);
    procedure StringGridPMColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGridPMDblClick(Sender: TObject);
    procedure StringGridPMHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure StringGridPMKeyPress(Sender: TObject; var Key: char);
    procedure StringGridPMMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure clicklabel_pm(var a: TLabel; var b:TShape);
procedure deselectlabels_pm;

var
  FormPM: TFormPM;
  txt_edit,caption_build,delimiter,wincomspec,winver,validate_txt,local_desktop,confpath,color2:ansistring;
  Column,Row,alttabstyle,highlighttabs,previndex:integer;
  desk_env:byte;
  pmfilet:text;
  txt_clear,txt_pr1,txt_pr2,txt_pr3,txt_pr4:AnsiString;
  activelabel_pm:TLabel;
  tabpencol,tablowcol,tabbrushcol,tabbrushhighcol:tcolor;

implementation

{ TFormPM }

///pm
procedure exitlabel_pm(var a: TLabel; var b:TShape);
begin
if activelabel_pm=a then exit;
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

procedure deselectlabels_pm;
begin
with FormPM do
begin
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   exitlabel_pm(LabelTitlePM1,ShapeTitlePMb1);
   exitlabel_pm(LabelTitlePM2,ShapeTitlePMb2);
   end
else
   begin
   exitlabel_pm(LabelTitlePM1,Shapelinkpm1);
   exitlabel_pm(LabelTitlePM2,Shapelinkpm2)
   end;
end;
end;

procedure setpanel_pm(i:integer);
begin
case i of
   1: begin
   FormPM.TabSheetPWlist.Visible:=true;
   FormPM.TabSheetMasterPW.Visible:=false;
   end;
   2: begin
   FormPM.TabSheetPWlist.Visible:=false;
   FormPM.TabSheetMasterPW.Visible:=true;
   end;
end;
end;

procedure setlabelpanel_pm(var a: Tlabel);
begin
with FormPM do
begin
if a = LabelTitlePM1 then setpanel_pm(1);
if a = LabelTitlePM2 then setpanel_pm(2);
end;
end;

procedure clicklabel_pm(var a: TLabel; var b:TShape);
begin
activelabel_pm:=a;
deselectlabels_pm;
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
setlabelpanel_pm(a);
end;

procedure enterlabel_pm(var a: TLabel; var b:TShape);
begin
if activelabel_pm=a then exit;
b.Brush.Color:=tabbrushhighcol;
b.Pen.Color:=tabpencol;
b.Pen.Style:=psSolid;
a.Font.Color:=clDefault;
end;

///

procedure new_pm;
var
  i,rc:integer;
  s:ansistring;
begin
if InputQuery(txt_edit+' '+FormPM.mpw.Caption, '', s) then
   begin
   rc:=FormPM.StringGridPM.Rowcount+1;
   FormPM.StringGridPM.RowCount:=rc;
   FormPM.StringGridPM.Cells[1,rc-1]:=s;
   FormPM.StringGridPM.Cells[2,rc-1]:='';
   if rc>2 then
      for i:=1 to rc-2 do FormPM.StringGridPM.Cells[3,i]:='0'
   else
      i:=0;
   FormPM.StringGridPM.Cells[3,rc-1]:='1';
   FormPM.StringGridPM.Cells[0,rc-1]:=inttostr(i+1);
   FormPM.StringGridPM.AutosizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure editpw_pm;
var s:ansistring;
begin
if FormPM.StringGridPM.Row=0 then
   begin
   new_pm;
   exit;
   end;
s:=FormPM.StringGridPM.Cells[1,FormPM.StringGridPM.Row];
if InputQuery(txt_edit+' '+FormPM.mpw.Caption, '', s) then
   begin
   FormPM.StringGridPM.Cells[1,FormPM.StringGridPM.Row]:=s;
   FormPM.StringGridPM.AutoSizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure setdescription_pm;
var s:ansistring;
begin
if FormPM.StringGridPM.Row=0 then
   begin
   new_pm;
   exit;
   end;
s:=FormPM.StringGridPM.Cells[2,FormPM.StringGridPM.Row];
if InputQuery(txt_edit+' '+FormPM.mnote.Caption, '', s) then
   begin
   FormPM.StringGridPM.Cells[2,FormPM.StringGridPM.Row]:=s;
   FormPM.StringGridPM.AutoSizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure TFormPM.mpwClick(Sender: TObject);
begin
editpw_pm;
end;

procedure TFormPM.mnoteClick(Sender: TObject);
begin
setdescription_pm;
end;

procedure TFormPM.FormCreate(Sender: TObject);
begin
getdesk_env(desk_env,caption_build,delimiter);
{$IFDEF MSWINDOWS}
getwinenv(wincomspec,winver);
{$ENDIF}
color2:=colortostring(clWindow);
end;

procedure TFormPM.ImageInfoPM1Click(Sender: TObject);
begin
pMessageInfoOK(ImageInfoPM1.Hint);
end;

procedure TFormPM.LabelTitlePM1Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_pm(LabelTitlePM1,ShapeTitlePMb1) else clicklabel_pm(LabelTitlePM1,Shapelinkpm1);
end;

procedure TFormPM.LabelTitlePM1MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_pm(LabelTitlePM1,ShapeTitlePMb1) else enterlabel_pm(LabelTitlePM1,Shapelinkpm1);
end;

procedure TFormPM.LabelTitlePM1MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_pm(LabelTitlePM1,ShapeTitlePMb1) else exitlabel_pm(LabelTitlePM1,Shapelinkpm1);
end;

procedure TFormPM.LabelTitlePM2Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_pm(LabelTitlePM2,ShapeTitlePMb2) else clicklabel_pm(LabelTitlePM2,Shapelinkpm2);
end;

procedure TFormPM.LabelTitlePM2MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_pm(LabelTitlePM2,ShapeTitlePMb2) else enterlabel_pm(LabelTitlePM2,Shapelinkpm2);
end;

procedure TFormPM.LabelTitlePM2MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_pm(LabelTitlePM2,ShapeTitlePMb2) else exitlabel_pm(LabelTitlePM2,Shapelinkpm2);
end;

procedure TFormPM.LableListPath2Click(Sender: TObject);
begin
PanelKF.Visible:=not(PanelKF.Visible);
end;

procedure TFormPM.MenuItemPRClick(Sender: TObject);
begin
if FormPM.StringGridPM.Row=0 then exit;
pMessageInfoOK(ratepw(FormPM.StringGridPM.Cells[1,FormPM.StringGridPM.Row],txt_pr1,txt_pr2,txt_pr3,txt_pr4));
end;

procedure TFormPM.MenuItemPRsingleClick(Sender: TObject);
begin
if FormPM.EditUn7zaPW.Caption='' then exit;
pMessageInfoOK(ratepw(FormPM.EditUn7zaPW.Caption,txt_pr1,txt_pr2,txt_pr3,txt_pr4));
end;

procedure TFormPM.MenuItemPWresetClick(Sender: TObject);
begin
EditUn7zaPW.Text:='';
EditUn7zaPW1.Text:='';
EditUn7zaPW2.Text:='';
EditName3.Text:='';
EditName4.Text:='';
end;

procedure TFormPM.ButtonEditName3Click(Sender: TObject);
begin
if OpenDialogKFM.Execute then
   if OpenDialogKFM.FileName<>'' then EditName3.Text:=OpenDialogKFM.FileName
   else exit
else exit;
end;

procedure TFormPM.ctrlpmClick(Sender: TObject);
var
   p:TPoint;
begin
p.x:=ctrlpm.Left+ctrlpm.Width;
p.y:=ctrlpm.top+ctrlpm.Height;
p:=clienttoscreen(p);
pmPM.Alignment:=paRight;
pmPM.PopUp(p.x,p.y);
pmPM.Alignment:=paLeft;
end;

procedure TFormPM.LabelPMaddClick(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_pm(LabelTitlePM1,ShapeTitlePMb1) else clicklabel_pm(LabelTitlePM1,Shapelinkpm1);
new_pm;
end;

procedure TFormPM.ButtonEditNamePwClick(Sender: TObject);
var p:tpoint;
begin
p.x:=buttoneditnamepw.left;
p.y:=PageControlPM.top+buttoneditnamepw.top+buttoneditnamepw.height;
p:=clienttoscreen(p);
popupmenupw.popup(p.x,p.y);
end;

procedure TFormPM.LabelPMremoveClick(Sender: TObject);
var
   i:integer;
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_pm(LabelTitlePM1,ShapeTitlePMb1) else clicklabel_pm(LabelTitlePM1,Shapelinkpm1);
if StringGridPM.RowCount<2 then exit;
StringGridPM.DeleteColRow(false,Row);
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.mnewClick(Sender: TObject);
begin
new_pm;
end;

//open, cross platform, with sanitization of string passed to the function
function cp_open(s:ansistring; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s='' then exit;
if validatecl(s)<>0 then
   begin
   if s<>'' then pMessageWarningOK(validate_txt+' '+s);
   exit;
   end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(FormPM.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(FormPM.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF OPENBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF DARWIN}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TFormPM.mpwexploreClick(Sender: TObject);
var
   s:ansistring;
begin
s:=extractfilepath(EditName3.Text);
cp_open(s,desk_env);
end;

procedure TFormPM.mpwresetClick(Sender: TObject);
begin
EditName3.Text:='';
end;

procedure TFormPM.mremoveClick(Sender: TObject);
var
   i:integer;
begin
if StringGridPM.RowCount<2 then exit;
if Row=0 then exit;
StringGridPM.DeleteColRow(false,Row);
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.mresetClick(Sender: TObject);
begin
if pMessageWarningYesNo(txt_clear)=6 then
   StringGridPM.RowCount:=1;
end;

procedure TFormPM.pmexpencClick(Sender: TObject);
var
   numread,numwritten:integer;
   fin,fout:file of byte;
   read_data:array[0..65535]of byte;
begin
try
SelectDirectoryPM.InitialDir:=local_desktop;
if SelectDirectoryPM.Execute then
   if SelectDirectoryPM.FileName<>'' then
      begin
      assignfile(fin,(confpath+'pm'));
      filemode:=0;
      reset(fin);
      assignfile(fout,(SelectDirectoryPM.FileName+directoryseparator+'pm'+formatdatetime('-yyyymmdd-hh.nn.ss',now)+'.bak'));
      rewrite(fout);
      repeat
         blockread (fin,read_data,65536,numread);
         blockwrite (fout,read_data,numread,numwritten);
      until ((numread=0) or (numread<>numwritten));
      closefile(fin);
      closefile(fout);
      end;
except
   try
   closefile(fin);
   closefile(fout);
   except
   end;
end;
end;

procedure TFormPM.pmexpplainClick(Sender: TObject);
var
   i,j,rc:integer;
begin
try
SelectDirectoryPM.InitialDir:=local_desktop;
if SelectDirectoryPM.Execute then
   if SelectDirectoryPM.FileName<>'' then
      begin
      assignfile(pmfilet,(SelectDirectoryPM.FileName+directoryseparator+'pm'+formatdatetime('-yyyymmdd-hh.nn.ss',now)+'.txt'));
      rewrite(pmfilet);
      write_header(pmfilet);
      rc:=FormPM.StringGridPM.RowCount;
      for i:=1 to rc-1 do
          begin
          for j:=1 to 2 do writeln(pmfilet,FormPM.StringGridPM.Cells[j,i]);
          writeln(pmfilet,'');
          writeln(pmfilet,'');
          writeln(pmfilet,'');//3 free slots for each entry, for future use
          end;
      CloseFile(pmfilet);
      end;
except
   try
   CloseFile(pmfilet);
   except
   end;
end;
end;

procedure TFormPM.StringGridPMColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
var
   i:integer;
begin
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.StringGridPMDblClick(Sender: TObject);
begin
case StringGridPM.Col of
   1: editpw_pm;
   2: setdescription_pm;
   end;
end;

procedure TFormPM.StringGridPMHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
   i:integer;
begin
if IsColumn=false then exit;
if Index=previndex then
   if StringGridPM.SortOrder=soAscending then StringGridPM.SortOrder:=soDescending
   else StringGridPM.SortOrder:=soAscending
else StringGridPM.SortOrder:=soAscending;
previndex:=Index;
StringGridPM.SortColRow(true,Index);
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.StringGridPMKeyPress(Sender: TObject; var Key: char);
var
   i:integer;
begin
if StringGridPM.Rowcount<2 then exit;
//search char
if ((Column<1) or (Column>2)) then Column:=1;
for i:=StringGridPM.Row+1 to StringGridPM.Rowcount-1 do
   if upcase(copy(StringGridPM.Cells[Column,i],1,1))=upcase(Key) then
      begin
      StringGridPM.Row:=i;
      exit;
      end;
for i:=1 to StringGridPM.Row do
   if upcase(copy(StringGridPM.Cells[Column,i],1,1))=upcase(Key) then
      begin
      StringGridPM.Row:=i;
      exit;
      end;
end;

procedure TFormPM.StringGridPMMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
StringGridPM.MouseToCell(X, Y, Column, Row);
StringGridPM.Row:=Row;
end;

initialization
  {$I unitpm.lrs}

end.

