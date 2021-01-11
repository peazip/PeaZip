unit Unit3; 

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Unit7, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,
  pea_utils,list_utils, Buttons, ButtonPanel, Menus,
  Unit8;

type

  { TFormPW }

  TFormPW = class(TForm)
    ButtonEditNamePw: TSpeedButton;
    ButtonEditName3: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxEncFn: TCheckBox;
    CheckBoxKeepPW: TCheckBox;
    CheckBoxShowPWField: TCheckBox;
    EditName3: TEdit;
    EditUn7zaPW: TEdit;
    EditUn7zaPW1: TEdit;
    LableListPath1: TLabel;
    LableListPath2: TLabel;
    LableListPath3: TLabel;
    MenuItem1: TMenuItem;
    PanelKF: TPanel;
    PanelPWOpen1: TPanel;
    mpwmreset: TMenuItem;
    MenuItem3: TMenuItem;
    mpwman: TMenuItem;
    mpwman3: TMenuItem;
    mpwman4: TMenuItem;
    mpwman5: TMenuItem;
    mpwman6: TMenuItem;
    mpwman7: TMenuItem;
    mpwman8: TMenuItem;
    mpwman2: TMenuItem;
    mpwreset: TMenuItem;
    mpwexplore: TMenuItem;
    mpwman1: TMenuItem;
    OpenDialogKF: TOpenDialog;
    PanelPWOpen: TPanel;
    PopupMenupw: TPopupMenu;
    PopupMenupwman: TPopupMenu;
    Shape1: TPanel;
    procedure ButtonEditNamePwClick(Sender: TObject);
    procedure ButtonEditName3Click(Sender: TObject);
    procedure CheckBoxShowPWFieldClick(Sender: TObject);
    procedure EditUn7zaPWChange(Sender: TObject);
    procedure EditUn7zaPWKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure LableListPath2Click(Sender: TObject);
    procedure mpwexploreClick(Sender: TObject);
    procedure mpwman1Click(Sender: TObject);
    procedure mpwman2Click(Sender: TObject);
    procedure mpwman3Click(Sender: TObject);
    procedure mpwman4Click(Sender: TObject);
    procedure mpwman5Click(Sender: TObject);
    procedure mpwman6Click(Sender: TObject);
    procedure mpwman7Click(Sender: TObject);
    procedure mpwman8Click(Sender: TObject);
    procedure mpwmanClick(Sender: TObject);
    procedure mpwmresetClick(Sender: TObject);
    procedure mpwresetClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

procedure color_password;

const
  BARH          = 8;

var
  FormPW: TFormPW;
  desk_env,showpwfield:byte;
  caption_build,delimiter,wincomspec,winver,validate_txt,txt_pw:ansistring;
  activelabel_pw:TLabel;

implementation

{ TFormPW }

procedure TFormPW.EditUn7zaPWKeyPress(Sender: TObject; var Key: char);
begin
if Key=char(13) then
   begin
   FormPW.Close;
   FormPW.ModalResult:=1;
   end;
end;

procedure color_password;
var
   pw:ansistring;
   pw_strength:dword;
begin
pw:=FormPW.EditUn7zaPW.Text;
if pw='' then
   begin
   FormPW.Shape1.Color:=clWindow;
   FormPW.Shape1.Height:=0;
   exit;
   end;
evaluate_password(pw,pw_strength);
if pw_strength<24 then FormPW.Shape1.Color:=PRED
else
   if pw_strength<48 then FormPW.Shape1.Color:=PYELLOW
   else
      if pw_strength<72 then FormPW.Shape1.Color:=PLGREEN
      else FormPW.Shape1.Color:=PGREEN;
FormPW.Shape1.Height:=BARH;
end;

procedure TFormPW.CheckBoxShowPWFieldClick(Sender: TObject);
begin
if CheckBoxShowPWField.State=cbChecked then
   begin
   EditUn7zaPW.PasswordChar:=#0;
   lablelistPath3.Visible:=false;
   EditUn7zaPW1.visible:=false;
   EditUn7zaPW1.Text:='';
   end
else
   begin
   EditUn7zaPW.PasswordChar:='*';
   if FormPW.Caption=txt_pw then
      begin
      lablelistPath3.Visible:=true;
      EditUn7zaPW1.visible:=true;
      end;
   end;
FormPW.Refresh;
end;

procedure TFormPW.EditUn7zaPWChange(Sender: TObject);
begin
  color_password;
end;

procedure TFormPW.ButtonEditName3Click(Sender: TObject);
begin
if OpenDialogKF.Execute then
   if OpenDialogKF.FileName<>'' then EditName3.Text:=OpenDialogKF.FileName
   else exit
else exit;
end;

procedure TFormPW.ButtonEditNamePwClick(Sender: TObject);
var p:tpoint;
begin
p.x:=buttoneditnamepw.left;
p.y:=PanelPWOpen.top+buttoneditnamepw.top+buttoneditnamepw.height;
p:=clienttoscreen(p);
popupmenupwman.popup(p.x,p.y);
end;

procedure TFormPW.FormCreate(Sender: TObject);
begin
getdesk_env(desk_env,caption_build,delimiter);
{$IFDEF MSWINDOWS}
getwinenv(wincomspec,winver);
{$ENDIF}
end;

procedure TFormPW.LableListPath2Click(Sender: TObject);
begin
PanelKF.Visible:=not(PanelKF.Visible);
end;

//open, cross platform, with sanitization of string passed to the function
function cp_open(s:ansistring; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s='' then exit;
if validatecl(s)<>0 then begin pMessageWarningOK(validate_txt+' '+s); exit; end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(FormPW.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(FormPW.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TFormPW.mpwexploreClick(Sender: TObject);
var
   s:ansistring;
begin
s:=extractfilepath(EditName3.Text);
cp_open(s,desk_env);
end;

procedure TFormPW.mpwman1Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman1.Caption;
EditUn7zaPW1.Caption:=mpwman1.Caption;
end;

procedure TFormPW.mpwman2Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman2.Caption;
EditUn7zaPW1.Caption:=mpwman2.Caption;
end;

procedure TFormPW.mpwman3Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman3.Caption;
EditUn7zaPW1.Caption:=mpwman3.Caption;
end;

procedure TFormPW.mpwman4Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman4.Caption;
EditUn7zaPW1.Caption:=mpwman4.Caption;
end;

procedure TFormPW.mpwman5Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman5.Caption;
EditUn7zaPW1.Caption:=mpwman5.Caption;
end;

procedure TFormPW.mpwman6Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman6.Caption;
EditUn7zaPW1.Caption:=mpwman6.Caption;
end;

procedure TFormPW.mpwman7Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman7.Caption;
EditUn7zaPW1.Caption:=mpwman7.Caption;
end;

procedure TFormPW.mpwman8Click(Sender: TObject);
begin
EditUn7zaPW.Caption:=mpwman8.Caption;
EditUn7zaPW1.Caption:=mpwman8.Caption;
end;

procedure TFormPW.mpwmanClick(Sender: TObject);
begin
FormPW.ModalResult:=mrAbort;
end;

procedure TFormPW.mpwmresetClick(Sender: TObject);
begin
EditUn7zaPW.Text:='';
EditUn7zaPW1.Text:='';
EditName3.Text:='';
end;

procedure TFormPW.mpwresetClick(Sender: TObject);
begin
EditName3.Text:='';
end;

initialization
  {$I unit3.lrs}

end.

