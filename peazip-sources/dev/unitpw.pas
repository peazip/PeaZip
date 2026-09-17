unit UnitPW; //Form with password and keyfile fields, and password -relevant options

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  UnitDlg, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,
  pea_utils,list_utils, Buttons, ButtonPanel, Menus,
  UnitPM;

type

  { TFormPW }

  TFormPW = class(TForm)
    ButtonEditNamePw: TSpeedButton;
    ButtonPWkeyfile: TButton;
    ButtonPanelPW: TButtonPanel;
    CheckBoxEncFn: TCheckBox;
    CheckBoxIntPw: TCheckBox;
    CheckBoxKeepPW: TCheckBox;
    CheckBoxShowPWField: TCheckBox;
    EditPWkeyfile: TEdit;
    EditPWpass: TEdit;
    EditPWconfirm: TEdit;
    LabelPWpass: TLabel;
    LabelPWkeyfile: TLabel;
    LabelPWconfirm: TLabel;
    mpwpr: TMenuItem;
    mpwspacer2: TMenuItem;
    PanelKF: TPanel;
    PanelPWOpen1: TPanel;
    mpwmreset: TMenuItem;
    mpwspacer1: TMenuItem;
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
    ShapePW: TPanel;
    procedure ButtonEditNamePwClick(Sender: TObject);
    procedure ButtonPWkeyfileClick(Sender: TObject);
    procedure CheckBoxIntPwClick(Sender: TObject);
    procedure CheckBoxShowPWFieldClick(Sender: TObject);
    procedure EditPWpassChange(Sender: TObject);
    procedure EditPWpassKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure LabelPWkeyfileClick(Sender: TObject);
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
    procedure mpwprClick(Sender: TObject);
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
  caption_build,delimiter,wincomspec,winver,validate_txt,txt_pw,txt_pr1,txt_pr2,txt_pr3,txt_pr4:ansistring;
  activelabel_pw:TLabel;

implementation

{ TFormPW }

procedure TFormPW.EditPWpassKeyPress(Sender: TObject; var Key: char);
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
pw:=FormPW.EditPWpass.Text;
if pw='' then
   begin
   FormPW.ShapePW.Color:=clWindow;
   FormPW.ShapePW.Height:=0;
   exit;
   end;
evaluate_password(pw,pw_strength);
if pw_strength<24 then FormPW.ShapePW.Color:=PRED
else
   if pw_strength<48 then FormPW.ShapePW.Color:=PYELLOW
   else
      if pw_strength<72 then FormPW.ShapePW.Color:=PLGREEN
      else FormPW.ShapePW.Color:=PGREEN;
FormPW.ShapePW.Height:=BARH;
end;

procedure TFormPW.CheckBoxShowPWFieldClick(Sender: TObject);
begin
if CheckBoxShowPWField.State=cbChecked then
   begin
   EditPWpass.PasswordChar:=#0;
   LabelPWconfirm.Visible:=false;
   EditPWconfirm.visible:=false;
   EditPWconfirm.Text:='';
   end
else
   begin
   EditPWpass.PasswordChar:='*';
   if FormPW.Caption=txt_pw then
      begin
      LabelPWconfirm.Visible:=true;
      EditPWconfirm.visible:=true;
      end;
   end;
FormPW.Refresh;
end;

procedure TFormPW.EditPWpassChange(Sender: TObject);
begin
  color_password;
end;

procedure TFormPW.ButtonPWkeyfileClick(Sender: TObject);
begin
if OpenDialogKF.Execute then
   if OpenDialogKF.FileName<>'' then EditPWkeyfile.Text:=OpenDialogKF.FileName
   else exit
else exit;
end;

procedure TFormPW.CheckBoxIntPwClick(Sender: TObject);
begin
if CheckBoxIntPw.State=cbChecked then
   begin
   EditPWpass.Text:='';
   EditPWconfirm.Text:='';
   PanelKF.Visible:=true;
   EditPWpass.Enabled:=false;
   EditPWconfirm.Enabled:=false;
   LabelPWkeyfile.Enabled:=false;
   CheckBoxKeepPW.Enabled:=false;
   CheckBoxShowPWField.Enabled:=false;
   end
else
   begin
   EditPWpass.Enabled:=true;
   EditPWconfirm.Enabled:=true;
   LabelPWkeyfile.Enabled:=true;
   CheckBoxKeepPW.Enabled:=true;
   CheckBoxShowPWField.Enabled:=true;
   end;
FormPW.Refresh;
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

procedure TFormPW.LabelPWkeyfileClick(Sender: TObject);
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
if validatecl(s)<>0 then
   begin
   if s<>'' then pMessageWarningOK(validate_txt+' '+s);
   exit;
   end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(FormPW.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(FormPW.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF OPENBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF DARWIN}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TFormPW.mpwexploreClick(Sender: TObject);
var
   s:ansistring;
begin
s:=extractfilepath(EditPWkeyfile.Text);
cp_open(s,desk_env);
end;

procedure TFormPW.mpwman1Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman1.Caption;
EditPWconfirm.Caption:=mpwman1.Caption;
end;

procedure TFormPW.mpwman2Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman2.Caption;
EditPWconfirm.Caption:=mpwman2.Caption;
end;

procedure TFormPW.mpwman3Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman3.Caption;
EditPWconfirm.Caption:=mpwman3.Caption;
end;

procedure TFormPW.mpwman4Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman4.Caption;
EditPWconfirm.Caption:=mpwman4.Caption;
end;

procedure TFormPW.mpwman5Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman5.Caption;
EditPWconfirm.Caption:=mpwman5.Caption;
end;

procedure TFormPW.mpwman6Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman6.Caption;
EditPWconfirm.Caption:=mpwman6.Caption;
end;

procedure TFormPW.mpwman7Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman7.Caption;
EditPWconfirm.Caption:=mpwman7.Caption;
end;

procedure TFormPW.mpwman8Click(Sender: TObject);
begin
EditPWpass.Caption:=mpwman8.Caption;
EditPWconfirm.Caption:=mpwman8.Caption;
end;

procedure TFormPW.mpwmanClick(Sender: TObject);
begin
FormPW.ModalResult:=mrAbort;
end;

procedure TFormPW.mpwmresetClick(Sender: TObject);
begin
EditPWpass.Text:='';
EditPWconfirm.Text:='';
EditPWkeyfile.Text:='';
end;

procedure TFormPW.mpwprClick(Sender: TObject);
begin
if FormPW.EditPWpass.Caption='' then exit;
pMessageInfoOK(ratepw(FormPW.EditPWpass.Caption,txt_pr1,txt_pr2,txt_pr3,txt_pr4));
end;

procedure TFormPW.mpwresetClick(Sender: TObject);
begin
EditPWkeyfile.Text:='';
end;

initialization
  {$I unitpw.lrs}

end.

