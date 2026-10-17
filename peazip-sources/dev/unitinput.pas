unit UnitInput; //Multi-purpose user input form (search, rename)

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Buttons, ExtCtrls, list_utils;

type

  { TFormInput }

  TFormInput = class(TForm)
    BitBtnReset: TBitBtn;
    BitBtnBrowse: TBitBtn;
    ButtonPanelInput: TButtonPanel;
    CheckBoxInputRecoursive: TCheckBox;
    Editinputquery: TComboBox;
    LabelLow: TLabel;
    Labelrenamereset: TLabel;
    Labelinputspacer1: TLabel;
    Labelinputspacer4: TLabel;
    Labelappdirn: TLabel;
    Labelinputspacer2: TLabel;
    LabelMoveTo: TLabel;
    Labelinputspacer3: TLabel;
    LabelUp: TLabel;
    LabelTimestamp: TLabel;
    Labelprepdirn: TLabel;
    LabelWarning: TLabel;
    OpenDialogInput: TOpenDialog;
    PanelInput: TPanel;
    procedure BitBtnBrowseClick(Sender: TObject);
    procedure LabelappdirnClick(Sender: TObject);
    procedure LabelMoveToClick(Sender: TObject);
    procedure LabelLowClick(Sender: TObject);
    procedure LabelprepdirnClick(Sender: TObject);
    procedure LabelrenameresetClick(Sender: TObject);
    procedure LabelTimestampClick(Sender: TObject);
    procedure LabelUpClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormInput: TFormInput;
  tsstyle,moveclick:integer;
  dirn,instr:ansistring;

implementation

{ TFormInput }

procedure apply_timestamptofilename(var s:ansistring);
var
   dt_stamp,s0,s1,s2:ansistring;
begin
case tsstyle of
   0: dt_stamp:=formatdatetime('.yyyymmdd.hhnnss',now);
   1: dt_stamp:=formatdatetime('.yyyymmdd',now);
   2: dt_stamp:=formatdatetime('yyyymmdd.hhnnss.',now);
   3: dt_stamp:=formatdatetime('yyyymmdd.',now);
   end;
s0:=extractfilepath(s);
s1:=extractfilename(s);
if not(DirectoryExists(dirn+s)) then
   begin
   cutextension(s1);
   s2:=extractfileext(s);
   end
else s2:='';
case tsstyle of
   2: s1:=dt_stamp+s1;
   3: s1:=dt_stamp+s1;
   else s1:=s1+dt_stamp;
end;
s:=s0+s1+s2;
end;

procedure apply_dirnametoname(var s:ansistring; dmode:ansistring);
var
   s1,s2,sdir:ansistring;
begin
s1:=extractfilename(s);
if not(DirectoryExists(dirn+s)) then
   begin
   cutextension(s1);
   s2:=extractfileext(s);
   end
else s2:='';
sdir:=dirn;
if length(sdir)<=3 then exit;
if sdir[length(sdir)]=directoryseparator then setlength(sdir,length(sdir)-1);
sdir:=extractfilename(sdir);
if length(sdir)>128 then sdir:=copy(sdir,1,128);
case dmode of
   'prepend': s1:=sdir+' - '+s1;
   'append': s1:=s1+' - '+sdir;
   end;
s:=s1+s2;
end;

procedure TFormInput.LabelappdirnClick(Sender: TObject);
var
   s:ansistring;
begin
s:=Editinputquery.Text;
apply_dirnametoname(s,'append');
Editinputquery.Text:=s;
end;

procedure TFormInput.LabelMoveToClick(Sender: TObject);
begin
moveclick:=1;
FormInput.ModalResult:=mrCancel;
end;

procedure TFormInput.LabelLowClick(Sender: TObject);
begin
Editinputquery.Text:=LowerCase(Editinputquery.Text);
end;

procedure TFormInput.LabelprepdirnClick(Sender: TObject);
var
   s:ansistring;
begin
s:=Editinputquery.Text;
apply_dirnametoname(s,'prepend');
Editinputquery.Text:=s;
end;

procedure TFormInput.LabelrenameresetClick(Sender: TObject);
begin
Editinputquery.Text:=instr;
end;

procedure TFormInput.LabelTimestampClick(Sender: TObject);
var
   s:ansistring;
begin
s:=Editinputquery.Text;
apply_timestamptofilename(s);
Editinputquery.Text:=s;
end;

procedure TFormInput.LabelUpClick(Sender: TObject);
begin
Editinputquery.Text:=UpperCase(Editinputquery.Text);
end;

procedure TFormInput.BitBtnBrowseClick(Sender: TObject);
begin
if FormInput.OpenDialogInput.Execute then
   if FormInput.OpenDialogInput.FileName<>'' then
      FormInput.Editinputquery.Caption:=stringdelim(FormInput.OpenDialogInput.FileName);
end;

initialization
  {$I unitinput.lrs}

end.

