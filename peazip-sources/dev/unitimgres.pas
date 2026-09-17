unit UnitImgRes; //Form to resize and convert graphic image files selected in the main form file browser

{$mode objfpc}{$H+}

interface

uses
  UnitDlg, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Spin, StdCtrls, ButtonPanel, ExtCtrls;

type

  { TFormImgRes }

  TFormImgRes = class(TForm)
    ButtonPanelImgRes: TButtonPanel;
    CheckBoxAspectRatio: TCheckBox;
    CheckBoxPercent: TCheckBox;
    ComboBoxAction: TComboBox;
    ComboBoxConvert: TComboBox;
    ImageInfoImgRes: TImage;
    LabelConvert: TLabel;
    LabelH: TLabel;
    LabelW: TLabel;
    PanelImgRes: TPanel;
    SpinEditH: TSpinEdit;
    SpinEditJ: TSpinEdit;
    SpinEditW: TSpinEdit;
    procedure CheckBoxAspectRatioClick(Sender: TObject);
    procedure CheckBoxPercentChange(Sender: TObject);
    procedure ComboBoxActionChange(Sender: TObject);
    procedure ComboBoxConvertChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageInfoImgResClick(Sender: TObject);
    procedure SpinEditHChange(Sender: TObject);
    procedure SpinEditWChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormImgRes: TFormImgRes;
  origw,origh:integer;
  editingw,editingh:boolean;

procedure setformaspect(setformparam:ansistring);

implementation

{ TFormImgRes }

procedure hideresize;
begin
with FormImgRes do
begin
LabelW.Visible:=false;
LabelH.Visible:=false;
SpinEditW.Visible:=false;
SpinEditH.Visible:=false;
CheckBoxPercent.Visible:=false;
CheckBoxAspectRatio.Visible:=false;
end;
end;

procedure showresize;
begin
with FormImgRes do
begin
LabelW.Visible:=true;
LabelH.Visible:=true;
SpinEditW.Visible:=true;
SpinEditH.Visible:=true;
CheckBoxPercent.Visible:=true;
CheckBoxAspectRatio.Visible:=true;
end;
end;

procedure setformaspect(setformparam:ansistring);
begin
with FormImgRes do
begin
if setformparam='action' then
   begin
   if ComboBoxAction.ItemIndex=0 then hideresize else showresize;
   if (ComboBoxAction.ItemIndex=2) or (ComboBoxAction.ItemIndex=3) then
      begin
      CheckBoxAspectRatio.Checked:=true;
      CheckBoxPercent.Checked:=false;
      CheckBoxAspectRatio.Enabled:=false;
      CheckBoxPercent.Enabled:=false;
      end
   else
      begin
      CheckBoxAspectRatio.Checked:=true;
      CheckBoxPercent.Checked:=false;
      CheckBoxAspectRatio.Enabled:=true;
      CheckBoxPercent.Enabled:=true;
      end;
   end;

if setformparam='percent' then
   if CheckBoxPercent.Checked=true then
      begin
      SpinEditW.Value:=100;
      SpinEditH.Value:=100;
      end
   else
      begin
      SpinEditW.Value:=1024;
      SpinEditH.Value:=768;
      end;

if setformparam='convert' then
   if ComboBoxConvert.ItemIndex=3 then SpinEditJ.Visible:=true else SpinEditJ.Visible:=false;
end;
end;

procedure TFormImgRes.ComboBoxConvertChange(Sender: TObject);
begin
setformaspect('convert');
end;

procedure TFormImgRes.FormCreate(Sender: TObject);
begin
origw:=1024;
origh:=768;
editingw:=false;
editingh:=false;
end;

procedure TFormImgRes.ImageInfoImgResClick(Sender: TObject);
begin
pMessageInfoOK(ImageInfoImgRes.Hint);
end;

procedure TFormImgRes.SpinEditHChange(Sender: TObject);
begin
if editingw=true then exit;
editingh:=true;
if CheckBoxAspectRatio.Checked=true then
   if CheckBoxPercent.Checked=true then
      SpinEditW.Value:=SpinEditH.Value
   else
      SpinEditW.Value:=(SpinEditH.Value*origw) div origh;
editingh:=false;
end;

procedure TFormImgRes.SpinEditWChange(Sender: TObject);
begin
if editingh=true then exit;
editingw:=true;
if CheckBoxAspectRatio.Checked=true then
   if CheckBoxPercent.Checked=true then
      SpinEditH.Value:=SpinEditW.Value
   else
      SpinEditH.Value:=(SpinEditW.Value*origh) div origw;
editingw:=false;
end;

procedure TFormImgRes.CheckBoxPercentChange(Sender: TObject);
begin
setformaspect('percent');
end;

procedure TFormImgRes.CheckBoxAspectRatioClick(Sender: TObject);
begin
if CheckBoxAspectRatio.Checked=true then
   if CheckBoxPercent.Checked=true then
      SpinEditH.Value:=SpinEditW.Value;
end;

procedure TFormImgRes.ComboBoxActionChange(Sender: TObject);
begin
setformaspect('action');
end;

initialization
  {$I unitimgres.lrs}

end.

