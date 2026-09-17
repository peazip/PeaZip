unit UnitImgCrop; //Form to crop graphic image files selected in the main form file browser

{$mode objfpc}{$H+}

interface

uses
  UnitDlg, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ButtonPanel, StdCtrls, Spin;

type

  { TFormImgCrop }

  TFormImgCrop = class(TForm)
    ButtonPanelImgCrop: TButtonPanel;
    CheckBoxPercent: TCheckBox;
    ImageInfoImgCrop: TImage;
    LabelB: TLabel;
    LabelL: TLabel;
    LabelR: TLabel;
    LabelT: TLabel;
    PanelCrop: TPanel;
    SpinEditB: TSpinEdit;
    SpinEditL: TSpinEdit;
    SpinEditR: TSpinEdit;
    SpinEditT: TSpinEdit;
    procedure CheckBoxPercentChange(Sender: TObject);
    procedure ImageInfoImgCropClick(Sender: TObject);
    procedure SpinEditLChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormImgCrop: TFormImgCrop;

implementation

{ TFormImgCrop }

{ TFormImgCrop }

procedure TFormImgCrop.CheckBoxPercentChange(Sender: TObject);
begin
  if CheckBoxPercent.Checked=true then
     begin
     SpinEditL.Value:=10;
     SpinEditR.Value:=10;
     SpinEditT.Value:=10;
     SpinEditB.Value:=10;
     end
  else
     begin
     SpinEditL.Value:=300;
     SpinEditR.Value:=300;
     SpinEditT.Value:=300;
     SpinEditB.Value:=300;
     end;
end;

procedure TFormImgCrop.ImageInfoImgCropClick(Sender: TObject);
begin
pMessageInfoOK(ImageInfoImgCrop.Hint);
end;

procedure percentsetcrop(dim:integer);
begin
with FormImgCrop do
begin
if CheckBoxPercent.Checked=true then
   case dim of
   1: if SpinEditL.Value+SpinEditR.Value>=100 then SpinEditR.Value:=99-SpinEditL.Value;
   end;
end;
end;

procedure TFormImgCrop.SpinEditLChange(Sender: TObject);
begin
percentsetcrop(1);
end;

initialization
  {$I unitimgcrop.lrs}

end.

