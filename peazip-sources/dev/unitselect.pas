unit UnitSelect; //Form to apply advanced selection filter on the main form's file browser

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls;

type

  { TFormSelect }

  TFormSelect = class(TForm)
    ButtonPanelSelect: TButtonPanel;
    ComboBoxSelect1: TComboBox;
    ComboBoxSelect2: TComboBox;
    LabelSelectAll: TLabel;
    LabelSelectInvert: TLabel;
    LabelSelectSort: TLabel;
    LabelSelectSpacer: TLabel;
    PanelSelect: TPanel;
    procedure LabelSelectAllClick(Sender: TObject);
    procedure LabelSelectInvertClick(Sender: TObject);
    procedure LabelSelectSortClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSelect: TFormSelect;

implementation

{ TFormSelect }

procedure TFormSelect.LabelSelectAllClick(Sender: TObject);
begin
FormSelect.ModalResult:=mrAll;
end;

procedure TFormSelect.LabelSelectInvertClick(Sender: TObject);
begin
FormSelect.ModalResult:=mrNo;
end;

procedure TFormSelect.LabelSelectSortClick(Sender: TObject);
begin
FormSelect.ModalResult:=mrNoToAll;
end;

initialization
  {$I unitselect.lrs}

end.

