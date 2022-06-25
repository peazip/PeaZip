unit Unit13;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls;

type

  { TFormSelect }

  TFormSelect = class(TForm)
    ButtonPanel1: TButtonPanel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Labelspac2: TLabel;
    PanelSelect: TPanel;
    procedure Label1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSelect: TFormSelect;

implementation

{ TFormSelect }

procedure TFormSelect.Label1Click(Sender: TObject);
begin
FormSelect.ModalResult:=mrAll;
end;

procedure TFormSelect.Label3Click(Sender: TObject);
begin
FormSelect.ModalResult:=mrNo;
end;

procedure TFormSelect.Label5Click(Sender: TObject);
begin
FormSelect.ModalResult:=mrNoToAll;
end;

initialization
  {$I unit13.lrs}

end.

