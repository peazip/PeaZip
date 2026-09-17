unit UnitPaths; //Form to display and copy paths of items selected in the main form file browser

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ActnList;

type

  { TFormPaths }

  TFormPaths = class(TForm)
    ActionathsExit: TAction;
    ActionListPaths: TActionList;
    ButtonPanelPaths: TButtonPanel;
    MemoPaths: TMemo;
    procedure ActionathsExitExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormPaths: TFormPaths;

implementation

{ TFormPaths }

procedure TFormPaths.ActionathsExitExecute(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

initialization
  {$I unitpaths.lrs}

end.

