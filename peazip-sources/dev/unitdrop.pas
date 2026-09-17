unit UnitDrop; //Custom drag and drop floater form

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TFormDrop }

  TFormDrop = class(TForm)
    ImageDragFile: TImage;
    ImageDragFolder: TImage;
    ImageDragOp: TImage;
    LabelDragFile: TLabel;
    LabelDragFolder: TLabel;
    LabelDragTitle: TLabel;
    PanelDragDrop: TPanel;
    ShapeDragDrop: TShape;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDrop: TFormDrop;

implementation

{ TFormDrop }

initialization
  {$I unitdrop.lrs}

end.

