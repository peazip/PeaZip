unit Unit11;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TFormDrop }

  TFormDrop = class(TForm)
    Imagedragfile: TImage;
    Imagedragfolder: TImage;
    Imagedragop: TImage;
    Labeldragfile: TLabel;
    Labeldragfolder: TLabel;
    Labeldragtitle: TLabel;
    PanelDrag: TPanel;
    Shape1: TShape;
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
  {$I unit11.lrs}

end.

