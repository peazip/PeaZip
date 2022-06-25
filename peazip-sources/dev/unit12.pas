unit Unit12;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFormPaths }

  TFormPaths = class(TForm)
    MemoPaths: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormPaths: TFormPaths;

implementation

{ TFormPaths }

initialization
  {$I unit12.lrs}

end.

