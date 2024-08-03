unit DragDropDesign;
// TODO : Default event for target components should be OnDrop.
// TODO : Add parent form to Target property editor list.
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDropDesign
// Description:     Contains design-time support for the drag and drop
//                  components.
// Version:         5.2
// Date:            17-AUG-2010
// Target:          Win32, Delphi 5-2010
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//
// Lazarus adaption 10/2017 Michael Köcher / six1
// -----------------------------------------------------------------------------

interface

{$R *.dcr}

procedure Register;

implementation

uses
  DragDrop,
  DropSource,
  DropTarget,
  DragDropFile,
  DragDropGraphics,
  DragDropContext,
  DragDropHandler,
  DropHandler,
  DragDropInternet,
  DragDropPIDL,
  DragDropText,
  DropComboTarget
  , Classes
  , PropEdits
  , LResources
  ;


type
  TDataFormatNameEditor = class( TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues( Proc: TGetStrProc); override;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              Component and Design-time editor registration
//
////////////////////////////////////////////////////////////////////////////////
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TDataFormatAdapter, 'DataFormatName',
    TDataFormatNameEditor);
  RegisterComponents(DragDropComponentPalettePage,
    [TDropEmptySource, TDropEmptyTarget, TDropDummy, TDataFormatAdapter,
    TDropFileTarget, TDropFileSource, TDropBMPTarget, TDropBMPSource,
    TDropMetaFileTarget, TDropImageTarget, TDropURLTarget, TDropURLSource,
    TDropPIDLTarget, TDropPIDLSource, TDropTextTarget, TDropTextSource,
    TDropComboTarget]);
  RegisterComponents(DragDropComponentPalettePage,
    [TDropHandler, TDragDropHandler, TDropContextMenu]);
end;

{ TDataFormatNameEditor }

function TDataFormatNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDataFormatNameEditor.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  try
    for i := 0 to TDataFormatClasses.Count-1 do
      Proc( TDataFormatClasses.Formats[i].ClassName);
  except
  end;
end;

initialization
  {$I DragDropDesign.lrs}

end.
