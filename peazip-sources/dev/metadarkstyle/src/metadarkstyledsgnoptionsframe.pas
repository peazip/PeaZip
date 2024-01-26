unit MetaDarkStyleDSGNOptionsFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  IDEOptionsIntf,IDEOptEditorIntf,
  MetaDarkStyleDSGNOptions,uDarkStyleSchemes;

resourceString
  RSDarkStyleDSGNOptionsFrame='Dark style';


type

  { TDarkStyleDSGNOptionsFrame }

  TDarkStyleDSGNOptionsFrame = class(TAbstractIDEOptionsEditor)
    PAMComboBox: TComboBox;
    CSComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
  private

  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

function TDarkStyleDSGNOptionsFrame.GetTitle: String;
begin
  result:=RSDarkStyleDSGNOptionsFrame;
end;

procedure SchemeToComboSet(ASch:string;ACombo:TComboBox;curr:integer);
begin
  if ASch=MetaDarkStyleDSGNOpt.ColorScheme then
    ACombo.ItemIndex:=curr;
end;

procedure TDarkStyleDSGNOptionsFrame.Setup({%H-}ADialog: TAbstractOptionsEditorDialog);
var
  i:TAppModeOpt;
  itr:TSchemes.TIterator;
begin
  PAMComboBox.Items.Clear;
  for i:=low(AppModeOptStr) to high(AppModeOptStr) do
    PAMComboBox.Items.Add(AppModeOptLocalizedStr[i]);
  CSComboBox.Items.Clear;
  CSComboBox.Items.Add('Dark');
  CSComboBox.Items.Add('White');
  if Schemes<>nil then begin
    itr:=Schemes.Min;
    if itr<>nil then repeat
      CSComboBox.Items.Add(itr.Data.Value.Name);
    until not itr.Next;
    itr.free;
  end;
end;

procedure TDarkStyleDSGNOptionsFrame.ReadSettings({%H-}AOptions: TAbstractIDEOptions);
begin
  RestoreSettings(AOptions);
end;

procedure TDarkStyleDSGNOptionsFrame.WriteSettings({%H-}AOptions: TAbstractIDEOptions);
begin
  MetaDarkStyleDSGNOpt.AppMode:=TAppModeOpt(PAMComboBox.ItemIndex);
  MetaDarkStyleDSGNOpt.ColorScheme:=CSComboBox.Items[CSComboBox.ItemIndex];
  if MetaDarkStyleDSGNOpt.Modified then
    MetaDarkStyleDSGNOpt.SaveSafe;
end;

procedure TDarkStyleDSGNOptionsFrame.RestoreSettings({%H-}AOptions: TAbstractIDEOptions);
var
  itr:TSchemes.TIterator;
  i:integer;
begin
  PAMComboBox.ItemIndex:=ord(MetaDarkStyleDSGNOpt.AppMode);
  SchemeToComboSet('Dark',CSComboBox,0);
  SchemeToComboSet('White',CSComboBox,1);
  if Schemes<>nil then begin
    itr:=Schemes.Min;
    i:=2;
    if itr<>nil then repeat
      SchemeToComboSet(itr.Data.Value.Name,CSComboBox,i);
      inc(i);
    until not itr.Next;
    itr.free;
  end;
end;

class function TDarkStyleDSGNOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

