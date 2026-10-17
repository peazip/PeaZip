unit UnitAdvf; //Form to apply advanced filters (multiple inclusion and exclusion strings) to formats supported through 7z/p7zip backend

{$mode objfpc}{$H+}{$R+}

interface

uses
  UnitDlg, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls;

type

  { TFormAdvf }

  TFormAdvf = class(TForm)
    ButtonClearFilters: TButton;
    ButtonPanelAdvf: TButtonPanel;
    CheckBoxAdvFilters: TCheckBox;
    CheckBoxAdvRecurse: TCheckBox;
    CheckBoxAdvRecurse1: TCheckBox;
    CheckBoxAdvRecurseAlso: TCheckBox;
    ImageInfoAdvf: TImage;
    LabelAdvExclude: TLabel;
    LabelAdvInclude: TLabel;
    LabelAdvIncludeAlso: TLabel;
    MemoAdvExclude: TMemo;
    MemoAdvInclude: TMemo;
    MemoAdvIncludeAlso: TMemo;
    PanelAdvf: TPanel;
    procedure ButtonClearFiltersClick(Sender: TObject);
    procedure CheckBoxAdvFiltersClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ImageInfoAdvfClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormAdvf: TFormAdvf;

procedure set_advfilters_enabled(enstat:boolean);

implementation

{ TFormAdvf }

procedure set_advfilters_enabled(enstat:boolean);
begin
with FormAdvf do
begin
MemoAdvExclude.Enabled:=enstat;
MemoAdvInclude.Enabled:=enstat;
MemoAdvIncludeAlso.Enabled:=enstat;
LabelAdvExclude.Enabled:=enstat;
LabelAdvInclude.Enabled:=enstat;
LabelAdvIncludeAlso.Enabled:=enstat;
CheckBoxAdvRecurse1.Enabled:=enstat;
CheckBoxAdvRecurse.Enabled:=enstat;
CheckBoxAdvRecurseAlso.Enabled:=enstat;
end;
end;

procedure TFormAdvf.CheckBoxAdvFiltersClick(Sender: TObject);
begin
if CheckBoxAdvFilters.state=cbChecked then set_advfilters_enabled(true)
else
  begin
  set_advfilters_enabled(false);
  FormAdvf.Close;
  FormAdvf.ModalResult:=mrCancel;
  end;
end;

procedure TFormAdvf.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
   i:integer;
begin
if MemoAdvInclude.Focused=true then
   begin
   for i := 0 to High(FileNames) do
      MemoAdvInclude.Append(FileNames[i]);
   exit;
   end;
if MemoAdvIncludeAlso.Focused=true then
   begin
   for i := 0 to High(FileNames) do
      MemoAdvIncludeAlso.Append(FileNames[i]);
   exit;
   end;
if MemoAdvExclude.Focused=true then
   begin
   for i := 0 to High(FileNames) do
      MemoAdvExclude.Append(FileNames[i]);
   exit;
   end;
end;

procedure TFormAdvf.ImageInfoAdvfClick(Sender: TObject);
begin
pMessageInfoOK(ImageInfoAdvf.Hint);
end;

procedure TFormAdvf.ButtonClearFiltersClick(Sender: TObject);
begin
MemoAdvInclude.Clear;
MemoAdvIncludeAlso.Clear;
MemoAdvExclude.Clear;
end;

initialization
  {$I unitadvf.lrs}

end.

