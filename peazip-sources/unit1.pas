unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Unit7, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls;

type

  { TFormAdvf }

  TFormAdvf = class(TForm)
    ButtonClearFilters: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBoxAdvFilters: TCheckBox;
    CheckBoxAdvRecurse: TCheckBox;
    CheckBoxAdvRecurse1: TCheckBox;
    CheckBoxAdvRecurseAlso: TCheckBox;
    ImageInfoArchive4: TImage;
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
    procedure ImageInfoArchive4Click(Sender: TObject);
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

procedure TFormAdvf.ImageInfoArchive4Click(Sender: TObject);
begin
pMessageInfoOK(ImageInfoArchive4.Hint);
end;

procedure TFormAdvf.ButtonClearFiltersClick(Sender: TObject);
begin
MemoAdvInclude.Clear;
MemoAdvIncludeAlso.Clear;
MemoAdvExclude.Clear;
end;

initialization
  {$I unit1.lrs}

end.

