unit UnitWeb; //Form to serach string (filename of selected item in the main form file browser) on various web search engines

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, Buttons, StdCtrls, ExtCtrls;

type

  { TFormWeb }

  TFormWeb = class(TForm)
    ButtonPanelWeb: TButtonPanel;
    CheckBoxMultiWeb: TCheckBox;
    CheckGroupWeb: TCheckGroup;
    EditInputQueryWeb: TEdit;
    procedure CheckBoxMultiWebClick(Sender: TObject);
    procedure CheckGroupWebItemClick(Sender: TObject; Index: integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormWeb: TFormWeb;

implementation

{ TFormWeb }

procedure clearcb;
var
   i:integer;
begin
for i:=0 to 17 do FormWeb.CheckGroupWeb.Checked[i]:=false;
end;

procedure TFormWeb.CheckBoxMultiWebClick(Sender: TObject);
var
   i,j:integer;
begin
if CheckBoxMultiWeb.State=cbUnchecked then
   begin
   j:=10;
   for i:=0 to 17 do
      if FormWeb.CheckGroupWeb.Checked[i]=true then
      begin
      j:=i;
      break;
      end;
   clearcb;
   CheckGroupWeb.Checked[j]:=true;
   end;
end;

procedure TFormWeb.CheckGroupWebItemClick(Sender: TObject; Index: integer);
begin
if CheckBoxMultiWeb.State=cbUnchecked then
   begin
   clearcb;
   CheckGroupWeb.Checked[index]:=true;
   end;
end;

initialization
  {$I unitweb.lrs}

end.

