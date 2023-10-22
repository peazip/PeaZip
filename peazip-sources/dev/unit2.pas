unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, Buttons, StdCtrls, ExtCtrls;

type

  { TFormWeb }

  TFormWeb = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    Editinputquery: TEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
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
for i:=0 to 17 do FormWeb.CheckGroup1.Checked[i]:=false;
end;

procedure TFormWeb.CheckBox1Click(Sender: TObject);
var
   i,j:integer;
begin
if CheckBox1.State=cbUnchecked then
   begin
   j:=10;
   for i:=0 to 17 do
      if FormWeb.CheckGroup1.Checked[i]=true then
      begin
      j:=i;
      break;
      end;
   clearcb;
   CheckGroup1.Checked[j]:=true;
   end;
end;

procedure TFormWeb.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
if CheckBox1.State=cbUnchecked then
   begin
   clearcb;
   CheckGroup1.Checked[index]:=true;
   end;
end;

initialization
  {$I unit2.lrs}

end.

