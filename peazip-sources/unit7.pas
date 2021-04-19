unit Unit7;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, Buttons, ExtCtrls;

type

  { TFormDlg }

  TFormDlg = class(TForm)
    babort: TBitBtn;
    bcancel: TBitBtn;
    bno: TBitBtn;
    bok: TBitBtn;
    byes: TBitBtn;
    byesall: TBitBtn;
    ImageDlg: TImage;
    ldlg: TLabel;
    Panelal: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormDlg: TFormDlg;
  txt_7_8_update,txt_info,txt_7_2_updateclear,txt_6_5_warning,txt_6_5_error,txt_2_7_ok,txt_6_5_yesall,txt_6_5_yes,txt_no,txt_2_7_cancel,txt_6_5_abort,txt_clear:ansistring;
  binfodlg,bwarningdlg,berrordlg:TBitmap;

function pMessageDlg(const aMsg, aCaption: ansistring; btn1,btn2a,btn2,btn3,btn4,btn5: boolean): TModalResult;

function pMessageErrorOK(s:ansistring): TModalResult;
function pMessageWarningOK(s:ansistring): TModalResult;
function pMessageInfoOK(s:ansistring): TModalResult;

function pMessageErrorYesNo(s:ansistring): TModalResult;
function pMessageWarningYesNo(s:ansistring): TModalResult;
function pMessageInfoYesNo(s:ansistring): TModalResult;

function pMessageWarningYesNoCancel(s:ansistring): TModalResult;
function pMessageInfoYesNoCancel(s:ansistring): TModalResult;

function pMessageWarningOKAbort(s:ansistring): TModalResult;

function pMessageInfoAllYesNoCancel(s:ansistring): TModalResult;

function pMessagePreview(s:ansistring): TModalResult;
function pMessageNamingConflict(s:ansistring): TModalResult;

implementation

function pMessageDlg(const aMsg, aCaption: ansistring; btn1,btn2a,btn2,btn3,btn4,btn5: boolean): TModalResult;
begin
FormDlg.Caption:=aCaption;
FormDlg.ImageDlg.Transparent:=true;

if aCaption = txt_info then FormDlg.ImageDlg.Picture.Bitmap:=binfodlg
   else
      if aCaption = txt_6_5_warning then FormDlg.ImageDlg.Picture.Bitmap:=bwarningdlg
      else FormDlg.ImageDlg.Picture.Bitmap:=berrordlg;

FormDlg.ldlg.Caption:=aMsg;

FormDlg.bok.Caption:=txt_2_7_ok;
FormDlg.byesall.Caption:=txt_6_5_yesall;
FormDlg.byes.Caption:=txt_6_5_yes;
FormDlg.bno.Caption:=txt_no;
FormDlg.bcancel.Caption:=txt_2_7_cancel;
FormDlg.babort.Caption:=txt_6_5_abort;

FormDlg.bok.visible:=btn1;
FormDlg.byesall.visible:=btn2a;
FormDlg.byes.visible:=btn2;
FormDlg.bno.visible:=btn3;
FormDlg.bcancel.visible:=btn4;
FormDlg.babort.visible:=btn5;

result:=formdlg.showmodal;
end;

function pMessageErrorOK(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_error, true, false, false, false, false, false);
end;

function pMessageWarningOK(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_warning, true, false, false, false, false, false);
end;

function pMessageInfoOK(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_info, true, false, false, false, false, false);
end;

function pMessageErrorYesNo(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_warning, false, false, true, true, false, false);
end;

function pMessageWarningYesNo(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_warning, false, false, true, true, false, false);
end;

function pMessageInfoYesNo(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_info, false, false, true, true, false, false);
end;

function pMessageWarningYesNoCancel(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_warning, false, false, true, true, true, false);
end;

function pMessageInfoYesNoCancel(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_info, false, false, true, true, true, false);
end;

function pMessageWarningOKAbort(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_6_5_warning, true, false, false, false, false, true);
end;

function pMessageInfoAllYesNoCancel(s:ansistring): TModalResult;
begin
result:=pMessageDlg(s, txt_info, false, true, true, true, true, false);
end;

function pMessagePreview(s:ansistring): TModalResult;
var stemp:ansistring;
begin
stemp:=txt_2_7_ok;
txt_2_7_ok:=txt_6_5_yes+' / '+txt_clear;
FormDlg.bok.hint:=txt_7_2_updateclear;
result:=pMessageDlg(s, txt_info, true, false, true, true, false, false);
txt_2_7_ok:=stemp;
FormDlg.bok.hint:='';
end;

function pMessageNamingConflict(s:ansistring): TModalResult;
var stemp:ansistring;
begin
stemp:=txt_2_7_ok;
txt_2_7_ok:=txt_7_8_update;
FormDlg.bok.hint:=txt_7_2_updateclear;
result:=pMessageDlg(s, txt_info, true, false, true, true, true, false);
txt_2_7_ok:=stemp;
FormDlg.bok.hint:='';
end;

initialization
  {$I unit7.lrs}

end.

