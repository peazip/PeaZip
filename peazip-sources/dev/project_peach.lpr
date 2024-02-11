program project_peach;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  peach, Unit3, Unit5, Unit6, Unit2, Unit1, Unit8, Unit9, Unit10,
  Unit11, Unit12, Unit13, Unit7, Unit_gwrap, Unit14
  {$IFDEF MSWINDOWS}//W10+ dark mode
  ,umetadarkstyle,udarkstyleparams,udarkstyleschemes
  {$ENDIF};

{$IFDEF MSWINDOWS}
{$R peazip.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
Application.Scaled:=True;
{$IFDEF MSWINDOWS}//W10+ dark mode
PreferredAppMode:=pamallowdark;
umetadarkstyle.ApplyMetaDarkStyle(DefaultDark);
{$ENDIF}
Application.Title:='PeaZip';
Application.Initialize;
Application.CreateForm(TForm_peach, Form_peach);
{$IFDEF MSWINDOWS}
Application.MainFormOnTaskBar:=true;
{$ENDIF}
Application.CreateForm(TFormDrop, FormDrop);
Application.CreateForm(TFormInput, FormInput);
Application.CreateForm(TFormPW, FormPW);
Application.CreateForm(TFormPM, FormPM);
Application.CreateForm(TFormKF, FormKF);
Application.CreateForm(TFormAdvf, FormAdvf);
Application.CreateForm(TFormWeb, FormWeb);
Application.CreateForm(TFormImgRes, FormImgRes);
Application.CreateForm(TFormCrop, FormCrop);
Application.CreateForm(TFormPaths, FormPaths);
Application.CreateForm(TFormSelect, FormSelect);
Application.CreateForm(TFormDlg, FormDlg);
Application.CreateForm(TForm_gwrap, Form_gwrap);
Application.CreateForm(TFormComment, FormComment);
Application.Run;
end.

