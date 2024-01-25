program project_pea;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, 
  Unit_pea, pea_utils, list_utils, Unit_report
  {$IfDef WINDOWS}, uMetaDarkStyle, uDarkStyleParams, uDarkStyleSchemes{$EndIf};

{$IFDEF MSWINDOWS}
{$R pea.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
  {$IfDef WINDOWS}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  PreferredAppMode:=pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$EndIf}
  Application.Scaled:=True;
  Application.Title:='Pea';
  Application.Initialize;
  Application.CreateForm(TForm_pea, Form_pea);
  Application.CreateForm(TForm_report, Form_report);
  Application.Run;
end.

