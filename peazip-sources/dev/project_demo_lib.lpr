program project_demo_lib;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here },
  Unit_demo_lib, unit_pea, list_utils, unit_report;

begin
  Application.Title:='demo_lib';
  Application.Initialize;
  Application.CreateForm(TForm_demo_lib, Form_demo_lib);
  Application.CreateForm(TForm_pea, Form_pea);
  Application.CreateForm(TForm_report, Form_report);
  Application.Run;
end.

