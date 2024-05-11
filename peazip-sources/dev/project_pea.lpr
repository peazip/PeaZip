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
  {$IFDEF MSWINDOWS}
  ,SysUtils,fileutil,umetadarkstyle,udarkstyleparams,udarkstyleschemes
  {$ENDIF};

{$IFDEF MSWINDOWS}
{$R pea.res}
{$R manifest.res}
{$ENDIF}

{$R *.res}

begin
  Application.Scaled:=True;
  {$IFDEF MSWINDOWS}
  if fileexists(Programdirectory+'light') then
     begin
     PreferredAppMode:=pamForceLight;
     umetadarkstyle.ApplyMetaDarkStyle(DefaultWhite);
     end
  else
     if fileexists(Programdirectory+'dark') then
        begin
        PreferredAppMode:=pamForceDark ;
        umetadarkstyle.ApplyMetaDarkStyle(DefaultDark);
        end
     else
        begin
        PreferredAppMode:=pamallowdark;
        umetadarkstyle.ApplyMetaDarkStyle(DefaultDark);
        end;
  {$ENDIF}
  Application.Title:='Pea';
  Application.Initialize;
  Application.CreateForm(TForm_pea, Form_pea);
  Application.CreateForm(TForm_report, Form_report);
  Application.Run;
end.

