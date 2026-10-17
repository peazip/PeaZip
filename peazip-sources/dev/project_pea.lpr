program project_pea;

{$mode objfpc}{$H+}{$R+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, 
  pea, pea_utils, list_utils, UnitReport
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
  Application.CreateForm(TFormPea, FormPea);
  Application.CreateForm(TFormReport, FormReport);
  Application.Run;
end.

