unit registerMetaDarkStyleDSGN;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf, lazconf,
  uDarkStyleParams, uDarkStyleSchemes, uMetaDarkStyle,
  MetaDarkStyleDSGNOptionsFrame, MetaDarkStyleDSGNOptions;

var
  MetaDarkStyleOptionsID: integer = 1000;


procedure Register;

implementation

function AppModeOpt2PreferredAppMode(am:TAppModeOpt):TPreferredAppMode;
begin
  case am of
    amOptAllowDark:result:=pamAllowDark;
    amOptForceDark:result:=pamForceDark;
   amOptForceLight:result:=pamForceLight;
  end;
end;

procedure SetDarkStyle;
begin
 {$IF DEFINED(MSWINDOWS)}
  LoadLResources;
  LoadPath(GetPrimaryConfigPath+'/userschemes','*.'+DSColorsTypeName);
  LoadPath(GetSecondaryConfigPath+'/userschemes','*.'+DSColorsTypeName);
  MetaDarkStyleDSGNOpt:=TMetaDarkStyleDSGNOptions.Create;
  MetaDarkStyleDSGNOpt.LoadSafe;
  PreferredAppMode:=AppModeOpt2PreferredAppMode(MetaDarkStyleDSGNOpt.AppMode);
  ApplyMetaDarkStyle(GetScheme(MetaDarkStyleDSGNOpt.ColorScheme));
 {$ENDIF}
end;

procedure Register;
begin
  MetaDarkStyleOptionsID:=RegisterIDEOptionsEditor(GroupEnvironment,
                                                   TDarkStyleDSGNOptionsFrame,
                                                   MetaDarkStyleOptionsID)^.Index;
end;

initialization
  AddBootHandler(libhEnvironmentOptionsLoaded,@SetDarkStyle);
end.

