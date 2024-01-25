unit MetaDarkStyleDSGNOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazConfigStorage, LazFileUtils, LazFileCache,
  LCLProc, ComCtrls, Graphics,
  BaseIDEIntf;

const
  amOptAllowDarkName='Allow dark';
  amOptForceDarkName='Force dark';
  amOptForceLightName='Force light';
resourcestring
  RSamOptAllowDarkName=amOptAllowDarkName;
  RSamOptForceDarkName=amOptForceDarkName;
  RSamOptForceLightName=amOptForceLightName;

type
  TAppModeOpt=(amOptAllowDark,amOptForceDark,amOptForceLight);

const
  AppModeOptStr:array[TAppModeOpt] of String=(amOptAllowDarkName,amOptForceDarkName,amOptForceLightName);
  AppModeOptLocalizedStr:array[TAppModeOpt] of String=(RSamOptAllowDarkName,RSamOptForceDarkName,RSamOptForceLightName);

type

  TMetaDarkStyleDSGNOptions=class
  private
    const
      DefaultAppMode:TAppModeOpt=amOptAllowDark;
      DefaultColorScheme:String='Dark';
    var
      FAppMode:TAppModeOpt;
      FColorScheme:String;
      FChangeStamp:Integer;
      FLastSavedChangeStamp:Integer;
    procedure SetAppMode(AValue:TAppModeOpt);
    procedure SetColorScheme(AValue:String);
    function GetModified:Boolean;
    procedure SetModified(AValue: Boolean);
    function Str2AppModeOpt(str:string):TAppModeOpt;
  public
    constructor Create;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(AFilename:String);
    procedure LoadFromFile(AFilename:String);
    procedure IncreaseChangeStamp;
  public
    property ChangeStamp:Integer read FChangeStamp;
    property Modified:Boolean read GetModified write SetModified;

    property AppMode:TAppModeOpt read FAppMode write SetAppMode;
    property ColorScheme:String  read FColorScheme write SetColorScheme;
  end;

const
  MetaDarkStyleDSGNFileName='metadarkstyledsgnoptions.xml';

var
  MetaDarkStyleDSGNOpt: TMetaDarkStyleDSGNOptions = nil;

implementation

{ TDockedOptions }

function TMetaDarkStyleDSGNOptions.GetModified:Boolean;
begin
  Result:=FLastSavedChangeStamp<>FChangeStamp;
end;

procedure TMetaDarkStyleDSGNOptions.SetModified(AValue:Boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    FLastSavedChangeStamp:=FChangeStamp;
end;

procedure TMetaDarkStyleDSGNOptions.SetAppMode(AValue:TAppModeOpt);
begin
  if FAppMode=AValue then Exit;
  FAppMode:=AValue;
  IncreaseChangeStamp;
end;

procedure TMetaDarkStyleDSGNOptions.SetColorScheme(AValue:String);
begin
  if FColorScheme=AValue then Exit;
  FColorScheme:=AValue;
  IncreaseChangeStamp;
end;

constructor TMetaDarkStyleDSGNOptions.Create;
begin
  FAppMode:=DefaultAppMode;
  FChangeStamp:=LUInvalidChangeStamp+1;
  FLastSavedChangeStamp:=FChangeStamp;
end;

procedure TMetaDarkStyleDSGNOptions.SaveSafe;
begin
  try
    SaveToFile(MetaDarkStyleDSGNFileName);
    Modified:=False;
  except
    on E: Exception do
      DebugLn(['Error: (lazarus) [TMetaDarkStyleDSGNOptions.SaveSafe] ', E.Message]);
  end;
end;

procedure TMetaDarkStyleDSGNOptions.LoadSafe;
begin
  try
    LoadFromFile(MetaDarkStyleDSGNFileName);
  except
    on E: Exception do
      DebugLn(['Error: (lazarus) [TMetaDarkStyleDSGNOptions.LoadSafe] ', E.Message]);
  end;
  Modified:=False;
end;

procedure TMetaDarkStyleDSGNOptions.SaveToFile(AFilename: String);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(AFilename,False);
  try
    Cfg.SetDeleteValue('AppMode/Value',AppModeOptStr[AppMode],AppModeOptStr[DefaultAppMode]);
    Cfg.SetDeleteValue('ColorScheme/Value',ColorScheme,DefaultColorScheme);
  finally
    Cfg.Free;
  end;
end;

function TMetaDarkStyleDSGNOptions.Str2AppModeOpt(str:string):TAppModeOpt;
var
  i:TAppModeOpt;
begin
  for i:=low(AppModeOptStr) to high(AppModeOptStr) do
    if AppModeOptStr[i]=str then
      exit(i);
  result:=DefaultAppMode;
end;

procedure TMetaDarkStyleDSGNOptions.LoadFromFile(AFilename: String);
var
  Cfg: TConfigStorage;
begin
  Cfg := GetIDEConfigStorage(AFilename,True);
  try
    AppMode:= Str2AppModeOpt(Cfg.GetValue('AppMode/Value',AppModeOptStr[DefaultAppMode]));
    ColorScheme:= Cfg.GetValue('ColorScheme/Value',DefaultColorScheme);
  finally
    Cfg.Free;
  end;
end;

procedure TMetaDarkStyleDSGNOptions.IncreaseChangeStamp;
begin
  LUIncreaseChangeStamp(FChangeStamp);
end;

end.

