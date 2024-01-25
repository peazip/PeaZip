{
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}

unit uDarkStyleSchemes;

interface

uses
  SysUtils,
  LCLType,LCLIntf,Graphics,Masks,
  LResources,ComCtrls,
  uDarkStyleParams,
  gmap,gutil;

const
  DSColorsTypeName='DARKSTYLECOLORS';

type
  TSchemeName=String;
  PTSchemeData=^TSchemeData;
  TSchemeData=record
    Name:TSchemeName;
    Data:TDSColors;
  end;
  TSchemeKey=String;
  TSchemes=class(specialize TMap<TSchemeKey,TSchemeData,specialize TLess<TSchemeKey>>)
    function GetMutableValue(key:TSchemeKey):PTSchemeData;
  end;


var
  DefaultDark,DefaultWhite:TDSColors;
  Schemes:TSchemes=nil;

function GetScheme(AName:TSchemeName):TDSColors;
function GetSchemeMutable(AName:TSchemeName):PTSchemeData;
procedure AddScheme(AName:TSchemeName;AData:TDSColors);
procedure LoadLResources;
procedure LoadPath(APath,AMask:string);

implementation
uses
  uDarkStyleSchemesLoader;

function TSchemes.GetMutableValue(key:TSchemeKey):PTSchemeData;
var
 Pair:TPair;
 Node:TMSet.PNode;
begin
 Pair.Key:=key;
 Node:=FSet.NFind(Pair);
 if Node=nil then
   result:=nil
 else
   result:=@Node^.Data.Value;
end;

function SchameName2SchameID(AName:TSchemeName):TSchemeKey;inline;
begin
  result:=UpperCase(AName);
end;

function GetSchemeMutable(AName:TSchemeName):PTSchemeData;
begin
  if Schemes=nil then
    exit(nil);
  result:=Schemes.GetMutableValue(SchameName2SchameID(AName));
end;

function GetScheme(AName:TSchemeName):TDSColors;
var
  ps:PTSchemeData;
  UCName:string;
begin
  UCName:=UpperCase(AName);
  if UCName='DARK' then
    result:=DefaultDark
  else if UCName='WHITE' then
    result:=DefaultWhite
  else begin
    ps:=GetSchemeMutable(AName);
    if ps=nil then
      result:=DefaultDark
    else
      result:=ps^.Data;
  end;
end;

function CreateTSchemeData(AName:TSchemeName;AData:TDSColors):TSchemeData;
begin
  result.Data:=AData;
  result.Name:=AName;
end;

procedure AddScheme(AName:TSchemeName;AData:TDSColors);
var
  id:TSchemeKey;
begin
  id:=SchameName2SchameID(AName);
  if Schemes=nil then begin
    Schemes:=TSchemes.Create;
    Schemes.Insert(id,CreateTSchemeData(AName,AData));
  end else begin
    if Schemes.GetMutableValue(id)=nil then
      Schemes.Insert(id,CreateTSchemeData(AName,AData));
  end;
end;

procedure LoadLResources;
var
  r:TLResource;
  DSC:TDSColors;
  i:integer;
begin
  for i:=0 to LazarusResources.Count-1 do begin
    r:=LazarusResources.Items[i];
    if UpperCase(r.ValueType)=DSColorsTypeName then
      if GetSchemeMutable(r.Value)=nil then
        if ParseColors(r.Name,r.Value,DSC) then
          AddScheme(r.Name,DSC);
  end;
end;

procedure LoadPath(APath,AMask:string);
var
  DSC:TDSColors;
  sr: TSearchRec;
begin
  if FindFirst(APath+'/*',faAnyFile,sr) = 0 then begin
    repeat
      if (sr.Name <> '.') and (sr.Name <> '..') then begin
        if MatchesMask(sr.Name,AMask) then
          if ParseColorsFile(APath+'/'+sr.Name,DSC) then
            AddScheme(ChangeFileExt(sr.Name,''),DSC);
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure InitializeDefaultColors;
begin
  DefaultDark.SysColor[COLOR_SCROLLBAR]:=                RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_BACKGROUND]:=               RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_ACTIVECAPTION]:=            RGBToColor(42, 130, 218);
  DefaultDark.SysColor[COLOR_INACTIVECAPTION]:=          RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_MENU]:=                     RGBToColor(42, 42, 42);
  DefaultDark.SysColor[COLOR_WINDOW]:=                   RGBToColor(42, 42, 42);
  DefaultDark.SysColor[COLOR_WINDOWFRAME]:=              RGBToColor(20, 20, 20);
  DefaultDark.SysColor[COLOR_MENUTEXT]:=                 RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_WINDOWTEXT]:=               RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_CAPTIONTEXT]:=              RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_ACTIVEBORDER] :=            RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_INACTIVEBORDER]:=           RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_APPWORKSPACE]:=             RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_HIGHLIGHT]:=                RGBToColor(42, 130, 218);
  DefaultDark.SysColor[COLOR_HIGHLIGHTTEXT]:=            RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_BTNFACE]:=                  RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_BTNSHADOW]:=                RGBToColor(35, 35, 35);
  DefaultDark.SysColor[COLOR_GRAYTEXT]:=                 RGBToColor(160, 160, 160);
  DefaultDark.SysColor[COLOR_BTNTEXT]:=                  RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_INACTIVECAPTIONTEXT]:=      RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_BTNHIGHLIGHT]:=             RGBToColor(66, 66, 66);
  DefaultDark.SysColor[COLOR_3DDKSHADOW]:=               RGBToColor(20, 20, 20);
  DefaultDark.SysColor[COLOR_3DLIGHT]:=                  RGBToColor(40, 40, 40);
  DefaultDark.SysColor[COLOR_INFOTEXT]:=                 RGBToColor(53, 53, 53);
  DefaultDark.SysColor[COLOR_INFOBK]:=                   RGBToColor(245, 245, 245);
  DefaultDark.SysColor[COLOR_HOTLIGHT]:=                 RGBToColor(66, 66, 66);
  DefaultDark.SysColor[COLOR_GRADIENTACTIVECAPTION]:=    GetSysColor(COLOR_GRADIENTACTIVECAPTION);
  DefaultDark.SysColor[COLOR_GRADIENTINACTIVECAPTION]:=  GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
  DefaultDark.SysColor[COLOR_MENUHILIGHT]:=              RGBToColor(66, 66, 66);
  DefaultDark.SysColor[COLOR_MENUBAR]:=                  RGBToColor(42, 42, 42);
  DefaultDark.SysColor[COLOR_FORM]:=                     RGBToColor(53, 53, 53);
  DefaultDark.DrawControl.CustomDrawScrollbars:=         False;
  DefaultDark.DrawControl.TreeViewDisableHideSelection:= False;
  DefaultDark.DrawControl.TreeViewExpandSignOverride:=   False;
  DefaultDark.DrawControl.TreeViewExpandSignValue:=      tvestTheme;
  DefaultDark.DrawControl.CustomDrawPushButtons:=        False;
  DefaultDark.DrawControl.CustomDrawComboBoxs:=          False;
  DefaultDark.DrawControl.CustomDrawTreeViews:=          False;

  DefaultWhite.SysColor[COLOR_SCROLLBAR]:=               GetSysColor(COLOR_SCROLLBAR);
  DefaultWhite.SysColor[COLOR_BACKGROUND]:=              GetSysColor(COLOR_BACKGROUND);
  DefaultWhite.SysColor[COLOR_ACTIVECAPTION]:=           GetSysColor(COLOR_ACTIVECAPTION);
  DefaultWhite.SysColor[COLOR_INACTIVECAPTION]:=         GetSysColor(COLOR_INACTIVECAPTION);
  DefaultWhite.SysColor[COLOR_MENU]:=                    GetSysColor(COLOR_MENU);
  DefaultWhite.SysColor[COLOR_WINDOW]:=                  GetSysColor(COLOR_WINDOW);
  DefaultWhite.SysColor[COLOR_WINDOWFRAME]:=             GetSysColor(COLOR_WINDOWFRAME);
  DefaultWhite.SysColor[COLOR_MENUTEXT]:=                GetSysColor(COLOR_MENUTEXT);
  DefaultWhite.SysColor[COLOR_WINDOWTEXT]:=              GetSysColor(COLOR_WINDOWTEXT);
  DefaultWhite.SysColor[COLOR_CAPTIONTEXT]:=             GetSysColor(COLOR_CAPTIONTEXT);
  DefaultWhite.SysColor[COLOR_ACTIVEBORDER] :=           GetSysColor(COLOR_ACTIVEBORDER);
  DefaultWhite.SysColor[COLOR_INACTIVEBORDER]:=          GetSysColor(COLOR_INACTIVEBORDER);
  DefaultWhite.SysColor[COLOR_APPWORKSPACE]:=            GetSysColor(COLOR_APPWORKSPACE);
  DefaultWhite.SysColor[COLOR_HIGHLIGHT]:=               GetSysColor(COLOR_HIGHLIGHT);
  DefaultWhite.SysColor[COLOR_HIGHLIGHTTEXT]:=           GetSysColor(COLOR_HIGHLIGHTTEXT);
  DefaultWhite.SysColor[COLOR_BTNFACE]:=                 GetSysColor(COLOR_BTNFACE);
  DefaultWhite.SysColor[COLOR_BTNSHADOW]:=               GetSysColor(COLOR_BTNSHADOW);
  DefaultWhite.SysColor[COLOR_GRAYTEXT]:=                GetSysColor(COLOR_GRAYTEXT);
  DefaultWhite.SysColor[COLOR_BTNTEXT]:=                 GetSysColor(COLOR_BTNTEXT);
  DefaultWhite.SysColor[COLOR_INACTIVECAPTIONTEXT]:=     GetSysColor(COLOR_INACTIVECAPTIONTEXT);
  DefaultWhite.SysColor[COLOR_BTNHIGHLIGHT]:=            GetSysColor(COLOR_BTNHIGHLIGHT);
  DefaultWhite.SysColor[COLOR_3DDKSHADOW]:=              GetSysColor(COLOR_3DDKSHADOW);
  DefaultWhite.SysColor[COLOR_3DLIGHT]:=                 GetSysColor(COLOR_3DLIGHT);
  DefaultWhite.SysColor[COLOR_INFOTEXT]:=                GetSysColor(COLOR_INFOTEXT);
  DefaultWhite.SysColor[COLOR_INFOBK]:=                  GetSysColor(COLOR_INFOBK);
  DefaultWhite.SysColor[COLOR_HOTLIGHT]:=                GetSysColor(COLOR_HOTLIGHT);
  DefaultWhite.SysColor[COLOR_GRADIENTACTIVECAPTION]:=   GetSysColor(COLOR_GRADIENTACTIVECAPTION);
  DefaultWhite.SysColor[COLOR_GRADIENTINACTIVECAPTION]:= GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
  DefaultWhite.SysColor[COLOR_MENUHILIGHT]:=             GetSysColor(COLOR_MENUHILIGHT);
  DefaultWhite.SysColor[COLOR_MENUBAR]:=                 GetSysColor(COLOR_MENUBAR);
  DefaultWhite.SysColor[COLOR_FORM]:=                    GetSysColor(COLOR_FORM);
  DefaultWhite.DrawControl.CustomDrawScrollbars:=        True;
  DefaultWhite.DrawControl.TreeViewDisableHideSelection:=False;
  DefaultWhite.DrawControl.TreeViewExpandSignOverride:=  False;
  DefaultWhite.DrawControl.TreeViewExpandSignValue:=     tvestTheme;
  DefaultWhite.DrawControl.CustomDrawPushButtons:=       True;
  DefaultWhite.DrawControl.CustomDrawComboBoxs:=         True;
  DefaultWhite.DrawControl.CustomDrawTreeViews:=         True;
end;

initialization
  InitializeDefaultColors;
finalization
  if Schemes<>nil then
    Schemes.Destroy;
end.
