{
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}

unit uDarkStyleSchemesLoader;

interface

uses
  SysUtils,Classes,contnrs,bufstream,
  LCLProc,LCLType,LCLIntf,Graphics,LCLVersion,
  LResources,ComCtrls,
  PScanner, PParser, PasTree,
  uDarkStyleParams,uDarkStyleSchemes;

function ParseColors(modulename,module:string;out DSC:TDSColors):Boolean;overload;
function ParseColors(modulename:string;module:TStream;out DSC:TDSColors):Boolean;overload;
function ParseColorsFile(AFile:string;out DSC:TDSColors):Boolean;overload;

implementation

type
  TIdent=(IdUnknown,
          IdColors,
          IdScheme,
          IdDefaultDark,IdDefaultWhite,
          IdRGBToColor,IdGetSysColor,
          IdTreeViewDisableHideSelection,IdTreeViewExpandSignOverride,IdTreeViewExpandSignValue,
          IdtvestTheme,IdtvestPlusMinus,IdtvestArrow,IdtvestArrowFill,IdtvestAngleBracket,
          IdCustomDrawScrollbars,
          IdCustomDrawPushButtons,
          IdCustomDrawComboBoxs,
          IdCustomDrawTreeViews,
          IdCOLOR_SCROLLBAR,
          IdCOLOR_BACKGROUND,
          IdCOLOR_ACTIVECAPTION,
          IdCOLOR_INACTIVECAPTION,
          IdCOLOR_MENU,
          IdCOLOR_WINDOW,
          IdCOLOR_WINDOWFRAME,
          IdCOLOR_MENUTEXT,
          IdCOLOR_WINDOWTEXT,
          IdCOLOR_CAPTIONTEXT,
          IdCOLOR_ACTIVEBORDER,
          IdCOLOR_INACTIVEBORDER,
          IdCOLOR_APPWORKSPACE,
          IdCOLOR_HIGHLIGHT,
          IdCOLOR_HIGHLIGHTTEXT,
          IdCOLOR_BTNFACE,
          IdCOLOR_BTNSHADOW,
          IdCOLOR_GRAYTEXT,
          IdCOLOR_BTNTEXT,
          IdCOLOR_INACTIVECAPTIONTEXT,
          IdCOLOR_BTNHIGHLIGHT,
          IdCOLOR_3DDKSHADOW,
          IdCOLOR_3DLIGHT,
          IdCOLOR_INFOTEXT,
          IdCOLOR_INFOBK,
          IdCOLOR_25,
          IdCOLOR_HOTLIGHT,
          IdCOLOR_GRADIENTACTIVECAPTION,
          IdCOLOR_GRADIENTINACTIVECAPTION,
          IdCOLOR_MENUHILIGHT,
          IdCOLOR_MENUBAR,
          IdCOLOR_FORM);
const
  TIdents2Name:array[TIdent] of string=(
  '',
  'COLORS',
  'SCHEME',
  'DEFAULTDARK','DEFAULTWHITE',
  'RGBTOCOLOR','GETSYSCOLOR',
  'TREEVIEWDISABLEHIDESELECTION','TREEVIEWEXPANDSIGNOVERRIDE','TREEVIEWEXPANDSIGNVALUE',
  'TVESTTHEME','TVESTPLUSMINUS','TVESTARROW','TVESTARROWFILL','TVESTANGLEBRACKET',
  'CUSTOMDRAWSCROLLBARS',
  'CUSTOMDRAWPUSHBUTTONS',
  'CUSTOMDRAWCOMBOBOXS',
  'CUSTOMDRAWTREEVIEWS',
  'COLOR_SCROLLBAR',
  'COLOR_BACKGROUND',
  'COLOR_ACTIVECAPTION',
  'COLOR_INACTIVECAPTION',
  'COLOR_MENU',
  'COLOR_WINDOW',
  'COLOR_WINDOWFRAME',
  'COLOR_MENUTEXT',
  'COLOR_WINDOWTEXT',
  'COLOR_CAPTIONTEXT',
  'COLOR_ACTIVEBORDER',
  'COLOR_INACTIVEBORDER',
  'COLOR_APPWORKSPACE',
  'COLOR_HIGHLIGHT',
  'COLOR_HIGHLIGHTTEXT',
  'COLOR_BTNFACE',
  'COLOR_BTNSHADOW',
  'COLOR_GRAYTEXT',
  'COLOR_BTNTEXT',
  'COLOR_INACTIVECAPTIONTEXT',
  'COLOR_BTNHIGHLIGHT',
  'COLOR_3DDKSHADOW',
  'COLOR_3DLIGHT',
  'COLOR_INFOTEXT',
  'COLOR_INFOBK',
  'COLOR_25',
  'COLOR_HOTLIGHT',
  'COLOR_GRADIENTACTIVECAPTION',
  'COLOR_GRADIENTINACTIVECAPTION',
  'COLOR_MENUHILIGHT',
  'COLOR_MENUBAR',
  'COLOR_FORM'
  );

type
  TSimpleEngine = class(TPasTreeContainer)
    private
      uname:string;
      FElements:TObjectList;
    public

    constructor Create;
    destructor Destroy;override;
    Procedure Log(Sender : TObject; Const Msg : String);

    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  TOnMemoryStream = class(TCustomMemoryStream)
    private
      FReadOnly: Boolean;
    protected
      procedure SetSize(NewSize: Longint); override;
    public
      constructor Create(Ptr: Pointer; ASize: Longint; ReadOnlyMode: Boolean = True);
      function Write(const Buffer; Count: Longint): Longint; override;
      property ReadOnly: Boolean read FReadOnly write FReadOnly;
    end;

  constructor TOnMemoryStream.Create(Ptr: Pointer; ASize: Longint; ReadOnlyMode: Boolean = True);
  begin
    inherited Create;
    SetPointer(Ptr, ASize);
    FReadOnly := ReadOnlyMode;
  end;
  {------------------------------------------------------------------------------}
  function TOnMemoryStream.Write(const Buffer; Count: Longint): Longint;
  var
    Pos: Longint;
  begin
    if (Position >= 0) and (Count >= 0) and (not ReadOnly) and (Position + Count <=Size) then
      begin
        Pos := Position + Count;
        System.Move(Buffer, Pointer(NativeUInt(Memory) + NativeUInt(Position))^, Count);
        Position := Pos;
        Result := Count;
      end
    else
      Result := 0;
  end;
  {------------------------------------------------------------------------------}
  procedure TOnMemoryStream.SetSize(NewSize: Longint);
  begin
  //ничего не делаем
  end;

destructor TSimpleEngine.Destroy;
begin
  if assigned(FPackage) then
    FPackage.Destroy;
  if FElements<>nil then
    FElements.Destroy;
  inherited;
end;

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  if FElements=nil then
    FElements:=TObjectList.Create;
  FElements.Add(result);
end;
constructor TSimpleEngine.Create;
begin
  inherited;
  FPackage:=TPasPackage.Create('',nil);
  FElements:=nil;
end;
procedure TSimpleEngine.Log(Sender : TObject; Const Msg : String);
begin
  DebugLn(format('[MetaDarkStyle] %s',[Msg]));
end;
function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

function Identifer2TIdent(id:string):TIdent;
begin
  id:=UpperCase(id);
  for Result:=Succ(Low(TIdent)) to High(TIdent) do
   if TIdents2Name[Result]=id then
     exit;
  Result:=Low(TIdent);
end;

function GetTreeViewExpandSignValue(pn:TPASEXPR):TTreeViewExpandSignType;
var
  lid:TIdent;
begin
  if pn is TPrimitiveExpr then begin
    lid:=Identifer2TIdent(TPrimitiveExpr(pn).Value);
    case lid of
       IdtvestTheme:result:=tvestTheme;
   IdtvestPlusMinus:result:=tvestPlusMinus;
       IdtvestArrow:result:=tvestArrow;
   IdtvestArrowFill:result:=tvestArrowFill;
IdtvestAngleBracket:result:={$If declared(tvestAngleBracket)}
                              tvestAngleBracket;//появилось в 02eed0c903e14a33c95b4abded0c66d193678d70
                            {$Else}
                              tvestArrow;
                            {$EndIf}
    else
      Exception.Create(format('Error in line %d (only allowed "tvestTheme", "tvestPlusMinus", "tvestArrow", "tvestArrowFill", "tvestAngleBracket")',[pn.SourceLinenumber]));
    end;
  end else
    Exception.Create(format('Error in line %d (only palette names allowed "DefaultDark", "DefaultWhite")',[pn.SourceLinenumber]));
end;

function GetPaletteByName(pn:TPASEXPR):TDSColors;
var
  lid:TIdent;
begin
  if pn is TPrimitiveExpr then begin
    lid:=Identifer2TIdent(TPrimitiveExpr(pn).Value);
    case lid of
 IdDefaultDark:result:=DefaultDark;
IdDefaultWhite:result:=DefaultWhite;
    else
      Exception.Create(format('Error in line %d (only palette names allowed "DefaultDark", "DefaultWhite")',[pn.SourceLinenumber]));
    end;
  end else
    Exception.Create(format('Error in line %d (only palette names allowed "DefaultDark", "DefaultWhite")',[pn.SourceLinenumber]));
end;

function GetArrayIndex(indxs:TPasExprArray):integer;
var
  lid:TIdent;
begin
  if Length(indxs)<>1 then
    Exception.Create(format('Error in line %d (only one index allowed)',[indxs[0].SourceLinenumber]));
  if indxs[0] is TPrimitiveExpr then begin
    case TPrimitiveExpr(indxs[0]).Kind of
      pekIdent:begin
        lid:=Identifer2TIdent(TPrimitiveExpr(indxs[0]).Value);
        case lid of
          IdCOLOR_SCROLLBAR..IdCOLOR_FORM:Result:=ord(lid)-ord(IdCOLOR_SCROLLBAR);
        else
          Exception.Create(format('Error in line %d (unknown index [%s])',[indxs[0].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
        end;
      end;
      pekNumber:begin
        if not TryStrToInt(TPrimitiveExpr(indxs[0]).Value,result) then
          Exception.Create(format('Error in line %d (unknown index [%s])',[indxs[0].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
      end;
      else
        Exception.Create(format('Error in line %d (unknown index [%s])',[indxs[0].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
    end;
  end else
    Exception.Create(format('Error in line %d (unknown index [])',[indxs[0].SourceLinenumber]));
end;

function GetRGBColor(indxs:TPasExprArray):TColor;
var
  lid:TIdent;
  r,g,b:integer;
begin
  if Length(indxs)<>3 then
    Exception.Create(format('Error in line %d (only 3 params allowed)',[indxs[0].SourceLinenumber]));
  if indxs[0] is TPrimitiveExpr then begin
    case TPrimitiveExpr(indxs[0]).Kind of
      pekNumber:begin
        if not TryStrToInt(TPrimitiveExpr(indxs[0]).Value,r) then
          Exception.Create(format('Error in line %d (unknown unknown param 1 [%s])',[indxs[0].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
      end;
      else
        Exception.Create(format('Error in line %d (unknown unknown param 1 [%s])',[indxs[0].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
    end;
  end else
    Exception.Create(format('Error in line %d (unknown param 1 )',[indxs[0].SourceLinenumber]));
  if indxs[1] is TPrimitiveExpr then begin
    case TPrimitiveExpr(indxs[1]).Kind of
      pekNumber:begin
        if not TryStrToInt(TPrimitiveExpr(indxs[1]).Value,g) then
          Exception.Create(format('Error in line %d (unknown unknown param 2 [%s])',[indxs[1].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
      end;
      else
        Exception.Create(format('Error in line %d (unknown unknown param 2 [%s])',[indxs[1].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
    end;
  end else
    Exception.Create(format('Error in line %d (unknown param 2 )',[indxs[1].SourceLinenumber]));
  if indxs[2] is TPrimitiveExpr then begin
    case TPrimitiveExpr(indxs[2]).Kind of
      pekNumber:begin
        if not TryStrToInt(TPrimitiveExpr(indxs[2]).Value,b) then
          Exception.Create(format('Error in line %d (unknown unknown param 3 [%s])',[indxs[2].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
      end;
      else
        Exception.Create(format('Error in line %d (unknown unknown param 3 [%s])',[indxs[2].SourceLinenumber,TPrimitiveExpr(indxs[0]).Value]));
    end;
  end else
    Exception.Create(format('Error in line %d (unknown param 3 )',[indxs[2].SourceLinenumber]));
  result:=RGBToColor(r,g,b);
end;

function GetColor(fn:TPASEXPR):TColor;
var
  lid:TIdent;
  i:integer;
begin
  if fn.Kind=pekFuncParams then begin
    lid:=Identifer2TIdent(TPrimitiveExpr(TParamsExpr(fn).Value).Value);
    case lid of
      IdRGBToColor:begin
        result:=GetRGBColor(TParamsExpr(fn).Params);
      end;
      IdGetSysColor:begin
        i:=GetArrayIndex(TParamsExpr(fn).Params);
        result:=GetSysColor(i);
      end
    else
          Exception.Create(format('Error in line %d (only "RGBToColor()", "GetSysColor()" allowed, but "%s" found)',[fn.SourceLinenumber,TParamsExpr(fn).Value]));
    end;
  end else
    Exception.Create(format('Error in line %d (only "RGBToColor()", "GetSysColor()" allowed)',[fn.SourceLinenumber]));
end;

procedure SetBoolean(var ABoolean:Boolean;pn:TPASEXPR);
begin
  if pn is TBoolConstExpr then
    ABoolean:=TBoolConstExpr(pn).Value
  else
    Exception.Create(format('Error in line %d (only True or False allowed)',[pn.SourceLinenumber]));
end;


procedure PrepareAssign(Ass:TPasImplAssign;var DSC:TDSColors);
var
  lid:TIdent;
  i:integer;
begin
  if Ass.Left.OpCode=eopNone then begin
    if Ass.Left.Kind=pekIdent then begin
      lid:=Identifer2TIdent(TPrimitiveExpr(Ass.Left).Value);
      case lid of
                      IdScheme:DSC:=GetPaletteByName(Ass.Right);
  IdTreeViewExpandSignOverride:SetBoolean(DSC.DrawControl.TreeViewExpandSignOverride,Ass.Right);
     IdTreeViewExpandSignValue:DSC.DrawControl.TreeViewExpandSignValue:=GetTreeViewExpandSignValue(Ass.Right);
        IdCustomDrawScrollbars:SetBoolean(DSC.DrawControl.CustomDrawScrollbars,Ass.Right);
       IdCustomDrawPushButtons:SetBoolean(DSC.DrawControl.CustomDrawPushButtons,Ass.Right);
         IdCustomDrawComboBoxs:SetBoolean(DSC.DrawControl.CustomDrawComboBoxs,Ass.Right);
         IdCustomDrawTreeViews:SetBoolean(DSC.DrawControl.CustomDrawTreeViews,Ass.Right);
IdTreeViewDisableHideSelection:SetBoolean(DSC.DrawControl.TreeViewDisableHideSelection,Ass.Right);
      else
        Exception.Create(format('Error in line %d (wrong left side)',[Ass.SourceLinenumber]));
      end;
    end else if Ass.Left.Kind=pekArrayParams then begin
      if (not(Ass.Left is TParamsExpr))and(not(TParamsExpr(Ass.Left).Value is TPrimitiveExpr)) then
        Exception.Create(format('Error in line %d (wrong left side: wrong array index)',[Ass.SourceLinenumber]));
      lid:=Identifer2TIdent(TPrimitiveExpr(TParamsExpr(Ass.Left).Value).Value);
      if lid<>IdColors then
        Exception.Create(format('Error in line %d (wrong left side: only "Colors[]" allowed)',[Ass.SourceLinenumber]));
      i:=GetArrayIndex(TParamsExpr(Ass.Left).Params);
      DSC.SysColor[i]:=GetColor(Ass.Right)
    end else
      Exception.Create(format('Error in line %d (wrong left side: not ident[] or ident)',[Ass.SourceLinenumber]));
  end else
    raise Exception.Create(format('Error in line %d (wrong left side)',[Ass.SourceLinenumber]));
end;

procedure PrepareElement(pie:TPasImplElement;var DSC:TDSColors);
begin
  if pie is TPasImplAssign then
    PrepareAssign(pie as TPasImplAssign,DSC)
  else
    raise Exception.Create(format('Error in line %d (only := alowed)',[pie.SourceLinenumber]));
end;

function PrepareModule(m:TPasProgram):TDSColors;
var
  pie:TPasImplElement;
begin
  if not(m is TPasProgram) then
    raise Exception.Create('Program is expected');
  if not assigned((m as TPasProgram).InitializationSection) then
    raise Exception.Create('Program is empty');
  if ((m as TPasProgram).InputFile<>'')or((m as TPasProgram).OutPutFile<>'') then
    raise Exception.Create('No input/output file needed');

  for pointer(pie) in (m as TPasProgram).InitializationSection.Elements do
   PrepareElement(pie,Result);
end;

function ScanModule(modulename:string;module:TStream;out DSC:TDSColors):Boolean;
var
  E:TSimpleEngine;
  Parser: TPasParser;
  Resolver:TStreamResolver;
  Scanner: TPascalScanner;
  m:TPasModule;

begin
   E := TSimpleEngine.Create;
   E.uname:=modulename;

   Resolver:=TStreamResolver.Create;
   Scanner:=TPascalScanner.Create(Resolver);
   Scanner.LogEvents:=[sleFile,sleLineNumber,sleConditionals,sleDirective];
   Scanner.OnLog:=E.Onlog;
   Parser := TPasParser.Create(Scanner, Resolver, E);
   Parser.LogEvents:=[pleInterface,pleImplementation];
   Parser.OnLog:=E.Onlog;
   result:=False;

   try
     try
       Resolver.AddStream(modulename,module);
       Scanner.OpenFile(modulename);
       Parser.ParseMain(m);
       try
         result:=True;
         dsc:=PrepareModule(TPasProgram(m));
       except
         on excep:Exception do begin
           DebugLn(format('{EM}[MetaDarkStyle]DSScheme prepare exception: "%s" in file "%s"',[excep.message,modulename]));
           result:=false;
         end
         else;
       end;

     except
       on excep:EParserError do begin
          DebugLn(format('{EM}[MetaDarkStyle]DSScheme parse error: "%s" line:%d column:%d  file:%s',[excep.message,excep.row,excep.column,excep.filename]));
          FreeAndNil(result);
       end;
       on excep:Exception do begin
          DebugLn(format('{EM}[MetaDarkStyle]DSScheme parse exception: "%s" in file "%s"',[excep.message,modulename]));
          FreeAndNil(result);
       end
       else begin
         DebugLn(format('{EM}[MetaDarkStyle]Error in file "%s"',[modulename]));
         FreeAndNil(result);
       end;
     end;
   finally
     Parser.Free;
     {$IFDEF FPC_FULLVERSION}{$IF FPC_FULLVERSION > 30202}
     //error in 3.2.2 cause memoryleak
     E.Free;
     {$ENDIF}{$ENDIF}
     Resolver.Free;
     Scanner.Free;
   end;
end;

function ParseColors(modulename:string;module:TStream;out DSC:TDSColors):Boolean;overload;
var
  m:TPasProgram;
begin
  result:=false;
  try
    try
     result:=ScanModule(modulename,module,DSC);
    except
      on excep:Exception do begin
        DebugLn(format('{EM}[MetaDarkStyle]DSScheme prepare exception: "%s" in file "%s"',[excep.message,modulename]));
        result:=false;
      end
      else;
    end;
  finally
  end;
end;

function ParseColors(modulename,module:string;out DSC:TDSColors):Boolean;overload;
var
  ms:TOnMemoryStream;
begin
  ms:=TOnMemoryStream.Create(@module[1],Length(module)*sizeof(module[1]));
  result:=ParseColors(modulename,ms,DSC);
  ms.Destroy;
end;

function ParseColorsFile(AFile:string;out DSC:TDSColors):Boolean;overload;
var
  bfs:TBufferedFileStream;
begin
  bfs:=TBufferedFileStream.Create(AFile,fmOpenRead or fmShareDenyWrite);
  result:=ParseColors(AFile,bfs,DSC);
  bfs.Destroy;
end;

end.
