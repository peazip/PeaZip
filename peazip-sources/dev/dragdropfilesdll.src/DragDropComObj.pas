unit DragDropComObj;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropComObj
// Description:     Implements misc COM support classes.
// Version:         5.2
// Date:            17-AUG-2010
// Target:          Win32, Delphi 5-2010
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//
// Lazarus adaption 10/2017 Michael Köcher / six1
// -----------------------------------------------------------------------------

interface

uses
  ComObj,
  Classes,
  ActiveX;

{$include DragDrop.inc}

////////////////////////////////////////////////////////////////////////////////
//
//              TVCLComObject
//
////////////////////////////////////////////////////////////////////////////////
// Based on TVCLAutoObject.
////////////////////////////////////////////////////////////////////////////////
type
  TVCLComObject = class(TComObject, IVCLComObject, IUnknown)
  private
    FComponent: TComponent;
    FOwnsComponent: Boolean;
  protected
    // IVCLComObject implementation
    procedure FreeOnRelease;
    function Invoke(DispID: Integer; const IID: TGUID;
      LocaleID: Integer; Flags: Word; var Params;
      VarResult, ExcepInfo, ArgErr: Pointer): HResult;  virtual; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
  public
    // TODO : For now, please ignore linker warning about TVCLComObject.Create
    constructor Create(Factory: TComObjectFactory; Component: TComponent);
    destructor Destroy; override;
    procedure Initialize; override;
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; virtual; //override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TVCLComObjectFactory
//
////////////////////////////////////////////////////////////////////////////////
// Class factory for component based COM classes.
// Does not require a type library.
// Based on TComponentFactory and TComObjectFactory.
////////////////////////////////////////////////////////////////////////////////
type
  TVCLComObjectFactory = class(TComObjectFactory, IClassFactory)
  private
  protected
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
      out Obj): HResult; stdcall;
  public
    constructor Create(ComServer: TComServerObject; ComponentClass: TComponentClass;
      const ClassID: TGUID; const ClassName, Description: string;
      Instancing: TClassInstancing);
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    procedure UpdateRegistry(Register: Boolean); override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TShellExtFactory
//
////////////////////////////////////////////////////////////////////////////////
// Class factory for component based COM classes.
// Specialized for Shell Extensions.
////////////////////////////////////////////////////////////////////////////////
  TShellExtFactory = class(TVCLComObjectFactory)
  private
    FFileExtension: string;
    FFileClass: string;
  protected
    function GetProgID: string;// override;
  public
    constructor Create(ComServer: TComServerObject; ComponentClass: TComponentClass;
      const ClassID: TGUID; const ClassName, Description, AFileClass,
      AFileExtension: string; Instancing: TClassInstancing);
    procedure UpdateRegistry(Register: Boolean); override;
    property FileClass: string read FFileClass write FFileClass;
    property FileExtension: string read FFileExtension write FFileExtension;
  end;



////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
procedure DeleteDefaultRegValue(const Key: string);
function DeleteEmptyRegKey(Key: string; DeleteTree: boolean = True): Boolean;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//              	IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils,
  Windows;

////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
procedure DeleteDefaultRegValue(const Key: string);
var
  SubKey: HKey;
begin
  if (RegOpenKey(HKEY_CLASSES_ROOT, PChar(Key), SubKey) = ERROR_SUCCESS) then
    try
      RegDeleteValue(SubKey, nil);
    finally
      RegCloseKey(SubKey);
    end;
end;

function DeleteEmptyRegKey(Key: string; DeleteTree: boolean = True): Boolean;
var
  SubKey: HKey;
  NumSubKeys, NumValues: DWORD;
  p: PChar;
begin
  p := nil;
  repeat
    Result := False;
    if (RegOpenKey(HKEY_CLASSES_ROOT, PChar(Key), SubKey) = ERROR_SUCCESS) then
      try
        Result := (RegQueryInfoKey(SubKey, nil, nil, nil, @NumSubKeys, nil, nil,
          @NumValues, nil, nil, nil, nil) = ERROR_SUCCESS);
        // Only delete key if it doesn't contain values or sub keys.
        Result := Result and (NumSubKeys = 0) and (NumValues = 0);
      finally
        RegCloseKey(SubKey);
      end;

    if (Result) then
    begin
      Result := (RegDeleteKey(HKEY_CLASSES_ROOT, PChar(Key)) = ERROR_SUCCESS);

      if (Result and DeleteTree) then
      begin
        // Move to parent key.
        p := AnsiStrRScan(PChar(Key), '\');
        if (p <> nil) then
        begin
          p^ := #0;
          Key := PChar(Key);
        end;
      end;
    end;

  until (not Result) or (not DeleteTree) or (p = nil);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TVCLComObject
//
////////////////////////////////////////////////////////////////////////////////
constructor TVCLComObject.Create(Factory: TComObjectFactory;
  Component: TComponent);
begin
  FComponent := Component;
  CreateFromFactory(Factory, nil);
end;

destructor TVCLComObject.Destroy;
begin
  if FComponent <> nil then
  begin
    FComponent.VCLComObject := nil;
    if FOwnsComponent then
      FComponent.Free;
  end;
  inherited Destroy;
end;

procedure TVCLComObject.FreeOnRelease;
begin
  FOwnsComponent := True;
end;

function TVCLComObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLComObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TVCLComObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

procedure TVCLComObject.Initialize;
begin
  inherited Initialize;
  if FComponent = nil then
  begin
    FComponent := TComponentClass(Factory.ComClass).Create(nil);
    FOwnsComponent := True;
  end;
  FComponent.VCLComObject := Pointer(IVCLComObject(Self));
end;

function TVCLComObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLComObject.ObjQueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  Result := inherited ObjQueryInterface(IID, Obj);
  if (Result <> 0) and (FComponent <> nil) then
    if FComponent.GetInterface(IID, Obj) then
      Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TApartmentThread
//
////////////////////////////////////////////////////////////////////////////////
// Copied from VCLCom unit.
////////////////////////////////////////////////////////////////////////////////
type
  TApartmentThread = class(TThread)
  private
    FFactory: IClassFactory2;
    FUnkOuter: IUnknown;
    FIID: TGuid;
    FSemaphore: THandle;
    FStream: Pointer;
    FCreateResult: HResult;
  protected
    procedure Execute; override;
  public
    constructor Create(Factory: IClassFactory2; UnkOuter: IUnknown; IID: TGuid);
    destructor Destroy; override;
    property Semaphore: THandle read FSemaphore;
    property CreateResult: HResult read FCreateResult;
    property ObjStream: Pointer read FStream;
  end;

constructor TApartmentThread.Create(Factory: IClassFactory2;
  UnkOuter: IUnknown; IID: TGuid);
begin
  FFactory := Factory;
  FUnkOuter := UnkOuter;
  FIID := IID;
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TApartmentThread.Destroy;
begin
  CloseHandle(FSemaphore);
  inherited Destroy;
end;

procedure TApartmentThread.Execute;
var
  msg: TMsg;
  Unk: IUnknown;
begin
  try
    CoInitialize(nil);
    try
      FCreateResult := FFactory.CreateInstanceLic(FUnkOuter, nil, FIID, '', Unk);
      FUnkOuter := nil;
      FFactory := nil;
      if FCreateResult = S_OK then
        CoMarshalInterThreadInterfaceInStream(FIID, Unk, IStream(FStream));
      ReleaseSemaphore(FSemaphore, 1, nil);
      if FCreateResult = S_OK then
        while GetMessage(msg, 0, 0, 0) do
        begin
          DispatchMessage(msg);
          Unk._AddRef;
          if Unk._Release = 1 then break;
        end;
    finally
      Unk := nil;
      CoUninitialize;
    end;
  except
    { No exceptions should go unhandled }
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TVCLComObjectFactory
//
////////////////////////////////////////////////////////////////////////////////
constructor TVCLComObjectFactory.Create(ComServer: TComServerObject;
  ComponentClass: TComponentClass; const ClassID: TGUID; const ClassName,
  Description: string; Instancing: TClassInstancing);
begin
  inherited Create(ComServer, TComClass(ComponentClass), ClassID, ClassName,
    Description, Instancing, tmApartment);
end;

function TVCLComObjectFactory.CreateComObject(const Controller: IUnknown): TComObject;
begin
  Result := TVCLComObject.CreateFromFactory(Self, Controller);
end;

function TVCLComObjectFactory.CreateInstance(const UnkOuter: IUnknown;
  const IID: TGUID; out Obj): HResult;
begin
  if not IsLibrary then
  begin
    LockServer(True);
    try
      with TApartmentThread.Create(Self, UnkOuter, IID) do
      begin
        if WaitForSingleObject(Semaphore, INFINITE) = WAIT_OBJECT_0 then
        begin
          Result := CreateResult;
          if Result <> S_OK then Exit;
          Result := CoGetInterfaceAndReleaseStream(IStream(ObjStream), IID, Obj);
        end else
          Result := E_FAIL
      end;
    finally
      LockServer(False);
    end;
  end else
    Result := inherited CreateInstance(UnkOuter, IID, Obj);
end;

type
  TComponentProtectedAccess = class(TComponent);
  TComponentProtectedAccessClass = class of TComponentProtectedAccess;

procedure TVCLComObjectFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
    inherited UpdateRegistry(Register);
  TComponentProtectedAccessClass(ComClass).UpdateRegistry(Register,
    GUIDToString(ClassID), ProgID);
  if not Register then
    inherited UpdateRegistry(Register);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TShellExtFactory
//
////////////////////////////////////////////////////////////////////////////////
constructor TShellExtFactory.Create(ComServer: TComServerObject;
  ComponentClass: TComponentClass; const ClassID: TGUID; const ClassName,
  Description, AFileClass, AFileExtension: string; Instancing: TClassInstancing);
begin
  inherited Create(ComServer, ComponentClass, ClassID, ClassName,
    Description, Instancing);
  FFileClass := AFileClass;
  FFileExtension := AFileExtension;
end;

function TShellExtFactory.GetProgID: string;
begin
  Result := '';
end;

procedure TShellExtFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
  begin
    inherited UpdateRegistry(Register);
    if (FileExtension <> '') then
      CreateRegKey(FileExtension, '', FileClass);
  end else
  begin
    if (FileExtension <> '') then
      DeleteDefaultRegValue(FileExtension);
    inherited UpdateRegistry(Register);
  end;
end;

end.
