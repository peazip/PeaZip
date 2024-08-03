unit DragDropFormats;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropFormats
// Description:     Implements commonly used clipboard formats and base classes.
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
  DragDrop,
  Windows,
  types,
  Classes,
  ActiveX;

{$IF (FPC_FULLVERSION >= 30101)}
   {$DEFINE FPC311}
{$ENDIF}

{$include DragDrop.inc}

type
  PLargeint = ^Largeint;

type
////////////////////////////////////////////////////////////////////////////////
//
//              TStreamList
//
////////////////////////////////////////////////////////////////////////////////
// Utility class used by TFileContentsStreamClipboardFormat and
// TDataStreamDataFormat.
////////////////////////////////////////////////////////////////////////////////
  TStreamList = class(TObject)
  private
    FStreams: TStrings;
    FOnChanging: TNotifyEvent;
  protected
    function GetStream(Index: integer): TStream;
    function GetCount: integer;
    procedure Changing;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Stream: TStream): integer;
    function AddNamed(Stream: TStream; Name: string): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure Assign(Value: TStreamList);
    property Count: integer read GetCount;
    property Streams[Index: integer]: TStream read GetStream; default;
    property Names: TStrings read FStreams;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TNamedInterfaceList
//
////////////////////////////////////////////////////////////////////////////////
// List of named interfaces.
// Note: Delphi 5 also implements a TInterfaceList, but it can not be used
// because it doesn't support change notification and isn't extensible.
////////////////////////////////////////////////////////////////////////////////
// Utility class used by TFileContentsStorageClipboardFormat.
////////////////////////////////////////////////////////////////////////////////
  TNamedInterfaceList = class(TObject)
  private
    FList: TStrings;
    FOnChanging: TNotifyEvent;
  protected
    function GetCount: integer;
    function GetName(Index: integer): string;
    procedure SetName(Index: integer; const Value: string);
    function GetItem(Index: integer): IUnknown;
    procedure Changing;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Item: IUnknown): integer;
    function AddNamed(const Item: IUnknown; Name: string): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    procedure Assign(Value: TNamedInterfaceList);
    property Items[Index: integer]: IUnknown read GetItem; default;
    property Names[Index: integer]: string read GetName write SetName;
    property Count: integer read GetCount;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TStorageInterfaceList
//
////////////////////////////////////////////////////////////////////////////////
// List of IStorage interfaces.
// Used by TFileContentsStorageClipboardFormat and TStorageDataFormat.
////////////////////////////////////////////////////////////////////////////////
  TStorageInterfaceList = class(TNamedInterfaceList)
  private
  protected
    function GetStorage(Index: integer): IStorage;
  public
    property Storages[Index: integer]: IStorage read GetStorage; default;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFixedStreamAdapter
//
////////////////////////////////////////////////////////////////////////////////
// TFixedStreamAdapter fixes several serious bugs in TStreamAdapter.CopyTo.
////////////////////////////////////////////////////////////////////////////////
  TFixedStreamAdapter = class(TStreamAdapter, IStream)
  private
    FHasSeeked: boolean;
  public
    {$IF FPC_FULLVERSION >= 30101}
       function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; {override;} stdcall;
       function Seek(dlibMove: LargeInt; dwOrigin: DWORD; out libNewPosition: LargeUint): HResult; {override;} stdcall;
       function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult; {override;} stdcall;
       function CopyTo(stm: IStream; cb: LargeUint; out cbRead: LargeUint; out cbWritten: LargeUint): HResult;{override;} stdcall;
    {$ELSE}
       function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; {override;} stdcall;
       function Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult; {override;} stdcall;
       function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult; {override;} stdcall;
       function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult; {override;} stdcall;
    {$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TMemoryList
//
////////////////////////////////////////////////////////////////////////////////
// List which owns the memory blocks it points to.
////////////////////////////////////////////////////////////////////////////////
  TMemoryList = class(TObject)
  private
    FList: TList;
  protected
    function Get(Index: Integer): Pointer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read Get; default;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomSimpleClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for simple clipboard formats stored in global memory
// or a stream.
////////////////////////////////////////////////////////////////////////////////
//
// Two different methods of data transfer from the medium to the object are
// supported:
//
//   1) Descendant class reads data from a buffer provided by the base class.
//
//   2) Base class reads data from a buffer provided by the descendant class.
//
// Method #1 only requires that the descedant class implements the ReadData.
//
// Method #2 requires that the descedant class overrides the default
// DoGetDataSized method. The descendant DoGetDataSized method should allocate a
// buffer of the specified size and then call the ReadDataInto method to
// transfer data to the buffer. Even though the ReadData method will not be used
// in this scenario, it should be implemented as an empty method (to avoid
// abstract warnings).
//
// The WriteData method must be implemented regardless of which of the two
// approaches the class implements.
//
////////////////////////////////////////////////////////////////////////////////
  TCustomSimpleClipboardFormat = class(TClipboardFormat)
  private
  protected
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
    //: Transfer data from medium to a buffer of the specified size.
    function DoGetDataSized(const ADataObject: IDataObject; const AMedium: TStgMedium;
      Size: integer): boolean; virtual;
    //: Transfer data from the specified buffer to the objects storage.
    function ReadData(Value: pointer; Size: integer): boolean; virtual; abstract;
    //: Transfer data from the medium to the specified buffer.
    function ReadDataInto(const ADataObject: IDataObject; const AMedium: TStgMedium;
      Buffer: pointer; Size: integer): boolean; virtual;

    function DoSetData(const FormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; override;
    //: Transfer data from the objects storage to the specified buffer.
    function WriteData(Value: pointer; Size: integer): boolean; virtual; abstract;
    function GetSize: integer; virtual; abstract;
  public
    constructor Create; override;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomAnsiStringClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Base class for simple clipboard formats.
// The data is stored in an 8 bit ansi string.
////////////////////////////////////////////////////////////////////////////////
  TCustomAnsiStringClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FData: AnsiString;
    FTrimZeroes: boolean;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;

    function GetString: AnsiString;
    procedure SetString(const Value: AnsiString);
    property Data: AnsiString read FData write FData;
  public
    procedure Clear; override;
    function HasData: boolean; override;
    property TrimZeroes: boolean read FTrimZeroes write FTrimZeroes;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiStringClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TAnsiStringClipboardFormat = class(TCustomAnsiStringClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Data;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomStringListClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for simple cr/lf delimited string clipboard formats.
// The data is stored in a TStringList.
// The strings are assumed to contain ANSI data - even when they are Unicode
// strings.
////////////////////////////////////////////////////////////////////////////////
  TCustomStringListClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FLines: TStrings;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;

    function GetLines: TStrings;
    property Lines: TStrings read FLines;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    function HasData: boolean; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDWORDClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TCustomDWORDClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FValue: DWORD;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;

    function GetValueDWORD: DWORD;
    procedure SetValueDWORD(Value: DWORD);
    function GetValueInteger: integer;
    procedure SetValueInteger(Value: integer);
    function GetValueLongInt: longInt;
    procedure SetValueLongInt(Value: longInt);
    function GetValueBoolean: boolean;
    procedure SetValueBoolean(Value: boolean);
  public
    procedure Clear; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPreferredDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TPreferredDropEffectClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    class function GetClassClipboardFormat: TClipFormat;
    function GetClipboardFormat: TClipFormat; override;
    function HasData: boolean; override;
    property Value: longInt read GetValueLongInt write SetValueLongInt;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPerformedDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TPerformedDropEffectClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    class function DataDirection: TDataDirections; override;
    property Value: longInt read GetValueLongInt write SetValueLongInt;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TLogicalPerformedDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Microsoft's latest (so far) "logical" solution to the never ending attempts
// of reporting back to the source which operation actually took place. Sigh!
////////////////////////////////////////////////////////////////////////////////
  TLogicalPerformedDropEffectClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    class function DataDirection: TDataDirections; override;
    property Value: longInt read GetValueLongInt write SetValueLongInt;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPasteSucceededClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TPasteSucceededClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    class function DataDirection: TDataDirections; override;
    property Value: longInt read GetValueLongInt write SetValueLongInt;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TInDragLoopClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TInShellDragLoopClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property InShellDragLoop: boolean read GetValueBoolean write SetValueBoolean;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TTargetCLSIDClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TTargetCLSIDClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FCLSID: TCLSID;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    class function DataDirection: TDataDirections; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property CLSID: TCLSID read FCLSID write FCLSID;
  end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDataStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TDataStreamDataFormat = class(TCustomDataFormat)
  private
    FStreams: TStreamList;
  protected
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property Streams: TStreamList read FStreams;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFeedbackDataFormat
//
////////////////////////////////////////////////////////////////////////////////
// Data used for communication between source and target.
// Only used by the drop source.
////////////////////////////////////////////////////////////////////////////////
  TFeedbackDataFormat = class(TCustomDataFormat)
  private
    FPreferredDropEffect: longInt;
    FPerformedDropEffect: longInt;
    FLogicalPerformedDropEffect: longInt;
    FPasteSucceeded: longInt;
    FInShellDragLoop: boolean;
    FGotInShellDragLoop: boolean;
    FTargetCLSID: TCLSID;
  protected
    class procedure RegisterCompatibleFormats; override;
    procedure SetInShellDragLoop(const Value: boolean);
    procedure SetPasteSucceeded(const Value: longInt);
    procedure SetPerformedDropEffect(const Value: longInt);
    procedure SetPreferredDropEffect(const Value: longInt);
    procedure SetTargetCLSID(const Value: TCLSID);
    procedure SetLogicalPerformedDropEffect(const Value: Integer);
  public
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property PreferredDropEffect: longInt read FPreferredDropEffect
      write SetPreferredDropEffect;
    property PerformedDropEffect: longInt read FPerformedDropEffect
      write SetPerformedDropEffect;
    property LogicalPerformedDropEffect: longInt read FLogicalPerformedDropEffect
      write SetLogicalPerformedDropEffect;
    property PasteSucceeded: longInt read FPasteSucceeded write SetPasteSucceeded;
    property InShellDragLoop: boolean read FInShellDragLoop
      write SetInShellDragLoop;
    property TargetCLSID: TCLSID read FTargetCLSID write SetTargetCLSID;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TGenericClipboardFormat & TGenericDataFormat
//
////////////////////////////////////////////////////////////////////////////////
// TGenericDataFormat is not used internally by the library, but can be used to
// add support for new formats with a minimum of custom code.
// Even though TGenericDataFormat represents the data as a string, it can be
// used to transfer any kind of data.
// TGenericClipboardFormat is used internally by TGenericDataFormat but can also
// be used by other TCustomDataFormat descendants or as a base class for new
// clipboard formats.
// Note that you should not register TGenericClipboardFormat as compatible with
// TGenericDataFormat.
// To use TGenericDataFormat, all you need to do is instantiate it against
// the desired component and register your custom clipboard formats:
//
// var
//   MyCustomData: TGenericDataFormat;
//
//   MyCustomData := TGenericDataFormat.Create(DropTextTarget1);
//   MyCustomData.AddFormat('MyCustomFormat');
//
////////////////////////////////////////////////////////////////////////////////
  TGenericDataFormat = class(TCustomDataFormat)
  private
    FData: AnsiString;
  protected
    function GetSize: integer;
    procedure DoSetData(const Value: AnsiString);
  public
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    procedure AddFormat(const AFormat: string);
    procedure SetDataHere(const AData; ASize: integer);
    function GetDataHere(var AData; ASize: integer): integer;
    property Data: AnsiString read FData write DoSetData;
    property Size: integer read GetSize;
  end;

  TGenericClipboardFormat = class(TCustomAnsiStringClipboardFormat)
  private
    FFormat: string;
  protected
    procedure SetClipboardFormatName(const Value: string); override;
    function GetClipboardFormatName: string; override;
    function GetClipboardFormat: TClipFormat; override;
  public
    class function DataDirection: TDataDirections; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Data;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////

// CreateIStreamFromIStorage stores a copy of an IStorage object on an IStream
// object and returns the IStream object.
// It is the callers resposibility to dispose of the IStream. Any modifications
// made to the IStream does not affect the original IStorage object.
//
// CreateIStreamFromIStorage and the work to integrate it into
// TFileContentsStreamClipboardFormat was funded by ThoughtShare Communications
// Inc.
function CreateIStreamFromIStorage(const Storage: IStorage): IStream;
function CreateIStorageOnHGlobal(GlobalSource: HGLOBAL): IStorage;
function GetMediumDataSize(Medium: TStgMedium): integer;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//                      IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  DropSource,
  DropTarget,
  ComObj,
  ShlObj,
 // AxCtrls,
  SysUtils;


////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////
function GetMediumDataSize(Medium: TStgMedium): integer;
var
  Stream: IStream;
  StatStg: TStatStg;
begin
  Result := -1;
  // Get size from HGlobal.
  if (Medium.tymed = TYMED_HGLOBAL) then
  begin
    Result := GlobalSize(Medium.HGlobal);
  end else
  // Get size from IStream.
  if (Medium.tymed = TYMED_ISTREAM) then
  begin
    Stream := IStream(Medium.pstm);
    if (Stream <> nil) and (Succeeded(Stream.Stat(StatStg, STATFLAG_NONAME))) then
      Result := StatStg.cbSize;
  end else
  // Get size and stream from IStorage.
  if (Medium.tymed = TYMED_ISTORAGE) then
  begin
    Stream := CreateIStreamFromIStorage(IStorage(Medium.pstg));
    if (Stream <> nil) and (Succeeded(Stream.Stat(StatStg, STATFLAG_NONAME))) then
      Result := StatStg.cbSize;
    (*
    // Unfortunately we can't just get the size directly from the IStorage -
    // that would have been too easy:
    if (Succeeded(IStorage(Medium.stg).Stat(StatStg, STATFLAG_NONAME))) then
      Result := StatStg.cbSize;
    *)
  end;
end;

function CreateIStreamFromIStorage(const Storage: IStorage): IStream;
var
  LockBytes: ILockBytes;
  HGlob: HGLOBAL;
  NewStorage: IStorage;
begin
  // Start with a zero size memory block. ILockBytes will expand it as needed.
  // Note: The memory *MUST* be allocated with GMEM_MOVEABLE or the memory will
  // become corrupt and we will get a Windows diagnostic message later on:
  // Invalid Address specified to RtlGetUserInfoHeap( XXXXXXXX, XXXXXXXX )
  HGlob := GlobalAlloc(GMEM_MOVEABLE, 0);
  try
    // Wrap the memory in an ILockBytes object. The ILockBytes does not own the
    // memory - i.e. will not free it.
    OleCheck(CreateILockBytesOnHGlobal(HGlob, False, LockBytes));
    try
      // Create an IStorage on the ILockBytes.
      OleCheck(StgCreateDocfileOnILockBytes(LockBytes,
        STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE or STGM_DIRECT,
        0, NewStorage));
      try
        // Copy the source IStorage to our IStorage.
        OleCheck(Storage.CopyTo(0, nil, nil, NewStorage));
      finally
        // We now have the IStorage data in the ILockBytes, so we don't need the
        // IStorage anymore.
        // NewStorage.Commit(STGC_DEFAULT);
        NewStorage := nil;
      end;
    finally
      // We now have the IStorage data in the global memory, so we dont need the
      // ILockBytes anymore.
      // LockBytes.Flush;
      LockBytes := nil;
    end;
    // Create a stream which owns the memory block.
    OleCheck(CreateStreamOnHGlobal(HGlob, True, Result));
  except
    // Eat exceptions since they won't work inside drag/drop anyway.
    GlobalFree(HGlob);
    Result := nil;
  end;
end;

function CreateIStorageOnHGlobal(GlobalSource: HGLOBAL): IStorage;
var
  LockBytesSource, LockBytesDest: ILockBytes;
  GlobalDest: HGLOBAL;
  StorageSource: IStorage;
begin
  // Start with a zero size memory block. ILockBytes will expand it as needed.
  // Note: The memory *MUST* be allocated with GMEM_MOVEABLE or the memory will
  // become corrupt and we will get a Windows diagnostic message later on:
  // Invalid Address specified to RtlGetUserInfoHeap( XXXXXXXX, XXXXXXXX )
  GlobalDest := GlobalAlloc(GMEM_MOVEABLE, 0);
  try
    // Wrap the memory in an ILockBytes object. The ILockBytes owns the memory -
    // i.e. it will free it.
    try
      OleCheck(CreateILockBytesOnHGlobal(GlobalDest, True, LockBytesDest));
    except
      GlobalFree(GlobalDest);
      raise;
    end;
    // Wrap the source memory in an ILockBytes object. The ILockBytes does not
    // own the memory - i.e. will not free it.
    OleCheck(CreateILockBytesOnHGlobal(GlobalSource, False, LockBytesSource));
    // Create IStorage on source.
    OleCheck(StgCreateDocfileOnILockBytes(LockBytesSource,
      STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE{ or STGM_DIRECT},
      0, StorageSource));
    // Create IStorage on dest. Since the IStorage references the ILockBytes and
    // the ILockBytes owns the memory block, the memory will be freed once the
    // IStorage is freed.
    OleCheck(StgCreateDocfileOnILockBytes(LockBytesDest,
      STGM_CREATE or STGM_READWRITE or STGM_SHARE_EXCLUSIVE{ or STGM_DIRECT},
      0, Result));
    // Copy source IStorage to dest IStorage.
    OleCheck(StorageSource.CopyTo(0, nil, nil, Result));
  except
    // Eat exceptions since they won't work inside drag/drop anyway.
    Result := nil;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TStreamList
//
////////////////////////////////////////////////////////////////////////////////
constructor TStreamList.Create;
begin
  inherited Create;
  FStreams := TStringList.Create;
end;

destructor TStreamList.Destroy;
begin
  Clear;
  FStreams.Free;
  inherited Destroy;
end;

procedure TStreamList.Changing;
begin
  if (Assigned(OnChanging)) then
    OnChanging(Self);
end;

function TStreamList.GetStream(Index: integer): TStream;
begin
  Result := TStream(FStreams.Objects[Index]);
end;

function TStreamList.Add(Stream: TStream): integer;
begin
  Result := AddNamed(Stream, '');
end;

function TStreamList.AddNamed(Stream: TStream; Name: string): integer;
begin
  Changing;
  Result := FStreams.AddObject(Name, Stream);
end;

function TStreamList.GetCount: integer;
begin
  Result := FStreams.Count;
end;

procedure TStreamList.Assign(Value: TStreamList);
begin
  Clear;
  FStreams.Assign(Value.Names);
  // Transfer ownership of objects
  Value.FStreams.Clear;
end;

procedure TStreamList.Delete(Index: integer);
begin
  Changing;
  FStreams.Delete(Index);
end;

procedure TStreamList.Clear;
var
  i: integer;
begin
  Changing;
  for i := 0 to FStreams.Count-1 do
    if (FStreams.Objects[i] <> nil) then
      FStreams.Objects[i].Free;
  FStreams.Clear;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TNamedInterfaceList
//
////////////////////////////////////////////////////////////////////////////////
constructor TNamedInterfaceList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TNamedInterfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TNamedInterfaceList.Add(const Item: IUnknown): integer;
begin
  Result := AddNamed(Item, '');
end;

function TNamedInterfaceList.AddNamed(const Item: IUnknown; Name: string): integer;
begin
  Changing;
  with FList do
  begin
    Result := AddObject(Name, nil);
    Objects[Result] := TObject(pointer(Item)); // pointer(Item) is work around for Weaver
    Item._AddRef;
  end;
end;

procedure TNamedInterfaceList.Changing;
begin
  if (Assigned(OnChanging)) then
    OnChanging(Self);
end;

procedure TNamedInterfaceList.Clear;
var
  i: Integer;
  p: pointer;
begin
  Changing;
  with FList do
  begin
    for i := 0 to Count - 1 do
    begin
      p := Objects[i];
      IUnknown(p)._Release;
    end;
    Clear;
  end;
end;

procedure TNamedInterfaceList.Assign(Value: TNamedInterfaceList);
var
  i: Integer;
begin
  Changing;
  for i := 0 to Value.Count - 1 do
    AddNamed(Value.Items[i], Value.Names[i]);
end;

procedure TNamedInterfaceList.Delete(Index: integer);
var
  p: pointer;
begin
  Changing;
  with FList do
  begin
    p := Objects[Index];
    IUnknown(p)._Release;
    Delete(Index);
  end;
end;

function TNamedInterfaceList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TNamedInterfaceList.GetName(Index: integer): string;
begin
  Result := FList[Index];
end;

procedure TNamedInterfaceList.SetName(Index: integer; const Value: string);
begin
  FList[Index] := Value;
end;

function TNamedInterfaceList.GetItem(Index: integer): IUnknown;
var
  p: pointer;
begin
  p := FList.Objects[Index];
  Result := IUnknown(p);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TStorageInterfaceList
//
////////////////////////////////////////////////////////////////////////////////
function TStorageInterfaceList.GetStorage(Index: integer): IStorage;
begin
  Result := IStorage( TClass(Items[Index]) );
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TMemoryList
//
////////////////////////////////////////////////////////////////////////////////
function TMemoryList.Add(Item: Pointer): Integer;
begin
  Result := FList.Add(Item);
end;

procedure TMemoryList.Clear;
var
  i: integer;
begin
  for i := FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TMemoryList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TMemoryList.Delete(Index: Integer);
begin
  Freemem(FList[Index]);
  FList.Delete(Index);
end;

destructor TMemoryList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TMemoryList.Get(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TMemoryList.GetCount: Integer;
begin
  Result := FList.Count;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFixedStreamAdapter
//
////////////////////////////////////////////////////////////////////////////////


{$IF FPC_FULLVERSION >= 30101}
  function TFixedStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
{$ELSE}
  function TFixedStreamAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
{$ENDIF}
begin
  Result := inherited Stat(statstg, grfStatFlag);
  statstg.pwcsName := nil;
end;

{$IF FPC_FULLVERSION >= 30101}
  function TFixedStreamAdapter.Seek(dlibMove: LargeInt; dwOrigin: DWORD; out libNewPosition: LargeUint): HResult;
{$ELSE}
  function TFixedStreamAdapter.Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult;
{$ENDIF}
begin
  Result := inherited Seek(dlibMove, dwOrigin, qword(libNewPosition));
  FHasSeeked := True;
end;

{$IF FPC_FULLVERSION >= 30101}
  function TFixedStreamAdapter.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
  begin
    if (not FHasSeeked) then
      Seek(0, STREAM_SEEK_SET, PLargeUint(nil)^);
    Result := inherited Read(pv, cb, pdword(pcbRead));
  end;
{$ELSE}
  function TFixedStreamAdapter.Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
  begin
    if (not FHasSeeked) then
      Seek(0, STREAM_SEEK_SET, PLargeint(nil)^);
    Result := inherited Read(pv, cb, pdword(pcbRead));
  end;
{$ENDIF}

{$IF FPC_FULLVERSION >= 30101}
  function TFixedStreamAdapter.CopyTo(stm: IStream; cb: LargeUint; out cbRead: LargeUint; out cbWritten: LargeUint): HResult;
{$ELSE}
  function TFixedStreamAdapter.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult;
{$ENDIF}
const
  MaxBufSize = 1024 * 1024;  // 1mb
var
  Buffer: Pointer;
  BufSize, BurstReadSize, BurstWriteSize: Integer;
  BytesRead, BytesWritten, BurstWritten: LongInt;
begin
  Result := S_OK;
  BytesRead := 0;
  BytesWritten := 0;
  try
    if (cb < 0) then
    begin
      // Note: The folowing is a workaround for a design bug in either explorer
      // or the clipboard. See comment in TCustomSimpleClipboardFormat.DoSetData
      // for an explanation.
      if (Stream.Position = Stream.Size) then
        Stream.Position := 0;

      cb := Stream.Size - Stream.Position;
    end;
    if cb > MaxBufSize then
      BufSize := MaxBufSize
    else
      BufSize := Integer(cb);
    GetMem(Buffer, BufSize);
    try
      while cb > 0 do
      begin
        if cb > BufSize then
          BurstReadSize := BufSize
        else
          BurstReadSize := cb;

        BurstWriteSize := Stream.Read(Buffer^, BurstReadSize);
        if (BurstWriteSize = 0) then
          break;
        Inc(BytesRead, BurstWriteSize);
        BurstWritten := 0;
        // TODO : Add support for partial writes.
        Result := stm.Write(Buffer, BurstWriteSize, pdword(@BurstWritten));
        Inc(BytesWritten, BurstWritten);
        if (Succeeded(Result)) and (Integer(BurstWritten) <> BurstWriteSize) then
          Result := E_FAIL;
        if (Failed(Result)) then
          Exit;
        Dec(cb, BurstWritten);
      end;
    finally
      FreeMem(Buffer);
      if (@cbWritten <> nil) then
        cbWritten := BytesWritten;
      if (@cbRead <> nil) then
        cbRead := BytesRead;
    end;
  except
    Result := E_UNEXPECTED;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomSimpleClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomSimpleClipboardFormat.Create;
begin
  CreateFormat(TYMED_HGLOBAL or TYMED_ISTREAM);

  // Note: Don't specify TYMED_ISTORAGE here or simple clipboard operations
  // (e.g. copy/paste text) will fail. The clipboard apparently prefers
  // TYMED_ISTORAGE over other mediums and some applications can't handle that
  // medium.
end;

function TCustomSimpleClipboardFormat.DoGetData(const ADataObject: IDataObject;
  const AMedium: TStgMedium): boolean;
var
  Stream: IStream;
  StatStg: TStatStg;
  Size: integer;
  Medium: TStgMedium;
begin
  // Get size from HGlobal.
  if (AMedium.tymed = TYMED_HGLOBAL) then
  begin
    Size := GlobalSize(AMedium.HGlobal);
    Result := True;
    // Read the given amount of data.
    if (Size > 0) then
      Result := DoGetDataSized(ADataObject, AMedium, Size);
  end else
  // Get size from IStream.
  if (AMedium.tymed = TYMED_ISTREAM) then
  begin
    Stream := IStream(AMedium.pstm);
    Result := (Stream <> nil) and (Succeeded(Stream.Stat(StatStg, STATFLAG_NONAME)));
    Stream := nil;
    Size := StatStg.cbSize;
    // Read the given amount of data.
    if (Result) and (Size > 0) then
      Result := DoGetDataSized(ADataObject, AMedium, Size);
  end else
  // Get size and stream from IStorage.
  if (AMedium.tymed = TYMED_ISTORAGE) then
  begin
    Stream := CreateIStreamFromIStorage(IStorage(AMedium.pstg));
    Result := (Stream <> nil) and (Succeeded(Stream.Stat(StatStg, STATFLAG_NONAME)));
    Size := StatStg.cbSize;
    Medium.tymed := TYMED_ISTREAM;
    Medium.punkForRelease := nil;
    Medium.pstm := pointer(Stream);
    if (Result) and (Size > 0) then
      // Read the given amount of data.
      Result := DoGetDataSized(ADataObject, Medium, Size);
  end else
    Result := False;
end;

function TCustomSimpleClipboardFormat.DoGetDataSized(const ADataObject: IDataObject;
  const AMedium: TStgMedium; Size: integer): boolean;
var
  Buffer: pointer;
  Stream: IStream;
  Remaining: longInt;
  Chunk: longInt;
  pChunk: PByte;
  HGlob: HGLOBAL;
  ChunkBuffer: pointer;
const
  MaxChunk = 1024*1024; // 1Mb.
begin
  if (Size > 0) then
  begin
    // Read data from HGlobal
    if (AMedium.tymed = TYMED_HGLOBAL) then
    begin
      // Use global memory as buffer
      Buffer := GlobalLock(AMedium.HGlobal);
      try
        // Read data from buffer into object
        Result := (Buffer <> nil) and (ReadData(Buffer, Size));
      finally
        GlobalUnlock(AMedium.HGlobal);
      end;
    end else
    // Read data from IStream
    if (AMedium.tymed = TYMED_ISTREAM) then
    begin
      // Allocate buffer
      GetMem(Buffer, Size);
      try
        // Read data from stream into buffer
        Stream := IStream(AMedium.pstm);
        if (Stream <> nil) then
        begin
          Stream.Seek(0, STREAM_SEEK_SET, qword(PLargeint(nil)^));
          Result := True;
          Remaining := Size;
          pChunk := Buffer;

          // If we have to transfer large amounts of data it is much more
          // efficient to do so in small chunks in and to use global memory.
          // Memory allocated with GetMem is much too slow because it is paged
          // and thus causes trashing (excessive page faults) if we access a
          // large memory block sequentially.
          // Tests has shown that allocating a 10Mb buffer and trying to read
          // data into it in 1Kb chunks takes several minutes, while the same
          // data can be read into a 32Kb buffer in 1Kb chunks in seconds. The
          // Windows explorer uses a 1 Mb buffer.
          // The above tests were performed using the AsyncSource demo.
          HGlob := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, MaxChunk);
          if (HGlob = 0) then
          begin
            Result := False;
            exit;
          end;
          try
            ChunkBuffer := GlobalLock(HGlob);
            try
              if (ChunkBuffer = nil) then
              begin
                Result := False;
                exit;
              end;

              while (Result) and (Remaining > 0) do
              begin
                if (Remaining > MaxChunk) then
                  Chunk := MaxChunk
                else
                  Chunk := Remaining;
                // Result := (Stream.Read(pChunk, Chunk, @Chunk) = S_OK);
                Result := (Succeeded(Stream.Read(ChunkBuffer, Chunk, pdword(@Chunk))));
                if (Chunk = 0) then
                  break;
                Move(ChunkBuffer^, pChunk^, Chunk);
                inc(pChunk, Chunk);
                dec(Remaining, Chunk);
              end;
            finally
              GlobalUnlock(hGlob);
            end;
          finally
            GlobalFree(HGlob);
          end;
          Stream := nil; // Not really nescessary.
        end else
          Result := False;
        // Transfer data from buffer into object.
        Result := Result and (ReadData(Buffer, Size));
      finally
        FreeMem(Buffer);
      end;
    end else
      Result := False;
  end else
    Result := False;
end;

function TCustomSimpleClipboardFormat.ReadDataInto(const ADataObject: IDataObject;
  const AMedium: TStgMedium; Buffer: pointer; Size: integer): boolean;
var
  Stream: IStream;
  p: pointer;
  Remaining: longInt;
  Chunk: longInt;
begin
  Result := (Buffer <> nil) and (Size > 0);
  if (Result) then
  begin
    // Read data from HGlobal
    if (AMedium.tymed = TYMED_HGLOBAL) then
    begin
      p := GlobalLock(AMedium.HGlobal);
      try
        Result := (p <> nil);
        if (Result) then
          Move(p^, Buffer^, Size);
      finally
        GlobalUnlock(AMedium.HGlobal);
      end;
    end else
    // Read data from IStream
    if (AMedium.tymed = TYMED_ISTREAM) then
    begin
      Stream := IStream(AMedium.pstm);
      if (Stream <> nil) then
      begin
        Stream.Seek(0, STREAM_SEEK_SET, qword(PLargeint(nil)^));
        Remaining := Size;
        while (Result) and (Remaining > 0) do
        begin
          Result := (Succeeded(Stream.Read(Buffer, Remaining, pdword(@Chunk))));
          if (Chunk = 0) then
            break;
          inc(PByte(Buffer), Chunk);
          dec(Remaining, Chunk);
        end;
      end else
        Result := False;
    end else
      Result := False;
  end;
end;

function TCustomSimpleClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
var
  p: pointer;
  Size: integer;
  Global: HGLOBAL;
  Stream: IStream;
  OleStream: TStream;
begin
  Result := False;

  Size := GetSize;
  if (Size <= 0) then
    exit;

  (*
  ** In this method we prefer TYMED_ISTREAM over TYMED_HGLOBAL and thus check
  ** for TYMED_ISTREAM first.
  *)

  // (FormatEtcIn.tymed <> -1) is a work around for drop targets that specify
  // the tymed incorrectly. E.g. the Nero Express CD burner does this and thus
  // asks for more than it can handle. 
  if (FormatEtcIn.tymed <> -1) and
    (FormatEtc.tymed and FormatEtcIn.tymed and TYMED_ISTREAM <> 0) then
  begin

    // Problems related to position of cursor in returned stream:
    //
    //   1) In some situations (e.g. after OleFlushClipboard) the clipboard
    //      uses an IStream.Seek(0, STREAM_SEEK_CUR) to determine the size of
    //      the stream and thus requires that Stream.Position=Stream.Size.
    //
    //   2) On Windows NT 4 the shell (shell32.dll 4.71) uses an
    //      IStream.Read(-1) to read all of the stream and thus requires that
    //      Stream.Position=0.
    //
    //   3) On Windows 2K the shell (shell32.dll 5.0) uses a IStream.Read(16K)
    //      in a loop to sequentially read all of the stream and thus requires
    //      that Stream.Position=0.
    //
    // This library uses an IStream.Stat to determine the size of the stream,
    // then uses an IStream.Seek(0, STREAM_SEEK_SET) to position to start of
    // stream and finally reads sequentially to end of stream with a number of
    // IStream.Read().
    //
    // Since we have to satisfy #1 above in order to support the clipboard, we
    // must work around #2 in TFixedStreamAdapter.CopyTo.
    //
    // At present there is no satisfactory solution to problem #3 so Windows
    // 2000 might not be fully supported. One possible (but not fully tested)
    // solution would be to implement special handling of IStream.Read(16K).

    Global := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, Size);
    if (Global = 0) then
      exit;
    try
      p := GlobalLock(Global);
      try
        Result := WriteData(p, Size);
      finally
        GlobalUnlock(Global);
      end;

      if (not Result) or (Failed(CreateStreamOnHGlobal(Global, True, Stream))) then
      begin
        GlobalFree(Global);
        exit;
      end;

      Stream.Seek(0, STREAM_SEEK_END, qword(PLargeint(nil)^));

      (*
      ** The following is a bit weird...
      ** In order to intercept the calls which the other end will make into our
      ** IStream object, we have to first wrap it in a TOleStream and then wrap
      ** the TOleStream in a TFixedStreamAdapter. The TFixedStreamAdapter will
      ** then be able to work around the problems mentioned above.
      **
      ** However...
      ** If you copy something to the clipboard and then close the source
      ** application, the clipboard will make a copy of the stream, release our
      ** TFixedStreamAdapter object and we are out of luck. If clipboard
      ** operations are of no importance to you, you can disable the two lines
      ** below which deals with TOLEStream and TFixedStreamAdapter and insert
      ** the following instead:
      **
      **   Stream.Seek(0, STREAM_SEEK_SET, LargeInt(nil^));
      *)
      OleStream := TOLEStream.Create(Stream);
      Stream := TFixedStreamAdapter.Create(OleStream, soOwned) as IStream;

      IStream(AMedium.pstm) := Stream;
    except
      Result := False;
    end;

    if (not Result) then
      IStream(AMedium.pstm) := nil
    else
      AMedium.tymed := TYMED_ISTREAM;

  end else
  if (FormatEtc.tymed and FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin

    AMedium.hGlobal := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, Size);
    if (AMedium.hGlobal = 0) then
      exit;

    try
      p := GlobalLock(AMedium.hGlobal);
      try
        Result := (p <> nil) and WriteData(p, Size);
      finally
        GlobalUnlock(AMedium.hGlobal);
      end;
    except
      Result := False;
    end;

    if (not Result) then
    begin
      GlobalFree(AMedium.hGlobal);
      AMedium.hGlobal := 0;
    end else
      AMedium.tymed := TYMED_HGLOBAL;

  end else
(*
  if (FormatEtcIn.tymed and TYMED_ISTORAGE <> 0) then
  begin

    Global := GlobalAlloc(GMEM_SHARE or GHND, Size);
    if (Global = 0) then
      exit;

    try
      try
        p := GlobalLock(Global);
        try
          Result := (p <> nil) and WriteData(p, Size);
        finally
          GlobalUnlock(Global);
        end;

        if (Result) then
        begin
          IStorage(AMedium.stg) := CreateIStorageOnHGlobal(Global);
          Result := (AMedium.stg <> nil);
        end;
      finally
        GlobalFree(Global);
      end;
      if (Result) then
        AMedium.tymed := TYMED_ISTORAGE;
    except
      IStorage(AMedium.stg) := nil;
      Result := False;
    end;

  end else
*)
    Result := False;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomAnsiStringClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TCustomAnsiStringClipboardFormat.Clear;
begin
  FData := '';
end;

function TCustomAnsiStringClipboardFormat.HasData: boolean;
begin
  Result := (FData <> '');
end;


function TCustomAnsiStringClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  SetLength(FData, Size);
  Move(Value^, PAnsiChar(FData)^, Size);

  // IE adds a lot of trailing zeroes which is included in the string length.
  // To avoid confusion, we trim all trailing zeroes but the last (which is
  // managed automatically by Delphi).
  // Note that since this work around, if applied generally, would mean that we
  // couldn't use this class to handle arbitrary binary data (which might
  // include zeroes), we are required to explicitly enable it in the classes
  // where we need it (e.g. all TCustomTextClipboardFormat descedants).
  if (FTrimZeroes) then
    SetLength(FData, Length(PAnsiChar(FData)));

  Result := True;
end;

function TCustomAnsiStringClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  // Transfer string including terminating zero if requested.
  Result := (Size <= Length(FData)+1);
  if (Result) then
    Move(PAnsiChar(FData)^, Value^, Size);
end;

function TCustomAnsiStringClipboardFormat.GetSize: integer;
begin
  Result := Length(FData);
end;

function TCustomAnsiStringClipboardFormat.GetString: AnsiString;
begin
  Result := FData;
end;

procedure TCustomAnsiStringClipboardFormat.SetString(const Value: AnsiString);
begin
  FData := Value;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiStringClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TAnsiStringClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := FFormatEtc.cfFormat;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomStringListClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomStringListClipboardFormat.Create;
begin
  inherited Create;
  FLines := TStringList.Create
end;

destructor TCustomStringListClipboardFormat.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TCustomStringListClipboardFormat.Clear;
begin
  FLines.Clear;
end;

function TCustomStringListClipboardFormat.HasData: boolean;
begin
  Result := (FLines.Count > 0);
end;

function TCustomStringListClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
var
  s: AnsiString;
begin
  SetLength(s, Size+1);
  Move(Value^, PAnsiChar(s)^, Size);
  s[Size] := AnsiChar(0);
  FLines.Text := String(s);
  Result := True;
end;

function TCustomStringListClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
var
  s: AnsiString;
begin
  s := AnsiString(FLines.Text);
  Result := (Size = Length(s)+1);
  if (Result) then
    Move(PAnsiChar(s)^, Value^, Size);
end;

function TCustomStringListClipboardFormat.GetSize: integer;
begin
  Result := Length(FLines.Text)+1;
end;

function TCustomStringListClipboardFormat.GetLines: TStrings;
begin
  Result := FLines;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDWORDClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TCustomDWORDClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  FValue := PDWORD(Value)^;
  Result := True;
end;

function TCustomDWORDClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size = SizeOf(DWORD));
  if (Result) then
    PDWORD(Value)^ := FValue;
end;

function TCustomDWORDClipboardFormat.GetSize: integer;
begin
  Result := SizeOf(DWORD);
end;

procedure TCustomDWORDClipboardFormat.Clear;
begin
  FValue := 0;
end;

function TCustomDWORDClipboardFormat.GetValueDWORD: DWORD;
begin
  Result := FValue;
end;

procedure TCustomDWORDClipboardFormat.SetValueDWORD(Value: DWORD);
begin
  FValue := Value;
end;

function TCustomDWORDClipboardFormat.GetValueInteger: integer;
begin
  Result := integer(FValue);
end;

procedure TCustomDWORDClipboardFormat.SetValueInteger(Value: integer);
begin
  FValue := DWORD(Value);
end;

function TCustomDWORDClipboardFormat.GetValueLongInt: longInt;
begin
  Result := longInt(FValue);
end;

procedure TCustomDWORDClipboardFormat.SetValueLongInt(Value: longInt);
begin
  FValue := DWORD(Value);
end;

function TCustomDWORDClipboardFormat.GetValueBoolean: boolean;
begin
  Result := (FValue <> 0);
end;

procedure TCustomDWORDClipboardFormat.SetValueBoolean(Value: boolean);
begin
  FValue := ord(Value);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPreferredDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_PREFERREDDROPEFFECT: TClipFormat = 0;

// GetClassClipboardFormat is used by TCustomDropTarget.GetPreferredDropEffect 
class function TPreferredDropEffectClipboardFormat.GetClassClipboardFormat: TClipFormat;
begin
  // CFSTR_PERFORMEDDROPEFFECT = PChar('Preferred DropEffect');
  if (CF_PREFERREDDROPEFFECT = 0) then
    CF_PREFERREDDROPEFFECT := RegisterClipboardFormat(PChar('Preferred DropEffect'));
  Result := CF_PREFERREDDROPEFFECT;
end;

function TPreferredDropEffectClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := GetClassClipboardFormat;
end;

function TPreferredDropEffectClipboardFormat.HasData: boolean;
begin
  Result := True; //(Value <> DROPEFFECT_NONE);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPerformedDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_PERFORMEDDROPEFFECT: TClipFormat = 0;

class function TPerformedDropEffectClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddWrite];
end;

function TPerformedDropEffectClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_PERFORMEDDROPEFFECT = 0) then
    CF_PERFORMEDDROPEFFECT := RegisterClipboardFormat(PChar('Preferred DropEffect'));
  Result := CF_PERFORMEDDROPEFFECT;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TLogicalPerformedDropEffectClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_LOGICALPERFORMEDDROPEFFECT: TClipFormat = 0;

class function TLogicalPerformedDropEffectClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddWrite];
end;

function TLogicalPerformedDropEffectClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_LOGICALPERFORMEDDROPEFFECT = 0) then
    CF_LOGICALPERFORMEDDROPEFFECT := RegisterClipboardFormat('Logical Performed DropEffect'); // *** DO NOT LOCALIZE ***
  Result := CF_LOGICALPERFORMEDDROPEFFECT;
end;



////////////////////////////////////////////////////////////////////////////////
//
//              TPasteSucceededClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_PASTESUCCEEDED: TClipFormat = 0;

class function TPasteSucceededClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddWrite];
end;

function TPasteSucceededClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_PASTESUCCEEDED = 0) then
    CF_PASTESUCCEEDED := RegisterClipboardFormat(PChar('Paste Succeeded'));
  Result := CF_PASTESUCCEEDED;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TInShellDragLoopClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_InDragLoop: TClipFormat = 0;

function TInShellDragLoopClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_InDragLoop = 0) then
    CF_InDragLoop := RegisterClipboardFormat(PChar('InShellDragLoop'));
  Result := CF_InDragLoop;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TTargetCLSIDClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TTargetCLSIDClipboardFormat.Clear;
begin
  FCLSID := GUID_NULL;
end;

var
  CF_TargetCLSID: TClipFormat = 0;

class function TTargetCLSIDClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddWrite];
end;

function TTargetCLSIDClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_TargetCLSID = 0) then
    CF_TargetCLSID := RegisterClipboardFormat('TargetCLSID'); // *** DO NOT LOCALIZE ***
  Result := CF_TargetCLSID;
end;

function TTargetCLSIDClipboardFormat.GetSize: integer;
begin
  Result := SizeOf(TCLSID);
end;

function TTargetCLSIDClipboardFormat.HasData: boolean;
begin
  Result := not IsEqualCLSID(FCLSID, GUID_NULL);
end;

function TTargetCLSIDClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  // Validate size.
  Result := (Size = SizeOf(TCLSID));
  if (Result) then
    FCLSID := PCLSID(Value)^;
end;

function TTargetCLSIDClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  // Validate size.
  Result := (Size = SizeOf(TCLSID));
  if (Result) then
    PCLSID(Value)^ := FCLSID;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDataStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TDataStreamDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FStreams := TStreamList.Create;
  FStreams.OnChanging := DoOnChanging;
end;

destructor TDataStreamDataFormat.Destroy;
begin
  Clear;
  FStreams.Free;
  inherited Destroy;
end;

procedure TDataStreamDataFormat.Clear;
begin
  Changing;
  FStreams.Clear;
end;

function TDataStreamDataFormat.HasData: boolean;
begin
  Result := (Streams.Count > 0);
end;

function TDataStreamDataFormat.NeedsData: boolean;
begin
  Result := (Streams.Count = 0);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFeedbackDataFormat
//
////////////////////////////////////////////////////////////////////////////////
function TFeedbackDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TPreferredDropEffectClipboardFormat) then
    FPreferredDropEffect := TPreferredDropEffectClipboardFormat(Source).Value

  else if (Source is TPerformedDropEffectClipboardFormat) then
    FPerformedDropEffect := TPerformedDropEffectClipboardFormat(Source).Value

  else if (Source is TLogicalPerformedDropEffectClipboardFormat) then
    FLogicalPerformedDropEffect := TLogicalPerformedDropEffectClipboardFormat(Source).Value

  else if (Source is TPasteSucceededClipboardFormat) then
    FPasteSucceeded := TPasteSucceededClipboardFormat(Source).Value

  else if (Source is TTargetCLSIDClipboardFormat) then
    FTargetCLSID := TTargetCLSIDClipboardFormat(Source).CLSID

  else if (Source is TInShellDragLoopClipboardFormat) then
  begin
    FInShellDragLoop := TInShellDragLoopClipboardFormat(Source).InShellDragLoop;
    FGotInShellDragLoop := True;
  end else
    Result := inherited Assign(Source);
end;

function TFeedbackDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TPreferredDropEffectClipboardFormat) then
    TPreferredDropEffectClipboardFormat(Dest).Value := FPreferredDropEffect

  else if (Dest is TPerformedDropEffectClipboardFormat) then
    TPerformedDropEffectClipboardFormat(Dest).Value := FPerformedDropEffect

  else if (Dest is TLogicalPerformedDropEffectClipboardFormat) then
    TLogicalPerformedDropEffectClipboardFormat(Dest).Value := FLogicalPerformedDropEffect

  else if (Dest is TPasteSucceededClipboardFormat) then
    TPasteSucceededClipboardFormat(Dest).Value := FPasteSucceeded

  else if (Dest is TTargetCLSIDClipboardFormat) then
    TTargetCLSIDClipboardFormat(Dest).CLSID := FTargetCLSID

  else if (Dest is TInShellDragLoopClipboardFormat) then
    TInShellDragLoopClipboardFormat(Dest).InShellDragLoop := FInShellDragLoop

  else
    Result := inherited AssignTo(Dest);
end;

procedure TFeedbackDataFormat.Clear;
begin
  Changing;
  FPreferredDropEffect := DROPEFFECT_NONE;
  FPerformedDropEffect := DROPEFFECT_NONE;
  FInShellDragLoop := False;
  FGotInShellDragLoop := False;
end;

procedure TFeedbackDataFormat.SetInShellDragLoop(const Value: boolean);
begin
  Changing;
  FInShellDragLoop := Value;
end;

procedure TFeedbackDataFormat.SetPasteSucceeded(const Value: longInt);
begin
  Changing;
  FPasteSucceeded := Value;
end;

procedure TFeedbackDataFormat.SetPerformedDropEffect(
  const Value: longInt);
begin
  Changing;
  FPerformedDropEffect := Value;
end;

procedure TFeedbackDataFormat.SetLogicalPerformedDropEffect(
  const Value: longInt);
begin
  Changing;
  FLogicalPerformedDropEffect := Value;
end;

procedure TFeedbackDataFormat.SetPreferredDropEffect(
  const Value: longInt);
begin
  Changing;
  FPreferredDropEffect := Value;
end;

procedure TFeedbackDataFormat.SetTargetCLSID(const Value: TCLSID);
begin
  Changing;
  FTargetCLSID := Value;
end;

function TFeedbackDataFormat.HasData: boolean;
begin
  Result := (FPreferredDropEffect <> DROPEFFECT_NONE) or
    (FPerformedDropEffect <> DROPEFFECT_NONE) or
    (FPasteSucceeded <> DROPEFFECT_NONE) or
    (FGotInShellDragLoop);
end;

function TFeedbackDataFormat.NeedsData: boolean;
begin
  Result := (FPreferredDropEffect = DROPEFFECT_NONE) or
    (FPerformedDropEffect = DROPEFFECT_NONE) or
    (FPasteSucceeded = DROPEFFECT_NONE) or
    (not FGotInShellDragLoop);
end;


class procedure TFeedbackDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TPreferredDropEffectClipboardFormat);
  RegisterDataConversion(TPerformedDropEffectClipboardFormat);
  RegisterDataConversion(TPasteSucceededClipboardFormat);
  RegisterDataConversion(TInShellDragLoopClipboardFormat);
  RegisterDataConversion(TTargetCLSIDClipboardFormat);
  RegisterDataConversion(TLogicalPerformedDropEffectClipboardFormat);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TGenericClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TGenericClipboardFormat.SetClipboardFormatName(const Value: string);
begin
  FFormat := Value;
  if (FFormat <> '') then
    ClipboardFormat := RegisterClipboardFormat(PChar(FFormat));
end;

function TGenericClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (FFormatEtc.cfFormat = 0) and (FFormat <> '') then
    FFormatEtc.cfFormat := RegisterClipboardFormat(PChar(FFormat));
  Result := FFormatEtc.cfFormat;
end;

function TGenericClipboardFormat.GetClipboardFormatName: string;
begin
  Result := FFormat;
end;

function TGenericClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TGenericDataFormat) then
  begin
    Data := TGenericDataFormat(Source).Data;
    Result := True;
  end else
    Result := inherited Assign(Source);
end;

function TGenericClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TGenericDataFormat) then
  begin
    TGenericDataFormat(Dest).Data := Data;
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;

class function TGenericClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddRead, ddWrite];
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TGenericDataFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TGenericDataFormat.AddFormat(const AFormat: string);
var
  ClipboardFormat: TGenericClipboardFormat;
begin
  ClipboardFormat := TGenericClipboardFormat.Create;
  ClipboardFormat.ClipboardFormatName := AFormat;
  CompatibleFormats.Add(ClipboardFormat);
end;

procedure TGenericDataFormat.Clear;
begin
  Changing;
  FData := '';
end;

function TGenericDataFormat.HasData: boolean;
begin
  Result := (FData <> '');
end;

function TGenericDataFormat.NeedsData: boolean;
begin
  Result := (FData = '');
end;

procedure TGenericDataFormat.DoSetData(const Value: AnsiString);
begin
  Changing;
  FData := Value;
end;

procedure TGenericDataFormat.SetDataHere(const AData; ASize: integer);
begin
  Changing;
  SetLength(FData, ASize);
  Move(AData, PByte(FData)^, ASize);
end;

function TGenericDataFormat.GetSize: integer;
begin
  Result := length(FData);
end;

function TGenericDataFormat.GetDataHere(var AData; ASize: integer): integer;
begin
  Result := Size;
  if (ASize < Result) then
    Result := ASize;
  Move(PByte(FData)^, AData, Result);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              Data format registration
//
////////////////////////////////////////////////////////////////////////////////

initialization
  // Data format registration
  TDataStreamDataFormat.RegisterDataFormat;
  TFeedbackDataFormat.RegisterDataFormat;

  // Clipboard format registration
  TPreferredDropEffectClipboardFormat.RegisterFormat;
  TPerformedDropEffectClipboardFormat.RegisterFormat;
  TLogicalPerformedDropEffectClipboardFormat.RegisterFormat;
  TPasteSucceededClipboardFormat.RegisterFormat;
  TInShellDragLoopClipboardFormat.RegisterFormat;
  TTargetCLSIDClipboardFormat.RegisterFormat;

finalization
end.

