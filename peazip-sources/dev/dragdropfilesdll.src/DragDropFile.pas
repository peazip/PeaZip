unit DragDropFile;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropFile
// Description:     Implements Dragging and Dropping of files and folders.
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
  DropTarget,
  DropSource,
  DragDropFormats,
  DragDropText,
  ShlObj,
  ActiveX,
  Windows,
  Classes;

{$include DragDrop.inc}

////////////////////////////////////////////////////////////////////////////////
//
//              Version independent string lists for Ansi and Unicode
//
////////////////////////////////////////////////////////////////////////////////
type
  // Note: TAnsiStrings and TAnsiStringList is unused and thus untested.
  TAnsiStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    function GetCapacity: Integer; virtual;
    procedure SetCapacity(const Value: Integer); virtual;
    function GetCount: Integer; virtual; abstract;
    function Get(Index: Integer): AnsiString; virtual; abstract;
    function GetTextStr: AnsiString;
    procedure Put(Index: Integer; const Value: AnsiString); virtual; abstract;
    procedure SetTextStr(const Value: AnsiString);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property UpdateCount: Integer read FUpdateCount;
    procedure Changed; virtual;
    procedure Changing; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function Add(const S: AnsiString): Integer; virtual;
//    function GetEnumerator: TAnsiStringsEnumerator;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IndexOf(const S: AnsiString): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TAnsiStringList = class(TAnsiStrings)
  private
    FStrings: array of AnsiString;
    FCount: integer;
  protected
    procedure Grow;
    function GetCapacity: Integer; override;
    procedure SetCapacity(const Value: Integer); override;
    function GetCount: Integer; override;
    function Get(Index: Integer): AnsiString; override;
    procedure Put(Index: Integer; const Value: AnsiString); override;
  public
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

type
  TUnicodeStrings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FUpdateCount: Integer;
  protected
    function GetCapacity: Integer; virtual;
    procedure SetCapacity(const Value: Integer); virtual;
    function GetCount: Integer; virtual; abstract;
    function Get(Index: Integer): UnicodeString; virtual; abstract;
    function GetTextStr: UnicodeString;
    procedure Put(Index: Integer; const Value: UnicodeString); virtual; abstract;
    procedure SetTextStr(const Value: UnicodeString);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property UpdateCount: Integer read FUpdateCount;
    procedure Changed; virtual;
    procedure Changing; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function Add(const S: UnicodeString): Integer; virtual;
//    function GetEnumerator: TAnsiStringsEnumerator;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IndexOf(const S: UnicodeString): Integer; virtual;
    procedure Insert(Index: Integer; const S: UnicodeString); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Exchange(Index1, Index2: Integer); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: UnicodeString read Get write Put; default;
    property Text: UnicodeString read GetTextStr write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TUnicodeStringList = class(TUnicodeStrings)
  private
    FStrings: array of UnicodeString;
    FCount: integer;
  protected
    procedure Grow;
    function GetCapacity: Integer; override;
    procedure SetCapacity(const Value: Integer); override;
    function GetCount: Integer; override;
    function Get(Index: Integer): UnicodeString; override;
    procedure Put(Index: Integer; const Value: UnicodeString); override;
  public
    procedure Insert(Index: Integer; const S: UnicodeString); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

type
  TUnicodeStringsAdapter = class(TUnicodeStrings)
  private
    FStrings: TStrings;
  protected
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function Get(Index: Integer): UnicodeString; override;
    procedure Put(Index: Integer; const Value: UnicodeString); override;
  public
    constructor Create(AStrings: TStrings);
    procedure Insert(Index: Integer; const S: UnicodeString); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

type
  TFileStrings = TAnsiStrings;
  TFileStringList = TAnsiStringList;

type
  TFileStringsAdapter = class(TFileStrings)
  private
    FStrings: TStrings;
  protected
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const Value: string); override;
  public
    constructor Create(AStrings: TStrings);
    procedure Insert(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;


type
////////////////////////////////////////////////////////////////////////////////
//
//              TFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFiles: TUnicodeStrings;
    FWide: boolean;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
    property Wide: boolean read FWide;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property Files: TUnicodeStrings read FFiles;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TAnsiFilenameClipboardFormat = class(TCustomAnsiTextClipboardFormat)
  private
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Filename: AnsiString read GetString write SetString;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TUnicodeFilenameClipboardFormat = class(TCustomUnicodeTextClipboardFormat)
  private
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Filename: UnicodeString read GetText write SetText;
  end;

  TFilenameWClipboardFormat = TUnicodeFilenameClipboardFormat deprecated ;

////////////////////////////////////////////////////////////////////////////////
//
//              TFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFilenameClipboardFormat = TUnicodeFilenameClipboardFormat;
{$else}
  TFilenameClipboardFormat = TAnsiFilenameClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TAnsiFilenameMapClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFileMaps: TStrings;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property FileMaps: TStrings read FFileMaps;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TUnicodeFilenameMapClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFileMaps: TUnicodeStrings;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property FileMaps: TUnicodeStrings read FFileMaps;
  end;

  TFilenameMapWClipboardFormat = TUnicodeFilenameMapClipboardFormat deprecated;

//////////////////////////////////////////////////////////////////////////////
//
//              TFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFilenameMapClipboardFormat = TUnicodeFilenameMapClipboardFormat;
{$else}
  TFilenameMapClipboardFormat = TAnsiFilenameMapClipboardFormat;
{$endif}


////////////////////////////////////////////////////////////////////////////////
//
//              TFileMapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileMapDataFormat = class(TCustomDataFormat)
  private
    FFileMaps: TUnicodeStrings;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property FileMaps: TUnicodeStrings read FFileMaps;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileDataFormat = class(TCustomDataFormat)
  private
    FFiles: TUnicodeStrings;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property Files: TUnicodeStrings read FFiles;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileGroupDescriptorCustomClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileGroupDescriptorCustomClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FBuffer: pointer;
  protected
    function GetFileDescriptor(Index: integer): pointer;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetFilename(Index: integer): string; virtual; abstract;
    procedure SetFilename(Index: integer; const Value: string); virtual; abstract;
    function GetBufferSize(Count: integer): integer; virtual; abstract;
    function GetSize: integer; override;
    procedure CopyFrom(Source: pointer);
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    property FileGroupDescriptor: pointer read FBuffer;
    property FileDescriptors[Index: integer]: pointer read GetFileDescriptor;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property Count: integer read GetCount write SetCount;
    property Filenames[Index: integer]: string read GetFilename write SetFilename;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
const
  // Missing declaration from shlobj.pas (D6 and earlier)
  FD_PROGRESSUI = $4000; // Show Progress UI w/Drag and Drop

type
  TAnsiFileGroupDescriptorClipboardFormat = class(TFileGroupDescriptorCustomClipboardFormat)
  private
  protected
    function GetAnsiFilename(Index: integer): AnsiString;
    procedure SetAnsiFilename(Index: integer; const Value: AnsiString);
    function GetFileDescriptor(Index: integer): PFileDescriptorA;
    function GetFileGroupDescriptor: PFileGroupDescriptorA;
    function GetBufferSize(Count: integer): integer; override;
    function GetFilename(Index: integer): string; override;
    procedure SetFilename(Index: integer; const Value: string); override;
  public
    function GetClipboardFormat: TClipFormat; override;
    procedure CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorA);
    property FileGroupDescriptor: PFileGroupDescriptorA read GetFileGroupDescriptor;
    property FileDescriptors[Index: integer]: PFileDescriptorA read GetFileDescriptor;
    property Filenames[Index: integer]: AnsiString read GetAnsiFilename write SetAnsiFilename;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  // Warning: TFileGroupDescriptorW has wrong declaration in ShlObj.pas!
  TFileGroupDescriptorW = record
    cItems: UINT;
    fgd: array[0..0] of TFileDescriptorW;
  end;

  PFileGroupDescriptorW = ^TFileGroupDescriptorW;

  TUnicodeFileGroupDescriptorClipboardFormat = class(TFileGroupDescriptorCustomClipboardFormat)
  private
  protected
    function GetUnicodeFilename(Index: integer): UnicodeString;
    procedure SetUnicodeFilename(Index: integer; const Value: UnicodeString);
    function GetFileDescriptor(Index: integer): PFileDescriptorW;
    function GetFileGroupDescriptor: PFileGroupDescriptorW;
    function GetBufferSize(Count: integer): integer; override;
    function GetFilename(Index: integer): string; override;
    procedure SetFilename(Index: integer; const Value: string); override;
  public
    function GetClipboardFormat: TClipFormat; override;
    procedure CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorW);
    property FileGroupDescriptor: PFileGroupDescriptorW read GetFileGroupDescriptor;
    property FileDescriptors[Index: integer]: PFileDescriptorW read GetFileDescriptor;
    property Filenames[Index: integer]: UnicodeString read GetUnicodeFilename write SetUnicodeFilename;
  end;

  TFileGroupDescriptorWClipboardFormat = TUnicodeFileGroupDescriptorClipboardFormat deprecated;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFileGroupDescriptorClipboardFormat = TUnicodeFileGroupDescriptorClipboardFormat;
{$else}
  TFileGroupDescriptorClipboardFormat = TAnsiFileGroupDescriptorClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsClipboardFormat = class(TCustomAnsiStringClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    constructor Create; override;
    property Data;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStreamClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsStreamClipboardFormat = class(TClipboardFormat)
  private
    FStreams: TStreamList;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Streams: TStreamList read FStreams;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStreamOnDemandClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TVirtualFileStreamDataFormat = class;
  TFileContentsStreamOnDemandClipboardFormat = class;

  TOnGetStreamEvent = procedure(Sender: TFileContentsStreamOnDemandClipboardFormat;
    Index: integer; out AStream: IStream) of object;

  TFileContentsStreamOnDemandClipboardFormat = class(TClipboardFormat)
  private
    FOnGetStream: TOnGetStreamEvent;
    FGotData: boolean;
    FDataRequested: boolean;
  protected
    function DoSetData(const AFormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    function Assign(Source: TCustomDataFormat): boolean; override;

    function GetStream(Index: integer): IStream;

    property OnGetStream: TOnGetStreamEvent read FOnGetStream write FOnGetStream;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStorageClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsStorageClipboardFormat = class(TClipboardFormat)
  private
    FStorages: TStorageInterfaceList;
  protected
    function DoSetData(const AFormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Storages: TStorageInterfaceList read FStorages;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TVirtualFileStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TVirtualFileStreamDataFormat = class(TCustomDataFormat)
  private
    FFileDescriptors: TMemoryList;
    FFileNames: TStrings;
    FFileContentsClipboardFormat: TFileContentsStreamOnDemandClipboardFormat;
    FAnsiFileGroupDescriptorClipboardFormat: TAnsiFileGroupDescriptorClipboardFormat;
    FUnicodeFileGroupDescriptorClipboardFormat: TUnicodeFileGroupDescriptorClipboardFormat;
    FHasContents: boolean;
  protected
    class procedure RegisterCompatibleFormats; override;
    procedure SetFileNames(const Value: TStrings);
    function GetOnGetStream: TOnGetStreamEvent;
    procedure SetOnGetStream(const Value: TOnGetStreamEvent);
    function GetFileDescriptor(Index: integer): PFileDescriptorW;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;

    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property FileDescriptors[Index: integer]: PFileDescriptorW read GetFileDescriptor;
    property FileNames: TStrings read FFileNames write SetFileNames;
    property FileContentsClipboardFormat: TFileContentsStreamOnDemandClipboardFormat
      read FFileContentsClipboardFormat;
    property AnsiFileGroupDescriptorClipboardFormat: TAnsiFileGroupDescriptorClipboardFormat
      read FAnsiFileGroupDescriptorClipboardFormat;
    property UnicodeFileGroupDescriptorClipboardFormat: TUnicodeFileGroupDescriptorClipboardFormat
      read FUnicodeFileGroupDescriptorClipboardFormat;
{$ifdef UNICODE}
    property FileGroupDescriptorClipboardFormat: TFileGroupDescriptorClipboardFormat
      read FUnicodeFileGroupDescriptorClipboardFormat;
{$else}
    property FileGroupDescriptorClipboardFormat: TFileGroupDescriptorClipboardFormat
      read FAnsiFileGroupDescriptorClipboardFormat;
{$endif}
    property OnGetStream: TOnGetStreamEvent read GetOnGetStream write SetOnGetStream;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropFileTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropFileTarget = class(TCustomDropMultiTarget)
  private
    FFileFormat: TFileDataFormat;
    FFileMapFormat: TFileMapDataFormat;
  protected
    function GetFiles: TUnicodeStrings;
    function GetMappedNames: TUnicodeStrings;
    function GetPreferredDropEffect: LongInt; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TUnicodeStrings read GetFiles;
    property MappedNames: TUnicodeStrings read GetMappedNames;
    property OptimizedMove default True;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropFileSource
//
////////////////////////////////////////////////////////////////////////////////
  TDropFileSource = class(TCustomDropMultiSource)
  private
    FFileFormat: TFileDataFormat;
    FFileMapFormat: TFileMapDataFormat;
    function GetFiles: TUnicodeStrings;
    function GetMappedNames: TUnicodeStrings;
  protected
    procedure SetFiles(AFiles: TUnicodeStrings);
    procedure SetMappedNames(ANames: TUnicodeStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Files: TUnicodeStrings read GetFiles write SetFiles;
    // MappedNames is only needed if files need to be renamed during a drag op.
    // E.g. dragging from 'Recycle Bin'.
    property MappedNames: TUnicodeStrings read GetMappedNames write SetMappedNames;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////
function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TUnicodeStrings): boolean; overload;
function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean; overload;

function ReadFilesFromData(Data: pointer; Size: integer; Files: TUnicodeStrings): boolean; overload;
function ReadFilesFromData(Data: pointer; Size: integer; Files: TStrings): boolean; overload;

function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TUnicodeStrings): boolean; overload;
function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TStrings): boolean; overload;

function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TUnicodeStrings): boolean; overload;
function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TStrings): boolean; overload;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//                      IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  RTLConsts,
  DragDropPIDL,
  ComObj,
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiStrings
//
////////////////////////////////////////////////////////////////////////////////
function TAnsiStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

procedure TAnsiStrings.Assign(Source: TPersistent);
var
  i: integer;
begin
  if (Source is TAnsiStrings) then
  begin
    BeginUpdate;
    try
      Clear;
      Capacity := TAnsiStrings(Source).Count;
      for i := 0 to TAnsiStrings(Source).Count-1 do
        Add(TAnsiStrings(Source).Strings[i]);
    finally
      EndUpdate;
    end;
  end else
  if (Source is TStrings) then
  begin
    BeginUpdate;
    try
      Clear;
      Capacity := TStrings(Source).Count;
      for i := 0 to TStrings(Source).Count-1 do
        Add(AnsiString(TStrings(Source).Strings[i]));
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TAnsiStrings.AssignTo(Dest: TPersistent);
var
  i: integer;
begin
  if (Dest is TStrings) then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      TStrings(Dest).Capacity := Count;
      for i := 0 to Count-1 do
        TStrings(Dest).Add(String(Strings[i]));
    finally
      TStrings(Dest).EndUpdate;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TAnsiStrings.BeginUpdate;
begin
  if (FUpdateCount = 0) then
    Changing;
  Inc(FUpdateCount);
end;

procedure TAnsiStrings.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAnsiStrings.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TAnsiStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
    Changed;
end;

procedure TAnsiStrings.Exchange(Index1, Index2: Integer);
var
  Temp: AnsiString;
begin
  if (Index1 < 0) or (Index1 >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index2]);

  BeginUpdate;
  try
    Temp := Strings[Index1];
    Strings[Index2] := Strings[Index1];
    Strings[Index1] := Temp;
  finally
    EndUpdate;
  end;
end;

function TAnsiStrings.GetCapacity: Integer;
begin
  Result := Count;
end;

function TAnsiStrings.GetTextStr: AnsiString;
var
  Size: integer;
  i, j: integer;
  p: PAnsiChar;
  s: AnsiString;
begin
  Size := (Count-1)*Length(sLineBreak); // Size of the CRs
  for i := 0 to Count-1 do
    inc(Size, Length(Strings[i]));
  SetLength(Result, Size);
  p := PAnsiChar(Result);
  for i := 0 to Count-1 do
  begin
    if (i > 0) then
      for j := 1 to Length(sLineBreak) do
      begin
        p^ := AnsiChar(sLineBreak[j]);
        inc(p);
      end;
    s := Strings[i];
    Size := Length(s);
    System.Move(PAnsiChar(s)^, p^, Size*SizeOf(AnsiChar));
    inc(p, Size);
  end;
end;

function TAnsiStrings.IndexOf(const S: AnsiString): Integer;
begin
  Result := Count-1;
  while (Result >= 0) and (Strings[Result] <> S) do
    Dec(Result);
end;

procedure TAnsiStrings.Move(CurIndex, NewIndex: Integer);
var
  Temp: AnsiString;
begin
  if (CurIndex < 0) or (CurIndex >= Count) then
    raise Exception.CreateFmt(SListIndexError, [CurIndex]);
  if (NewIndex < 0) or (NewIndex >= Count) then
    raise Exception.CreateFmt(SListIndexError, [NewIndex]);

  // TODO : Very inefficient
  BeginUpdate;
  try
    Temp := Strings[CurIndex];
    Delete(CurIndex);
    Insert(NewIndex, Temp);
  finally
    EndUpdate;
  end;
end;

procedure TAnsiStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAnsiStrings.SaveToStream(Stream: TStream);
var
  Buffer: AnsiString;
begin
  Buffer := GetTextStr;
  Stream.Write(PAnsiChar(Buffer)^, Length(Buffer)*SizeOf(AnsiChar));
end;

procedure TAnsiStrings.SetCapacity(const Value: Integer);
begin
end;

procedure TAnsiStrings.SetTextStr(const Value: AnsiString);
var
  LineCount: integer;
  p, Start: PAnsiChar;
  s: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    // Count number of lines
    p := PAnsiChar(Value);
    if (p^ = #0) then
      exit;
    LineCount := 1;
    while (p^ <> #0) do
    begin
      if (p^ in [#10, #13]) then
        inc(LineCount);
      inc(p);
      if (p^ = #10) then
        inc(p);
    end;
    Capacity := LineCount;
    // Extract lines
    p := PAnsiChar(Value);
    while (p^ <> #0) do
    begin
      Start := p;
      while not (p^ in [#0, #10, #13]) do
        inc(p);
      SetString(s, Start, p-Start);
      Add(s);
      if (p^ = #13) then
        inc(p);
      if (p^ = #10) then
        Inc(p);
    end;
  finally
    EndUpdate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiStringList
//
////////////////////////////////////////////////////////////////////////////////
procedure TAnsiStringList.Clear;
var
  i: integer;
begin
  Changing;
  for i := 0 to FCount-1 do
    FStrings[i] := '';
  FCount := 0;
  Changed;
end;

procedure TAnsiStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);
  ASSERT(FCount > 0);

  Changing;
  Dec(FCount);

  while (Index < FCount) do
  begin
    FStrings[Index] := FStrings[Index+1];
    inc(Index);
  end;
  FStrings[FCount] := '';
  Changed;
end;

function TAnsiStringList.Get(Index: Integer): AnsiString;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FStrings[Index];
end;

function TAnsiStringList.GetCapacity: Integer;
begin
  Result := Length(FStrings);
end;

function TAnsiStringList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TAnsiStringList.Grow;
var
  Delta: Integer;
begin
  if (Length(FStrings) > 64) then
    Delta := Length(FStrings) div 4
  else
    if (Length(FStrings) > 8) then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(Length(FStrings) + Delta);
end;

procedure TAnsiStringList.Insert(Index: Integer; const S: AnsiString);
var
  i: integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Changing;
  if (FCount = Capacity) then
    Grow;

  for i := Index to FCount-1 do
    FStrings[i+1] := FStrings[i];

  FStrings[Index] := S;
  inc(FCount);
  Changed;
end;

procedure TAnsiStringList.Put(Index: Integer; const Value: AnsiString);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Changing;
  FStrings[Index] := Value;
  Changed;
end;

procedure TAnsiStringList.SetCapacity(const Value: Integer);
begin
  ASSERT(FCount <= Value);
  SetLength(FStrings, Value);
end;



////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeStrings
//
////////////////////////////////////////////////////////////////////////////////
function TUnicodeStrings.Add(const S: UnicodeString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

procedure TUnicodeStrings.Assign(Source: TPersistent);
var
  i: integer;
begin
  if (Source is TUnicodeStrings) then
  begin
    BeginUpdate;
    try
      Clear;
      Capacity := TUnicodeStrings(Source).Count;
      for i := 0 to TUnicodeStrings(Source).Count-1 do
        Add(TUnicodeStrings(Source).Strings[i]);
    finally
      EndUpdate;
    end;
  end else
  if (Source is TStrings) then
  begin
    BeginUpdate;
    try
      Clear;
      Capacity := TStrings(Source).Count;
      for i := 0 to TStrings(Source).Count-1 do
        Add(TStrings(Source).Strings[i]);
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TUnicodeStrings.AssignTo(Dest: TPersistent);
var
  i: integer;
begin
  if (Dest is TStrings) then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      TStrings(Dest).Capacity := Count;
      for i := 0 to Count-1 do
        TStrings(Dest).Add(Strings[i]);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end else
    inherited AssignTo(Dest);
end;

procedure TUnicodeStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    Changing;
  Inc(FUpdateCount);
end;

procedure TUnicodeStrings.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TUnicodeStrings.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TUnicodeStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TUnicodeStrings.Exchange(Index1, Index2: Integer);
var
  Temp: UnicodeString;
begin
  if (Index1 < 0) or (Index1 >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= Count) then
    raise Exception.CreateFmt(SListIndexError, [Index2]);

  BeginUpdate;
  try
    Temp := Strings[Index1];
    Strings[Index2] := Strings[Index1];
    Strings[Index1] := Temp;
  finally
    EndUpdate;
  end;
end;

function TUnicodeStrings.GetCapacity: Integer;
begin
  Result := Count;
end;

function TUnicodeStrings.GetTextStr: UnicodeString;
var
  Size: integer;
  i, j: integer;
  p: PWideChar;
  s: UnicodeString;
begin
  Size := (Count-1)*Length(sLineBreak); // Size of the CRs
  for i := 0 to Count-1 do
    inc(Size, Length(Strings[i]));
  SetLength(Result, Size);
  p := PWideChar(Result);
  for i := 0 to Count-1 do
  begin
    if (i > 0) then
      (* What we would like to do:
      StrPLCopy(p, sLineBreak, Length(sLineBreak));
      inc(p, Length(sLineBreak));
      What we have to do to maintain backward compatibility with pre-D2009:
      *)
      for j := 1 to Length(sLineBreak) do
      begin
        p^ := WideChar(sLineBreak[j]);
        inc(p);
      end;
    s := Strings[i];
    Size := Length(s);
    System.Move(PWideChar(s)^, p^, Size*SizeOf(WideChar));
    inc(p, Size);
  end;
end;

function TUnicodeStrings.IndexOf(const S: UnicodeString): Integer;
begin
  Result := Count-1;
  while (Result >= 0) and (Strings[Result] <> S) do
    Dec(Result);
end;

procedure TUnicodeStrings.Move(CurIndex, NewIndex: Integer);
var
  Temp: UnicodeString;
begin
  if (CurIndex < 0) or (CurIndex >= Count) then
    raise Exception.CreateFmt(SListIndexError, [CurIndex]);
  if (NewIndex < 0) or (NewIndex >= Count) then
    raise Exception.CreateFmt(SListIndexError, [NewIndex]);

  // TODO : Very inefficient
  BeginUpdate;
  try
    Temp := Strings[CurIndex];
    Delete(CurIndex);
    Insert(NewIndex, Temp);
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TUnicodeStrings.SaveToStream(Stream: TStream);
var
  Buffer: UnicodeString;
begin
  Buffer := GetTextStr;
  Stream.Write(PWideChar(Buffer)^, Length(Buffer)*SizeOf(WideChar));
end;

procedure TUnicodeStrings.SetCapacity(const Value: Integer);
begin
end;

procedure TUnicodeStrings.SetTextStr(const Value: UnicodeString);
var
  LineCount: integer;
  p, Start: PWideChar;
  s: UnicodeString;
begin
  BeginUpdate;
  try
    Clear;
    // Count number of lines
    p := PWideChar(Value);
    if (p^ = #0) then
      exit;
    LineCount := 1;
    while (p^ <> #0) do
    begin
      if (Ord(p^) <= 13) and (Byte(Ord(p^)) in [10, 13]) then
        inc(LineCount);
      inc(p);
      if (p^ = #10) then
        inc(p);
    end;
    Capacity := LineCount;
    // Extract lines
    p := PWideChar(Value);
    while (p^ <> #0) do
    begin
      Start := p;
      while (Ord(p^) > 13) or not (Byte(Ord(p^)) in [0, 10, 13]) do
        inc(p);
      SetString(s, Start, p-Start);
      Add(s);
      if (p^ = #13) then
        inc(p);
      if (p^ = #10) then
        Inc(p);
    end;
  finally
    EndUpdate;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeStringList
//
////////////////////////////////////////////////////////////////////////////////
procedure TUnicodeStringList.Clear;
var
  i: integer;
begin
  Changing;
  for i := 0 to FCount-1 do
    FStrings[i] := '';
  FCount := 0;
  Changed;
end;

procedure TUnicodeStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);
  ASSERT(FCount > 0);

  Changing;
  Dec(FCount);

  while (Index < FCount) do
  begin
    FStrings[Index] := FStrings[Index+1];
    inc(Index);
  end;
  FStrings[FCount] := '';
  Changed;
end;

function TUnicodeStringList.Get(Index: Integer): UnicodeString;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FStrings[Index];
end;

function TUnicodeStringList.GetCapacity: Integer;
begin
  Result := Length(FStrings);
end;

function TUnicodeStringList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TUnicodeStringList.Grow;
var
  Delta: Integer;
begin
  if (Length(FStrings) > 64) then
    Delta := Length(FStrings) div 4
  else
    if (Length(FStrings) > 8) then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(Length(FStrings) + Delta);
end;

procedure TUnicodeStringList.Insert(Index: Integer; const S: UnicodeString);
var
  i: integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Changing;
  if (FCount = Capacity) then
    Grow;

  for i := Index to FCount-1 do
    FStrings[i+1] := FStrings[i];

  FStrings[Index] := S;
  inc(FCount);
  Changed;
end;

procedure TUnicodeStringList.Put(Index: Integer; const Value: UnicodeString);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Changing;
  FStrings[Index] := Value;
  Changed;
end;

procedure TUnicodeStringList.SetCapacity(const Value: Integer);
begin
  ASSERT(FCount <= Value);
  SetLength(FStrings, Value);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeStringsAdapter
//
////////////////////////////////////////////////////////////////////////////////
procedure TUnicodeStringsAdapter.Clear;
begin
  FStrings.Clear;
end;

constructor TUnicodeStringsAdapter.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

procedure TUnicodeStringsAdapter.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
end;

function TUnicodeStringsAdapter.Get(Index: Integer): UnicodeString;
begin
  Result := FStrings.Strings[Index];
end;

function TUnicodeStringsAdapter.GetCapacity: Integer;
begin
  Result := FStrings.Capacity;
end;

function TUnicodeStringsAdapter.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

procedure TUnicodeStringsAdapter.Insert(Index: Integer; const S: UnicodeString);
begin
  FStrings.Insert(Index, S);
end;

procedure TUnicodeStringsAdapter.Put(Index: Integer; const Value: UnicodeString);
begin
  FStrings.Strings[Index] := Value;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileStringsAdapter
//
////////////////////////////////////////////////////////////////////////////////
procedure TFileStringsAdapter.Clear;
begin
  FStrings.Clear;
end;

constructor TFileStringsAdapter.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

procedure TFileStringsAdapter.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
end;

function TFileStringsAdapter.Get(Index: Integer): string;
begin
  Result := FStrings.Strings[Index];
end;

function TFileStringsAdapter.GetCapacity: Integer;
begin
  Result := FStrings.Capacity;
end;

function TFileStringsAdapter.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

procedure TFileStringsAdapter.Insert(Index: Integer; const S: string);
begin
  FStrings.Insert(Index, S);
end;

procedure TFileStringsAdapter.Put(Index: Integer; const Value: string);
begin
  FStrings.Strings[Index] := Value;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////

function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TUnicodeStrings): boolean;
var
  DropFiles: PDropFiles;
begin
  DropFiles := PDropFiles(GlobalLock(HGlob));
  try
    Result := ReadFilesFromData(DropFiles, GlobalSize(HGlob), Files)
  finally
    GlobalUnlock(HGlob);
  end;
end;

function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := ReadFilesFromHGlobal(HGlob, Adapter);
  finally
    Adapter.Free;
  end;
end;

function ReadFilesFromData(Data: pointer; Size: integer; Files: TUnicodeStrings): boolean;
var
  Wide: boolean;
begin
  Files.Clear;
  if (Data <> nil) then
  begin
    Wide := PDropFiles(Data)^.fWide;
    dec(Size, PDropFiles(Data)^.pFiles);
    inc(PByte(Data), PDropFiles(Data)^.pFiles);
    ReadFilesFromZeroList(Data, Size, Wide, Files);
  end;

  Result := (Files.Count > 0);
end;

function ReadFilesFromData(Data: pointer; Size: integer; Files: TStrings): boolean;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := ReadFilesFromData(Data, Size, Adapter);
  finally
    Adapter.Free;
  end;
end;

function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TUnicodeStrings): boolean;
var
  p: PAnsiChar;
  StringSize: integer;
begin
  Result := False;
  if (Data <> nil) then
  begin
    p := Data;
    while (Size > 0) and (p^ <> #0) do
    begin
      if (Wide) then
      begin
        Files.Add(PWideChar(p));
        StringSize := (Length(PWideChar(p)) + 1) * 2;
      end else
      begin
        Files.Add(String(p));
        StringSize := Length(p) + 1;
      end;
      inc(p, StringSize);
      dec(Size, StringSize);
      Result := True;
    end;
  end;
end;

function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TStrings): boolean;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := ReadFilesFromZeroList(Data, Size, Wide, Adapter);
  finally
    Adapter.Free;
  end;
end;

function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TUnicodeStrings): boolean;
var
  j: integer;
  ws: UnicodeString;
  pws: PWideChar;
  i: integer;
  p: PAnsiChar;
  pw: PWideChar;
  StringSize: integer;
  s: string;
begin
  Result := False;
  if (Data <> nil) then
  begin
    p := Data;
    i := 0;
    dec(Size);
    while (Size > 0) and (i < Files.Count) do
    begin
      if (Wide) then
      begin
        pw := PWideChar(p);
        ws := Files[i];
        pws := PWideChar(ws);
        j := Size;
        while (j > 0) and (pws^ <> #0) do
        begin
          pw^ := pws^;
          inc(pw);
          inc(pws);
          dec(j, SizeOf(WideChar));
        end;
        pw^ := #0;
        StringSize := (Length(ws)+1)*2;
      end else
      begin
        s := Files[i];
        StrPLCopy(p, AnsiString(s), Size);
        StringSize := Length(s)+1;
      end;
      inc(p, StringSize);
      dec(Size, StringSize);
      inc(i);
      Result := True;
    end;

    // Final teminating zero.
    if (Size >= 0) then
      PByte(p)^ := 0;
  end;
end;

function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TStrings): boolean;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := WriteFilesToZeroList(Data, Size, Wide, Adapter);
  finally
    Adapter.Free;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileClipboardFormat.Create;
begin
  inherited Create;
  FFiles := TUnicodeStringList.Create;
  // Note: Setting dwAspect to DVASPECT_SHORT will request that the data source
  // returns the file names in short (8.3) format.
  // FFormatEtc.dwAspect := DVASPECT_SHORT;
  FWide := (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

destructor TFileClipboardFormat.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TFileClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_HDROP;
end;

procedure TFileClipboardFormat.Clear;
begin
  FFiles.Clear;
end;

function TFileClipboardFormat.HasData: boolean;
begin
  Result := (FFiles.Count > 0);
end;

function TFileClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FFiles.Count-1 do
    inc(Result, Length(FFiles[i])+1);
  if (Wide) then
    // Wide strings
    Inc(Result, Result);
  inc(Result, SizeOf(TDropFiles)+2);
end;

function TFileClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size > SizeOf(TDropFiles));
  if (not Result) then
    exit;

  Result := ReadFilesFromData(Value, Size, FFiles);
end;

function TFileClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size > SizeOf(TDropFiles));
  if (not Result) then
    exit;

  FillChar(Value^, Size, 0);
  PDropFiles(Value)^.pfiles := SizeOf(TDropFiles);
  PDropFiles(Value)^.fwide := BOOL(ord(Wide));
  inc(PByte(Value), SizeOf(TDropFiles));
  dec(Size, SizeOf(TDropFiles));

  WriteFilesToZeroList(Value, Size, Wide, FFiles);
end;

function TFileClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    FFiles.Assign(TFileDataFormat(Source).Files);
    Result := True;
  end else
    Result := inherited Assign(Source);
end;

function TFileClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Assign(FFiles);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEA: TClipFormat = 0;

function TAnsiFilenameClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEA = 0) then
    CF_FILENAMEA := RegisterClipboardFormat(PChar('FileName'));
  Result := CF_FILENAMEA;
end;

class procedure TAnsiFilenameClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConversion(TFileDataFormat, 3+AnsiBoost);
end;

function TAnsiFilenameClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    Result := (TFileDataFormat(Source).Files.Count > 0);
    if (Result) then
      Filename := AnsiString(TFileDataFormat(Source).Files[0]);
  end else
    Result := inherited Assign(Source);
end;

function TAnsiFilenameClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Clear;
    TFileDataFormat(Dest).Files.Add(String(Filename));
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEW: TClipFormat = 0;

function TUnicodeFilenameClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEW = 0) then
    CF_FILENAMEW := RegisterClipboardFormat(PChar('FileName'));
  Result := CF_FILENAMEW;
end;

class procedure TUnicodeFilenameClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConversion(TFileDataFormat, 3+UnicodeBoost);
end;

function TUnicodeFilenameClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    Result := (TFileDataFormat(Source).Files.Count > 0);
    if (Result) then
      Filename := TFileDataFormat(Source).Files[0];
  end else
    Result := inherited Assign(Source);
end;

function TUnicodeFilenameClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Add(Filename);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEMAP: TClipFormat = 0;

constructor TAnsiFilenameMapClipboardFormat.Create;
begin
  inherited Create;
  FFileMaps := TStringList.Create;
end;

destructor TAnsiFilenameMapClipboardFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TAnsiFilenameMapClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEMAP = 0) then
    CF_FILENAMEMAP := RegisterClipboardFormat(Pchar('FileNameMap'));
  Result := CF_FILENAMEMAP;
end;

procedure TAnsiFilenameMapClipboardFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TAnsiFilenameMapClipboardFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TAnsiFilenameMapClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := FFileMaps.Count + 1;
  for i := 0 to FFileMaps.Count-1 do
    inc(Result, Length(FFileMaps[i]));
end;

function TAnsiFilenameMapClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := ReadFilesFromZeroList(Value, Size, False, FFileMaps);
end;

function TAnsiFilenameMapClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := WriteFilesToZeroList(Value, Size, False, FFileMaps);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEMAPW: TClipFormat = 0;

constructor TUnicodeFilenameMapClipboardFormat.Create;
begin
  inherited Create;
  FFileMaps := TUnicodeStringList.Create;
end;

destructor TUnicodeFilenameMapClipboardFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TUnicodeFilenameMapClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEMAPW = 0) then
    CF_FILENAMEMAPW := RegisterClipboardFormat(Pchar('FileNameMapW'));
  Result := CF_FILENAMEMAPW;
end;

procedure TUnicodeFilenameMapClipboardFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TUnicodeFilenameMapClipboardFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TUnicodeFilenameMapClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := FFileMaps.Count + 1;
  for i := 0 to FFileMaps.Count-1 do
    inc(Result, Length(FFileMaps[i]));
  inc(Result, Result);
end;

function TUnicodeFilenameMapClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := ReadFilesFromZeroList(Value, Size, True, FFileMaps);
end;

function TUnicodeFilenameMapClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := WriteFilesToZeroList(Value, Size, True, FFileMaps);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileGroupDescriptorCustomClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TFileGroupDescriptorCustomClipboardFormat.Clear;
begin
  if (FBuffer <> nil) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
end;

procedure TFileGroupDescriptorCustomClipboardFormat.CopyFrom(Source: pointer);
var
  Size: integer;
begin
  Clear;
  if (Source <> nil) then
  begin
    Size := GetBufferSize(PFileGroupDescriptor(Source)^.cItems);
    GetMem(FBuffer, Size);
    Move(Source^, FBuffer^, Size);
  end;
end;

destructor TFileGroupDescriptorCustomClipboardFormat.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetCount: integer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  if (FileGroupDescriptor <> nil) then
    Result := PFileGroupDescriptor(FileGroupDescriptor)^.cItems
  else
    Result := 0;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetFileDescriptor(
  Index: integer): pointer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  if (FileGroupDescriptor <> nil) then
    Result := @(PFileGroupDescriptor(FileGroupDescriptor)^.fgd[Index])
  else
    Result := nil;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetSize: integer;
begin
  if (FileGroupDescriptor <> nil) then
    Result := GetBufferSize(Count)
  else
    Result := 0;
end;

function TFileGroupDescriptorCustomClipboardFormat.HasData: boolean;
begin
  Result := (Count > 0);
end;

function TFileGroupDescriptorCustomClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  // Note: Some sources (e.g. Outlook) provides a larger buffer than is needed.
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  Result := (Value <> nil) and
    (Size >= GetBufferSize(PFileGroupDescriptor(Value)^.cItems));
  if (Result) then
    CopyFrom(Value);
end;

procedure TFileGroupDescriptorCustomClipboardFormat.SetCount(
  const Value: integer);
var
  Size: integer;
begin
  Clear;
  Size := GetBufferSize(Value);
  GetMem(FBuffer, Size);
  FillChar(FBuffer^, Size, 0);
  PFileGroupDescriptor(FBuffer)^.cItems := Value;
end;

function TFileGroupDescriptorCustomClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
var
  BufferSize: integer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  BufferSize := GetBufferSize(Count);
  Result := (FileGroupDescriptor <> nil) and (Value <> nil) and
    (Size >= BufferSize);
  if (Result) then
    Move(FileGroupDescriptor^, Value^, BufferSize);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILEGROUPDESCRIPTOR: TClipFormat = 0;

function TAnsiFileGroupDescriptorClipboardFormat.GetBufferSize(
  Count: integer): integer;
begin
  Result := SizeOf(UINT) + Count * SizeOf(TFileDescriptorA);
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILEGROUPDESCRIPTOR = 0) then
    CF_FILEGROUPDESCRIPTOR := RegisterClipboardFormat(pchar('FileGroupDescriptor'));
  Result := CF_FILEGROUPDESCRIPTOR;
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetFileDescriptor(
  Index: integer): PFileDescriptorA;
begin
  Result := PFileDescriptorA(inherited GetFileDescriptor(Index));
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetFileGroupDescriptor: PFileGroupDescriptorA;
begin
  Result := inherited FileGroupDescriptor;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TAnsiFileGroupDescriptorClipboardFormat.GetAnsiFilename(
  Index: integer): AnsiString;
var
  s: AnsiString;
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  SetLength(s, MAX_PATH);
  StrLCopy(PAnsiChar(s), @FileGroupDescriptor^.fgd[Index].cFileName[0], MAX_PATH);
  Result := PAnsiChar(s);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TAnsiFileGroupDescriptorClipboardFormat.GetFilename(
  Index: integer): string;
begin
  Result := String(GetAnsiFilename(Index));
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TAnsiFileGroupDescriptorClipboardFormat.SetAnsiFilename(Index: integer;
  const Value: AnsiString);
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  StrPLCopy(@FileGroupDescriptor^.fgd[Index].cFileName[0], Value, MAX_PATH);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TAnsiFileGroupDescriptorClipboardFormat.SetFilename(
  Index: integer; const Value: string);
begin
  SetAnsiFilename(Index, AnsiString(Value));
end;

procedure TAnsiFileGroupDescriptorClipboardFormat.CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorA);
begin
  inherited CopyFrom(AFileGroupDescriptor);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILEGROUPDESCRIPTORW: TClipFormat = 0;

function TUnicodeFileGroupDescriptorClipboardFormat.GetBufferSize(
  Count: integer): integer;
begin
  Result := SizeOf(UINT) + Count * SizeOf(TFileDescriptorW);
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILEGROUPDESCRIPTORW = 0) then
    CF_FILEGROUPDESCRIPTORW := RegisterClipboardFormat(Pchar('FileGroupDescriptorW'));
  Result := CF_FILEGROUPDESCRIPTORW;
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetFileDescriptor(
  Index: integer): PFileDescriptorW;
begin
  Result := PFileDescriptorW(inherited GetFileDescriptor(Index));
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetFileGroupDescriptor: PFileGroupDescriptorW;
begin
  Result := inherited FileGroupDescriptor;
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TUnicodeFileGroupDescriptorClipboardFormat.GetUnicodeFilename(Index: integer): UnicodeString;
var
  s: UnicodeString;
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  SetLength(s, MAX_PATH);
  WStrLCopy(PWideChar(s), @FileGroupDescriptor^.fgd[Index].cFileName[0], MAX_PATH);
  Result := PWideChar(s);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

function TUnicodeFileGroupDescriptorClipboardFormat.GetFilename(
  Index: integer): string;
begin
  Result := String(GetUnicodeFilename(Index));
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
procedure TUnicodeFileGroupDescriptorClipboardFormat.SetUnicodeFilename(Index: integer;
  const Value: UnicodeString);
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  WStrPLCopy(@FileGroupDescriptor^.fgd[Index].cFileName[0], Value, MAX_PATH);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TUnicodeFileGroupDescriptorClipboardFormat.SetFilename(
  Index: integer; const Value: string);
begin
  SetUnicodeFilename(Index, UnicodeString(Value));
end;

procedure TUnicodeFileGroupDescriptorClipboardFormat.CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorW);
begin
  inherited CopyFrom(AFileGroupDescriptor);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILECONTENTS: TClipFormat = 0;

constructor TFileContentsClipboardFormat.Create;
begin
  // Note: Previous versions trimmed zeroes and added a terminating zero.
  // This just seems wrong as we shouldn't modify the content at this level. If
  // the data need to be trimmed or zero terminated it should be done at a
  // higher level. The purpose of this class is just to deliver the raw data.
  inherited Create;
  FFormatEtc.lindex := 0;
end;

function TFileContentsClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(PChar('FileContents'));
  Result := CF_FILECONTENTS;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStreamClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStreamClipboardFormat.Create;
begin
  CreateFormat(TYMED_ISTREAM or TYMED_ISTORAGE);
  FStreams := TStreamList.Create;
end;

destructor TFileContentsStreamClipboardFormat.Destroy;
begin
  Clear;
  FStreams.Free;
  inherited Destroy;
end;

function TFileContentsStreamClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(PChar('FileContents'));
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStreamClipboardFormat.Clear;
begin
  FStreams.Clear;
end;

function TFileContentsStreamClipboardFormat.HasData: boolean;
begin
  Result := (FStreams.Count > 0);
end;

class procedure TFileContentsStreamClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConsumer(TDataStreamDataFormat);
end;

function TFileContentsStreamClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  Result := True;
  if (Dest is TDataStreamDataFormat) then
  begin
    TDataStreamDataFormat(Dest).Streams.Assign(Streams);
  end else
    Result := inherited AssignTo(Dest);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFileContentsStreamClipboardFormat.GetData(const DataObject: IDataObject): boolean;
var
  AFormatEtc: TFormatEtc;
  FGD: TFileGroupDescriptorCustomClipboardFormat;
  Count: integer;
  Medium: TStgMedium;
  Stream: IStream;
  Name: string;
  MemStream: TMemoryStream;
  StatStg: TStatStg;
  Size: longInt;
  Remaining: longInt;
  pChunk: PByte;
begin
  Result := False;

  Clear;
  FGD := nil;
  try
    FGD := TUnicodeFileGroupDescriptorClipboardFormat.Create;
    if (not FGD.HasValidFormats(DataObject)) then
    begin
      FreeAndNil(FGD);
      FGD := TAnsiFileGroupDescriptorClipboardFormat.Create;
    end;

    // Make copy of original FormatEtc and work with the copy.
    // If we modify the original, we *must* change it back when we are done with
    // it.
    AFormatEtc := FormatEtc;

    if (FGD.GetData(DataObject)) then
    begin
      // Multiple objects, retrieve one at a time
      Count := FGD.Count;
      AFormatEtc.lindex := 0;
    end else
    begin
      // Single object, retrieve "all" at once
      Count := 0;
      AFormatEtc.lindex := -1;
      Name := '';
    end;

    while (AFormatEtc.lindex < Count) do
    begin
      FillChar(Medium, SizeOf(Medium), 0);
      if (Failed(DataObject.GetData(AFormatEtc, Medium))) then
        break;
      try
        inc(AFormatEtc.lindex);

        if (Medium.tymed = TYMED_ISTORAGE) then
        begin
          Stream := CreateIStreamFromIStorage(IStorage(Medium.pstg));
          if (Stream = nil) then
          begin
            Result := False;
            break;
          end;
        end else
        if (Medium.tymed = TYMED_ISTREAM) then
          Stream := IStream(Medium.pstm)
        else
          continue;

        Stream.Stat(StatStg, STATFLAG_NONAME);
        MemStream := TMemoryStream.Create;
        try
          Remaining := StatStg.cbSize;
          MemStream.Size := Remaining;
          pChunk := MemStream.Memory;

          // Fix for Outlook attachment paste bug #1.
          // Some versions of Outlook doesn't reset the stream position after we
          // have read data from the stream, so the next time we ask Outlook for
          // the same stream (e.g. by pasting the same attachment twice), we get
          // a stream where the current position is at EOS.
          Stream.Seek(0, STREAM_SEEK_SET, qword(PLargeint(nil)^));

          while (Remaining > 0) do
          begin
            if (Failed(Stream.Read(pChunk, Remaining, pdword(@Size)))) or
              (Size = 0) then
              break;
            inc(pChunk, Size);
            dec(Remaining, Size);
          end;
          // Fix for Outlook attachment paste bug  #2.
          // We reset the stream position here just to be nice to other
          // applications which might not have work arounds for this problem
          // (e.g. Windows Explorer).
          Stream.Seek(0, STREAM_SEEK_SET, qword(PLargeint(nil)^));

          if (AFormatEtc.lindex > 0) then
            Name := FGD.Filenames[AFormatEtc.lindex-1];
          Streams.AddNamed(MemStream, Name);
        except
          MemStream.Free;
          raise;
        end;
        Stream := nil;
        Result := True;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
  finally
    FGD.Free;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStreamOnDemandClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStreamOnDemandClipboardFormat.Create;
begin
  // We also support TYMED_ISTORAGE for drop targets, but since we only support
  // TYMED_ISTREAM for both source and targets, we can't specify TYMED_ISTORAGE
  // here. See GetStream method.
  CreateFormat(TYMED_ISTREAM or TYMED_HGLOBAL);
end;

destructor TFileContentsStreamOnDemandClipboardFormat.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(PChar('FileContents'));
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStreamOnDemandClipboardFormat.Clear;
begin
  FGotData := False;
  FDataRequested := False;
end;

function TFileContentsStreamOnDemandClipboardFormat.HasData: boolean;
begin
  Result := FGotData or FDataRequested;
end;

function TFileContentsStreamOnDemandClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TVirtualFileStreamDataFormat) then
  begin
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;

function TFileContentsStreamOnDemandClipboardFormat.Assign(
  Source: TCustomDataFormat): boolean;
begin
  if (Source is TVirtualFileStreamDataFormat) then
  begin
    // Acknowledge that we can offer the requested data, but defer the actual
    // data transfer.
    FDataRequested := True;
    Result := True
  end else
    Result := inherited Assign(Source);
end;

function TFileContentsStreamOnDemandClipboardFormat.DoSetData(
  const AFormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean;
var
  Stream: IStream;
  Index: integer;
  StatStg: TStatStg;
  Data: pointer;
  ReadSize: longInt;
begin
  Result := False;
  Index := AFormatEtcIn.lindex;
  (*
  ** Warning:
  ** The meaning of the value -1 in FormatEtcIn.lindex is undocumented in this
  ** context (TYMED_ISTREAM), but can occur when pasting to the clipboard.
  ** Apparently the clipboard doesn't use the stream returned from a call with
  ** lindex = -1, but only uses it as a test to see if data is available.
  ** When the clipboard actually needs the data it will specify correct values
  ** for lindex.
  ** In version 4.0 we rejected the call if -1 was specified, but in order to
  ** support clipboard operations we now map -1 to 0.
  *)
  if (Index = -1) then
    Index := 0;

  if (Assigned(FOnGetStream)) and (AFormatEtcIn.tymed and FormatEtc.tymed <> 0) and
    (Index >= 0) then
  begin
    FOnGetStream(Self, Index, Stream);

    if (Stream <> nil) then
    begin
      if (AFormatEtcIn.tymed and TYMED_ISTREAM <> 0) then
      begin
        IStream(AMedium.pstm) := Stream;
        AMedium.tymed := TYMED_ISTREAM;
        Result := True;
      end else
      if (AFormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
      begin
        if (Failed(Stream.Stat(StatStg, STATFLAG_NONAME))) then
          exit;
        AMedium.hGlobal := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, StatStg.cbSize);
        if (AMedium.hGlobal <> 0) then
          try
            try
              Data := GlobalLock(AMedium.hGlobal);
              try
                OleCheck(Stream.Seek(0, STREAM_SEEK_SET, qword(PLargeint(nil)^)));
                OleCheck(Stream.Read(Data, StatStg.cbSize, pdword(@ReadSize)));
                Result := (ReadSize = StatStg.cbSize);
              finally
                GlobalUnlock(AMedium.hGlobal);
              end;
            finally
              if (not Result) then
              begin
                GlobalFree(AMedium.hGlobal);
                AMedium.hGlobal := 0;
              end;
            end;
          except
            // Eat exceptions since they wont work inside drag/drop anyway.
            Result := False;
          end;
      end;
    end;
  end;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetData(const DataObject: IDataObject): boolean;
begin
  // Flag that data has been offered to us, but defer the actual data transfer.
  FGotData := True;
  Result := True;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetStream(Index: integer): IStream;
var
  Medium: TStgMedium;
  AFormatEtc: TFormatEtc;
begin
  Result := nil;
  // Get an IStream interface from the source.
  AFormatEtc := FormatEtc;
  AFormatEtc.tymed := AFormatEtc.tymed or TYMED_ISTORAGE;
  AFormatEtc.lindex := Index;
  if (Succeeded((DataFormat.Owner as TCustomDroptarget).DataObject.GetData(AFormatEtc,
    Medium))) then
    try
      case Medium.tymed of
        TYMED_ISTREAM:
          Result := IStream(Medium.pstm);
        TYMED_ISTORAGE:
          Result := CreateIStreamFromIStorage(IStorage(Medium.pstg));
        TYMED_HGLOBAL:
          if (Succeeded(CreateStreamOnHGlobal(Medium.hGlobal,
            (Medium.punkForRelease = nil), Result))) then
            // Prevent ReleaseStgMedium from freeing the Medium.hGlobal.
            Medium.hGlobal := 0
          else
            Result := nil;
      end;
    finally
      ReleaseStgMedium(Medium);
    end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileContentsStorageClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStorageClipboardFormat.Create;
begin
  CreateFormat(TYMED_ISTORAGE);
  FStorages := TStorageInterfaceList.Create;
end;

destructor TFileContentsStorageClipboardFormat.Destroy;
begin
  Clear;
  FStorages.Free;
  inherited Destroy;
end;

function TFileContentsStorageClipboardFormat.DoSetData(const AFormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
var
  Index: integer;
begin
  Index := AFormatEtcIn.lindex;
  if (Index = -1) then // See TFileContentsStreamOnDemandClipboardFormat.DoSetData
    Index := 0;
  if (AFormatEtcIn.tymed and TYMED_ISTORAGE <> 0) and (Index < FStorages.Count) then
  begin
    IStorage(AMedium.pstg) := FStorages[Index];
    AMedium.tymed := TYMED_ISTORAGE;
    Result := True;
  end else
    Result := inherited DoSetData(AFormatEtcIn, AMedium);
end;

function TFileContentsStorageClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(PChar('FileContents'));
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStorageClipboardFormat.Clear;
begin
  FStorages.Clear;
end;

function TFileContentsStorageClipboardFormat.HasData: boolean;
begin
  Result := (FStorages.Count > 0);
end;

function TFileContentsStorageClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
(*
  Result := True;
  if (Dest is TDataStreamDataFormat) then
  begin
    TDataStreamDataFormat(Dest).Streams.Assign(Streams);
  end else
*)
    Result := inherited AssignTo(Dest);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFileContentsStorageClipboardFormat.GetData(const DataObject: IDataObject): boolean;
var
  FGD: TFileGroupDescriptorCustomClipboardFormat;
  Count: integer;
  Medium: TStgMedium;
  Storage: IStorage;
  Name: string;
  AFormatEtc: TFormatEtc;
begin
  Result := False;

  Clear;
  // The FileContents formats is always accompanied by the FileGroupDescriptor
  // format, so we can get the names from the FileGroupDescriptor format.
  FGD := nil;
  try
    FGD := TAnsiFileGroupDescriptorClipboardFormat.Create;
    if (not FGD.HasValidFormats(DataObject)) then
    begin
      FreeAndNil(FGD);
      FGD := TUnicodeFileGroupDescriptorClipboardFormat.Create;
    end;
    // Work on a temporary copy of the FormatEtc structure so we can modify it
    // without side effects (Thanks Tom!).
    AFormatEtc := FormatEtc;
    if (FGD.GetData(DataObject)) then
    begin
      // Multiple objects, retrieve one at a time
      Count := FGD.Count;
      AFormatEtc.lindex := 0;
    end else
    begin
      // Single object, retrieve "all" at once
      Count := 0;
      AFormatEtc.lindex := -1;
      Name := '';
    end;

    while (AFormatEtc.lindex < Count) do
    begin
      if (Failed(DataObject.GetData(AFormatEtc, Medium))) then
        break;
      try
        inc(AFormatEtc.lindex);
        if (Medium.tymed <> TYMED_ISTORAGE) then
          continue;
        Storage := IStorage(Medium.pstg);
        try
          if (AFormatEtc.lindex > 0) then
            Name := FGD.Filenames[AFormatEtc.lindex-1];
          Storages.AddNamed(Storage, Name);
        finally
          Storage := nil;
        end;
        Result := True;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
  finally
    FGD.Free;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//
//              TFileDescriptorToFilenameStrings
//              TFileDescriptorWToFilenameStrings
//
////////////////////////////////////////////////////////////////////////////////
// Used internally to convert between FileDescriptors and filenames on-demand.
////////////////////////////////////////////////////////////////////////////////
// TODO : Need Unicode/WideString support for pre D2009
type
  TFileDescriptorToFilenameStrings = class(TStrings)
  private
    FFileDescriptors: TMemoryList;
    FObjects: TList;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
  public
    constructor Create(AFileDescriptors: TMemoryList);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Assign(Source: TPersistent); override;
  end;

constructor TFileDescriptorToFilenameStrings.Create(AFileDescriptors: TMemoryList);
begin
  inherited Create;
  FFileDescriptors := AFileDescriptors;
  FObjects := TList.Create;
end;

destructor TFileDescriptorToFilenameStrings.Destroy;
begin
  FObjects.Free;
  inherited Destroy;
end;

function TFileDescriptorToFilenameStrings.Get(Index: Integer): string;
var
  testansi:ansistring;
  testwide:widestring;
begin
  testwide :=  PFileDescriptorW(FFileDescriptors[Index]).cFileName;
  testansi:=utf8toansi(testwide);
  result:=testansi;
end;

function TFileDescriptorToFilenameStrings.GetCount: Integer;
begin
  Result := FFileDescriptors.Count;
end;

procedure TFileDescriptorToFilenameStrings.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      FFileDescriptors.Clear;
      for i := 0 to TStrings(Source).Count-1 do
        AddObject(TStrings(Source)[i], TStrings(Source).Objects[i]);
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TFileDescriptorToFilenameStrings.Clear;
begin
  FFileDescriptors.Clear;
  FObjects.Clear;
end;

procedure TFileDescriptorToFilenameStrings.Delete(Index: Integer);
begin
  FFileDescriptors.Delete(Index);
  FObjects.Delete(Index);
end;

procedure TFileDescriptorToFilenameStrings.Insert(Index: Integer; const S: string);
var
  FD: PFileDescriptorW;
begin
  if (Index = FFileDescriptors.Count) then
  begin
    GetMem(FD, SizeOf(TFileDescriptorW));
    try
      FillChar(FD^, SizeOf(TFileDescriptorW), 0);
      StringToWideChar(S, FD.cFileName, SizeOf(FD.cFileName));
      FFileDescriptors.Add(FD);
      FObjects.Add(nil);
    except
      FreeMem(FD);
      raise;
    end;
  end;
end;

procedure TFileDescriptorToFilenameStrings.PutObject(Index: Integer;
  AObject: TObject);
begin
  FObjects[Index] := AObject;
end;

function TFileDescriptorToFilenameStrings.GetObject(Index: Integer): TObject;
begin
  Result := FObjects[Index];
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TVirtualFileStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TVirtualFileStreamDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FFileDescriptors := TMemoryList.Create;
  FFileNames := TFileDescriptorToFilenameStrings.Create(FFileDescriptors);

  FFileContentsClipboardFormat := TFileContentsStreamOnDemandClipboardFormat(CompatibleFormats.GetFormat(TFileContentsStreamOnDemandClipboardFormat));
  FAnsiFileGroupDescriptorClipboardFormat := TAnsiFileGroupDescriptorClipboardFormat(CompatibleFormats.GetFormat(TAnsiFileGroupDescriptorClipboardFormat));
  FUnicodeFileGroupDescriptorClipboardFormat := TUnicodeFileGroupDescriptorClipboardFormat(CompatibleFormats.GetFormat(TUnicodeFileGroupDescriptorClipboardFormat));
end;

destructor TVirtualFileStreamDataFormat.Destroy;
begin
  FFileDescriptors.Free;
  FFileNames.Free;
  inherited Destroy;
end;

procedure TVirtualFileStreamDataFormat.SetFileNames(const Value: TStrings);
begin
  FFileNames.Assign(Value);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TVirtualFileStreamDataFormat.Assign(Source: TClipboardFormat): boolean;
var
  i: integer;
  FDA: PFileDescriptorA;
  FDW: PFileDescriptorW;
begin
  Result := True;

  (*
  ** TFileContentsStreamOnDemandClipboardFormat
  *)
  if (Source is TFileContentsStreamOnDemandClipboardFormat) then
  begin
    FHasContents := TFileContentsStreamOnDemandClipboardFormat(Source).HasData;
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Source is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    FFileDescriptors.Clear;
    for i := 0 to TAnsiFileGroupDescriptorClipboardFormat(Source).Count-1 do
    begin
      GetMem(FDW, SizeOf(TFileDescriptorW));
      try
        // Copy the common A&W part

        // Argh! Delphi can't compute SizeOf a type member at compile time
        // ASSERT(SizeOf(TFileDescriptorA)-SizeOf(TFileDescriptorA.cFileName) = SizeOf(TFileDescriptorW)-SizeOf(TFileDescriptorW.cFileName));
        ASSERT(pointer(@PFileDescriptorA(nil)^.cFileName) = pointer(@PFileDescriptorW(nil)^.cFileName));

        FDA := @TAnsiFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.fgd[i];
        Move(FDA^, FDW^, integer(@PFileDescriptorA(nil)^.cFileName));
        // Convert and copy the Ansi string to Unicode
        MultiByteToWideChar(DefaultSystemCodePage, 0,
          FDA^.cFileName, SizeOf(FDA^.cFileName),
          FDW^.cFileName, SizeOf(FDW^.cFileName));
        FFileDescriptors.Add(FDW);
      except
        FreeMem(FDW);
        raise;
      end;
    end;
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Source is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    FFileDescriptors.Clear;
    for i := 0 to TUnicodeFileGroupDescriptorClipboardFormat(Source).Count-1 do
    begin
      GetMem(FDW, SizeOf(TFileDescriptorW));
      try
        Move(TUnicodeFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.fgd[i],
          FDW^, SizeOf(TFileDescriptorW));
        FFileDescriptors.Add(FDW);
      except
        FreeMem(FDW);
        raise;
      end;
    end;
  end else
  (*
  ** None of the above...
  *)
    Result := inherited Assign(Source);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TVirtualFileStreamDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
var
  FGDA: PFileGroupDescriptorA;
  FGDW: PFileGroupDescriptorW;
  i: integer;
begin
  (*
  ** TFileContentsStreamOnDemandClipboardFormat
  *)
  if (Dest is TFileContentsStreamOnDemandClipboardFormat) then
  begin
    // Let the clipboard format handle the transfer.
    // No data is actually transferred, but TFileContentsStreamOnDemandClipboardFormat
    // needs to set a flag when data is requested.
    Result := Dest.Assign(Self);
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    if (FFileDescriptors.Count > 0) then
    begin
      // Copy the common A&W part
      // Argh! Delphi can't compute SizeOf a type member at compile time
      // ASSERT(SizeOf(TFileDescriptorA)-SizeOf(TFileDescriptorA.cFileName) = SizeOf(TFileDescriptorW)-SizeOf(TFileDescriptorW.cFileName));
      ASSERT(pointer(@PFileDescriptorA(nil)^.cFileName) = pointer(@PFileDescriptorW(nil)^.cFileName));

      GetMem(FGDA, SizeOf(UINT) + FFileDescriptors.Count * SizeOf(TFileDescriptorA));
      try
        FGDA.cItems := FFileDescriptors.Count;
        for i := 0 to FFileDescriptors.Count-1 do
        begin
          Move(FFileDescriptors[i]^, FGDA.fgd[i], integer(@PFileDescriptorA(nil)^.cFileName));
          // Convert and copy the Unicode string to Ansi
          WideCharToMultiByte(DefaultSystemCodePage, 0,
            PFileDescriptorW(FFileDescriptors[i])^.cFileName,
              SizeOf(PFileDescriptorW(FFileDescriptors[i])^.cFileName),
            FGDA.fgd[i].cFileName,
              SizeOf(FGDA.fgd[i].cFileName),
              nil, nil);
        end;
        TAnsiFileGroupDescriptorClipboardFormat(Dest).CopyFrom(FGDA);
      finally
        FreeMem(FGDA);
      end;
      Result := True;
    end else
      Result := False;
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    if (FFileDescriptors.Count > 0) then
    begin
      GetMem(FGDW, SizeOf(UINT) + FFileDescriptors.Count * SizeOf(TFileDescriptorW));
      try
        FGDW.cItems := FFileDescriptors.Count;
        for i := 0 to FFileDescriptors.Count-1 do
          Move(FFileDescriptors[i]^, FGDW.fgd[i], SizeOf(TFileDescriptorW));
        TUnicodeFileGroupDescriptorClipboardFormat(Dest).CopyFrom(FGDW);
      finally
        FreeMem(FGDW);
      end;
      Result := True;
    end else
      Result := False;
  end else
  (*
  ** None of the above...
  *)
    Result := inherited AssignTo(Dest);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TVirtualFileStreamDataFormat.Clear;
begin
  FFileDescriptors.Clear;
  FHasContents := False;
end;

function TVirtualFileStreamDataFormat.HasData: boolean;
begin
  Result := (FFileDescriptors.Count > 0) and
    ((FHasContents) or Assigned(FFileContentsClipboardFormat.OnGetStream));
end;

function TVirtualFileStreamDataFormat.NeedsData: boolean;
begin
  Result := (FFileDescriptors.Count = 0) or (not FHasContents);
end;

class procedure TVirtualFileStreamDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TFileContentsStreamOnDemandClipboardFormat);
  RegisterDataConversion(TAnsiFileGroupDescriptorClipboardFormat, 1);
  RegisterDataConversion(TUnicodeFileGroupDescriptorClipboardFormat, 0);
end;

function TVirtualFileStreamDataFormat.GetFileDescriptor(Index: integer): PFileDescriptorW;
begin
  Result := PFileDescriptorW(FFileDescriptors[Index]);
end;

function TVirtualFileStreamDataFormat.GetOnGetStream: TOnGetStreamEvent;
begin
  Result := FFileContentsClipboardFormat.OnGetStream;
end;

procedure TVirtualFileStreamDataFormat.SetOnGetStream(const Value: TOnGetStreamEvent);
begin
  FFileContentsClipboardFormat.OnGetStream := Value;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TFileMapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileMapDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FFileMaps := TUnicodeStringList.Create;
  TUnicodeStringList(FFileMaps).OnChanging := DoOnChanging;
end;

destructor TFileMapDataFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TFileMapDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TAnsiFilenameMapClipboardFormat) then
    FFileMaps.Assign(TAnsiFilenameMapClipboardFormat(Source).FileMaps)

  else if (Source is TUnicodeFilenameMapClipboardFormat) then
    FFileMaps.Assign(TUnicodeFilenameMapClipboardFormat(Source).FileMaps)

  else
    Result := inherited Assign(Source);
end;

function TFileMapDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TAnsiFilenameMapClipboardFormat) then
    TAnsiFilenameMapClipboardFormat(Dest).FileMaps.Assign(FFileMaps)

  else if (Dest is TUnicodeFilenameMapClipboardFormat) then
    TUnicodeFilenameMapClipboardFormat(Dest).FileMaps.Assign(FFileMaps)

  else
    Result := inherited AssignTo(Dest);
end;

procedure TFileMapDataFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TFileMapDataFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TFileMapDataFormat.NeedsData: boolean;
begin
  Result := (FFileMaps.Count = 0);
end;


class procedure TFileMapDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TAnsiFilenameMapClipboardFormat, AnsiBoost);
  RegisterDataConversion(TUnicodeFilenameMapClipboardFormat, UnicodeBoost);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FFiles := TUnicodeStringList.Create;
  TUnicodeStringList(FFiles).OnChanging := DoOnChanging;
  // FFiles := TStringList.Create;
  // TStringList(FFiles).OnChanging := DoOnChanging;
end;

destructor TFileDataFormat.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TFileDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TFileClipboardFormat) then
    FFiles.Assign(TFileClipboardFormat(Source).Files)

  else if (Source is TPIDLClipboardFormat) then
    FFiles.Assign(TPIDLClipboardFormat(Source).Filenames)

  else
    Result := inherited Assign(Source);
end;

function TFileDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TFileClipboardFormat) then
    TFileClipboardFormat(Dest).Files.Assign(FFiles)

  else if (Dest is TPIDLClipboardFormat) then
    TPIDLClipboardFormat(Dest).Filenames.Assign(FFiles)

  else
    Result := inherited AssignTo(Dest);
end;

procedure TFileDataFormat.Clear;
begin
  FFiles.Clear;
end;

function TFileDataFormat.HasData: boolean;
begin
  Result := (FFiles.Count > 0);
end;

function TFileDataFormat.NeedsData: boolean;
begin
  Result := (FFiles.Count = 0);
end;


class procedure TFileDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TFileClipboardFormat, 0);
  RegisterDataConversion(TPIDLClipboardFormat, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropFileTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropFileTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OptimizedMove := True;

  FFileFormat := TFileDataFormat.Create(Self);
  FFileMapFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropFileTarget.Destroy;
begin
  FFileFormat.Free;
  FFileMapFormat.Free;
  inherited Destroy;
end;

function TDropFileTarget.GetFiles: TUnicodeStrings;
begin
  Result := FFileFormat.Files;
end;

function TDropFileTarget.GetMappedNames: TUnicodeStrings;
begin
  Result := FFileMapFormat.FileMaps;
end;

function TDropFileTarget.GetPreferredDropEffect: LongInt;
begin
  // TODO : Needs explanation of why this is nescessary.
  Result := inherited GetPreferredDropEffect;
  if (Result = DROPEFFECT_NONE) then
    Result := DROPEFFECT_COPY;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropFileSource
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropFileSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFileFormat := TFileDataFormat.Create(Self);
  FFileMapFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropFileSource.Destroy;
begin
  FFileFormat.Free;
  FFileMapFormat.Free;
  inherited Destroy;
end;

function TDropFileSource.GetFiles: TUnicodeStrings;
begin
  Result := FFileFormat.Files;
end;

function TDropFileSource.GetMappedNames: TUnicodeStrings;
begin
  Result := FFileMapFormat.FileMaps;
end;

procedure TDropFileSource.SetFiles(AFiles: TUnicodeStrings);
begin
  FFileFormat.Files.Assign(AFiles);
end;

procedure TDropFileSource.SetMappedNames(ANames: TUnicodeStrings);
begin
  FFileMapFormat.FileMaps.Assign(ANames);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              Initialization/Finalization
//
////////////////////////////////////////////////////////////////////////////////

initialization
  // Data format registration
  TFileDataFormat.RegisterDataFormat;
  TFileMapDataFormat.RegisterDataFormat;
  TVirtualFileStreamDataFormat.RegisterDataFormat;

  // Clipboard format registration
  TAnsiFilenameClipboardFormat.RegisterFormat;
  TFileClipboardFormat.RegisterFormat;
  TPIDLClipboardFormat.RegisterFormat;
  TAnsiFilenameClipboardFormat.RegisterFormat;
  TUnicodeFilenameClipboardFormat.RegisterFormat;
  TAnsiFilenameMapClipboardFormat.RegisterFormat;
  TUnicodeFilenameMapClipboardFormat.RegisterFormat;
  TAnsiFileGroupDescriptorClipboardFormat.RegisterFormat;
  TUnicodeFileGroupDescriptorClipboardFormat.RegisterFormat;
  TFileContentsClipboardFormat.RegisterFormat;
  TFileContentsStreamClipboardFormat.RegisterFormat;
  TFileContentsStorageClipboardFormat.RegisterFormat;

finalization

end.
