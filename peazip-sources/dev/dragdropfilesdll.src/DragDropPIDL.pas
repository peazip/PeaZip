unit DragDropPIDL;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDropPIDL
// Description:     Implements Dragging & Dropping of PIDLs (files and folders).
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
  DragDropFile,
  Windows,
  ActiveX,
  Classes,
  ShlObj;

{$include DragDrop.inc}

////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLList
//
////////////////////////////////////////////////////////////////////////////////
// A list of PIDLs
////////////////////////////////////////////////////////////////////////////////
type
  TPIDLList = class
  private
    FList: TList;
    FOnChanging: TNotifyEvent;
  protected
    function GetCount: integer;
    function GetItem(Index: integer): PItemIDList;
    procedure SetItem(Index: integer; const Value: PItemIDList);
    function GetString(Index: integer): AnsiString;
    procedure Changing;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: PItemIDList); overload;
    procedure Add(Item: AnsiString); overload;
    procedure Assign(Source: TPIDLList);
    procedure Clear;
    property Count: integer read GetCount;
    property Items[Index: integer]: PItemIDList read GetItem write SetItem;
    property Strings[Index: integer]: AnsiString read GetString; default;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Supports the 'Shell IDList Array' format.
////////////////////////////////////////////////////////////////////////////////
type
  TPIDLClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FPIDLs: TPIDLList;
    FFilenames: TUnicodeStrings;
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
    property PIDLs: TPIDLList read FPIDLs;
    property Filenames: TUnicodeStrings read FFilenames;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLDataFormat
//
////////////////////////////////////////////////////////////////////////////////
type
  TPIDLDataFormat = class(TCustomDataFormat)
  private
    FPIDLs: TPIDLList;
    FFilenames: TUnicodeStrings;
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
    property PIDLs: TPIDLList read FPIDLs;
    property Filenames: TUnicodeStrings read FFilenames;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropPIDLTarget
//
////////////////////////////////////////////////////////////////////////////////
type
  TDropPIDLTarget = class(TCustomDropMultiTarget)
  private
    FPIDLDataFormat: TPIDLDataFormat;
    FFileMapDataFormat: TFileMapDataFormat;
  protected
    function GetPIDLs: TPIDLList;
    function GetPIDLCount: integer;
    function GetMappedNames: TUnicodeStrings;
    function GetFilenames: TUnicodeStrings;
    property PIDLs: TPIDLList read GetPIDLs;
    function DoGetPIDL(Index: integer): pItemIdList;
    function GetPreferredDropEffect: LongInt; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;

    // Note: It is the callers responsibility to cleanup
    // the returned PIDLs from the following 3 methods:
    // - GetFolderPidl
    // - GetRelativeFilePidl
    // - GetAbsoluteFilePidl
    // Use the CoTaskMemFree procedure to free the PIDLs.
    function GetFolderPIDL: pItemIdList;
    function GetRelativeFilePIDL(Index: integer): pItemIdList;
    function GetAbsoluteFilePIDL(Index: integer): pItemIdList;
    property PIDLCount: integer read GetPIDLCount; // Includes folder pidl in count

    // If you just want the filenames (not PIDLs) then use ...
    property Filenames: TUnicodeStrings read GetFilenames;
    // MappedNames is only needed if files need to be renamed after a drag or
    // e.g. dragging from 'Recycle Bin'.
    property MappedNames: TUnicodeStrings read GetMappedNames;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropPIDLSource
//
////////////////////////////////////////////////////////////////////////////////
type
  TDropPIDLSource = class(TCustomDropMultiSource)
  private
    FPIDLDataFormat: TPIDLDataFormat;
    FFileMapDataFormat: TFileMapDataFormat;
  protected
    function GetMappedNames: TUnicodeStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyFolderPIDLToList(pidl: PItemIDList);
    procedure CopyFilePIDLToList(pidl: PItemIDList);
    property MappedNames: TUnicodeStrings read GetMappedNames;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              PIDL utility functions
//
////////////////////////////////////////////////////////////////////////////////

//: GetPIDLsFromData extracts a PIDL list from a memory block and stores the
// PIDLs in a list.
function GetPIDLsFromData(Data: pointer; Size: integer; PIDLs: TPIDLList): boolean;

//: GetPIDLsFromHGlobal extracts a PIDL list from a global memory block and
// stores the PIDLs in a list.
function GetPIDLsFromHGlobal(const HGlob: HGlobal; PIDLs: TPIDLList): boolean;

//: GetPIDLsFromFilenames converts a list of files to PIDLs and stores the
// PIDLs in a list. All the PIDLs are relative to a common root.
function GetPIDLsFromFilenames(const Files: TUnicodeStrings; PIDLs: TPIDLList): boolean; overload;
function GetPIDLsFromFilenames(const Files: TStrings; PIDLs: TPIDLList): boolean; overload;

//: GetRootFolderPIDL finds the PIDL of the folder which is the parent of a list
// of files. The PIDl is returned as a string. If the files do not share a
// common root, an empty string is returnde.
function GetRootFolderPIDL(const Files: TUnicodeStrings): AnsiString; overload;
function GetRootFolderPIDL(const Files: TStrings): AnsiString; overload;

//: GetFullPIDLFromPath converts a path (filename and path) to a folder/filename
// PIDL pair.
function GetFullPIDLFromPath(const Path: UnicodeString): pItemIDList;

//: GetFullPathFromPIDL converts a folder/filename PIDL pair to a full path.
function GetFullPathFromPIDL(PIDL: pItemIDList): string;

//: PIDLToString converts a single PIDL to a string.
function PIDLToString(pidl: PItemIDList): AnsiString;

//: StringToPIDL converts a PIDL string to a PIDL.
function StringToPIDL(const PIDL: AnsiString): PItemIDList;

//: JoinPIDLStrings merges two PIDL strings into one.
function JoinPIDLStrings(const pidl1, pidl2: AnsiString): AnsiString;

//: ConvertFilesToShellIDList converts a list of files to a PIDL list. The
// files are relative to the folder specified by the Path parameter. The PIDLs
// are returned as a global memory handle.
function ConvertFilesToShellIDList(const Path: string; Files: TStrings): HGlobal;

//: GetSizeOfPIDL calculates the size of a PIDL list.
function GetSizeOfPIDL(PIDL: pItemIDList): integer;

//: CopyPIDL makes a copy of a PIDL.
// It is the callers responsibility to free the returned PIDL.
function CopyPIDL(PIDL: pItemIDList): pItemIDList;





type
  TILFindLastID = function(Pidl: PItemIDList): PItemIDList; stdcall;
  TILRemoveLastID = function(Pidl: PItemIDList): LongBool; stdcall;
  TILClone = function(Pidl: PItemIDList): PItemIDList; stdcall;
  TILCloneFirst = function(Pidl: PItemIDList): PItemIDList; stdcall;
  TILIsEqual = function(Pidl1, Pidl2: PItemIDList): LongBool; stdcall;
  TILCombine = function (Pidl1, Pidl2: PItemIDList): PItemIDList; stdcall;
  TILGetSize = function(Pidl: PItemIDList): Word; stdcall;
  TILGetNext = function(Pidl: PItemIDList): PItemIDList; stdcall;
  TILFree = procedure(Pidl: PItemIDList); stdcall;

var
  ILFindLastID: TILFindLastID = nil;
  ILRemoveLastID: TILRemoveLastID = nil;
  ILClone: TILClone = nil;
  ILCloneFirst: TILCloneFirst = nil;
  ILIsEqual: TILIsEqual = nil;
  ILCombine: TILCombine = nil;
  ILGetSize: TILGetSize = nil;
  ILGetNext: TILGetNext = nil;
  ILFree: TILFree = nil;

////////////////////////////////////////////////////////////////////////////////
//
//              PIDL/IShellFolder utility functions
//
////////////////////////////////////////////////////////////////////////////////

//: GetShellFolderOfPath retrieves an IShellFolder interface which can be used
// to manage the specified folder.
function GetShellFolderOfPath(const FolderPath: UnicodeString): IShellFolder;

//: GetPIDLDisplayName retrieves the display name of the specified PIDL,
// relative to the specified folder.
function GetPIDLDisplayName(Folder: IShellFolder; PIDL: PItemIdList; Flags: DWORD = SHGDN_NORMAL): string;

//: GetSubPIDL retrieves the PIDL of the specified file or folder to a PIDL.
// The PIDL is relative to the folder specified by the Folder parameter.
function GetSubPIDL(Folder: IShellFolder; const Sub: UnicodeString): pItemIDList;




implementation

uses
  ShellAPI,
  SysUtils;

resourcestring
  sNoFolderPIDL = 'Folder PIDL must be added first';

////////////////////////////////////////////////////////////////////////////////
//
//              PIDL utility functions
//
////////////////////////////////////////////////////////////////////////////////



function GetPIDLsFromData(Data: pointer; Size: integer; PIDLs: TPIDLList): boolean;
var
  i: integer;
  pOffset: ^UINT;
  PIDL: PItemIDList;
begin
  PIDLs.Clear;

  Result := (Data <> nil) and
    (Size >= integer(PIDA(Data)^.cidl) * (SizeOf(UINT)+SizeOf(PItemIDList)) + SizeOf(UINT));
  if (not Result) then
    exit;

  pOffset := @(PIDA(Data)^.aoffset[0]);
  i := PIDA(Data)^.cidl; // Note: Count doesn't include folder PIDL
  while (i >= 0) do
  begin
    PIDL := PItemIDList(Data);
    inc(PByte(PIDL), pOffset^);
    PIDLs.Add(PIDL);
    inc(pOffset);
    dec(i);
  end;
  Result := (PIDLs.Count > 1);
end;

function GetPIDLsFromHGlobal(const HGlob: HGlobal; PIDLs: TPIDLList): boolean;
var
  pCIDA: PIDA;
begin
  pCIDA := PIDA(GlobalLock(HGlob));
  try
    Result := GetPIDLsFromData(pCIDA, GlobalSize(HGlob), PIDLs);
  finally
    GlobalUnlock(HGlob);
  end;
end;

resourcestring
  sBadDesktop = 'Failed to get interface to Desktop';
  sBadFilename = 'Invalid filename: %s';

(*
** Find the folder which is the parent of all the files in a list.
*)
function GetRootFolderPIDL(const Files: TUnicodeStrings): AnsiString;
var
  DeskTopFolder: IShellFolder;
  WidePath: UnicodeString;
  PIDL: pItemIDList;
  PIDLs: TPIDLList;
  PIDL1, PIDL2: pItemIDList;
  Size, MaxSize: integer;
  i: integer;
begin
  Result := '';
  if (Files.Count = 0) then
    exit;

  if (SHGetDesktopFolder(DeskTopFolder) <> NOERROR) then
    raise Exception.Create(sBadDesktop);

  PIDLs := TPIDLList.Create;
  try
    // First convert all paths to PIDLs.
    for i := 0 to Files.Count-1 do
    begin
      WidePath := ExtractFilePath(Files[i]);
      if (DesktopFolder.ParseDisplayName(0, nil, PWideChar(WidePath), PULONG(nil)^,
        PIDL, PULONG(nil)^) <> NOERROR) then
        raise Exception.CreateFmt(sBadFilename, [WidePath]);
      try
        PIDLs.Add(PIDL);
      finally
        coTaskMemFree(PIDL);
      end;
    end;

    Result := PIDLs[0];
    MaxSize := Length(Result)-SizeOf(Word);
    PIDL := PIDLs.Items[0];
    for i := 1 to PIDLs.Count-1 do
    begin
      PIDL1 := PIDL;
      PIDL2 := PIDLs.Items[1];
      Size := 0;
      while (Size < MaxSize) and (PIDL1^.mkid.cb <> 0) and (PIDL1^.mkid.cb = PIDL2^.mkid.cb) and (CompareMem(PIDL1, PIDL2, PIDL1^.mkid.cb)) do
      begin
        inc(Size, PIDL1^.mkid.cb);
        inc(PByte(PIDL2), PIDL1^.mkid.cb);
        inc(PByte(PIDL1), PIDL1^.mkid.cb);
      end;
      if (Size <> MaxSize) then
      begin
        MaxSize := Size;
        SetLength(Result, Size+SizeOf(Word));
        PIDL1^.mkid.cb := 0;
      end;
      if (Size = 0) then
        break;
    end;
  finally
    PIDLs.Free;
  end;
end;

function GetRootFolderPIDL(const Files: TStrings): AnsiString;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := GetRootFolderPIDL(Adapter);
  finally
    Adapter.Free;
  end;
end;

function GetPIDLsFromFilenames(const Files: TUnicodeStrings; PIDLs: TPIDLList): boolean;
var
  RootPIDL: AnsiString;
  i: integer;
  PIDL: pItemIdList;
  FilePIDL: AnsiString;
begin
  Result := False;
  PIDLs.Clear;
  if (Files.Count = 0) then
    exit;

  // Get the PIDL of the root folder...
  // All the file PIDLs will be relative to this PIDL
  RootPIDL := GetRootFolderPIDL(Files);
  if (RootPIDL = '') then
    exit;

  Result := True;

  PIDLS.Add(RootPIDL);
  // Add the file PIDLs (all relative to the root)...
  for i := 0 to Files.Count-1 do
  begin
    PIDL := GetFullPIDLFromPath(Files[i]);
    if (PIDL = nil) then
    begin
      Result := False;
      PIDLs.Clear;
      break;
    end;
    try
      FilePIDL := PIDLToString(PIDL);
    finally
      coTaskMemFree(PIDL);
    end;
    // Remove the root PIDL from the file PIDL making it relative to the root.
    PIDLS.Add(Copy(FilePIDL, Length(RootPIDL)-SizeOf(Word)+1,
      Length(FilePIDL)-(Length(RootPIDL)-SizeOf(Word))));
  end;
end;

function GetPIDLsFromFilenames(const Files: TStrings; PIDLs: TPIDLList): boolean;
var
  Adapter: TUnicodeStringsAdapter;
begin
  Adapter := TUnicodeStringsAdapter.Create(Files);
  try
    Result := GetPIDLsFromFilenames(Adapter, PIDLs);
  finally
    Adapter.Free;
  end;
end;

function GetSizeOfPIDL(PIDL: pItemIDList): integer;
var
  Size: integer;
begin
  // TODO : Replace with ILGetSize
  if (PIDL <> nil) then
  begin
    Result := SizeOf(PIDL^.mkid.cb);
    while (PIDL^.mkid.cb <> 0) do
    begin
      Size := PIDL^.mkid.cb;
      inc(Result, Size);
      inc(PByte(PIDL), Size);
    end;
  end else
    Result := 0;
end;

function CopyPIDL(PIDL: pItemIDList): pItemIDList;
var
  Size: integer;
begin
  // TODO : Replace with ILClone
  Size := GetSizeOfPIDL(PIDL);
  if (Size > 0) then
  begin
    Result := ShellMalloc.Alloc(Size);
    if (Result <> nil) then
      Move(PIDL^, Result^, Size);
  end else
    Result := nil;
end;

function GetFullPIDLFromPath(const Path: UnicodeString): pItemIDList;
var
  DeskTopFolder: IShellFolder;
begin
  // TODO : Replace with ILCreateFromPath
  if (SHGetDesktopFolder(DeskTopFolder) = NOERROR) then
  begin
    if (DesktopFolder.ParseDisplayName(0, nil, PWideChar(Path), PULONG(nil)^,
      Result, PULONG(nil)^) <> NOERROR) then
      Result := nil;
  end else
    Result := nil;
end;

function GetFullPathFromPIDL(PIDL: pItemIDList): string;
var
  Path: array[0..MAX_PATH] of char;
begin
  if SHGetPathFromIDList(PIDL, Path) then
    Result := Path
  else
    Result := '';
end;

// See "Clipboard Formats for Shell Data Transfers" in Ole.hlp...
// (Needed to drag links (shortcuts).)
type
  POffsets = ^TOffsets;
  TOffsets = array[0..$FFFF] of UINT;

function ConvertFilesToShellIDList(const Path: string; Files: TStrings): HGlobal;
var
  shf: IShellFolder;
  PathPidl, pidl: pItemIDList;
  Ida: PIDA;
  pOffset: POffsets;
  ptrByte: PByte;
  i, PathPidlSize, IdaSize, PreviousPidlSize: integer;
begin
  Result := 0;
  shf := GetShellFolderOfPath(path);
  if shf = nil then
    exit;
  // Calculate size of IDA structure ...
  // cidl: UINT ; Directory pidl
  // offset: UINT ; all file pidl offsets
  IdaSize := (Files.Count + 2) * SizeOf(UINT);

  PathPidl := GetFullPIDLFromPath(path);
  if PathPidl = nil then
    exit;
  try
    PathPidlSize := GetSizeOfPidl(PathPidl);

    // Add to IdaSize space for ALL pidls...
    IdaSize := IdaSize + PathPidlSize;
    for i := 0 to Files.Count-1 do
    begin
      pidl := GetSubPidl(shf, files[i]);
      try
        Inc(IdaSize, GetSizeOfPidl(Pidl));
      finally
        ShellMalloc.Free(pidl);
      end;
    end;

    // Allocate a block of memory for the list of PIDLs
    Result := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, IdaSize);
    if (Result = 0) then
      exit;
    try
      Ida := GlobalLock(Result);
      try
        FillChar(Ida^, IdaSize, 0);

        // Fill in offset and pidl data...
        Ida^.cidl := Files.Count; // cidl = file count
        pOffset := POffsets(@(Ida^.aoffset));
        pOffset^[0] := (Files.Count+2) * sizeof(UINT); // offset of Path pidl

        ptrByte := pointer(Ida);
        inc(ptrByte, pOffset^[0]); // ptrByte now points to Path pidl
        Move(PathPidl^, ptrByte^, PathPidlSize); // copy path pidl

        PreviousPidlSize := PathPidlSize;
        for i := 1 to Files.Count do
        begin
          pidl := GetSubPidl(shf,files[i-1]);
          try
            pOffset^[i] := pOffset^[i-1] + UINT(PreviousPidlSize); // offset of pidl
            PreviousPidlSize := GetSizeOfPidl(Pidl);

            ptrByte := pointer(Ida);
            inc(ptrByte, pOffset^[i]); // ptrByte now points to current file pidl
            Move(Pidl^, ptrByte^, PreviousPidlSize); // copy file pidl
            // PreviousPidlSize = current pidl size here
          finally
            ShellMalloc.Free(pidl);
          end;
        end;
      finally
        GlobalUnLock(Result);
      end;
    except
      GlobalFree(Result);
      raise;
    end;
  finally
    ShellMalloc.Free(PathPidl);
  end;
end;

function PIDLToString(pidl: PItemIDList): AnsiString;
var
  PidlLength: integer;
begin
  PidlLength := GetSizeOfPidl(pidl);
  SetLength(Result, PidlLength);
  Move(pidl^, PAnsiChar(Result)^, PidlLength);
end;

function StringToPIDL(const PIDL: AnsiString): PItemIDList;
begin
  Result := ILClone(PItemIDList(PAnsiChar(PIDL)));
end;

function JoinPIDLStrings(const pidl1, pidl2: AnsiString): AnsiString;
var
  PidlLength: integer;
begin
  if Length(pidl1) <= 2 then
    PidlLength := 0
  else
    PidlLength := Length(pidl1)-2;
  SetLength(Result, PidlLength + Length(pidl2));
  if PidlLength > 0 then
    Move(PAnsiChar(pidl1)^, PAnsiChar(Result)^, PidlLength);
  Move(PAnsiChar(pidl2)^, Result[PidlLength+1], Length(pidl2));
end;

////////////////////////////////////////////////////////////////////////////////
//
//              PIDL/IShellFolder utility functions
//
////////////////////////////////////////////////////////////////////////////////
function GetShellFolderOfPath(const FolderPath: UnicodeString): IShellFolder;
var
  DeskTopFolder: IShellFolder;
  PathPidl: pItemIDList;
  pdwAttributes: ULONG;
begin
  Result := nil;
  pdwAttributes := SFGAO_FOLDER;
  if (SHGetDesktopFolder(DeskTopFolder) <> NOERROR) then
    exit;
  if (DesktopFolder.ParseDisplayName(0, nil, PWideChar(FolderPath), PULONG(nil)^,
    PathPidl, pdwAttributes) = NOERROR) then
    try
      if (pdwAttributes and SFGAO_FOLDER <> 0) then
        DesktopFolder.BindToObject(PathPidl, nil, IID_IShellFolder,
          // Note: For Delphi 4 and prior, the ppvOut parameter must be a pointer.
          pointer(Result));
    finally
      ShellMalloc.Free(PathPidl);
    end;
end;

function GetSubPIDL(Folder: IShellFolder; const Sub: UnicodeString): pItemIDList;
begin
  Folder.ParseDisplayName(0, nil, PWideChar(Sub), PULONG(nil)^, Result,
    PULONG(nil)^);
end;

function GetPIDLDisplayName(Folder: IShellFolder; PIDL: PItemIdList; Flags: DWORD): string;
var
  StrRet: TStrRet;
begin
  Result := '';

  Folder.GetDisplayNameOf(PIDL, Flags, StrRet);
  case StrRet.uType of
    STRRET_WSTR:
      try
        Result := WideCharToString(StrRet.pOleStr);
      finally
        CoTaskMemFree(StrRet.pOleStr);
      end;
    STRRET_OFFSET:
      Result := PChar(UINT(PIDL)+StrRet.uOffset);
    STRRET_CSTR:
      Result := String(StrRet.cStr);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLList
//
////////////////////////////////////////////////////////////////////////////////
procedure TPIDLList.Add(Item: PItemIDList);
begin
  if (Item <> nil) then
  begin
    FList.Add(ILClone(Item));
    Changing;
  end;
end;

procedure TPIDLList.Add(Item: AnsiString);
begin
  FList.Add(StringToPIDL(Item));
  Changing;
end;

procedure TPIDLList.Assign(Source: TPIDLList);
var
  i: integer;
begin
  Clear;
  for i := 0 to Source.Count-1 do
    Add(Source.Items[i]);
  Changing;
end;

procedure TPIDLList.Changing;
begin
  if (Assigned(OnChanging)) then
    OnChanging(Self);
end;

procedure TPIDLList.Clear;
var
  i: integer;
begin
  for i := 0 to FList.Count-1 do
    if (FList[i] <> nil) then
      coTaskMemFree(FList[i]);
  FList.Clear;
  Changing;
end;

constructor TPIDLList.Create;
begin
  FList := TList.Create;
end;

destructor TPIDLList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPIDLList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TPIDLList.GetItem(Index: integer): PItemIDList;
begin
  Result := PItemIDList(FList[Index]);
end;

function TPIDLList.GetString(Index: integer): AnsiString;
begin
  Result := PIDLToString(Items[Index]);
end;

procedure TPIDLList.SetItem(Index: integer; const Value: PItemIDList);
begin
  if (FList[Index] <> nil) then
    coTaskMemFree(FList[Index]);

  if (Value <> nil) then
    FList[Index] := ILClone(Value)
  else
    FList[Index] := nil;
  Changing;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLsToFilenamesStrings
//
////////////////////////////////////////////////////////////////////////////////
// Used internally to convert PIDLs to filenames on-demand.
////////////////////////////////////////////////////////////////////////////////
type
  TPIDLsToFilenamesStrings = class(TUnicodeStrings)
  private
    FPIDLs: TPIDLList;
  protected
    function Get(Index: Integer): UnicodeString; override;
    procedure Put(Index: Integer; const S: UnicodeString); override;

    function GetCount: Integer; override;
  public
    constructor Create(APIDLs: TPIDLList);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: UnicodeString); override;
  end;

constructor TPIDLsToFilenamesStrings.Create(APIDLs: TPIDLList);
begin
  inherited Create;
  FPIDLs := APIDLs;
end;

function TPIDLsToFilenamesStrings.GetCount: Integer;
begin
  if FPIDLs.Count < 2 then
    Result := 0
  else
    Result := FPIDLs.Count-1;
end;

function TPIDLsToFilenamesStrings.Get(Index: Integer): UnicodeString;
var
  PIDL: AnsiString;
  Path: array [0..MAX_PATH] of WideChar;
begin
  if (Index < 0) or (Index > FPIDLs.Count-2) then
    raise Exception.create('Filename index out of range');
  PIDL := JoinPIDLStrings(FPIDLs[0], FPIDLs[Index+1]);
  if SHGetPathFromIDListW(PItemIDList(PAnsiChar(PIDL)), Path) then
    Result := Path
  else
    Result := '';
end;

procedure TPIDLsToFilenamesStrings.Assign(Source: TPersistent);
begin
  if Source is TUnicodeStrings then
  begin
    BeginUpdate;
    try
      GetPIDLsFromFilenames(TUnicodeStrings(Source), FPIDLs);
    finally
      EndUpdate;
    end;
  end else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      GetPIDLsFromFilenames(TStrings(Source), FPIDLs);
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

// Inherited abstract methods which do not need implementation...
procedure TPIDLsToFilenamesStrings.Put(Index: Integer; const S: UnicodeString);
begin
end;

procedure TPIDLsToFilenamesStrings.Clear;
begin
end;

procedure TPIDLsToFilenamesStrings.Delete(Index: Integer);
begin
end;

procedure TPIDLsToFilenamesStrings.Insert(Index: Integer; const S: UnicodeString);
begin
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TPIDLClipboardFormat.Create;
begin
  inherited Create;
  FPIDLs := TPIDLList.Create;
  FFilenames := TPIDLsToFilenamesStrings.Create(FPIDLs);
end;

destructor TPIDLClipboardFormat.Destroy;
begin
  FFilenames.Free;
  FPIDLs.Free;
  inherited Destroy;
end;

var
  CF_IDLIST: TClipFormat = 0;

function TPIDLClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_IDLIST = 0) then
    CF_IDLIST := RegisterClipboardFormat(pchar('Shell IDList Array'));
  Result := CF_IDLIST;
end;

procedure TPIDLClipboardFormat.Clear;
begin
  FPIDLs.Clear;
end;

function TPIDLClipboardFormat.HasData: boolean;
begin
  Result := (FPIDLs.Count > 0);
end;

function TPIDLClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := (FPIDLs.Count+1) * SizeOf(UINT);
  for i := 0 to FPIDLs.Count-1 do
    inc(Result, GetSizeOfPIDL(FPIDLs.Items[i]));
end;

function TPIDLClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := GetPIDLsFromData(Value, Size, FPIDLs);
end;

function TPIDLClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
var
  i: integer;
  pCIDA: PIDA;
  Offset: integer;
  pOffset: ^UINT;
  PIDL: PItemIDList;
  SourcePIDL: PItemIDList;
  PIDLSize: integer;
begin
  pCIDA := PIDA(Value);
  pCIDA^.cidl := FPIDLs.Count-1; // Don't count folder PIDL
  pOffset := @(pCIDA^.aoffset[0]); // Points to aoffset[0]
  Offset := (FPIDLs.Count+1)*SizeOf(UINT); // Size of CIDA structure
  PIDL := PItemIDList(pCIDA);
  inc(PByte(PIDL), Offset); // PIDLs are stored after CIDA structure.

  for i := 0 to FPIDLs.Count-1 do
  begin
    pOffset^ := Offset; // Store relative offset of PIDL into aoffset[i]
    // Copy the PIDL
    SourcePIDL := FPIDLs.Items[i];
    PIDLSize := GetSizeOfPIDL(SourcePIDL);
    Move(SourcePIDL^, PIDL^, PIDLSize);
    // Move on to next PIDL
    inc(Offset, PIDLSize);
    inc(pOffset);
    inc(PByte(PIDL), PIDLSize);
  end;

  Result := True;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPIDLDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TPIDLDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FPIDLs := TPIDLList.Create;
  FPIDLs.OnChanging := DoOnChanging;
  FFilenames := TPIDLsToFilenamesStrings.Create(FPIDLs);
end;

destructor TPIDLDataFormat.Destroy;
begin
  FFilenames.Free;
  FPIDLs.Free;
  inherited Destroy;
end;

function TPIDLDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TPIDLClipboardFormat) then
    FPIDLs.Assign(TPIDLClipboardFormat(Source).PIDLs)

  else if (Source is TFileClipboardFormat) then
    Result := GetPIDLsFromFilenames(TFileClipboardFormat(Source).Files, FPIDLs)

  else
    Result := inherited Assign(Source);
end;

function TPIDLDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TPIDLClipboardFormat) then
    TPIDLClipboardFormat(Dest).PIDLs.Assign(FPIDLs)

  else if (Dest is TFileClipboardFormat) then
    TFileClipboardFormat(Dest).Files.Assign(Filenames)

  else
    Result := inherited Assign(Dest);
end;

procedure TPIDLDataFormat.Clear;
begin
  FPIDLs.Clear;
end;

function TPIDLDataFormat.HasData: boolean;
begin
  Result := (FPIDLs.Count > 0);
end;

function TPIDLDataFormat.NeedsData: boolean;
begin
  Result := (FPIDLs.Count = 0);
end;


class procedure TPIDLDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  // Clipboard format registration
  RegisterDataConversion(TPIDLClipboardFormat, 0);
  RegisterDataConversion(TFileClipboardFormat, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropPIDLTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropPIDLTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPIDLDataFormat := TPIDLDataFormat.Create(Self);
  FFileMapDataFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropPIDLTarget.Destroy;
begin
  FPIDLDataFormat.Free;
  FFileMapDataFormat.Free;
  inherited Destroy;
end;

function TDropPIDLTarget.GetPIDLs: TPIDLList;
begin
  Result := FPIDLDataFormat.PIDLs;
end;

function TDropPIDLTarget.DoGetPIDL(Index: integer): pItemIdList;
begin
  Result := ILClone(PIDLs.Items[Index])
end;

function TDropPIDLTarget.GetFolderPidl: pItemIdList;
begin
  Result := DoGetPIDL(0);
end;

function TDropPIDLTarget.GetRelativeFilePidl(Index: integer): pItemIdList;
begin
  Result := nil;
  if (index < 1) then
    exit;
  Result := DoGetPIDL(Index);
end;

function TDropPIDLTarget.GetAbsoluteFilePidl(Index: integer): pItemIdList;
begin
  Result := nil;
  if (index < 1) then
    exit;
  Result := ILCombine(PIDLs.Items[0], PIDLs.Items[Index])
end;

function TDropPIDLTarget.GetPIDLCount: integer;
begin
   // Note: Includes folder PIDL in count!
  Result := FPIDLDataFormat.PIDLs.Count;
end;

function TDropPIDLTarget.GetFilenames: TUnicodeStrings;
begin
  Result := FPIDLDataFormat.Filenames;
end;

function TDropPIDLTarget.GetMappedNames: TUnicodeStrings;
begin
  Result := FFileMapDataFormat.FileMaps;
end;

function TDropPIDLTarget.GetPreferredDropEffect: LongInt;
begin
  Result := inherited GetPreferredDropEffect;
  if (Result = DROPEFFECT_NONE) then
    Result := DROPEFFECT_COPY;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropPIDLSource
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropPIDLSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPIDLDataFormat := TPIDLDataFormat.Create(Self);
  FFileMapDataFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropPIDLSource.Destroy;
begin
  FPIDLDataFormat.Free;
  FFileMapDataFormat.Free;
  inherited Destroy;
end;

procedure TDropPIDLSource.CopyFolderPIDLToList(pidl: PItemIDList);
begin
  // Note: Once the PIDL has been copied into the list it can be 'freed'.
  FPIDLDataFormat.Clear;
  FFileMapDataFormat.Clear;
  FPIDLDataFormat.PIDLs.Add(pidl);
end;

procedure TDropPIDLSource.CopyFilePIDLToList(pidl: PItemIDList);
begin
  // Note: Once the PIDL has been copied into the list it can be 'freed'.
  // Make sure that folder pidl has been added.
  if (FPIDLDataFormat.PIDLs.Count < 1) then
    raise Exception.Create(sNoFolderPIDL);
  FPIDLDataFormat.PIDLs.Add(pidl);
end;

function TDropPIDLSource.GetMappedNames: TUnicodeStrings;
begin
  Result := FFileMapDataFormat.FileMaps;
end;




////////////////////////////////////////////////////////////////////////////////
//
//              Dynamic PIDL functions
//
////////////////////////////////////////////////////////////////////////////////
var
  Shell32Handle: THandle = 0;

procedure BindILFunctions;
begin
  Shell32Handle := LoadLibrary('shell32.dll');
  if (Shell32Handle <> 0) then
  begin
    @ILFindLastID := GetProcAddress(Shell32Handle, PChar(16));
    @ILRemoveLastID := GetProcAddress(Shell32Handle, PChar(17));
    @ILClone := GetProcAddress(Shell32Handle, PChar(18));
    @ILCloneFirst := GetProcAddress(Shell32Handle, PChar(19));
    @ILIsEqual := GetProcAddress(Shell32Handle, PChar(21));
    @ILCombine := GetProcAddress(Shell32Handle, PChar(25));
    @ILGetSize := GetProcAddress(Shell32Handle, PChar(152));
    @ILGetNext := GetProcAddress(Shell32Handle, PChar(153));
    @ILFree := GetProcAddress(Shell32Handle, PChar(155));
  end;
end;

initialization
  BindILFunctions;
  TPIDLDataFormat.RegisterDataFormat;
  TPIDLClipboardFormat.RegisterFormat;

finalization
  if (Shell32Handle <> 0) then
  begin
    FreeLibrary(Shell32Handle);
    Shell32Handle := 0;
  end;
end.
