unit DragDropGraphics;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropGraphics
// Description:     Implements Dragging and Dropping of graphic data.
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
  LCLType,LCLIntf,
  DragDrop,
  DropTarget,
  DropSource,
  ActiveX,
  Windows,
  Graphics,
  Classes
  , mymetafile
  ;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TGDIClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Base class for GDI clipboard formats (TYMED_GDI).
////////////////////////////////////////////////////////////////////////////////
  TGDIClipboardFormat = class(TClipboardFormat)
  public
    constructor Create; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TPaletteClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Only used internally by TBitmapClipboardFormat - Not registered
////////////////////////////////////////////////////////////////////////////////
  TPaletteClipboardFormat = class(TGDIClipboardFormat)
  private
    FPalette: hPalette;
  protected
  public
    function GetClipboardFormat: TClipFormat; override;
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
    function DoSetData(const FormatEtcIn: TFormatEtc; var Medium: TStgMedium): boolean; override;
    procedure Clear; override;
    property Palette: hPalette read FPalette write FPalette;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomBitmapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TCustomBitmapClipboardFormat = class(TGDIClipboardFormat)
  private
    FBitmap: TBitmap;
  protected
    constructor CreateFormat(Atymed: Longint); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Bitmap: TBitmap read FBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TBitmapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TBitmapClipboardFormat = class(TCustomBitmapClipboardFormat)
  protected
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
    function DoSetData(const FormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean; override;
  public
    function GetClipboardFormat: TClipFormat; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDIBClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TDIBClipboardFormat = class(TCustomBitmapClipboardFormat)
  private
  protected
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
    function DoSetData(const FormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean; override;
  public
    constructor Create; override;
    function GetClipboardFormat: TClipFormat; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TCustomMetaFileClipboardFormat = class(TClipboardFormat)
  private
    FMetaFile: TMetaFile;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    property MetaFile: TMetaFile read FMetaFile;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TMetaFileClipboardFormat = class(TCustomMetaFileClipboardFormat)
  private
  protected
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
  public
    function GetClipboardFormat: TClipFormat; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TEnhMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TEnhMetaFileClipboardFormat = class(TCustomMetaFileClipboardFormat)
  private
  protected
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; override;
  public
    function GetClipboardFormat: TClipFormat; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TBitmapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TBitmapDataFormat = class(TCustomDataFormat)
  private
    FBitmap: TBitmap;
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
    property Bitmap: TBitmap read FBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TMetaFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TMetaFileDataFormat = class(TCustomDataFormat)
  private
    FMetaFile: TMetaFile;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    function Assign(Source: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property MetaFile: TMetaFile read FMetaFile;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropBMPTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropBMPTarget = class(TCustomDropMultiTarget)
  private
    FBitmapFormat: TBitmapDataFormat;
  protected
    function GetBitmap: TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read GetBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropBMPSource
//
////////////////////////////////////////////////////////////////////////////////
  TDropBMPSource = class(TCustomDropMultiSource)
  private
    FBitmapFormat: TBitmapDataFormat;
  protected
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmap: TBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropMetaFileTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropMetaFileTarget = class(TCustomDropMultiTarget)
  private
    FMetaFileFormat: TMetaFileDataFormat;
  protected
    function GetMetaFile: TMetaFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MetaFile: TMetaFile read GetMetaFile;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropImageTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropImageTarget = class(TCustomDropMultiTarget)
  private
    FMetaFileFormat: TMetaFileDataFormat;
    FBitmapFormat: TBitmapDataFormat;
    FPicture: TPicture;
  protected
    function DoGetData: boolean; override;
    procedure ClearData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Picture: TPicture read FPicture;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////
procedure CopyDIBToBitmap(Bitmap: TBitmap; BitmapInfo: PBitmapInfo; DIBSize: integer);
function GetHGlobalDIBFromBitmap(Bitmap: TBitmap): HGlobal;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//              	IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////
procedure CopyDIBToBitmap(Bitmap: TBitmap; BitmapInfo: PBitmapInfo; DIBSize: integer);
var
  BitmapFileHeader: TBitmapFileHeader;
  FileSize: integer;
  InfoSize: integer;
  Stream: TMemoryStream;
begin
  // Write DIB to a stream in the BMP file format
  Stream := TMemoryStream.Create;
  try
    FileSize := sizeof(TBitmapFileHeader) + DIBSize;
    InfoSize := sizeof(TBitmapInfoHeader);
    if (BitmapInfo^.bmiHeader.biBitCount > 8) then
    begin
      if ((BitmapInfo^.bmiHeader.biCompression and BI_BITFIELDS) <> 0) then
        Inc(InfoSize, 12);
    end else
      Inc(InfoSize, sizeof(TRGBQuad) * (1 shl BitmapInfo^.bmiHeader.biBitCount));
    Stream.SetSize(FileSize);
    // Initialize file header
    FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
    with BitmapFileHeader do
    begin
      bfType := $4D42; // 'BM' = Windows BMP signature
      bfSize := FileSize; // File size (not needed)
      bfOffBits := sizeof(TBitmapFileHeader) + InfoSize; // Offset of pixel data
    end;
    // Save file header
    Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
    // Save TBitmapInfo structure and pixel data
    Stream.Write(BitmapInfo^, DIBSize);

    // Rewind and load bitmap from stream
    Stream.Position := 0;
    Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function GetHGlobalDIBFromBitmap(Bitmap: TBitmap): HGlobal;
var
  Stream: TMemoryStream;
  DIB: pointer;
  DIBSize: integer;
begin
  Stream := TMemoryStream.Create;
  try
    // Write bitmap to a stream and extract the DIB data from it.
    Bitmap.SaveToStream(Stream);

    // Calculate size of DIB block.
    DIBSize := Stream.Size - SizeOf(TBitmapFileHeader);

    // Allocate memory for DIB data.
    Result := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, DIBSize);
    if (Result = 0) then
      exit;

    DIB := GlobalLock(Result);
    if DIB = nil then
    begin
      GlobalFree(Result);
      Result := 0;
    end else
    begin
      // Skip BMP file header.
      Stream.Seek(SizeOf(TBitmapFileHeader), soFromBeginning);
      // Transfer data from stream to global memory.
      if (Stream.Read(DIB^, DIBSize) <> DIBSize) then
      begin
        GlobalUnlock(Result);
        GlobalFree(Result);
        Result := 0;
      end else
        GlobalUnlock(Result);
    end;
  finally
    Stream.free;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TGDIClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TGDIClipboardFormat.Create;
begin
  CreateFormat(TYMED_GDI);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TPaletteClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function CopyPalette(Source: hPalette): hPalette;
var
  LP: ^TLogPalette;
  NumEntries: integer;
begin
  Result := 0;
  GetMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
  try
    with LP^ do
      begin
      palVersion := $300;
      palNumEntries := 256;
      NumEntries := GetPaletteEntries(Source, 0, 256, palPalEntry);
      if NumEntries > 0 then
        begin
        palNumEntries := NumEntries;
        Result := CreatePalette(LP^);
        end;
      end;
  finally
    FreeMem(LP, Sizeof(TLogPalette) + 256*Sizeof(TPaletteEntry));
  end;
end;

function TPaletteClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_PALETTE;
end;

procedure TPaletteClipboardFormat.Clear;
begin
  if (FPalette <> 0) then
  begin
    DeleteObject(FPalette);
    FPalette := 0;
  end;
end;

function TPaletteClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
begin
  if (AMedium.hBitmap <> 0) then
  begin
    FPalette := CopyPalette(AMedium.hBitmap);
    Result := (FPalette <> 0);
  end else
    Result := False;
end;

function TPaletteClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var Medium: TStgMedium): boolean;
begin
  Result := False;

  try
    Medium.hBitmap := CopyPalette(FPalette);
  except
    exit;
  end;

  if (Medium.hBitmap <> 0) then
  begin
    Medium.tymed := TYMED_GDI;
    result := True;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TBitmapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomBitmapClipboardFormat.CreateFormat(Atymed: Longint);
begin
  inherited CreateFormat(Atymed);

  FBitmap := Graphics.TBitmap.Create;
end;

destructor TCustomBitmapClipboardFormat.Destroy;
begin
  if (FBitmap <> nil) then
    FBitmap.Free;

  inherited Destroy;
end;

procedure TCustomBitmapClipboardFormat.Clear;
begin
  FBitmap.Handle := 0;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TBitmapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TBitmapClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_BITMAP;
end;

function TBitmapClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
var
  Palette: TPaletteClipboardFormat;
begin
  Result := False;
  if (AMedium.hBitmap = 0) then
    exit;
  Palette := TPaletteClipboardFormat.Create;
  try
    // Attempt to get palette from source. However in case the bitmap is in a
    // format which doesn't use palettes, there might not be one available.
    // The CF_BITMAP/CF_PALETTE documentation doesn't mention if CF_BITMAP must
    // always be accompanied with a CF_PALETTE.
    Palette.GetData(ADataObject);
    // Let TBitmap do the work for us.
//    FBitmap.LoadFromClipboardFormat(CF_BITMAP, AMedium.hBitmap, Palette.Palette);
    FBitmap.LoadFromClipboardFormat( Palette.Palette);
  finally
    Palette.Free;
  end;
  Result := True;
end;

function TBitmapClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
var
  Palette: HPalette;
  Format: Word;
  hBitmap: THandle;
begin
  Result := False;

  try
    Format := CF_BITMAP;
//    FBitmap.SaveToClipboardFormat(Format, hBitmap, Palette);
    FBitmap.SaveToClipboardFormat( Palette);
    AMedium.hBitmap := hBitmap;
  except
    exit;
  end;

  try
    if (Format <> CF_BITMAP) then
    begin
      DeleteObject(AMedium.hBitmap);
      AMedium.hBitmap := 0;
      exit;
    end;
    AMedium.tymed := TYMED_GDI;
  finally
    DeleteObject(Palette);
  end;
  Result := True;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDIBClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TDIBClipboardFormat.Create;
begin
  // Note: We must override Create since base class Create sets tymed to
  // TYMED_GDI. 
  CreateFormat(TYMED_HGLOBAL);
end;

function TDIBClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_DIB;
end;

// http://x5.dejanews.com/[ST_rn=ps]/getdoc.xp?AN=382056726.2&CONTEXT=925473183.2090336317&hitnum=0
function TDIBClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
var
  BitmapInfo: PBitmapInfo;
  BitmapFileHeader: TBitmapFileHeader;
  DIBSize: integer;
  FileSize: integer;
  InfoSize: integer;
  Stream: TMemoryStream;
begin
  // Get data source's DIB block
  BitmapInfo := GlobalLock(AMedium.HGlobal);
  try
    Result := (BitmapInfo <> nil);
    if (not Result) then
      exit;

    // Write DIB to a stream in the BMP file format
    Stream := TMemoryStream.Create;
    try
      // Get size of data source's DIB block
      DIBSize := GlobalSize(AMedium.HGlobal);
      // Calculate total bitmap file size
      FileSize := sizeof(TBitmapFileHeader) + DIBSize;
      // Calculate bitmap header size
      InfoSize := sizeof(TBitmapInfoHeader);
      if (BitmapInfo^.bmiHeader.biBitCount > 8) then
      begin
        if ((BitmapInfo^.bmiHeader.biCompression and BI_BITFIELDS) <> 0) then
          Inc(InfoSize, 12);
      end else
        Inc(InfoSize, sizeof(TRGBQuad) * (1 shl BitmapInfo^.bmiHeader.biBitCount));

      Stream.SetSize(FileSize);
      // Initialize file header
      FillChar(BitmapFileHeader, sizeof(TBitmapFileHeader), 0);
      with BitmapFileHeader do
      begin
        bfType := $4D42; // 'BM' = Windows BMP signature
        bfSize := FileSize; // File size (not needed)
        bfOffBits := sizeof(TBitmapFileHeader) + InfoSize; // Offset of pixel data
      end;
      // Save file header
      Stream.Write(BitmapFileHeader, sizeof(TBitmapFileHeader));
      // Save TBitmapInfo structure and pixel data
      Stream.Write(BitmapInfo^, DIBSize);

      // Rewind and load bitmap from stream
      Stream.Position := 0;
      FBitmap.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    GlobalUnlock(AMedium.HGlobal);
  end;
end;

function TDIBClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
begin
  AMedium.hBitmap := GetHGlobalDIBFromBitmap(FBitmap);
  Result := (AMedium.hBitmap <> 0);
  if (Result) then
    AMedium.tymed := TYMED_HGLOBAL;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomMetaFileClipboardFormat.Create;
begin
  CreateFormat(TYMED_MFPICT);
  FMetaFile := TMetaFile.Create;
end;

destructor TCustomMetaFileClipboardFormat.Destroy;
begin
  if (FMetaFile <> nil) then
    FMetaFile.Free;
  inherited Destroy;
end;

procedure TCustomMetaFileClipboardFormat.Clear;
begin
  FMetaFile.Clear;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TMetaFileClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_METAFILEPICT;
end;

function WMF2EMF(const MetaFile: TMetaFilePict): hEnhMetaFile;
var
  Bits: Pointer;
  Length: UINT;
  RefDC: HDC;
begin
  Length := GetMetaFileBitsEx(MetaFile.hMF, 0, nil);
  if (Length = 0) then
    _RaiseLastWin32Error;
  GetMem(Bits, Length);
  try
    if (GetMetaFileBitsEx(MetaFile.hMF, Length, Bits) < Length) then
      _RaiseLastWin32Error;
    RefDC := GetDC(0);
    try
        Result := SetWinMetaFileBits(Length, Bits, RefDC, MetaFile);
    finally
      ReleaseDC(0, RefDC);
    end;
    if (Result = 0) then
      _RaiseLastWin32Error;
  finally
    FreeMem(Bits);
  end;
end;

function TMetaFileClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
var
  pMetaFile: PMetaFilePict;
begin
  pMetaFile := GlobalLock( longword(AMedium.hMetaFilePict^));
  try
    Result := (pMetaFile <> nil);
    if (Result) then
      FMetaFile.Handle := WMF2EMF(pMetaFile^);
  finally
    GlobalUnlock(longword(AMedium.hMetaFilePict^));
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TEnhMetaFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TEnhMetaFileClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_ENHMETAFILE;
end;

function TEnhMetaFileClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
begin
  Result := (AMedium.hEnhMetaFile <> 0);
  if (Result) then
    FMetaFile.Handle := CopyEnhMetafile(AMedium.hEnhMetaFile, nil);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TBitmapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TBitmapDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
  // TGraphic.OnChange is fired too late (after change), but it's the best
  // we can get.
  FBitmap.OnChange := DoOnChanging;
end;

destructor TBitmapDataFormat.Destroy;
begin
  Clear;
  FBitmap.Free;
  inherited Destroy;
end;

function TBitmapDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TDIBClipboardFormat) then
    FBitmap.Assign(TDIBClipboardFormat(Source).Bitmap)

  else if (Source is TBitmapClipboardFormat) then
    FBitmap.Assign(TBitmapClipboardFormat(Source).Bitmap)

  // TODO -oanme : Is this nescessary? Palette is extracted in TBitmapClipboardFormat GetData.
  else if (Source is TPaletteClipboardFormat) then
    FBitmap.Palette := CopyPalette(TPaletteClipboardFormat(Source).Palette)

  else
    Result := inherited Assign(Source);
end;

function TBitmapDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TDIBClipboardFormat) then
    TDIBClipboardFormat(Dest).Bitmap.Assign(FBitmap)

  else if (Dest is TBitmapClipboardFormat) then
    TBitmapClipboardFormat(Dest).Bitmap.Assign(FBitmap)

  else if (Dest is TPaletteClipboardFormat) then
    TPaletteClipboardFormat(Dest).Palette := CopyPalette(FBitmap.Palette)

  else
    Result := inherited AssignTo(Dest);
end;

procedure TBitmapDataFormat.Clear;
begin
  Changing;
  FBitmap.Handle := 0;
end;

function TBitmapDataFormat.HasData: boolean;
begin
  Result := (not FBitmap.Empty);
end;

function TBitmapDataFormat.NeedsData: boolean;
begin
  Result := (FBitmap.Empty);
end;


class procedure TBitmapDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TDIBClipboardFormat, 0);
  RegisterDataConversion(TBitmapClipboardFormat, 1);
  RegisterDataConversion(TPaletteClipboardFormat, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TMetaFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TMetaFileDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FMetaFile := TMetaFile.Create;
  // TGraphic.OnChange is fired too late (after change), but it's the best
  // we can get.
  FMetaFile.OnChange := DoOnChanging;
end;

destructor TMetaFileDataFormat.Destroy;
begin
  Clear;
  FMetaFile.Free;
  inherited Destroy;
end;

function TMetaFileDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TMetaFileClipboardFormat) then
    FMetaFile.Assign(TMetaFileClipboardFormat(Source).MetaFile)

  else if (Source is TEnhMetaFileClipboardFormat) then
    FMetaFile.Assign(TEnhMetaFileClipboardFormat(Source).MetaFile)

  else
    Result := inherited Assign(Source);
end;

procedure TMetaFileDataFormat.Clear;
begin
  Changing;
  FMetaFile.Clear;
end;

function TMetaFileDataFormat.HasData: boolean;
begin
  Result := (FMetaFile.Handle <> 0);
end;

function TMetaFileDataFormat.NeedsData: boolean;
begin
  Result := (FMetaFile.Handle = 0);
end;


class procedure TMetaFileDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataProvider(TEnhMetaFileClipboardFormat, 0);
  RegisterDataProvider(TMetaFileClipboardFormat, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropBMPTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropBMPTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmapFormat := TBitmapDataFormat.Create(Self);
end;

destructor TDropBMPTarget.Destroy;
begin
  FBitmapFormat.Free;
  inherited Destroy;
end;

function TDropBMPTarget.GetBitmap: TBitmap;
begin
  Result := FBitmapFormat.Bitmap;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropBMPSource
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropBMPSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DragTypes := [dtCopy]; // Default to Copy

  FBitmapFormat := TBitmapDataFormat.Create(Self);
end;

destructor TDropBMPSource.destroy;
begin
  FBitmapFormat.Free;
  inherited Destroy;
end;

function TDropBMPSource.GetBitmap: TBitmap;
begin
  Result := FBitmapFormat.Bitmap;
end;

procedure TDropBMPSource.SetBitmap(const Value: TBitmap);
begin
  FBitmapFormat.Bitmap.Assign(Value);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropMetaFileTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropMetaFileTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMetaFileFormat := TMetaFileDataFormat.Create(Self);
end;

destructor TDropMetaFileTarget.Destroy;
begin
  FMetaFileFormat.Free;
  inherited Destroy;
end;

function TDropMetaFileTarget.GetMetaFile: TMetaFile;
begin
  Result := FMetaFileFormat.MetaFile;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropMetaFileTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropImageTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMetaFileFormat := TMetaFileDataFormat.Create(Self);
  FBitmapFormat := TBitmapDataFormat.Create(Self);
  FPicture := TPicture.Create;
end;

destructor TDropImageTarget.Destroy;
begin
  FPicture.Free;
  FBitmapFormat.Free;
  FMetaFileFormat.Free;
  inherited Destroy;
end;

procedure TDropImageTarget.ClearData;
begin
  inherited ClearData;
  FPicture.Assign(nil);
end;

function TDropImageTarget.DoGetData: boolean;
begin
  Result := inherited DoGetData;
  if (Result) then
  begin
    if (FBitmapFormat.HasData) then
      FPicture.Assign(FBitmapFormat.Bitmap)
    else if (FMetaFileFormat.HasData) then
      FPicture.Assign(FMetaFileFormat.MetaFile)
    else
      Result := False;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              Initialization/Finalization
//
////////////////////////////////////////////////////////////////////////////////

initialization
  // Data format registration
  TBitmapDataFormat.RegisterDataFormat;
  TMetaFileDataFormat.RegisterDataFormat;
  // Clipboard format registration
  TDIBClipboardFormat.RegisterFormat;
  TBitmapClipboardFormat.RegisterFormat;
  TPaletteClipboardFormat.RegisterFormat;
  TEnhMetaFileClipboardFormat.RegisterFormat;
  TMetaFileClipboardFormat.RegisterFormat;

finalization
end.

