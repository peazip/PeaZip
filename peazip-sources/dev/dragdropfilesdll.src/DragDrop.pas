unit DragDrop;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDrop
// Description:     Implements base classes and utility functions.
// Version:         5.2
// Date:            17-AUG-2010
// Target:          Win32, Delphi 5-2010
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//
// Lazarus adaption 10/2017 Michael Köcher / six1
// -----------------------------------------------------------------------------
// TODO -oanme -cPortability : Replace all public use of HWND with THandle. BCB's HWND <> Delphi's HWND.

interface

{$include DragDrop.inc}

uses
  Graphics,
  Classes,
  Controls,
  Windows,
  ActiveX;




const
  DragDropSuiteVersion = 5.0;
  DragDropSuiteVersionMajor = 5;
  DragDropSuiteVersionMinor = 0;

// Pre-unicode compatibility
{$ifndef UNICODE}
type
  UnicodeString = type WideString;
{$endif}

type
  TBitmap = class(Graphics.TBitmap)
  public
    procedure SetSize(NewWidth, NewHeight: integer);
  end;

const
  {$EXTERNALSYM DROPEFFECT_NONE}
  {$EXTERNALSYM DROPEFFECT_COPY}
  {$EXTERNALSYM DROPEFFECT_MOVE}
  {$EXTERNALSYM DROPEFFECT_LINK}
  {$EXTERNALSYM DROPEFFECT_SCROLL}
  DROPEFFECT_NONE   = ActiveX.DROPEFFECT_NONE;
  DROPEFFECT_COPY   = ActiveX.DROPEFFECT_COPY;
  DROPEFFECT_MOVE   = ActiveX.DROPEFFECT_MOVE;
  DROPEFFECT_LINK   = ActiveX.DROPEFFECT_LINK;
  DROPEFFECT_SCROLL = ActiveX.DROPEFFECT_SCROLL;

type
  (*
  ** TDragType enumerates the three possible drag/drop operations.
  *)
  TDragType = (dtCopy, dtMove, dtLink);
  TDragTypes = set of TDragType;

type
  (*
  ** TDataDirection is used by TClipboardFormat to specify if the drop target
  ** can read and/or write to the clipboard format.
  **
  ** ddRead     Destination (IDropTarget) can read data from IDataObject.
  **            I.e. the target can read from the source.
  ** ddWrite    Destination (IDropTarget) can write data to IDataObject.
  **            I.e. the target can write to the source.
  *)
  TDataDirection = (ddRead, ddWrite);
  TDataDirections = set of TDataDirection;

type
  (*
  ** TConversionCapability is used by the clipboard format registration to
  ** specify if a clipboard format conversion can provide and/or consume data.
  **
  ** ccWriteToDataFormat        The clipboard format can provide data to the
  **                            data format.
  ** ccReadFromDataFormat       The clipboard format can consume data from the
  **                            data format.
  *)
  TConversionCapability = (ccWriteToDataFormat, ccReadFromDataFormat);
  TConversionCapabilities = set of TConversionCapability;

const
  // UnicodeBoost is used to give Unicode dataformat/clipboardformat conversions
  // priority if Unicode is enabled.
  // UnicodeBoost is -1 if Unicode is enabled and 0 if it isn't.
  // AnsiBoost is the opposite of UnicodeBoost.
  UnicodeBoost = {$ifdef UNICODE} -1 {$else} 0 {$endif};
  AnsiBoost = {$ifdef UNICODE} 0 {$else} -1 {$endif};

////////////////////////////////////////////////////////////////////////////////
//
//              TInterfacedComponent
//
////////////////////////////////////////////////////////////////////////////////
// Top level base class for the drag/drop component hierachy.
// Implements the IUnknown interface.
// Corresponds to TInterfacedObject (see VCL online help), but descends from
// TComponent instead of TObject.
// Reference counting is disabled (_AddRef and _Release methods does nothing)
// since the component life span is controlled by the component owner.
////////////////////////////////////////////////////////////////////////////////
type
  TInterfacedComponent = class(TComponent, IUnknown)
  protected
    function QueryInterface(constref IID: TGuid; out Obj): HRESULT; virtual; stdcall;
//      {$IFDEF VER13_PLUS} override; {$ELSE} reintroduce; {$ENDIF} stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class. Extracts or injects data of a specific low level format
// from or to an IDataObject.
////////////////////////////////////////////////////////////////////////////////
type
  TCustomDataFormat = class;
  TDataFormatClass = class of TCustomDataFormat;

  TClipboardFormat = class(TObject)
  private
    FDataFormat: TCustomDataFormat;
  protected
    FFormatEtc: TFormatEtc;
    constructor CreateFormat(Atymed: Longint); virtual;
    constructor CreateFormatEtc(const AFormatEtc: TFormatEtc); virtual;
    // Extract data from the specified medium
    function DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean; virtual;
    // Transfer data to the specified medium
    function DoSetData(const FormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean; virtual;
    function GetClipboardFormat: TClipFormat; virtual;
    procedure SetClipboardFormat(Value: TClipFormat); virtual;
    function GetClipboardFormatName: string; virtual;
    procedure SetClipboardFormatName(const Value: string); virtual;
    procedure SetFormatEtc(const Value: TFormatEtc);
    // Determines which media the data object support
    function GetValidMedia(const ADataObject: IDataObject): longInt; virtual;
    class procedure RegisterCompatibleFormats; virtual;
    // Register a data format that is handled by the Assign method
    class procedure RegisterDataProvider(DataFormatClass: TDataFormatClass; Priority: integer = 0);
    // Register a data format that is handled by the AssignTo method
    class procedure RegisterDataConsumer(DataFormatClass: TDataFormatClass);
    // Register a data format that is handled by both the Assign and the AssignTo method
    class procedure RegisterDataConversion(DataFormatClass: TDataFormatClass;
      Priority: integer = 0;
      Capabilities: TConversionCapabilities = [ccWriteToDataFormat, ccReadFromDataFormat]);
  public
    constructor Create; virtual; abstract;
    destructor Destroy; override;
    // Determines if the object can read from the specified data object
    function HasValidFormats(const ADataObject: IDataObject): boolean; virtual;
    // Determines if the object can read the specified format
    function AcceptFormat(const AFormatEtc: TFormatEtc): boolean; virtual;
    // Extracts data from the specified IDataObject
    function GetData(const ADataObject: IDataObject): boolean; virtual;
    // Extracts data from the specified IDataObject via the specified medium
    function GetDataFromMedium(const ADataObject: IDataObject;
      var AMedium: TStgMedium): boolean; virtual;
    // Transfers data to the specified IDataObject
    function SetData(const ADataObject: IDataObject; const FormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; virtual;
    // Transfers data to the specified medium
    function SetDataToMedium(const FormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean;
    // Copies data from the specified source format to the object
    function Assign(Source: TCustomDataFormat): boolean; virtual;
    // Copies data from the object to the specified target format
    function AssignTo(Dest: TCustomDataFormat): boolean; virtual;
    // Clears the objects data
    procedure Clear; virtual; abstract;
    // Returns true if object can supply data
    function HasData: boolean; virtual;
    // Register all data formats that this class can read or write to
    class procedure RegisterFormat; virtual;
    // Unregisters the clipboard format and all mappings involving it from the global database
    class procedure UnregisterClipboardFormat;
    // Specifies whether the IDropTarget can read and write to the IDataObject
    class function DataDirection: TDataDirections; virtual;
    // Returns the clipboard format value
    property ClipboardFormat: TClipFormat read GetClipboardFormat write SetClipboardFormat;
    // Returns the clipboard format name
    property ClipboardFormatName: string read GetClipboardFormatName write SetClipboardFormatName;
    // Provides access to the objects format specification
    property FormatEtc: TFormatEtc read FFormatEtc;
    // Specifies the data format which owns and controls this clipboard format
    property DataFormat: TCustomDataFormat read FDataFormat write FDataFormat;
  end;

  TClipboardFormatClass = class of TClipboardFormat;

  // TClipboardFormats
  // List of TClipboardFormat objects.
  TClipboardFormats = class(TObject)
  private
    FList: TList;
    FOwnsObjects: boolean;
    FDataFormat: TCustomDataFormat;
  protected
    function DoGetFormat(Index: integer): TClipboardFormat;
    function GetCount: integer;
  public
    constructor Create(ADataFormat: TCustomDataFormat; AOwnsObjects: boolean = True);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TClipboardFormats);
    function Add(ClipboardFormat: TClipboardFormat): integer;
    function FindFormat(ClipboardFormat: TClipFormat): TClipboardFormat; overload;
    function FindFormat(ClipboardFormatClass: TClipboardFormatClass): TClipboardFormat; overload;
    function GetFormat(ClipboardFormat: TClipFormat): TClipboardFormat; overload;
    function GetFormat(ClipboardFormatClass: TClipboardFormatClass): TClipboardFormat; overload;
    property Formats[Index: integer]: TClipboardFormat read DoGetFormat; default;
    property Count: integer read GetCount;
    property DataFormat: TCustomDataFormat read FDataFormat;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropComponent
//
////////////////////////////////////////////////////////////////////////////////
// Base class for drag/drop components.
////////////////////////////////////////////////////////////////////////////////
  TDataFormats = class;

  TDragDropComponent = class(TInterfacedComponent)
  private
  protected
    FDataFormats: TDataFormats;
    //: Only used by TCustomDropMultiSource and TCustomDropMultiTarget and
    // their descendants.
    property DataFormats: TDataFormats read FDataFormats;
  public
    // Descendant classes override GetCompatibleClipboardFormats to fetch the
    // clipboard formats that are compatible with the class.
    procedure GetCompatibleClipboardFormats(const DataFormatClass: TDataFormatClass;
      ClipboardFormats: TClipboardFormats); virtual;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class.
// Renders the data of one or more TClipboardFormat objects to or from a
// specific high level data format.
////////////////////////////////////////////////////////////////////////////////
  TDataFormatDirection = (dfdConsumer, dfdProvider);

  TCustomDataFormat = class(TObject)
  private
    FCompatibleFormats: TClipboardFormats;
    FFormatList: TDataFormats;
    FOwner: TDragDropComponent;
    FOnChanging: TNotifyEvent;
  protected
    { Determines if the object can accept data from the specified source format }
    function SupportsFormat(ClipboardFormat: TClipboardFormat): boolean;
    procedure DoOnChanging(Sender: TObject);
    procedure Changing; virtual;
    procedure CreateCompatibleFormatsList;
    { Register all clipboard formats that this data format can read and write to }
    class procedure RegisterCompatibleFormats; virtual;
    // Register a clipboard format that is handled by the Assign method
    class procedure RegisterDataProvider(ClipboardFormatClass: TClipboardFormatClass; Priority: integer = 0);
    // Register a clipboard format that is handled by the AssignTo method
    class procedure RegisterDataConsumer(ClipboardFormatClass: TClipboardFormatClass);
    // Register a clipboard format that is handled by both the Assign and the AssignTo method
    class procedure RegisterDataConversion(ClipboardFormatClass: TClipboardFormatClass;
      Priority: integer = 0;
      Capabilities: TConversionCapabilities = [ccWriteToDataFormat, ccReadFromDataFormat]);

    function GetCompatibleFormats: TClipboardFormats;
    property FormatList: TDataFormats read FFormatList;
  public
    constructor Create(AOwner: TDragDropComponent);  overload; virtual;
    constructor Create(Dummy: pointer); overload; deprecated;
    constructor Create(Direction: TDataFormatDirection); overload; virtual;
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
    { Copies data to and from the specified clipboard format object }
    function Assign(Source: TClipboardFormat): boolean; virtual;
    function AssignTo(Dest: TClipboardFormat): boolean; virtual;
    { Extracts data from the specified IDataObject }
    function GetData(DataObject: IDataObject): boolean; virtual;
    { Determines if the object contains *any* data }
    function HasData: boolean; virtual; abstract;
    { Determines if the object needs/can use *more* data }
    function NeedsData: boolean; virtual;
    { Determines if the object can read from the specified data object }
    function HasValidFormats(const ADataObject: IDataObject): boolean; virtual;
    { Determines if the object can read the specified format }
    function AcceptFormat(const FormatEtc: TFormatEtc): boolean; virtual;
    { Registers the data format in the data format list }
    class procedure RegisterDataFormat;
    { Unregisters data format and all mappings involving it from the global database }
    class procedure UnregisterDataFormat;
    { List of compatible source formats }
    property CompatibleFormats: TClipboardFormats read GetCompatibleFormats;
    property Owner: TDragDropComponent read FOwner;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    // TODO : Add support for delayed rendering with DelayedRender property.
  end;

  // TDataFormats
  // List of TCustomDataFormat objects.
  TDataFormats = class(TObject)
  private
    FList: TList;
  protected
    function GetFormat(Index: integer): TCustomDataFormat;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(DataFormat: TCustomDataFormat): integer; virtual;
    function IndexOf(DataFormat: TCustomDataFormat): integer; virtual;
    procedure Remove(DataFormat: TCustomDataFormat); virtual;
    property Formats[Index: integer]: TCustomDataFormat read GetFormat; default;
    property Count: integer read GetCount;
  end;

  TDataFormatClassListProxy = class
  private
    class function GetItem(Index: integer): TDataFormatClass;
  public
    property Items[Index: integer]: TDataFormatClass read GetItem; default;
  end;

  // TDataFormatClasses
  // List of TCustomDataFormat classes.
  TDataFormatClasses = class(TObject)
  private
    FList: TList;

    { Provides singleton access to the global data format database }
    class function Instance: TDataFormatClasses;
  protected
    class function GetFormat(Index: integer): TDataFormatClass; static;
    class function GetCount: integer; static;
  public
    constructor Create;
    destructor Destroy; override;
    class function Add(DataFormat: TDataFormatClass): integer; // virtual;
    class procedure Remove(DataFormat: TDataFormatClass); // virtual;
    class function Count: integer;
    class function Formats: TDataFormatClassListProxy;
  end;

  // TDataFormatMap
  // Format conversion database. Contains mappings between TClipboardFormat
  // and TCustomDataFormat.
  // Used internally by TCustomDropMultiTarget and TCustomDropMultiSource.
  TDragDropTier = (ddtSource, ddtTarget);

  TDataFormatMap = class(TObject)
  private
    FList: TList;
  protected
    function FindMap(DataFormatClass: TDataFormatClass; ClipboardFormatClass: TClipboardFormatClass): integer;
    procedure Sort;
    { Provides singleton access to the global format map database }
    class function Instance: TDataFormatMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(DataFormatClass: TDataFormatClass;
      ClipboardFormatClass: TClipboardFormatClass;
      Priority: integer = 0;
      Capabilities: TConversionCapabilities = [ccWriteToDataFormat, ccReadFromDataFormat]);
    procedure Delete(DataFormatClass: TDataFormatClass;
      ClipboardFormatClass: TClipboardFormatClass);
    procedure DeleteByClipboardFormat(ClipboardFormatClass: TClipboardFormatClass);
    procedure DeleteByDataFormat(DataFormatClass: TDataFormatClass);
    class procedure GetClipboardFormats(const DataFormatClass: TDataFormatClass;
      ClipboardFormats: TClipboardFormats; Tier: TDragDropTier);
    function CanMap(DataFormatClass: TDataFormatClass;
      ClipboardFormatClass: TClipboardFormatClass): boolean;

    { Registers the specified format mapping }
    procedure RegisterFormatMap(DataFormatClass: TDataFormatClass;
      ClipboardFormatClass: TClipboardFormatClass;
      Priority: integer = 0;
      Capabilities: TConversionCapabilities = [ccWriteToDataFormat, ccReadFromDataFormat]);
    { Unregisters the specified format mapping }
    procedure UnregisterFormatMap(DataFormatClass: TDataFormatClass;
      ClipboardFormatClass: TClipboardFormatClass);
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDataFormatAdapter
//
////////////////////////////////////////////////////////////////////////////////
// Helper component used to add additional data formats to a drop source or
// target at design time.
// Requires that data formats have been registered with
// TCustomDataFormat.RegisterDataFormat.
////////////////////////////////////////////////////////////////////////////////
  TDataFormatAdapter = class(TComponent)
  private
    FDragDropComponent: TDragDropComponent;
    FDataFormat: TCustomDataFormat;
    FDataFormatClass: TDataFormatClass;
    FDataFormatClassName: string; // only used at design time
    FEnabled: boolean;
  protected
    function GetDataFormatName: string;
    procedure SetDataFormatName(const Value: string);
    procedure SetDataFormatClass(const Value: TDataFormatClass);
    procedure SetDragDropComponent(const Value: TDragDropComponent);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataFormatClass: TDataFormatClass read FDataFormatClass
      write SetDataFormatClass;
    property DataFormat: TCustomDataFormat read FDataFormat;
  published
    property DragDropComponent: TDragDropComponent read FDragDropComponent
      write SetDragDropComponent;
    property DataFormatName: string read GetDataFormatName
      write SetDataFormatName;
    property Enabled: boolean read GetEnabled write SetEnabled default True;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Drag Drop helper interfaces
//
////////////////////////////////////////////////////////////////////////////////
// Requires Windows 2000 or later.
////////////////////////////////////////////////////////////////////////////////
type
  PSHDRAGIMAGE = ^TSHDRAGIMAGE;
  {_$EXTERNALSYM _SHDRAGIMAGE}
  _SHDRAGIMAGE = packed record
    sizeDragImage: TSize;               { The length and Width of the rendered image }
    ptOffset: TPoint;                   { The Offset from the mouse cursor to the upper left corner of the image }
    hbmpDragImage: HBitmap;             { The Bitmap containing the rendered drag images }
    crColorKey: COLORREF;               { The COLORREF that has been blitted to the background of the images }
  end;
  TSHDRAGIMAGE = _SHDRAGIMAGE;
  {_$EXTERNALSYM SHDRAGIMAGE}
  SHDRAGIMAGE = _SHDRAGIMAGE;

const
  CLSID_DragDropHelper: TGUID = (
    D1:$4657278a; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));
  SID_DragDropHelper = '{4657278A-411B-11d2-839A-00C04FD918D0}';

const
  IID_IDropTargetHelper: TGUID = (
    D1:$4657278b; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';

type
  {_$EXTERNALSYM IDropTargetHelper}
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const DataObj: IDataObject;
      var pt: TPoint; dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function DragOver(var pt: TPoint; dwEffect: longInt): HResult; stdcall;
    function Drop(const DataObj: IDataObject; var pt: TPoint;
      dwEffect: longInt): HResult; stdcall;
    function Show(Show: BOOL): HResult; stdcall;
  end;

const
  IID_IDragSourceHelper: TGUID = (
    D1:$de5bf786; D2:$477a; D3:$11d2; D4:($83,$9d,$00,$c0,$4f,$d9,$18,$d0));
  SID_IDragSourceHelper = '{DE5BF786-477A-11d2-839D-00C04FD918D0}';

type
  {_$EXTERNALSYM IDragSourceHelper}
  IDragSourceHelper = interface(IUnknown)
    [SID_IDragSourceHelper]
    function InitializeFromBitmap(var shdi: TSHDRAGIMAGE;
      const DataObj: IDataObject): HResult; stdcall;
    function InitializeFromWindow(hwnd: HWND; var pt: TPoint;
      const DataObj: IDataObject): HResult; stdcall;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Async data transfer interfaces
//
////////////////////////////////////////////////////////////////////////////////
// Requires Windows 2000 or later.
////////////////////////////////////////////////////////////////////////////////
const
  IID_IAsyncOperation: TGUID = ( D1:$3D8B0590; D2:$F691; D3:$11D2; D4:($8E,$A9,$00,$60,$97,$DF,$5B,$D4));
  SID_IAsyncOperation = '{3D8B0590-F691-11D2-8EA9-006097DF5BD4}';

type
  // Note:
  // IAsyncOperation is declared in C++Builder 5 and later (in shldisp.h), but
  // for some reason C++Builder can't handle that we also define it here. Not
  // even if we use the $EXTERNALSYM compiler switch.
  // To work around this problem we have renamed IAsyncOperation to
  // IAsyncOperation2.
  IAsyncOperation = interface(IUnknown)
    [SID_IAsyncOperation]
    function SetAsyncMode(fDoOpAsync: BOOL): HResult; stdcall;
    function GetAsyncMode(out pfIsOpAsync: BOOL): HResult; stdcall;
    function StartOperation(const pbcReserved: IBindCtx): HResult; stdcall;
    function InOperation(out pfInAsyncOp: BOOL): HResult; stdcall;
    function EndOperation(hResult: HRESULT; const pbcReserved: IBindCtx;
      dwEffects: DWORD): HResult; stdcall;
  end;

  IAsyncOperation2 = interface(IAsyncOperation)
    [SID_IAsyncOperation]
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TRawClipboardFormat & TRawDataFormat
//
////////////////////////////////////////////////////////////////////////////////
// These clipboard and data format classes are special in that they don't
// interpret the data in any way.
// Their primary purpose is to enable the TCustomDropMultiSource class to accept
// and store arbitrary (and unknown) data types. This is a requirement for
// drag drop helper object support.
////////////////////////////////////////////////////////////////////////////////
// The TRawDataFormat class does not perform any storage of data itself. Instead
// it relies on the TRawClipboardFormat objects to store data.
////////////////////////////////////////////////////////////////////////////////
  TRawDataFormat = class(TCustomDataFormat)
  private
  protected
  public
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
  end;

  // TODO : Check TRawClipboardFormat for leak of medium
  TRawClipboardFormat = class(TClipboardFormat)
  private
    FMedium: TStgMedium;
  protected
    function DoGetData(const ADataObject: IDataObject;
      const AMedium: TStgMedium): boolean; override;
    function DoSetData(const FormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; override;
    procedure SetClipboardFormatName(const Value: string); override;
    function GetClipboardFormat: TClipFormat; override;
    function GetString: AnsiString;
    procedure SetString(const Value: AnsiString);
  public
    constructor Create; override;
    destructor Destroy; override;
    constructor CreateFormatEtc(const AFormatEtc: TFormatEtc); override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    procedure Clear; override;
    // Methods to handle the corresponding TRawDataFormat functionality.
    procedure ClearData;
    function HasData: boolean; override;
    function NeedsData: boolean;

    // All of these should be moved/mirrored in TRawDataFormat:
    function CopyFromStgMedium(const AMedium: TStgMedium): boolean;
    function CopyToStgMedium(var AMedium: TStgMedium): boolean;
    property Medium: TStgMedium read FMedium write FMedium;
    // Debug info:
    property AsString: AnsiString read GetString write SetString;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
function DropEffectToDragType(DropEffect: longInt; var DragType: TDragType): boolean;
function DragTypesToDropEffect(DragTypes: TDragTypes): longint; // V4: New

// Coordinate space conversion.
function ClientPtToWindowPt(Handle: THandle; pt: TPoint): TPoint;

// Replacement for KeysToShiftState.
function KeysToShiftStatePlus(Keys: Word): TShiftState; // V4: New
function ShiftStateToDropEffect(Shift: TShiftState; AllowedEffects: longint;
  Fallback: boolean): longint;

// Replacement for the buggy DragDetect API function.
// Differences from DragDetect:
// - Does not eat mouse messages. I.e. leaves mouse-down and mouse-up messages
//   in the message queue.
// - Works with both left and right mouse button.
function DragDetectPlus(Handle: THandle; Position: TPoint): boolean;  overload;
function DragDetectPlus(Handle: THandle): boolean; overload;
function DragDetectPlus(Control: TWinControl; Position: TPoint): boolean;  overload;
function DragDetectPlus(Control: TWinControl): boolean;  overload;


// Wrapper for urlmon.CopyStgMedium.
// Note: Only works with IE4 or later installed.
function CopyStgMedium(const SrcMedium: TStgMedium; var DstMedium: TStgMedium): boolean;

// Get the name of a clipboard format as a Delphi string.
function GetClipboardFormatNameStr(Value: TClipFormat): string;

// Raise last Windows API error as an exception.
procedure _RaiseLastWin32Error;

////////////////////////////////////////////////////////////////////////////////
//
//              Global variables
//
////////////////////////////////////////////////////////////////////////////////
var
  ShellMalloc: IMalloc;

// Name of the IDE component palette page the drag drop components are
// registered to
var
  DragDropComponentPalettePage: string = 'DragDrop';

////////////////////////////////////////////////////////////////////////////////
//
//              Misc drop target related configuration
//
////////////////////////////////////////////////////////////////////////////////
var
  // Default inset-width of the auto-scroll hot zone.
  // Specified in pixels.
  // Not used! Instead the height/width of the target scroll bar is used.
  DragDropScrollInset: integer = 11;//DD_DEFSCROLLINSET; // 11

  // Default delay after entering the scroll zone, before auto-scrolling starts.
  // Specified in milliseconds.
  DragDropScrollDelay: integer = 50;//DD_DEFSCROLLDELAY; //  50

  // Default scroll interval during auto-scroll.
  // Specified in milliseconds.
  DragDropScrollInterval: integer = 50;//DD_DEFSCROLLINTERVAL; // 50

  // Maximum mouse velocity of auto-scroll.
  // Specified in pixels/seconds.
  DragDropScrollMaxVelocity: integer = 20;

  // Sample rate for cursor velocity calculation used in auto-scroll.
  // Should be <= DragDropScrollInterval. Larger values can have side effects.
  // Set to 0 to sample as fast as possible.
  // Specified in milliseconds.
  DragDropScrollVelocitySample: integer = 10;

  // Default delay before dragging should start.
  // Used by DragDetectPlus.
  // Specified in milliseconds.
  DragDropDragDelay: integer = 200;// DD_DEFDRAGDELAY; // 200

  // Default max period with no mouse or key input before dragging starts auto-
  // matically. This feature is only enabled if DragDropAutoDrag is True.
  // Used by DragDetectPlus.
  // The default value of 500mS is hardcoded inside the DragDetect API function.
  // Specified in milliseconds.
  DragDropDragDelayMax: integer = 500;
  DragDropAutoDrag: boolean = True;

  // Default minimum distance (radius) before dragging should start.
  // Specified in pixels.
  // Deprecated! The SM_CXDRAG and SM_CYDRAG system metrics are used instead.
  DragDropDragMinDistance: integer = 2;//DD_DEFDRAGMINDIST; // 2


////////////////////////////////////////////////////////////////////////////////
//
//              Shutdown handling
//
////////////////////////////////////////////////////////////////////////////////
var
  // DragDropShutdown is set to True in the finalization section of this unit
  // and is used to avoid a one-time memory leak.
  // Once DragDropShutdown is True, the TDataFormatClasses and TDataFormatMap
  // singleton objects have been freed and should no longer be used.
  DragDropShutdown: boolean = False;

////////////////////////////////////////////////////////////////////////////////
//
//              Misc drag drop API related constants
//
////////////////////////////////////////////////////////////////////////////////

// The following DVASPECT constants are missing from some versions of Delphi and
// C++Builder.
{$ifndef VER135_PLUS}
const
{$ifndef VER10_PLUS}
  DVASPECT_SHORTNAME = 2; // use for CF_HDROP to get short name version of file paths
{$endif}
  DVASPECT_COPY = 3; // use to indicate format is a "Copy" of the data (FILECONTENTS, FILEDESCRIPTOR, etc)
  DVASPECT_LINK = 4; // use to indicate format is a "Shortcut" to the data (FILECONTENTS, FILEDESCRIPTOR, etc)
{$endif}


////////////////////////////////////////////////////////////////////////////////
//
//              Wide string functions from WideStrUtils unit
//
////////////////////////////////////////////////////////////////////////////////
// WideStrUtils is included with D2006 and later.
// We could also have wrapped the StrCpyNW API
////////////////////////////////////////////////////////////////////////////////
function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;

////////////////////////////////////////////////////////////////////////////////
//
//              Tiburón
//
////////////////////////////////////////////////////////////////////////////////

const
  CP_THREAD_ACP = 3; // current thread's ANSI code page
  DefaultSystemCodePage = CP_THREAD_ACP;

const
  // AnsiString makes the linker include the string even if it isn't referenced
  sDragDropCopyright: AnsiString = 'Drag and Drop Component Suite. Copyright © 1997-2010 Anders Melander';


(*******************************************************************************
**
**			IMPLEMENTATION
**
*******************************************************************************)
implementation

uses
  DragDropFormats, // Used by TRawClipboardFormat
  DropSource,
  DropTarget,
  Messages,
  ShlObj,
  MMSystem,
  SysUtils;

resourcestring
  sImplementationRequired = 'Internal error: %s.%s needs implementation';
  sFormatNameReadOnly = '%s.ClipboardFormat is read-only';
  sNoCopyStgMedium = 'A required system function (URLMON.CopyStgMedium) was not available on this system. Operation aborted.';
  sBadConstructor = 'The %s class can not be instantiated with the default constructor';
  sUnregisteredDataFormat = 'The %s data format has not been registered by any of the used units';


////////////////////////////////////////////////////////////////////////////////
//
//              Pre D2005 support
//
////////////////////////////////////////////////////////////////////////////////
{$ifndef VER17_PLUS}
procedure TBitmap.SetSize(NewWidth, NewHeight: integer);
begin
  Width := NewWidth;
  Height := NewHeight;
end;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              Tiburón
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              Wide string functions from WideStrUtils unit
//
////////////////////////////////////////////////////////////////////////////////
function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) and (MaxLen > 0) do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
    Dec(MaxLen);
  end;
  Dest^ := #$00;
end;

function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TInterfacedComponent
//
////////////////////////////////////////////////////////////////////////////////
function TInterfacedComponent.QueryInterface(constref IID: TGuid; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else if (VCLComObject <> nil) then
    Result := IVCLComObject(VCLComObject).QueryInterface(IID, Obj)
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedComponent._AddRef: Integer;
var
  Outer: IUnknown;
begin
  // In case we are the inner object of an aggregation, we attempt to delegate
  // the reference counting to the outer object. We assume that the component
  // owner is the outer object.
  if (Owner <> nil) and (Owner.GetInterface(IUnknown, Outer)) then
    Result := Outer._AddRef
  else
  begin
    inherited _AddRef;
    Result := -1;
  end;
end;

function TInterfacedComponent._Release: Integer;
var
  Outer: IUnknown;
begin
  // See _AddRef for comments.
  if (Owner <> nil) and (Owner.GetInterface(IUnknown, Outer)) then
    Result := Outer._Release
  else
  begin
    inherited _Release;
    Result := -1;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropComponent
//
////////////////////////////////////////////////////////////////////////////////
procedure TDragDropComponent.GetCompatibleClipboardFormats(const DataFormatClass: TDataFormatClass;
  ClipboardFormats: TClipboardFormats);
begin
  ClipboardFormats.Clear;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
destructor TClipboardFormat.Destroy;
begin
  // Warning: Do not call Clear here. Descendant class has already
  // cleaned up and released resources!
  inherited Destroy;
end;

constructor TClipboardFormat.CreateFormat(Atymed: Longint);
begin
  inherited Create;
  FFormatEtc.cfFormat := ClipboardFormat;
  FFormatEtc.ptd := nil;
  FFormatEtc.dwAspect := DVASPECT_CONTENT;
  FFormatEtc.lindex := -1;
  FFormatEtc.tymed := Atymed;
end;

constructor TClipboardFormat.CreateFormatEtc(const AFormatEtc: TFormatEtc);
begin
  inherited Create;
  FFormatEtc := AFormatEtc;
end;

function TClipboardFormat.GetValidMedia(const ADataObject: IDataObject): longInt;
var
  Mask: longInt;
  AFormatEtc: TFormatEtc;
begin
  // Because some IDataObject.QueryGetData implementations (e.g. Outlook
  // Express) can't handle multiple TYMED values specified in the same
  // FormatEtc, we have to try each in turn.
  Mask := $0000001;
  AFormatEtc := FormatEtc;
  Result := 0;
  while (Mask <> 0) and (Mask <= FormatEtc.tymed) do
  begin
    AFormatEtc.tymed := FormatEtc.tymed and Mask;
    if (AFormatEtc.tymed <> 0) then
      if (Succeeded(ADataObject.QueryGetData(AFormatEtc))) then
        Result := Result or Mask;
    Mask := Mask shl 1;
  end;
end;

function TClipboardFormat.HasValidFormats(const ADataObject: IDataObject): boolean;
var
  Mask: longInt;
  AFormatEtc: TFormatEtc;
begin
  // Because some IDataObject.QueryGetData implementations (e.g. Outlook
  // Express) can't handle multiple TYMED values specified in the same
  // FormatEtc, we have to try each in turn.
  Mask := $0000001;
  AFormatEtc := FormatEtc;
  Result := False;
  while (not Result) and (Mask <> 0) and (Mask <= FormatEtc.tymed) do
  begin
    AFormatEtc.tymed := FormatEtc.tymed and Mask;
    if (AFormatEtc.tymed <> 0) then
      Result := (Succeeded(ADataObject.QueryGetData(AFormatEtc)));
    Mask := Mask shl 1;
  end;
  // We could have used Result := (GetValidMedia(ADataObjecy) <> 0), but the
  // above is a bit more efficient.
end;

class procedure TClipboardFormat.RegisterCompatibleFormats;
begin
  // Override this class method in descendant classes to call
  // RegisterDataProvider and RegisterDataConsumer
end;

class procedure TClipboardFormat.RegisterDataConsumer(DataFormatClass: TDataFormatClass);
begin
  RegisterDataConversion(DataFormatClass, MaxInt, [ccWriteToDataFormat]);
end;

class procedure TClipboardFormat.RegisterDataProvider(
  DataFormatClass: TDataFormatClass; Priority: integer);
begin
  RegisterDataConversion(DataFormatClass, Priority, [ccReadFromDataFormat]);
end;

class procedure TClipboardFormat.RegisterDataConversion(DataFormatClass: TDataFormatClass;
  Priority: integer; Capabilities: TConversionCapabilities);
begin
  TDataFormatMap.Instance.RegisterFormatMap(DataFormatClass, Self, Priority, Capabilities);
end;

class procedure TClipboardFormat.RegisterFormat;
begin
  RegisterCompatibleFormats;
end;

function TClipboardFormat.AcceptFormat(const AFormatEtc: TFormatEtc): boolean;
begin
  Result := (AFormatEtc.cfFormat = FFormatEtc.cfFormat) and
    (AFormatEtc.ptd = nil) and
    (AFormatEtc.dwAspect = FFormatEtc.dwAspect) and
    (AFormatEtc.tymed AND FFormatEtc.tymed <> 0);
end;

function TClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  Result := False;
end;

function TClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  Result := False;
end;

function TClipboardFormat.DoGetData(const ADataObject: IDataObject; const AMedium: TStgMedium): boolean;
begin
  Result := False;
end;

function TClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
begin
  Result := False;
end;

function TClipboardFormat.GetData(const ADataObject: IDataObject): boolean;
var
  Medium: TStgMedium;
  AFormatEtc: TFormatEtc;
begin
  Clear;
  FillChar(Medium, SizeOf(Medium), 0);
  AFormatEtc := FFormatEtc;
  // Make sure we only ask for data on a media supported by the data object.
  // Apparently some data objects (e.g. Bluesoleil Shell Integration) fail if
  // we ask for more than they can deliver.
  AFormatEtc.tymed := GetValidMedia(ADataObject);

  if (Succeeded(ADataObject.GetData(AFormatEtc, Medium))) then
    Result := GetDataFromMedium(ADataObject, Medium)
  else
    Result := False;
end;

class function TClipboardFormat.DataDirection: TDataDirections;
begin
  Result := [ddRead];
end;

function TClipboardFormat.GetDataFromMedium(const ADataObject: IDataObject;
  var AMedium: TStgMedium): boolean;
begin
  Result := False;
  try
    Clear;
    if (AMedium.tymed and FFormatEtc.tymed <> 0) then
      Result := DoGetData(ADataObject, AMedium);
  finally
    ReleaseStgMedium(AMedium);
  end;
end;

function TClipboardFormat.SetDataToMedium(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
begin
  Result := False;

  FillChar(AMedium, SizeOf(AMedium), 0);

  if (FormatEtcIn.cfFormat <> FFormatEtc.cfFormat) or
    (FormatEtcIn.dwAspect <> FFormatEtc.dwAspect) or
    (FormatEtcIn.tymed and FFormatEtc.tymed = 0) then
    exit;

  // Call descendant to allocate medium and transfer data to it
  Result := DoSetData(FormatEtcIn, AMedium);
end;

function TClipboardFormat.SetData(const ADataObject: IDataObject;
  const FormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean;
begin
  // Transfer data to medium
  Result := SetDataToMedium(FormatEtcIn, AMedium);

  // Call IDataObject to set data
  if (Result) then
    Result := (Succeeded(ADataObject.SetData(FormatEtc, AMedium, True)));

  // If we didn't succeed in transfering ownership of the data medium to the
  // IDataObject, we must deallocate the medium ourselves.
  if (not Result) then
    ReleaseStgMedium(AMedium);
end;

class procedure TClipboardFormat.UnregisterClipboardFormat;
begin
  if (not DragDropShutdown) then
    TDataFormatMap.Instance.DeleteByClipboardFormat(Self);
end;

function TClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  // This should have been a virtual abstract class method, but that isn't supported by C++Builder.
  raise Exception.CreateFmt(sImplementationRequired, [ClassName, 'GetClipboardFormat']);
end;

procedure TClipboardFormat.SetClipboardFormat(Value: TClipFormat);
begin
  FFormatEtc.cfFormat := Value;
end;

function TClipboardFormat.GetClipboardFormatName: string;
var
  Len: integer;
begin
  SetLength(Result, 255); // 255 is just an artificial limit.
  Len := Windows.GetClipboardFormatName(GetClipboardFormat, PChar(Result), 255);
  SetLength(Result, Len);
end;

procedure TClipboardFormat.SetClipboardFormatName(const Value: string);
begin
  raise Exception.CreateFmt(sFormatNameReadOnly, [ClassName]);
end;

function TClipboardFormat.HasData: boolean;
begin
  // Descendant classes are not required to override this method, so by default
  // we just pretend that data is available. No harm is done by this.
  Result := True;
end;

procedure TClipboardFormat.SetFormatEtc(const Value: TFormatEtc);
begin
  FFormatEtc := Value;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TClipboardFormats
//
////////////////////////////////////////////////////////////////////////////////
constructor TClipboardFormats.Create(ADataFormat: TCustomDataFormat;
  AOwnsObjects: boolean);
begin
  inherited Create;
  FList := TList.Create;
  FDataFormat := ADataFormat;
  FOwnsObjects := AOwnsObjects;
end;

destructor TClipboardFormats.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TClipboardFormats.Add(ClipboardFormat: TClipboardFormat): integer;
begin
  Result := FList.Add(ClipboardFormat);
  if (FOwnsObjects) and (DataFormat <> nil) then
    ClipboardFormat.DataFormat := DataFormat;
end;

function TClipboardFormats.FindFormat(ClipboardFormatClass: TClipboardFormatClass): TClipboardFormat;
var
  i: integer;
begin
  // Search list for an object of the specified type
  for i := 0 to Count-1 do
    if (Formats[i].InheritsFrom(ClipboardFormatClass)) then
    begin
      Result := Formats[i];
      exit;
    end;
  Result := nil;
end;

function TClipboardFormats.GetCount: integer;
begin
  Result := FList.Count;
end;

function TClipboardFormats.GetFormat(ClipboardFormat: TClipFormat): TClipboardFormat;
begin
  Result := FindFormat(ClipboardFormat);
  if (Result = nil) then
    raise Exception.CreateFmt('Clipboard format %d does not exist in %s''s list of copatible formats',
      [ClipboardFormat, DataFormat.ClassName]);
end;

function TClipboardFormats.GetFormat(ClipboardFormatClass: TClipboardFormatClass): TClipboardFormat;
begin
  Result := FindFormat(ClipboardFormatClass);
  if (Result = nil) then
    raise Exception.CreateFmt('Clipboard format %s does not exist in %s''s list of copatible formats',
      [ClipboardFormatClass.ClassName, DataFormat.ClassName]);
end;

function TClipboardFormats.DoGetFormat(Index: integer): TClipboardFormat;
begin
  Result := TClipboardFormat(FList[Index]);
end;

function TClipboardFormats.FindFormat(ClipboardFormat: TClipFormat): TClipboardFormat;
var
  i: integer;
begin
  // Search list for an object of the specified type
  for i := 0 to Count-1 do
    if (Formats[i].ClipboardFormat = ClipboardFormat) then
    begin
      Result := Formats[i];
      exit;
    end;
  Result := nil;
end;

procedure TClipboardFormats.Assign(Source: TClipboardFormats);
var
  i: integer;
begin
  Clear;
  OwnsObjects := False;
  for i := 0 to Source.Count-1 do
    Add(Source[i]);
end;

procedure TClipboardFormats.Clear;
var
  i: integer;
  Format: TObject;
begin
  if (FOwnsObjects) then
    // Empty list and delete all objects in it
    for i := Count-1 downto 0 do
    begin
      Format := Formats[i];
      FList.Delete(i);
      Format.Free;
    end;

  FList.Clear;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomDataFormat.Create(Dummy: pointer);
begin
  Create(dfdConsumer);
end;

constructor TCustomDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create;
  FOwner := AOwner;

  if (Owner <> nil) then
    // Add object to owners list of data formats.
    Owner.DataFormats.Add(Self);
end;

constructor TCustomDataFormat.Create(Direction: TDataFormatDirection);
const
  DirectionToScope: array[TDataFormatDirection] of TDragDropTier =
    (ddtSource, ddtTarget);
begin
  Create(TDragDropComponent(nil));

  CreateCompatibleFormatsList;
  // Populate the compatible formats list
  TDataFormatMap.GetClipboardFormats(TDataFormatClass(Self.ClassType),
    FCompatibleFormats, DirectionToScope[Direction]);
end;

destructor TCustomDataFormat.Destroy;
begin
  FCompatibleFormats.Free;
  // Remove object from owners list of target formats
  if (Owner <> nil) then
    Owner.DataFormats.Remove(Self);
  inherited Destroy;
end;

function TCustomDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  // Called when derived class(es) couldn't convert from the source format.
  // Try to let source format convert to this format instead.
  Result := Source.AssignTo(Self);
end;

function TCustomDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  // Called when derived class(es) couldn't convert to the destination format.
  // Try to let destination format convert from this format instead.
  Result := Dest.Assign(Self);
end;

procedure TCustomDataFormat.CreateCompatibleFormatsList;
begin
  if (FCompatibleFormats = nil) then
    FCompatibleFormats := TClipboardFormats.Create(Self, True);
end;

function TCustomDataFormat.GetCompatibleFormats: TClipboardFormats;
begin
  if (FCompatibleFormats = nil) then
  begin
    // With no owner we can't determine if this object is to be used for
    // reading or writing - or both.
    ASSERT(Owner <> nil);

    // Create and populate list on demand
    CreateCompatibleFormatsList;

    // Populate list with all the clipboard formats that have been registered as
    // compatible with this data format.
    Owner.GetCompatibleClipboardFormats(TDataFormatClass(Self.ClassType), FCompatibleFormats);
  end;
  Result := FCompatibleFormats;
end;

function TCustomDataFormat.GetData(DataObject: IDataObject): boolean;
var
  i: integer;
begin
  Result := False;
  i := 0;
  // Get data from each of our associated clipboard formats until we don't
  // need anymore data.
  while (NeedsData) and (i < CompatibleFormats.Count) do
  begin
    CompatibleFormats[i].Clear;

    if (CompatibleFormats[i].GetData(DataObject)) then
      if (CompatibleFormats[i].HasData) then
      begin
        if (Assign(CompatibleFormats[i])) then
        begin
          // Once data has been sucessfully transfered to the TDataFormat object,
          // we clear the data in the TClipboardFormat object in order to conserve
          // resources.
          CompatibleFormats[i].Clear;
          Result := True;
        end;
      end;

    inc(i);
  end;
end;

function TCustomDataFormat.NeedsData: boolean;
begin
  Result := not HasData;
end;

function TCustomDataFormat.HasValidFormats(const ADataObject: IDataObject): boolean;
var
  i: integer;
begin
  // Determine if any of the registered clipboard formats can read from the
  // specified data object.
  Result := False;
  for i := 0 to CompatibleFormats.Count-1 do
    if (CompatibleFormats[i].HasValidFormats(ADataObject)) then
    begin
      Result := True;
      break;
    end;
end;

function TCustomDataFormat.AcceptFormat(const FormatEtc: TFormatEtc): boolean;
var
  i: integer;
begin
  // Determine if any of the registered clipboard formats can handle the
  // specified clipboard format.
  Result := False;
  for i := 0 to CompatibleFormats.Count-1 do
    if (CompatibleFormats[i].AcceptFormat(FormatEtc)) then
    begin
      Result := True;
      break;
    end;
end;

class procedure TCustomDataFormat.RegisterDataFormat;
begin
  if (not DragDropShutdown) then
  begin
    TDataFormatClasses.Add(Self);
    RegisterCompatibleFormats;
  end;
end;

class procedure TCustomDataFormat.RegisterDataProvider(ClipboardFormatClass: TClipboardFormatClass; Priority: integer);
begin
  RegisterDataConversion(ClipboardFormatClass, Priority, [ccWriteToDataFormat]);
end;

class procedure TCustomDataFormat.RegisterDataConversion(ClipboardFormatClass: TClipboardFormatClass;
  Priority: integer; Capabilities: TConversionCapabilities);
begin
  // Register format mapping.
  if (not DragDropShutdown) then
    TDataFormatMap.Instance.RegisterFormatMap(Self, ClipboardFormatClass,
      Priority, Capabilities);
end;

class procedure TCustomDataFormat.RegisterDataConsumer(ClipboardFormatClass: TClipboardFormatClass);
begin
  RegisterDataConversion(ClipboardFormatClass, MaxInt, [ccReadFromDataFormat]);
end;

class procedure TCustomDataFormat.RegisterCompatibleFormats;
begin
  // Override this class method in descendant classes to call
  // RegisterDataProvider and RegisterDataConsumer
end;

function TCustomDataFormat.SupportsFormat(ClipboardFormat: TClipboardFormat): boolean;
begin
  Result := (CompatibleFormats.FindFormat(ClipboardFormat.ClipboardFormat) <> nil);
end;

class procedure TCustomDataFormat.UnregisterDataFormat;
begin
  if (not DragDropShutdown) then
  begin
    TDataFormatMap.Instance.DeleteByDataFormat(Self);
    TDataFormatClasses.Remove(Self);
  end;
end;

procedure TCustomDataFormat.DoOnChanging(Sender: TObject);
begin
  Changing;
end;

procedure TCustomDataFormat.Changing;
begin
  if (Assigned(OnChanging)) then
    OnChanging(Self);
end;



////////////////////////////////////////////////////////////////////////////////
//
//              TDataFormats
//
////////////////////////////////////////////////////////////////////////////////
function TDataFormats.Add(DataFormat: TCustomDataFormat): integer;
begin
  Result := FList.IndexOf(DataFormat);
  if (Result = -1) then
    Result := FList.Add(DataFormat);
end;

constructor TDataFormats.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDataFormats.Destroy;
var
  i: integer;
begin
  for i := FList.Count-1 downto 0 do
    Remove(TCustomDataFormat(FList[i]));
  FList.Free;
  inherited Destroy;
end;

function TDataFormats.GetCount: integer;
begin
  Result := FList.Count;
end;

function TDataFormats.GetFormat(Index: integer): TCustomDataFormat;
begin
  Result := TCustomDataFormat(FList[Index]);
end;

function TDataFormats.IndexOf(DataFormat: TCustomDataFormat): integer;
begin
  Result := FList.IndexOf(DataFormat);
end;

procedure TDataFormats.Remove(DataFormat: TCustomDataFormat);
begin
  FList.Remove(DataFormat);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDataFormatClasses
//
////////////////////////////////////////////////////////////////////////////////
var
  FDataFormatClasses: TDataFormatClasses = nil;

class function TDataFormatClasses.Add(DataFormat: TDataFormatClass): integer;
begin
  Result := Instance.FList.IndexOf(DataFormat);
  if (Result = -1) then
    Result := Instance.FList.Add(DataFormat);
end;

constructor TDataFormatClasses.Create;
begin
  ASSERT(FDataFormatClasses = nil);
  inherited Create;
  FList := TList.Create;
end;

destructor TDataFormatClasses.Destroy;
var
  i: integer;
begin
  ASSERT(FDataFormatClasses = self);
  for i := FList.Count-1 downto 0 do
    Remove(TDataFormatClass(FList[i]));
  FList.Free;
  inherited Destroy;
  FDataFormatClasses := nil;
end;

class function TDataFormatClasses.GetCount: integer;
begin
  Result := Instance.FList.Count;
end;

class function TDataFormatClasses.GetFormat(Index: integer): TDataFormatClass;
begin
  Result := TDataFormatClass(Instance.FList[Index]);
end;

class function TDataFormatClasses.Instance: TDataFormatClasses;
begin
  if (FDataFormatClasses = nil) then
  begin
    if (DragDropShutdown) then
      OutputDebugString('DragDrop.TDataFormatClasses recreated during shutdown');
    FDataFormatClasses := TDataFormatClasses.Create;
  end;
  Result := FDataFormatClasses;
end;

class procedure TDataFormatClasses.Remove(DataFormat: TDataFormatClass);
begin
  Instance.FList.Remove(DataFormat);
end;

{$ifndef VER17_PLUS}
class function TDataFormatClasses.Count: integer;
begin
  Result := TDataFormatClasses.GetCount;
end;

class function TDataFormatClasses.Formats: TDataFormatClassListProxy;
begin
  Result := TDataFormatClassListProxy(Self); // Dirty hack!
end;

class function TDataFormatClassListProxy.GetItem(Index: integer): TDataFormatClass;
begin
  Result := TDataFormatClasses(Self).GetFormat(Index); // Dirty hack!
end;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              TDataFormatMap
//
////////////////////////////////////////////////////////////////////////////////
type
  // TTargetFormat / TClipboardFormat association
  TFormatMap = record
    DataFormat: TDataFormatClass;
    ClipboardFormat: TClipboardFormatClass;
    Priority: integer;
    Capabilities: TConversionCapabilities;
  end;

  PFormatMap = ^TFormatMap;

var
  FDataFormatMap: TDataFormatMap = nil;

constructor TDataFormatMap.Create;
begin
  ASSERT(FDataFormatMap = nil);
  inherited Create;
  FList := TList.Create;
end;

destructor TDataFormatMap.Destroy;
var
  i: integer;
begin
  ASSERT(FDataFormatMap = self);
  // Zap any mapings which hasn't been unregistered
  // yet (actually an error condition)
  for i := FList.Count-1 downto 0 do
    Dispose(FList[i]);
  FList.Free;
  inherited Destroy;
  FDataFormatMap := nil;
end;

procedure TDataFormatMap.Sort;
var
  i: integer;
  NewMap: PFormatMap;
begin
  // Note: We do not use the built-in Sort method of TList because
  // we need to preserve the order in which the mappings were added.
  // New mappings have higher precedence than old mappings (within the
  // same priority).

  // Preconditions:
  // 1) The list is already sorted before a new mapping is added.
  // 2) The new mapping is always added to the end of the list.

  NewMap := PFormatMap(FList.Last);

  // Scan the list for a map with the same TTargetFormat type
  i := FList.Count-2;
  while (i > 0) do
  begin
    if (PFormatMap(FList[i])^.DataFormat = NewMap^.DataFormat) then
    begin
      // Scan the list for a map with lower priority
      repeat
        if (PFormatMap(FList[i])^.Priority < NewMap^.Priority) then
        begin
          // Move the mapping to the new position
          FList.Move(FList.Count-1, i+1);
          exit;
        end;
        dec(i);
      until (i < 0) or (PFormatMap(FList[i])^.DataFormat <> NewMap^.DataFormat);
      // Move the mapping to the new position
      FList.Move(FList.Count-1, i+1);
      exit;
    end;
    dec(i);
  end;
end;

procedure TDataFormatMap.Add(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass; Priority: integer;
  Capabilities: TConversionCapabilities);
var
  FormatMap: PFormatMap;
  OldMap: integer;
begin
  // Avoid duplicate mappings
  OldMap := FindMap(DataFormatClass, ClipboardFormatClass);
  if (OldMap = -1) then
  begin
    // Add new mapping...
    New(FormatMap);
    FList.Add(FormatMap);
    FormatMap^.ClipboardFormat := ClipboardFormatClass;
    FormatMap^.DataFormat := DataFormatClass;
    FormatMap^.Capabilities := Capabilities;
    FormatMap^.Priority := 0;
  end else
  begin
    // Replace old mapping...
    FormatMap := FList[OldMap];
    FList.Move(OldMap, FList.Count-1);
    FormatMap^.Capabilities := FormatMap^.Capabilities + Capabilities;
  end;

  if (ccReadFromDataFormat in Capabilities) then
    FormatMap^.Priority := Priority;
  // ...and sort list
  Sort;
end;

function TDataFormatMap.CanMap(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass): boolean;
begin
  Result := (FindMap(DataFormatClass, ClipboardFormatClass) <> -1);
end;

procedure TDataFormatMap.Delete(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass);
var
  Index: integer;
begin
  Index := FindMap(DataFormatClass, ClipboardFormatClass);
  if (Index <> -1) then
  begin
    Dispose(FList[Index]);
    FList.Delete(Index);
  end;
end;

procedure TDataFormatMap.DeleteByClipboardFormat(ClipboardFormatClass: TClipboardFormatClass);
var
  i: integer;
begin
  // Delete all mappings associated with the specified clipboard format
  for i := FList.Count-1 downto 0 do
    if (PFormatMap(FList[i])^.ClipboardFormat.InheritsFrom(ClipboardFormatClass)) then
    begin
      Dispose(FList[i]);
      FList.Delete(i);
    end;
end;

procedure TDataFormatMap.DeleteByDataFormat(DataFormatClass: TDataFormatClass);
var
  i: integer;
begin
  // Delete all mappings associated with the specified target format
  for i := FList.Count-1 downto 0 do
    if (PFormatMap(FList[i])^.DataFormat.InheritsFrom(DataFormatClass)) then
    begin
      Dispose(FList[i]);
      FList.Delete(i);
    end;
end;

function TDataFormatMap.FindMap(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass): integer;
var
  i: integer;
begin
  for i := 0 to FList.Count-1 do
    if (PFormatMap(FList[i])^.DataFormat.InheritsFrom(DataFormatClass)) and
      (PFormatMap(FList[i])^.ClipboardFormat.InheritsFrom(ClipboardFormatClass)) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

class procedure TDataFormatMap.GetClipboardFormats(const DataFormatClass: TDataFormatClass;
  ClipboardFormats: TClipboardFormats; Tier: TDragDropTier);
var
  i: integer;
  ClipboardFormat: TClipboardFormat;
  FormatMap: PFormatMap;
begin
  // Clear the list...
  ClipboardFormats.Clear;
  // ...and populate it with *instances* of all the clipbard
  // formats associated with the specified target format and
  // registered with the specified data direction.
  for i := 0 to Instance.FList.Count-1 do
  begin
    FormatMap := PFormatMap(Instance.FList[i]);
    if (FormatMap.DataFormat.InheritsFrom(DataFormatClass)) and
      (((Tier = ddtSource) and ((ccReadFromDataFormat in FormatMap.Capabilities) or (ddWrite in FormatMap.ClipboardFormat.DataDirection))) or
       ((Tier = ddtTarget) and ((ccWriteToDataFormat in FormatMap.Capabilities) or (ddRead in FormatMap.ClipboardFormat.DataDirection)))) then
    begin
      ClipboardFormat := FormatMap.ClipboardFormat.Create;
      ClipboardFormats.Add(ClipboardFormat);
    end;
  end;
end;

procedure TDataFormatMap.RegisterFormatMap(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass; Priority: integer;
  Capabilities: TConversionCapabilities);
begin
  Add(DataFormatClass, ClipboardFormatClass, Priority, Capabilities);
end;

procedure TDataFormatMap.UnregisterFormatMap(DataFormatClass: TDataFormatClass;
  ClipboardFormatClass: TClipboardFormatClass);
begin
  Delete(DataFormatClass, ClipboardFormatClass);
end;

class function TDataFormatMap.Instance: TDataFormatMap;
begin
  if (FDataFormatMap = nil) then
  begin
    if (DragDropShutdown) then
      OutputDebugString('DragDrop.TDataFormatMap recreated during shutdown');
    FDataFormatMap := TDataFormatMap.Create;
  end;
  Result := FDataFormatMap;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDataFormatAdapter
//
////////////////////////////////////////////////////////////////////////////////
constructor TDataFormatAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

destructor TDataFormatAdapter.Destroy;
begin
  FDataFormat.Free;
  FDataFormat := nil;
  inherited Destroy;
end;

function TDataFormatAdapter.GetDataFormatName: string;
begin
  if (csDesigning in ComponentState) then
    Result := FDataFormatClassName
  else
  if Assigned(FDataFormatClass) then
    Result := FDataFormatClass.ClassName
  else
    Result := '';
end;

function TDataFormatAdapter.GetEnabled: boolean;
begin
  if (csDesigning in ComponentState) then
    Result := FEnabled
  else
    Result := Assigned(FDataFormat) and Assigned(FDataFormatClass);
end;

procedure TDataFormatAdapter.Loaded;
begin
  inherited;
  if (FEnabled) then
    Enabled := True;
end;

procedure TDataFormatAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDragDropComponent) then
    DragDropComponent := nil;
  inherited;
end;

procedure TDataFormatAdapter.SetDataFormatClass(const Value: TDataFormatClass);
begin
  if (Value <> FDataFormatClass) then
  begin
    if not(csLoading in ComponentState) then
      Enabled := False;
    FDataFormatClass := Value;
  end;
end;

procedure TDataFormatAdapter.SetDataFormatName(const Value: string);
var
  i: integer;
  ADataFormatClass: TDataFormatClass;
begin
  if (csDesigning in ComponentState) then
    FDataFormatClassName := Value
  else
  begin
    ADataFormatClass := nil;
    if (Value <> '') then
    begin
      for i := 0 to TDataFormatClasses.Count-1 do
        if (AnsiCompareText(TDataFormatClasses.Formats[i].ClassName, Value) = 0) then
        begin
          ADataFormatClass := TDataFormatClasses.Formats[i];
          break;
        end;
      if (ADataFormatClass = nil) then
        raise Exception.CreateFmt(sUnregisteredDataFormat, [Value]);
    end;
    DataFormatClass := ADataFormatClass;
  end;
end;

procedure TDataFormatAdapter.SetDragDropComponent(const Value: TDragDropComponent);
begin
  if (Value <> NIL) and (Value <> FDragDropComponent) then
  begin
    if not(csLoading in ComponentState) then
      Enabled := False;
    if (FDragDropComponent <> nil) then
      FDragDropComponent.RemoveFreeNotification(Self);
    FDragDropComponent := Value;
    if (Value <> nil) then
      Value.FreeNotification(Self);
  end;
end;

procedure TDataFormatAdapter.SetEnabled(const Value: boolean);
begin
  if (csLoading in ComponentState) then
  begin
    FEnabled := Value;
  end else
  if (csDesigning in ComponentState) then
  begin
    FEnabled := Value and Assigned(FDragDropComponent) and
      (FDataFormatClassName <> '');
  end else
  if (Value) then
  begin
    if (Assigned(FDragDropComponent)) and (Assigned(FDataFormatClass)) and
      (not Assigned(FDataFormat)) then
      FDataFormat := FDataFormatClass.Create(FDragDropComponent);
  end else
  begin
    if Assigned(FDataFormat) then
    begin
      if Assigned(FDragDropComponent) and
        (FDragDropComponent.DataFormats.IndexOf(FDataFormat) <> -1) then
        FDataFormat.Free;
      FDataFormat := nil;
    end;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TRawClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TRawClipboardFormat.Create;
begin
  // Yeah, it's a hack but blame Borland for making TObject.Create public!
  raise Exception.CreateFmt(sBadConstructor, [ClassName]);
end;

constructor TRawClipboardFormat.CreateFormatEtc(const AFormatEtc: TFormatEtc);
begin
  inherited CreateFormatEtc(AFormatEtc);
end;

procedure TRawClipboardFormat.SetClipboardFormatName(const Value: string);
begin
  ClipboardFormat := RegisterClipboardFormat(PChar(Value));
end;

function TRawClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := FFormatEtc.cfFormat;
end;

function TRawClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TRawDataFormat) then
  begin
    Result := True;
  end else
    Result := inherited Assign(Source);
end;

function TRawClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TRawDataFormat) then
  begin
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;

procedure TRawClipboardFormat.Clear;
begin
  // Since TRawClipboardFormat performs storage for TRawDataFormat we only allow
  // TRawDataFormat to clear. To accomplish this TRawClipboardFormat ignores
  // calls to the clear method and instead introduces the ClearData method.

  // Since Clear() doesn't do anything the class must make sure that ClearData()
  // is called before FMedium is modified. Otherwise we will leak. 
end;

procedure TRawClipboardFormat.ClearData;
begin
  ReleaseStgMedium(FMedium);
  FillChar(FMedium, SizeOf(FMedium), 0);
end;

function TRawClipboardFormat.HasData: boolean;
begin
  Result := (FMedium.tymed <> TYMED_NULL);
end;

function TRawClipboardFormat.NeedsData: boolean;
begin
  Result := (FMedium.tymed = TYMED_NULL);
end;

function TRawClipboardFormat.CopyFromStgMedium(const AMedium: TStgMedium): boolean;
begin
  // Release old data before new is set.
  ClearData;
  Result := CopyStgMedium(AMedium, FMedium);
end;

function TRawClipboardFormat.CopyToStgMedium(var AMedium: TStgMedium): boolean;
begin
  Result := CopyStgMedium(FMedium, AMedium);
end;

destructor TRawClipboardFormat.Destroy;
begin
  ClearData;
  inherited Destroy;
end;

function TRawClipboardFormat.DoGetData(const ADataObject: IDataObject;
  const AMedium: TStgMedium): boolean;
begin
  Result := CopyFromStgMedium(AMedium);

  // Quote from .\Samples\winui\Shell\DragImg\DragImg.doc from the
  // Platform SDK:
  // Currently, there is a bug in that the Drag Source Helper assumes the
  // position of the current pointer in an IStream retrieved from
  // IDataObject::GetData is at the beginning of the stream. Until NTBUG #242463
  // is resolved, it is necessary for the data object to set the IStream's seek
  // pointer to the beginning of the stream.

  // This doesn't seem to be necessary on any version of Win2K, but if they
  // say it must be done then I'll better do it...
  if (Result) and (FMedium.tymed = TYMED_ISTREAM) then
    IStream(FMedium.pstm).Seek(0, STREAM_SEEK_SET,qword( PLargeint(nil)^));
end;

function TRawClipboardFormat.DoSetData(const FormatEtcIn: TFormatEtc;
  var AMedium: TStgMedium): boolean;
begin
  Result := CopyToStgMedium(AMedium);
end;

function TRawClipboardFormat.GetString: AnsiString;
begin
  with TAnsiStringClipboardFormat.Create do
    try
      FFormatEtc := Self.FFormatEtc;
      if GetDataFromMedium(nil, FMedium) then
        Result := Data
      else
        Result := '';
    finally
      Free;
    end;
end;

procedure TRawClipboardFormat.SetString(const Value: AnsiString);
begin
  ClearData;

  with TAnsiStringClipboardFormat.Create do
    try
      FFormatEtc := Self.FFormatEtc;
      Data := Value;
      SetDataToMedium(FormatEtc, FMedium);
    finally
      Free;
    end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TRawDataFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TRawDataFormat.Clear;
var
 i: integer;
begin
  Changing;
  for i := 0 to CompatibleFormats.Count-1 do
    TRawClipboardFormat(CompatibleFormats[i]).ClearData;
end;

function TRawDataFormat.HasData: boolean;
var
 i: integer;
begin
  i := 0;
  Result := False;
  while (not Result) and (i < CompatibleFormats.Count) do
  begin
    Result := TRawClipboardFormat(CompatibleFormats[i]).HasData;
    inc(i);
  end;
end;

function TRawDataFormat.NeedsData: boolean;
var
 i: integer;
begin
  i := 0;
  Result := False;
  while (not Result) and (i < CompatibleFormats.Count) do
  begin
    Result := TRawClipboardFormat(CompatibleFormats[i]).NeedsData;
    inc(i);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
procedure _RaiseLastWin32Error;
begin
  RaiseLastOSError;
end;

function DropEffectToDragType(DropEffect: longInt; var DragType: TDragType): boolean;
begin
  Result := True;
  if ((DropEffect and DROPEFFECT_COPY) <> 0) then
    DragType := dtCopy
  else
    if ((DropEffect and DROPEFFECT_MOVE) <> 0) then
      DragType := dtMove
    else
      if ((DropEffect and DROPEFFECT_LINK) <> 0) then
        DragType := dtLink
      else
      begin
        DragType := dtCopy;
        Result := False;
      end;
end;

function DragTypesToDropEffect(DragTypes: TDragTypes): longint;
begin
  Result := DROPEFFECT_NONE;
  if (dtCopy in DragTypes) then
    Result := Result OR DROPEFFECT_COPY;
  if (dtMove in DragTypes) then
    Result := Result OR DROPEFFECT_MOVE;
  if (dtLink in DragTypes) then
    Result := Result OR DROPEFFECT_LINK;
end;

function DragDetectPlus(Control: TWinControl): boolean; // V4: New
var
  Position: TPoint;
begin
  GetCursorPos(Position);
  Result := DragDetectPlus(Control, Position);
end;

function DragDetectPlus(Control: TWinControl; Position: TPoint): boolean;
begin
  Result := DragDetectPlus(Control.Handle, Position);
end;

function DragDetectPlus(Handle: THandle): boolean;
var
  Position: TPoint;
begin
  GetCursorPos(Position);
  Result := DragDetectPlus(Handle, Position);
end;

{.$define DEBUG_DRAGDETECT}
function DragDetectPlus(Handle: THandle; Position: TPoint): boolean;
var
  DragRect: TRect;
  Msg: TMsg;
  StartTime: DWORD;
const
  PM_QS_INPUT = QS_INPUT shl 16;
  PM_QS_KEY = QS_KEY shl 16;
  PM_QS_MOUSEMOVE = QS_MOUSEMOVE shl 16;
  PM_QS_MOUSEBUTTON = QS_MOUSEBUTTON shl 16;
begin
  Result := False;

  // Attempt to detect when the position is passed in client (window) coordinates.
  // If we believe the position is in client coordinates, we translate them to
  // screen coordinates.
  // This is nescessary since previous versions of this function incorrectly
  // required the position to be in client coordinates.
  GetWindowRect(Handle, DragRect);
  // Is position outside screen coordinates...
  if (not PtInRect(DragRect, Position)) then
  begin
    GetClientRect(Handle, DragRect);
    // ... and inside client coordinates
    if (PtInRect(DragRect, Position)) then
      if (not ClientToScreen(Handle, Position)) then
        exit;
  end;

  // Check mouse state, and punt if the mouse isn't down.
{$ifndef DEBUG_DRAGDETECT}
  if ((GetKeyState(VK_LBUTTON) AND $8000) = 0) and
    ((GetKeyState(VK_RBUTTON) AND $8000) = 0) then
    exit;
{$endif DEBUG_DRAGDETECT}

  // Calculate the drag rect.
  // If the mouse leaves this rect, while the mouse button is pressed, a drag is
  // detected.
  DragRect.TopLeft := Position;
  DragRect.BottomRight := Position;
  InflateRect(DragRect, GetSystemMetrics(SM_CXDRAG), GetSystemMetrics(SM_CYDRAG));

  StartTime := TimeGetTime;

  // Capture the mouse so that we will receive mouse messages even after the
  // mouse leaves the control rect.
  SetCapture(Handle);
  try
    // Abort if we failed to capture the mouse.
    if (GetCapture <> Handle) then
      exit;

    while (not Result) do
    begin
      // Wait for mouse or keyboard events.
      // - but do not eat mouse button messages (so we don't break popup menus etc).
      if (PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONUP, PM_NOREMOVE)) or
        (PeekMessage(Msg, 0, WM_RBUTTONDOWN, WM_RBUTTONUP, PM_NOREMOVE)) or
        (PeekMessage(Msg, 0, WM_MBUTTONDOWN, WM_MBUTTONUP, PM_NOREMOVE)) then
      begin
        // Mouse button was changed - bail out.
        exit;
      end;

      while (not PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONUP, PM_NOREMOVE)) and
        (not PeekMessage(Msg, 0, WM_RBUTTONDOWN, WM_RBUTTONUP, PM_NOREMOVE)) and
        (not PeekMessage(Msg, 0, WM_MBUTTONDOWN, WM_MBUTTONUP, PM_NOREMOVE)) and
       (not PeekMessage(Msg, 0, 0, 0, PM_REMOVE or PM_QS_KEY or PM_QS_MOUSEMOVE)) and
        (GetCapture = Handle) do
      begin
        // If there are no events for 500mS start drag without further ado.
        if (MsgWaitForMultipleObjects(0, nil^, False, DragDropDragDelayMax,
          QS_INPUT) = WAIT_TIMEOUT) then
          if (DragDropAutoDrag) then
          begin
            Result := True;
            exit;
          end;
      end;

      // Bail out if someone else has captured the mouse.
      if (GetCapture <> Handle) then
        break;

      case (Msg.message) of
        // Mouse was moved.
        WM_MOUSEMOVE:
          // Start drag if mouse has moved outside the drag rect and the minimum
          // time has elapsed.
          // Note that we ignore time warp (wrap around) and that Msg.Time
          // might be smaller than StartTime.
          Result := (not PtInRect(DragRect, Msg.pt)) and
            (Msg.time >= StartTime + DWORD(DragDropDragDelay));

        // [Esc] cancels drag detection.
        WM_KEYDOWN:
          if (Msg.wParam = VK_ESCAPE) then
            break;

        // Some operation cancelled our mouse capture.
        WM_CANCELMODE:
          break;

        // Application is shutting down.
        WM_QUIT:
          begin
            // Put quit message back in queue and abort.
            PostQuitMessage(Msg.wParam);
            exit;
          end;
      end;
    end;
  finally
    // Release capture - if we still have it
    if (GetCapture = Handle) then
      ReleaseCapture;
  end;
end;

function ClientPtToWindowPt(Handle: THandle; pt: TPoint): TPoint;
var
  Rect: TRect;
begin
  ClientToScreen(Handle, pt);
  GetWindowRect(Handle, Rect);
  Result.X := pt.X - Rect.Left;
  Result.Y := pt.Y - Rect.Top;
end;

const
  // Note: The definition of MK_ALT is missing from the current Delphi (D2007)
  // declarations.
  MK_ALT = $20;

function KeysToShiftStatePlus(Keys: Word): TShiftState;
begin
  Result := [];
  if (Keys and MK_SHIFT <> 0) then
    Include(Result, ssShift);
  if (Keys and MK_CONTROL <> 0) then
    Include(Result, ssCtrl);
  if (Keys and MK_LBUTTON <> 0) then
    Include(Result, ssLeft);
  if (Keys and MK_RBUTTON <> 0) then
    Include(Result, ssRight);
  if (Keys and MK_MBUTTON <> 0) then
    Include(Result, ssMiddle);
  if (Keys and MK_ALT <> 0) then
    Include(Result, ssAlt);
end;

function ShiftStateToDropEffect(Shift: TShiftState; AllowedEffects: longint;
  Fallback: boolean): longint;
begin
  // As we're only interested in ssShift & ssCtrl here,
  // mouse button states are screened out.
  Shift := Shift * [ssShift, ssCtrl];

  Result := DROPEFFECT_NONE;
  if (Shift = [ssShift, ssCtrl]) then
  begin
    if (AllowedEffects AND DROPEFFECT_LINK <> 0) then
      Result := DROPEFFECT_LINK;
  end else
  if (Shift = [ssCtrl]) then
  begin
    if (AllowedEffects AND DROPEFFECT_COPY <> 0) then
      Result := DROPEFFECT_COPY;
  end else
  begin
    if (AllowedEffects AND DROPEFFECT_MOVE <> 0) then
      Result := DROPEFFECT_MOVE;
  end;

  // Fall back to defaults if the shift-states specified an
  // unavailable drop effect.
  if (Result = DROPEFFECT_NONE) and (Fallback) then
  begin
    if (AllowedEffects AND DROPEFFECT_COPY <> 0) then
      Result := DROPEFFECT_COPY
    else if (AllowedEffects AND DROPEFFECT_MOVE <> 0) then
      Result := DROPEFFECT_MOVE
    else if (AllowedEffects AND DROPEFFECT_LINK <> 0) then
      Result := DROPEFFECT_LINK;
  end;
end;

var
  URLMONDLL: THandle = 0;
  _CopyStgMedium: function(const cstgmedSrc: TStgMedium; var stgmedDest: TStgMedium): HResult; stdcall = nil;

function CopyStgMedium(const SrcMedium: TStgMedium; var DstMedium: TStgMedium): boolean;
begin
  // Copy the medium via the URLMON CopyStgMedium function. This should be safe
  // since this function is only called when the drag drop helper object is
  // used and the drag drop helper object is only supported on Windows 2000
  // and later.
  // URLMON.CopyStgMedium requires IE4 or later.
  // An alternative approach would be to use OleDuplicateData, but based on a
  // disassembly of urlmon.dll, CopyStgMedium seems to do a lot more than
  // OleDuplicateData.
  if (URLMONDLL = 0) then
  begin
    URLMONDLL := LoadLibrary('URLMON.DLL');
    if (URLMONDLL <> 0) then
      @_CopyStgMedium := GetProcAddress(URLMONDLL, 'CopyStgMedium');
  end;

  if (@_CopyStgMedium = nil) then
    raise Exception.Create(sNoCopyStgMedium);

  Result := (Succeeded(_CopyStgMedium(SrcMedium, DstMedium)));
end;

function GetClipboardFormatNameStr(Value: TClipFormat): string;

  function GetNamedClipboardFormatName(Value: TClipFormat): string;
  var
    len: integer;
  begin
    SetLength(Result, 255);
    len := GetClipboardFormatName(Value, PChar(Result), 255);
    SetLength(Result, len);
  end;

const
  CF_TEXT_CF_MAX=16;
  sClipNames: array[0..CF_TEXT_CF_MAX-1] of string =
    ('CF_TEXT', 'CF_BITMAP', 'CF_METAFILEPICT', 'CF_SYLK', 'CF_DIF', 'CF_TIFF',
    'CF_OEMTEXT', 'CF_DIB', 'CF_PALETTE', 'CF_PENDATA', 'CF_RIFF', 'CF_WAVE',
    'CF_UNICODETEXT', 'CF_ENHMETAFILE', 'CF_HDROP', 'CF_LOCALE');
begin
  case Value of
    0..CF_TEXT_CF_MAX-1: Result := sClipNames[Value];
    17: Result := 'CF_DIBV5';
    18: Result := 'CF_MAX_XP';
    128: Result := 'CF_OWNERDISPLAY';
    129: Result := 'CF_DSPTEXT';
    130: Result := 'CF_DSPBITMAP';
    131: Result := 'CF_DSPMETAFILEPICT';
    142: Result := 'CF_DSPENHMETAFILE';
  else
    Result := GetNamedClipboardFormatName(Value);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              Initialization/Finalization
//
////////////////////////////////////////////////////////////////////////////////
var
  NeedOleUninitialize: boolean = False;

initialization
  NeedOleUninitialize := Succeeded(OleInitialize(nil));
  ShGetMalloc(ShellMalloc);
  GetClipboardFormatNameStr(0); // Avoid GetClipboardFormatNameStr getting eliminated by linker. This is so we can use it in debugger

finalization
  DragDropShutdown := True;
  // Note: Due to unit finalization order, is is possible for the following two
  // objects to be recreated after this units' finalization has executed.
  // If that happens it will result in a harmless one-time memory leak.
  if (FDataFormatMap <> nil) then
    FDataFormatMap.Free;
  if (FDataFormatClasses <> nil) then
    FDataFormatClasses.Free;

  ShellMalloc := nil;

  if (URLMONDLL <> 0) then
    FreeLibrary(URLMONDLL);

  if (NeedOleUninitialize) then
    OleUninitialize;
end.

