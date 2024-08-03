unit DropSource;

{$MODE Delphi}

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DropSource
// Description:     Implements Dragging & Dropping of data
//                  FROM your application to another.
// Version:         5.2
// Date:            17-AUG-2010
// Target:          Win32, Delphi 5-2010
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//
// Lazarus adaption 10/2017 Michael Köcher / six1
// -----------------------------------------------------------------------------
// TODO -oanme -cCheckItOut : OleQueryLinkFromData
// TODO -oanme -cDocumentation : CutToClipboard and CopyToClipboard alters the value of PreferredDropEffect.
// TODO -oanme -cDocumentation : Clipboard must be flushed or emptied manually after CutToClipboard and CopyToClipboard. Automatic flush is not guaranteed.
// TODO -oanme -cDocumentation : Delete-on-paste. Why and How.
// TODO -oanme -cDocumentation : Optimized move. Why and How.
// TODO -oanme -cDocumentation : OnPaste event is only fired if target sets the "Paste Succeeded" clipboard format. Explorer does this for delete-on-paste move operations.
// TODO -oanme -cDocumentation : DragDetectPlus. Why and How.
// -----------------------------------------------------------------------------

interface

uses
  LCLIntf, LCLType, LMessages, LCLVersion,
  DragDrop,
  DragDropFormats,
  ActiveX,
  Controls,
  Classes
  , windows
  , types
  ;

{$include DragDrop.inc}

type
  // These have been disabled since they break the generated C++ headers.
  // I can't remember why I did them anyway. Probably something to do with pre
  // BCB5 support.
  // tagSTGMEDIUM = ActiveX.TStgMedium;
  // tagFORMATETC = ActiveX.TFormatEtc;

  TDragResult = (drDropCopy, drDropMove, drDropLink, drCancel,
    drOutMemory, drAsync, drUnknown);

  TDropEvent = procedure(Sender: TObject; DragType: TDragType;
    var ContinueDrop: Boolean) of object;

  //: TAfterDropEvent is fired after the target has finished processing a
  // successfull drop.
  // The Optimized parameter is True if the target either performed an operation
  // other than a move or performed an "optimized move". In either cases, the
  // source isn't required to delete the source data.
  // If the Optimized parameter is False, the target performed an "unoptimized
  // move" operation and the source is required to delete the source data to
  // complete the move operation.
  TAfterDropEvent = procedure(Sender: TObject; DragResult: TDragResult;
    Optimized: Boolean) of object;

  TFeedbackEvent = procedure(Sender: TObject; Effect: LongInt;
    var UseDefaultCursors: Boolean) of object;

  //: The TDropDataEvent event is fired when the target requests data from the
  // drop source or offers data to the drop source.
  // The Handled flag should be set if the event handler satisfied the request.
  TDropDataEvent = procedure(Sender: TObject; const FormatEtc: TFormatEtc;
    out Medium: TStgMedium; var Handled: Boolean) of object;

  //: TPasteEvent is fired when the target sends a "Paste Succeeded" value
  // back to the drop source after a clipboard transfer.
  // The DeleteOnPaste parameter is True if the source is required to delete
  // the source data. This will only occur after a CutToClipboard operation
  // (corresponds to a move drag/drop).
  TPasteEvent = procedure(Sender: TObject; Action: TDragResult;
    DeleteOnPaste: boolean) of object;


  // : TGetDragImageEvent is fired when the drop source initializes the drag
  // image.
  TGetDragImageEvent = procedure(Sender: TObject;
    const DragSourceHelper: IDragSourceHelper; var Handled: boolean) of object;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropSource
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for all Drop Source components.
// Implements the IDropSource and az interfaces.
////////////////////////////////////////////////////////////////////////////////
TCustomDropSource = class(TDragDropComponent, IDropSource, IDataObject,
    IAsyncOperation2)
  private
    FDragTypes: TDragTypes;
    FFeedbackEffect: LongInt;
    // Events...
    FOnDrop: TDropEvent;
    FOnAfterDrop: TAfterDropEvent;
    FOnFeedback: TFeedBackEvent;
    FOnGetData: TDropDataEvent;
    FOnSetData: TDropDataEvent;
    FOnPaste: TPasteEvent;
    FOnGetDragImage: TGetDragImageEvent;
    // Drag images...
    FImages: TImageList;
    FShowImage: boolean;
    FImageIndex: integer;
    FImageHotSpot: TPoint;
    FDragSourceHelper: IDragSourceHelper;
    // Async transfer...
    FAllowAsync: boolean;
    FRequestAsync: boolean;
    FAsyncSourceTransfer: boolean;
    FAsyncTargetTransfer: boolean;
    FAsyncDataObject: IDataObject;
    FAsyncDropSource: IDropSource;
    FAsyncTargetEvent: THandle;
    FDragInProgress: boolean;

  protected
    property FeedbackEffect: LongInt read FFeedbackEffect write FFeedbackEffect;

    // IDropSource implementation
    function QueryContinueDrag(fEscapePressed: bool;
      grfKeyState: LongWord): HRESULT; stdcall;
    function GiveFeedback(dwEffect: LongWord): HRESULT; stdcall;

    // IDataObject implementation
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;  out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    {$IF FPC_FULLVERSION >= 30101}
      function SetData(Const pformatetc : FORMATETC; var medium:STGMEDIUM; FRelease : BOOL):HRESULT; StdCall;
    {$ELSE}
      function SetData(Const pformatetc : FORMATETC; const medium:STGMEDIUM; FRelease : BOOL):HRESULT; StdCall;
      //SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: Bool): HRESULT; stdcall;
    {$ENDIF}
    function EnumFormatEtc(dwDirection: LongWord;   out enumformatetcpara: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: LongWord;   const advsink: IAdviseSink; out dwConnection: LongWord): HRESULT; stdcall;
    function dUnadvise(dwConnection: LongWord): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;

    // IAsyncOperation implementation
    function EndOperation(hResult: HRESULT; const pbcReserved: IBindCtx;
      dwEffects: DWORD): HResult; stdcall;
    function GetAsyncMode(out pfIsOpAsync: Bool): HRESULT; stdcall;
    function InOperation(out pfInAsyncOp: Bool): HRESULT; stdcall;
    function SetAsyncMode(fDoOpAsync: Bool): HRESULT; stdcall;
    function StartOperation(const pbcReserved: IBindCtx): HRESULT; stdcall;

    function GetEnumFormatEtc(dwDirection: Longint): IEnumFormatEtc; //override;

    // Abstract methods
    function DoGetData(const FormatEtcIn: TFormatEtc;  out Medium: TStgMedium): HRESULT; virtual; abstract;
    function DoSetData(const FormatEtc: TFormatEtc;  var Medium: TStgMedium): HRESULT; virtual;
    function HasFormat(const FormatEtc: TFormatEtc): boolean; virtual; abstract;
//    function GetEnumFormatEtc(dwDirection: LongWord): IEnumFormatEtc; virtual; abstract;

    function DoExecute: TDragResult; virtual;

    // Data format event sink
    procedure DataChanging(Sender: TObject); virtual;

    // Clipboard
    function CutOrCopyToClipboard: boolean; virtual;
    procedure DoOnPaste(Action: TDragResult; DeleteOnPaste: boolean); virtual;

    // Property access
    procedure SetImages(const Value: TImageList);
    procedure SetImageIndex(const Value: integer);
    procedure SetPoint(Index: integer; Value: integer);
    function GetPoint(Index: integer): integer;
    function GetPerformedDropEffect: longInt; virtual;
    function GetLogicalPerformedDropEffect: longInt; virtual;
    procedure SetPerformedDropEffect(const Value: longInt); virtual;
    function GetPreferredDropEffect: longInt; virtual;
    procedure SetPreferredDropEffect(const Value: longInt); virtual;
    function GetInShellDragLoop: boolean; virtual;
    function GetTargetCLSID: TCLSID; virtual;
    procedure SetInShellDragLoop(const Value: boolean); virtual;
    function GetLiveDataOnClipboard: boolean;
    procedure SetAllowAsync(const Value: boolean);

    // Component management
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // Drag image support
    function GetDragImage: boolean; virtual;
    function GetDragImageFromImageList: boolean;
    property DragSourceHelper: IDragSourceHelper read FDragSourceHelper;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(Asynchronous: boolean = False): TDragResult; virtual;
    function CutToClipboard: boolean; virtual;
    function CopyToClipboard: boolean; virtual;
    procedure FlushClipboard; virtual;
    procedure EmptyClipboard; virtual;

    property PreferredDropEffect: longInt read GetPreferredDropEffect
      write SetPreferredDropEffect;
    property PerformedDropEffect: longInt read GetPerformedDropEffect
      write SetPerformedDropEffect;
    property LogicalPerformedDropEffect: longInt read GetLogicalPerformedDropEffect;
    property InShellDragLoop: boolean read GetInShellDragLoop
      write SetInShellDragLoop;
    property TargetCLSID: TCLSID read GetTargetCLSID;
    property LiveDataOnClipboard: boolean read GetLiveDataOnClipboard;
    property AsyncTransfer: boolean read FAsyncTargetTransfer;
    property DragInProgress: boolean read FDragInProgress;

  published
    property DragTypes: TDragTypes read FDragTypes write FDragTypes;
    // Events
    property OnFeedback: TFeedbackEvent read FOnFeedback write FOnFeedback;
    property OnDrop: TDropEvent read FOnDrop write FOnDrop;
    property OnAfterDrop: TAfterDropEvent read FOnAfterDrop write FOnAfterDrop;
    property OnGetData: TDropDataEvent read FOnGetData write FOnGetData;
    property OnSetData: TDropDataEvent read FOnSetData write FOnSetData;
    property OnPaste: TPasteEvent read FOnPaste write FOnPaste;
    property OnGetDragImage: TGetDragImageEvent read FOnGetDragImage write FOnGetDragImage;

    // Drag Images...
    property Images: TImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property ShowImage: boolean read FShowImage write FShowImage default False;
    property ImageHotSpotX: integer index 1 read GetPoint write SetPoint default 16;
    property ImageHotSpotY: integer index 2 read GetPoint write SetPoint default 16;
    // Async transfer...
    property AllowAsyncTransfer: boolean read FAllowAsync write SetAllowAsync default False;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropMultiSource
//
////////////////////////////////////////////////////////////////////////////////
// Drop target base class which can accept multiple formats.
////////////////////////////////////////////////////////////////////////////////
TCustomDropMultiSource = class(TCustomDropSource)
private
  FFeedbackDataFormat: TFeedbackDataFormat;
  FRawDataFormat: TRawDataFormat;

protected
  function DoGetData(const FormatEtcIn: TFormatEtc;
    out Medium: TStgMedium):HRESULT; override;
  function DoSetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium): HRESULT; override;
  function HasFormat(const FormatEtc: TFormatEtc): boolean; override;
  function GetEnumFormatEtc(dwDirection: Longint): IEnumFormatEtc; //override;

  function GetPerformedDropEffect: DWord; //override;
  function GetLogicalPerformedDropEffect: DWord;// override;
  function GetPreferredDropEffect: DWord; //override;
  procedure SetPerformedDropEffect(const Value: LongWord); //override;
  procedure SetPreferredDropEffect(const Value: LongWord); //override;
  function GetInShellDragLoop: boolean; override;
  procedure SetInShellDragLoop(const Value: boolean); override;
  function GetTargetCLSID: TCLSID; override;

  procedure DoOnSetData(DataFormat: TCustomDataFormat;
    ClipboardFormat: TClipboardFormat);

public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure GetCompatibleClipboardFormats(const DataFormatClass: TDataFormatClass;
    ClipboardFormats: TClipboardFormats); override;
  property DataFormats;
  // TODO : Add support for delayed rendering with OnRenderData event.
published
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropEmptySource
//
////////////////////////////////////////////////////////////////////////////////
// Do-nothing source for use with TDataFormatAdapter and such
////////////////////////////////////////////////////////////////////////////////
  TDropEmptySource = class(TCustomDropMultiSource);


////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
  function DropEffectToDragResult(DropEffect: longInt): TDragResult;





(*******************************************************************************
**
**			IMPLEMENTATION
**
*******************************************************************************)
implementation

uses
  Messages,
  CommCtrl,
  ComObj,
  Graphics,
  SysUtils;

resourcestring
  sDropSourceBusy = 'A drag and drop operation is already in progress';
  sDropSourceAsyncFailed = 'Failed to initiate asynchronouse drag and drop operation';
  sDropSourceAsyncBusy = 'An asynchronous drag and drop operation is in progress';

////////////////////////////////////////////////////////////////////////////////
//
//              Utility functions
//
////////////////////////////////////////////////////////////////////////////////
function DropEffectToDragResult(DropEffect: longInt): TDragResult;
begin
  case DropEffect of
    DROPEFFECT_NONE:
      Result := drCancel;
    DROPEFFECT_COPY:
      Result := drDropCopy;
    DROPEFFECT_MOVE:
      Result := drDropMove;
    DROPEFFECT_LINK:
      Result := drDropLink;
  else
    Result := drUnknown; // This is probably an error condition
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
// Experimental undocumented Windows Shell Drag Image support
//
////////////////////////////////////////////////////////////////////////////////
// This will probably only work on Windows 9x or in Shell Extensions/Shell Name
// Space Extensions.
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDropSourceThread
//
////////////////////////////////////////////////////////////////////////////////
// Executes a drop source operation from a thread.
// TDropSourceThread is an alternative to the Windows 2000 Asynchronous Data
// Transfer support.
////////////////////////////////////////////////////////////////////////////////
type
  TDropSourceThread = class(TThread)
  private
    FDropSource: TCustomDropSource;
    FDragResult: TDragResult;
    FDataObjectStream: pointer;
    FDropSourceStream: pointer;
    FStarted: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(ADropSource: TCustomDropSource);
    destructor Destroy; override;
    procedure Start;
    property DragResult: TDragResult read FDragResult;
    property Started: THandle read FStarted;
  end;

constructor TDropSourceThread.Create(ADropSource: TCustomDropSource);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FDropSource := ADropSource;
  FDragResult := drAsync;
  FStarted := Windows.CreateEvent(nil, False, False, nil);

  // Marshall interfaces to thread for use by DoDragDrop API function.
  OleCheck(CoMarshalInterThreadInterfaceInStream(IDataObject, FDropSource,
    IStream(FDataObjectStream)));
  // Note: For some reason I get an "Interface not supported" if I attempt to
  // marshall the IDropSource interface. It seems to work fine if I marshall the
  // IUnknown interface instead.
  OleCheck(CoMarshalInterThreadInterfaceInStream(IUnknown, FDropSource,
    IStream(FDropSourceStream)));
end;

destructor TDropSourceThread.Destroy;
begin
  if (FDataObjectStream <> nil) then
    IUnknown(FDataObjectStream)._Release;
  if (FDropSourceStream <> nil) then
    IUnknown(FDropSourceStream)._Release;
  FileClose(FStarted); { *Konvertiert von CloseHandle* }
  inherited Destroy;
end;

procedure TDropSourceThread.Execute;
var
  pt: TPoint;
  hwndAttach: HWND;
  AttachThreadID, CurrentThreadID: DWORD;
  Msg: TMsg;
begin
  (*
  ** See Microsoft Knowledgebase Article Q139408 for an explanation of the
  ** AttachThreadInput stuff.
  **   http://support.microsoft.com/support/kb/articles/Q139/4/08.asp
  *)

  try
    hwndAttach := GetForegroundWindow();
    // Fallback to the unsafe method in case GetForegroundWindow didn't work
    // out (from MSDN: The foreground window can be NULL in certain
    // circumstances, such as when a window is losing activation).
    if (hwndAttach = 0) then
    begin
      // Get handle of window under mouse-cursor.
      // Warning: This introduces a race condition. The cursor might have moved
      // from the original drop source window to another window. This can happen
      // easily if the user moves the cursor rapidly or if sufficient time has
      // elapsed since DragDetect exited.
      GetCursorPos(pt);
      hwndAttach := WindowFromPoint(pt);
    end;

    if (hwndAttach <> 0) then
      AttachThreadID := GetWindowThreadProcessId(hwndAttach, nil)
    else
      // Fall back to main thread (correct in most cases anyway)
      AttachThreadID := MainThreadID;

    // Get thread IDs.
    CurrentThreadID := GetCurrentThreadId();

    // Attach input queues if necessary.
    if (AttachThreadID <> CurrentThreadID) then
      AttachThreadInput(AttachThreadID, CurrentThreadID, True);
    try

      // Notify drop source that drag is progressing
      SetEvent(FStarted);

      // Initialize OLE for this thread.
      OleInitialize(nil);
      try
        // Unmarshall interfaces passed to us from main thread and give them to
        // the drop source component for use in the DoDragDrop API call.
        try
          OleCheck(CoGetInterfaceAndReleaseStream(IStream(FDataObjectStream),
            IDataObject, FDropSource.FAsyncDataObject));
          FDataObjectStream := nil;
          OleCheck(CoGetInterfaceAndReleaseStream(IStream(FDropSourceStream),
            IAsyncOperation, FDropSource.FAsyncDropSource));
          FDropSourceStream := nil;

          // Start drag & drop.
          FDragResult := FDropSource.DoExecute;

        finally
          FDropSource.FAsyncDropSource := nil;
          FDropSource.FAsyncDataObject := nil;
        end;

        // In case the drop target is also performing an asynchronous transfer
        // (via IAsyncOperation), we must wait for the transfer to complete.
        // Warning: We have to do this because the drop target will be
        // disconnected from the drop source if the thread that started the
        // drag/drop terminates before the transfer has completed.
        while (FDropSource.AsyncTransfer) and (not Terminated) do
        begin
          // Must pump message queue or drag/drop will freeze and we will never
          // get out of this loop.
          while (PeekMessage(Msg, 0, 0, 0, PM_REMOVE)) do
          begin
            if (Msg.Message = WM_QUIT) then
            begin
              PostQuitMessage(Msg.wParam);
              Terminate;
              break;
            end;
            DispatchMessage(Msg);
          end;
          MsgWaitForMultipleObjects(1, FDropSource.FAsyncTargetEvent, False, 1000, QS_ALLINPUT);
        end;
      finally
        OleUninitialize;
      end;

    finally
      // Restore input queue settings.
      if (AttachThreadID <> CurrentThreadID) then
        AttachThreadInput(AttachThreadID, CurrentThreadID, False);
      // Set Terminated flag so owner knows that drag has finished.
      Terminate;

      FDropSource.FDragInProgress := False;
      FDropSource.FAsyncSourceTransfer := False;
    end;
  except
    FDragResult := drUnknown;
    // Make sure OnAfterDrop gets called to notify user that things went wrong
    // and reset the async flag.
    FDropSource.EndOperation(E_UNEXPECTED, nil, DROPEFFECT_NONE);
    Terminate;
  end;
end;

procedure TDropSourceThread.Start;
begin
  Resume;
end;

// -----------------------------------------------------------------------------
//                      TCustomDropSource
// -----------------------------------------------------------------------------

constructor TCustomDropSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragTypes := [dtCopy]; //default to Copy.

  // Note: Normally we would call _AddRef or coLockObjectExternal(Self) here to
  // make sure that the component wasn't deleted prematurely (e.g. after a call
  // to RegisterDragDrop), but since our ancestor class TInterfacedComponent
  // disables reference counting, we do not need to do so.

  FImageHotSpot := Point(16,16);
  FImages := nil;
  FImageIndex := -1;
end;

destructor TCustomDropSource.Destroy;
begin
  FlushClipboard;
  if (FAsyncTargetEvent <> 0) then
    FileClose(FAsyncTargetEvent); { *Konvertiert von CloseHandle* }
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TCustomDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

// -----------------------------------------------------------------------------


{$IF FPC_FULLVERSION >= 30101}
  function TCustomDropSource.SetData(Const pformatetc : FORMATETC; var medium:STGMEDIUM; FRelease : BOOL):HRESULT;
{$ELSE}
  function TCustomDropSource.SetData(Const pformatetc : FORMATETC; const medium:STGMEDIUM; FRelease : BOOL):HRESULT;
      //SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: Bool): HRESULT; stdcall;
{$ENDIF}
begin
  // Warning: Ordinarily it would be much more efficient to just call
  // HasFormat(FormatEtc) to determine if we support the given format, but
  // because we have to able to accept *all* data formats, even unknown ones, in
  // order to support the Windows 2000 drag helper functionality, we can't
  // reject any formats here. Instead we pass the request on to DoSetData and
  // let it worry about the details.

  // if (HasFormat(FormatEtc)) then
  // begin
    try
// todo      Result := DoSetData(FormatEtc, Medium);
    finally
// todo
      //if (fRelease) then
      //  ReleaseStgMedium(Medium);
    end;
  // end else
  //   Result:= DV_E_FORMATETC;
end;
// -----------------------------------------------------------------------------

function TCustomDropSource.DAdvise(const FormatEtc: TFormatEtc; advf: Longword;
  const advSink: IAdviseSink; out dwConnection: Longword): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomDropSource.DUnadvise(dwConnection: Longword): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomDropSource.EnumDAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TCustomDropSource.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium):HRESULT; stdcall;
var
  Handled: boolean;
begin
  Handled := False;
  if (Assigned(FOnGetData)) then
    // Fire event to ask user for data.
    FOnGetData(Self, FormatEtcIn, Medium, Handled);

  // If user provided data, there is no need to call descendant for it.
  if (Handled) then
    Result := S_OK
  else if (HasFormat(FormatEtcIn)) then
    // Call descendant class to get data.
    Result := DoGetData(FormatEtcIn, Medium)
  else
    Result:= DV_E_FORMATETC;
end;

function TCustomDropSource.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium):HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TCustomDropSource.GetDragImage: boolean;
begin
  Result := False;
  if (Assigned(FOnGetDragImage)) then
    FOnGetDragImage(Self, DragSourceHelper, Result);

  if (not Result) then
    Result := GetDragImageFromImageList;
end;

function TCustomDropSource.GetDragImageFromImageList: boolean;

  function GetRGBColor(Value: TColor): DWORD;
  begin
    Result := ColorToRGB(Value);
    case Result of
      clNone: Result := CLR_NONE;
      clDefault: Result := CLR_DEFAULT;
    end;
  end;

var
  shDragImage: TSHDRAGIMAGE;
  DragBitmap: TBitmap;
begin
  Result := False;
  if (Images <> nil) and (ImageIndex >= 0) then
  begin
    DragBitmap := TBitmap.Create;
    try
      DragBitmap.PixelFormat := pfDevice;

      // TImageList.GetBitmap uses TImageList.Draw to extract the bitmap so we
      // must clear the destination bitmap before extraction.
      if (FImages.BkColor <> clNone) then
        DragBitmap.Canvas.Brush.Color := Images.BkColor;
      DragBitmap.Canvas.FillRect(DragBitmap.Canvas.ClipRect);
      Images.GetBitmap(ImageIndex, DragBitmap);
      shDragImage.crColorKey := GetRGBColor(Images.BkColor);

      shDragImage.hbmpDragImage := DragBitmap.Handle;
      shDragImage.sizeDragImage.cx := DragBitmap.Width;
      shDragImage.sizeDragImage.cy := DragBitmap.Height;
      shDragImage.ptOffset.x := ImageHotSpotX;
      shDragImage.ptOffset.y := ImageHotSpotY;

      if (Succeeded(DragSourceHelper.InitializeFromBitmap(shDragImage, Self))) then
      begin
        // Apparently the bitmap is now owned by the drag/drop image handler...
        // The documentation doesn't mention this explicitly, but the
        // implemtation of Microsoft's SDK samples suggests that this is the
        // case.
        DragBitmap.ReleaseHandle;
        Result := True;
      end;
    finally
      DragBitmap.Free;
    end;
  end;
end;

function TCustomDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
begin
  if (HasFormat(FormatEtc)) then
    Result:= S_OK
  else
    Result:= DV_E_FORMATETC;
end;

function TCustomDropSource.EnumFormatEtc(dwDirection: Longword; out enumformatetcpara:IEnumFormatEtc): HRESULT; stdcall;
begin
  enumformatetcpara := GetEnumFormatEtc(dwDirection);
  if (enumformatetcpara <> nil) then
    Result := S_OK
  else
    Result := E_NOTIMPL;
end;

// Implements IDropSource.QueryContinueDrag
function TCustomDropSource.QueryContinueDrag(fEscapePressed: bool;
  grfKeyState: Longword): HRESULT; stdcall;
var
  ContinueDrop: Boolean;
  DragType: TDragType;
begin
  if FEscapePressed then
    Result := DRAGDROP_S_CANCEL
  // Allow drag and drop with either mouse buttons.
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := DropEffectToDragType(FeedbackEffect, DragType) and
      (DragType in DragTypes);

    InShellDragLoop := False;

    // If a valid drop then do OnDrop event if assigned...
    if ContinueDrop and Assigned(OnDrop) then
      OnDrop(Self, DragType, ContinueDrop);

    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end else
    Result := S_OK;
end;

// Implements IDropSource.GiveFeedback
function TCustomDropSource.GiveFeedback(dwEffect: Longword): HRESULT; stdcall;
var
  UseDefaultCursors: Boolean;
begin
  UseDefaultCursors := True;
  FeedbackEffect := dwEffect;
  if Assigned(OnFeedback) then
    OnFeedback(Self, dwEffect, UseDefaultCursors);
  if UseDefaultCursors then
    Result := DRAGDROP_S_USEDEFAULTCURSORS
  else
    Result := S_OK;
end;

function TCustomDropSource.DoSetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium): HRESULT;
var
  Handled: boolean;
begin
  Result := E_NOTIMPL;
  if (Assigned(FOnSetData)) then
  begin
    Handled := False;
    // Fire event to ask user to handle data.
    FOnSetData(Self, FormatEtc, Medium, Handled);
    if (Handled) then
      Result := S_OK;
  end;
end;

procedure TCustomDropSource.SetAllowAsync(const Value: boolean);
begin
  if (FAllowAsync <> Value) then
  begin
    FAllowAsync := Value;
    if (not FAllowAsync) then
    begin
      FRequestAsync := False;
      FAsyncTargetTransfer := False;
    end;
  end;
end;

function TCustomDropSource.GetAsyncMode(out pfIsOpAsync: Bool): HRESULT;
begin
  pfIsOpAsync := FRequestAsync;
  Result := S_OK;
end;

function TCustomDropSource.SetAsyncMode(fDoOpAsync: Bool): HRESULT;
begin
  if (FAllowAsync) then
  begin
    Result := S_OK;
    if (FRequestAsync <> fDoOpAsync) then
    begin
      FRequestAsync := fDoOpAsync;

      // The following AddRef is required according to SDK and MSDN. Don't know
      // why...
      // _AddRef;
      // The corresponding Release is in IAsynOperation.EndOperation.
    end;
  end else
    Result := E_NOTIMPL;
end;

function TCustomDropSource.InOperation(out pfInAsyncOp: Bool): HRESULT;
begin
  pfInAsyncOp := AsyncTransfer;
  Result := S_OK;
end;

function TCustomDropSource.StartOperation(const pbcReserved: IBindCtx): HRESULT;
begin
  if (FRequestAsync) then
  begin
    if (FAsyncTargetEvent <> 0) then
    begin
      FileClose(FAsyncTargetEvent); { *Konvertiert von CloseHandle* }
      FAsyncTargetEvent := 0;
    end;
    FAsyncTargetEvent := Windows.CreateEvent(nil, False, False, nil);
    FAsyncTargetTransfer := True;
    Result := S_OK;
  end else
    Result := E_NOTIMPL;
end;

function TCustomDropSource.EndOperation(hResult: HRESULT;
  const pbcReserved: IBindCtx; dwEffects: DWORD): HRESULT;
var
  DropResult: TDragResult;
begin
  if (AsyncTransfer) then
  begin
    // The following Release is required according to SDK and MSDN. Don't know
    // why...
    // _Release;
    // The corresponding AddRef is in IAsynOperation.SetAsyncMode.

    SetEvent(FAsyncTargetEvent);
    FileClose(FAsyncTargetEvent); { *Konvertiert von CloseHandle* }
    FAsyncTargetEvent := 0;

    FAsyncTargetTransfer := False;
    if (Assigned(FOnAfterDrop)) then
    begin
      if (Succeeded(hResult)) then
        DropResult := DropEffectToDragResult(dwEffects and DragTypesToDropEffect(FDragTypes))
      else
        DropResult := drUnknown;
      // Note: The following logic is slightly different from the corresponding
      // code in TCustomDropSource.DoExecute. This might be a bug.
      FOnAfterDrop(Self, DropResult,
        (DropResult <> drDropMove) or (PerformedDropEffect <> DROPEFFECT_MOVE));
    end;
    Result := S_OK;
  end else
    Result := E_FAIL;
end;

function TCustomDropSource.DoExecute: TDragResult;
var
  DropResult: HRESULT;
  AllowedEffects,
  DropEffect: longint;
  IsDraggingImage: boolean;
begin
  AllowedEffects := DragTypesToDropEffect(FDragTypes);

  if (FShowImage) then
  begin
    // Attempt to create Drag Drop helper object.
    // At present this is only supported on Windows 2000 and later (and reported
    // broken in Windows ME).
    // If the object can't be created, we fall back to the old image list based
    // method (which only works on Win9x or within the application).
    // TODO : IDragSourceHelper2 (Vista), CFSTR_DROPDESCRIPTION
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
      IDragSourceHelper, FDragSourceHelper))) and (FDragSourceHelper <> nil) then
    begin
      // Create a drag image.
      IsDraggingImage := GetDragImage;
    end else
      IsDraggingImage := False;

    if (not IsDraggingImage) then
      FDragSourceHelper := nil;

    // Fall back to image list drag image if platform doesn't support
    // IDragSourceHelper or if we "just" failed to initialize properly.
    if (FDragSourceHelper = nil) and (FImages <> nil) then
    begin
      {$IF LCL_FULLVERSION >= 1090000}
         IsDraggingImage := ImageList_BeginDrag(FImages.ResolutionByIndex[0].Reference.Handle, FImageIndex, FImageHotSpot.X, FImageHotSpot.Y);
      {$else}
         IsDraggingImage := ImageList_BeginDrag(FImages.Handle, FImageIndex, FImageHotSpot.X, FImageHotSpot.Y);
      {$endif}
     end;
    end else
      IsDraggingImage := False;

  try
    InShellDragLoop := True;
    try

      (*************************************************************************
      **
      **               DoDragDrop - this is were it all starts
      **
      *************************************************************************)
(*
      if (FAsyncSourceTransfer) then
      begin
        DropResult := DoDragDrop(FAsyncDataObject, FAsyncDropSource, AllowedEffects, DropEffect);
        FAsyncDataObject := nil;
        FAsyncDropSource := nil;
      end else
*)
      DropResult := DoDragDrop(Self, Self, AllowedEffects, lpdword(@DropEffect));

      if (FAsyncSourceTransfer) then
      begin
        FAsyncDataObject := nil;
        FAsyncDropSource := nil;
      end;
    finally
      // InShellDragLoop is also reset in TCustomDropSource.QueryContinueDrag.
      // This is just to make absolutely sure that it is reset (actually no big
      // deal if it isn't).
      InShellDragLoop := False;
    end;

  finally
    if IsDraggingImage then
    begin
      if (FDragSourceHelper <> nil) then
      begin
        FDragSourceHelper := nil;
      end else
        ImageList_EndDrag;
    end;
  end;

  case DropResult of
    DRAGDROP_S_DROP:
      (*
      ** Special handling of "optimized move".
      ** If PerformedDropEffect has been set by the target to DROPEFFECT_MOVE
      ** and the drop effect returned from DoDragDrop is different from
      ** DROPEFFECT_MOVE, then an optimized move was performed.
      ** Note: This is different from how MSDN states that an optimized move is
      ** signalled, but matches how Windows 2000 signals an optimized move.
      **
      ** On Windows 2000 an optimized move is signalled by:
      ** 1) Returning DRAGDROP_S_DROP from DoDragDrop.
      ** 2) Setting drop effect to DROPEFFECT_NONE.
      ** 3) Setting the "Performed Dropeffect" format to DROPEFFECT_MOVE.
      **
      ** On previous version of Windows, an optimized move is signalled by:
      ** 1) Returning DRAGDROP_S_DROP from DoDragDrop.
      ** 2) Setting drop effect to DROPEFFECT_MOVE.
      ** 3) Setting the "Performed Dropeffect" format to DROPEFFECT_NONE.
      **
      ** The documentation states that an optimized move is signalled by:
      ** 1) Returning DRAGDROP_S_DROP from DoDragDrop.
      ** 2) Setting drop effect to DROPEFFECT_NONE or DROPEFFECT_COPY.
      ** 3) Setting the "Performed Dropeffect" format to DROPEFFECT_NONE.
      *)
      if (LogicalPerformedDropEffect = DROPEFFECT_MOVE) or
        ((DropEffect <> DROPEFFECT_MOVE) and (PerformedDropEffect = DROPEFFECT_MOVE)) then
        Result := drDropMove
      else
        Result := DropEffectToDragResult(DropEffect and AllowedEffects);
    DRAGDROP_S_CANCEL:
      Result := drCancel;
    E_OUTOFMEMORY:
      Result := drOutMemory;
    else
      // This should never happen!
      // ...but can happen if we pass something invalid to the drop target.
      // e.g. if we drop empty text on WordPad it will return DV_E_FORMATETC.
      Result := drUnknown;
  end;

  // Reset PerformedDropEffect if the target didn't set it.
  if (PerformedDropEffect = -1) then
    PerformedDropEffect := DROPEFFECT_NONE;

  // Abort the async transfer in progress if DoDragDrop returned an error.
  if (AsyncTransfer) and (DropResult <> DRAGDROP_S_DROP) then
    FAsyncTargetTransfer := False;

  // Fire OnAfterDrop event unless we are in the middle of an async data
  // transfer.
  if (not AsyncTransfer) and (Assigned(FOnAfterDrop)) then
    FOnAfterDrop(Self, Result,
      (Result = drDropMove) and
      ((DropEffect <> DROPEFFECT_MOVE) or (PerformedDropEffect <> DROPEFFECT_MOVE)));

end;


function TCustomDropSource.Execute(Asynchronous: boolean): TDragResult;
var
  AsyncThread: TDropSourceThread;
begin
  if (AsyncTransfer) then
    raise Exception.Create(sDropSourceAsyncBusy);
  if (DragInProgress) then
    raise Exception.Create(sDropSourceBusy);

  // Reset the "Performed Drop Effect" value. If it is supported by the target,
  // the target will set it to the desired value when the drop occurs.
  PerformedDropEffect := -1;

  FAsyncDataObject := nil;
  FAsyncDropSource := nil;
  FAsyncTargetTransfer := False;
  FAsyncSourceTransfer := False;
  FRequestAsync := False;
  if (AllowAsyncTransfer) then
    SetAsyncMode(True);

  if (Asynchronous) then
  begin
    // Perform an asynchronous drag and drop operation.
    FAsyncSourceTransfer := True;

    // Create a thread to perform the drag...
    AsyncThread := TDropSourceThread.Create(Self);
    try
      FDragInProgress := True;
      // ...and launch it.
      AsyncThread.Start;

      // Wait for thread to start.
      // If the thread takes longer than 10 seconds to start we assume that
      // something went wrong.
      if (WaitForSingleObject(AsyncThread.Started, 10000) <> WAIT_OBJECT_0) then
        raise Exception.Create(sDropSourceAsyncFailed);

(*
      // Wait for the transfer to complete.
      while DropEmptySource1.DragInProgress do
        Application.ProcessMessages;
      // Wait for the thread to terminate (it should do so itself when the
      // transfer completes)
      WaitFor;
*)
      Result := drAsync;
    except
      FAsyncSourceTransfer := False;
      FDragInProgress := False;
      AsyncThread.Terminate;
      raise;
    end;
  end else
  begin
    // Perform a normal synchronous drag and drop operation.
    FDragInProgress := True;
    try
      Result := DoExecute;
    finally
      FDragInProgress := False;
    end;
  end;
end;

function TCustomDropSource.GetPerformedDropEffect: longInt;
begin
  Result := DROPEFFECT_NONE;
end;

function TCustomDropSource.GetLogicalPerformedDropEffect: longInt;
begin
  Result := DROPEFFECT_NONE;
end;

procedure TCustomDropSource.SetPerformedDropEffect(const Value: longInt);
begin
  // Not implemented in base class
end;

function TCustomDropSource.GetPreferredDropEffect: longInt;
begin
  Result := DROPEFFECT_NONE;
end;

procedure TCustomDropSource.SetPreferredDropEffect(const Value: longInt);
begin
  // Not implemented in base class
end;

function TCustomDropSource.GetInShellDragLoop: boolean;
begin
  Result := False;
end;

function TCustomDropSource.GetTargetCLSID: TCLSID;
begin
  Result := GUID_NULL;
end;

procedure TCustomDropSource.SetInShellDragLoop(const Value: boolean);
begin
  // Not implemented in base class
end;

procedure TCustomDropSource.DataChanging(Sender: TObject);
begin
  // Data is changing - Flush clipboard to freeze the contents. 
  FlushClipboard;
end;

procedure TCustomDropSource.FlushClipboard;
begin
  // If we have live data on the clipboard...
  if (LiveDataOnClipboard) then
    // ...we force the clipboard to make a static copy of the data
    // before the data changes.
    OleCheck(OleFlushClipboard);
end;

procedure TCustomDropSource.EmptyClipboard;
begin
  // If we have live data on the clipboard...
  if (LiveDataOnClipboard) then
    // ...we empty the clipboard.
    OleCheck(OleSetClipboard(nil));
end;

function TCustomDropSource.CutToClipboard: boolean;
begin
  PreferredDropEffect := DROPEFFECT_MOVE;
  // Copy data to clipboard
  Result := CutOrCopyToClipboard;
end;
// -----------------------------------------------------------------------------

function TCustomDropSource.CopyToClipboard: boolean;
begin
  PreferredDropEffect := DROPEFFECT_COPY;
  // Copy data to clipboard
  Result := CutOrCopyToClipboard;
end;
// -----------------------------------------------------------------------------

function TCustomDropSource.CutOrCopyToClipboard: boolean;
begin
  Result := (Succeeded(OleSetClipboard(Self as IDataObject)));
end;

procedure TCustomDropSource.DoOnPaste(Action: TDragResult; DeleteOnPaste: boolean);
begin
  if (Assigned(FOnPaste)) then
    FOnPaste(Self, Action, DeleteOnPaste);
end;

function TCustomDropSource.GetLiveDataOnClipboard: boolean;
begin
  Result := (OleIsCurrentClipboard(Self as IDataObject) = S_OK);
end;

// -----------------------------------------------------------------------------

procedure TCustomDropSource.SetImages(const Value: TImageList);
begin
  if (FImages = Value) then
    exit;

  FImages := Value;

  if (csLoading in ComponentState) then
    exit;

  if (FImages = nil) or (FImageIndex >= FImages.Count) then
    FImageIndex := -1;
end;
// -----------------------------------------------------------------------------

procedure TCustomDropSource.SetImageIndex(const Value: integer);
begin
  if (csLoading in ComponentState) then
  begin
    FImageIndex := Value;
    exit;
  end;

  if (Value < 0) or (FImages = nil) or (FImages.Count = 0) then
    FImageIndex := -1
  else
  if (Value < FImages.Count) then
    FImageIndex := Value;
end;
// -----------------------------------------------------------------------------

procedure TCustomDropSource.SetPoint(Index: integer; Value: integer);
begin
  if (Index = 1) then
    FImageHotSpot.x := Value
  else
    FImageHotSpot.y := Value;
end;
// -----------------------------------------------------------------------------

function TCustomDropSource.GetPoint(Index: integer): integer;
begin
  if (Index = 1) then
    Result := FImageHotSpot.x
  else
    Result := FImageHotSpot.y;
end;

// -----------------------------------------------------------------------------

procedure TCustomDropSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TEnumFormatEtc
//
////////////////////////////////////////////////////////////////////////////////
// Format enumerator used by TCustomDropMultiTarget.
////////////////////////////////////////////////////////////////////////////////
type
  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FFormats: TClipboardFormats;
    FIndex: integer;
  protected
    constructor CreateClone(AFormats: TClipboardFormats; AIndex: Integer);
    property Formats: TClipboardFormats read FFormats;
    property Index: integer read FIndex write FIndex;
  public
    constructor Create; overload;
    constructor Create(AFormats: TDataFormats; Direction: TDataDirection); overload;
    destructor Destroy; override;
    { IEnumFormatEtc implentation }
    function Next(Celt:ULong;Out ELT:FormatEtc;pceltFetched:pULong=nil):HResult; StdCall;
    function Skip(Celt: LongWord): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HRESULT; stdcall;
  end;

constructor TEnumFormatEtc.Create(AFormats: TDataFormats; Direction: TDataDirection);
var
  i, j: integer;
begin
  Create;
  for i := 0 to AFormats.Count-1 do
    (*
    ** Only offer formats which contain data at this time.
    ** This might cause problems with formats which provides data on-demand.
    *)
    if AFormats[i].HasData then
      for j := 0 to AFormats[i].CompatibleFormats.Count-1 do
        if (Direction in AFormats[i].CompatibleFormats[j].DataDirection) and
          (Formats.FindFormat(AFormats[i].CompatibleFormats[j].ClipboardFormat) = nil) then
          Formats.Add(AFormats[i].CompatibleFormats[j]);
end;

constructor TEnumFormatEtc.Create;
begin
  inherited Create;
  FFormats := TClipboardFormats.Create(nil, False);
  FIndex := 0;
end;

constructor TEnumFormatEtc.CreateClone(AFormats: TClipboardFormats; AIndex: Integer);
begin
  Create;
  FIndex := AIndex;
  Formats.Assign(AFormats);
end;

destructor TEnumFormatEtc.Destroy;
begin
  FFormats.Free;
  FFormats := nil;
  inherited Destroy;
end;

function TEnumFormatEtc.Next(Celt:ULong;Out Elt:FormatEtc;pceltFetched:pULong=nil):HResult;
var
  i: integer;
  FormatEtc: PFormatEtc;
begin
  i := 0;
  FormatEtc := PFormatEtc(@Elt);
  while (i < Celt) and (FIndex < Formats.Count) do
  begin
    FormatEtc^ := Formats[FIndex].FormatEtc;
    Inc(FormatEtc);
    Inc(i);
    Inc(FIndex);
  end;

  if (pCeltFetched <> nil) then
    pCeltFetched^ := i;

  if (i = Celt) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumFormatEtc.Skip(Celt: LongWord): HRESULT;
begin
  if (FIndex + Celt <= Formats.Count) then
  begin
    inc(FIndex, Celt);
    Result := S_OK;
  end else
  begin
    FIndex := Formats.Count;
    Result := S_FALSE;
  end;
end;

function TEnumFormatEtc.Reset: HRESULT;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HRESULT;
begin
  Enum := TEnumFormatEtc.CreateClone(Formats, FIndex);
  Result := S_OK;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropMultiSource
//
////////////////////////////////////////////////////////////////////////////////
type
  TSourceDataFormats = class(TDataFormats)
  public
    function Add(DataFormat: TCustomDataFormat): integer; override;
  end;

function TSourceDataFormats.Add(DataFormat: TCustomDataFormat): integer;
begin
  Result := inherited Add(DataFormat);
  // Set up change notification so drop source can flush clipboard if data changes.
  DataFormat.OnChanging := TCustomDropMultiSource(DataFormat.Owner).DataChanging;
end;

constructor TCustomDropMultiSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataFormats := TSourceDataFormats.Create;
  FFeedbackDataFormat := TFeedbackDataFormat.Create(Self);
  FRawDataFormat := TRawDataFormat.Create(Self);
end;

destructor TCustomDropMultiSource.Destroy;
var
  i: integer;
begin
  // Must flush clipboard before data formats are destroyed. Otherwise clipboard
  // can be left with references to data which can no longer be supplied.
  FlushClipboard;
  
  // Delete all target formats owned by the object
  for i := FDataFormats.Count-1 downto 0 do
    FDataFormats[i].Free;
  FDataFormats.Free;
  inherited Destroy;
end;

function TCustomDropMultiSource.DoGetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HRESULT;
var
  i, j: integer;
  DF: TCustomDataFormat;
  CF: TClipboardFormat;
begin
  // TODO : Add support for delayed rendering with OnRenderData event.
  Medium.tymed := 0;
  Medium.PUnkForRelease := nil;
  Medium.hGlobal := 0;

  Result := DV_E_FORMATETC;

  (*
  ** Loop through all data formats associated with this drop source to find one
  ** which can offer the clipboard format requested by the target.
  *)
  for i := 0 to DataFormats.Count-1 do
  begin
    DF := DataFormats[i];

    // Ignore empty data formats.
    if (not DF.HasData) then
      continue;

    (*
    ** Loop through all the data format's supported clipboard formats to find
    ** one which contains data and can provide it in the format requested by the
    ** target.
    *)
    for j := 0 to DF.CompatibleFormats.Count-1 do
    begin
      CF := TClipboardFormat( DF.CompatibleFormats[j]);
      (*
      ** 1) Determine if the clipboard format supports the format requested by
      **    the target.
      ** 2) Transfer data from the data format object to the clipboard format
      **    object.
      ** 3) Determine if the clipboard format object now has data to offer.
      ** 4) Transfer the data from the clipboard format object to the medium.
      *)
      if (CF.AcceptFormat(FormatEtcIn)) and
        (DataFormats[i].AssignTo(CF)) and
        (CF.HasData) and
        (CF.SetDataToMedium(FormatEtcIn, Medium)) then
      begin
        // Once data has been sucessfully transfered to the medium, we clear
        // the data in the TClipboardFormat object in order to conserve
        // resources.
        CF.Clear;
        Result := S_OK;
        exit;
      end;
    end;
  end;
end;

function TCustomDropMultiSource.DoSetData(Const FormatEtc: TFormatEtc;
  var Medium: TStgMedium): HRESULT;
var
  i, j: integer;
  GenericClipboardFormat: TRawClipboardFormat;
begin
  Result := E_NOTIMPL;

  // Get data for requested source format.
  for i := 0 to DataFormats.Count-1 do
    for j := 0 to DataFormats[i].CompatibleFormats.Count-1 do
      if (DataFormats[i].CompatibleFormats[j].AcceptFormat(FormatEtc)) and
        (DataFormats[i].CompatibleFormats[j].GetDataFromMedium(Self, Medium)) and
        (DataFormats[i].Assign(DataFormats[i].CompatibleFormats[j])) then
      begin
        DoOnSetData(DataFormats[i], DataFormats[i].CompatibleFormats[j]);
        // Once data has been sucessfully transfered to the medium, we clear
        // the data in the TClipboardFormat object in order to conserve
        // resources.
        DataFormats[i].CompatibleFormats[j].Clear;
        Result := S_OK;
        exit;
      end;

  // The requested data format wasn't supported by any of the registered
  // clipboard formats, but in order to support the Windows 2000 drag drop helper
  // object we have to accept any data which is written to the IDataObject.
  // To do this we create a new clipboard format object, initialize it with the
  // format information passed to us and copy the data.
  GenericClipboardFormat := TRawClipboardFormat.CreateFormatEtc(FormatEtc);
  FRawDataFormat.CompatibleFormats.Add(GenericClipboardFormat);
  if (GenericClipboardFormat.GetDataFromMedium(Self, Medium)) and
    (FRawDataFormat.Assign(GenericClipboardFormat)) then
    Result := S_OK;
  // In reality we just have to support the undocumented DragContext and
  // DragImageBits formats as these are the ones used by the drag drop helpers,
  // but there's no benefit in implementing these two formats compared to this
  // generic solution.
end;

function TCustomDropSource.GetEnumFormatEtc(dwDirection: Integer): IEnumFormatEtc;
begin
  if (dwDirection = DATADIR_GET) then
    Result := TEnumFormatEtc.Create(FDataFormats, ddRead)
  else if (dwDirection = DATADIR_SET) then
    Result := TEnumFormatEtc.Create(FDataFormats, ddWrite)
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

procedure TCustomDropMultiSource.GetCompatibleClipboardFormats(
  const DataFormatClass: TDataFormatClass; ClipboardFormats: TClipboardFormats);
begin
  TDataFormatMap.GetClipboardFormats(DataFormatClass, ClipboardFormats, ddtSource);
end;

function TCustomDropMultiSource.GetEnumFormatEtc(dwDirection: Integer): IEnumFormatEtc;
begin
  if (dwDirection = DATADIR_GET) then
    Result := TEnumFormatEtc.Create(FDataFormats, ddRead)
  else if (dwDirection = DATADIR_SET) then
    Result := TEnumFormatEtc.Create(FDataFormats, ddWrite)
  else
    Result := nil;
end;

function TCustomDropMultiSource.HasFormat(const FormatEtc: TFormatEtc): boolean;
var
  i, j: integer;
begin
  Result := False;

  for i := 0 to DataFormats.Count-1 do
    for j := 0 to DataFormats[i].CompatibleFormats.Count-1 do
      if (DataFormats[i].CompatibleFormats[j].AcceptFormat(FormatEtc)) then
      begin
        Result := True;
        exit;
      end;
end;

function TCustomDropMultiSource.GetPerformedDropEffect: DWord;
begin
  Result := FFeedbackDataFormat.PerformedDropEffect;
end;

function TCustomDropMultiSource.GetLogicalPerformedDropEffect: DWord;
begin
  Result := FFeedbackDataFormat.LogicalPerformedDropEffect;
end;

function TCustomDropMultiSource.GetPreferredDropEffect: DWord;
begin
  Result := FFeedbackDataFormat.PreferredDropEffect;
end;

procedure TCustomDropMultiSource.SetPerformedDropEffect(const Value: DWord);
begin
  FFeedbackDataFormat.PerformedDropEffect := Value;
end;

procedure TCustomDropMultiSource.SetPreferredDropEffect(const Value: DWord);
begin
  FFeedbackDataFormat.PreferredDropEffect := Value;
end;

function TCustomDropMultiSource.GetInShellDragLoop: boolean;
begin
  Result := FFeedbackDataFormat.InShellDragLoop;
end;

procedure TCustomDropMultiSource.SetInShellDragLoop(const Value: boolean);
begin
  FFeedbackDataFormat.InShellDragLoop := Value;
end;

function TCustomDropMultiSource.GetTargetCLSID: TCLSID;
begin
  Result := FFeedbackDataFormat.TargetCLSID;
end;

procedure TCustomDropMultiSource.DoOnSetData(DataFormat: TCustomDataFormat;
  ClipboardFormat: TClipboardFormat);
var
  DropEffect: longInt;
begin
  if (ClipboardFormat is TPasteSucceededClipboardFormat) then
  begin
    DropEffect := TPasteSucceededClipboardFormat(ClipboardFormat).Value;
    DoOnPaste(DropEffectToDragResult(DropEffect),
      (DropEffect = DROPEFFECT_MOVE) and (PerformedDropEffect = DROPEFFECT_MOVE));
  end else
  if (ClipboardFormat is TPerformedDropEffectClipboardFormat) then
  begin
    // Sometimes the Explorer breaks the asyncronous transfer contract by
    // failing to call IAsyncOperation.EndOperation even though it has started
    // an asynchrounous transfer by calling IAsyncOperation.StartOperation.
    //
    // From the platform SDK's shldisp.idl:
    //
    // IDropTarget Object:
    //   IDropTarget::Drop() If asynch operations aren't supported, nothing is required.
    //     The asynch operation can only happen if GetAsyncMode() returns VARIANT_TRUE.
    //     Before starting the asynch operation, StartOperation(NULL) needs to be called before
    //     returning from IDropTarget::Drop().
    //     If the operation is asynch, ::EndOperation() needs to be called upon completion and
    //     the IAsyncOperation pointer needs to be released.  Do not set CFSTR_PERFORMEDDROPEFFECT
    //     if the IAsyncOperation interface is being used.
    //
    // From MSDN
    // http://msdn.microsoft.com/en-us/library/bb776904(VS.85).aspx#async
    // Dragging and Dropping Shell Objects Asynchronously, Using IASyncOperation:
    //
    // The following procedure outlines how the drop target uses the
    // IAsyncOperation interface to extract data asynchronously:
    // 1. When the system calls IDropTarget::Drop, call IDataObject::QueryInterface
    //    and request an IAsyncOperation interface (IID_IAsyncOperation) from the
    //    data object.
    // 2. Call IAsyncOperation::GetAsyncMode. If the method returns VARIANT_TRUE,
    //    the data object supports asynchronous data extraction.
    // 3. Create a separate thread to handle data extraction and call
    //    IAsyncOperation::StartOperation.
    // 4. Return the IDropTarget::Drop call, as you would for a normal data
    //    transfer operation. DoDragDrop will return and unblock the drop
    //    source. Do not call IDataObject::SetData to indicate the outcome of an
    //    optimized move or delete-on-paste operation. Wait until the operation
    //    is finished.
    // 5. Extract the data on the background thread. The target's primary thread
    //    is unblocked and free to proceed.
    // 6. If the data transfer was an optimized move or delete-on-paste operation,
    //    call IDataObject::SetData to indicate the outcome.
    // 7. Notify the data object that extraction is finished by calling
    //    IAsyncOperation::EndOperation.
    //
    // We attempt to fix that problem here:
    if (AsyncTransfer) then
    begin
      DropEffect := TPerformedDropEffectClipboardFormat(ClipboardFormat).Value;
      EndOperation(S_OK, nil, DropEffect);
    end;
  end;
end;

var
  DelphiDropTargets: TList = nil;

const
  sOleDropTargetInterface = 'OleDropTargetInterface';
  sOleDropTargetInterfaceSaved = 'OleDropTargetInterfaceSaved';

type
  TDropTargetData = record
    Window: HWND;
    DropTarget: THandle;
  end;
  PDropTargetData = ^TDropTargetData;

procedure DisableDropTarget(Window: HWND);
var
  DropTarget: THandle;
  DropTargetData: PDropTargetData;
begin
  // Warning : This is completely undocumented!
  DropTarget := GetProp(Window, sOleDropTargetInterface);

  // If the debugged application is reset or otherwise stopped before
  // RestoreDelphiDropTargets is called, then the saved handle will be lost.
  // In order to recover from this scenario we also save the target handle in
  // a window property.
  if (DropTarget = 0) then
    DropTarget := GetProp(Window, sOleDropTargetInterfaceSaved);

  if (DropTarget <> 0) then
  begin
    RemoveProp(Window, sOleDropTargetInterface);
    // Save target handle in case we need to recover it
    SetProp(Window, sOleDropTargetInterfaceSaved, DropTarget);

    if (DelphiDropTargets = nil) then
      DelphiDropTargets := TList.Create;

    New(DropTargetData);
    DropTargetData.Window := Window;
    DropTargetData.DropTarget := DropTarget;

    DelphiDropTargets.Add(DropTargetData);
  end;
end;

function DisableDropTargetsCallback(Window: HWND; Param: LPARAM): BOOL; stdcall;
begin
  DisableDropTarget(Window);

  Result := True;
end;

procedure DisableDelphiDropTargets;
var
  DelphiWindow: HWND;
begin
  DelphiWindow := FindWindow('TAppBuilder', nil);
  if (DelphiWindow <> 0) then
  begin
    DisableDropTarget(DelphiWindow);

    EnumChildWindows(DelphiWindow, @DisableDropTargetsCallback, 0);
  end;
end;

procedure RestoreDelphiDropTargets;
var
  i: integer;
  DropTargetData: PDropTargetData;
begin
  if (DelphiDropTargets <> nil) then
  begin
    for i := 0 to DelphiDropTargets.Count-1 do
    begin
      DropTargetData := PDropTargetData(DelphiDropTargets[i]);

      // Warning : This is completely undocumented!
      SetProp(DropTargetData.Window, sOleDropTargetInterface, DropTargetData.DropTarget);
      RemoveProp(DropTargetData.Window, sOleDropTargetInterfaceSaved);

      Dispose(DropTargetData);
    end;
    DelphiDropTargets.Free;
    DelphiDropTargets := nil;
  end;
end;



initialization
  // Disable Delphi as a drop target so we won't deadlock if we accidentally
  // drag over the IDE while debugging.
  //if (DebugHook <> 0) then
  //  DisableDelphiDropTargets;

finalization
  //if (DebugHook <> 0) then
  //  RestoreDelphiDropTargets;
end.


