unit DropTarget;

{$MODE Delphi}

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DropTarget
// Description:     Implements the drop target base classes which allows your
//                  application to accept data dropped on it from other
//                  applications.
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
  DragDrop
  , windows
  , LCLIntf, LCLType, LMessages, LCLVersion, ActiveX, Classes, Controls, CommCtrl, ExtCtrls, Forms
  , types
  , StdCtrls
  ;

{$include DragDrop.inc}

////////////////////////////////////////////////////////////////////////////////
//
//              TControlList
//
////////////////////////////////////////////////////////////////////////////////
// List of TWinControl objects.
// Used for the TCustomDropTarget.Targets property.
////////////////////////////////////////////////////////////////////////////////
type
  TControlList = class(TObject)
  private
    FList: TList;
    function GetControl(AIndex: integer): TWinControl;
    function GetCount: integer;
  protected
    function Add(AControl: TWinControl): integer;
    procedure Insert(Index: Integer; AControl: TWinControl);
    procedure Remove(AControl: TWinControl);
    procedure Delete(AIndex: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOf(AControl: TWinControl): integer;
    property Count: integer read GetCount;
    property Controls[AIndex: integer]: TWinControl read GetControl; default;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropTarget
//
////////////////////////////////////////////////////////////////////////////////
// Top level abstract base class for all drop target classes.
// Implements the IDropTarget and IDataObject interfaces.
////////////////////////////////////////////////////////////////////////////////
type
  TScrollDirection = (sdUp, sdDown, sdLeft, sdRight);
  TScrollDirections = set of TScrollDirection;
  TScrolDirections = TScrollDirections deprecated;

  TDropTargetScrollEvent = procedure(Sender: TObject; Point: TPoint;
    var Scroll: TScrollDirections; var Interval: integer) of object;

  TScrollBars = set of TScrollBarKind;

  TDropTargetEvent = procedure(Sender: TObject; ShiftState: TShiftState;
    APoint: TPoint; var Effect: Longint) of object;

  TMouseSample = record
    Time: DWORD;
    Pos: TPoint;
  end;

  TCustomDropTarget = class(TDragDropComponent, IDropTarget)
  private
    FDataObject: IDataObject;
    FDragTypes: TDragTypes;
    FGetDataOnEnter: boolean;
    FOnEnter: TDropTargetEvent;
    FOnDragOver: TDropTargetEvent;
    FOnLeave: TNotifyEvent;
    FOnDrop: TDropTargetEvent;
    FOnGetDropEffect: TDropTargetEvent;
    FOnScroll: TDropTargetScrollEvent;
    FTargets: TControlList;
    FMultiTarget: boolean;
    FOptimizedMove: boolean;
    FTarget: TWinControl;

    FImages: TImageList;
    FDragImageHandle: HImageList;
    FShowImage: boolean;
    FImageHotSpot: TPoint;
    FDropTargetHelper: IDropTargetHelper;
    // FLastPoint points to where DragImage was last painted (used internally)
    FLastPoint: TPoint;
    // Auto-scrolling enables scrolling of target window during drags and
    // paints any drag image 'cleanly'.
    FScrollBars: TScrollBars;
    FScrollTimer: TTimer;
    FAutoScroll: boolean;
    FNoScrollZone: TRect;
    FCustomScrollZone: boolean;
    FMouseSample: TMouseSample;
    FMouseNextSample: TMouseSample;
    FIsAsync: boolean;
    FOnEndAsyncTransfer: TNotifyEvent;
    FOnStartAsyncTransfer: TNotifyEvent;
    FAllowAsync: boolean;
    FAutoRegister: boolean;
    FEnabled: boolean;
    procedure SetNoScrollZone(const Value: TRect);
  protected
    // IDropTarget  implementation
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    //DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;StdCall;
    //DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD):HResult;StdCall;
    //Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;

    procedure DoEnter(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord); virtual;
    procedure DoDragOver(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord); virtual;
    procedure DoDrop(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord); virtual;
    procedure DoLeave; virtual;
    procedure DoOnPaste(var Effect: Integer); virtual;
    procedure DoScroll(Point: TPoint; var Scroll: TScrollDirections;
      var Interval: integer); virtual;

    function GetData(Effect: longInt): boolean; virtual;
    function DoGetData: boolean; virtual; abstract;
    procedure ClearData; virtual; abstract;
    function GetValidDropEffect(ShiftState: TShiftState; pt: TPoint;
      dwEffect: LongInt): LongInt; virtual;
    function GetPreferredDropEffect: LongInt; virtual;
    function SetPerformedDropEffect(Effect: LongInt): boolean; virtual;
    function SetPasteSucceeded(Effect: LongInt): boolean; virtual;
    procedure DoRegister(ATarget: TWinControl);
    procedure DoUnregister(ATarget: TWinControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetTarget: TWinControl;
    procedure SetTarget(const Value: TWinControl);
    procedure DoAutoScroll(Sender: TObject);
    function SampleMouse(MousePos: TPoint; First: boolean = False): boolean;
    procedure SetShowImage(Show: boolean);
    procedure SetDataObject(Value: IDataObject);
    procedure DoEndAsyncTransfer(Sender: TObject);
    property DropTargetHelper: IDropTargetHelper read FDropTargetHelper;
    property CustomScrollZone: boolean read FCustomScrollZone write FCustomScrollZone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Register(ATarget: TWinControl);
    procedure Unregister(ATarget: TWinControl = nil);
    function FindTarget(p: TPoint): TWinControl; virtual;
    function FindNearestTarget(p: TPoint): TWinControl;
    procedure Assign(Source: TPersistent); override;
    function HasValidFormats(const ADataObject: IDataObject): boolean; virtual; abstract;
    function PasteFromClipboard: longint; virtual;
    function CanPasteFromClipboard: boolean; virtual;
    procedure ResetScrollZone; virtual;

    procedure GetCompatibleClipboardFormats(const DataFormatClass: TDataFormatClass;
      ClipboardFormats: TClipboardFormats); override;

    property DataObject: IDataObject read FDataObject;
    property Targets: TControlList read FTargets;
    property NoScrollZone: TRect read FNoScrollZone write SetNoScrollZone;
    property AsyncTransfer: boolean read FIsAsync;
  published
    property DragTypes: TDragTypes read FDragTypes write FDragTypes;
    property GetDataOnEnter: Boolean read FGetDataOnEnter write FGetDataOnEnter default False;
    property Enabled: boolean read FEnabled write FEnabled default True;
    // Events...
    property OnEnter: TDropTargetEvent read FOnEnter write FOnEnter;
    property OnDragOver: TDropTargetEvent read FOnDragOver write FOnDragOver;
    property OnLeave: TNotifyEvent read FOnLeave write FOnLeave;
    property OnDrop: TDropTargetEvent read FOnDrop write FOnDrop;
    property OnGetDropEffect: TDropTargetEvent read FOnGetDropEffect
      write FOnGetDropEffect;
    property OnScroll: TDropTargetScrollEvent read FOnScroll write FOnScroll;
    property OnStartAsyncTransfer: TNotifyEvent read FOnStartAsyncTransfer
      write FOnStartAsyncTransfer;
    property OnEndAsyncTransfer: TNotifyEvent read FOnEndAsyncTransfer
      write FOnEndAsyncTransfer;
    // Drag Images...
    property ShowImage: boolean read FShowImage write SetShowImage default True;
    // Target
    property Target: TWinControl read GetTarget write SetTarget;
    property MultiTarget: boolean read FMultiTarget write FMultiTarget default False;
    property AutoRegister: boolean read FAutoRegister write FAutoRegister default True;
    // Auto-scroll
    property AutoScroll: boolean read FAutoScroll write FAutoScroll default True;
    // Misc
    property OptimizedMove: boolean read FOptimizedMove write FOptimizedMove default False;
    // Async transfer...
    property AllowAsyncTransfer: boolean read FAllowAsync write FAllowAsync default False;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropTarget
//
////////////////////////////////////////////////////////////////////////////////
// Deprecated base class for all drop target components.
// Replaced by the TCustomDropTarget class.
////////////////////////////////////////////////////////////////////////////////
  TDropTarget = class(TCustomDropTarget)
  end deprecated;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropDummy
//
////////////////////////////////////////////////////////////////////////////////
// The sole purpose of this component is to enable drag images to be displayed
// over the registered TWinControl(s). The component does not accept any drops.
////////////////////////////////////////////////////////////////////////////////
  TDropDummy = class(TCustomDropTarget)
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
  public
    function HasValidFormats(const ADataObject: IDataObject): boolean; override;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropMultiTarget
//
////////////////////////////////////////////////////////////////////////////////
// Drop target base class which can accept multiple formats.
////////////////////////////////////////////////////////////////////////////////
  TAcceptFormatEvent = procedure(Sender: TObject;
    const DataFormat: TCustomDataFormat; var Accept: boolean) of object;

  TCustomDropMultiTarget = class(TCustomDropTarget)
  private
    FOnAcceptFormat: TAcceptFormatEvent;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    procedure DoAcceptFormat(const DataFormat: TCustomDataFormat;
      var Accept: boolean); virtual;
    property OnAcceptFormat: TAcceptFormatEvent read FOnAcceptFormat
      write FOnAcceptFormat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasValidFormats(const ADataObject: IDataObject): boolean; override;
    property DataFormats;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropEmptyTarget
//
////////////////////////////////////////////////////////////////////////////////
// Do-nothing target for use with TDataFormatAdapter and such
////////////////////////////////////////////////////////////////////////////////
  TDropEmptyTarget = class(TCustomDropMultiTarget);


////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////



(*******************************************************************************
**
**			IMPLEMENTATION
**
*******************************************************************************)
implementation

uses
  DragDropFormats,
  ComObj,
  SysUtils,
  Graphics,
  Messages,
  ShlObj,
  ClipBrd,
  Dialogs,
  ComCtrls;

resourcestring
  sTargetAsyncBusy = 'Can''t clear data while async data transfer is in progress';
  // sRegisterFailed	= 'Failed to register %s as a drop target';
  // sUnregisterActiveTarget = 'Can''t unregister target while drag operation is in progress';

////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
//
//              TControlList
//
////////////////////////////////////////////////////////////////////////////////
constructor TControlList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TControlList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TControlList.Add(AControl: TWinControl): integer;
begin
  Result := FList.Add(AControl);
end;

procedure TControlList.Insert(Index: Integer; AControl: TWinControl);
begin
  FList.Insert(Index, AControl);
end;

procedure TControlList.Delete(AIndex: integer);
begin
  FList.Delete(AIndex);
end;

function TControlList.IndexOf(AControl: TWinControl): integer;
begin
  Result := FList.IndexOf(AControl);
end;

function TControlList.GetControl(AIndex: integer): TWinControl;
begin
  Result := TWinControl(FList[AIndex]);
end;

function TControlList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TControlList.Remove(AControl: TWinControl);
begin
  FList.Remove(AControl);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomDropTarget.Create(AOwner: TComponent);
var
  bm: TBitmap;
begin
  inherited Create(AOwner);
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := DoAutoScroll;

  // Note: Normally we would call _AddRef or coLockObjectExternal(Self) here to
  // make sure that the component wasn't deleted prematurely (e.g. after a call
  // to RegisterDragDrop), but since our ancestor class TInterfacedComponent
  // disables reference counting, we do not need to do so.

  FGetDataOnEnter := False;
  FTargets :=  TControlList.Create;

  FImages := TImageList.Create(Self);
  // Create a blank image for FImages which we will use to hide any cursor
  // 'embedded' in a drag image.
  // This avoids the possibility of two cursors showing.
  bm := TBitmap.Create;
  try
    bm.Height := 32;
    bm.Width := 32;
    bm.Canvas.Brush.Color := clWindow;
    bm.Canvas.FillRect(bm.Canvas.ClipRect);
    FImages.AddMasked(bm, clWindow);
  finally
    bm.Free;
  end;
  FDataObject := nil;
  ShowImage := True;
  FMultiTarget := False;
  FOptimizedMove := False;
  FAutoScroll := True;
  FAllowAsync := False;
  FAutoRegister := True;
  FEnabled := True;
end;

destructor TCustomDropTarget.Destroy;
begin
  FDataObject := nil;
  FDropTargetHelper := nil;
  Unregister;
  FImages.Free;
  FScrollTimer.Free;
  FTargets.Free;
  inherited Destroy;
end;

// TDummyWinControl is declared just to expose the protected property - Font -
// which is used to calculate the 'scroll margin' for the target window.
type
  TDummyWinControl = Class(TWinControl);



function TCustomDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
//DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  ShiftState: TShiftState;
  ClientPt: TPoint;
begin
  try
    if (Enabled) then
    begin
      ClearData;
      FDataObject := dataObj;
    end;
    Result := S_OK;


    // Find the target control.
    // TODO : FTarget is modified even when Enabled=False.
    FTarget := FindTarget(pt);

    (*
    ** If no target control has been registered we disable all features which
    ** depends on the existence of a drop target (e.g. drag images and auto
    ** scroll). Presently, this situation can only arise if the drop target is
    ** being used as a drop handler (TDrophandler component).
    ** Note also that if no target control exists, the mouse coordinates are
    ** relative to the screen, not the control as is normally the case.
    *)
    if (FTarget = nil) then
    begin
      ShowImage := False;
      AutoScroll := False;
    end else
    begin
      ClientPt := FTarget.ScreenToClient(pt);
      FLastPoint := ClientPt;
    end;

    // Sample mouse for auto-scroll.
    SampleMouse(Pt, True);

    (*
    ** Refuse the drag if we can't handle any of the data formats offered by
    ** the drop source. We must return S_OK here in order for the drop to continue
    ** to generate DragOver events for this drop target (needed for drag images).
    *)
    if (Enabled) and (HasValidFormats(FDataObject)) then
    begin
      ResetScrollZone; // Updates FScrollBars and FNoScrollZone

      // It's generally more efficient to get data only if and when a drop occurs
      // rather than on entering a potential target window.
      // However - sometimes there is a good reason to get it here.
      if FGetDataOnEnter then
        if (not GetData(dwEffect)) then
        begin
          FDataObject := nil;
          dwEffect := DROPEFFECT_NONE;
          Result := DV_E_CLIPFORMAT;
          exit;
        end;

      ShiftState := KeysToShiftStatePlus(grfKeyState);

      // Create a default drop effect based on the shift state and allowed
      // drop effects (or an OnGetDropEffect event if implemented).
      dwEffect := GetValidDropEffect(ShiftState, ClientPt, dwEffect);

      // Generate an OnEnter event
      DoEnter(ShiftState, ClientPt, dwEffect);

      // If IDropTarget.DragEnter returns with dwEffect set to DROPEFFECT_NONE it
      // means that the drop has been rejected and IDropTarget.DragOver should
      // not be called (according to MSDN). Unfortunately IDropTarget.DragOver is
      // called regardless of the value of dwEffect. We work around this problem
      // (bug?) by setting FDataObject to nil and thus internally rejecting the
      // drop in TCustomDropTarget.DragOver.
      if ((dwEffect and not(DROPEFFECT_SCROLL)) = DROPEFFECT_NONE) then
        FDataObject := nil;

    end else
    begin
      FDataObject := nil;
      dwEffect := DROPEFFECT_NONE;
    end;

    // Display drag image.
    // Note: This was previously done prior to caling GetValidDropEffect and
    // DoEnter. The SDK documentation states that IDropTargetHelper.DragEnter
    // should be called last in IDropTarget.DragEnter (presumably after dwEffect
    // has been modified), but Microsoft's own demo application calls it as the
    // very first thing (same for all other IDropTargetHelper methods).
    if ShowImage then
    begin
      // Attempt to create Drag Drop helper object.
      // At present this is only supported on Windows 2000 and later (and
      // reported broken in Windows ME).
      // If the object can't be created, we fall back to the old image list
      // based method (which only works on Win9x or within the application).
      if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
        IDropTargetHelper, FDropTargetHelper))) and
        (FDropTargetHelper <> nil) then
      begin
        // If the call to DragEnter fails (which it will do if the drop source
        // doesn't support IDropSourceHelper or hasn't specified a drag image),
        // we release the drop target helper and fall back to imagelist based
        // drag images.
        // Note: According to .\Samples\winui\Shell\DragImg\DragImg.doc from the
        // Platform SDK, the return value of IDropTargetHelper.DragEnter should
        // be ignored, but my tests show that it shouldn't.
        if (Failed(DropTargetHelper.DragEnter(FTarget.Handle, DataObj, pt, dwEffect))) then
          FDropTargetHelper := nil;
      end;

      if (FDropTargetHelper = nil) then
      begin
        FDragImageHandle := ImageList_GetDragImage(nil, @FImageHotSpot);
        if (FDragImageHandle <> 0) then
        begin
          // Currently we will just replace any 'embedded' cursor with our
          // blank (transparent) image otherwise we sometimes get 2 cursors ...
          {$IF LCL_FULLVERSION >= 1090000}
             ImageList_SetDragCursorImage(FImages.ResolutionByIndex[0].Reference.Handle, 0, FImageHotSpot.x, FImageHotSpot.y);
          {$else}
             ImageList_SetDragCursorImage(FImages.Handle, 0, FImageHotSpot.x, FImageHotSpot.y);
          {$endif}
          with ClientPtToWindowPt(FTarget.Handle, ClientPt) do
            ImageList_DragEnter(FTarget.handle, x, y);
        end;
      end;
    end else
      FDragImageHandle := 0;
  except
    // We must not allow exceptions to escape from any of the COM methods since
    // COM doesn't support exceptions.
    FDataObject := nil;
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

procedure TCustomDropTarget.DoEnter(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self, ShiftState, Point, Effect);
end;

function TCustomDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HResult;
var
  ShiftState: TShiftState;
  ClientPt: TPoint;
  CanScroll: boolean;
begin
  // Refuse drop if we dermined in DragEnter that a drop weren't possible,
  // but still handle drag images provided we have a valid target.
  if (FTarget = nil) then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
    exit;
  end;

  try
    ClientPt := FTarget.ScreenToClient(pt);

    ShiftState := KeysToShiftStatePlus(grfKeyState);

    // Update scroll zone unless user has specified a custom scroll zone
    if (not CustomScrollZone) then
      ResetScrollZone; // Updates FScrollBars and FNoScrollZone

    // Create a default drop effect based on the shift state and allowed
    // drop effects (or an OnGetDropEffect event if implemented).
    dwEffect := GetValidDropEffect(ShiftState, ClientPt, dwEffect);

    if (Enabled) and (FDataObject <> nil) then
    begin
      // Generate an OnDragOver event
      DoDragOver(ShiftState, ClientPt, dwEffect);
      Result := S_OK;
    end else
    begin
      // Even though this isn't an error condition per se, we must return
      // an error code (e.g. E_UNEXPECTED) in order for the cursor to change
      // to DROPEFFECT_NONE.
      Result := DV_E_CLIPFORMAT;
    end;

    // Sample mouse pos and calculate velocity for auto-scroll.
    CanScroll := SampleMouse(Pt);

    // Note: Auto-scroll is detected by the GetValidDropEffect method, but can
    // also be started by the user via the OnDragOver or OnGetDropEffect events.
    // Auto-scroll is initiated by specifying the DROPEFFECT_SCROLL value as
    // part of the drop effect.

    // Start the auto-scroll timer if auto-scroll was requested. Do *not* rely
    // on any other mechanisms to detect auto-scroll since the user can only
    // specify auto-scroll with the DROPEFFECT_SCROLL value.
    if (dwEffect and DROPEFFECT_SCROLL <> 0) then
    begin
      if (not FScrollTimer.Enabled) then
        FScrollTimer.Interval := DragDropScrollDelay; // hardcoded to 100 in previous versions.

      // Sample mouse for auto-scroll.
      FScrollTimer.Enabled := CanScroll;
    end else
      FScrollTimer.Enabled := False;

    // Move drag image
    if (DropTargetHelper <> nil) then
    begin
      OleCheck(DropTargetHelper.DragOver(pt, dwEffect));
    end else
    if (FDragImageHandle <> 0) then
    begin
      if ((FLastPoint.x <> ClientPt.x) or (FLastPoint.y <> ClientPt.y)) then
        with ClientPtToWindowPt(FTarget.Handle, ClientPt) do
          ImageList_DragMove(x, y);
    end;

    FLastPoint := ClientPt;
  except
    // We must not allow exceptions to escape from any of the COM methods since
    // COM doesn't support exceptions.
    FDataObject := nil;
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

procedure TCustomDropTarget.DoDragOver(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord);
begin
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, ShiftState, Point, Effect);
end;

function TCustomDropTarget.DragLeave: HResult;
begin
  try
    FScrollTimer.Enabled := False;

    // Don't mess with our stuff if DragLeave is fired while an async is in progress
    if (not AsyncTransfer) then
    begin
      ClearData;
      FDataObject := nil;
    end;

    if (DropTargetHelper <> nil) then
    begin
      DropTargetHelper.DragLeave;
    end else
      if (FDragImageHandle <> 0) then
        ImageList_DragLeave(FTarget.Handle);

    // Generate an OnLeave event.
    // Protect resources against exceptions in event handler.
    try
      DoLeave;
    finally
      FTarget := nil;
      FDropTargetHelper := nil;
    end;

    Result := S_OK;
  except
    // We must not allow exceptions to escape from any of the COM methods since
    // COM doesn't support exceptions.
    FDataObject := nil;
    Result := E_UNEXPECTED;
  end;
end;

procedure TCustomDropTarget.DoLeave;
begin
  if Assigned(FOnLeave) then
    FOnLeave(Self);
end;

function TCustomDropTarget.Drop(const dataObj: IDataObject; grfKeyState: LongWord;
  pt: TPoint; var dwEffect: LongWord): HResult;
var
  ShiftState: TShiftState;
  ClientPt: TPoint;
begin
  FScrollTimer.Enabled := False;

  // Protect resources against exceptions in OnDrop event handler.
  try
    // Refuse drop if we have lost the data object somehow.
    // This can happen if the drop is rejected in one of the other IDropTarget
    // methods (e.g. DragOver).
    if (not Enabled) or (FDataObject = nil) then
    begin
      dwEffect := DROPEFFECT_NONE;
      Result := E_UNEXPECTED;
    end else
    begin

      ShiftState := KeysToShiftStatePlus(grfKeyState);

      // Create a default drop effect based on the shift state and allowed
      // drop effects (or an OnGetDropEffect event if implemented).
      if (FTarget <> nil) then
        ClientPt := FTarget.ScreenToClient(pt)
      else
        ClientPt := pt;
      dwEffect := GetValidDropEffect(ShiftState, ClientPt, dwEffect);

      // Get data from source and generate an OnDrop event unless we failed to
      // get data.
      try
        if (FGetDataOnEnter or GetData(dwEffect)) then
        begin
          if (not AsyncTransfer) then
            DoDrop(ShiftState, ClientPt, dwEffect);
        end else
          dwEffect := DROPEFFECT_NONE;
        Result := S_OK;
      except
        // We must not allow exceptions to escape from any of the COM methods since
        // COM doesn't support exceptions.
        dwEffect := DROPEFFECT_NONE;
        Result := E_UNEXPECTED;
      end;
    end;

    if (DropTargetHelper <> nil) then
      DropTargetHelper.Drop(DataObj, pt, dwEffect)
    else
      if (FDragImageHandle <> 0) and (FTarget <> nil) then
        ImageList_DragLeave(FTarget.Handle);
  finally
    // clean up!
    if (not AsyncTransfer) then
    begin
      ClearData;
      FDataObject := nil;
      FTarget := nil;
    end;
    FDropTargetHelper := nil;
  end;
end;

procedure TCustomDropTarget.DoDrop(ShiftState: TShiftState; Point: TPoint; var Effect: LongWord);
var
  DesiredEffect: LongInt;
begin
  if Assigned(FOnDrop) then
    FOnDrop(Self, ShiftState, Point, Effect);

  (*
  Optimized move (from MSDN):

  Scenario: A file is moved from the file system to a namespace extension using
  an optimized move.

  In a conventional move operation, the target makes a copy of the data and the
  source deletes the original. This procedure can be inefficient because it
  requires two copies of the data. With large objects such as databases, a
  conventional move operation might not even be practical.

  With an optimized move, the target uses its understanding of how the data is
  stored to handle the entire move operation. There is never a second copy of
  the data, and there is no need for the source to delete the original data.
  Shell data is well suited to optimized moves because the target can handle the
  entire operation using the shell API. A typical example is moving files. Once
  the target has the path of a file to be moved, it can use SHFileOperation to
  move it. There is no need for the source to delete the original file.

  Note The shell normally uses an optimized move to move files. To handle shell
  data transfer properly, your application must be capable of detecting and
  handling an optimized move.

  Optimized moves are handled in the following way:

  1) The source calls DoDragDrop with the dwEffect parameter set to
     DROPEFFECT_MOVE to indicate that the source objects can be moved.
  2) The target receives the DROPEFFECT_MOVE value through one of its
     IDropTarget methods, indicating that a move is allowed.
  3) The target either copies the object (unoptimized move) or moves the object
     (optimized move).
  4) The target then tells the source whether it needs to delete the original
     data.
     An optimized move is the default operation, with the data deleted by the
     target. To inform the source that an optimized move was performed:
     - The target sets the pdwEffect value it received through its
       IDropTarget::Drop method to some value other than DROPEFFECT_MOVE. It is
       typically set to either DROPEFFECT_NONE or DROPEFFECT_COPY. The value
       will be returned to the source by DoDragDrop.
     - The target also calls the data object's IDataObject::SetData method and
       passes it a CFSTR_PERFORMEDDROPEFFECT format identifier set to
       DROPEFFECT_NONE. This method call is necessary because some drop targets
       might not set the pdwEffect parameter of DoDragDrop properly. The
       CFSTR_PERFORMEDDROPEFFECT format is the reliable way to indicate that an
       optimized move has taken place.
     If the target did an unoptimized move, the data must be deleted by the
     source. To inform the source that an unoptimized move was performed:
     - The target sets the pdwEffect value it received through its
       IDropTarget::Drop method to DROPEFFECT_MOVE. The value will be returned
       to the source by DoDragDrop.
     - The target also calls the data object's IDataObject::SetData method and
       passes it a CFSTR_PERFORMEDDROPEFFECT format identifier set to
       DROPEFFECT_MOVE. This method call is necessary because some drop targets
       might not set the pdwEffect parameter of DoDragDrop properly. The
       CFSTR_PERFORMEDDROPEFFECT format is the reliable way to indicate that an
       unoptimized move has taken place.
  5) The source inspects the two values that can be returned by the target. If
     both are set to DROPEFFECT_MOVE, it completes the unoptimized move by
     deleting the original data. Otherwise, the target did an optimized move and
     the original data has been deleted.
  *)

  // TODO : Why isn't this code in the Drop method?
  // Report performed drop effect back to data originator.
  if (Effect <> DROPEFFECT_NONE) then
  begin
    // If the transfer was an optimized move operation (target deletes data),
    // we convert the move operation to a copy operation to prevent that the
    // source deletes the data.
    // Since an optimized move requires that we report the fact that an
    // optimized move is being performed back to the drop source, we should only
    // convert the drop effect if we succeed in setting the performed drop
    // effect on the drop source.
    if (OptimizedMove) and (Effect = DROPEFFECT_MOVE) then
      DesiredEffect := DROPEFFECT_COPY
    else
      DesiredEffect := Effect;

    if (SetPerformedDropEffect(DesiredEffect)) then
      Effect := DesiredEffect;
  end;
end;

const
  MSG_DROP = WM_USER;

type
  TDropTargetTransferThread = class(TThread)
  private
    FDropTarget: TCustomDropTarget;
    FDataObject: IDataObject;
    FEffect: LongWord;
    FDataObjectStream: pointer;
    FAsyncOperationStream: pointer;
  protected
    procedure Execute; override;
    property DataObjectStream: pointer read FDataObjectStream;
    property AsyncOperationStream: pointer read FAsyncOperationStream;
  public
    constructor Create(ADropTarget: TCustomDropTarget;
      const ADataObject: IDataObject; AEffect: Longint);
    destructor Destroy; override;
    procedure Start;
    property DropTarget: TCustomDropTarget read FDropTarget;
    property DataObject: IDataObject read FDataObject;
    property Effect: LongWord read FEffect;
  end;

constructor TDropTargetTransferThread.Create(ADropTarget: TCustomDropTarget;
  const ADataObject: IDataObject; AEffect: longInt);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FDropTarget := ADropTarget;
  OnTerminate := FDropTarget.DoEndAsyncTransfer;
  FEffect := AEffect;
  FDataObject := ADataObject;
  OleCheck(CoMarshalInterThreadInterfaceInStream(IDataObject, FDataObject,
    IStream(FDataObjectStream)));
  OleCheck(CoMarshalInterThreadInterfaceInStream(IAsyncOperation2, FDataObject,
    IStream(FAsyncOperationStream)));
end;

destructor TDropTargetTransferThread.Destroy;
begin
  FDataObjectStream := nil;
  FAsyncOperationStream := nil;
  FDataObject := nil;
  inherited Destroy;
end;

procedure TDropTargetTransferThread.Execute;
var
  Res: HResult;
  FDataObject: IDataObject;
  FAsyncOperation: IAsyncOperation2;
begin
  CoInitialize(nil);
  try
    try
      OleCheck(CoGetInterfaceAndReleaseStream(IStream(DataObjectStream),
        IDataObject, FDataObject));
      OleCheck(CoGetInterfaceAndReleaseStream(IStream(AsyncOperationStream),
        IAsyncOperation, FAsyncOperation));

      // Switch drop target to use the marshalled data object
      DropTarget.FDataObject := FDataObject;
      // Get data from the drop source
      DropTarget.DoGetData;

      // Generate an OnDrop event
      // Note that this event is executed in the context of this thread and thus
      // must adhere to the rules of thread safe use of the VCL (e.g. don't
      // update visual stuff directly).
      DropTarget.DoDrop([], Point(0,0), FEffect);

      Res := S_OK;
    except
      on E: EOleSysError do
        Res := E.ErrorCode
      else
        Res := E_UNEXPECTED;
    end;
    // Notify drop source that we are done
    FAsyncOperation.EndOperation(Res, nil, Effect);
  finally
    DropTarget.FDataObject := nil;
    FDataObject := nil;
    FAsyncOperation := nil;
    CoUninitialize;
  end;
end;

procedure TDropTargetTransferThread.Start;
begin
  Resume;
end;

procedure TCustomDropTarget.DoEndAsyncTransfer(Sender: TObject);
begin
  // Reset async transfer flag and clean up once transfer completes and...
  FDataObject := nil;
  FTarget := nil;
  FIsAsync := False; // Set this last to minimize race condition with DragLeave
  ClearData;

  // ...Fire event.
  if Assigned(FOnEndAsyncTransfer) then
    FOnEndAsyncTransfer(Self);
end;

procedure TCustomDropTarget.GetCompatibleClipboardFormats(
  const DataFormatClass: TDataFormatClass; ClipboardFormats: TClipboardFormats);
begin
  TDataFormatMap.GetClipboardFormats(DataFormatClass, ClipboardFormats, ddtTarget);
end;

function TCustomDropTarget.GetData(Effect: longInt): boolean;
var
  DoAsync: Bool;
  AsyncOperation: IAsyncOperation2;
//  h: HResult;
begin
  ClearData;

  // Determine if drop source supports (requires shell32.dll v5.0 or later) and
  // has enabled asynchronous data transfer.
  // The Windows 2000 Explorer does support IAsyncOperation, but it seems that
  // it is a bogus implementation.
(*
  h := DataObject.QueryInterface(IAsyncOperation2, AsyncOperation);
//  h := DataObject.QueryInterface(IDropSource, AsyncOperation);
  OutputDebugString(PChar(SysErrorMessage(h)));
*)
  if (not AllowAsyncTransfer) or
    (not Supports(DataObject, IAsyncOperation2, AsyncOperation)) or
    (Failed(AsyncOperation.GetAsyncMode(DoAsync))) then
    DoAsync := False;

  // Start an async data transfer.
  if (DoAsync) and
    // Notify drop source that an async data transfer is starting.
    Succeeded(AsyncOperation.StartOperation(nil)) then
  begin
    try
      // Fire event.
      if Assigned(FOnStartAsyncTransfer) then
        FOnStartAsyncTransfer(Self);
      FIsAsync := True;
      // Create the data transfer thread and launch it.
      with TDropTargetTransferThread.Create(Self, DataObject, Effect) do
        Start;

      Result := True;
    except
      // Notify drop source that async data transfer failed
      AsyncOperation.EndOperation(E_UNEXPECTED, nil, DROPEFFECT_NONE);
      Result := False;
    end;
  end else
    // Perform a normal data transfer
    Result := DoGetData;
end;

procedure TCustomDropTarget.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TWinControl) then
  begin
    if (csDesigning in ComponentState) and (AComponent = FTarget) then
      FTarget := nil;
    if (FTargets.IndexOf(TWinControl(AComponent)) <> -1) then
      Unregister(TWinControl(AComponent));
  end;
end;

type
  TWinControlProxy = class(TWinControl)
  protected
    procedure DestroyWnd; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure TWinControlProxy.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Avoid that control draws itself in case it becomes visible (e.g. if a
  // TToolBar is specified as target.
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TWinControlProxy.CreateWnd;
begin
  inherited CreateWnd;
  OleCheck(RegisterDragDrop(Parent.Handle, TCustomDropTarget(Owner)));
  Visible := False;
  Width := 0;
end;

procedure TWinControlProxy.DestroyWnd;
begin
  if (Parent.HandleAllocated) then
    RevokeDragDrop(Parent.Handle);
  // Control must be visible in order to guarantee that CreateWnd is called when
  // parent control recreates window handle.
  Visible := True;
  inherited DestroyWnd;
end;

procedure TCustomDropTarget.DoRegister(ATarget: TWinControl);
begin
  if (AutoRegister) then
  begin
    // Create a child control to monitor the target window handle.
    // The child control will perform the drop target registration for us.
    with TWinControlProxy.Create(Self) do
      Parent := ATarget;
  end else
    OleCheck(RegisterDragDrop(ATarget.Handle, Self));
end;

procedure TCustomDropTarget.Register(ATarget: TWinControl);

  function Contains(Parent, Child: TWinControl): boolean;
  var
    i: integer;
  begin
    if (Child.Parent <> Parent) then
    begin
      Result := False;
      for i := 0 to Parent.ControlCount-1 do
        if (Parent.Controls[i] is TWinControl) and
          Contains(TWinControl(Parent.Controls[i]), Child) then
        begin
          Result := True;
          break;
        end;
    end else
      Result := True;
  end;

var
  i: integer;
  Inserted: boolean;
begin
  // Don't register if the target is already registered.
  // TODO -cImprovement : Maybe we should unregister and reregister the target if it has already been registered (in case the handle has changed)...
  if (FTargets.IndexOf(ATarget) <> -1) then
    exit;

  // Unregister previous target unless MultiTarget is enabled (for backwards
  // compatibility).
  if (not FMultiTarget) and not(csLoading in ComponentState) then
    Unregister;

  if (ATarget = nil) then
    exit;

  // Insert the target in Z order, Topmost last.
  // Note: The target is added to the target list even though the drop target
  // registration may fail below. This is done because we would like
  // the target to be unregistered (RevokeDragDrop) even if we failed to
  // register it.
  Inserted := False;
  for i := FTargets.Count-1 downto 0 do
    if Contains(FTargets[i], ATarget) then
    begin
      FTargets.Insert(i+1, ATarget);
      Inserted := True;
      break;
    end;
  if (not Inserted) then
  begin
    FTargets.Add(ATarget);
    // ATarget.FreeNotification(Self);
  end;


  // If the target is a TRichEdit control, we disable the rich edit control's
  // built-in drag/drop support.
//  if (ATarget is TCustomRichEdit) then

  if (ATarget is TCustomMemo) then
    RevokeDragDrop(ATarget.Handle);

  DoRegister(ATarget);
end;

procedure TCustomDropTarget.Unregister(ATarget: TWinControl = nil);
var
  i: integer;
begin
  // Recursively unregister all targets if ATarget is nil.
  if (ATarget = nil) then
  begin
    for i := FTargets.Count-1 downto 0 do
      Unregister(FTargets[i]);
    exit;
  end;

  i := FTargets.IndexOf(ATarget);
  if (i = -1) then
    exit;

  if (ATarget = FTarget) then
    FTarget := nil;
    // raise Exception.Create(sUnregisterActiveTarget);

  DoUnregister(ATarget);
end;

procedure TCustomDropTarget.ResetScrollZone;
var
  TargetStyles: longint;
begin
  CustomScrollZone := False;

  FScrollBars := [];

  if (AutoScroll) and (FTarget <> nil) then
  begin
    // Determine if the target control has scroll bars (and which).
    TargetStyles := GetWindowLong(FTarget.Handle, GWL_STYLE);
    if (TargetStyles and WS_HSCROLL <> 0) then
      include(FScrollBars, sbHorizontal);
    if (TargetStyles and WS_VSCROLL <> 0) then
      include(FScrollBars, sbVertical);

    // The Windows UI guidelines recommends that the scroll margin be based on
    // the width/height of the scroll bars:
    // From "The Windows Interface Guidelines for Software Design", page 82:
    //   "Use twice the width of a vertical scroll bar or height of a
    //   horizontal scroll bar to determine the width of the hot zone."
    // Previous versions of these components used the height of the current
    // target control font as the scroll margin. Yet another approach would be
    // to use the DragDropScrollInset constant.
    if (FScrollBars <> []) then
    begin
      FNoScrollZone := FTarget.ClientRect;
      if (sbVertical in FScrollBars) then
        InflateRect(FNoScrollZone, 0, -2*GetSystemMetrics(SM_CYHSCROLL));
        // InflateRect(FNoScrollZone, 0, -abs(TDummyWinControl(FTarget).Font.Height));
      if (sbHorizontal in FScrollBars) then
        InflateRect(FNoScrollZone, -2*GetSystemMetrics(SM_CXHSCROLL), 0);
        // InflateRect(FNoScrollZone, -abs(TDummyWinControl(FTarget).Font.Height), 0);
    end;
  end;
end;

procedure TCustomDropTarget.DoUnregister(ATarget: TWinControl);
var
  i: integer;
begin
  FTargets.Remove(ATarget);

  if (AutoRegister) then
  begin
    // Delete target proxy.
    // The target proxy will unregister the drop target for us when it is
    // destroyed.
    for i := ATarget.ControlCount-1 downto 0 do
      if (ATarget.Controls[i] is TWinControlProxy) and
        (TWinControlProxy(ATarget.Controls[i]).Owner = Self) then
      with TWinControlProxy(ATarget.Controls[i]) do
      begin
        Parent := nil;
        Free;
        break;
      end;
  end else
    if (ATarget.HandleAllocated) then
      // Ignore failed unregistrations - nothing to do about it anyway
      RevokeDragDrop(ATarget.Handle);
end;

function TCustomDropTarget.FindTarget(p: TPoint): TWinControl;
var
  i: integer;
  r: TRect;
  Parent: TWinControl;
begin
  {
  Result := FindVCLWindow(p);
  while (Result <> nil) and (Targets.IndexOf(Result) = -1) do
  begin
    Result := Result.Parent;
  end;
  }
  // Search list in Z order. Top to bottom.
  for i := Targets.Count-1 downto 0 do
  begin
    Result := Targets[i];

    // If the control or any of its parent aren't visible, we can't drop on it.
    Parent := Result;
    while (Parent <> nil) do
    begin
      if (not Parent.Showing) then
        break;
      Parent := Parent.Parent;
    end;
    if (Parent <> nil) then
      continue;
    GetWindowRect(Result.Handle, r);
//    if PtInRect(r, p) then
      exit;
  end;
  Result := nil;


end;

function TCustomDropTarget.FindNearestTarget(p: TPoint): TWinControl;
var
  i: integer;
  r: TRect;
  pc: TPoint;
  Control: TWinControl;
  Dist, BestDist: integer;

  function Distance(r: TRect; p: TPoint): integer;
  var
    dx, dy: integer;
  begin
    if (p.x < r.Left) then
      dx := r.Left - p.x
    else if (p.x > r.Right) then
      dx := r.Right - p.x
    else
      dx := 0;
    if (p.y < r.Top) then
      dy := r.Top - p.y
    else if (p.y > r.Bottom) then
      dy := r.Bottom - p.y
    else
      dy := 0;
    Result := dx*dx + dy*dy;
  end;

begin
  Result := nil;
  BestDist := high(integer);
  for i := 0 to Targets.Count-1 do
  begin
    Control := Targets[i];
    r := Control.ClientRect;
    inc(r.Right);
    inc(r.Bottom);
    pc := Control.ScreenToClient(p);
    if (PtInRect(r, p)) then
    begin
      Result := Control;
      exit;
    end;
    Dist := Distance(r, pc);
    if (Dist < BestDist) then
    begin
      Result := Control;
      BestDist := Dist;
    end;
  end;
end;

function TCustomDropTarget.GetTarget: TWinControl;
begin
  Result := FTarget;
(*
  Disabled so that Target will be nil when there are no real targets. E.g. when
  OnDrop is being fired as a result of a PasteFromClipboard.

  if (Result = nil) and not(csDesigning in ComponentState) then
  begin
    if (FTargets.Count > 0) then
      Result := TWinControl(FTargets[0])
    else
      Result := nil;
  end;
*)
end;

resourcestring
  sRichEditWarning =
    'It is strongly recommended that you set the AutoRegister'+#13+
    'property to False when the drop target is a rich edit control.'+#13+#13+
    'Otherwise the text of the rich edit control might become'+#13+
    'invisible when the control is scrolled or modifified';
    
procedure TCustomDropTarget.SetTarget(const Value: TWinControl);
var
  Res: Word;
begin
  if (FTarget = Value) then
    exit;

  if (csDesigning in ComponentState) then
  begin
    if (Value is TCustomMemo) and (AutoRegister) then
    begin
      Res := MessageDlg(sRichEditWarning, mtWarning, [mbOK, mbIgnore, mbCancel], 0);
      case res of
        mrCancel:
          exit;
        mrOK:
          AutoRegister := False;
      end;
    end;
    FTarget := Value;
  end else
  begin
    // If MultiTarget isn't enabled, Register will automatically unregister so
    // no need to do it here.
    if (FMultiTarget) and not(csLoading in ComponentState) then
      Unregister;
    Register(Value);
  end;
end;

procedure TCustomDropTarget.SetDataObject(Value: IDataObject);
begin
  FDataObject := Value;
end;

procedure TCustomDropTarget.SetNoScrollZone(const Value: TRect);
begin
  FNoScrollZone := Value;
  FCustomScrollZone := True;
end;

procedure TCustomDropTarget.SetShowImage(Show: boolean);
begin
  FShowImage := Show;
  if (DropTargetHelper <> nil) then
    DropTargetHelper.Show(Show)
  else
    if (FDataObject <> nil) then
      ImageList_DragShowNolock(FShowImage);
end;

function TCustomDropTarget.GetValidDropEffect(ShiftState: TShiftState;
  pt: TPoint; dwEffect: LongInt): LongInt;
begin
  // The dwEffect parameter is a set of drop effects allowed by drop source.
  // We start by finding the effects that both the source and target can agree
  // on.
  Result := dwEffect AND DragTypesToDropEffect(FDragTypes);

  // Then we find the default drop effect based on the allowed drop effects and
  // the key shift state unless the user has provided an OnGetDropEffect event
  // handler. This is nescessary since we need to pass the allowed drop effects
  // on to the event handler.
  if (not Assigned(FOnGetDropEffect)) then
    Result := ShiftStateToDropEffect(ShiftState, Result, True);

  // Add scroll effect if necessary.
  if (FAutoScroll) and (FScrollBars <> []) then
  begin
    // If the cursor is inside the no-scroll zone, clear the drag scroll flag,
    // otherwise set it.
    if (PtInRect(FNoScrollZone, pt)) then
      Result := Result AND NOT integer(DROPEFFECT_SCROLL)
    else
      Result := Result OR integer(DROPEFFECT_SCROLL);
  end;

  // Finally give the user a chance to override the 'Default' behaviour with an
  // OnGetDropEffect event handler.
  if Assigned(FOnGetDropEffect) then
    FOnGetDropEffect(Self, ShiftState, pt, Result);
end;

function TCustomDropTarget.GetPreferredDropEffect: LongInt;
begin
  with TPreferredDropEffectClipboardFormat.Create do
    try
      if GetData(DataObject) then
        Result := Value
      else
        Result := DROPEFFECT_NONE;
    finally
      Free;
    end;
end;

function TCustomDropTarget.SetPasteSucceeded(Effect: LongInt): boolean;
var
  Medium: TStgMedium;
begin
  with TPasteSucceededClipboardFormat.Create do
    try
      Value := Effect;
      Result := SetData(DataObject, FormatEtc, Medium);
    finally
      Free;
    end;
end;

function TCustomDropTarget.SetPerformedDropEffect(Effect: longInt): boolean;
var
  Medium: TStgMedium;
begin
  with TPerformedDropEffectClipboardFormat.Create do
    try
      Value := Effect;
      Result := SetData(DataObject, FormatEtc, Medium);
    finally
      Free;
    end;
end;

(*
The basic procedure for a delete-on-paste operation is as follows (from MSDN):

1) The source marks the screen display of the selected data.
2) The source creates a data object. It indicates a cut operation by adding the
   CFSTR_PREFERREDDROPEFFECT format with a data value of DROPEFFECT_MOVE.
3) The source places the data object on the Clipboard using OleSetClipboard.
4) The target retrieves the data object from the Clipboard using
   OleGetClipboard.
5) The target extracts the CFSTR_PREFERREDDROPEFFECT data. If it is set to only
   DROPEFFECT_MOVE, the target can either do an optimized move or simply copy
   the data.
6) If the target does not do an optimized move, it calls the
   IDataObject::SetData method with the CFSTR_PERFORMEDDROPEFFECT format set
   to DROPEFFECT_MOVE.
7) When the paste is complete, the target calls the IDataObject::SetData method
   with the CFSTR_PASTESUCCEEDED format set to DROPEFFECT_MOVE.
8) When the source's IDataObject::SetData method is called with the
  CFSTR_PASTESUCCEEDED format set to DROPEFFECT_MOVE, it must check to see if it
  also received the CFSTR_PERFORMEDDROPEFFECT format set to DROPEFFECT_MOVE. If
  both formats are sent by the target, the source will have to delete the data.
  If only the CFSTR_PASTESUCCEEDED format is received, the source can simply
  remove the data from its display. If the transfer fails, the source updates
  the display to its original appearance.
*)
function TCustomDropTarget.PasteFromClipboard: longint;
var
  Effect: longInt;
begin
  FTarget := nil;
  // Get an IDataObject interface to the clipboard.
  // Temporarily pretend that the IDataObject has been dropped on the target.
  OleCheck(OleGetClipboard(FDataObject));
  try
    Effect := GetPreferredDropEffect;
    // Get data from the IDataObject.
    if (GetData(Effect)) then
      Result := Effect
    else
      Result := DROPEFFECT_NONE;

    DoOnPaste(Result);
  finally
    // Clean up
    FDataObject := nil;
  end;
end;

function TCustomDropTarget.CanPasteFromClipboard: boolean;
var
  ADataObject: IDataObject;
begin
  // Get an IDataObject interface to the clipboard.
  // Note: Don't use the objects FDataObject since this method can be called
  // while a drag/drop is in progress.
  OleCheck(OleGetClipboard(ADataObject));
  try
    // Determine if clipboard has anything to offer.
    Result := HasValidFormats(ADataObject);
  finally
    // Clean up
    ADataObject := nil;
  end;
end;

procedure TCustomDropTarget.DoOnPaste(var Effect: longint);
begin
  // Generate an OnDrop event
  DoDrop([], Point(0,0), longword(Effect));

  // Report performed drop effect back to data originator.
  if (Effect <> DROPEFFECT_NONE) then
    // Delete on paste:
    // We now set the CF_PASTESUCCEEDED format to indicate to the source
    // that we are using the "delete on paste" protocol and that the
    // paste has completed.
    SetPasteSucceeded(Effect);
end;

procedure TCustomDropTarget.Assign(Source: TPersistent);
begin
  if (Source is TClipboard) then
    PasteFromClipboard
  else if (Source.GetInterface(IDataObject, FDataObject)) then
  begin
    try
      // Get data from the IDataObject
      if (not GetData(DROPEFFECT_COPY)) then
        inherited Assign(Source);
    finally
      // Clean up
      FDataObject := nil;
    end;
  end else
    inherited Assign(Source);
end;

function TCustomDropTarget.SampleMouse(MousePos: TPoint; First: boolean): boolean;
var
  Tick: DWORD;
  Distance: TPoint;
  dTime: DWORD;
  Velocity: integer;
  Sample: TMouseSample;
begin
  Tick := GetTickCount;
  Result := True;
  with Sample do
  begin
    Time := Tick;
    Pos := MousePos;
  end;

  if (not First) then
  begin
    dTime := Tick-FMouseSample.Time;
    if (dTime >= DWORD(DragDropScrollVelocitySample)) then
    begin
      // Calculate velocity of mouse movement to filter out high speed mouse
      // movements over hot zone.
      Distance.X := FMouseSample.Pos.X-MousePos.X;
      Distance.Y := FMouseSample.Pos.Y-MousePos.Y;
      if (dTime <> 0) then
        Velocity := MulDiv(Distance.X*Distance.X+Distance.Y*Distance.Y, 1000*1000,
          dTime*dTime)
      else
        Velocity := 0;
      Result := (Velocity <= DragDropScrollMaxVelocity*DragDropScrollMaxVelocity);
      if (FMouseNextSample.Time = 0) then
        FMouseNextSample := Sample;
    end;
  end;

  if (First) then
  begin
    FMouseSample := Sample;
    FMouseNextSample.Time := 0;
  end else
  if (FMouseNextSample.Time <> 0) and
    (Tick-FMouseNextSample.Time >= DWORD(DragDropScrollVelocitySample)) then
  begin
    FMouseSample := FMouseNextSample;
    FMouseNextSample := Sample;
  end;
end;

procedure TCustomDropTarget.DoAutoScroll(Sender: TObject);
var
  Scroll: TScrollDirections;
  Interval: integer;
  si: TScrollInfo;
  Pt: TPoint;
begin
  // Disable timer until we are ready to auto-repeat the scroll.
  // If no scroll is performed, the scroll stops here.
  FScrollTimer.Enabled := False;;

  Interval := DragDropScrollInterval;
  Scroll := [];

  // Only scroll if the pointer is outside the non-scroll area (i.e. inside the
  // auto-scroll zone).
  if (not PtInRect(FNoScrollZone, FLastPoint)) then
  begin
    GetCursorPos(Pt);
    // Only scroll if mouse velocity is below limit.
    if (SampleMouse(Pt)) then
      with FLastPoint do
      begin
        // Don't scroll unless scroll bars are visible.
        if (sbVertical in FScrollBars) then
        begin
          // Get scroll info to check if we *need* to scroll vertically.
          FillChar(si, SizeOf(si), 0);
          si.cbSize := SizeOf(si);
          si.fMask  := SIF_PAGE or SIF_POS or SIF_RANGE;
          GetScrollInfo(FTarget.Handle, SB_VERT, si);
          // Determine which way to scroll.
          if (Y < FNoScrollZone.Top) then
          begin
            if (si.nPos > si.nMin) then
              include(Scroll, sdUp)
          end else
          if (Y > FNoScrollZone.Bottom) then
          begin
            if (si.nPos + integer(si.nPage) <= si.nMax) then
              include(Scroll, sdDown);
          end;
        end;

        if (sbHorizontal in FScrollBars) then
        begin
          // Get scroll info to check if we *need* to scroll horizontally.
          FillChar(si, SizeOf(si), 0);
          si.cbSize := SizeOf(si);
          si.fMask  := SIF_POS or SIF_RANGE;
          GetScrollInfo(FTarget.Handle, SB_HORZ, si);
          if (X < FNoScrollZone.Left) then
          begin
            if (si.nPos > si.nMin) then
              include(Scroll, sdLeft)
          end else
          if (X > FNoScrollZone.Right) then
          begin
            if (si.nPos + integer(si.nPage) <= si.nMax) then
              include(Scroll, sdRight);
          end;
        end;
      end;
  end;

  if (Scroll <> []) then
    DoScroll(FLastPoint, Scroll, Interval);

  // Note: Once the OnScroll event has been fired and the user has had a
  // chance of overriding the auto-scroll logic, we should *only* use the Scroll
  // variable to determine if and how to scroll. Do not use FScrollBars past
  // this point.

  // Only scroll if the pointer is inside the auto-scroll zone.
  if (Scroll <> []) then
  begin

    // Remove drag image before scrolling.
    if (DropTargetHelper <> nil) then
      DropTargetHelper.Show(False)
    else
    if (FDragImageHandle <> 0) then
      ImageList_DragLeave(FTarget.Handle);
    try

      if (sdUp in Scroll) then
        FTarget.Perform(WM_VSCROLL,SB_LINEUP, 0)
      else if (sdDown in Scroll) then
        FTarget.Perform(WM_VSCROLL,SB_LINEDOWN, 0);

      if (sdLeft in Scroll) then
        FTarget.Perform(WM_HSCROLL,SB_LINEUP, 0)
      else if (sdRight in Scroll) then
        FTarget.Perform(WM_HSCROLL,SB_LINEDOWN, 0);

    finally
      // Restore drag image
      if (DropTargetHelper <> nil) then
        DropTargetHelper.Show(True)
      else
      if (FDragImageHandle <> 0) then
        with ClientPtToWindowPt(FTarget.Handle, FLastPoint) do
          ImageList_DragEnter(FTarget.Handle, x, y);
    end;

    // Reset scroll timer interval once timer has fired once.
    FScrollTimer.Interval := Interval;
    FScrollTimer.Enabled := True;
  end;
end;

procedure TCustomDropTarget.DoScroll(Point: TPoint;
  var Scroll: TScrollDirections; var Interval: integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, FLastPoint, Scroll, Interval);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropDummy
//
////////////////////////////////////////////////////////////////////////////////
function TDropDummy.HasValidFormats(const ADataObject: IDataObject): boolean;
begin
  Result := False;
end;

procedure TDropDummy.ClearData;
begin
  // Abstract method override - doesn't do anything as you can see.
end;

function TDropDummy.DoGetData: boolean;
begin
  Result := False;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomDropMultiTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomDropMultiTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragTypes := [dtLink, dtCopy];
  GetDataOnEnter := False;
  FDataFormats := TDataFormats.Create;
end;

destructor TCustomDropMultiTarget.Destroy;
var
  i: integer;
begin
  // Delete all target formats owned by the object.
  for i := FDataFormats.Count-1 downto 0 do
    FDataFormats[i].Free;
  FDataFormats.Free;
  inherited Destroy;
end;

function TCustomDropMultiTarget.HasValidFormats(const ADataObject: IDataObject): boolean;
var
  GetNum, GotNum: longInt;
  FormatEnumerator: IEnumFormatEtc;
  i: integer;
  SourceFormatEtc: TFormatEtc;
begin
  Result := False;

  if (ADataObject.EnumFormatEtc(DATADIR_GET, FormatEnumerator) <> S_OK) or
    (FormatEnumerator.Reset <> S_OK) then
    exit;

  GetNum := 1; // Get one format at a time.

  // Enumerate all data formats offered by the drop source.
  // Note: Depends on order of evaluation.
  while (not Result) and
    (FormatEnumerator.Next(GetNum, SourceFormatEtc, PULONG(@GotNum)) = S_OK) and
    (GetNum = GotNum) do
  begin
    // Determine if any of the associated clipboard formats can
    // read the current data format.
    for i := 0 to FDataFormats.Count-1 do
      if (FDataFormats[i].AcceptFormat(SourceFormatEtc)) and
        (FDataFormats[i].HasValidFormats(ADataObject)) then
      begin
        Result := True;
        DoAcceptFormat(FDataFormats[i], Result);
        if (Result) then
          break;
      end;
  end;
end;

procedure TCustomDropMultiTarget.ClearData;
var
  i: integer;
begin
  if (AsyncTransfer) then
    raise Exception.Create(sTargetAsyncBusy);
  for i := 0 to DataFormats.Count-1 do
    DataFormats[i].Clear;
end;

function TCustomDropMultiTarget.DoGetData: boolean;
var
  i: integer;
  Accept: boolean;
begin
  Result := False;

  // Get data for all target formats
  for i := 0 to DataFormats.Count-1 do
  begin
    // This isn't strictly nescessary and adds overhead, but it reduces
    // unnescessary calls to DoAcceptData (format is asked if it can accept data
    // even though no data is available to the format).
    if not(FDataFormats[i].HasValidFormats(DataObject)) then
      continue;

    // Only get data from accepted formats.
    // TDropComboTarget uses the DoAcceptFormat method to filter formats and to
    // allow the user to disable formats via an event.
    Accept := True;
    DoAcceptFormat(DataFormats[i], Accept);
    if (not Accept) then
      Continue;

    Result := DataFormats[i].GetData(DataObject) or Result;
  end;
end;

procedure TCustomDropMultiTarget.DoAcceptFormat(const DataFormat: TCustomDataFormat;
  var Accept: boolean);
begin
  if Assigned(FOnAcceptFormat) then
    FOnAcceptFormat(Self, DataFormat, Accept);
end;


end.

