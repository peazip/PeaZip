unit DragDropContext;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropContext
// Description:     Implements Context Menu Handler Shell Extensions.
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
  Windows,
  Graphics,
  DragDrop,
  DragDropComObj,
  Menus,
  ShlObj,
  ActiveX,
  Messages,
  Classes;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TDropContextMenu
//
////////////////////////////////////////////////////////////////////////////////
//
// A typical shell context menu handler session goes like this:
//
// 1. User selects one or more files and right clicks on them.
//    The files must of a file type which has a context menu handler registered.
//
// 2. The shell loads the context menu handler module.
//
// 3. The shell instantiates the registered context menu handler object as an
//    in-process COM server.
//
// 4. The IShellExtInit.Initialize method is called with a data object which
//    contains the dragged data.
//
// 5. The IContextMenu.QueryContextMenu method is called to populate the popup
//    menu. This fires the TDragDropHandler.OnPrepareMenu event.
//    TDropContextMenu uses the PopupMenu property to populate the shell context
//    menu.
//
// 6. If the user chooses one of the context menu items we have supplied,
//    the IContextMenu.InvokeCommand method is called.
//
// 7. TDropContextMenu locates the corresponding TMenuItem and fires the menu
//    items OnClick event.
//
// 8. The shell unloads the context menu handler module (usually after a few
//    seconds if you're lucky).
//
////////////////////////////////////////////////////////////////////////////////
  TPrepareContextMenuEvent = procedure(Sender: TObject; var Continue: boolean) of object;

  TDropContextMenu = class(TInterfacedComponent, IShellExtInit, IContextMenu,
    IContextMenu2, IContextMenu3)
  private
    FContextMenu: TPopupMenu;
    FMenuOffset, FLastMenuID: DWORD;
    FDataObject: IDataObject;
    FOnPopup: TNotifyEvent;
    FFiles: TStrings;
    FMenuHandle: HMenu;
    FFolderPIDL: PItemIDList;
    FOnPrepareMenu: TPrepareContextMenuEvent;

  protected
    procedure SetContextMenu(const Value: TPopupMenu);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetMenuItem(Index: integer): TMenuItem;
    procedure DrawMenuItem(var DrawItemStruct: TDrawItemStruct);
    procedure MeasureItem(var MeasureItemStruct: TMeasureItemStruct);
    function ProcessMenuChar(Menu: HMenu; Shortcut: Char): integer;
    procedure SetFolder(Folder: PItemIDList);
    function GetFolder: string;
    function GetSupportsOwnerDraw: boolean; virtual;
    function HasGlyph(Item: TMenuItem): boolean;
    property SupportsOwnerDraw: boolean read GetSupportsOwnerDraw;
    property MenuHandle: HMenu read FMenuHandle;

    { IShellExtInit }
    function Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;

    { IContextMenu }
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast,
      uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;

    function GetCommandString({$IFDEF win64}idcmd:UINT_Ptr;{$ELSE}idcmd: UINT;{$ENDIF} uType:UINT; pwreserved:puint;
      pszName:LPStr;cchMax:uint):HResult;StdCall;

    { IContextMenu2 }
    {$IFDEF win64}
      function HandleMenuMsg(uMsg: UINT; wParam: WPARAM; lParam: WPARAM):HResult; stdCall;
    {$ELSE}
      function HandleMenuMsg(uMsg: UINT; WParam, LParam: Integer): HResult; stdcall;
    {$ENDIF}




    { IContextMenu3 }
//    function HandleMenuMsg2(uMsg: UINT; wParam, lParam: Integer;  lpResult: Integer): HResult; stdcall;
    function HandleMenuMsg2(uMsg:UINT; wParam:WPARAM; lParam:WPARAM; presult:PLRESULT):HResult;StdCall;


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataObject: IDataObject read FDataObject;
    property Files: TStrings read FFiles;
    property FolderPIDL: PItemIDList read FFolderPIDL;
    property Folder: string read GetFolder;

  published
    property ContextMenu: TPopupMenu read FContextMenu write SetContextMenu;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnPrepareMenu: TPrepareContextMenuEvent read FOnPrepareMenu write FOnPrepareMenu;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropContextMenuFactory
//
////////////////////////////////////////////////////////////////////////////////
// COM Class factory for TDropContextMenu.
////////////////////////////////////////////////////////////////////////////////
  TDropContextMenuFactory = class(TShellExtFactory)
  protected
    function HandlerRegSubKey: string; virtual;
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//                      IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  Dialogs,
  DragDropFile,
  DragDropPIDL,
  Registry,
  ComObj,
  SysUtils,
  ImgList,
  Controls, // TControlCanvas
  Forms; // Screen


////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////
function IsLine(Item: TMenuItem): boolean;
begin
  Result := Item.IsLine;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropContextMenu
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropContextMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFiles := TStringList.Create;
end;

destructor TDropContextMenu.Destroy;
begin
  SetFolder(nil);

  FFiles.Free;
  inherited Destroy;
end;

function TDropContextMenu.GetCommandString({$IFDEF win64}idcmd:UINT_Ptr;{$ELSE}idcmd: UINT;{$ENDIF} uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
var
  ItemIndex: integer;
  MenuItem: TMenuItem;
  sAnsi: AnsiString;
begin
{$ifopt D+}
  OutputDebugString('IContextMenu.GetCommandString');
{$endif}

  ItemIndex := integer(idCmd);
  MenuItem := GetMenuItem(ItemIndex);

  // Make sure we aren't being passed an invalid argument number
  if (MenuItem <> nil) then
  begin
    Result := S_OK;
    case uType of
      GCS_HELPTEXTA:
        // return ANSI help string for menu item.
        begin
          sAnsi := AnsiString(MenuItem.Hint);
          StrLCopy(pszName, PAnsiChar(sAnsi), cchMax);
        end;

      GCS_HELPTEXTW:
        // return UNICODE help string for menu item.
        StringToWideChar(MenuItem.Hint, PWideChar(pszName), cchMax);

      GCS_VERBA:
        PAnsiChar(pszName)^ := AnsiChar(0);

      GCS_VERBW:
        PWideChar(pszName)^ := WideChar(0);

      GCS_VALIDATEA, GCS_VALIDATEW:
        // The GCS_VALIDATEA and GCS_VALIDATEW flags are not used by context
        // menu handlers.
        ;
    end;
  end else
    Result := E_INVALIDARG;
end;

function TDropContextMenu.GetFolder: string;
begin
  Result := GetFullPathFromPIDL(FolderPIDL);
end;

function TDropContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
var
  ItemIndex: integer;
  MenuItem: TMenuItem;
begin
{$ifopt D+}
  OutputDebugString('IContextMenu.InvokeCommand');
{$endif}

  Result := E_FAIL;

  // Make sure we are not being called by an application or with a verb.
  if (FContextMenu = nil) or (HiWord(Integer(lpici.lpVerb)) <> 0) then
    Exit;

  ItemIndex := LoWord(Integer(lpici.lpVerb));

  // Find the menu item specified by lpici.lpVerb.
  MenuItem := GetMenuItem(ItemIndex);

  if (MenuItem <> nil) then
  begin
    try
      try
        MenuItem.Click;
        Result := NOERROR;
      except
        on E: Exception do
        begin
          Windows.MessageBox(0, PChar(E.Message), 'Error',
            MB_OK or MB_ICONEXCLAMATION or MB_SYSTEMMODAL);
          Result := E_UNEXPECTED;
        end;
      end;
    finally
      FFiles.Clear;
    end;
  end else
    Result := E_INVALIDARG;
end;

function TDropContextMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst,
  idCmdLast, uFlags: UINT): HResult;

  procedure TraverseMenu(MenuItem: TMenuItem; MenuIndex: integer; var MenuID: DWORD);
  var
    i: integer;
    MenuItemInfo: TMenuItemInfo;
  begin
    if (MenuItem.Parent.Parent <> nil) then
    begin
      // Store the command ID and a reference to the TMenuItem
      FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
      MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
      MenuItemInfo.fMask := MIIM_ID or MIIM_DATA;
      MenuItemInfo.wID := MenuID;
      MenuItemInfo.dwItemData := integer(MenuItem);

      Win32Check(SetMenuItemInfo(MenuItem.Parent.Handle, MenuIndex, True, MenuItemInfo));

      inc(MenuID);
    end;

    MenuIndex := 0;

    for i := 0 to MenuItem.Count-1 do
      if (MenuItem[i].Visible) then
      begin
        TraverseMenu(MenuItem[i], MenuIndex, MenuID);
        inc(MenuIndex);
      end;
  end;

var
  i: integer;
  NextMenuID: DWORD;
  MenuItemInfo: TMenuItemInfo;
  Continue: boolean;
begin
{$ifopt D+}
  OutputDebugString('IContextMenu.QueryContextMenu');
{$endif}

  // Before the menu is processed/built we give the user a chance to customize
  // it or to disable it altogether.
  if (Assigned(FOnPrepareMenu)) then
  begin
    Continue := True;
    try
      FOnPrepareMenu(Self, Continue);
    except
      Result := E_UNEXPECTED;
      exit;
    end;
    if (not Continue) then
    begin
      Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL, 0);
      exit;
    end;
  end;

  (*
  ** MSDN states that:
  ** ----
  ** Only three of the flags that can be passed in through the uFlags parameter
  ** are relevant to context menu handlers.
  **
  ** CMF_DEFAULTONLY
  ** The user has selected the default command, usually by double-clicking the
  ** object. IContextMenu::QueryContextMenu should return control to the Shell
  ** without modifying the menu.
  **
  ** CMF_NODEFAULT
  ** No item in the menu should be the default item. The method should add its
  ** commands to the menu.
  **
  ** CMF_NORMAL
  ** The context menu will be displayed normally. The method should add its
  ** commands to the menu.
  ** ----
  **
  ** This does not mean that we should bail if they aren't present.
  ** If the context menu is invoked from the Explorer menu, then one of the
  ** three flags above will be set. If the context menu is invoked from the
  ** explorer listview, then none of them will be set.
  *)

  if (FContextMenu <> nil) and (uFlags and CMF_DEFAULTONLY = 0) then
  begin
    FMenuOffset := idCmdFirst;
    NextMenuID := idCmdFirst;
    for i := 0 to FContextMenu.Items.Count-1 do
      if (FContextMenu.Items[i].Visible) then
      begin
        FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
        MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
        MenuItemInfo.fMask := MIIM_FTYPE;

        // We must set menu ID in order to work around a problem with duplicate
        // entries in Shell 4.71 Explorer file menu.
        // See MSDN: Duplicate Menu Items In the File Menu For a Shell Context
        // Menu Extension.
        // http://support.microsoft.com/default.aspx? scid=kb;en-us;214477

        // Reserve a command ID for the menu entry now in case we have to
        // process a sub menu before the item is created.
        MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_ID;
        MenuItemInfo.wID := NextMenuID;
        inc(NextMenuID);

        // Store a reference to the TMenuItem
        MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_DATA;
        MenuItemInfo.dwItemData := integer(FContextMenu.Items[i]);

        if (IsLine(FContextMenu.Items[i])) then
        begin
          MenuItemInfo.fType := MFT_SEPARATOR;
        end else
        begin
          MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_STRING;
          MenuItemInfo.dwTypeData := PChar(FContextMenu.Items[i].Caption);
          MenuItemInfo.cch := Length(FContextMenu.Items[i].Caption)+1;
          MenuItemInfo.fType := MFT_STRING;

          if (FContextMenu.Items[i].Count > 0) then
          begin
            // Item has sub menu - make the new item a popup menu
            MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_SUBMENU;
            MenuItemInfo.hSubMenu := FContextMenu.Items[i].Handle;

            TraverseMenu(FContextMenu.Items[i], 0, NextMenuID);
          end;

          // Special handling of menu bitmap for top level menu item.
          // Note: In older versions of the shell, top level popup items
          // didn't support owner draw; If MFT_OWNERDRAW were specified
          // together with MIIM_SUBMENU then the menu item would not be drawn.
          //
          // Presently there are three methods of getting a top level menu item
          // with a bitmap:
          //
          // 1) Using the MIIM_BITMAP item flag.
          //    This method has the disadvantage that it changes the apperance
          //    of *all* menu items in the menu in order to room for the menu
          //    item bitmap.
          //    One possible work around of this would be to use SetMenuInfo()
          //    API with the MNS_CHECKORBMP flag to alter the whole menu, but I
          //    don't think it's wise to alter a system menu in this way.
          //
          // 2) Using the MFT_RADIOCHECK item type.
          //    This method has the advantage that it it doesn't alter the
          //    layout of either the menu or the item. It does however have the
          //    disadvantages that the bitmap will be drawn with the dimensions
          //    of of a menu checkmark (SM_CXMENUCHECK x SM_CYMENUCHECK) and
          //    that selected item is drawn in XOR mode instead of just
          //    transparently as would normally be the case. Method #1 also has
          //    the latter characteristic.
          //    At the time of writing, the WinRar and WinZip shell extensions
          //    uses this method.
          //
          // 3) Using the MFT_OWNERDRAW item type.
          //    This method has the disadvantage that we (or rather "I") have to
          //    perform all the work ourselves, and that we (or rather "you")
          //    risk drawing the item in a way that is inconsistent with what
          //    ever style Microsoft may come up with in the future.
          //    The advantage is that we can position the bitmap and text where
          //    ever we please.
          //    At the time of writing, the TortoiseSVN shell extension use this
          //    method.
          //
          // Drag-drop handler shell extensions (e.g. the TDragDropHandler
          // component) doesn't support method #3.
          //
          // As of Vista method #1 with a 32 bit premultipled ARGB bitmap
          // appears to be the preferred method.
          //
          if (HasGlyph(FContextMenu.Items[i])) then
          begin
            if (SupportsOwnerDraw) then
            begin
              // Method #3: MFT_OWNERDRAW
              MenuItemInfo.fType := MFT_OWNERDRAW;
              (* Method #1
              MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_BITMAP;
              MenuItemInfo.hbmpItem := FContextMenu.Items[i].Bitmap.Handle;
              *)
            end else
            begin
              // Method #2: MFT_RADIOCHECK

              // If the menu item is using an imagelist we extract the relevant
              // bitmap from the image list so we can use it as a checkmark.
              if (FContextMenu.Items[i].Bitmap.Empty) then
                FContextMenu.Images.GetBitmap(FContextMenu.Items[i].ImageIndex,
                  FContextMenu.Items[i].Bitmap);

              MenuItemInfo.fType := MFT_RADIOCHECK;
              MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_CHECKMARKS;
              // TODO : Who owns these bitmap handles?
              MenuItemInfo.hbmpChecked := FContextMenu.Items[i].Bitmap.Handle;
              MenuItemInfo.hbmpUnchecked := MenuItemInfo.hbmpChecked;
            end;
          end;

          MenuItemInfo.fMask := MenuItemInfo.fMask or MIIM_STATE;

          if (not FContextMenu.Items[i].Enabled) then
            MenuItemInfo.fState := MenuItemInfo.fState or MFS_DISABLED;

          if (FContextMenu.Items[i].Checked) then
            MenuItemInfo.fState := MenuItemInfo.fState or MFS_CHECKED;
        end;

        // Add one menu item to context menu
        Win32Check(InsertMenuItem(Menu, integer(indexMenu), True, MenuItemInfo));

        inc(indexMenu);
      end;
  end else
  begin
    FMenuOffset := 0;
    NextMenuID := 0;
  end;

  FLastMenuID := NextMenuID-1;

  // Return number of menu items added - plus one (according to MSDN)
  Result := MakeResult(SEVERITY_SUCCESS, FACILITY_NULL,
    NextMenuID-FMenuOffset);
end;


{$IFDEF win64}
function TDropContextMenu.HandleMenuMsg(uMsg: UINT; wParam: WPARAM; lParam: WPARAM):HResult;
var
  lpResult: LONG_PTR;
begin
  {$ifopt D+}
    OutputDebugString('IContextMenu2.HandleMenuMsg');
  {$endif}
  Result := HandleMenuMsg2(uMsg, WParam, LParam, @lpResult);
end;

{$ELSE}
function TDropContextMenu.HandleMenuMsg(uMsg: UINT; WParam, LParam: Integer): HResult;
var
  lpResult: Integer;
begin
  {$ifopt D+}
  OutputDebugString('IContextMenu2.HandleMenuMsg');
  {$endif}
  Result := HandleMenuMsg2(uMsg, WParam, LParam, @lpResult);
end;
{$ENDIF}

function TDropContextMenu.HandleMenuMsg2(uMsg:UINT;wParam:WPARAM;lParam:WPARAM;presult:PLRESULT):HResult;

//HandleMenuMsg2(uMsg: UINT; wParam, lParam: Integer; var lpResult: Integer): HResult;
{$IFDEF win64}
var lpResult: LONG_PTR;
{$ELSE}
var lpResult: Integer;
{$ENDIF}

begin
  Result := S_OK;

  case uMsg of
    WM_INITMENUPOPUP:
      begin
{$ifopt D+}
        OutputDebugString('IContextMenu3.HandleMenuMsg2(WM_INITMENUPOPUP)');
{$endif}
        FMenuHandle := HMENU(wParam);
      end;

    WM_MENUCHAR:
      (*
      ** We are required to process the WM_MENUCHAR message in order to make
      ** owner drawn context menus support accelerator keys.
      *)
      begin
{$ifopt D+}
        OutputDebugString('IContextMenu3.HandleMenuMsg2(WM_MENUCHAR)');
{$endif}
        // Since the internal command IDs of the menu item wrappers
        // (TMenuItem.Command) doesn't match the IDs we assigned the menu items
        // (in IContextMenu.QueryContextMenu), we can't just forward the
        // WM_MENUCHAR message to the VCL and let that sort it out:
        //   lpResult := SendMessage(PopupList.Window, uMsg, wParam, lParam);
        //
        // Instead we have to duplicate much of the functionality of
        // TMenu.ProcessMenuChar.
        ASSERT(Hiword(wParam) = MF_POPUP);
        lpResult := ProcessMenuChar(lParam, Char(LoWord(wParam)));
        pResult:=@lpResult;
      end;

    WM_DRAWITEM:
      begin
{$ifopt D+}
        OutputDebugString('IContextMenu3.HandleMenuMsg2(WM_DRAWITEM)');
{$endif}
        DrawMenuItem(PDrawItemStruct(lParam)^);
      end;

    WM_MEASUREITEM:
      begin
{$ifopt D+}
        OutputDebugString('IContextMenu3.HandleMenuMsg2(WM_MEASUREITEM)');
{$endif}
        MeasureItem(PMeasureItemStruct(lParam)^);
      end;
  else
{$ifopt D+}
    OutputDebugString(PChar(Format('IContextMenu3.HandleMenuMsg2(%d)', [uMsg])));
{$endif}
  end;
end;

function TDropContextMenu.HasGlyph(Item: TMenuItem): boolean;
begin
  Result := (not Item.Bitmap.Empty) or
    ((FContextMenu.Images <> nil) and (Item.ImageIndex <> -1));
end;

function TDropContextMenu.Initialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
begin
{$ifopt D+}
  OutputDebugString('IShellExtInit.Initialize');
{$endif}
  Result := NOERROR;

  FFiles.Clear;
  SetFolder(pidlFolder);

  // Save a reference to the source data object.
  FDataObject := lpdobj;
  try

    // Extract source file names and store them in a string list.
    // Note that not all shell objects provide us with a IDataObject (e.g. the
    // Directory\Background object).
    if (DataObject <> nil) then
      with TFileDataFormat.Create(dfdConsumer) do
        try
          if GetData(DataObject) then
            FFiles.Assign(Files);
        finally
          Free;
        end;

    if (Assigned(FOnPopup)) then
      try
        FOnPopup(Self);
      except
        Result := E_UNEXPECTED;
        exit;
      end;

  finally
    FDataObject := nil;
  end;
end;


procedure TDropContextMenu.SetContextMenu(const Value: TPopupMenu);
begin
  if (Value <> FContextMenu) then
  begin
    if (FContextMenu <> nil) then
      FContextMenu.RemoveFreeNotification(Self);
    FContextMenu := Value;
    if (Value <> nil) then
      Value.FreeNotification(Self);
  end;
end;

procedure TDropContextMenu.SetFolder(Folder: PItemIDList);
begin
  if (FFolderPIDL <> Folder) then
  begin
    if (FFolderPIDL <> nil) then
      coTaskMemFree(FFolderPIDL);

    FFolderPIDL := nil;

    if (Folder <> nil) then
      FFolderPIDL := ILClone(Folder);
  end;
end;

procedure TDropContextMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FContextMenu) then
    FContextMenu := nil;
  inherited;
end;

function TDropContextMenu.ProcessMenuChar(Menu: HMenu; Shortcut: Char): integer;

  function DoProcessMenu(MenuHandle: HMENU; Menu: TMenuItem; var Index: integer;
    var FirstItem, SelectedItem, NextItem: integer; CheckAccelerators: boolean): integer;
  var
    i: integer;
    MenuItemInfo: TMenuItemInfo;
    ItemIndex: integer;
    Hit: boolean;
  begin

    Result := 0;
    ItemIndex := 0;

    for i := 0 to Menu.Count-1 do
    begin
      if (not Menu[i].Visible) then
        Continue;

      FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
      MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
      MenuItemInfo.fMask := MIIM_ID or MIIM_STATE;
      Win32Check(GetMenuItemInfo(MenuHandle, ItemIndex, True, MenuItemInfo));


      if (not IsLine(Menu[i])) and (MenuItemInfo.fState and MF_DISABLED = 0) then
      begin
        if (CheckAccelerators) then
          Hit := (IsAccel(Ord(ShortCut), Menu[i].Caption))
        else
          Hit := (AnsiSameText(ShortCut, Copy(Menu[i].Caption, 1, 1))) and (Menu[i].ShortCut = 0);
//          Hit := (AnsiSameText(ShortCut, Copy(Menu[i].Caption, 1, 1))) and (GetHotkey(Menu[i].Caption) = '');

        if (Hit) then
        begin
          inc(Result);

          if (FirstItem = -1) then
            FirstItem := Index;

          if (MenuItemInfo.fState and MFS_HILITE <> 0) then
          begin
            // Item is already selected
            ASSERT(SelectedItem = -1);
            SelectedItem := Index;
          end else
          if (SelectedItem <> -1) then
          begin
            // Multiple items - previous one was selected
            NextItem := Index;
            // No need to continue once we have First, Selected and Next item
            exit;
          end;
        end;
      end;
      inc(Index);
      inc(ItemIndex);

      // I originally designed this method to handle cascading menus, but as it
      // turns out, the framework (IContextMenu3.HandleMenuMsg2) doesn't really
      // support or need this.
      // Result := Result+DoProcessMenu(Menu[i].Handle, Menu[i], Index, FirstItem, SelectedItem, NextItem);

      // No need to continue once we have First, Selected and Next item
      if (NextItem <> -1) then
        break;
    end;
  end;

var
  i: integer;
  FirstItem, SelectedItem, NextItem: integer;
  Item: integer;
  Count: integer;
  MenuItemInfo: TMenuItemInfo;
begin
  ASSERT(Menu = FMenuHandle);

  FirstItem := -1;
  SelectedItem := -1;
  NextItem := -1;
  i := 0;

  // Scan the menu looking for one of our menu items
  Count := GetMenuItemCount(Menu);
  i := 0;
  while (Count > 0) do
  begin
    // Look for an item with a command ID in our range
    FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
    MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_ID or MIIM_DATA;
    Win32Check(GetMenuItemInfo(Menu, i, True, MenuItemInfo));

    if (MenuItemInfo.wID >= FMenuOffset) and (MenuItemInfo.wID <= FLastMenuID) and
      (MenuItemInfo.dwItemData <> 0) then
    begin
      // Found an item.
      // Now get the TMenuItem from the menu item and scan all the siblings for a hit
      ASSERT(TMenuItem(MenuItemInfo.dwItemData) is TMenuItem);
      i := 0;
      // First look for an accelerator
      Count := DoProcessMenu(Menu, TMenuItem(MenuItemInfo.dwItemData).Parent, i,
        FirstItem, SelectedItem, NextItem, True);

      // If no accelerator was found, try to match the first char of the caption
      if (Count = 0) then
      begin
        i := 0;
        Count := DoProcessMenu(Menu, TMenuItem(MenuItemInfo.dwItemData).Parent, i,
          FirstItem, SelectedItem, NextItem, False);
      end;

      break;
    end;
    inc(i);
    dec(Count);
  end;

  if (Count = 0) then
  begin
    // No hit - pretend we didn't notice anything
    Result := MNC_IGNORE shl 16;
    // Result := MNC_CLOSE shl 16;
    exit;
  end;

  if (Count > 1) then
  begin
    if (NextItem = -1) then
      // Last item was selected - move to first item
      NextItem := FirstItem;
    // Select next item
    Item := NextItem;
    Result := MNC_SELECT;
  end else
  begin
    // Only one item - execute it
    Item := FirstItem;
    Result := MNC_EXECUTE;
  end;

  ASSERT(Item <> -1);

  Result := (Result shl 16) or Item;
end;

function TDropContextMenu.GetMenuItem(Index: integer): TMenuItem;

  function DoGetMenuItem(Menu: TMenuItem): TMenuItem;
  var
    i: integer;
  begin
    i := 0;

    Result := nil;
    while (Result = nil) and (i < Menu.Count) do
    begin
      if (Menu.Items[i].Visible) then
      begin
        if (Index = 0) then
          Result := Menu.Items[i];
        Dec(Index);
        if (Result = nil) and (Menu.Items[i].Count > 0) then
          Result := DoGetMenuItem(Menu.Items[i]);
      end;
      inc(i);
    end;
  end;

begin
  if (FContextMenu <> nil) then
    Result := DoGetMenuItem(FContextMenu.Items)
  else
    Result := nil;
end;

function TDropContextMenu.GetSupportsOwnerDraw: boolean;
begin
  Result := True;
end;

type
  TMenuItemCracker = class(TMenuItem);


procedure TDropContextMenu.DrawMenuItem(var DrawItemStruct: TDrawItemStruct);
var
  ItemIndex: integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  SaveIndex: Integer;
  Win98Plus: Boolean;
  State: TOwnerDrawState;
  r: TRect;
  Bitmap: TBitmap;
  ImageIndex: integer;
  ImageList: TCustomImageList;
begin

// todo six1
  // there is no onOwnerDraw in Lazarus TMenuItem yet!
//
//  // Make sure context is valid.
//  if (FContextMenu = nil) or (DrawItemStruct.CtlType <> ODT_MENU) then
//    Exit;
//
//  ItemIndex := integer(DrawItemStruct.itemID-FMenuOffset);
//  MenuItem := GetMenuItem(ItemIndex);
//
//  // Make sure we aren't being passed an invalid item ID.
//  if (MenuItem <> nil) then
//  begin
//    ASSERT(MenuItem = TMenuItem(DrawItemStruct.itemData));
//
//    Canvas := TControlCanvas.Create;
//    try
//      SaveIndex := SaveDC(DrawItemStruct.hDC);
//      try
//        Canvas.Handle := DrawItemStruct.hDC;
//{$ifdef VER13_PLUS}
//        Canvas.Font := Screen.MenuFont;
//{$else}
//        Canvas.Font.Handle := GetMenuFont;
//{$endif}
//
//        State := TOwnerDrawState(LongRec(DrawItemStruct.itemState).Lo);
//
//        Win98Plus := (Win32MajorVersion > 4) or
//          ((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
//
//        (*
//        ** The Following code works around two problems :
//        ** 1) The shell context menu always draws text with a horizontal offset
//        **    of 16. Since it is impossible to specify the horizontal offset of
//        **    a VCL menu item without resorting to a completely ownerdrawn item,
//        **    we have to fool the VCL into drawing the menu item at the correct
//        **    position. We do this by shifting the draw rect 8 pixels to the
//        **    right for top level menu items. When this happens...
//        ** 2) ...the menu item background will also be shifted 8 pixels to the
//        **    right. Because of this we have to draw the background manually.
//        ** 3) The VCLs drawing of selected menu bitmaps interferes with 1 & 2,
//        **    so we have to fool the VCL into believing that the menu item isn't
//        **    selected even if it is.
//        **
//        ** Normally we would just call Menus.DrawMenuItem and let it set up the
//        ** canvas for us, but because we have to disable TMenuItem's drawing of
//        ** the selected state, we must do it manually here and instead call
//        ** TMenuItem.AdvancedDrawItem.
//        **
//        ** We could get away with using Menus.DrawMenuItem for sub menu items,
//        ** but its easier to use the same code for all items. A side effect of
//        ** this is that our selected menu bitmaps will look like other shell
//        ** context menu bitmaps, instead of Delphi's button image look.
//        *)
//        if (odSelected in State) then
//        begin
//          Canvas.Brush.Color := clHighlight;
//          Canvas.Font.Color := clHighlightText;
//          Exclude(State, odSelected);
//        end else
//        if Win98Plus and (odInactive in State) then
//        begin
//          Canvas.Brush.Color := clMenu;
//          Canvas.Font.Color := clGrayText;
//        end else
//        begin
//          Canvas.Brush.Color := clMenu;
//          Canvas.Font.Color := clMenuText;
//        end;
//
//        if ((MenuItem.Parent <> nil) and (MenuItem.Parent.Parent = nil)) and
//          not((MenuItem.GetParentMenu <> nil) and
//           (MenuItem.GetParentMenu.OwnerDraw or (MenuItem.GetParentMenu.Images <> nil)) and
////{$ifdef VER13_PLUS}
////           (Assigned(MenuItem.OnAdvancedDrawItem) or Assigned(MenuItem.OnDrawItem))) then
////{$else}
//           (Assigned(MenuItem.OnDrawItem))) then
////{$endif}
//        begin
//          Canvas.FillRect(DrawItemStruct.rcItem);
//
//          // Work around: ImageList images are drawn with different rules...
//          // I'm not really sure for which versions of Delphi this is necessary.
//{$ifndef VER15_PLUS}
//          if (MenuItem.GetParentMenu.Images = nil) or (MenuItem.ImageIndex = -1) then
//            Inc(DrawItemStruct.rcItem.Left, 8);
//{$endif}
//        end;
//
//        // TODO : Unless menu item is ownerdraw we should handle the draw internally instead of relying on TMenuItem's draw code.
//{$ifdef VER13_PLUS}
//        // Because the VCL draws menu items different from standard shell menu
//        // items, we need to manually handle the drawing of the bitmap and
//        // positioning of the text relative to the bitmap.
//        // We only do this for top level items as it doesn't matter if sub menus
//        // are drawn a bit different from the rest.
//        if ((MenuItem.Parent <> nil) and (MenuItem.Parent.Parent = nil)) then
//        begin
//          Bitmap := TBitmap.Create;
//          try
//            // Get a copy of the menu item bitmap and temporarily clear the menu item bitmap
//            ImageList := MenuItem.GetParentMenu.Images;
//            ImageIndex := MenuItem.ImageIndex;
//            MenuItem.ImageIndex := -1;
//
//            if (not MenuItem.Bitmap.Empty) then
//            begin
//              Bitmap.Assign(MenuItem.Bitmap);
//              MenuItem.Bitmap.Assign(nil);
//            end;
//
//            r := DrawItemStruct.rcItem;
//            // Compensate for the various stuff the VCL makes room for (5 pixels)
//            dec(r.Left, 5);
//            // Presence of imagelist moves text even if item doesn't use the imagelist
//            if (ImageList <> nil) then
//              dec(r.Left, ImageList.Width);
//            // Draw the menu item text at the desired position (X=16)
//            inc(r.Left, 16);
//            TMenuItemCracker(MenuItem).AdvancedDrawItem(Canvas, r, State, False);
//
//            // Draw the bitmap at the desired position (X=0)
//            if (not Bitmap.Empty) then
//            begin
//              r.Left := DrawItemStruct.rcItem.Left;
//              r.Right := r.Left+16;
//              r.Top := DrawItemStruct.rcItem.Top+(DrawItemStruct.rcItem.Bottom-DrawItemStruct.rcItem.Top-16) div 2;
//              r.Bottom := r.Top+16;
//
//              Canvas.StretchDraw(r, Bitmap);
//
//              // Restore the menu item bitmap
//              MenuItem.Bitmap.Assign(Bitmap);
//            end else
//            if (ImageList <> nil) and (ImageIndex <> -1) then
//            begin
//              if (ImageList.Width > 16) or (ImageList.Height > 16) then
//              begin
//                // Glyph is too big. Extract it into a bitmap and stretchdraw
//                Bitmap.SetSize(ImageList.Width, ImageList.Height);
//                Bitmap.Canvas.Brush.Color := clFuchsia;
//                Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
//                ImageList.GetBitmap(ImageIndex, Bitmap);
//                Bitmap.Transparent := True;
//
//                r.Left := DrawItemStruct.rcItem.Left;
//                r.Right := r.Left+16;
//                r.Top := DrawItemStruct.rcItem.Top+(DrawItemStruct.rcItem.Bottom-DrawItemStruct.rcItem.Top-16) div 2;
//                r.Bottom := r.Top+16;
//                Canvas.StretchDraw(r, Bitmap);
//              end else
//                ImageList.Draw(Canvas,
//                  DrawItemStruct.rcItem.Left+(16-ImageList.Width) div 2,
//                  DrawItemStruct.rcItem.Top+
//                    (DrawItemStruct.rcItem.Bottom-DrawItemStruct.rcItem.Top-ImageList.Height) div 2,
//                  ImageIndex);
//              MenuItem.ImageIndex := ImageIndex;
//            end;
//          finally
//            Bitmap.Free;
//          end;
//        end else
//          TMenuItemCracker(MenuItem).AdvancedDrawItem(Canvas, DrawItemStruct.rcItem, State, False);
//{$else}
//        TMenuItemCracker(MenuItem).DrawItem(Canvas, DrawItemStruct.rcItem, (odSelected in State));
//{$endif}
//        // Menus.DrawMenuItem(MenuItem, Canvas, DrawItemStruct.rcItem, State);
//      finally
//        Canvas.Handle := 0;
//        RestoreDC(DrawItemStruct.hDC, SaveIndex);
//      end;
//    finally
//      Canvas.Free;
//    end;
//  end;
end;

procedure TDropContextMenu.MeasureItem(var MeasureItemStruct: TMeasureItemStruct);
var
  ItemIndex: integer;
  MenuItem: TMenuItem;
  Canvas: TCanvas;
  SaveIndex: Integer;
  DC: HDC;
begin
// todo six1
  // there is no onOwnwerdraw in Lazarus TMenuItem yet!
//  // Make sure context is valid.
//  if (FContextMenu = nil) or (MeasureItemStruct.CtlType <> ODT_MENU) then
//    Exit;
//
//  ItemIndex := integer(MeasureItemStruct.itemID-FMenuOffset);
//  MenuItem := GetMenuItem(ItemIndex);
//
//  // Make sure we aren't being passed an invalid item ID.
//  if (MenuItem <> nil) then
//  begin
//    ASSERT(MenuItem = TMenuItem(MeasureItemStruct.itemData));
//
//    DC := GetWindowDC(GetForegroundWindow);
//    try
//      Canvas := TControlCanvas.Create;
//      try
//        SaveIndex := SaveDC(DC);
//        try
//          Canvas.Handle := DC;
//{$ifdef VER13_PLUS}
//          Canvas.Font := Screen.MenuFont;
//{$else}
//          Canvas.Font.Handle := GetMenuFont;
//{$endif}
//          TMenuItemCracker(MenuItem).MeasureItem(Canvas, Integer(MeasureItemStruct.itemWidth),
//            Integer(MeasureItemStruct.itemHeight));
//        finally
//          Canvas.Handle := 0;
//          RestoreDC(DC, SaveIndex);
//        end;
//      finally
//        Canvas.Free;
//      end;
//    finally
//      ReleaseDC(GetForegroundWindow, DC);
//    end;
//
//    // Make room for sub menu arrow
//    if (MenuItem.Count > 0) then
//      inc(MeasureItemStruct.itemWidth, 16);
//  end else
//  begin
//    MeasureItemStruct.itemWidth := 150;
//    MeasureItemStruct.itemHeight := 20;
//  end;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropContextMenuFactory
//
////////////////////////////////////////////////////////////////////////////////
function TDropContextMenuFactory.HandlerRegSubKey: string;
begin
  Result := 'ContextMenuHandlers';
end;

procedure TDropContextMenuFactory.UpdateRegistry(Register: Boolean);
var
  ClassIDStr: string;
begin
  ClassIDStr := GUIDToString(ClassID);
  
  if Register then
  begin
    inherited UpdateRegistry(Register);
    CreateRegKey(FileClass+'\shellex\'+HandlerRegSubKey+'\'+ClassName, '', ClassIDStr);

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved',
            False) then
            WriteString(ClassIDStr, Description);
        finally
          Free;
        end;
  end else
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      with TRegistry.Create do
        try
          RootKey := HKEY_LOCAL_MACHINE;
          if OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions\Approved',
            False) then
            DeleteKey(ClassIDStr);
        finally
          Free;
        end;

    DeleteDefaultRegValue(FileClass+'\shellex\'+HandlerRegSubKey+'\'+ClassName);
    DeleteEmptyRegKey(FileClass+'\shellex\'+HandlerRegSubKey+'\'+ClassName, True);
    inherited UpdateRegistry(Register);
  end;
end;

end.

