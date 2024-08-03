unit DragDropHandler;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropHandler
// Description:     Implements Drop and Drop Context Menu Shell Extenxions
//                  (a.k.a. drag-and-drop handlers).
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
  DragDropComObj,
  DragDropContext,
  Menus,
  ShlObj,
  ActiveX,
  Windows,
  Classes;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandler
//
////////////////////////////////////////////////////////////////////////////////
//
// A typical drag-and-drop handler session goes like this:
//
// 1. User right-drags (drags with the right mouse button) and drops one or more
//    source files which has a registered drag-and-drop handler.
//
// 2. The shell loads the drag-and-drop handler module.
//
// 3. The shell instantiates the registered drag drop handler object as an
//    in-process COM server.
//
// 4. The IShellExtInit.Initialize method is called with the name of the target
//    folder and a data object which contains the dragged data.
//    The target folder name is stored in the TDragDropHandler.Folder
//    property as a string and in the FolderPIDL property as a PIDL.
//
// 5. The IContextMenu.QueryContextMenu method is called to populate the popup
//    menu. This fires the TDragDropHandler.OnPrepareMenu event.
//    TDragDropHandler uses the PopupMenu property to populate the drag-and-drop
//    context menu.
//
// 6. If the user chooses one of the context menu items we have supplied,
//    the IContextMenu.InvokeCommand method is called.
//
// 7. TDragDropHandler locates the corresponding TMenuItem and fires the menu
//    items OnClick event.
//
// 8. The shell unloads the context menu handler module (usually after a few
//    seconds if you're lucky).
//
////////////////////////////////////////////////////////////////////////////////
//
// Note that some version of the windows shell does not support owner draw and
// cascaded menus (IContextMenu and IContextMenu2 interfaces) for Drag Drop
// Handler shell extensions.
//
// The XP shell appears to support cascaded menus, but not owner draw.
//
////////////////////////////////////////////////////////////////////////////////
  TDragDropHandler = class(TDropContextMenu, IShellExtInit, IContextMenu,
    IContextMenu2, IContextMenu3)
  private
  protected
    function GetSupportsOwnerDraw: boolean; override;
    { IShellExtInit }
    function Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject;
      hKeyProgID: HKEY): HResult; stdcall;
  public
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
// COM Class factory for TDragDropHandler.
////////////////////////////////////////////////////////////////////////////////
  TDragDropHandlerFactory = class(TDropContextMenuFactory)
  protected
    function HandlerRegSubKey: string; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//              	IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  DragDropFile,
  DragDropPIDL,
  Registry,
  ComObj,
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandler
//
////////////////////////////////////////////////////////////////////////////////
function TDragDropHandler.GetSupportsOwnerDraw: boolean;
begin
  Result := False;
end;

function TDragDropHandler.Initialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
begin
  if (pidlFolder <> nil) then
    Result := inherited Initialize(pidlFolder, lpdobj, hKeyProgID)
  else
    Result := E_INVALIDARG;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDragDropHandlerFactory
//
////////////////////////////////////////////////////////////////////////////////
function TDragDropHandlerFactory.HandlerRegSubKey: string;
begin
  Result := 'DragDropHandlers';
end;

end.
