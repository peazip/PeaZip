unit Unitdragdropfilesdll;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DragDropFile, DragDrop;

type

  { TFormdragdropfilesdll }

  TFormdragdropfilesdll = class(TForm)
    DropFileSource1: TDropFileSource;
    procedure DropFileSource1Drop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);

  private

  public

  end;

var
  Formdragdropfilesdll: TFormdragdropfilesdll;
  dragproc:integer;
  vdragpath,vdragpath2,vstatus:ansistring;

implementation

{$R *.lfm}

{ TFormdragdropfilesdll }

procedure TFormdragdropfilesdll.DropFileSource1Drop(Sender: TObject; DragType: TDragType;
  var ContinueDrop: Boolean);
var
  s:ansistring;
begin
if dragproc=2 then
   begin
   vdragpath2:=vdragpath;
   {1) Report when the mouse button is released from a drag&drop operation (should
   run in main application's thread), main application should be listening on a
   separate thread}
   vstatus:='.preparedrop';
   {2) Wait until main application reports the content is ready for drag&drop operation
   in the intended source\ subfolder.
   Please note that if preparing the content from the main app involves the GUI,
   it should be done in Synchronize thread sections}
   while vstatus<>'.finalizedrop' do
      begin
      application.ProcessMessages;
      sleep(50);
      end;
   {3) Verify if the main application has changed the drop source path on the fly,
   and update dodropvfiles source accordingly if needed, before finalyzing drag&drop
   operation}
   if vdragpath2<>vdragpath then
      begin
      DropFileSource1.Files.Clear;
      s:=vdragpath2+'source\*';
      DropFileSource1.Files.Add(s);
      end;
   vstatus:='.enddrop';
   end;
end;

end.

