unit Unitdragdropfilesdll;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DragDropFile, DragDrop;

type Tfound = array of ansistring;

type

  { TFormdragdropfilesdll }

  TFormdragdropfilesdll = class(TForm)
    DropFileSource1: TDropFileSource;
    procedure DropFileSource1Drop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);

  private

  public

  end;

function dList(dir, mask: ansistring; var flist: Tfound): integer;

var
  Formdragdropfilesdll: TFormdragdropfilesdll;
  dragproc:integer;
  vdragpath,vdragpath2,vstatus:ansistring;

implementation

{$R *.lfm}

{ TFormdragdropfilesdll }

function dList(dir, mask: ansistring; var flist: Tfound): integer;
var
  r: TSearchRec;
begin
  result := 0;
  if FindFirst(dir + mask, faAnyFile, r) = 0 then
  begin
    try
        repeat
          if ((r.Name <> '.') and (r.Name <> '..')) then
          begin
            SetLength(flist, length(flist) + 1);
            flist[length(flist)-1] := dir + (r.Name);
          end;
        until findnext(r) <> 0;
    except
      FindClose(r);
      result := -1;
      exit;
    end;
    FindClose(r);
  end;
  if result = 0 then
    result := 1;
end;

procedure TFormdragdropfilesdll.DropFileSource1Drop(Sender: TObject; DragType: TDragType;
  var ContinueDrop: Boolean);
var
  s:ansistring;
  flist:Tfound;
  i,k:integer;
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
   if vdragpath2='' then
      begin
      DropFileSource1.Files.Clear;
      vstatus:='.enddrop';
      exit;
      end;
   DropFileSource1.Files.Clear;
   s:=vdragpath2+'source\';
   dlist(s,'*',flist);
   k:=length(flist);
   for i:=0 to k-1 do
      Formdragdropfilesdll.DropFileSource1.Files.Add(flist[i]);
   vstatus:='.enddrop';
   end;
end;

end.

