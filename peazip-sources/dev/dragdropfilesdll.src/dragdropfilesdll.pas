library dragdropfilesdll;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop files library
// Description:     Drag and Drop files from application to Windows system
//                  either using as source already existing files, or waiting
//                  for files being created
// Authors:         Giorgio Tani
//
// Based on the work of Angus Johnson & Anders Melander (Delphi),
// Michael Köcher / six (Lazarus), works compiled with Lazarus for
// Win32 i386, and compiled with Lazarus for Win64 x86_64
//
// -----------------------------------------------------------------------------

{
Version  Date      Author      Modification
-------  --------  -------     ------------------------------------------
DRAGDROPFILESDLL
0.10     20190501  G.Tani      Initial version, implement dodropfiles and dodropvfiles
0.11     20190515  G.Tani      Drop source path for dodropvfiles can be changed on the fly calling changevpath before finalyzing drag&drop operation
                               Status of drag and drop process is now communicated through Unitdragdropfilesdll.vstatus
0.12     20210202  G.Tani      dodropvfiles can be stopped calling changevpath('') which resets virtual output path
                               dodropvfiles expands the list of files and folders in source's path at the end of DropFileSource1Drop (synced by semaphore with main app to allow extraction to take place) in order to be compatible with most of drag and drop implementations in third parts apps
}

{$mode objfpc}{$H+}

uses
  DragDrop, DragDropFile, Unitdragdropfilesdll, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, Interfaces;

procedure dodropfiles(winc:TWinControl; sarr: array of ansistring; dropmode:integer);
{Drop files from app to system, source are files already existing on the filesystem
1) When drag&drop event is detected from the winc control (e.g. a listview), drop
the files/folders listed in sarr array of ansystring (containing full names) which
must exists when the drag&drop operation takes places.
Type of allowed drag&drop operations is encoded in dropmode variable}
var
  i,k:integer;
begin
unitdragdropfilesdll.dragproc:=1;
unitdragdropfilesdll.vdragpath:='';
case dropmode of
   0: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtMove];
   1: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtCopy];
   2: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtCopy,dtMove];
   3: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink];
   4: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtMove];
   5: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtCopy];
   6: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtCopy,dtMove];
   end;
k:=length(sarr);
if (k >= 0) and (DragDetectPlus(winc)) then
   begin
   Formdragdropfilesdll.DropFileSource1.Files.Clear;
   for i:=0 to k-1 do
      Formdragdropfilesdll.DropFileSource1.Files.Add(sarr[i]);
   Formdragdropfilesdll.DropFileSource1.Execute;
   end;
end;

procedure dodropvfiles(winc:TWinControl; vpath: ansistring; dropmode:integer);
{Delayed drop files from app to system, from a virtual source (wait for files being
created)
1) Unitdragdropfilesdll.vstatus is set to '.preparedrop' at ondrop event to report
to the main application that a drag&drop event is taking place, main application
should be listening using readvstatus procedure.
2) The main application should then prepare the content in vpath\source\ subfolder
and report to the dll when it is ready, setting Unitdragdropfilesdll.vstatus to
'.finalizedrop' using setvstatus procedure - which the dll waits for.
3) Before completing the ondrop event the dll reads if the main application changed
the vpath (which can ve done using changevpath procedure in main app), always updates
drop source expanding (non recursively) list of files and folder in the source path,
and finally completes the drag & drop.
NOTE: removing vpath folder(s) is let to the main application}
var
   s:ansistring;
begin
unitdragdropfilesdll.dragproc:=2;
unitdragdropfilesdll.vdragpath:=vpath;
case dropmode of
   0: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtMove];
   1: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtCopy];
   2: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtCopy,dtMove];
   3: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink];
   4: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtMove];
   5: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtCopy];
   6: Formdragdropfilesdll.DropFileSource1.DragTypes:=[dtLink,dtCopy,dtMove];
   end;
if DragDetectPlus(winc) then
   begin
   Formdragdropfilesdll.DropFileSource1.Files.Clear;
   s:=vpath+'source\';
   Formdragdropfilesdll.DropFileSource1.Files.Add(s);
   Formdragdropfilesdll.DropFileSource1.Execute;
   end;
end;

procedure changevpath(vpath2: ansistring);
begin
Unitdragdropfilesdll.vdragpath2:=vpath2;
end;

procedure readvstatus(var vstat: ansistring);
begin
vstat:=Unitdragdropfilesdll.vstatus;
end;

procedure setvstatus(vstat: ansistring);
begin
Unitdragdropfilesdll.vstatus:=vstat;
end;

exports
 dodropfiles;

exports
 dodropvfiles;

exports
 changevpath;

exports
 readvstatus;

exports
 setvstatus;

begin
  Application.Initialize;
  Application.CreateForm(TFormdragdropfilesdll, Formdragdropfilesdll);
end.

