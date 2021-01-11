unit fio;

{File IO with $I- and Err variable, (without try/finally)}

interface

{$i std.inc}

{$i-}

(*************************************************************************

 DESCRIPTION     :  File IO with $I- and Err variable, (without try/finally).
                    Simple wrapper calls for system routines

 REQUIREMENTS    :  TP5-7, D1-D7/9, FPC, VP, TPW1.5,BCB3/4

 EXTERNAL DATA   :  system.FileMode

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.04  W.Ehrhardt  Initial version
 0.11     26.11.04  we          fio_FileExists
 0.12     12.04.06  we          Special filemode for VP/RdOnly,
 0.13     12.04.06  we          D1,BP7Win,TPW1.5,BCB3/4
 0.14     12.04.06  we          Word type Err
 0.15     12.04.06  we          No fio_seek error if seek position > file size
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2004-2006 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

{todo: fio_FilesizeEx for >2GB ifdef win32}

{$ifdef win32}
  uses windows;
{$else}
  {$ifdef WINCRT}
    {$ifdef Ver80}
      uses sysutils;
    {$else}
      uses windos;
    {$endif}
  {$else}
    uses dos;
  {$endif}
{$endif}


{$ifdef win32}
function  fio_FileExists(const FileName: AnsiString): Boolean;
  {-Test if file Fname exists}
{$else}
function  fio_FileExists({$ifdef CONST} const {$endif} FName: string): boolean;
  {-Test if file Fname exists}
{$endif}

procedure fio_Reset(var F: file; recsize: word; RdOnly: boolean; var Err: word);
  {-Reset a file, with temp. FileMode=0 if RdOnly=true, returns Err code}

procedure fio_Rewrite(var F: file; recsize: word; var Err: word);
  {-Rewrite a file, returns Err code}

function  fio_Filesize(var F: file; var Err: word): longint;
  {-Returns size of a file < 2GB, returns Err code}

procedure fio_Seek(var F: file; FP: longint; var Err: word);
  {-Positions filepointer to FP, returns Err code}

function  fio_Filepos(var F: file; var Err: word): longint;
  {-Returns current filepointer position, returns Err code}

procedure fio_Blockwrite(var F: file; var data; count: word; var Err: word);
  {-Write a block of data, returns Err code}

procedure fio_Blockread(var F: file; var data; count: word; var Err: word);
  {-Read a block of data, returns Err code}

procedure fio_Close(var F: file; var Err: word);
  {-Close a file, returns Err code}


implementation



{$ifdef win32}
{--------------------------------------------------------------------------}
function fio_FileExists(const FileName: AnsiString): Boolean;
  {-Test if file Fname exists}
var
  Code: integer;
begin
  Code := GetFileAttributes(PChar(FileName));
  fio_FileExists := (Code<>-1) and ((Code and FILE_ATTRIBUTE_DIRECTORY)=0);
end;
{$else}
{$ifdef WINCRT}
  {$ifdef Ver80}
    function  fio_FileExists(const FName: string): boolean;
      {-Test if file Fname exists}
    begin
      fio_FileExists := FileExists(FName)
    end;
  {$else}
    function  fio_FileExists({$ifdef CONST} const {$endif} FName: string): boolean;
      {-Test if file Fname exists}
    const
      VolDir = faVolumeId or faDirectory;
    var
      Attr: word;
      f: file;
    begin
      if FName='' then fio_FileExists := false
      else begin
        assign(f,FName);
        GetFAttr(f, Attr);
        fio_FileExists := (DosError=0) and ((Attr and VolDir)=0);
      end;
    end;
  {$endif}
{$else}
  {--------------------------------------------------------------------------}
  function  fio_FileExists({$ifdef CONST} const {$endif} FName: string): boolean;
    {-Test if file Fname exists}
  const
    VolDir = VolumeId or Directory;
  var
    Attr: word;
    f: file;
  begin
    if FName='' then fio_FileExists := false
    else begin
      assign(f,FName);
      GetFAttr(f, Attr);
      fio_FileExists := (DosError=0) and ((Attr and VolDir)=0);
    end;
  end;
{$endif}
{$endif}

{--------------------------------------------------------------------------}
procedure fio_Reset(var F: file; recsize: word; RdOnly: boolean; var Err: word);
  {-Reset a file, with temp. FileMode=0 if RdOnly=true, returns Err code}
var
  fm: byte;
  {$ifdef VirtualPascal}
  const
    FMRead = $40; {open_access_ReadOnly or open_share_DenyNone;}
  {$else}
  const
    FMRead = 0;
  {$endif}
begin
  fm := FileMode;
  if RdOnly then FileMode := FMRead;
  reset(F, recsize);
  Err := IOResult;
  FileMode := fm;
end;



{--------------------------------------------------------------------------}
procedure fio_Rewrite(var F: file; recsize: word; var Err: word);
  {-Rewrite a file, returns Err code}
begin
  rewrite(F, recsize);
  Err := IOResult;
end;



{--------------------------------------------------------------------------}
function  fio_Filesize(var F: file; var Err: word): longint;
  {-Returns size of a file < 2GB, returns Err code}
begin
  fio_Filesize := FileSize(F);
  Err := IOResult;
end;

(*
  {$ifdef D4Plus}
    FSC: _Large_Integer;   //file size from GetFileSize
  {$else}
    FSC: TLargeInteger;    //file size from GetFileSize
  {$endif}

      FSC.LowPart := GetFileSize(PHandle(@f)^,@FSC.HighPart);
      if (FSC.LowPart=$FFFFFFFF) and (GetLastError<>NO_Error) then FSC.QuadPart := 0;

*)

{--------------------------------------------------------------------------}
procedure fio_Seek(var F: file; FP: longint; var Err: word);
  {-Positions filepointer to FP, returns Err code}
begin
  seek(F, FP);
  Err := IOResult;
end;


{--------------------------------------------------------------------------}
function  fio_Filepos(var F: file; var Err: word): longint;
  {-Returns current filepointer position, returns Err code}
begin
  fio_Filepos := FilePos(F);
  Err := IOResult;
end;



{--------------------------------------------------------------------------}
procedure fio_Blockwrite(var F: file; var data; count: word; var Err: word);
  {-Write a block of data, returns Err code}
begin
  blockwrite(F, data, count);
  Err := IOResult;
end;


{--------------------------------------------------------------------------}
procedure fio_Blockread(var F: file; var data; count: word; var Err: word);
  {-Read a block of data, returns Err code}
begin
  blockread(F, data, count);
  Err := IOResult;
end;


{--------------------------------------------------------------------------}
procedure fio_Close(var F: file; var Err: word);
  {-Close a file, returns Err code}
begin
  close(F);
  Err := IOResult;
end;




end.
