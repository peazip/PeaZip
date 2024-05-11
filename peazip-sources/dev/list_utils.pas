unit list_utils;

{
 DESCRIPTION     :  Unit providing UI-neutral routines related to listing,
                    counting and check accessibility of objects in a given path

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060831  G.Tani      Initial version
 0.11     20060917  G.Tani      removed *_VER; P_RELEASE constant in pea_utils
                                is used to keep track of release level;
                                changed extractdirname: if the path is a root (i.e.
                                disk name in Windows) the path becomes the dir path
                                and the dir name is blanked;
 0.12     20060925  G.Tani      fixed a bug in extractdirname showing up only
                                with Lazarus 0.9.18
 0.13     20080721  G.Tani      Enabled utf8
 0.14     20080825  G.Tani      Added srcfilesize to get file size using TSearchRec
 0.15     20090430  G.Tani      Added rCount for counting files in a path (optionally recursive)
 0.16     20090530  G.Tani      Added srcfilesize_multipart to get size of spanned archives (found on the same path of .001 file)
 0.17     20090601  G.Tani      Added rCountSize for counting number of files and total size in a path (optionally recursive)
 0.18     20091026  G.Tani      Added checksingle
 0.19     20091211  G.Tani      Fixed reporting the number of directories in rList
 0.20     20100124  G.Tani      Added nicenumber function to display a numeric string with size suffixes
 0.21     20101105  G.Tani      Added nicetime to display time in ms, or seconds with one decimal
 0.22     20110115  G.Tani      Added ShellTreeViewSetTextPath to update a specified shelltreeview object selecting the specified path, if exists
                                Added ComboBoxSetPaths to update a combobox to display up to 8 parent paths of the directory set as item 0
                                Moved from pea_utils various file manager functions and procedures not depending on the crypto library framework
                                129 extensions supported
 0.23     20110413  G.Tani      Improved handling of bz2 extensions variants
                                132 extensions supported
 0.24     20110413  G.Tani      Added support to IPA (iPhone application archive file, .zip variant)
                                133 extensions supported
 0.25     20110618  G.Tani      Added support to ZIPX, 134 extensions supported
 0.26     20110825  G.Tani      Minor fix: detection of .squashfs extension
 0.27     20110913  G.Tani      Modified speed calculation to display minutes if needed
 0.28     20111208  G.Tani      Added support to EPUB file format, 135 extensions supported
 0.29     20120115  G.Tani      Added support to OXT file format, 136 extensions supported
 0.30     20120414  G.Tani      added support to:
                                legacy OOo 1.x file formats SX* and ST* files
                                R00 rar files
                                PCV MozBackup files
                                BSZ (BS Player), RMSKIN (Rainmeter), WSZ/WAL (Winamp), and WMZ (Windows Media Player) skin files
 0.30     20120604  G.Tani      added support to IMA, IMG, and IMZ disk images, and AIR Adobe installers files (Adobe Integrated Runtime)
                                getwinenvadv for improved system version/subversion detection
 0.31     20120717  G.Tani      added testpcomp and dirsizetc for a very quick test of possible compression potential of a given file
 0.32     20121014  G.Tani      added explicit support for Open Packaging Conventions file formats:
                                MS Windows 8 App Package .appx
                                MS Windows Azure C# Package .cspkg
                                Autodesk AutoCAD .dwfx
                                Family.Show .familyx
                                Field Device .fdix
                                NuGet Package .nupkg
                                Siemens PLM Software file format .jtx
                                Open XML Paper Specification .oxps
                                MS Semblio .semblio
                                SMPTE Media Package .smpk
                                MS Visual Studio 2010 Extensions .vsix
                                Microsoft XML Paper Specification .xps
 0.33     20130602  G.Tani      Code cleanup
                                added support to Open Packaging Conventions formats .cddx and .appv (Microsoft Application Virtualization)
                                added support to .mdf (Alcohol 120 image file)
                                added support to .crx Google Chrome extension
                                added support to .maff Mozilla archive format
 0.34     20130822  G.Tani      added support for .ipsw iOS devices firmware packages
                                175 file extensions supported
 0.35     20131029  G.Tani      added support for .msu (Microsoft update) and .mpp (Microsoft Project file)
                                177 file extensions supported
 0.36     20140216  G.Tani      Added support for .iwd Infinity Ward format, 178 file extensions supported
 0.37     20140913  G.Tani      New functions
                                 fget_usrtmp_path (get user's temp path)
                                 checkfilename_acceptblank (check string valid for being used in file name, accept blank input)
                                 checksingle_intdir (check if a folder contains a single directory, i.e. to check for extraction creating intermediate dirs)
                                 checkempty_dir (check if a directory is empty)
                                 movecontent_todir (move content to one dir to other, optionally delete input dir if successful)
 0.38     20141009  G.Tani      Added support for .xzm Porteus Linux packages, 179 file extensions supported
 0.39     20141028  G.Tani      Added support for Microsoft's .mlc Language Interface Pack and .mui Multilingual User Interface, 181 file extensions supported
 0.40     20150331  G.Tani      Added full support to BCM files
 0.41     20150619  G.Tani      Added support for Unix/Linux compressed .man files
 0.42     20150923  G.Tani      Updated various functions to use current Lazarus/FPC UTF-8 compliant functions
                                Added support (total 188 extensions) for CPGZ files, GPT GUID Partition Table, QCOW2 QUEMU image, VMDK VMware Virtual Machine Disk, VDI Oracle VirtualBox Virtual Drive Image
 0.43     20170706  G.Tani      Various updates
 0.44     20190226  G.Tani      Various updates
 0.45     20190706  G.Tani      Added rLast function to find last modified time of most recent non-locked file in a directory
 0.46     20190824  G.Tani      Improved controls of rLast functions to exclude temporary work files
 0.47     20190911  G.Tani      Added .xip file type (total 189 extensions)
 0.48     20191228  G.Tani      Improved srcfilesize_multipart to find file parts of spanned archive following different naming conventions
 0.49     20200106  G.Tani      Added .br Brotli and .zst Zstd extensions (total 191 extensions supported)
                                Minor fixes
 0.50     20200301  G.Tani      Added support for Microsoft MSIX and ESD packages, 193 extensions supported
 0.51     20200509  G.Tani      Added support for Ubuntu .snap, AppImage's .appimage, and Listaller .ipk packages, 196 extensions supported
 0.52     20200524  G.Tani      Added support for .zstd extension, as used in compressed cipo in new rpm installers, 197 extemsions supported
 0.53     20200614  G.Tani      Added support for additional extensions, 210 extensions supported
                                .cramfs compressed ROM/RAM file system
                                .ext, .ext2, .ext3, and .ext4 filesystem images
                                .hfsx filesystem images
                                .qcow, and .qcow2c QEMU disk images
                                .scap, and .uefif firmware files
                                .mub, .pkg, and .ppmd compressed files
 0.54     20201230  G.Tani      Added support for .appxbundle format, 211 extensions supported
                                Improved recognition of temp and other system's paths
 0.55     20210123  G.Tani      Improved handling of special characters in passwords and filenames
 0.56     20210224  G.Tani      Improved checkfilename function
 0.57     20210509  G.Tani      Added support for .xappx, .3mf, .vsdx, .mmzx, .aasx, .slx, ad .scdoc files, 218 extensions supported
                                Reorganized extensions codes in testext function: 0..99 traditional archives 100..499 containers 500..599 containers that are usually not expected to be handled as archives 1000+archive types handled (for browsing) through separate backends
 0.58     20210629  G.Tani      Improved size detection for multipart archives
                                Added support for .whl Python packages and .gem Ruby gem packages, 220 extensions supported
                                Reorganized codes in testext function for containers: 100..199 package formats 200..499 filesystems and others
 0.59     20210727  G.Tani      Added CPU architecture, and widget set strings to info about build
 0.60     20210817  G.Tani      Added support for .lz file extension, and for .apkm, .apks, .aab packages: 225 extensions supported
 0.61     20210925  G.Tani      Nicenumber can now display various types of multiple-bytes fomats for file sizes (binary, decimanl, none)
 0.62     20220125  G.Tani      Added support for .vhdx format: 226 extensions supported
 0.63     20221209  G.Tani      Added support for .pmdx, .pmvx, .tmdx, .prdx SoftMaker Office files, 230 extensions supported
 0.64     20240228  G.Tani      Added function to check if a directory exists, checking both address with and without ending separator character

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com
The program is released under GNU LGPL http://www.gnu.org/licenses/lgpl.txt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, StrUtils,
  StdCtrls, ComCtrls, ShellCtrls, FileUtil, Process, UTF8Process;

type
  TFoundList = array of ansistring;
  //dynamic array of ansistring, each time a new file is found new space is allocated (function rList)
  TFoundListBool = array of boolean;
  TFoundListAges = array of dword;
  TFoundListAttrib = array of dword;
  TFoundListSizes = array of qword;
  TFoundListArray64 = array of array [0..63] of byte;
  TFileOfByte = file of byte;
  TCopyDataProc = procedure(oldnode, newnode : TTreenode);

const
  SUCCESS = 0;
  INCOMPLETE_FUNCTION = 1;
  LIST_ERROR = 2;
  LIST_RECURSION_ERROR = 3;
  CALL_ERROR = 4;
  DWORD_DECODE_TO_ATTRIBUTES_ERROR = 5;
  STRING_DECODE_TO_ATTRIBUTES_ERROR = 6;
  OBJECT_NOT_ACCESSIBLE = 7;

//represent a time string with suffixes
function nicetime(s: ansistring): ansistring;

//represent a numeric string with size suffixes
function nicenumber(s: ansistring; const b: integer): ansistring;

//return the name of first object found in directory, check if dir contains 1 object or more (break if contains more than 1 file, nfiles=2)
function checksingle(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var oname: ansistring
  //name of the first object found (excluding . and ..)
  ): integer;

//check if a directory contains a single intermediate dir
function checksingle_intdir(dir:ansistring; var oname: ansistring):boolean;

//check if a directory contains a single object (file or directory)
function checksingle_obj(dir:ansistring; var oname: ansistring):boolean;

//check if a directory is empty
function checkempty_dir(dir:ansistring):boolean;

//move content from d1 to d2, mode 0:keep input dir 1:delete input dir if empty
function movecontent_todir(d1,d2:ansistring; mode:integer):integer;

//optionally recursive function to count files
function rCount(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs:
  qword                                       //number of files and dirs found
  ): integer;

//optionally recursive function to count files and total size
function rCountSize(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize:
  qword                              //number of files and dirs found, total size
  ): integer;

{
if an input object is accessible, assign it as a file of byte
otherwise mark it as skipped at the given address in the status_files TFoundList
}
function check_in(var f_in: TFileOfByte;
  //file of byte to assign
  in_qualified_name: ansistring;
  //qualified name of the object to test
  var status_files: TFoundListBool;
  //TFoundListBool containing accessibility boolean value of the object to test
  addr: dword
  //address of the object to test in the TFoundList
  ): integer;

function srcfilesize(s: ansistring; var fsize: qword): integer;

function srcfilesize_multipart(s: ansistring; var fsize: qword): integer;
function srcfilesize_multipart_v(s: ansistring; var vn,fsize: qword): integer;

{
list files, dirs and size matching the given path, mask and file attributes;
optionally include main dir as first object;
optionally list object with or without recursion
}
function dirsize(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size: qword
  //total size (Byte)
  ): integer;

{
decode file attributes given attributes as a dword
}
function dword2decodedFileAttributes(d: dword;
  //input dword
  var fattrib: ansistring
  //string of file attributes abbreviations
  ): integer;

{
extract directory name and path were the directory is in (nesting level
unlimited by implementation, only memory limited, whidely exceeding needs)
}
function extractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;

function ansiextractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;

{
expand an object, given it's name, to a list of objects (TFoundList).
If the object is a file, its name is returned as sole element of the list;
otherwise if the object is a directory it's name and, recursively, all the
content, will be added to the list
}
function expand(s: ansistring;
  //input object
  var exp_files: TFoundList;
  //expanded list of objects
  var exp_fsizes: TFoundListSizes;
  //expanded list of object sizes
  var exp_ftimes: TFoundListAges;
  //expanded list of object ages
  var exp_fattr: TFoundListAttrib;
  //expanded list of object attributes
  var exp_fattr_dec: TFoundList;
  //expanded list of object decoded attributes
  var nfound: qword
  //number of objects found
  ): integer;
//take the exit code from ListFile function that it calls

{
list files and optionally dirs in a path (optionally with recursion in subdirs),
list objects details to other separate lists
}
function listdetails(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec: TFoundList
  //list of file decoded attributes
  ): integer;

//copy treeview
procedure CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil);

//update the specified treeview or shelltreeview object pointing to the specified path, if exists
function TreeViewSetTextPath(atreeview: TTreeView;
  anode: TTreeNode; Path: ansistring): integer;
function ShellTreeViewSetTextPath(ashelltreeview: TShellTreeView;
  Path: ansistring): integer;

//set parent paths as entries in a combobox
procedure ComboBoxSetPaths(acombobox: TComboBox; apath:ansistring);

//check full name against reserved and illegal characters, and reserved filenames
function checkfiledirname(s: ansistring): integer;
function checkfiledirname_acceptblank(s: ansistring): integer;

//check for some special characters in filenames in *x environments
function checkescapedoutname(s: ansistring):ansistring;

//escape filenames in *x environments likely using Gnome or KDE as desktop environment (Linux, *BSD)
function escapefilenamelinuxlike(s: ansistring; desk_env: byte): ansistring;

//cross platform filename escaping
function escapefilename(s: ansistring; desk_env: byte): ansistring;

//escape file names and apply correct quotes
function escapefilenamedelim(s: ansistring; desk_env: byte): ansistring;

//apply correct quotes (on *x like swap ' and " quotes if needed)
function stringdelim(s:ansistring): ansistring;

//remove correct quotes (on *x like swap ' and " quotes if needed)
function stringundelim(s:ansistring): ansistring;

//open Windows File Explorer selecting specified file
procedure winexplorepath(s: ansistring);

//open macOS Finder with reveal option to select specified file
procedure macexplorepath(s: ansistring);

//open files in *x environments likely using Gnome or KDE as desktop environment (Linux, *BSD)
function cp_open_linuxlike(s: ansistring; desk_env: byte): integer;

//open Gnome or KDE search interface
procedure cp_search_linuxlike(desk_env: byte);

//get correct quotes to delimit a string, swapping ' and " quotes if needed and if it is supported by the OS
function correctdelimiter(s:AnsiString): AnsiString;

//get desktop environment
procedure getdesk_env(var bytedesk: byte; var caption_build, delimiter: ansistring);

//set ending directory separator character if missing
procedure setendingdirseparator(var s:ansistring);

//get desktop path
procedure get_desktop_path(var s: ansistring);

//get home path (*x) or profile path (win)
procedure get_home_path(var s: ansistring);

//get sub paths in home (*x)
procedure get_home_subpaths(var usr_documents,usr_downloads,usr_music,usr_pictures,usr_videos: ansistring);

//get a temporary work path writeable from current user
procedure get_usrtmp_path(var s: ansistring);
function fget_usrtmp_path:ansistring;

//cut extension from filename
procedure cutextension(var s: ansistring);

//cut extension from filename and return string
function cutext(var s: ansistring):ansistring;

//check file name against reserved and illegal characters, and reserved filenames
function checkfilename(s: ansistring): integer;
function checkfilename_acceptblank(s: ansistring): integer;

//generic command string sanitization
function validatecl(var s: ansistring): integer;

//get comspec and Windows version
{$IFDEF MSWINDOWS}
procedure getwinenv(var wincomspec, winver: ansistring);
{$ENDIF}

//get comspec and Windows version (advanced)
{$IFDEF MSWINDOWS}
procedure getwinenvadv(var wincomspec, winver, majmin: ansistring);
{$ENDIF}

//assign a code to all supported filetypes
function testext(s: ansistring): integer;

//test what backend should handle a given file
function testinput(infile: ansistring; testdir: boolean): integer;

//quickly test potential compression ratio of a given file
function testpcomp(var s:ansistring):integer;

//dirsize function variant with quick compression test
function dirsizetc(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size, tcsize: qword
  //total size (Byte)
  ): integer;

function rLast(dir, mask: ansistring; //last modified time
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var ltime:longint
  ): integer;

function checkdirexists(s:ansistring):boolean;

function checkfiledirexists(s:ansistring):boolean;

implementation

function nicetime(s: ansistring): ansistring;
var
  ntime, ints, decs: qword;
  s1: ansistring;
begin
  try
    ntime := strtoqword(s);
  except
    Result := s;
    exit;
  end;
  if ntime < 1000 then
    s1 := s + ' ms'
  else
  if ntime < 60000 then
     begin
        ints := ntime div 1000;
        decs := ((ntime * 10) div 1000) - ints * 10;
       s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' s';
     end
  else
     begin
        ints := ntime div 1000 div 60;
        decs := (ntime div 1000) - (ints*60); //((ntime * 10) div 1000) - ints * 10;
        s1 := IntToStr(ints) + ' m ' + IntToStr(decs) + ' s';
     end;
  Result := s1;
end;

//represent a numeric string with size suffixes
function nicenumber(s: ansistring; const b: integer): ansistring; //b is the base: 0 binary (multiply by 1024), 1 decimal (multiply by 1000), otherwise do not convert byte size
var
  fs, ints, decs, bs: qword;
  s1: ansistring;
begin
case b of
   0:bs:=1024;
   1:bs:=1000;
   else //NO
      begin
      Result := s;
      exit;
      end;
end;
try
  fs := strtoqword(s);
  if fs < bs then
    s1 := s + ' B'
  else
  if fs < bs * bs then
  begin
    ints := fs div bs;
    decs := ((fs * 10) div bs) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' KB';
  end
  else
  if fs < bs * bs * bs then
  begin
    ints := fs div (bs * bs);
    decs := ((fs * 10) div (bs * bs)) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' MB';
  end
  else
  if fs < bs * bs * bs * bs then
  begin
    ints := fs div (bs * bs * bs);
    decs := ((fs * 10) div (bs * bs * bs)) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' GB';
  end
  else
  begin
    ints := fs div (bs * bs * bs * bs);
    decs := ((fs * 10) div (bs * bs * bs * bs)) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' TB';
  end;
  Result := s1;
except
   Result := s;
   exit;
end;
end;

//optionally recursive function used internally to list files (optionally including dirs) and object details
function rList(mode: string;
  //mode of operation, currently implemented only DETAILS: give detailed output (1+4 lists);
  dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec:
  TFoundList                                     //list of file decoded attributes
  ): integer;
var
  r: TSearchRec;
begin
  rList := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      if upcase(mode) = 'DETAILS' then
        repeat
          if ((r.Name <> '.') and (r.Name <> '..')) then
          begin
            SetLength(flist, nfiles + 1);
            SetLength(fsizes, nfiles + 1);
            SetLength(ftimes, nfiles + 1);
            SetLength(fattr, nfiles + 1);
            SetLength(fattr_dec, nfiles + 1);
            flist[nfiles] := dir + (r.Name);
            if (r.Attr and faDirectory <> 0) then
            begin
              if flist[nfiles][length(flist[nfiles])] <> DirectorySeparator then
                flist[nfiles] := flist[nfiles] + DirectorySeparator;
              Inc(ndirs, 1);
            end;
            fsizes[nfiles] := r.Size;
            ftimes[nfiles] := r.Time;
            fattr[nfiles] := r.Attr;
            dword2decodedFileAttributes(r.Attr, fattr_dec[nfiles]);
            Inc(nfiles, 1);
          end;
        until findnext(r) <> 0;
    except
      FindClose(r);
      rList := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if FindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
          begin
            rList(mode, dir + (r.Name) +
              DirectorySeparator, mask, fattrib, recur, nfiles, ndirs, flist, fsizes, ftimes, fattr, fattr_dec);
            Dec(ndirs, 1);
          end;
        until findnext(r) <> 0;
      except
        FindClose(r);
        rList := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rList = INCOMPLETE_FUNCTION then
    rList := SUCCESS;
end;

//return the name of first object found in directory, check if dir contains 1 object or more (break if contains more than 1 file, nfiles=2)
function checksingle(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var oname: ansistring
  //name of the first object found (excluding . and ..)
  ): integer;
var
  r: TSearchRec;
begin
  checksingle := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          if nfiles = 1 then
            oname := r.Name;
          if nfiles > 1 then
            break;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      checksingle := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if checksingle = INCOMPLETE_FUNCTION then
    checksingle := SUCCESS;
end;

//check if output contains a single intermediate dir
function checksingle_intdir(dir:ansistring; var oname: ansistring):boolean;
var
  mask: ansistring;
  fattrib: qword;
  nfiles, ndirs: qword;
  r: TSearchRec;
begin
result:=false;
mask:='*';
fattrib:=faAnyFile;
nfiles:=0;
ndirs:=0;
if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          if (r.Attr and faDirectory <> 0) then
             begin
             result:=true;
             oname := r.Name;
             Inc(ndirs, 1);
             end;
          if nfiles > 1 then
            begin
            result:=false;
            break;
            end;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      result:=false;
      exit;
    end;
    FindClose(r);
  end;
end;

//check if output contains a single object (file or directory)
function checksingle_obj(dir:ansistring; var oname: ansistring):boolean;
var
  mask: ansistring;
  fattrib: qword;
  nobj: qword;
  r: TSearchRec;
begin
result:=false;
mask:='*';
fattrib:=faAnyFile;
nobj:=0;
if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nobj, 1);
          result:=true;
          oname := r.Name;
          if nobj > 1 then
            begin
            result:=false;
            break;
            end;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      result:=false;
      exit;
    end;
    FindClose(r);
  end;
end;

function checkempty_dir(dir:ansistring):boolean;
var
  mask: ansistring;
  fattrib: qword;
  r: TSearchRec;
begin
result:=true; //if not found treat as empty
mask:='*';
fattrib:=faAnyFile;
if FindFirst(dir + mask, fattrib, r) = 0 then
   begin
   result:=true;
   try
      repeat
      if ((r.Name <> '.') and (r.Name <> '..')) then
         begin
         result:=false;
         break;
         end;
      until findnext(r) <> 0;
   except
      FindClose(r);
      result:=false;
      exit;
   end;
   FindClose(r);
   end;
end;

function movecontent_todir(d1,d2:ansistring; mode:integer):integer; //input, output, mode 0:keep input dir 1:delete input dir if empty
//results -1 copy error, 0 success, 1 remove input dir error
var
   r:TSearchRec;
begin
Result:=-1;
if (FindFirst(d1 + '*', faAnyFile, r) = 0) then //scan for naming conflicts
   try
      repeat
      if (r.Name <> '.') and (r.Name <> '..') then
         begin
         if FileExists(d2+r.name) then
            begin
            FindClose(r);
            exit;
            end;
         if DirectoryExists(d2+r.name) then
            begin
            FindClose(r);
            exit;
            end;
         end;
      until findnext(r) <> 0;
   except
      FindClose(r);
      exit;
   end;
FindClose(r);
if (FindFirst(d1 + '*', faAnyFile, r) = 0) then
   try
      repeat
      if (r.Name <> '.') and (r.Name <> '..') then
         if renamefile(d1+r.name, d2+r.name)=false then
            begin
            FindClose(r);
            exit;
            end;
      until findnext(r) <> 0;
   except
      FindClose(r);
      exit;
   end;
FindClose(r);
Result:=0;
if mode=1 then
   if checkempty_dir(d1)= true then
      try
      if removedir(d1)=true then else Result:=1;
      except
      Result:=1;
      end
   else Result:=1;
end;

//optionally recursive function to count files
function rCount(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs:
  qword                                      //number of files and dirs found
  ): integer;
var
  r: TSearchRec;
begin
  rCount := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      rCount := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if FindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
            rCount(dir + (r.Name) + DirectorySeparator, mask, fattrib, recur, nfiles, ndirs);
        until findnext(r) <> 0;
      except
        FindClose(r);
        rCount := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rCount = INCOMPLETE_FUNCTION then
    rCount := SUCCESS;
end;

//optionally recursive function to count files and total size
function rCountSize(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize:
  qword                                 //number of files and dirs found, total size
  ): integer;
var
  r: TSearchRec;
begin
  rCountSize := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          tsize := tsize + r.Size;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      rCountSize := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if FindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
            rCountSize(dir + (r.Name) + DirectorySeparator, mask, fattrib,
              recur, nfiles, ndirs, tsize);
        until findnext(r) <> 0;
      except
        FindClose(r);
        rCountSize := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rCountSize = INCOMPLETE_FUNCTION then
    rCountSize := SUCCESS;
end;

function check_in(var f_in: TFileOfByte;
  //file of byte to assign
  in_qualified_name: ansistring;
  //qualified name of the object to test
  var status_files: TFoundListBool;
  //TFoundListBool containing accessibility boolean value of the object to test
  addr: dword
  //address of the object to test in the TFoundList
  ): integer;
var
  in_ok: boolean;
begin
  check_in := INCOMPLETE_FUNCTION;
  status_files[addr] := False;
  try
    if filegetattr(in_qualified_name) > 0 then
      if filegetattr(in_qualified_name) and faDirectory = 0 then
      begin
        assignfile(f_in, in_qualified_name);
        filemode := 0;
        reset(f_in);
        in_ok := True;
      end
      else
        in_ok := True
    else
      in_ok := False;
  except
    in_ok := False;
  end;
  if in_ok = False then
  begin
    status_files[addr] := False;
    check_in := OBJECT_NOT_ACCESSIBLE;
  end
  else
  begin
    status_files[addr] := True;
    check_in := SUCCESS;
  end;
end;

function srcfilesize(s: ansistring; var fsize: qword): integer;
var
  r: TSearchRec;
begin
  srcfilesize := -1;
  if filegetattr(s) and faDirectory = 0 then //object is a file
  begin
    if FindFirst(s, faAnyFile, r) = 0 then
    begin
      fsize := r.Size;
      srcfilesize := 0;
    end;
    FindClose(r);
  end;
end;

function srcfilesize_multipart(s: ansistring; var fsize: qword): integer;
var
  vn:qword;
begin
result:=srcfilesize_multipart_v(s,vn,fsize);
end;

function srcfilesize_multipart_v(s: ansistring; var vn,fsize: qword): integer;
var
  s_ext, sh_ext, s_name, sh_name, s_path: ansistring;
  k, j, sh_len: integer;
  r: TSearchRec;
begin
result := -1;
fsize := 0;
vn:=0;
if filegetattr(s) and faDirectory = 0 then //object is a file
   begin
   vn:=1;
   s_ext := extractfileext(s);
   s_name := extractfilename(s);
   setlength(s_name, length(s_name) - length(s_ext));
   s_path := extractfilepath(s);
   if FindFirst(s, faAnyFile, r) = 0 then //size of a single file or part
      begin
      fsize := r.Size;
      result := 0;
      end;
   FindClose(r);
   case s_ext of
   '.001':  //multipart .xxx
      begin
      j:=1;
      repeat
         j := j + 1;
         if j < 10 then s_ext := '.00' + IntToStr(j)
         else
            if j < 100 then s_ext := '.0' + IntToStr(j)
            else s_ext := '.' + IntToStr(j);
         k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
         if k = 0 then
            fsize := fsize + r.Size;
         FindClose(r);
      until k <> 0;
      vn:=j-1;
      end;
   '.pea':
      begin
      if pos('.000001',lowercase(extractfileext(s_name)))<>0 then //multipart pea .000001.pea
         begin
         j:=0;
         sh_name:=s_name;
         setlength(sh_name, length(s_name) - 7);
         repeat
            j := j + 1;
            sh_ext:='.'+IntToStr(j).PadLeft(6,'0');
            k := FindFirst(s_path + sh_name + sh_ext + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
         vn:=j;
         end;
      end;
   '.zip':
      begin
      s_ext:='.z01';
      j:=0;
      if FindFirst(s_path + s_name + s_ext, faAnyFile, r)=0 then //multipart zip .zxx
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.z0' + IntToStr(j)
            else s_ext := '.z' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
      vn:=j;
      if vn=0 then vn:=1;
      end;
   '.zipx':
      begin
      s_ext:='.zx01';
      j:=0;
      if FindFirst(s_path + s_name + s_ext, faAnyFile, r)=0 then //multipart zipx .zxyy
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.zx0' + IntToStr(j)
            else s_ext := '.zx' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
      vn:=j;
      if vn=0 then vn:=1;
      end;
   '.rar':
      begin
      s_ext:='.r01';
      j:=0;
      if FindFirst(s_path + s_name + s_ext, faAnyFile, r)=0 then //multipart rar .rxx
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.r0' + IntToStr(j)
            else s_ext := '.r' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0
      else
         begin
         s_ext:='.rar';
         if pos('.part',lowercase(extractfileext(s_name)))<>0 then //multipart rar .partx.rar
            begin
            sh_ext:=lowercase(extractfileext(s_name));
            sh_len:=length(sh_ext)-5;
            sh_name:=s_name;
            setlength(sh_name, length(s_name) - length(sh_ext));
            repeat
               j := j + 1;
               sh_ext:='.part'+IntToStr(j).PadLeft(sh_len,'0');
               k := FindFirst(s_path + sh_name + sh_ext + s_ext, faAnyFile, r);
               if k = 0 then
                  fsize := fsize + r.Size;
               FindClose(r);
            until k <> 0;
            j:=j-1;
            end;
         end;
      vn:=j;
      if vn=0 then vn:=1;
      end;
   end;
   end;
end;

function dirsize(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size: qword
  //total size (Byte)
  ): integer;
var
  i: integer;
  dir: ansistring;
  flist: TFoundList;
  fsizes: TFoundListSizes;
  ftimes: TFoundListAges;
  fattr: TFoundListAttrib;
  fattr_dec: TFoundList;
begin
  DirSize := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  size := 0;
  try
    //check for valid path
    if length(path) = 0 then
      begin
      result := CALL_ERROR;
      exit;
      end
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir<>'' then
       if dir[length(dir)] <> DirectorySeparator then
       dir := dir + DirectorySeparator;
    DirSize := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs, flist,
      fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
    for i := 0 to length(fsizes) - 1 do
      size := size + fsizes[i];
  except
    DirSize := CALL_ERROR;
    exit;
  end;
  if DirSize = INCOMPLETE_FUNCTION then
    DirSize := SUCCESS;
end;

function dword2decodedFileAttributes(d: dword;
  //input dword
  var fattrib: ansistring
  //string of file attributes abbreviations
  ): integer;
begin
  dword2decodedFileAttributes := INCOMPLETE_FUNCTION;
  try
    fattrib := '';
    if (d and $01) <> 0 then
      fattrib := fattrib + 'R';
    if (d and $02) <> 0 then
      fattrib := fattrib + 'H';
    if (d and $04) <> 0 then
      fattrib := fattrib + 'S';
    if (d and $08) <> 0 then
      fattrib := fattrib + 'V';
    if (d and $10) <> 0 then
      fattrib := fattrib + 'D';
    if (d and $20) <> 0 then
      fattrib := fattrib + 'A';
  except
    dword2decodedFileAttributes := DWORD_DECODE_TO_ATTRIBUTES_ERROR;
  end;
  if dword2decodedFileAttributes = INCOMPLETE_FUNCTION then
    dword2decodedFileAttributes := SUCCESS;
end;

function extractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;
var
  s: ansistring;
  i, j: integer;
  dirarr: array of ansistring;
begin
  extractdirname := INCOMPLETE_FUNCTION;
  s := extractfilepath(inpath);
  i := 0;
  while length(s) > 0 do
  begin
    setlength(dirarr, i + 1);
    dirarr[i] := copy2symbdel(s, directoryseparator);
    if length(s) > 0 then
      if s[1] = directoryseparator then
        s := copy(s, 2, length(s) - 1);
    Inc(i, 1);
  end;
  dirname := dirarr[i - 1];
  dirpath := '';
  for j := 0 to i - 2 do
    dirpath := dirpath + dirarr[j] + directoryseparator;
  if dirpath = '' then
  begin
    dirpath := dirname;
    dirname := '';
  end;
  extractdirname := SUCCESS;
end;

function ansiextractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;
var
  s: ansistring;
  i, j: integer;
  dirarr: array of ansistring;
begin
  ansiextractdirname := INCOMPLETE_FUNCTION;
  s := extractfilepath(inpath);
  i := 0;
  while length(s) > 0 do
  begin
    setlength(dirarr, i + 1);
    dirarr[i] := copy2symbdel(s, directoryseparator);
    if length(s) > 0 then
      if s[1] = directoryseparator then
        s := copy(s, 2, length(s) - 1);
    Inc(i, 1);
  end;
  dirname := dirarr[i - 1];
  dirpath := '';
  for j := 0 to i - 2 do
    dirpath := dirpath + dirarr[j] + directoryseparator;
  if dirpath = '' then
  begin
    dirpath := dirname;
    dirname := '';
  end;
  ansiextractdirname := SUCCESS;
end;

function expand(s: ansistring;
  //input object
  var exp_files: TFoundList;
  //expanded list of objects
  var exp_fsizes: TFoundListSizes;
  //expanded list of object sizes
  var exp_ftimes: TFoundListAges;
  //expanded list of object ages
  var exp_fattr: TFoundListAttrib;
  //expanded list of object attributes
  var exp_fattr_dec: TFoundList;
  //expanded list of object decoded attributes
  var nfound: qword
  //number of objects found
  ): integer;
  //take the exit code from ListFile function that it calls
var
  i, j: qword;
  r: TSearchRec;
begin
  result:=-1;
  nfound := 0;
  i := 0;
  j := 0;
  SetLength(exp_files, i + 1);
  SetLength(exp_fsizes, i + 1);
  SetLength(exp_ftimes, i + 1);
  SetLength(exp_fattr, i + 1);
  SetLength(exp_fattr_dec, i + 1);
  exp_files[0] := s;
  dword2decodedFileAttributes(filegetattr(s), exp_fattr_dec[0]);
  if filegetattr(s) and faDirectory = 0 then //object is a file
  begin
    if FindFirst(s, faAnyFile, r) = 0 then
    begin
      exp_fsizes[0] := r.Size;
      exp_ftimes[0] := r.Time;
      exp_fattr[0] := r.Attr;
      dword2decodedFileAttributes(r.Attr, exp_fattr_dec[0]);
    end;
    FindClose(r);
  end
  else
  begin
    if exp_files[0][length(exp_files[0])] <> DirectorySeparator then
      exp_files[0] := exp_files[0] + DirectorySeparator;
    exp_fsizes[0] := 0;
    if FindFirst(s + '.', faDirectory, r) = 0 then
    begin
      exp_fsizes[0] := r.Size;
      exp_ftimes[0] := r.Time;
      exp_fattr[0] := r.Attr;
      dword2decodedFileAttributes(r.Attr, exp_fattr_dec[0]);
    end;
    FindClose(r);
    expand := ListDetails(s, '*', faAnyFile,
      True, //recursive listing
      True, //add objects in list after the main one
      i, j, exp_files,
      exp_fsizes, exp_ftimes, exp_fattr, exp_fattr_dec);
  end;
  nfound := i;
  result:=1;
end;

function listdetails(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec: TFoundList
  //list of file decoded attributes
  ): integer;
var
  dir: ansistring;
begin
  ListDetails := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  try
    //check for valid path, else set executable path as path
    if length(path) = 0 then
       begin
       result := CALL_ERROR;
       exit;
       end
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir<>'' then
       if dir[length(dir)] <> DirectorySeparator then
       dir := dir + DirectorySeparator;
    ListDetails := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs,
      flist, fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
  except
    ListDetails := CALL_ERROR;
    exit;
  end;
  if ListDetails = INCOMPLETE_FUNCTION then
    ListDetails := SUCCESS;
end;

procedure DefaultCopyDataProc(oldnode, newnode : TTreenode);
begin
  newnode.Assign(oldnode);
  newnode.ImageIndex:=3;
  newnode.SelectedIndex:=3;
end;

procedure CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil);
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }

  if not Assigned(CopyProc) then
    CopyProc := @DefaultCopyDataProc;

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc);
    child := child.getNextSibling;
  end;
end;

function TreeViewSetTextPath(atreeview: TTreeView;
  anode: TTreeNode; Path: ansistring): integer;
var
  Str: TStringList;
  i, sc: integer;
begin
  Result := -1;
  if not directoryexists(Path) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := Path;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  //{$IFDEF WINDOWS}
  //Str[0] := Str[0] + PathDelim; //needed in 0.9.28.2, not needed in 0.9.30
  //{$ENDIF}
  atreeview.BeginUpdate;
  for i := 0 to sc - 1 do
  begin
    while (ANode <> nil) and (ANode.Text <> Str[i]) do
      ANode := ANode.GetNextSibling;
    if Anode <> nil then
    begin
      Anode.Expanded := True;
      ANode.Selected := True;
      Anode := ANode.GetFirstChild;
    end
    else
    begin
      atreeview.EndUpdate;
      str.Free;
      Result := 1;
      Exit;
    end;
  end;
  atreeview.EndUpdate;
  str.Free;
  Result := 0;
end;

function ShellTreeViewSetTextPath(ashelltreeview: TShellTreeView;
  Path: ansistring): integer;
var
  Str: TStringList;
  ANode: TTreeNode;
  i, sc: integer;
begin
  Result := -1;
  if not directoryexists(Path) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := Path;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  //{$IFDEF WINDOWS}
  //Str[0] := Str[0] + PathDelim; //needed in 0.9.28.2, not needed in 0.9.30
  //{$ENDIF}
  ashelltreeview.BeginUpdate;
  ANode := ashelltreeview.Items[0];
  for i := 0 to sc - 1 do
  begin
    while (ANode <> nil) and (ANode.Text <> Str[i]) do
      ANode := ANode.GetNextSibling;
    if Anode <> nil then
    begin
      Anode.Expanded := True;
      ANode.Selected := True;
      Anode := ANode.GetFirstChild;
    end
    else
    begin
      ashelltreeview.EndUpdate;
      str.Free;
      Result := 1;
      Exit;
    end;
  end;
  ashelltreeview.EndUpdate;
  str.Free;
  Result := 0;
end;

procedure ComboBoxSetPaths(acombobox: TCombobox; apath:ansistring);
var
  Str: TStringList;
  i, j, sc: integer;
  s: array [0..8] of ansistring;
begin
  if not directoryexists(apath) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := apath;
  s[0] := apath;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  for j := 8 downto 1 do
    if (sc - 1 - (8 - j)) >= 0 then
      for i := 0 to sc - 1 - (8 - j) do
        s[j] := s[j] + str[i] + PathDelim;
  acombobox.Clear;
  acombobox.Caption := s[0];
  for i := 8 downto 1 do
    if s[i] <> '' then
      acombobox.Items.Add(s[i]);
  str.Free;
end;

function checkfiledirname(s: ansistring): integer;
var
  sf: ansistring;
  i: integer;
begin
  checkfiledirname := -1;
  if s = '' then
    exit;
  //illegal characters, full name
  for i := 0 to 31 do
    if pos(char(i), s) <> 0 then
      exit;
  //reserved characters, full name
  if pos('*', s) <> 0 then
    exit;
  if pos('?', s) <> 0 then
    exit;
{$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then
    exit;
{$ENDIF}
  if pos('<', s) <> 0 then
    exit;
  if pos('>', s) <> 0 then
    exit;
  if pos('|', s) <> 0 then
    exit;
  if pos('       ', s) <> 0 then
    exit;
  sf := extractfilename(s);
  //reserved characters, filename only (others are checked for the full name)
  if pos('\', sf) <> 0 then
    exit;
  if pos('/', sf) <> 0 then
    exit;
  if pos(':', sf) <> 0 then
    exit;
  //reserved filenames (Windows)
{$IFDEF MSWINDOWS}
  cutextension(sf);
  sf := upcase(sf);
  if (sf = 'CON') or (sf = 'PRN') or (sf = 'AUX') or (sf = 'NUL') or
    (sf = 'COM1') or (sf = 'COM2') or (sf = 'COM3') or (sf = 'COM4') or
    (sf = 'COM5') or (sf = 'COM6') or (sf = 'COM7') or (sf = 'COM8') or
    (sf = 'COM9') or (sf = 'LPT1') or (sf = 'LPT2') or (sf = 'LPT3') or
    (sf = 'LPT4') or (sf = 'LPT5') or (sf = 'LPT6') or (sf = 'LPT7') or
    (sf = 'LPT8') or (sf = 'LPT9') then
    exit;
{$ENDIF}
  checkfiledirname := 0;
end;

//wrapper for filename/dir check, accepts empty or valid name as valid input for special purposes i.e. replace string in file name with a valid string or nothing (it is needed a separate final check for the file name to not be empty)
function checkfiledirname_acceptblank(s: ansistring): integer;
begin
result := -1;
if s = '' then result:=0
else result:=checkfiledirname(s);
end;

function checkescapedoutname(s: ansistring):ansistring;
var
   varstr: ansistring;
   i: integer;
begin
varstr := s;
{$IFNDEF MSWINDOWS}
repeat
   i := pos('?', varstr);
   if i > 0 then
      varstr[pos('?', varstr)] := '_';
until i = 0;
{$ENDIF}
result:=varstr;
end;

function escapefilenamelinuxlike(s: ansistring; desk_env: byte): ansistring;
var
  varstr,dstr: ansistring;
  i: integer;
begin
  varstr := s;
  dstr:=correctdelimiter(s);
  // replace quote characters if found in string;
  //correctdelimiter swaps ' and " quotes, but that cannot guarantee against both characters being used in the same string - the remaining character is replaced by a jolly
  i := 1;
  if dstr='''' then
     repeat
     i := pos('''', varstr);
     if i > 0 then
        varstr[pos('''', varstr)] := '?';
     until i = 0;
  if dstr='"' then
     repeat
     i := pos('"', varstr);
     if i > 0 then
        varstr[pos('"', varstr)] := '?';
     until i = 0;
  // find and delete 'file://' (and any part before) if it is passed as part of filename (it happens sometimes in Gnome, i.e. using "open with" context menu entry)
  i := pos('file://', varstr);
  if i > 0 then
    varstr := copy(varstr, i + 7, length(varstr) - i - 6);
  //replace %20 with space (if inverse replacement was done by desktop environment, happens in Gnome passing input from desktop environment rather than from application's dialogs)
  if desk_env = 1 then //apply it strictly only on Gnome
    if filegetattr(s) <= 0 then
      repeat
        i := pos('%20', varstr);
        if i > 0 then
        begin
          Delete(varstr, i, 3);
          insert(' ', varstr, i);
        end;
      until i = 0;
  escapefilenamelinuxlike := varstr;
end;

function escapefilename(s: ansistring; desk_env: byte): ansistring;
begin
{$IFDEF MSWINDOWS}
  escapefilename := s;
{$ENDIF}
{$IFDEF LINUX}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF FREEBSD}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF NETBSD}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF OPENBSD}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF DARWIN}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
end;

function escapefilenamedelim(s: ansistring; desk_env: byte): ansistring;
var
   cdelim,estr:utf8string;
begin
estr:=escapefilename(s, desk_env);
if pos(' ',estr)<>0 then
   cdelim:=correctdelimiter(s)
else
   cdelim:='';
result:=cdelim+estr+cdelim;
end;

function stringdelim(s:ansistring): ansistring;
var
   cdelim:utf8string;
begin
cdelim:=correctdelimiter(s);
result := cdelim+s+cdelim;
end;

function stringundelim(s:ansistring): ansistring;
var
   cdelim:utf8string;
   st:ansistring;
begin
st:=s;
cdelim:=correctdelimiter(st);
if length(st)>1 then if st[1]=cdelim then st:=copy(st,2,length(st)-1);
if length(st)>1 then if st[length(st)]=cdelim then st:=copy(st,1,length(st)-1);
result:=st;
end;

function cp_open_linuxlike(s: ansistring; desk_env: byte): integer;
var
  P: TProcessUTF8;
begin
  cp_open_linuxlike := -1;
  if s = '' then
    exit;
  if (desk_env = 10) then //continue for gnome=1 kde=2 and unknown desktop manager =0, exit for Windows=10
    exit;
  P := TProcessUTF8.Create(nil);
  P.Options := [poWaitOnExit];
  if desk_env = 20 then // Darwin=20
    begin
    P.CommandLine:='open ' + escapefilenamedelim(s, desk_env);
    end
  else
    begin
    P.Executable:='xdg-open';
    P.Parameters.Add(escapefilename(s, desk_env));
    end;
  P.Execute;
  cp_open_linuxlike := P.ExitStatus;
  P.Free;
end;

procedure winexplorepath(s: ansistring);
var
  P: TProcessUTF8;
  cl: ansistring;
begin
{$IFDEF MSWINDOWS}
P := TProcessUTF8.Create(nil);
if fileexists(s) then
   if s[1] = '"' then
      cl := 'explorer /select,' + s
   else
      cl := 'explorer /select,"' + s + '"'
else
   if s[1] = '"' then
      cl := 'explorer ' + extractfilepath(s)
   else
       cl := 'explorer "' + extractfilepath(s) + '"';
cl := (cl);
P.CommandLine := cl;
P.Execute;
P.Free;
{$ENDIF}
end;

procedure macexplorepath(s: ansistring);
var
  P: TProcessUTF8;
begin
  if s = '' then
    exit;
  P := TProcessUTF8.Create(nil);
  P.Options := [poWaitOnExit];
  P.CommandLine:='open -R ' + escapefilenamedelim(s, 20);
  P.Execute;
  P.Free;
end;

procedure cp_search_linuxlike(desk_env: byte);
var
  P: TProcessUTF8;
begin
  if (desk_env = 0) or (desk_env = 10) then
    exit;
  P := TProcessUTF8.Create(nil);
  P.Options := [poWaitOnExit];
  case desk_env of
     1: P.Executable := 'gnome-search-tool';
     2: P.Executable := 'kfind';
     20: P.CommandLine := 'open /';
     end;
  P.Execute;
  P.Free;
end;

function correctdelimiter(s:AnsiString): AnsiString;
begin
result := '''';
{$IFDEF MSWINDOWS}
result := '"';
{$ELSE}
if pos('''',s)<>0 then result := '"';
{$ENDIF}
end;

procedure getdesk_env(var bytedesk: byte; var caption_build, delimiter: ansistring);
//0 unknown, 1 Gnome, 2 KDE, 10 MS Windows, 20 Darwin
begin
  caption_build := 'Unknown OS Build';
  delimiter := '''';
  //defaults, overwritten by specifical values if target system is recognized: unknown OS, ' as delimiter character (*x like)
{$IFDEF WIN32}
  caption_build := 'Windows Build';
  delimiter := '"';
{$ENDIF}
{$IFDEF WIN64}
  caption_build := 'Win64 Build';
  delimiter := '"';
{$ENDIF}
{$IFDEF LINUX}
  caption_build := 'Linux Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF FREEBSD}
  caption_build := 'FreeBSD Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF NETBSD}
  caption_build := 'NetBSD Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF OPENBSD}
  caption_build := 'OpenBSD Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF DARWIN}
  caption_build := 'Darwin Build';
  delimiter := '''';
{$ENDIF}
  bytedesk := 0; //unrecognized desktop environment
{$IFDEF MSWINDOWS}
  bytedesk := 10;
{$ENDIF}
{$IFDEF DARWIN}
  bytedesk := 20;
{$ENDIF}

//Architecture
{$IFDEF CPUX86} caption_build := caption_build + ', x86'; {$ENDIF}
{$IFDEF CPUX86_64} caption_build := caption_build + ', x86_64'; {$ENDIF}
{$IFDEF CPUARM} caption_build := caption_build + ', ARM'; {$ENDIF}
{$IFDEF CPUAARCH64} caption_build := caption_build + ', AARCH64'; {$ENDIF}

//Widget set
{$IFDEF LCLCARBON} caption_build := caption_build + ', Carbon'; {$ENDIF}
{$IFDEF LCLCOCOA} caption_build := caption_build + ', Cocoa'; {$ENDIF}
{$IFDEF LCLQT} caption_build := caption_build + ', QT4'; {$ENDIF}
{$IFDEF LCLQT5} caption_build := caption_build + ', QT5'; {$ENDIF}
{$IFDEF LCLQT6} caption_build := caption_build + ', QT6'; {$ENDIF}
{$IFDEF LCLGTK} caption_build := caption_build + ', GTK'; {$ENDIF}
{$IFDEF LCLGTK2} caption_build := caption_build + ', GTK2'; {$ENDIF}
{$IFDEF LCLGTK3} caption_build := caption_build + ', GTK3'; {$ENDIF}

  if getenvironmentvariable('GNOME_DESKTOP_SESSION_ID') <> '' then
    bytedesk := 1; //if this Gnome specific env variable is set, probably the user is running Gnome
  if getenvironmentvariable('KDE_FULL_SESSION') <> '' then
    bytedesk := 2; //if this KDE specific env variable is set, probably the user is running KDE
  if getenvironmentvariable('DESKTOP_SESSION') = 'gnome' then
    bytedesk :=
      1 //if gnome or kde is explicitly declared in DESKTOP_SESSION env variable, override previously assumed result
  else
  if getenvironmentvariable('DESKTOP_SESSION') = 'kde' then
    bytedesk := 2;
  case bytedesk of
    1: caption_build := caption_build + ' (Gnome)';
    2: caption_build := caption_build + ' (KDE)';
  end;
end;

procedure setendingdirseparator(var s:ansistring);
begin
if s<>'' then
   if s[length(s)]<>directoryseparator then
      s:=s+directoryseparator;
end;

procedure get_home_path(var s: ansistring); //superseded in Windows
begin
{$IFDEF MSWINDOWS}
  s := (GetEnvironmentVariable('USERPROFILE'));
{$ENDIF}
{$IFDEF LINUX}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF FREEBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF NETBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF OPENBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF DARWIN}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
if s = '' then
   s := (getcurrentdir);
setendingdirseparator(s);
end;

procedure get_home_subpaths(var usr_documents,usr_downloads,usr_music,usr_pictures,usr_videos: ansistring);
var
  s:ansistring;
begin
{$IFNDEF MSWINDOWS}
get_home_path(s);
if DirectoryExists(s + 'Documents/') then usr_documents := s + 'Documents/';
if DirectoryExists(s + 'Downloads/') then usr_downloads := s + 'Downloads/';
if DirectoryExists(s + 'Music/') then usr_music := s + 'Music/';
if DirectoryExists(s + 'Pictures/') then usr_pictures := s + 'Pictures/';
if DirectoryExists(s + 'Videos/') then usr_videos := s + 'Videos/';
{$IFDEF DARWIN}
if DirectoryExists(s + 'Movies/') then usr_videos := s + 'Movies/';
{$ENDIF}
{$ENDIF}
end;

procedure get_desktop_path(var s: ansistring); //superseded in Windows
begin
get_home_path(s);
{$IFDEF MSWINDOWS}
  s := s + 'Desktop\';
{$ENDIF}
{$IFDEF LINUX}
  s := s + 'Desktop/';
{$ENDIF}
{$IFDEF FREEBSD}
  s := s + 'Desktop/';
{$ENDIF}
{$IFDEF NETBSD}
  s := s + 'Desktop/';
{$ENDIF}
{$IFDEF OPENBSD}
  s := s + 'Desktop/';
{$ENDIF}
{$IFDEF DARWIN}
  s := s + 'Desktop/';
{$ENDIF}
end;

procedure get_usrtmp_path(var s: ansistring);
//works fine in Windows even if username contains extended characters
begin
s:=GetTempDir;
if s = '' then get_desktop_path(s);
setendingdirseparator(s);
end;

function fget_usrtmp_path:ansistring;
var
   s:ansistring;
begin
get_usrtmp_path(s);
result:=s;
end;

procedure cutextension(var s: ansistring);//uses a small sett of rules to avoid cutting strings which are not really meant as extensions
var
   sext:ansistring;
begin
   sext:=extractfileext(s);
   if sext='' then exit;
   if pos(' ',sext) <>0 then exit;
   if length(sext)>6 then exit;
   setlength(s, length(s) - length(extractfileext(s)));
end;

function cutext(var s: ansistring):ansistring;
var s1:ansistring;
begin
   s1:=s;
   cutextension(s1);
   result:=s1;
end;

function checkfilename(s: ansistring): integer;
var
  s1: ansistring;
  i: integer;
begin
  checkfilename := -1;
  if (s = '') or (s='.') or (s='..') then exit;
  //illegal characters (no problem for additional UTF8 bytes since have MSB set to 1 to avoid conflict with those control characters)
  for i := 0 to 31 do
  begin
    if pos(char(i), s) <> 0 then exit;
  end;
  //reserved characters
  if pos('\', s) <> 0 then exit;
  if pos('/', s) <> 0 then exit;
  if pos(':', s) <> 0 then exit;
  if pos('*', s) <> 0 then exit;
  if pos('?', s) <> 0 then exit;
{$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then exit;
{$ENDIF}
  if pos('<', s) <> 0 then exit;
  if pos('>', s) <> 0 then exit;
  if pos('|', s) <> 0 then exit;
  if pos('       ', s) <> 0 then exit;
  //reserved filenames (Windows)
{$IFDEF MSWINDOWS}
  s1 := extractfilename(s);
  cutextension(s1);
  s1 := upcase(s1);
  if (s1 = 'CON') or (s1 = 'PRN') or (s1 = 'AUX') or (s1 = 'NUL') or
    (s1 = 'COM1') or (s1 = 'COM2') or (s1 = 'COM3') or (s1 = 'COM4') or
    (s1 = 'COM5') or (s1 = 'COM6') or (s1 = 'COM7') or (s1 = 'COM8') or
    (s1 = 'COM9') or (s1 = 'LPT1') or (s1 = 'LPT2') or (s1 = 'LPT3') or
    (s1 = 'LPT4') or (s1 = 'LPT5') or (s1 = 'LPT6') or (s1 = 'LPT7') or
    (s1 = 'LPT8') or (s1 = 'LPT9') then
    exit;
{$ENDIF}
  checkfilename := 0;
end;

//wrapper for filename check, accepts empty or valid file name as valid input for special purposes i.e. replace string in file name with a valid string or nothing (it is needed a separate final check for the file name to not be empty)
function checkfilename_acceptblank(s: ansistring): integer;
begin
result := -1;
if s = '' then result:=0
else result:=checkfilename(s);
end;

function validatecl(var s: ansistring): integer;
var
  i: integer;
  s1,delimch:ansistring;
begin
validatecl := -1;
if s = '' then   exit;
for i := 0 to 31 do if pos(char(i), s) <> 0 then exit; //illegal characters

delimch := correctdelimiter(s);

s1:=s;
if (pos('bin'+directoryseparator+'7z',s)<>0) or (pos('bin'+directoryseparator+'arc',s)<>0) or (pos('Rar.exe',s)<>0) then
   if pos(delimch+'-p',s)<>0 then
      begin
      s1:=copy(s,pos(delimch+'-p',s)+3,length(s)-pos(delimch+'-p',s)-2);
      s1:=copy(s1,pos(delimch,s1)+1,length(s1)-pos(delimch,s1));
      s1:=copy(s,1,pos(delimch+'-p',s))+s1;
      end
   else
      if pos(' -p',s)<>0 then
      begin
      s1:=copy(s,pos(' -p',s)+3,length(s)-pos(' -p',s)-2);
      s1:=copy(s1,pos(' ',s1)+1,length(s1)-pos(' ',s1));
      s1:=copy(s,1,pos(' -p',s))+s1;
      end;

if (pos('bin'+directoryseparator+'arc',s)<>0) or (pos('Rar.exe',s)<>0) then
   if pos(delimch+'-hp',s)<>0 then
      begin
      s1:=copy(s,pos(delimch+'-hp',s)+4,length(s)-pos(delimch+'-hp',s)-3);
      s1:=copy(s1,pos(delimch,s1)+1,length(s1)-pos(delimch,s1));
      s1:=copy(s,1,pos(delimch+'-hp',s))+s1;
      end
   else
      if pos(' -hp',s)<>0 then
      begin
      s1:=copy(s,pos(' -hp',s)+4,length(s)-pos(' -hp',s)-3);
      s1:=copy(s1,pos(' ',s1)+1,length(s1)-pos(' ',s1));
      s1:=copy(s,1,pos(' -hp',s))+s1;
      end;

if (pos(directoryseparator+'pea',s)<>0) and
   (pos('bin'+directoryseparator+'7z',s)=0) and
   (pos('bin'+directoryseparator+'arc',s)=0) then
   if pos('BATCH',s)<>0 then
      begin
      s1:=copy(s,pos('BATCH',s)+6,length(s)-pos('BATCH',s)-5);
      if pos('FROMCL',s1)<>0 then s1:=copy(s1,pos('FROMCL',s1)+7,length(s1)-pos('FROMCL',s1)-6)
      else s1:=copy(s1,pos('NOKEYFILE',s1)+9,length(s1)-pos('NOKEYFILE',s1)-8);
      s1:=copy(s,1,pos('BATCH',s))+s1;
      end;

{if pos('<',s1)<>0 then exit;
if pos('>',s1)<>0 then exit;}
if pos('|', s1) <> 0 then exit;
if pos('       ', s1) <> 0 then exit; //more than 6 consecutive spaces may be intentional attempt to hamper readability (as in 7-Zip)
validatecl := 0;
end;

{$IFDEF MSWINDOWS}
procedure getwinenv(var wincomspec, winver: ansistring);
var
  osVerInfo: TOSVersionInfo;
begin
  wincomspec := extractfilename(GetEnvironmentVariable('COMSPEC'));
  if (upcase(wincomspec)<> 'COMMAND.COM') and (upcase(wincomspec)<> 'CMD.EXE') then wincomspec:='CMD.EXE';
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    if osVerInfo.dwMajorVersion <= 4 then
      if upcase(wincomspec) = 'CMD.EXE' then
        winver := 'nt4'
      else
        winver := '9x';
    if osVerInfo.dwMajorVersion = 5 then
      winver := 'nt5';
    if osVerInfo.dwMajorVersion >= 6 then
      winver := 'nt6+';
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure getwinenvadv(var wincomspec, winver, majmin: ansistring);
var
  osVerInfo: TOSVersionInfo;
begin
  wincomspec := extractfilename(GetEnvironmentVariable('COMSPEC'));
  if (upcase(wincomspec)<> 'COMMAND.COM') and (upcase(wincomspec)<> 'CMD.EXE') then wincomspec:='CMD.EXE';
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    majmin:=inttostr(osVerInfo.dwMajorVersion)+'.'+inttostr(osVerInfo.dwMinorVersion);
    if osVerInfo.dwMajorVersion <= 4 then
      if upcase(wincomspec) = 'CMD.EXE' then
        winver := 'nt4'
      else
        winver := '9x';
    if osVerInfo.dwMajorVersion = 5 then
      winver := 'nt5';
    if osVerInfo.dwMajorVersion >= 6 then
      winver := 'nt6+';
  end;
end;
{$ENDIF}

function testext(s: ansistring): integer;
var
  ext: ansistring;
begin
  testext := -1;
  ext := lowercase(extractfileext(s));
  //file types supported through 7z backend
  case ext of
    //trditioanl archive types 0..99
    '.7z', '.cb7': testext := 0;
    '.bz', '.bz2', '.bzip2', '.bzip', '.tbz2', '.tbz', '.tbzip2', '.tbzip', '.tb2': testext := 1;
    '.gz', '.gzip', '.tgz', '.tpz', '.cpgz' : testext := 2;
    '.tar', '.cbt': testext := 5;
    '.zip', '.cbz', '.smzip', '.ppmd': testext := 6;
    '.arj': testext := 7;
    '.cpio': testext := 10;
    '.lz': testext := 11;
    '.lzh': testext := 12;
    '.rar', '.cbr', '.r00', '.r01': testext := 13;
    '.00':
       begin
       cutextension(s);
       if lowercase(extractfileext(s)) = '.rar' then
       testext := 13;
       end;//.rar.00
    '.z', '.taz', '.tz': testext := 14;
    '.zipx': testext := 15;//non-legacy WinZip archives (supported for reading as for 7z 9.22)
    '.xz', '.txz': testext := 16;
    '.lha': testext := 17;
    '.wim', '.swm': testext := 18;//MS WIM Windows image file, SWM Split WIM image file
    '.lzma86', '.lzma': testext := 19;
    '.part1', '.split': testext := 20; //generic spanned archive, open with 7z binary

    //container types 100..499
    //100..199 packages
    '.cab', '.imf': testext := 100; //cab and derived formats
    '.chm', '.chi', '.chq', '.chw', '.hxs', '.hxi', '.hxr', '.hxq', '.hxw', '.lit': testext := 101;
    '.swf', '.fla': testext := 102; //Adobe Flash projects
    '.air': testext := 103; // Adobe Integrated Runtime
    '.xpi',//Mozilla installer package
    '.maff': testext := 104; //Mozilla web archive
    '.pcv': testext := 105; //Thunderbird profile MozBackup
    '.crx': testext := 106; //Chrome extension
    '.pak', '.pk3', '.pk4', '.iwd': testext := 107; //package format used in Quake3 (pk3) and Quake 4 and Doom3 (pk4), zip with checksum appended
    '': if extractfilename(s) = '[Content]' then testext := 108;//in case of [Content] filename as when extracting RPM or SLP files
    '.u3p': testext := 109;//U3P portable application package
    '.xar', '.pkg': testext := 110;
    '.kmz': testext := 111;
    '.mslz': testext := 112;
    '.bsz': testext := 113;
    '.rmskin': testext := 114;
    '.wal', '.wsz': testext := 115;
    '.wmz': testext := 116; //compressed Windows Media Player file
    '.xip': testext := 117;//Apple signed zip files
    '.mub': testext := 118; //mub compressed files
    '.dmg': testext := 119;
    '.deb': testext := 120;
    '.rpm': testext := 121;
    '.pet', '.pup': testext := 122;//PuppyLinux packages
    '.slp': testext := 123;//Stampede Linux packages
    '.ipa', '.ipsw': testext := 124; //.ipa iPhone application archive file .ipsw iOS devices firmware packages (.zip variants)
    '.jar', '.ear', '.sar', '.war': testext := 125;//Java packages (.zip derived)
    '.xzm': testext := 126;//Porteus Linux packages
    '.mlc', '.mui': testext := 127;//Microsoft's Language Interface Pack and Multilingua User Interface packages
    '.appx', '.appxbundle', '.appv', '.smpk', '.nupkg',
    '.dwfx', '.familyx', '.fdix', '.semblio', '.vsix', '.cspkg', '.scdoc',
    '.xps', '.oxps', '.jtx', '.cddx', '.3mf', '.vsdx', '.mmzx', '.aasx', '.slx': testext := 128;//OPC files treated as archives
    '.msix','.esd': testext := 129;//Microsoft MSIX app packages and ESD images
    '.snap': testext := 130;//Canonical Ubuntu Snap packages
    '.appimage': testext := 131;//AppImage packages
    '.ipk': testext := 132; //Freedesktop's Listaller .ipk packages
    '.whl': testext := 133; //Python package
    '.gem': testext := 134; //Ruby gem package
    '.apk', '.xapk', '.apkm', '.apks', '.aab' : testext := 134; //Android packages (.zip derived)

    //200..499 filesystems
    '.iso': testext := 200;
    '.udf': testext := 201;
    '.hfs', '.hfsx': testext := 202;
    '.vhd', '.vhdx': testext := 203;//Microsoft Virtual PC Virtual Hard Disk
    '.apm': testext := 204; //Apple Partition Map disk images
    '.ima', '.img': testext := 205;
    '.imz': testext := 206;
    '.mdf': testext := 207; //Alchool 120 image file
    '.gpt':  testext := 208; //GPT GUID Partition Table file
    '.qcow', '.qcow2', '.qcow2c': testext := 209;//QUEMU image file
    '.vmdk': testext := 210;//VMware Virtual Machine Disk
    '.vdi': testext := 211;//Oracle VirtualBox Virtual Drive Image
    '.mbr': testext := 212;
    '.fat': testext := 213;
    '.ntfs': testext := 214;
    '.sfs': testext := 215;
    '.image': testext := 216;
    '.squashfs': testext := 217;
    '.cramfs': testext := 218;
    '.ext', '.ext2', '.ext3', '.ext4': testext := 219;
    '.scap', '.uefif': testext := 220;

    //file types usually not handled as archives, can be supported through 7z backend 500..599
    '.exe', '.dll', '.sys': testext := 500; //most executables can be opened
    '.msi', '.msp', '.msu': testext := 500;
    '.sxc', '.sxd', '.sxi', '.sxw', '.stc', '.std', '.sti', '.stw', '.sxg', '.sxm': testext := 501; //OOo 1.x legacy filetypes
    '.ods', '.ots', '.odm', '.oth', '.oxt', '.odb', '.odf', '.odg', '.otg', '.odp', '.otp', '.odt', '.ott': testext := 501; //OOo filetypes
    '.gnm': testext := 501; //Gnumeric spreadsheet
    '.pmdx', '.pmvx', '.tmdx', '.prdx': testext := 501; //SoftMaker Office compressed formats
    '.doc', '.dot', '.xls', '.xlt', '.ppt', '.pps', '.pot': testext := 502; //non executable COMPOUND files
    '.docx', '.dotx', '.xlsx', '.xltx', '.pptx','.mpp': testext := 502; //MS compressed formats, treated as othes MS Office formats
    '.iwa','.numbers','.pages','.key': testext := 502; //Apple iWork compressed IWA file types
    //misc formats to be handled primarily as non-archive:
    '.flv', //flash videos
    '.epub', //EPUB ebook (.zip variant)
    '.man': testext := 503; //compressed Unix/Linux man files

    //files supported through other backends
    '.quad': testext := 1001;
    '.balz': testext := 1002;
    '.bcm': testext := 1003;
    '.zpaq': testext := 2000;
    '.paq8f': testext := 2001;
    '.paq8jd': testext := 2002;
    '.paq8l': testext := 2003;
    '.paq8o': testext := 2004;
    '.lpaq1': testext := 2501;
    '.lpaq5': testext := 2505;
    '.lpaq8': testext := 2508;
    '.ace', '.cba': testext := 3001;
    '.arc': testext := 4001;
    '.wrc': testext := 4002;
    '.br': testext := 5001;//Brotli .br compressed file
    '.zst','.zstd','.tzst': testext := 5002;//Zstd .zst compressed file
    '.001': testext := 10000;//generic spanned archive
    '.pea': testext := 10001;
    else
      if length(ext)>2 then if ext[length(ext)-1]+ext[length(ext)]='aa' then testext := 10000;
  end;
end;

function testinput(infile: ansistring; testdir: boolean): integer;
var
  i: integer;
begin
  testinput := 0;//not supported filetype
  if testdir = True then
    if checkdirexists(infile) then
      testinput := 1000;
  i := testext(infile);
  if i >= 0 then
    testinput := 3;//7z archive
  //specific conditions which overwrites previous result
  case i of
    2000: testinput := 10; //zpaq
    2001, 2002, 2003, 2004: testinput := 5;//paq archive
    2501, 2505, 2508: testinput := 8;//lpaq
    1001, 1002, 1003: testinput := 6;//quad
    3001: testinput := 7;//ace
    4001, 4002: testinput := 9;//freearc
    5001: testinput:=11;
    5002: testinput:=12;
    10000: testinput := 4; //use 7z backend to handle split archives
    10001: testinput := 1; //Pea
  end;
end;

function testpcomp(var s:ansistring):integer;
var
   ext:ansistring;
begin
ext:=lowercase(extractfileext(s));
case ext of
   '.lnk','.txt','.rtf','.wri','.ini','.log','.mid': begin result:=10; exit; end;
   '.htm','.html','.xml','.mht','.url','.css','.xhtml': begin result:=10; exit; end;
   '.bat','.pif','.scr','.vbs','.cmd','.reg': begin result:=10; exit; end;
   '.pas','.pp','.h','.c','.hh','.cpp','.cc','.cxx','.hxx','.cs','.java','.pl','.pm','.php','.py','.p','rb',
   '.js','.asp','.aspx','.vb','.manifest': begin result:=10; exit; end;
   '.xls','.xlt','.gnm','.csv': begin result:=30; exit; end;
   '.ani','.cur','.ico','.icl': begin result:=35; exit; end;
   '.eml': begin result:=40; exit; end;
   '.doc','.dot': begin result:=40; exit; end;
   '.dll','.sys','.so','.dylib': begin result:=40; exit; end;
   '.bmp','.tga','.tif','.tiff': begin result:=40; exit; end;
   '.wav': begin result:=45; exit; end;
   '.exe': begin result:=50; exit; end;
   '.db','.dbf','.mdb','.nsf': begin result:=50; exit; end;
   '.pps','.ppt','.odp': begin result:=50; exit; end;
   '.gif': begin result:=70; exit; end;
   '.xlsx','.ods','.numbers': begin result:=80; exit; end;
   '.docx','.odt','.pages': begin result:=80; exit; end;
   '.pptx','.key': begin result:=80; exit; end;
   '.svg','.ps','.eps','.cdr','.ai','.psd','.psp': begin result:=80; exit; end;
   '.pdf': begin result:=90; exit; end;
   '.png': begin result:=90; exit; end;
   '.jpg','.jpe','.jpeg','.jif', '.jfif', '.jfi','.jpx','.jp2','.j2k': begin result:=90; exit; end;
   '.avi','.mpg','.mpeg','.xvid','.divx','.mp4','.mov','.3gp','.wmv','.swf','.flv','.fla': begin result:=99; exit; end;
   '.mp3','.wma','.aiff','.ogg': begin result:=99; exit; end;
  end;
case testext(s) of //most used special formats are intercepted before
   -1: result:=50; //unknown
   else
     begin
     result:=95;//archives, guess medium/high compression ratio unless differently specified after
     if (ext='.tar') or (ext='.iso') or (ext='.wim') then begin result:=55; exit; end;
     if (ext='.rar') or (ext='.zipx') or (ext='.7z') or (ext='.xz') or (ext='.arc') or (ext='.wrc') then begin result:=100; exit; end;
     end;
   end;
end;

function dirsizetc(path, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size, tcsize: qword
  //total size (Byte)
  ): integer;
var
  i,tpcomp: integer;
  dir: ansistring;
  flist: TFoundList;
  fsizes: TFoundListSizes;
  ftimes: TFoundListAges;
  fattr: TFoundListAttrib;
  fattr_dec: TFoundList;
begin
  dirsizetc := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  size := 0;
  tcsize := 0;
  try
    //check for valid path, else set executable path as path
    if length(path) = 0 then
       begin
       result := CALL_ERROR;
       exit;
       end
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir<>'' then
      if dir[length(dir)] <> DirectorySeparator then
      dir := dir + DirectorySeparator;
    dirsizetc := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs, flist,
      fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
    for i := 0 to length(fsizes) - 1 do
      begin
      size := size + fsizes[i];
      tpcomp:=testpcomp(flist[i]);
      tcsize := tcsize+ (fsizes[i] * tpcomp);
      end;
  except
    dirsizetc := CALL_ERROR;
    exit;
  end;
  if dirsizetc = INCOMPLETE_FUNCTION then
    dirsizetc := SUCCESS;
end;

function finuse(FileName: TFileName): Boolean;
var
  tf: file of byte;
begin
result:=true;
if DirectoryExists(FileName+directoryseparator) then exit;
if not FileExists(FileName) then exit;
if upcase(extractfileext(FileName))='.TMP' then exit;
if pos('~$',FileName)<>0 then exit;
result:=false;
try
assignfile(tf,FileName);
{$I-} Reset(tf); {$I+}
if IOResult<>0 then result:=true;
closefile(tf);
except
result:=true;
closefile(tf);
end
end;

function rLast(dir, mask: ansistring; //last modified time
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var ltime:longint
  ): integer;
var
  r: TSearchRec;
begin
  result := INCOMPLETE_FUNCTION;
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          if ltime<r.Time then
             if finuse(dir + r.name)=false then
                ltime:=r.Time;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      result := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if FindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
            rLast(dir + (r.Name) + DirectorySeparator, mask, fattrib, recur, ltime);
        until findnext(r) <> 0;
      except
        FindClose(r);
        result := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if result = INCOMPLETE_FUNCTION then
    result := SUCCESS;
end;

function checkdirexists(s:ansistring):boolean;
var s1:ansistring;
begin
result:=true;
if s='' then
   begin
   result:=false;
   exit;
   end;
s1:=s;
if not(directoryexists(s1)) then
   if s1[length(s1)]=directoryseparator then
      begin
      setlength(s1,length(s1)-1);
      if not(directoryexists(s1)) then result:=false;
      end
   else result:=false;
end;

function checkfiledirexists(s:ansistring):boolean;
begin
result:=true;
if not(fileexists(s)) then
   if not(checkdirexists(s)) then result:=false;
end;

end.
