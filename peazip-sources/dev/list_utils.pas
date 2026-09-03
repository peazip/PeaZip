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
 0.65     20250430  G.Tani      Can now toggle show hidden files on/off setting the variable showhidden (fahidden on Windows, .file on non-Windows systems)
 0.66     20250704  G.Tani      Improved multi-voulme detection, provided more comments, context, and possible improvement path for characters escaping procedures
 0.67     20251010  G.Tani      Moved here various filename handling functions from peach unit
                                Moved here ansiutf8_utils functions as the unit was suppressed
                                New functions to read file header's magic bytes, and to split strings
 0.68     20251110  G.Tani      New function to conditionally pass ansistring to TProcess as command line or as executable name + list of parameters
 0.69     20260702  G.Tani      Code reviewed and modernized
 0.70     20260826  G.Tani      Improved sanitization of special characters in validatecl: the use of more special characters is prevented to avoid generating potentially unsafe scripts
                                Improved sanitization of input string in stringdelim: the string is discarded if already containing the quotation character, to avoid propagating strings which can be not correctly handled in scripts

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
  //PeaZip's folder in working directory, containing all temporary work files unless 1) set to be created in output 2) archive conversion, always run in output
  STR_PZWORKTMP = 'peazip-tmp';
  //temp app subfolder, where different peazip instances can exchange messages and data
  STR_PEAZIPTMP = '.pztmp';
  //temp subdirs, deleted as soon as possible by each instance during use
  STR_TMP       = '.ptmp';
  STR_STMP      = '.pstmp';//special preview
  STR_TMPEXT    = '.petmp';//interactive extraction
  STR_TMPDD     = '.pdtmp';//drag and drop extraction
  //temp files
  STR_TESTOUT   = '.ptestouttmp';
  STR_STOPALL   = '.pstopalltmp';
  STR_TMPDROPE  = '.pdropetmp';
  STR_TMPERRI   = '.perritmp';

var
  showhidden:boolean;
  cllog:TStringList;
  pmode:integer = 0;//set, and changed, in main unit
  logcommands:integer = 0;//set, and changed, in main unit

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
  var nfiles, ndirs, tsize: qword
  //number of files and dirs found, total size
  ): integer;

//optionally recursive function to count files, total size, and return string with all files/dirs names (sorted as in Find function)
function rCountSizeN(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize: qword;
  //number of files and dirs found, total size
  var strn: ansistring
  //string with found files/dirs names
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

//replace special characters disallowd in Windows paths
procedure replacespecialpathchars(var s:ansistring);

//sanitize file/dir name string to avoid relative paths
function sanitizenamestr(var s:ansistring): integer;
function sanitizepath(s0,s1:ansistring): integer;

//check full name against reserved and illegal characters, and reserved filenames
function checkfiledirname(s: ansistring): integer;
function checkfiledirname_acceptblank(s: ansistring): integer;

//check for some special characters in filenames in *x environments
function checkescapedoutname(s: ansistring):ansistring;

//escape filenames in *x environments likely using Gnome or KDE as desktop environment (Linux, *BSD)
function escapefilenamelinuxlike(s: ansistring; desk_env: byte): ansistring;

//cross platform filename escaping
function escapefilename(s: ansistring; desk_env: byte): ansistring;

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
//command string sanitization for real console instance, meant as additional control to validatecl
function validatecl_console(var s: ansistring): integer;

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

//test if file type can be possibly read as plain text
function istexttype(s:ansistring):boolean;

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

function dirExtractFileName(const FileName: ansistring): ansistring;

function dirExtractFilePath(const FileName: ansistring): ansistring;

function isawebservice(s:ansistring):boolean;

function check7zvolume(s:ansistring):boolean;

function checkUNCpath(s:ansistring):boolean;

function checklinapp(s:ansistring):boolean;

function checkmacapp(s:ansistring):boolean;

function checkmacappbin(s:ansistring):boolean;

function smartsortstring(s:ansistring):ansistring;

function GetUNCName(const uLocalPath: ansistring): ansistring;

function strisascii(s: ansistring):boolean;

function strsafeenc(s:ansistring):ansistring;

function strsafedec(s:ansistring):ansistring;

procedure cutendspaces(var s1:ansistring); //if archive name ends with spaces (allowed, since they are before the extension) cut them to get a valid folder name (can't end with spaces)

procedure cutenddot(var s1:ansistring); //if archive name ends with dot cut it to get a valid folder name (can't end with dot)

procedure cutendforbid(var s1:ansistring); //if archive name ends with non allowed characters cut them to get a valid folder name

procedure cutenddelim(var s1:ansistring; inparam:ansistring); //if archive name ends with dot cut it to get a valid folder name (can't end with dot)

function unpadaddress(s:ansistring):ansistring;

procedure write_header(var tf:text); //write UTF-8 with BOM text file header
function read_header(var tf:text; fm:integer):boolean; //read optional UTF-8 with BOM text file header
function read_header_strict(var tf:text):boolean; //read mandatory UTF-8 with BOM text file header
function read_header_16BE(var tf:text):boolean; //detect UTF-16 BE BOM
function read_header_16LE(var tf:text):boolean; //detect UTF-16 LE BOM
function read_header_7(var tf:text):boolean; //detect UTF-7 BOM
function udeletefile(s:ansistring):boolean; //on Windows changes file attributes to allow file to be deleted
function upredeletefile(s:ansistring):boolean; //on Windows changes file attributes to allow file to be deleted separately

//read magic bytes to gather information about the file type (image formats)
function getmagicbytes_img(s:ansistring):ansistring;
//read magic bytes to gather information about the file type (archive formats)
function getmagicbytes_arc(s:ansistring):ansistring;

//check if a path (of dir or file) is inside a PeaZip's temp path
function pathistmp(s:ansistring):boolean;

//split a string in words separated by space: ignore empty strings, ignore quotes
function peasplitstring(s:AnsiString; var sfin:TStringList):integer;

//launch TProcess with command line or parameters, optional log: pmode 0 use cl in Process.CommandLine; pmode 1 Process.ParseCmdLine cl, don't assign a value to Process.CommandLine (even empty CommandLine disables Parameters)
function peapexecute(var P:TProcessUTF8; var cl:ansistring): integer;
//launch TProcess with parameters, optional log
function peapparamexecute(var P:TProcessUTF8): integer;

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
  if FindFirst(dir + mask, fattrib, r) = 0 then //limited by FindFirst/FindNext speed
  begin
    try
      if upcase(mode) = 'DETAILS' then
        repeat
          if ((showhidden=true) or {$IFDEF MSWINDOWS}((r.Attr and faHidden)=0){$ELSE}(r.Name[1] <> '.'){$ENDIF}) then
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
        if ((showhidden=true) or {$IFDEF MSWINDOWS}((r.Attr and faHidden)=0){$ELSE}(r.Name[1] <> '.'){$ENDIF}) then
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
  var nfiles, ndirs, tsize: qword
  //number of files and dirs found, total size
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
        if ((showhidden=true) or {$IFDEF MSWINDOWS}((r.Attr and faHidden)=0){$ELSE}(r.Name[1] <> '.'){$ENDIF}) then
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

//optionally recursive function to count files, total size, and return string with all files/dirs names (sorted as in Find function)
function rCountSizeN(dir, mask: ansistring;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize: qword;
  //number of files and dirs found, total size
  var strn: ansistring
  //string with found files/dirs names
  ): integer;
var
  r: TSearchRec;
begin
  rCountSizeN := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if FindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((showhidden=true) or {$IFDEF MSWINDOWS}((r.Attr and faHidden)=0){$ELSE}(r.Name[1] <> '.'){$ENDIF}) then
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          tsize := tsize + r.Size;
          strn := strn + r.Name;
        end;
      until findnext(r) <> 0;
    except
      FindClose(r);
      rCountSizeN := LIST_ERROR;
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
            rCountSizeN(dir + (r.Name) + DirectorySeparator, mask, fattrib,
              recur, nfiles, ndirs, tsize, strn);
        until findnext(r) <> 0;
      except
        FindClose(r);
        rCountSizeN := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rCountSizeN = INCOMPLETE_FUNCTION then
    rCountSizeN := SUCCESS;
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
   s_ext := lowercase(extractfileext(s));
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
   '.tmp':  //multipart .xxx.tmp
      begin
      setlength(s_name, length(s_name) - length(s_ext));
      j:=1;
      repeat
         j := j + 1;
         if j < 10 then s_ext := '.00' + IntToStr(j)+'.tmp'
         else
            if j < 100 then s_ext := '.0' + IntToStr(j)+'.tmp'
            else s_ext := '.' + IntToStr(j)+'.tmp';
         k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
         if k = 0 then fsize := fsize + r.Size
         else
            begin  //cases when .tmp extension is removed in intermediate volumes (only first and last volumes are kept open, with .tmp extension)
            try setlength(s_ext, length(s_ext) - 4); except end;
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then fsize := fsize + r.Size;
            end;
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
      s_ext:='.r00';
      if FindFirst(s_path + s_name + s_ext, faAnyFile, r)=0 then //legacy multipart rar .rxx
         begin
         j:=-1;
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.r0' + IntToStr(j)
            else s_ext := '.r' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
         s_ext:='.s00';
         j:=-1;
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.s0' + IntToStr(j)
            else s_ext := '.s' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
         s_ext:='.t00';
         j:=-1;
         repeat
            j := j + 1;
            if j < 10 then s_ext := '.t0' + IntToStr(j)
            else s_ext := '.t' + IntToStr(j);
            k := FindFirst(s_path + s_name + s_ext, faAnyFile, r);
            if k = 0 then
               fsize := fsize + r.Size;
            FindClose(r);
         until k <> 0;
         //continue with u, v, w, etc
         end
      else
         begin
         s_ext:='.rar';
         j:=0;
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

{$IFDEF MSWINDOWS}
function wintrailing(var s:ansistring): boolean;
begin
result:=true;
if s='' then begin result:=false; exit; end;
if s[length(s)]=' ' then exit;
if s[length(s)]='.' then exit;
result:=false;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function winreserved(var s:ansistring): boolean;
begin
result:=false;
cutextension(s);
s:=upcase(s);
case s of
'CON','PRN','AUX','NUL',
'COM1','COM2','COM3','COM4','COM5','COM6','COM7','COM8','COM9',
'LPT1','LPT2','LPT3','LPT4','LPT5','LPT6','LPT7','LPT8','LPT9': result:=true;
end;
end;
{$ENDIF}

procedure replacespecialpathchars(var s:ansistring);//replace special characters disallowd in Windows paths
begin
{$IFDEF MSWINDOWS}
s:=StringReplace(s,':','_', [rfReplaceAll]); //remove : as in unit name in absolute paths
s:=StringReplace(s,'.'+DirectorySeparator,'_'+DirectorySeparator,[rfReplaceAll]); //remove last dir name char if dot (not allowed), only this pattern
s:=StringReplace(s,' '+DirectorySeparator,'_'+DirectorySeparator,[rfReplaceAll]); //remove last dir name char if space (not allowed), only this pattern
{$ENDIF}
end;

function sanitizenamestr(var s:ansistring):integer;
begin
result:=-1;
if s='' then begin result:=0; exit; end;
s:=StringReplace(s,'../','', [rfReplaceAll]);
s:=StringReplace(s,'..\','', [rfReplaceAll]);
s:=StringReplace(s,'..'+DirectorySeparator,'', [rfReplaceAll]);
{s:=StringReplace(s,'./','', [rfReplaceAll]);
s:=StringReplace(s,'.\','', [rfReplaceAll]);
s:=StringReplace(s,'.'+DirectorySeparator,'', [rfReplaceAll]);}
result:=0;
end;

function sanitizepath(s0,s1:AnsiString): integer;
var
  s:AnsiString;
begin
result:=-1;
if s1='' then begin result:=0; exit; end;
s:=ExpandFileName(s0+s1);
if pos(s0,s)<>1 then exit;
result:=0;
end;

function checkfiledirname(s: ansistring): integer;
//function errs on safe side to prevent cross platform interoperability issues
var
   sf: ansistring;
   i: integer;
begin
result := -1;
if s = '' then exit;
//illegal characters, full name
for i := 0 to 31 do
   if pos(char(i), s) <> 0 then exit;
//reserved characters, full name
if pos('*', s) <> 0 then exit;
if pos('?', s) <> 0 then exit;
if pos('<', s) <> 0 then exit;
if pos('>', s) <> 0 then exit;
if pos('|', s) <> 0 then exit;
if pos('       ', s) <> 0 then exit;
{$IFDEF MSWINDOWS}
if pos('"', s) <> 0 then exit;
if wintrailing(s)=true then exit;
{$ENDIF}
sf := extractfilename(s);
//reserved characters, filename only (others are checked for the full name)
if pos('\', sf) <> 0 then exit;
if pos('/', sf) <> 0 then exit;
if pos(':', sf) <> 0 then exit;
{$IFDEF MSWINDOWS}
//reserved filenames (Windows)
if winreserved(sf)=true then exit;
{$ENDIF}
result := 0;
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
//a more robust escaping procedure should be based on the POSIX compliant method of replacing ' with '\''
//however TProcess.CommandLine does not support this method of escaping, so the command line would work if pasted in shells but not from within the app
//the solution is replacing TProcess.CommandLine with TProcess.Executable and a list of TProcess.Parameters rewriting the sections composing the command lines, this method will also supersede most of the checks peformed in custom escaping functions
var
  varstr,dstr: ansistring;
  i,j: integer;
begin
  varstr := s;
  dstr:=correctdelimiter(s); //correctdelimiter swaps ' and " quotes, but that cannot guarantee against both characters being used in the same string
  case pmode of
     0: begin //replace correctdelimiter quote character with ? jolly
        i := 1;
        repeat
        i := pos(dstr, varstr);
        if i > 0 then
           varstr[pos(dstr, varstr)] := '?';
        until i = 0;
        end;
     1: begin //quote correctdelimiter quote character with \, command will be passed as list of parameters with ReadBackslash set to true
        //escape \ char with \
        j := 1;
        i := 1;
        repeat
        i := pos('\', varstr,j);
        if i > 0 then
           begin
           insert('\',varstr,i);
           j:=i+2;
           end;
        until i = 0;
        //escape quote char with \
        j := 1;
        i := 1;
        repeat
        i := pos(dstr, varstr,j);
        if i > 0 then
           begin
           insert('\',varstr,i);
           j:=i+2;
           end;
        until i = 0;
        end;
  end;
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
  result := varstr;
end;

function escapefilename(s: ansistring; desk_env: byte): ansistring;
begin
{$IFDEF MSWINDOWS}
result := s;
{$ELSE}
result := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
end;

function stringdelim(s:ansistring): ansistring;
var
   cdelim:ansistring;
begin
cdelim:=correctdelimiter(s);
if pos(cdelim, s)<>0 then begin result:='#invalid string#'; exit; end;//error, quotation character already used in the string: string is sanitized for security and replaced with a rejection string #invalid string# which is checked by validatecl before execution, assuring the containing command line will be discarded
result := cdelim+s+cdelim;
end;

function stringundelim(s:ansistring): ansistring;
var
   cdelim,st:ansistring;
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
  cl:AnsiString;
begin
  result := -1;
  if s = '' then
    exit;
  if (desk_env = 10) then //continue for gnome=1 kde=2 and unknown desktop manager =0, exit for Windows=10
    exit;
  if desk_env = 20 then // Darwin=20
    begin
    cl:='open ' + stringdelim(escapefilename(s, desk_env));
    end
  else
    begin
    cl:='xdg-open ' + stringdelim(escapefilename(s, desk_env));
    end;
  if validatecl(cl)<>0 then exit;
  P := TProcessUTF8.Create(nil);
  P.Options := [poWaitOnExit];
  peapexecute(P,cl);
  result := P.ExitStatus;
  P.Free;
end;

procedure winexplorepath(s: ansistring);
var
  P: TProcessUTF8;
  cl: ansistring;
begin
{$IFDEF MSWINDOWS}
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
if validatecl(cl)<>0 then exit;
P := TProcessUTF8.Create(nil);
peapexecute(P,cl);
P.Free;
{$ENDIF}
end;

procedure macexplorepath(s: ansistring);
var
  P: TProcessUTF8;
  cl:AnsiString;
begin
if s = '' then  exit;
cl:='open -R '+stringdelim(escapefilename(s, 20));
if validatecl(cl)<>0 then exit;
P := TProcessUTF8.Create(nil);
P.Options := [poWaitOnExit];
peapexecute(P,cl);
P.Free;
end;

procedure cp_search_linuxlike(desk_env: byte);
var
  P: TProcessUTF8;
  cl:AnsiString;
begin
  if (desk_env = 0) or (desk_env = 10) then exit;
  case desk_env of
     1: cl:='xdg-open /';//'gnome-search-tool';
     2: cl:='kfind';
     20: cl:='open /';
     end;
  if validatecl(cl)<>0 then exit;
  P := TProcessUTF8.Create(nil);
  P.Options := [poWaitOnExit];
  peapexecute(P,cl);
  P.Free;
end;

function correctdelimiter(s:AnsiString): AnsiString;
//TProcess does not expand variables ($, ~, %%) even if weak quotes " are used, neither in parameters nor in commandline
//UNLESS the process itself runs the command through a shell (cmd.exe, /bin/sh, bash, xterm, gnome-terminal, konsole, open on macOS...)
//in this specific case it is necessary to carefully evaluate if weak quotes " are applied AND a variable expansion character is used without strong quotes '
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
  s := GetEnvironmentVariable('HOME'); //env variable should be always set as required by POSIX standard
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
begin
s:=GetTempDir(false); //provides user's path on Windows and macOS
if s = '' then get_desktop_path(s);
//fallback to ensure a private path is returned on POSIX compliant systems if system temp path is returned
{$IFNDEF MSWINDOWS}
if (s = '/tmp/') or (s = '/var/tmp/') then
  begin
  get_home_path(s);
  if DirectoryExists(s+'.cache',False) then s:=s+'.cache'
  else get_desktop_path(s);
  end;
{$ENDIF}
setendingdirseparator(s);
end;

function fget_usrtmp_path:ansistring;
var
   s:ansistring;
begin
get_usrtmp_path(s);
result:=s;
end;

procedure cutextension(var s: ansistring);//uses a small set of rules to avoid cutting strings which are not really meant as extensions
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
//function errs on safe side to prevent cross platform interoperability issues
var
   {$IFDEF MSWINDOWS}
   s1: ansistring;
   {$ENDIF}
   i: integer;
begin
result := -1;
if (s = '') or (s='.') or (s='..') then exit;
//illegal characters (no problem for additional UTF8 bytes since have MSB set to 1 to avoid conflict with those control characters)
for i := 0 to 31 do
   if pos(char(i), s) <> 0 then exit;
//reserved characters
if pos('\', s) <> 0 then exit;
if pos('/', s) <> 0 then exit;
if pos(':', s) <> 0 then exit;
if pos('*', s) <> 0 then exit;
if pos('?', s) <> 0 then exit;
if pos('<', s) <> 0 then exit;
if pos('>', s) <> 0 then exit;
if pos('|', s) <> 0 then exit;
if pos('       ', s) <> 0 then exit;
{$IFDEF MSWINDOWS}
if pos('"', s) <> 0 then exit;
if wintrailing(s)=true then exit;
//reserved filenames (Windows)
s1 := extractfilename(s);
if winreserved(s1) then exit;
{$ENDIF}
result := 0;
end;

//wrapper for filename check, accepts empty or valid file name as valid input for special purposes i.e. replace string in file name with a valid string or nothing (it is needed a separate final check for the file name to not be empty)
function checkfilename_acceptblank(s: ansistring): integer;
begin
result := -1;
if s = '' then result:=0
else result:=checkfilename(s);
end;

function vis7z(s:ansistring): boolean;
begin
result:=false;
if (pos('bin'+directoryseparator+'7z'+directoryseparator,s)<>0) or //local binary, supports alias7z
   (pos('7z',s)=1) or (pos('7zalt',s)=1) or (pos('7za',s)=1) or (pos('7zr',s)=1) or (pos('7zz',s)=1) //system binary, does not support alias7z, but checks most common 7z aliases
then result:=true;
end;

function visarc(s:ansistring): boolean;
begin
result:=false;
if (pos('bin'+directoryseparator+'arc'+directoryseparator,s)<>0) or
   (pos('arc',s)=1)
then result:=true;
end;

function visrar(s:ansistring): boolean;
begin
result:=false;
if (pos(directoryseparator+'rar',s)<>0) or //rar found in custom path
   (pos('rar',s)=1) //rar found in system paths
then result:=true;
end;

function viszpaq(s:ansistring): boolean;
begin
result:=false;
if (pos('bin'+directoryseparator+'zpaq'+directoryseparator,s)<>0) or
   (pos('zpaq',s)=1)
then result:=true;
end;

function validatecl(var s: ansistring): integer;
var
  i: integer;
  s1,delimch:ansistring;
begin
result := -1;
if s = '' then   exit;

if pos('#invalid string#',s1)<>0 then exit;//rejection string, in depth safeguard to discard the containing cl if an appropriate check did not happened earlier: if a string sanitization routine has failed to produce a valid output it replaces the offending input with the rejection string

for i := 0 to 31 do if pos(char(i), s) <> 0 then exit; //illegal characters

delimch := correctdelimiter(s);

s1:=s;

//check for password field, if applicable for the inferred operation type
//if password field is found, do not check the filed for special characters
if vis7z(s) or visarc(s) or visrar(s) then
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

if visarc(s) or visrar(s) then
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

if viszpaq(s) then
   if pos(' -key ',s)<>0 then
      begin
      s1:=copy(s,pos(' -key ',s)+7,length(s)-pos(' -key ',s)-6); //skip one character, which is always the delimiter
      s1:=copy(s1,pos(delimch,s1)+1,length(s1)-pos(delimch,s1)); //field is always delimited, even if not needed
      s1:=copy(s,1,pos(' -key ',s))+s1;
      end;

if (pos(directoryseparator+'pea',s)<>0) then
   if pos('BATCH',s)<>0 then
      begin
      s1:=copy(s,pos('BATCH',s)+6,length(s)-pos('BATCH',s)-5);
      if pos('FROMCL',s1)<>0 then s1:=copy(s1,pos('FROMCL',s1)+7,length(s1)-pos('FROMCL',s1)-6)
      else s1:=copy(s1,pos('NOKEYFILE',s1)+9,length(s1)-pos('NOKEYFILE',s1)-8);
      s1:=copy(s,1,pos('BATCH',s))+s1;
      end;

if pos('|',s1)<>0 then exit;//critical pipe
if pos('&',s1)<>0 then exit;//critical ampersand command chaining
if pos(';',s1)<>0 then exit;//critical semicolon command chaining
if pos('<',s1)<>0 then exit;//critical input redirection
if pos('>',s1)<>0 then exit;//critical output redirection, disable generation of command scripts for tar with pipe
if pos('`',s1)<>0 then exit;//critical backtick command substitution
if pos('$',s1)<>0 then exit;//critical dollar variable expansion
//if pos('!',s1)<>0 then exit;//exclamation history expansion (needed by the syntax of some backends, e.g. 7z include switch)
//if pos('"',s1)<>0 then exit;//quote double (input quotation must be correctly handled before being passed here, relevant procedure stringdelim)
//if pos('''',s1)<>0 then exit;//quote single
//if pos('\',s1)<>0 then exit;//backslash escape character (needed as path separator for UNC and Windows, also used in escapefilenamelinuxlike when passing command as separate parameters on non-Windows systems)
//line separators \r \n and Unicode U+2028, U+2029 should not be supported in TProcess.Commandline (not a real console, limited support for meta characters)
if pos('#',s1)<>0 then exit;//hash bash comment or special character
if pos('~',s1)<>0 then exit;//tilde home directory expansion

if pos('       ',s1)<>0 then exit; //more than 6 consecutive spaces may be intentional attempt to hamper readability (as in 7-Zip)
result := 0;
end;

function validatecl_console(var s: ansistring): integer;
var
  i: integer;
  s1,delimch:ansistring;
begin
result := -1;

{$IFNDEF MSWINDOWS}
{$IFNDEF DARWIN}
if s='' then exit;
if pos('"',s)<>0 then exit;
if pos('\r',s)<>0 then exit; //carriage return
if pos('\n',s)<>0 then exit; //line feed
if pos(#$E2#$80#$A8,s)<>0 then exit; //Unicode line separator U+2028
if pos(#$E2#$80#$A9,s)<>0 then exit; //Unicode paragraph separator U+2028
//if pos('\',s)<>0 then exit;
{$ENDIF}
{$ENDIF}
result := 0;
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
    //traditioanl archive types 0..99
    '.7z', '.cb7': testext := 0;
    '.bz', '.bz2', '.bzip2', '.bzip', '.tbz2', '.tbz', '.tbzip2', '.tbzip', '.tb2': testext := 1;
    '.gz', '.gzip', '.tgz', '.tpz', '.cpgz' : testext := 2;
    '.tar', '.cbt': testext := 5;
    '.zip', '.cbz', '.smzip', '.ppmd': testext := 6; //, '.z01' operations should not be initiated from those volumes
    '.arj': testext := 7;
    '.cpio': testext := 10;
    '.lz': testext := 11;
    '.lzh': testext := 12;
    '.rar', '.cbr': testext := 13;//, '.r00' operations should not be initiated from those volumes
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
    '.ar': testext := 21; //Unix archiver

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
    '.apk', '.xapk', '.apkm', '.apks', '.aab': testext := 135; //Android packages (.zip derived)
    '.mcaddon', '.mcmeta', '.mcpack', '.mcproject', '.mcstructure', '.mctemplate', '.mcworld': testext := 136; //Minecraft packages (.zip derived)
    //'.app' macOS app bundles are not assigned as file extension, being treated as folders
    '.zim': testext := 137;

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
    '.vmdk', '.ova': testext := 210;//VMware formats
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
    '.exe', '.dll', '.sys', //most executables can be opened
    '.msi', '.msp', '.msu': testext := 500;
    '.sxc', '.sxd', '.sxi', '.sxw', '.stc', '.std', '.sti', '.stw', '.sxg', '.sxm', //OOo 1.x legacy filetypes
    '.ods', '.ots', '.odm', '.oth', '.oxt', '.odb', '.odf', '.odg', '.otg', '.odp', '.otp', '.odt', '.ott', //OOo filetypes
    '.gnm', //Gnumeric spreadsheet
    '.pmdx', '.pmvx', '.tmdx', '.prdx': testext := 501; //SoftMaker Office compressed formats
    '.doc', '.dot', '.xls', '.xlt', '.ppt', '.pps', '.pot', //non executable COMPOUND files
    '.docx', '.dotx', '.xlsx', '.xltx', '.pptx','.mpp', //MS compressed formats, treated as othes MS Office formats
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
    5001: testinput:=11;//brotli
    5002: testinput:=12;//zstandard
    10000: testinput := 4; //use 7z backend to handle split archives
    10001: testinput := 1; //Pea
  end;
end;

function istexttype(s:ansistring):boolean;
var
  sl:ansistring;
begin
sl:=lowercase(s);
case sl of
   '.txt','.asc','.readme',
   '.bat','.pif','.scr','.vbs','.cmd','.reg','.sh','.command','.csh',
   '.ini','.inf','.cfg','.config',
   '.lst','.log',
   '.lnk',
   '.csv',
   '.eml',
   '.html','.htm','.xml','.xhtml','.xht','.mht','.mhtml','.url',
   '.md','.markdown': result:=true
   else result:=false;
end;
end;

function testpcomp(var s:ansistring):integer;
var
   ext:ansistring;
begin
ext:=lowercase(extractfileext(s));
case ext of
   '.lnk','.txt','.asc','.readme','.rtf','.wri','.ini','.inf','.cfg','.config','.lst','.log','.mid','.midi',
   '.eml','.htm','.html','.xml','.mht','.mhtml','.url','.css','.xhtml','.md','.markdown',
   '.bat','.pif','.scr','.vbs','.cmd','.reg','.sh','.command','.csh',
   '.pas','.pp','.h','.c','.hh','.cpp','.cc','.cxx','.hxx','.cs','.java','.pl','.pm','.php','.py','.p','rb',
   '.js','.asp','.aspx','.vb','.manifest': begin result:=10; exit; end;
   '.xls','.xlt','.gnm','.csv',
   '.ani','.cur','.ico','.icl': begin result:=30; exit; end;
   '.doc','.dot',
   '.dll','.sys','.so','.dylib',
   '.bmp','.tga','.tif','.tiff',
   '.wav': begin result:=40; exit; end;
   '.exe',
   '.db','.dbf','.mdb','.nsf',
   '.pps','.ppt','.odp': begin result:=50; exit; end;
   '.gif': begin result:=70; exit; end;
   '.xlsx','.ods','.numbers',
   '.docx','.odt','.pages',
   '.pptx','.key',
   '.svg','.ps','.eps','.cdr','.ai','.psd','.psp': begin result:=80; exit; end;
   '.pdf',
   '.png',
   '.jpg','.jpe','.jpeg','.jif', '.jfif', '.jfi','.jpx','.jp2','.j2k': begin result:=90; exit; end;
   '.avi','.mpg','.mpeg','.xvid','.divx','.mp4','.mov','.3gp','.wmv','.swf','.flv','.fla',
   '.mp3','.wma','.aiff','.ogg': begin result:=95; exit; end;
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

function dirExtractFileName(const FileName: ansistring): ansistring;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators;
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function dirExtractFilePath(const FileName: ansistring): ansistring;
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators;
  while (i > 0) and not CharInSet(FileName[i],EndSep) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function isawebservice(s:ansistring):boolean;
begin
if (pos('http://',s)=1) or (pos('https://',s)=1) or (pos('www.',s)=1) then
   result:=true
else
   result:=false;
end;

function check7zvolume(s:ansistring):boolean;
begin
check7zvolume:=false;
if length(s)>=4 then
   if copy(s,1,4)='\\.\' then check7zvolume:=true;
end;

function checkUNCpath(s:ansistring):boolean;
begin
checkUNCpath:=false;
if length(s)>=2 then
   if copy(s,1,2)=DirectorySeparator+DirectorySeparator then checkUNCpath:=true;
end;

function checklinapp(s:ansistring):boolean;
begin
result:=false;
if (fileexists('/bin/'+s)) or
   (fileexists('/usr/bin/'+s)) then result:=true;
end;

function checkmacapp(s:ansistring):boolean;
begin
result:=false;
if (checkdirexists('/Applications/'+s)) or
   (checkdirexists('/System/Applications/'+s)) then result:=true;
end;

function checkmacappbin(s:ansistring):boolean;
begin
result:=false;
if (checkdirexists('/usr/local/'+s)) then result:=true;
end;

function smartsortstring(s:ansistring):ansistring;
var
   i,firstpos,nnumb:Integer;
   firstnumber:boolean;
   snumb,fnumb:string;
begin
firstnumber:=false;
firstpos:=0;
nnumb:=0;
snumb:='';
fnumb:='';
for i:=1 to length(s) do
   begin
   if s[i] in ['0'..'9'] then
      if firstnumber=false then
         begin
         firstnumber:=true;
         firstpos:=i;
         if s[i]<>'0' then
            begin
            nnumb:=1;
            snumb:='9';
            fnumb:=s[i];
            end;
         end
      else
         begin
         if s[i]<>'0' then nnumb:=1;
         if nnumb=1 then
            begin
            snumb:=snumb+'9';
            fnumb:=fnumb+s[i];
            end;
         end
   else
      if firstnumber=false then
         begin
         end
      else
         begin
         //fnumb:=fnumb+' ';
         break;
         end;
   end;
if firstnumber=true then
   result:=copy(s,1,firstpos-1)+snumb+' '+fnumb+copy(s,firstpos,length(s)+1-firstpos)
else
   result:=s;
end;

function GetUNCName(const uLocalPath: ansistring): ansistring;
var
  BufferSize: DWord;
  LocalPath: widestring;
  s:ansistring;
  wb: array of wchar;
  ws:widestring;
  DummyBuffer: wchar;
  Error: DWord;
  i:integer;
begin
{$IFDEF MSWINDOWS}
Result:='';
localpath:=utf8decode(ulocalpath);
if (upcase(LocalPath)='A:\') or (upcase(LocalPath)='B:\') or (upcase(LocalPath)='C:\') then exit;
BufferSize := 2;
WNetGetUniversalNameW(PWChar(LocalPath), UNIVERSAL_NAME_INFO_LEVEL, @DummyBuffer, BufferSize);
setlength(wb,buffersize);
try
   Error := WNetGetUniversalNameW(PWChar(LocalPath), UNIVERSAL_NAME_INFO_LEVEL, @wb[0], BufferSize);
   if Error <> NO_ERROR then
      begin
      Result:='';
      end;
   s:='';
   if buffersize>0 then
      for i:=0 to buffersize-1 do
         begin
         if i>1 then
            if ord(wb[i])<>0 then begin ws:=wb[i]; s:=s+AnsiString(ws); end;
         end;
   Result:=s;
finally
end;
{$ENDIF}
end;

function strisascii(s: ansistring):boolean;
var
   i:integer;
begin
for i:=1 to Length(s) do
   if s[i]>#127 then
      begin
      result:=false;
      exit;
      end;
result:=true;
end;

function strsafeenc(s:ansistring):ansistring;
var
   i:integer;
   sout:ansistring;
begin
if s='' then
   begin
   result:='';
   exit;
   end;
for i:=1 to Length(s) do sout:=sout+inttostr(ord(s[i]))+'_';
result:=sout;
end;

function strsafedec(s:ansistring):ansistring;
var
   stch,stin,stout:ansistring;
begin
if s='' then
   begin
   result:='';
   exit;
   end;
stin:=s;
repeat
stch:=copy(stin,1,pos('_',stin)-1);
stout:=stout+char(strtoint(stch));
stin:=copy(stin,pos('_',stin)+1,length(stin)-pos(' ',stin)-1);
until (stin='');
result:=stout;
end;

procedure cutendspaces(var s1:ansistring); //if archive name ends with spaces (allowed, since they are before the extension) cut them to get a valid folder name (can't end with spaces)
var endwithspace:boolean;
begin
endwithspace:=true;
repeat
if s1<>'' then
   if s1[length(s1)]=' ' then setlength(s1,length(s1)-1)
   else endwithspace:=false;
until endwithspace=false;
if s1='' then s1:='noname';
end;

procedure cutenddot(var s1:ansistring); //if archive name ends with dot cut it to get a valid folder name (can't end with dot)
begin
if s1<>'' then if s1[length(s1)]='.' then s1[length(s1)]:='_';
if s1='' then s1:='noname';
end;

procedure cutendforbid(var s1:ansistring); //if archive name ends with non allowed characters cut them to get a valid folder name
var
  endwithchar:boolean;
begin
endwithchar:=true;
repeat
{$IFDEF MSWINDOWS}
if s1<>'' then
   if s1[length(s1)]='"' then setlength(s1,length(s1)-1)
{$ELSE}
if s1<>'' then
   if s1[length(s1)]='''' then setlength(s1,length(s1)-1)
{$ENDIF}
else endwithchar:=false;
until endwithchar=false;
if s1='' then s1:='noname';
end;

procedure cutenddelim(var s1:ansistring; inparam:ansistring); //if archive name ends with dot cut it to get a valid folder name (can't end with dot)
begin
if s1<>'' then if s1[length(s1)]=correctdelimiter(inparam) then setlength(s1,length(s1)-1);
if s1='' then s1:='noname';
end;

function unpadaddress(s:ansistring):ansistring;
begin
result:=copy(s,2,length(s)-2);
end;

procedure write_header(var tf:text);
begin
write(tf,char($ef));
write(tf,char($bb));
write(tf,char($bf));
end;

function read_header(var tf:text; fm:integer):boolean;
var
   carr:array [1..3] of char;
begin
result:=false;
read(tf,carr[1]);
read(tf,carr[2]);
read(tf,carr[3]);
if (carr[1]<>char($ef)) or (carr[2]<>char($bb)) or (carr[3]<>char($bf)) then
  begin
  filemode:=fm;
  Reset(tf); //if BOM header is not found, return text file to first character
  end;
result:=true; //if no error is encountered, the function is successful
end;

function read_header_strict(var tf:text):boolean;
var
   c:char;
begin
result:=false;
read(tf,c);
if c<>char($ef) then exit;
read(tf,c);
if c<>char($bb) then exit;
read(tf,c);
if c<>char($bf) then exit;
result:=true; //if BOM header is found, the function is successful
end;

function read_header_16BE(var tf:text):boolean;
var
   c:char;
begin
result:=false;
read(tf,c);
if c<>char($fe) then exit;
read(tf,c);
if c<>char($ff) then exit;
result:=true; //if BOM 16BE header is found, the function is successful
end;

function read_header_16LE(var tf:text):boolean;
var
   c:char;
begin
result:=false;
read(tf,c);
if c<>char($ff) then exit;
read(tf,c);
if c<>char($fe) then exit;
result:=true; //if BOM 16BE header is found, the function is successful
end;

function read_header_7(var tf:text):boolean;
var
   c:char;
begin
result:=false;
read(tf,c);
if c<>char($2b) then exit;
read(tf,c);
if c<>char($2f) then exit;
read(tf,c);
if c<>char($76) then exit;
result:=true; //if UTF-7 BOM header is found, the function is successful
end;

function upredeletefile(s:ansistring):boolean;
var attr:integer;
begin
{$IFDEF MSWINDOWS}
try
attr:=filegetattr(s);
attr:=attr and (not faReadOnly);
attr:=attr and (not faHidden);
attr:=attr and (not faSysFile);
filesetattr(s,attr);
result:=true;
except
result:=false;
end;
{$ENDIF}
end;

function udeletefile(s:ansistring):boolean;
var attr:integer;
begin
{$IFDEF MSWINDOWS}
try
attr:=filegetattr(s);
attr:=attr and (not faReadOnly);
attr:=attr and (not faHidden);
attr:=attr and (not faSysFile);
filesetattr(s,attr);
except end;
{$ENDIF}
udeletefile:=DeleteFile(s);
end;

//read magic bytes to gather information about the file type (image formats)
function getmagicbytes_img(s:ansistring):ansistring;
var
  f:file of Byte;
  sbuf:array[0..7] of byte;
  n:integer;
  fext:AnsiString;
begin
result:='';
fext:='';
try
fext:=lowercase(ExtractFileExt(s));
result:=fext;
assignfile(f,s);
filemode:=0;
reset(f);
blockread(f,sbuf,8,n);
close(f);
if n<8 then exit;
if (sbuf[0]=66) and (sbuf[1]=77) then begin result:=fext+'/BMP'; exit; end;
if (sbuf[0]=71) and (sbuf[1]=73) and (sbuf[2]=70) and (sbuf[3]=56) then begin result:=fext+'/GIF'; exit; end;
if (sbuf[0]=105) and (sbuf[1]=99) and (sbuf[2]=110) and (sbuf[3]=115) then begin result:=fext+'/ICNS'; exit; end;
if (sbuf[0]=0) and (sbuf[1]=0) and (sbuf[2]=1) and (sbuf[3]=0) then begin result:=fext+'/ICO'; exit; end;
if (sbuf[0]=255) and (sbuf[1]=216) and (sbuf[2]=255) then begin result:=fext+'/JPG'; exit; end;
if (sbuf[0]=80) and (sbuf[1]=50) and (sbuf[2]=10) then begin result:=fext+'/PGM-ASCII'; exit; end;
if (sbuf[0]=80) and (sbuf[1]=53) and (sbuf[2]=10) then begin result:=fext+'/PGM_BINARY'; exit; end;
if (sbuf[0]=137) and (sbuf[1]=80) and (sbuf[2]=78) and (sbuf[3]=71) then begin result:=fext+'/PNG'; exit; end;
if (sbuf[0]=80) and (sbuf[1]=51) and (sbuf[2]=10) then begin result:=fext+'/PPM-ASCII'; exit; end;
if (sbuf[0]=80) and (sbuf[1]=54) and (sbuf[2]=10) then begin result:=fext+'/PPM_BINARY'; exit; end;
if (sbuf[0]=77) and (sbuf[1]=77) then begin result:=fext+'/TIFF-BE'; exit; end;
if (sbuf[0]=73) and (sbuf[1]=73) then begin result:=fext+'/TIFF-LE'; exit; end;
if (sbuf[0]=47) and (sbuf[1]=42) and (sbuf[2]=32) and (sbuf[3]=88) and (sbuf[4]=80) and (sbuf[5]=77) then begin result:=fext+'/XPM'; exit; end;
except
end;
end;

//read magic bytes to gather information about the file type (archive formats)
function getmagicbytes_arc(s:ansistring):ansistring;
var
  f:file of Byte;
  sbuf:array[0..7] of byte;
  n:integer;
  fext:AnsiString;
begin
result:='';
fext:='';
try
fext:=lowercase(ExtractFileExt(s));
result:=fext;
assignfile(f,s);
filemode:=0;
reset(f);
blockread(f,sbuf,8,n);
close(f);
if n<8 then exit;
//most common
if (sbuf[0]=80) and (sbuf[1]=75) then begin result:=fext+'/ZIP'; exit; end;
if (sbuf[0]=55) and (sbuf[1]=122) and (sbuf[2]=188) and (sbuf[3]=175) and (sbuf[4]=39) and (sbuf[5]=28) then begin result:=fext+'/7Z'; exit; end;
if (sbuf[0]=82) and (sbuf[1]=97) and (sbuf[2]=114) and (sbuf[3]=33) and (sbuf[4]=26) and (sbuf[5]=7) and (sbuf[6]=0) then begin result:=fext+'/RAR4-'; exit; end;
if (sbuf[0]=82) and (sbuf[1]=97) and (sbuf[2]=114) and (sbuf[3]=33) and (sbuf[4]=26) and (sbuf[5]=7) and (sbuf[6]=1) then begin result:=fext+'/RAR5+'; exit; end;
//offset 257 if (sbuf[0]=117) and (sbuf[1]=115) and (sbuf[2]=116) and (sbuf[3]=97) and (sbuf[4]=114) then begin result:=fext+'/TAR'; exit; end;
//less common
//offset 7 if (sbuf[0]=42) and (sbuf[1]=42) and (sbuf[2]=65) and (sbuf[3]=67) and (sbuf[4]=69) and (sbuf[5]=42) and (sbuf[6]=42) then begin result:=fext+'/ACE'; exit; end;
if (sbuf[0]=65) and (sbuf[1]=73) and (sbuf[2]=2) then begin result:=fext+'/APPIMAGE'; exit; end;
if (sbuf[0]=96) and (sbuf[1]=234) then begin result:=fext+'/ARJ'; exit; end;
if (sbuf[0]=66) and (sbuf[1]=67) and (sbuf[2]=77) then begin result:=fext+'/BCM'; exit; end;
if (sbuf[0]=225) and (sbuf[1]=80) and (sbuf[2]=220) then begin result:=fext+'/BR'; exit; end;
if (sbuf[0]=66) and (sbuf[1]=90) and (sbuf[2]=104) then begin result:=fext+'/BZIP2'; exit; end;
if (sbuf[0]=77) and (sbuf[1]=83) and (sbuf[2]=67) and (sbuf[3]=70) then begin result:=fext+'/CAB'; exit; end;
if (sbuf[0]=73) and (sbuf[1]=84) and (sbuf[2]=83) and (sbuf[3]=70) and (sbuf[4]=3) and (sbuf[5]=0) and (sbuf[6]=0) and (sbuf[7]=0) then begin result:=fext+'/CHM'; exit; end;
if (sbuf[0]=208) and (sbuf[1]=207) and (sbuf[2]=17) and (sbuf[3]=224) and (sbuf[4]=161) and (sbuf[5]=177) and (sbuf[6]=26) and (sbuf[7]=225) then begin result:=fext+'/COMPOUND'; exit; end;
if (sbuf[0]=48) and (sbuf[1]=55) and (sbuf[2]=48) and (sbuf[3]=55) and (sbuf[4]=48) then begin result:=fext+'/CPIO'; exit; end;
if (sbuf[0]=33) and (sbuf[1]=60) and (sbuf[2]=97) and (sbuf[3]=114) and (sbuf[4]=99) and (sbuf[5]=104) and (sbuf[6]=62) then begin result:=fext+'/DEB'; exit; end;
//offset end-512 if (sbuf[0]=107) and (sbuf[1]=111) and (sbuf[2]=108) and (sbuf[3]=121) then begin result:=fext+'/DMG'; exit; end;
if (sbuf[0]=127) and (sbuf[1]=69) and (sbuf[2]=76) and (sbuf[3]=70) then begin result:=fext+'/ELF'; exit; end;
if (sbuf[0]=77) and (sbuf[1]=90) then begin result:=fext+'/EXE'; exit; end;
if (sbuf[0]=65) and (sbuf[1]=114) and (sbuf[2]=67) then begin result:=fext+'/FREEARC'; exit; end;
if (sbuf[0]=31) and (sbuf[1]=139) then begin result:=fext+'/GZIP'; exit; end;
//offset 0x8001 if (sbuf[0]=67) and (sbuf[1]=68) and (sbuf[2]=48) and (sbuf[3]=48) and (sbuf[4]=49) then begin result:=fext+'/ISO'; exit; end;
if (sbuf[0]=4) and (sbuf[1]=34) and (sbuf[2]=77) and (sbuf[3]=24) then begin result:=fext+'/LZ4'; exit; end;
if (sbuf[2]=45) and (sbuf[3]=108) and (sbuf[4]=104) then begin result:=fext+'/LZH'; exit; end; //offset 2
if (sbuf[0]=76) and (sbuf[1]=90) and (sbuf[2]=73) and (sbuf[3]=80) then begin result:=fext+'/LZIP'; exit; end;
if (sbuf[0]=79) and (sbuf[1]=103) and (sbuf[2]=103) and (sbuf[3]=83) then begin result:=fext+'/OGG'; exit; end;
if (sbuf[0]=37) and (sbuf[1]=80) and (sbuf[2]=68) and (sbuf[3]=70) and (sbuf[4]=45) then begin result:=fext+'/PDF'; exit; end;
if (sbuf[0]=234) and (sbuf[1]=01) then begin result:=fext+'/PEA'; exit; end;
if (sbuf[0]=81) and (sbuf[1]=70) and (sbuf[2]=73) then begin result:=fext+'/QCOW'; exit; end;
if (sbuf[0]=237) and (sbuf[1]=171) and (sbuf[2]=238) and (sbuf[3]=219) then begin result:=fext+'/RPM'; exit; end;
if ((sbuf[0]=67) and (sbuf[1]=87) and (sbuf[2]=83)) or ((sbuf[0]=70) and (sbuf[1]=87) and (sbuf[2]=83)) then begin result:=fext+'/SWF'; exit; end;
if (sbuf[0]=60) and (sbuf[1]=60) and (sbuf[2]=60) and (sbuf[3]=32) and (sbuf[4]=79) and (sbuf[5]=114) and (sbuf[6]=97) and (sbuf[7]=99) then begin result:=fext+'/VDI'; exit; end;
if (sbuf[0]=99) and (sbuf[1]=111) and (sbuf[2]=110) and (sbuf[3]=101) and (sbuf[4]=99) and (sbuf[5]=116) and (sbuf[6]=105) and (sbuf[7]=120) then begin result:=fext+'/VHD'; exit; end;
if (sbuf[0]=118) and (sbuf[1]=104) and (sbuf[2]=100) and (sbuf[3]=120) and (sbuf[4]=102) and (sbuf[5]=105) and (sbuf[6]=108) and (sbuf[7]=101) then begin result:=fext+'/VHDX'; exit; end;
if (sbuf[0]=75) and (sbuf[1]=68) and (sbuf[2]=77) then begin result:=fext+'/VMDK'; exit; end;
if (sbuf[0]=73) and (sbuf[1]=87) and (sbuf[2]=65) and (sbuf[3]=68) then begin result:=fext+'/WAD'; exit; end;
if (sbuf[0]=77) and (sbuf[1]=83) and (sbuf[2]=87) and (sbuf[3]=73) and (sbuf[4]=77) then begin result:=fext+'/WIM'; exit; end;
if (sbuf[0]=120) and (sbuf[1]=97) and (sbuf[2]=114) and (sbuf[3]=73) and (sbuf[4]=33) then begin result:=fext+'/XAR'; exit; end;
if (sbuf[0]=253) and (sbuf[1]=55) and (sbuf[2]=122) and (sbuf[3]=88) and (sbuf[4]=90) and (sbuf[5]=0) and (sbuf[6]=28) then begin result:=fext+'/XZ'; exit; end;
if ((sbuf[0]=31) and (sbuf[1]=157)) or ((sbuf[0]=31) and (sbuf[1]=160)) then begin result:=fext+'/Z'; exit; end;
if (sbuf[0]=40) and (sbuf[1]=181) and (sbuf[2]=47) and (sbuf[3]=253) then begin result:=fext+'/ZST'; exit; end;
if (sbuf[0]=55) and (sbuf[1]=107) and (sbuf[2]=83) then begin result:=fext+'/ZPAQ'; exit; end;
except
end;
end;

function pathistmp(s:ansistring):boolean;
begin
result:=false;
if (pos(STR_TMP,s)) or
   (pos(STR_PZWORKTMP,s)) or
   (pos(STR_STMP,s)) or
   (pos(STR_TMPEXT,s)) or
   (pos(STR_TMPDD,s)) or
   (pos(STR_PEAZIPTMP,s))<>0 then result:=true;
end;

//split a string in words separated by space: ignore empty strings, ignore quotes
function peasplitstring(s:AnsiString; var sfin:TStringList):integer;
var
   sarr:TStringList;
   i:integer;
begin
result:=-1;
if s='' then exit;
sarr:=TStringList.Create;
sarr.Delimiter:=' ';
sarr.QuoteChar:=#0;//count words in quoted sections
sarr.StrictDelimiter:=False;//do not count line ending
sarr.DelimitedText:=s;
for i:=0 to sarr.Count-1 do
   begin
   if sarr[i]<>'' then sfin.Add(sarr[i]);//remove empty strings (multiple separators)
   end;
result:=sfin.Count;
end;

//launch TProcess with command line or parameters, optional log: pmode 0 use cl in Process.CommandLine; pmode 1 Process.ParseCmdLine cl, don't assign a value to Process.CommandLine (even empty CommandLine disables Parameters)
function peapexecute(var P:TProcessUTF8; var cl:ansistring): integer;
begin
result:=-1;
case pmode of //some composite command strings on Windows cannot be correctly passwed with pmode=1, i.e. add rar comment
   0: P.CommandLine:=cl;
   1: {$IFDEF MSWINDOWS}P.ParseCmdLine(cl);{$ELSE}P.ParseCmdLine(cl,True);{$ENDIF}//ReadBackslash set to true for non-Windows systems: when using parameters, non_Windows escaping uses \ character to escape the true string delimiter character
end;
P.Execute;
if logcommands=1 then
   begin
   if not Assigned(cllog) then cllog:=TStringList.Create;
   cllog.Add(cl);
   end;
result:=0;
end;

//launch TProcess with parameters, optional log
function peapparamexecute(var P:TProcessUTF8): integer;
var
   i:integer;
   cl:ansistring;
begin
result:=-1;
P.Execute;

cl:=P.Executable;
for i:=0 to P.Parameters.Count-1 do
begin
  cl:=cl+' '+P.Parameters[i];
end;

if logcommands=1 then
   begin
   if not Assigned(cllog) then cllog:=TStringList.Create;
   cllog.Add(cl);
   end;
result:=0;
end;

end.
