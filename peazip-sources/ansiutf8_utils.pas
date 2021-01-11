unit ansiutf8_utils;
{
 DESCRIPTION     :  Unit providing wrapper for ansi-based FPC functions, in order
                    to, when applicable, receive utf8input and give utf8output

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20080730  G.Tani      Initial version
 0.18     20080818  G.Tani      ucreatedir
 0.19     20081118  G.Tani      removed ucreatedir (replaced with mkdir); added urenamefile
 0.20     20081201  G.Tani      convertwincp procedure for Windows to convert oem codepages (as in text received from Windows console) to UTF-8
 0.20     20090906  G.Tani      forcedirectories
 0.21     20091116  G.Tani      extractrelativepath
 0.22     20100130  G.Tani      filesetattr
 0.23     20100228  G.Tani      functions rewritten to be full UTF8: extractfilepath, extractfilename, extractfileext, dodirseparators
 0.24     20110122  G.Tani      removed urenamefile
                                added uStringReplace
                                added functions to get paramstr in widestring form on Windows
 0.25     20111001  G.Tani      added udeletefile_simple that simply maps deletefile function
                                udeletefile modified in order to reset read only flag before attempting deletion
 0.26     20150828  G.Tani      Updated various functions to use current Lazarus/FPC UTF-8 compliant functions
                                Added testutf8needed to check if a string needs to be handled by UTF-8 functions or if it is ANSI-safe
                                 on Windows also test if it can be handled with current OEM codepage (to run in .bat scripts and CMD prompt)
                                Added utf8decodecond to test and apply utf8decode if required
 0.27     20151017  G.Tani      Added usetfsname, uforcefsname, and usetfsnameonly to set ansi-safe short name/path on Windows systems, to be used to provide strings to functions that are not yet utf-8 compliant
                                Updated assignfile and assignfile to provide ansi-safe short name/path on non-utf8 internal assign function Windows systems
 0.28     20151025  G.Tani      Updated testutf8needed to avoid possible conversion issues
 0.29     20151231  G.Tani      Rewritten for FPC3 removing all now-unnecessary character encoding handling routines
 0.30     20200509  G.Tani      upredelete function to remove attributes that may cause failing to delete the file

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

uses
Classes, SysUtils, FileUtil;

procedure write_header(var tf:text); //write utf-8 text file header
function read_header(var tf:text):boolean; //read utf-8 text file header
function udeletefile(s:ansistring):boolean; //on Windows changes file attributes to allow file to be deleted
function upredeletefile(s:ansistring):boolean; //on Windows changes file attributes to allow file to be deleted separately

implementation

procedure write_header(var tf:text);
begin
write(tf,char($ef));
write(tf,char($bb));
write(tf,char($bf));
end;

function read_header(var tf:text):boolean;
var
   c:char;
begin
read_header:=false;
read(tf,c);
if c<>char($ef) then exit;
read(tf,c);
if c<>char($bb) then exit;
read(tf,c);
if c<>char($bf) then exit;
read_header:=true;
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

end.
