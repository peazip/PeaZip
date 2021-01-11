unit rfs_utils;
{
 DESCRIPTION     :  Unit providing UI-neutral routines to be used in Raw File
                    Split and Raw File Join

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060816  G.Tani      Initial version
 0.11     20060917  G.Tani      removed *_VER; P_RELEASE constant in pea_utils
                                is used to keep track of release level;
 0.12     20080704  G.Tani      Enabled utf8
 0.13     20200508  G.Tani      Added SHA3_256, SHA3_512, BLAKE2S, BLAKE2B algorithms

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

uses SysUtils;

const
SUCCESS = 0;
INCOMPLETE_FUNCTION = 1;
UNKNOWN_VOLUME_CONTROL_ALGORITHM = 2;
NOT_RFS_HEADER = 3;

{
generate RFS 4 byte .check file header
}
function rfs_create_checkfile_hdr ( volume_control_algorithm:ansistring;        //algorithm to error check each volume
                                    var hdr_data:array of byte                  //buffer for header data
                                    ):integer;

{
read RFS 4 byte .check file header
}
function rfs_parse_archive_header ( read_data:array of byte;                    //buffer containig the header matherial
                                    var volume_algo:ansistring                  //control algorithm for volumes
                                    ):integer;                                  //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered

{
decode volume control algorithm
}
function decode_rfs_volume_control_algo ( volume_algo:ansistring;               //control algorithm
                                          var authsize:byte                     //authentication tag size
                                          ):integer;

{
update volume name following RFS volume naming convention
}
procedure update_rfs_filename ( fname:ansistring;                               //name to update
                                i:integer;                                      //counter (updated externally)
                                var newname:ansistring);                        //updated name: input name + 3 digit human readable counter

implementation

function rfs_create_checkfile_hdr ( volume_control_algorithm:ansistring;        //algorithm to error check each volume
                                    var hdr_data:array of byte                  //buffer for header data
                                    ):integer;
begin
rfs_create_checkfile_hdr:=INCOMPLETE_FUNCTION; //generic error code: the subroutine is jet not completed (useful if the function somehow cannot complete)
hdr_data[0]:=114; //r
hdr_data[1]:=102; //f
hdr_data[2]:=115; //s
//byte 3: control model for each single volume, to allow identification of corrupted chunks (i.e. to request resending, or to not trust to try to open them)
if (upcase(volume_control_algorithm)='NOALGO') then hdr_data[3]:=$00
else
if (upcase(volume_control_algorithm)='ADLER32') then hdr_data[3]:=$01
else
if (upcase(volume_control_algorithm)='CRC32') then hdr_data[3]:=$02
else
if (upcase(volume_control_algorithm)='CRC64') then hdr_data[3]:=$03
else
if (upcase(volume_control_algorithm)='MD5') then hdr_data[3]:=$10
else
if (upcase(volume_control_algorithm)='RIPEMD160') then hdr_data[3]:=$11
else
if (upcase(volume_control_algorithm)='SHA1') then hdr_data[3]:=$12
else
if (upcase(volume_control_algorithm)='SHA256') then hdr_data[3]:=$13
else
if (upcase(volume_control_algorithm)='SHA512') then hdr_data[3]:=$14
else
if (upcase(volume_control_algorithm)='WHIRLPOOL') then hdr_data[3]:=$15
else
if (upcase(volume_control_algorithm)='SHA3_256') then hdr_data[3]:=$16
else
if (upcase(volume_control_algorithm)='SHA3_512') then hdr_data[3]:=$17
else
if (upcase(volume_control_algorithm)='BLAKE2S') then hdr_data[3]:=$18
else
if (upcase(volume_control_algorithm)='BLAKE2B') then hdr_data[3]:=$19
else rfs_create_checkfile_hdr:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
rfs_create_checkfile_hdr:=SUCCESS;
end;

function rfs_parse_archive_header ( read_data:array of byte;                    //buffer containig the header matherial
                                    var volume_algo:ansistring                  //control algorithm for volumes
                                    ):integer;                                  //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
rfs_parse_archive_header:=INCOMPLETE_FUNCTION;
if ((read_data[0]=114) and (read_data[1]=102) and (read_data[2]=115)) then
   begin
   case read_data[3] of
      0: volume_algo:='NOALGO'; //hex 00
      1: volume_algo:='ADLER32'; //hex 01
      2: volume_algo:='CRC32';  //hex 02
      3: volume_algo:='CRC64';  //hex 03
     16: volume_algo:='MD5'; //hex 10
     17: volume_algo:='RIPEMD160'; //hex 11
     18: volume_algo:='SHA1'; //hex 12
     19: volume_algo:='SHA256'; //hex 13
     20: volume_algo:='SHA512'; //hex 14
     21: volume_algo:='WHIRLPOOL'; //hex 15
     22: volume_algo:='SHA3_256'; //hex 16
     23: volume_algo:='SHA3_512'; //hex 17
     24: volume_algo:='BLAKE2S'; //hex 18
     25: volume_algo:='BLAKE2B'; //hex 19
      else
         begin
         rfs_parse_archive_header:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
         exit;
         end;
      end;
   end
else rfs_parse_archive_header:=NOT_RFS_HEADER;
if rfs_parse_archive_header=INCOMPLETE_FUNCTION then rfs_parse_archive_header:=SUCCESS;
end;

function decode_rfs_volume_control_algo ( volume_algo:ansistring;               //control algorithm
                                          var authsize:byte                     //authentication tag size
                                          ):integer;
begin
decode_rfs_volume_control_algo:=INCOMPLETE_FUNCTION;
authsize:=255;
if upcase(volume_algo)='NOALGO' then authsize:=0;
if upcase(volume_algo)='ADLER32' then authsize:=4;
if upcase(volume_algo)='CRC32' then authsize:=4;
if upcase(volume_algo)='CRC64' then authsize:=8;
if upcase(volume_algo)='MD5' then authsize:=16;
if upcase(volume_algo)='RIPEMD160' then authsize:=20;
if upcase(volume_algo)='SHA1' then authsize:=20;
if upcase(volume_algo)='SHA256' then authsize:=32;
if upcase(volume_algo)='SHA512' then authsize:=64;
if upcase(volume_algo)='WHIRLPOOL' then authsize:=64;
if upcase(volume_algo)='SHA3_256' then authsize:=32;
if upcase(volume_algo)='SHA3_512' then authsize:=64;
if upcase(volume_algo)='BLAKE2S' then authsize:=32;
if upcase(volume_algo)='BLAKE2B' then authsize:=64;
if authsize=255 then decode_rfs_volume_control_algo:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
if decode_rfs_volume_control_algo=INCOMPLETE_FUNCTION then decode_rfs_volume_control_algo:=SUCCESS;
end;

procedure update_rfs_filename ( fname:ansistring;                               //name to update
                                i:integer;                                      //counter (updated externally)
                                var newname:ansistring);                        //updated name: input name + 3 digit human readable counter
begin
if i <10 then newname:=fname+'.00'+inttostr(i)
else
   if i<100 then newname:=fname+'.0'+inttostr(i)
   else newname:=fname+'.'+inttostr(i);
end;

end.
