unit pea_utils;
{
 DESCRIPTION     :  Unit providing UI-neutral routines to be used in PEA and
                    UnPEA

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060912  G.Tani      Initial version
 0.11     20060919  G.Tani      removed *_VER; P_RELEASE constant is used to
                                keep track of release level;
                                rng modified to use a persistent random collecting
                                file in generate_salt (via file's path) and
                                generate_keyfile (preloading it in the key array).
 0.12     20060926  G.Tani      added notes about compatibility for external functions
                                called;
 0.13     20060523  G.Tani      P_RELEASE moved to unit_pea; in pea_utils remains
                                _VER and _REV constants to declare PEA format support level
 0.14     20071130  G.Tani      Some cleanup
                                removed unit oldlinux (obsolete)
 0.15     20080314  G.Tani      moved here: get desktop environment routine, cross
                                platform filename escaping routine and linux-like
                                routines for filename escaping, open files and
                                search files.
 0.16     20080407  G.Tani      added routine to find host system's desktop (win)
                                or home (which makes more sense for Unixes)
 0.17     20080721  G.Tani      added procedures to read/write UTF-8 coded text file;
                                Enabled utf8
 0.19     20080922  G.Tani      Moved some peazip functions here to share them with pealauncher
                                OSX string replaced with DARWIN in IFDEFs
 0.20     20081024  G.Tani      Added procedure to get a temporary work path writeable from current user
 0.21     20090215  G.Tani      81 extensions supported
 0.22     20090327  G.Tani      84 extensions supported
 0.23     20090406  G.Tani      85 extensions supported
 0.24     20090601  G.Tani      87 extensions supported (.xz, .vhd)
 0.25     20090609  G.Tani      checkfilename and getwinver moved here
                                added checkfiledirname for sanitization, against nonvalid and reserved characters and reserved filenames, of path+filename strings
                                added validatecl for sanitization, against nonvalid and command concatenation characters, of all strings passed to any type of execution
 0.26     20090706  G.Tani      improved handling filenames both within delimiters characters or not
                                if selected file is not found, try to fallback showing the directory (no file selected) before raising error message
                                90 extensions supported (.xlt, .dot, .pot)
 0.27     20090822  G.Tani      95 extensions supported
 0.28     20091016  G.Tani      filesize replaced by list_utils' srcfilesize
 0.29     20091025  G.Tani      98 extensions supported
 0.30     20091227  G.Tani      118 extensions supported
 0.31     20100113  G.Tani      Modified testinput function to handle split files with 7z backed, to be able to extract split archives with wrong extensions (split/join through pea binary is used on direct request only)
 0.32     20101013  G.Tani      added support for .fla, 124 extensions supported
 0.33     20101107  G.Tani      added support for .sfs and .image, 126 extensions supported
 0.34     20101215  G.Tani      added support for .apk, .sar and .imf, 129 extensions supported
 0.35     20110111  G.Tani      Moved to list_utils various file manager functions and procedures not depending on the crypto library framework
 0.36     20130529  G.Tani      Code cleanup
 0.37     20160310  G.Tani      PEA 1.1 file format revision introducing support for SHA-3 256 and 512 hash, and Twofish and Serpent cyphers in EAX mode, 128 and 256 bit
 0.38     20170423  G.Tani      Moved here code to handle program-wide defined colors
 0.39     20200508  G.Tani      PEA 1.2 file format revision, introducing support for BLAKE2S 256 bit and BLAKE2B 512 bit
 0.40     20200511  G.Tani      Extended color management for theming
 0.41     20200905  G.Tani      Multiple encryption cascading AES, Twofish and Serpent, all in 256 bit EAX mode
                                Improved password strength test
 0.42     20201206  G.Tani      Updated theming

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

{$mode objfpc}{$H+}{$UNITPATH ./we}
{$INLINE ON}

interface

uses
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
SysUtils,
list_utils,ansiutf8_utils,
hash, sha1, sha256, sha512, whirl512, aes_ctr, AES_Type, TF_Base, tf_ctr,
aes_eax, tf_eax, sp_eax,
fcrypta, fcryptt, fcrypts,
FCAES256,FCTF256,FCSP256,
TSC, mem_util, graphics, img_utils,
Process;

type
   TKey2048 = array [0..255] of byte;

const
PEA_FILEFORMAT_VER = 1;
PEA_FILEFORMAT_REV = 3; //version and revision declared to be implemented must match with the ones in unit_pea, otherwise a warning will be raised (unit_pea)
SUCCESS = 0;
INCOMPLETE_FUNCTION = 1;
NOT_PEA_HEADER = 2;
UNKNOWN_COMPRESSION_ALGORITHM = 3;
UNKNOWN_CONTROL_ALGORITHM = 4;
UNKNOWN_OBJ_CONTROL_ALGORITHM = 5;
UNKNOWN_VOLUME_CONTROL_ALGORITHM = 6;
NON_ACCESSIBLE_KEYFILE = 7;
ERROR_IN_SALT_GENERATION = 8;
PEA_REVISION_NOT_SUPPORTED = 9;
INVALID_ENCRYPTION_HEADER = 10;
NOT_SUPPORTED_COMPRESSION_MODE = 11;
SUBHEADER_NOT_MATCH_HEADER = 12;
FILELIST_EMPTY = 13;
FILELIST_NOT_ACCESSIBLE = 14;
PRED          = $005050f0;
PLYELLOW      = $00aaeeff;
PYELLOW       = $0000c0f0;
PLGREEN       = $0040d080;
PGREEN        = $0000c060;
PAPPCOL       = $00ff9933;
PTACOL        = $00CC5511;

var
plblue,pblue,pvlblue,pvvvlblue,pvvlblue,pgray,psilver,ptextaccent,pltextaccent:tcolor;
colhigh,colmid,collow,colbtnhigh:string;

//get SHA256 hash of file from name
function getchash(fname:ansistring):ansistring;

// get program's colors
procedure getpcolors(basappcol,baseformcol,baselinkcol,temperature:TColor);

{
functions for generating PEA version1 revision1 fields
}

//generate PEA archive header as an array of 10 byte
function pea_archive_hdr ( volume_control_algorithm:ansistring;                 //algorithm to error check each volume
                           var hdr_data:array of byte;                          //buffer for header data
                           var hdr_size:dword                                   //size, in byte, of the volume header
                           ):integer;

//generate PEA stream header as an array of 10 byte
function pea_stream_hdr ( compression_algorithm:ansistring;                     //compression algorithm used
                          stream_control_algorithm:ansistring;                  //control algorithm used on the archive
                          obj_control_algorithm:ansistring;                     //control algorithm used on single objects into the stream
                          var hdr_data:array of byte;                           //buffer for header data
                          var hdr_size:dword                                    //size, in byte, of the stream header
                          ):integer;

{please note that cryptographic subheader FCAsig and Flags bytes are zeroed (information is declared in stream header instead)}
//initializes Authenticated Encryption and generate a FCA style header to append to PEA stream header if EAX is used as control algorithm; uses salt from generate_salt
function pea_eax_subhdr ( var cxe:TAES_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCAHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;
//Twofish variant
function pea_tfeax_subhdr ( var cxf:Ttf_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCfHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;
//Serpent variant
function pea_speax_subhdr ( var cxs:Tsp_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCsHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;

//initializes Authenticated Encryption and generate a FCA style header to append to PEA stream header if EAX256 is used as control algorithm; uses salt from generate_salt - note that the authentication tag will be 128 bit sized (AES block)
function pea_eax256_subhdr ( var cxe:TAES_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCA256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
//Twofish 256 variant
function pea_tfeax256_subhdr ( var cxf:TTF_EAXContext;                          //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCf256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
//Serpent 256 variant
function pea_speax256_subhdr ( var cxs:TSP_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCs256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;

//same as previous group but with slower 64000 iterations key derivation
function pea_eax256_subhdrP ( var cxe:TAES_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCA256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
//Twofish 256 variant
function pea_tfeax256_subhdrP ( var cxf:TTF_EAXContext;                          //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCf256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
//Serpent 256 variant
function pea_speax256_subhdrP ( var cxs:TSP_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCs256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;

//initializes Authenticated Encryption and generate a FCA style header to append to PEA stream header if HMAC is used as control algorithm; uses salt from generate_salt
function pea_hmac_subhdr ( var cxh:TFCA_HMAC_Context;                           //control algorithm context
                           persistent_source:ansistring;                        //path of persistent source of random data
                           fingerprint:TSHA512Digest;                           //system fingerprint
                           ment,kent,fent:THashContext;                         //entropy sources
                           postwhitening:byte;                                  //whitening of digest with other prng
                           var pw_array:array of byte;                          //keying matherial for control algorithm, zeroed on exit
                           var pw_len:word;                                     //size of keying matherial, zeroed on exit
                           var hdr:TFCAHdr;                                     //control algorithm header
                           var hdr_data:array of byte;                          //buffer for header data as array of byte
                           var hdr_size:dword                                   //size, in byte of the header
                           ):integer;

//generate end of stream trigger
procedure trigger_eos ( var buf: array of byte);

//generate end of archive trigger
procedure trigger_eoa ( var buf: array of byte);

{
functions for parsing PEA fields, from an array of byte loaded reading a PEA archive
}

//parse a PEA archive header (from an array of byte), please note that only volume control algorithm and system date and time encoding are returned, since are only information relevant for the actual implementation
function pea_parse_archive_header ( read_data:array of byte;                    //the buffer containing the header matherial
                                    var volume_algo:ansistring;                 //control algorithm for the volumes
                                    var datetimeencoding:byte                   //system date and time encoding
                                    ):integer;                                  //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered

//parse a PEA stream header (from an array of byte)
function pea_parse_stream_header ( read_data:array of byte;                     //the buffer containing the header matherial
                                   var compr:ansistring;                        //compression algorithm
                                   var compr_level:byte;                        //compression level
                                   var algo:ansistring;                         //control algorithm for the stream
                                   var obj_algo:ansistring                      //control algorithm for objects
                                   ):integer;                                   //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered

//parse a PEA crypto subheader, FCA style, (read from an array of byte)
function pea_parse_crypto_subheader ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var hdr:TFCAHdr                           //header structure were load read header matherial
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
//Twofish variant
function pea_parse_crypto_subheaderf ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var fhdr:TFCfHdr                           //header structure were load read header matherial
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
//Serpen variant
function pea_parse_crypto_subheaders ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var shdr:TFCsHdr                           //header structure were load read header matherial
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered

{
function and procedures for working on PEA variables
}

//get the compression algorithm and options from a command param
function decode_compression_algo ( compr:ansistring;                            //compressor
                                   var compr_level:byte                         //compressor option
                                   ):integer;

//get the control algorithm from a command param
function decode_control_algo ( algo:ansistring;                                 //control algorithm
                               var headersize:byte;                             //size of the header needed
                               var authsize:byte;                               //authntication tag size
                               var pwneeded:boolean                             //password is required by the control algorithm
                               ):integer;

//get the control algorithm for single objects (supported by PEA ver0rev1) from a command param;
function decode_obj_control_algo ( obj_algo:ansistring;                         //control algorithm
                                   var authsize:byte                            //authentication tag size
                                   ):integer;

//get control algorithm for volumes
function decode_volume_control_algo ( volume_algo:ansistring;                   //control algorithm
                                      var authsize:byte                         //authentication tag size
                                      ):integer;

//find the PEA code for a given compression scheme
function encode_compression ( compression_algorithm:ansistring;                 //compression scheme
                              var code:byte                                     //PEA code for given compression scheme
                              ):integer;

//find the PEA code for a given stream control algorithm
function encode_algo_stream ( stream_control_algorithm:ansistring;              //stream control algorithm
                              var code:byte                                     //PEA code for given algorithm
                              ):integer;

//find the PEA code for a given algorithm, not for stream control
function encode_algo_nonstream ( control_algorithm:ansistring;                  //volume or object control algorithm
                                 var code:byte                                  //PEA code for given algorithm
                                 ):integer;

//get PEA code for operating system
procedure get_OS(var code:byte);                                                //OS type

//get PEA code for date and time encoding of the system
procedure get_system_datetimeencoding(var code:byte);                           //system time-date encoding, uses same code as OS (family, if uses same encoding)

//get PEA code for CPU type and endianness (in msb)
procedure get_CPUe(var code:byte);                                              //CPU and endianness (in MSB)

{
functions related to keying, salting and entropy collection
}

//write the content of a keyfile (file of byte up to 16KB in size) to an array of byte from a given starting point
function use_keyfile ( keyfilename:ansistring;                                  //qualified name of the keyfile
                       keysize:word;                                            //size of keying matherial to read, in byte
                       var numread:dword;                                       //size of keying matherial actually read, in byte
                       var pw_array:array of byte;                              //keying matherial as array of byte
                       var ind:word                                             //index in the array where starting to write the keyfile provided keying matherial
                       ):integer;

//generate random salt sampling enthropy from the system and interactions with the user
function generate_salt ( var salt:TSHA512Digest;                                //give a SHA512 digest as random salt
                         persistent_source:ansistring;                          //path of persistent source of random data
                         fingerprint:TSHA512Digest;                             //fingerprint (SHA512 digest) of system state, usually created at application startup
                         var ment,kent,fent:THashContext;                       //hash context of mouse sampling and keyboard sampling
                         postwhitening:byte                                     //whitening of digest with other prng
                         ):integer;

//generate a 2048 bit random keyfile
function generate_keyf ( var key:TKEY2048;                                      //2048 bit key (array of byte); it can be also used to load external data to introduce additional entropy to the function
                         persistent_source:ansistring;                          //path of persistent source of random data
                         fingerprint:TSHA512Digest;                             //fingerprint (SHA512 digest) of system state, usually created at application startup
                         var ment,kent,fent:THashContext                        //hash context of mouse sampling and keyboard sampling
                         ):integer;

//give a SHA512 digest as whitened pool of enthropy gathered from times
function get_timers (var tdigest:TSHA512Digest):boolean;

//give a SHA512 digest as whitened pool of enthropy gathered from memory statistics (Windows only)
function get_memory (var memdigest:TSHA512Digest):boolean;

//generate SHA512 digest, optionally whitened, from some system and session variables ("fingerprint"), usually should be called only once (i.e. system startup) since probably only few values will change quickly
function get_fingerprint (var fingerprint:TSHA512Digest; usetempfiles:boolean):boolean;

//sample entropy from mouse movement (x, y and time) updating an hash context with Whirlpool512
function sample_mouse_ent(var ment:THashContext; x,y:integer):boolean;

//sample entropy from keypress (key pressed and time) updating an hash context with Whirlpool512
function sample_keyb_ent(var kent:THashContext; k:byte):boolean;

//sample entropy from file (content, filename, time of file's selection) updating an hash context with Whirlpool512
function sample_file_ent ( var fent:THashContext; fname:ansistring):boolean;

{
misc utility routines
}

//write a word to an array of byte
function word2bytebuf ( inword:word;                                            //word to write in the buffer starting from the given address, please note that size of the buffer is not checked
                        var buf:array of byte;
                        addr:dword
                        ):integer;

//write a dword to an array of byte
function dword2bytebuf ( indword:dword;                                         //dword to write in the buffer starting from the given address, please note that size of the buffer is not checked
                         var buf:array of byte;
                         addr:dword
                         ):integer;

//write a qword to an array of byte
function qword2bytebuf ( inqword:qword;                                         //qword to write in the buffer starting from the given address, please note that size of the buffer is not checked
                         var buf:array of byte;
                         addr:dword
                         ):integer;

//give the pea_utils's constant string of a given error code
function decode_pea_error ( err:integer;                                        //error code
                            var s:ansistring                                    //string of error description (constants of pea_utils unit)
                            ):integer;

//give pea file (or volume) updated name: input name + 6 digit human readable counter + .pea extension
procedure update_pea_filename ( fname:ansistring;                               //name to update
                                i:integer;                                      //counter (updated externally)
                                var newname:ansistring);                        //updated name: input name + 6 digit human readable counter + .pea extension

//read a list of input objects from a text file
function read_filelist ( listfile_param:ansistring;                             //name of file containing list of input objects
                         var in_param: TFoundlist                               //array of names of input objects
                         ):integer;

//create cl string for command line
function pw4cl(sw:ansistring; var pw:ansistring):integer;

//evaluate password strength;
procedure evaluate_password ( pw: ansistring;                                   //password
                              var pw_strength:dword);                           //entropy bits evaluation

//function to prepend keyfile to password string (used for non-pea encryption)
function prepend_keyfile(var pw:ansistring; keyfilename:ansistring):integer;

implementation

function getchash(fname:ansistring):ansistring;
var
   sbuf:array [1..32767] of byte;
   n:integer;
   k:qword;
   SHA256Context:THashContext;
   SHA256Digest:TSHA256Digest;
   f:file of byte;
begin
result:='';
if fname='' then exit;
filemode:=0;
try
assignfile(f,fname);
filemode:=0;
reset(f);
except
exit;
end;
SHA256Init(SHA256Context);
repeat
   blockread(f,sbuf,sizeof(sbuf),n);
   if n<>0 then
      begin
      inc(k,n);
      SHA256Update(SHA256Context,@sbuf,n);
      end;
until n<>sizeof(sbuf);
close(f);
SHA256Final(SHA256Context,SHA256Digest);
result:=upcase(hexstr(@SHA256Digest,sizeof(SHA256Digest)));
end;

procedure getpcolors(basappcol,baseformcol,baselinkcol,temperature:TColor);
begin
plblue:=basappcol;
ptextaccent:=baselinkcol;
if evalcolor(baseformcol)>128 then
   begin
   pltextaccent:=modpropcolor(baselinkcol,160,0);
   pblue:=modpropcolor(basappcol,-60,0);
   pvlblue:=modpropcolor(basappcol,100,0);
   pvvlblue:=modpropcolor(basappcol,160,0);
   pvvvlblue:=modpropcolor(basappcol,210,0);
   colhigh:=colortostring(modpropcolor(baseformcol,-28,temperature));
   colmid:=colortostring(modpropcolor(baseformcol,-20,temperature));
   collow:=colortostring(modpropcolor(baseformcol,-10,temperature));
   colbtnhigh:=colortostring(modpropcolor(baseformcol,-32,temperature));
   pgray:=modpropcolor(baseformcol,-128,temperature);
   psilver:=modpropcolor(baseformcol,-64,temperature);
   end
else
   begin
   pltextaccent:=modpropcolor(baselinkcol,-80,0);
   pblue:=modpropcolor(basappcol,80,0);
   pvlblue:=modpropcolor(basappcol,-40,0);
   pvvlblue:=modpropcolor(basappcol,-80,0);
   pvvvlblue:=modpropcolor(basappcol,-100,0);
   colhigh:=colortostring(modpropcolor(baseformcol,30,temperature));
   colmid:=colortostring(modpropcolor(baseformcol,20,temperature));
   collow:=colortostring(modpropcolor(baseformcol,10,temperature));
   colbtnhigh:=colortostring(modpropcolor(baseformcol,40,temperature));
   pgray:=modpropcolor(baseformcol,128,temperature);
   psilver:=modpropcolor(baseformcol,64,temperature);
   end;
end;

function pea_archive_hdr ( volume_control_algorithm:ansistring;                 //algorithm to error check each volume
                           var hdr_data:array of byte;                          //buffer for header data
                           var hdr_size:dword                                   //size, in byte, of the volume header
                           ):integer;
begin
pea_archive_hdr:=INCOMPLETE_FUNCTION; //generic error code: the subroutine is jet not completed (useful if the function somehow cannot complete)
hdr_data[0]:=$EA; //magic byte for format disambiguation
hdr_data[1]:=PEA_FILEFORMAT_VER; //version level
hdr_data[2]:=PEA_FILEFORMAT_REV; //revision level
//byte 3: control model for each single volume, to allow identification of corrupted volumes (i.e. to request resending, or to not trust to try to open them)
if encode_algo_nonstream(volume_control_algorithm,hdr_data[3])=SUCCESS then else pea_archive_hdr:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
//byte 4: archive-wide ECC scheme, zeroed since actually no ECC scheme is implemented
hdr_data[4]:=$00;
//byte 5: system were the archive was created (checked at compile time)
get_OS(hdr_data[5]);
//byte 6: system time-date encoding
get_system_datetimeencoding(hdr_data[6]);
//byte 7: character encoding
hdr_data[7]:=$01; //ANSI
{this implementation only support ANSI character encoding}
//byte 8: endianness and CPU type, endianness in the highest bit (0 little endian, 1 big endian)
get_CPUe(hdr_data[8]);
{this implementation only support x86 due to some parts in ASM in crypto library used}
//byte 9: reserved
hdr_data[9]:=$00; //actually unused, zeroed
hdr_size:=10;
pea_archive_hdr:=SUCCESS;
end;

function pea_stream_hdr ( compression_algorithm:ansistring;                     //compression algorithm used
                          stream_control_algorithm:ansistring;                  //control algorithm used on the archive
                          obj_control_algorithm:ansistring;                     //control algorithm used on single objects into the stream
                          var hdr_data:array of byte;                           //buffer for header data
                          var hdr_size:dword                                    //size, in byte, of the stream header
                          ):integer;
begin
pea_stream_hdr:=INCOMPLETE_FUNCTION;
//two $00 bytes, qualifying the object as a trigger
hdr_data[0]:=$00;
hdr_data[1]:=$00;
//4 bytes trigger definition = POD, marking the beginning of a stream (a pea pod)
hdr_data[2]:=$50;//P
hdr_data[3]:=$4F;//O
hdr_data[4]:=$44;//D
hdr_data[5]:=$00;//zeroed
//byte 0: compression model
if encode_compression(compression_algorithm,hdr_data[6])=SUCCESS then else pea_stream_hdr:=UNKNOWN_COMPRESSION_ALGORITHM;
//byte 1: stream-wide ECC scheme, zeroed since actually no ECC scheme is implemented
hdr_data[7]:=$00;
//byte 2: control model for the stream;
if encode_algo_stream(stream_control_algorithm,hdr_data[8])=SUCCESS then else pea_stream_hdr:=UNKNOWN_CONTROL_ALGORITHM;
//byte 3: control model for each single object in the archive, to allow recovery of non corrupted objects and discarding (depending from the user's threat model) of corrupted object
if encode_algo_nonstream(obj_control_algorithm,hdr_data[9])=SUCCESS then else pea_stream_hdr:=UNKNOWN_OBJ_CONTROL_ALGORITHM;
hdr_size:=10;
if pea_stream_hdr=INCOMPLETE_FUNCTION then pea_stream_hdr:=SUCCESS;
end;

function pea_eax_subhdr ( var cxe:TAES_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCAHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_eax_subhdr:=INCOMPLETE_FUNCTION;
//generate a 512 bit salt and reduce it to 160 bit trough SHA1 (then only 96 bit will be actually used)
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_eax_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCA_EAX_init(cxe, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_eax_subhdr=INCOMPLETE_FUNCTION then pea_eax_subhdr:=SUCCESS;
end;

function pea_tfeax_subhdr ( var cxf:Ttf_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCfHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_tfeax_subhdr:=INCOMPLETE_FUNCTION;
//generate a 512 bit salt and reduce it to 160 bit trough SHA1 (then only 96 bit will be actually used)
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_tfeax_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCf_EAX_init(cxf, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_tfeax_subhdr=INCOMPLETE_FUNCTION then pea_tfeax_subhdr:=SUCCESS;
end;

function pea_speax_subhdr ( var cxs:Tsp_EAXContext;                              //control algorithm context
                          persistent_source:ansistring;                         //path of persistent source of random data
                          fingerprint:TSHA512Digest;                            //system fingerprint
                          ment,kent,fent:THashContext;                          //entropy sources
                          postwhitening:byte;                                   //whitening of digest with other prng
                          var pw_array:array of byte;                           //keying matherial for control algorithm, zeroed on exit
                          var pw_len:word;                                      //size of keying matherial, zeroed on exit
                          var hdr:TFCsHdr;                                      //control algorithm header
                          var hdr_data:array of byte;                           //buffer for header data as array of byte
                          var hdr_size:dword                                    //size, in byte of the header
                          ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_speax_subhdr:=INCOMPLETE_FUNCTION;
//generate a 512 bit salt and reduce it to 160 bit trough SHA1 (then only 96 bit will be actually used)
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_speax_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCs_EAX_init(cxs, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_speax_subhdr=INCOMPLETE_FUNCTION then pea_speax_subhdr:=SUCCESS;
end;

function pea_eax256_subhdr ( var cxe:TAES_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCA256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_eax256_subhdr:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_eax256_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCA_EAX256_init(cxe, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_eax256_subhdr=INCOMPLETE_FUNCTION then pea_eax256_subhdr:=SUCCESS;
end;

function pea_tfeax256_subhdr ( var cxf:TTF_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCf256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_tfeax256_subhdr:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_tfeax256_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCf_EAX256_init(cxf, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_tfeax256_subhdr=INCOMPLETE_FUNCTION then pea_tfeax256_subhdr:=SUCCESS;
end;

function pea_speax256_subhdr ( var cxs:TSP_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCs256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_speax256_subhdr:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_speax256_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCS_EAX256_init(cxs, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_speax256_subhdr=INCOMPLETE_FUNCTION then pea_speax256_subhdr:=SUCCESS;
end;

function pea_eax256_subhdrP ( var cxe:TAES_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCA256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_eax256_subhdrP:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_eax256_subhdrP:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCA_EAX256_initP(cxe, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
//word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_data[14]:=$00; //verification at the end
hdr_data[15]:=$00;
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_eax256_subhdrP=INCOMPLETE_FUNCTION then pea_eax256_subhdrP:=SUCCESS;
end;

function pea_tfeax256_subhdrP ( var cxf:TTF_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCf256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_tfeax256_subhdrP:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_tfeax256_subhdrP:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCf_EAX256_initP(cxf, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
//word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_data[14]:=$00; //verification at the end
hdr_data[15]:=$00;
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_tfeax256_subhdrP=INCOMPLETE_FUNCTION then pea_tfeax256_subhdrP:=SUCCESS;
end;

function pea_speax256_subhdrP ( var cxs:TSP_EAXContext;                           //control algorithm context
                             persistent_source:ansistring;                      //path of persistent source of random data
                             fingerprint:TSHA512Digest;                         //system fingerprint
                             ment,kent,fent:THashContext;                       //entropy sources
                             postwhitening:byte;                                //whitening of digest with other prng
                             var pw_array:array of byte;                        //keying matherial for control algorithm, zeroed on exit
                             var pw_len:word;                                   //size of keying matherial, zeroed on exit
                             var hdr:TFCs256Hdr;                                //control algorithm header
                             var hdr_data:array of byte;                        //buffer for header data as array of byte
                             var hdr_size:dword                                 //size, in byte of the header
                             ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_speax256_subhdrP:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_speax256_subhdrP:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));
// init the context headers
FCS_EAX256_initP(cxs, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
//word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_data[14]:=$00; //verification at the end
hdr_data[15]:=$00;
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_speax256_subhdrP=INCOMPLETE_FUNCTION then pea_speax256_subhdrP:=SUCCESS;
end;

function pea_hmac_subhdr ( var cxh:TFCA_HMAC_Context;                           //control algorithm context
                           persistent_source:ansistring;                        //path of persistent source of random data
                           fingerprint:TSHA512Digest;                           //system fingerprint
                           ment,kent,fent:THashContext;                         //entropy sources
                           postwhitening:byte;                                  //whitening of digest with other prng
                           var pw_array:array of byte;                          //keying matherial for control algorithm, zeroed on exit
                           var pw_len:word;                                     //size of keying matherial, zeroed on exit
                           var hdr:TFCAHdr;                                     //control algorithm header
                           var hdr_data:array of byte;                          //buffer for header data as array of byte
                           var hdr_size:dword                                   //size, in byte of the header
                           ):integer;
var
   salt:TSHA512Digest;
   SHA1Context:THashContext;
   shortsalt:TSHA1Digest;
   k:integer;
begin
pea_hmac_subhdr:=INCOMPLETE_FUNCTION;
if generate_salt(salt,persistent_source,fingerprint,ment,kent,fent,postwhitening)<>SUCCESS then pea_hmac_subhdr:=ERROR_IN_SALT_GENERATION;
SHA1Init(SHA1Context);
SHA1Update(SHA1Context, @salt, sizeof(salt));
SHA1Final(SHA1Context, shortsalt);
move(shortsalt, hdr.salt, sizeof(hdr.salt));//96 bit of salt used
// init the context headers
FCA_HMAC_init(cxh, @pw_array, pw_len, hdr);
// generate the encryption header
hdr_data[0]:=$00; //hdr.FCAsig;
hdr_data[1]:=$00; //hdr.Flags;
dword2bytebuf(hdr.Salt[0],hdr_data,2);
dword2bytebuf(hdr.Salt[1],hdr_data,6);
dword2bytebuf(hdr.Salt[2],hdr_data,10);
word2bytebuf(hdr.PW_Ver,hdr_data,14);
hdr_size:=sizeof(hdr);
for k:=0 to pw_len-1 do pw_array[k]:=0;
pw_len:=0;
if pea_hmac_subhdr=INCOMPLETE_FUNCTION then pea_hmac_subhdr:=SUCCESS;
end;

procedure trigger_eos ( var buf: array of byte); //unused in PEA file format 1.0
begin
buf[0]:=0; // byte 0..1 (word) field for size of the input object qualified name, if 0, the object is a trigger
buf[1]:=0;
buf[2]:=69;//E //byte 2..5 (dword) field of message: E O S $00, End Of Stream
buf[3]:=79;//O
buf[4]:=83;//S
buf[5]:=0;
end;

procedure trigger_eoa ( var buf: array of byte);
begin
buf[0]:=0; // byte 0..1 (word) field for size of the input object qualified name, if 0, the object is a trigger
buf[1]:=0;
buf[2]:=69;//E //byte 2..5 (dword) field of message: E O A $00, End Of Archive
buf[3]:=79;//O
buf[4]:=65;//A
buf[5]:=0;
end;

function pea_parse_archive_header ( read_data:array of byte;                    //the buffer containing the header matherial
                                    var volume_algo:ansistring;                 //control algorithm for the volumes
                                    var datetimeencoding:byte                   //system date and time encoding
                                    ):integer;                                  //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
pea_parse_archive_header:=INCOMPLETE_FUNCTION;
if read_data[0]=234 then
   begin
   if (read_data[1]<>PEA_FILEFORMAT_VER) or (read_data[2]<>PEA_FILEFORMAT_REV) then
      if (read_data[1]>PEA_FILEFORMAT_VER) or ((read_data[1]=PEA_FILEFORMAT_VER) and (read_data[2]>PEA_FILEFORMAT_REV)) then
      begin
      pea_parse_archive_header:=PEA_REVISION_NOT_SUPPORTED;
      exit;
      end;
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
         pea_parse_archive_header:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
         exit;
         end;
      end;
   datetimeencoding:=read_data[6];
   end
else pea_parse_archive_header:=NOT_PEA_HEADER;
if pea_parse_archive_header=INCOMPLETE_FUNCTION then pea_parse_archive_header:=SUCCESS;
end;

function pea_parse_stream_header ( read_data:array of byte;                     //the buffer containing the header matherial
                                   var compr:ansistring;                        //compression algorithm
                                   var compr_level:byte;                        //compression level
                                   var algo:ansistring;                         //control algorithm for the strea
                                   var obj_algo:ansistring                      //control algorithm for objects
                                   ):integer;                                   //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
pea_parse_stream_header:=INCOMPLETE_FUNCTION;
if (read_data[0]<>$00) or //zeroed
   (read_data[1]<>$00) or //zeroed
   (read_data[2]<>$50) or //P
   (read_data[3]<>$4F) or //O
   (read_data[4]<>$44) or //D
   (read_data[5]<>$00)    //zeroed
   then
      begin
      pea_parse_stream_header:=NOT_PEA_HEADER;
      exit;
      end;
//byte 0: compression scheme
if read_data[6]=0 then
   begin compr:='PCOMPRESS0';
   compr_level:=0;
   end
else
   if read_data[6]=1 then
      begin
      compr:='PCOMPRESS1';
      compr_level:=3;
      end
   else
      if read_data[6]=2 then
         begin
         compr:='PCOMPRESS2';
         compr_level:=6;
         end
      else
         if read_data[6]=3 then
            begin
            compr:='PCOMPRESS3';
            compr_level:=9;
            end
         else
            begin
            pea_parse_stream_header:=UNKNOWN_COMPRESSION_ALGORITHM;
            exit;
            end;
//byte 1: stream-wide ECC scheme, ignored since actually (v1r0) none is implemented
//byte 2: stream level control algorithm
case read_data[8] of
   0: algo:='NOALGO'; //hex 00
   1: algo:='ADLER32'; //hex 01
   2: algo:='CRC32'; //hex 02
   3: algo:='CRC64'; //hex 03
  16: algo:='MD5'; //hex 10
  17: algo:='RIPEMD160'; //hex 11
  18: algo:='SHA1'; //hex 12
  19: algo:='SHA256'; //hex 13
  20: algo:='SHA512'; //hex 14
  21: algo:='WHIRLPOOL'; //hex 15
  22: algo:='SHA3_256'; //hex 16
  23: algo:='SHA3_512'; //hex 17
  24: algo:='BLAKE2S'; //hex 18
  25: algo:='BLAKE2B'; //hex 19
  48: algo:='HMAC'; //hex 30
  49: algo:='EAX'; //hex 31
  50: algo:='TF'; //hex 32
  51: algo:='SP'; //hex 33
  65: algo:='EAX256'; //hex 41
  66: algo:='TF256'; //hex 42
  67: algo:='SP256'; //hex 43
  68: algo:='TRIATS'; //hex 44
  69: algo:='TRITSA'; //hex 45
  70: algo:='TRISAT'; //hex 46
   else
      begin
      pea_parse_stream_header:=UNKNOWN_CONTROL_ALGORITHM;
      exit;
      end;
   end;
//byte 3: object level control algorithm
case read_data[9] of
   0: obj_algo:='NOALGO'; //hex 00
   1: obj_algo:='ADLER32'; //hex 01
   2: obj_algo:='CRC32';  //hex 02
   3: obj_algo:='CRC64';  //hex 03
  16: obj_algo:='MD5'; //hex 10
  17: obj_algo:='RIPEMD160'; //hex 11
  18: obj_algo:='SHA1'; //hex 12
  19: obj_algo:='SHA256'; //hex 13
  20: obj_algo:='SHA512'; //hex 14
  21: obj_algo:='WHIRLPOOL'; //hex 15
  22: obj_algo:='SHA3_256'; //hex 16
  23: obj_algo:='SHA3_512'; //hex 17
  24: obj_algo:='BLAKE2S'; //hex 18
  25: obj_algo:='BLAKE2B'; //hex 19
   else
      begin
      pea_parse_stream_header:=UNKNOWN_OBJ_CONTROL_ALGORITHM;
      exit;
      end;
   end;
if pea_parse_stream_header=INCOMPLETE_FUNCTION then pea_parse_stream_header:=SUCCESS;
end;

function pea_parse_crypto_subheader ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var hdr:TFCAHdr                           //header structure
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
pea_parse_crypto_subheader:=INCOMPLETE_FUNCTION;
hdr.Salt[0]:=read_data[2] + (read_data[3] shl 8) + (read_data[4] shl 16) + (read_data[5] shl 24);
hdr.Salt[1]:=read_data[6] + (read_data[7] shl 8) + (read_data[8] shl 16) + (read_data[9] shl 24);
hdr.Salt[2]:=read_data[10] + (read_data[11] shl 8) + (read_data[12] shl 16) + (read_data[13] shl 24);
hdr.PW_Ver:=read_data[14] + (read_data[15] shl 8);
{//unneeded since FCAsig and Flags information is declared in stream header; bytes are zeroed
hdr.FCAsig:=read_data[0];
hdr.Flags:=read_data[1];
if ((hdr.FCASig<>C_FCA_Sig) or (hdr.Flags and $F0 <> $A0)) then
   begin
   pea_parse_crypto_subheader:=INVALID_ENCRYPTION_HEADER;
   exit;
   end;
if hdr.Flags and $02 <> 0 then
   begin
   pea_parse_crypto_subheader:=NOT_SUPPORTED_COMPRESSION_MODE;
   exit;
   end;}
pea_parse_crypto_subheader:=SUCCESS;
end;

function pea_parse_crypto_subheaderf ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var fhdr:TFCfHdr                           //header structure
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
pea_parse_crypto_subheaderf:=INCOMPLETE_FUNCTION;
fhdr.Salt[0]:=read_data[2] + (read_data[3] shl 8) + (read_data[4] shl 16) + (read_data[5] shl 24);
fhdr.Salt[1]:=read_data[6] + (read_data[7] shl 8) + (read_data[8] shl 16) + (read_data[9] shl 24);
fhdr.Salt[2]:=read_data[10] + (read_data[11] shl 8) + (read_data[12] shl 16) + (read_data[13] shl 24);
fhdr.PW_Ver:=read_data[14] + (read_data[15] shl 8);
pea_parse_crypto_subheaderf:=SUCCESS;
end;

function pea_parse_crypto_subheaders ( read_data:array of byte;                  //the buffer containing the header matherial
                                      var shdr:TFCsHdr                           //header structure
                                      ):integer;                                //note that the function will exit at all error condition (excluded INCOMPLETE_FUNCTION); in this way it will always be returned the code of the first error encountered
begin
pea_parse_crypto_subheaders:=INCOMPLETE_FUNCTION;
shdr.Salt[0]:=read_data[2] + (read_data[3] shl 8) + (read_data[4] shl 16) + (read_data[5] shl 24);
shdr.Salt[1]:=read_data[6] + (read_data[7] shl 8) + (read_data[8] shl 16) + (read_data[9] shl 24);
shdr.Salt[2]:=read_data[10] + (read_data[11] shl 8) + (read_data[12] shl 16) + (read_data[13] shl 24);
shdr.PW_Ver:=read_data[14] + (read_data[15] shl 8);
pea_parse_crypto_subheaders:=SUCCESS;
end;

function decode_compression_algo ( compr:ansistring;                            //compressor
                                   var compr_level:byte                         //compressor option
                                   ):integer;
begin
decode_compression_algo:=INCOMPLETE_FUNCTION;
case upcase(compr) of
  'PCOMPRESS0': compr_level:=0;
  'PCOMPRESS1': compr_level:=3;
  'PCOMPRESS2': compr_level:=6;
  'PCOMPRESS3': compr_level:=9;
  else decode_compression_algo:=UNKNOWN_COMPRESSION_ALGORITHM;
  end;
if decode_compression_algo=INCOMPLETE_FUNCTION then decode_compression_algo:=SUCCESS;
end;

function decode_control_algo ( algo:ansistring;                                 //control algorithm
                               var headersize:byte;                             //size of the header needed
                               var authsize:byte;                               //authntication tag size
                               var pwneeded:boolean                             //password is required by the control algorithm
                               ):integer;
begin
authsize:=255;
decode_control_algo:=INCOMPLETE_FUNCTION;
case upcase(algo) of
   'NOALGO':
   begin
   headersize:=10;
   authsize:=0;
   pwneeded:=false;
   end;
   'TRIATS','TRITSA','TRISAT':
   begin
   headersize:=10+16+16+16;
   authsize:=16+16+16;
   pwneeded:=true;
   end;
   'EAX256':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'TF256':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'SP256':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'EAX':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'TF':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'SP':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'HMAC':
   begin
   headersize:=10+16;
   authsize:=16;
   pwneeded:=true;
   end;
   'ADLER32':
   begin
   headersize:=10;
   authsize:=4;
   pwneeded:=false;
   end;
   'CRC32':
   begin
   headersize:=10;
   authsize:=4;
   pwneeded:=false;
   end;
   'CRC64':
   begin
   headersize:=10;
   authsize:=8;
   pwneeded:=false;
   end;
   'MD5':
   begin
   headersize:=16;
   authsize:=16;
   pwneeded:=false;
   end;
   'RIPEMD160':
   begin
   headersize:=10;
   authsize:=20;
   pwneeded:=false;
   end;
   'SHA1':
   begin
   headersize:=10;
   authsize:=20;
   pwneeded:=false;
   end;
   'SHA256':
   begin
   headersize:=10;
   authsize:=32;
   pwneeded:=false;
   end;
   'SHA512':
   begin
   headersize:=10;
   authsize:=64;
   pwneeded:=false;
   end;
   'SHA3_256':
   begin
   headersize:=10;
   authsize:=32;
   pwneeded:=false;
   end;
   'SHA3_512':
   begin
   headersize:=10;
   authsize:=64;
   pwneeded:=false;
   end;
   'WHIRLPOOL':
   begin
   headersize:=10;
   authsize:=64;
   pwneeded:=false;
   end;
   'BLAKE2S':
   begin
   headersize:=10;
   authsize:=32;
   pwneeded:=false;
   end;
   'BLAKE2B':
   begin
   headersize:=10;
   authsize:=64;
   pwneeded:=false;
   end;
   end;
if authsize=255 then decode_control_algo:=UNKNOWN_CONTROL_ALGORITHM;
if decode_control_algo=INCOMPLETE_FUNCTION then decode_control_algo:=SUCCESS;
end;

function decode_obj_control_algo ( obj_algo:ansistring;                         //control algorithm
                                   var authsize:byte                            //authentication tag size
                                   ):integer;
begin
decode_obj_control_algo:=INCOMPLETE_FUNCTION;
case upcase(obj_algo) of
   'NOALGO': authsize:=0;
   'ADLER32': authsize:=4;
   'CRC32': authsize:=4;
   'CRC64': authsize:=8;
   'MD5': authsize:=16;
   'RIPEMD160': authsize:=20;
   'SHA1': authsize:=20;
   'SHA256': authsize:=32;
   'SHA512': authsize:=64;
   'SHA3_256': authsize:=32;
   'SHA3_512': authsize:=64;
   'WHIRLPOOL': authsize:=64;
   'BLAKE2S': authsize:=32;
   'BLAKE2B': authsize:=64;
   else authsize:=255;
   end;
if authsize=255 then decode_obj_control_algo:=UNKNOWN_OBJ_CONTROL_ALGORITHM;
if decode_obj_control_algo=INCOMPLETE_FUNCTION then decode_obj_control_algo:=SUCCESS;
end;

function decode_volume_control_algo ( volume_algo:ansistring;                   //control algorithm
                                      var authsize:byte                         //authentication tag size
                                      ):integer;
begin
decode_volume_control_algo:=INCOMPLETE_FUNCTION;
case upcase(volume_algo) of
   'NOALGO': authsize:=0;
   'ADLER32': authsize:=4;
   'CRC32': authsize:=4;
   'CRC64': authsize:=8;
   'MD5': authsize:=16;
   'RIPEMD160': authsize:=20;
   'SHA1': authsize:=20;
   'SHA256': authsize:=32;
   'SHA512': authsize:=64;
   'SHA3_256': authsize:=32;
   'SHA3_512': authsize:=64;
   'WHIRLPOOL': authsize:=64;
   'BLAKE2S': authsize:=32;
   'BLAKE2B': authsize:=64;
   else authsize:=255;
   end;
if authsize=255 then decode_volume_control_algo:=UNKNOWN_VOLUME_CONTROL_ALGORITHM;
if decode_volume_control_algo=INCOMPLETE_FUNCTION then decode_volume_control_algo:=SUCCESS;
end;

function encode_compression ( compression_algorithm:ansistring;                 //compression scheme
                              var code:byte                                     //PEA code for given compression scheme
                              ):integer;
begin
encode_compression:=INCOMPLETE_FUNCTION;
case upcase(compression_algorithm) of
   'PCOMPRESS0': code:=$00; //no compression scheme
   'PCOMPRESS1': code:=$01; //PEACH compression scheme using zcompres on given buffer size with deflate compression option = 3
   'PCOMPRESS2': code:=$02; //PEACH compression scheme using zcompres on given buffer size with deflate compression option = 6 (default for zcompres)
   'PCOMPRESS3': code:=$03; //PEACH compression scheme using zcompres on given buffer size with deflate compression option = 9 (max)
   else encode_compression:=UNKNOWN_COMPRESSION_ALGORITHM;
   end;
if encode_compression=INCOMPLETE_FUNCTION then encode_compression:=SUCCESS;
end;

function encode_algo_stream ( stream_control_algorithm:ansistring;              //stream control algorithm
                              var code:byte                                     //PEA code for given algorithm
                              ):integer;
begin
encode_algo_stream:=INCOMPLETE_FUNCTION;
case upcase(stream_control_algorithm) of
   'NOALGO': code:=$00;
   'ADLER32': code:=$01;
   'CRC32': code:=$02;
   'CRC64': code:=$03;
   'MD5': code:=$10;
   'RIPEMD160': code:=$11;
   'SHA1': code:=$12;
   'SHA256': code:=$13;
   'SHA512': code:=$14;
   'WHIRLPOOL': code:=$15;
   'SHA3_256': code:=$16;
   'SHA3_512': code:=$17;
   'BLAKE2S': code:=$18;
   'BLAKE2B': code:=$19;
   'HMAC': code:=$30;
   'EAX': code:=$31;
   'TF': code:=$32;
   'SP': code:=$33;
   'EAX256': code:=$41;
   'TF256': code:=$42;
   'SP256': code:=$43;
   'TRIATS': code:=$44;
   'TRITSA': code:=$45;
   'TRISAT': code:=$46;
   else encode_algo_stream:=UNKNOWN_CONTROL_ALGORITHM;
   end;
if encode_algo_stream=INCOMPLETE_FUNCTION then encode_algo_stream:=SUCCESS;
end;

function encode_algo_nonstream ( control_algorithm:ansistring;                  //volume or object control algorithm
                                 var code:byte                                  //PEA code for given algorithm
                                 ):integer;
begin
encode_algo_nonstream:=INCOMPLETE_FUNCTION;
case upcase(control_algorithm) of
   'NOALGO': code:=$00;
   'ADLER32': code:=$01;
   'CRC32': code:=$02;
   'CRC64': code:=$03;
   'MD5': code:=$10;
   'RIPEMD160': code:=$11;
   'SHA1': code:=$12;
   'SHA256': code:=$13;
   'SHA512': code:=$14;
   'WHIRLPOOL': code:=$15;
   'SHA3_256': code:=$16;
   'SHA3_512': code:=$17;
   'BLAKE2S': code:=$18;
   'BLAKE2B': code:=$19;
   else encode_algo_nonstream:=UNKNOWN_CONTROL_ALGORITHM;
   end;
if encode_algo_nonstream=INCOMPLETE_FUNCTION then encode_algo_nonstream:=SUCCESS;
end;

{please note that following systems and CPUs are the ones supported by FreePascal
Compiler, it doesn't mean that PEA was ported on those platforms; PEA is actually
tested by the Authore only under Windows and Linux and rely on some x86 ASM parts
in crypto library used}

procedure get_OS(var code:byte);                                                //OS type
begin
code:=$00;//unrecognized system, it is overwritten if one of the following conditions is met
{$IFDEF MSWINDOWS}//generic Windows, replaced with following $0* codes if a more specific match is found
code:=$10;
{$ENDIF}
{$IFDEF WIN32}
code:=$11;
{$ENDIF}
{$IFDEF WIN64}
code:=$12;
{$ENDIF}
{$IFDEF WINCE}
code:=$13;
{$ENDIF}
{$IFDEF GO32V2}
code:=$21;
{$ENDIF}
{$IFDEF OS2}
code:=$22;
{$ENDIF}
{$IFDEF UNIX}//generic UNIX, replaced with following $3* or $4* codes if a more specific match is found
code:=$30;
{$ENDIF}
{$IFDEF FREEBSD}
code:=$31;
{$ENDIF}
{$IFDEF NETBSD}
code:=$32;
{$ENDIF}
{$IFDEF LINUX}
code:=$33;
{$ENDIF}
{$IFDEF BEOS}
code:=$34;
{$ENDIF}
{$IFDEF QNX}
code:=$35;
{$ENDIF}
{$IFDEF SUNOS}
code:=$36;
{$ENDIF}
{$IFDEF DARWIN}
code:=$37;
{$ENDIF}
{$IFDEF AMIGA}
code:=$51;
{$ENDIF}
{$IFDEF ATARI}//Atari TOS
code:=$52;
{$ENDIF}
{$IFDEF MAC}//Classic
code:=$53;
{$ENDIF}
{$IFDEF PALMOS}
code:=$54;
{$ENDIF}
end;

procedure get_system_datetimeencoding(var code:byte);                           //system time-date encoding, uses same code as OS (family code, if uses same encoding)
begin
code:=$00; //unrecognized, replaced if a match is found
{$IFDEF MSWINDOWS}
code:=$10;
{$ENDIF}
{$IFDEF WIN32}
code:=$10;
{$ENDIF}
{$IFDEF WIN64}
code:=$10;
{$ENDIF}
{$IFDEF UNIX}
code:=$30;
{$ENDIF}
{$IFDEF MAC}//Classic
code:=$53;
{$ENDIF}
end;

procedure get_CPUe(var code:byte);                                              //CPU and endianness (in MSB)
begin
code:=$00; //cpu type, < $80 (encoded in ls 7 bit); 0:= unrecognized, replaced if a match is found
{$IFDEF CPU32}//generic 32 bit CPU, replaced if a more specific match is found
code:=$01;
{$ENDIF}
{$IFDEF CPU64}//generic 64 bit CPU, replaced if a more specific match is found
code:=$02;
{$ENDIF}
{$IFDEF CPU86}
code:=$11;
{$ENDIF}
{$IFDEF CPU87}
code:=$12;
{$ENDIF}
{$IFDEF CPUI386}
code:=$13;
{$ENDIF}
{$IFDEF CPUX86_64}
code:=$21;
{$ENDIF}
{$IFDEF CPU68k}
code:=$31;
{$ENDIF}
{$IFDEF CPUM68020}
code:=$32;
{$ENDIF}
{$IFDEF CPU68}
code:=$33;
{$ENDIF}
{$IFDEF CPUSPARC}
code:=$34;
{$ENDIF}
{$IFDEF CPUALPHA}
code:=$35;
{$ENDIF}
{$IFDEF CPUPOWERPC}
code:=$36;
{$ENDIF}
{$IFDEF ENDIAN_BIG}//if processor is declared big endian (some processors don't have a fixed endian setting) the msb is set to 1, otherwise is 0
code:=code + $80;
{$ENDIF}
end;

function use_keyfile ( keyfilename:ansistring;                                  //qualified name of the keyfile
                       keysize:word;                                            //size of keying matherial to read, in byte
                       var numread:dword;                                       //size of keying matherial actually read, in byte
                       var pw_array:array of byte;                              //keying matherial as array of byte
                       var ind:word                                             //index in the array where starting to write the keyfile provided keying matherial
                       ):integer;
var
   f_key:file of byte;
   buf_data:array [0..16383] of byte;
   k:integer;
begin
use_keyfile:=INCOMPLETE_FUNCTION;
try
   assignfile(f_key,keyfilename);
   filemode:=0;
   reset(f_key);
   blockread (f_key,buf_data,keysize,numread);
   closefile(f_key);
   for k:=0 to numread-1 do pw_array[ind+k]:=buf_data[k];
   ind:=ind+numread;
except
   use_keyfile:=NON_ACCESSIBLE_KEYFILE;
end;
if use_keyfile=INCOMPLETE_FUNCTION then use_keyfile:=SUCCESS;
end;

function generate_salt ( var salt:TSHA512Digest;                                //give a SHA512 digest as random salt
                         persistent_source:ansistring;                          //path of persistent source of random data
                         fingerprint:TSHA512Digest;                             //fingerprint (SHA512 digest) of system state, usually created at application startup
                         var ment,kent,fent:THashContext;                       //hash context of mouse sampling and keyboard sampling
                         postwhitening:byte                                     //whitening of digest with other prng
                         ):integer;
var
   SHA512Context: THashContext;
   tdigest,memdigest,salt1:TSHA512Digest;
   mentd,kentd,fentd: TWhirlDigest;
   i:integer;
   randf:file of byte;
   randarr:array [0..255] of byte;
   key256:array[0..31]of byte;
   aes_iv,tf_iv:array[0..15]of byte;
   aes_ctx:TAESContext;
   tf_ctx:TTFContext;
begin
generate_salt:=INCOMPLETE_FUNCTION;
SHA512Init(SHA512Context);
// 1: get and hash counters (SHA512 digest)
if get_timers(tdigest)=true then SHA512Update(SHA512Context, @tdigest, sizeof(tdigest)); //get timers entropy as SHA512 digest
// 2: get and hash memory statistics (SHA512 digest)
if get_memory(memdigest)=true then SHA512Update(SHA512Context, @memdigest, sizeof(memdigest)); //get memory statistics (true on Windows only) entropy as SHA512 digest
// 3: hash the fingerprint (SHA512 digest)
SHA512Update(SHA512Context, @fingerprint, sizeof(fingerprint));
// 4: finalize and hash Whirl512 digest of mouse enthropy, if collected
Whirl_Final(ment, mentd);
SHA512Update(SHA512Context, @mentd, sizeof(mentd));
// 5: finalize and hash Whirl512 digest of keyboard enthropy, if collected
Whirl_Final(kent, kentd);
SHA512Update(SHA512Context, @kentd, sizeof(kentd));
// 6: finalize and hash Whirl512 digest of enthropy from files, if collected
Whirl_Final(fent, fentd);
SHA512Update(SHA512Context, @fentd, sizeof(fentd));
// 7: hash first 256 bytes of persistent random-collecting file, if found
try
i:=0;
assignfile(randf,persistent_source);
reset(randf);
blockread(randf,randarr,256,i);
closefile(randf);
if i>0 then SHA512Update(SHA512Context, @randarr, i);
except
end;
SHA512Final(SHA512Context, salt);
// 8: optional further whitening of salt with prng functions
if (postwhitening and 1) <> 0 then //use internal prng (usually a weak linear prng)
   begin
   randomize;
   for i:=0 to 63 do salt[i]:=salt[i] xor random(256);
   end;
if (postwhitening and 2) <> 0 then //use AES256 CTR keyed with xor of first 256 bit of 4 digests (tdigest,memdigest,mentd,kentd,fentd,fingerprint), first 128 bit of tdigest xor memdigest are used as IV
   begin
   salt1:=salt;
   for i:=0 to 31 do key256[i]:=tdigest[i] xor memdigest[i] xor fingerprint[i] xor mentd[i] xor kentd[i] xor fentd[i];
   for i:=0 to 15 do aes_iv[i]:=tdigest[i] xor memdigest[i];
   AES_CTR_Init(key256, 256, aes_iv, aes_ctx);
   AES_CTR_Encrypt(@salt1, @salt, sizeof(salt), aes_ctx);
   end;
if (postwhitening and 4) <> 0 then //use Twofish256 CTR keyed with xor of last 256 bit of 4 digests (ment,kent,fent,fingerprint), second 128 bit of tdigest xor memdigest are used as IV
   begin
   salt1:=salt;
   for i:=0 to 31 do key256[i]:=tdigest[i+32] xor memdigest[i+32] xor fingerprint[i+32] xor mentd[i+32] xor kentd[i+32] xor fentd[i+32];
   for i:=0 to 15 do tf_iv[i]:=tdigest[i+16] xor memdigest[i+16];
   tf_CTR_Init(key256, 256, tf_iv, tf_ctx);
   tf_CTR_Encrypt(@salt1, @salt, sizeof(salt), tf_ctx);
   end;
if generate_salt=INCOMPLETE_FUNCTION then generate_salt:=SUCCESS;
end;

function generate_keyf ( var key:TKEY2048;                                      //2048 bit key (array of byte); it can be also used to load external data to introduce additional entropy to the function
                         persistent_source:ansistring;                          //path of persistent source of random data
                         fingerprint:TSHA512Digest;                             //fingerprint (SHA512 digest) of system state, usually created at application startup
                         var ment,kent,fent:THashContext                        //hash context of mouse sampling and keyboard sampling
                         ):integer;
var
   SHA512Context,WhirlContext:THashContext;
   tdigest,memdigest,d1,d3:TSHA512Digest;
   mentd,kentd,fentd,d2,d4: TWhirlDigest;
   i:integer;
   randf:file of byte;
   randarr:array [0..255] of byte;
   key256:array[0..31]of byte;
   aes_iv,tf_iv:array[0..15]of byte;
   aes_ctx:TAESContext;
   tf_ctx:TTFContext;
begin
generate_keyf:=INCOMPLETE_FUNCTION;
SHA512Init(SHA512Context);
Whirl_Init(WhirlContext);
// 1: get and hash counters
if get_timers(tdigest)=true then
   begin
   SHA512Update(SHA512Context, @tdigest, sizeof(tdigest));
   Whirl_Update(WhirlContext, @tdigest, sizeof(tdigest));
   end;
// 2: get and hash memory statistics (true on windows only)
if get_memory(memdigest)=true then
   begin
   SHA512Update(SHA512Context, @memdigest, sizeof(memdigest));
   Whirl_Update(WhirlContext, @memdigest, sizeof(memdigest));
   end;
// 3: hash the fingerprint (SHA512 digest)
SHA512Update(SHA512Context, @fingerprint, sizeof(fingerprint));
Whirl_Update(WhirlContext, @fingerprint, sizeof(fingerprint));
// 4: finalize and hash Whirl512 digest of mouse enthropy, if collected
Whirl_Final(ment, mentd);
SHA512Update(SHA512Context, @mentd, sizeof(mentd));
Whirl_Update(WhirlContext, @mentd, sizeof(mentd));
// 5: finalize and hash Whirl512 digest of keyboard enthropy, if collected
Whirl_Final(kent, kentd);
SHA512Update(SHA512Context, @kentd, sizeof(kentd));
Whirl_Update(WhirlContext, @kentd, sizeof(kentd));
// 6: finalize and hash Whirl512 digest of enthropy from files, if collected
Whirl_Final(fent, fentd);
SHA512Update(SHA512Context, @fentd, sizeof(fentd));
Whirl_Update(WhirlContext, @fentd, sizeof(fentd));
// 7: hash first 256 bytes of persistent random-collecting file, if found
try
i:=0;
assignfile(randf,persistent_source);
reset(randf);
blockread(randf,randarr,256,i);
closefile(randf);
if i>0 then
   begin
   SHA512Update(SHA512Context, @randarr, i);
   Whirl_Update(WhirlContext, @randarr, i);
   end;
except
end;
SHA512Final(SHA512Context, d1); //finalize the two hashes of entropy pools
Whirl_Final(WhirlContext, d2);
SHA512Init(SHA512Context); //hash the two digest with both hashes, creating d3 and d4
Whirl_Init(WhirlContext);
SHA512Update(SHA512Context, @d1, sizeof(d1));
Whirl_Update(WhirlContext, @d1, sizeof(d1));
SHA512Update(SHA512Context, @d2, sizeof(d2));
Whirl_Update(WhirlContext, @d2, sizeof(d2));
SHA512Final(SHA512Context, d3);
Whirl_Final(WhirlContext, d4);
for i:=0 to 63 do key[i]:=key[i] xor d1[i];
for i:=0 to 63 do key[i+64]:=key[i+64] xor d2[i];
for i:=0 to 63 do key[i+128]:=key[i+128] xor d3[i];
for i:=0 to 63 do key[i+192]:=key[i+192] xor d4[i];
// 8: whitening of key array with prng functions
//use internal prng (usually a weak linear prng)
randomize;
for i:=0 to 255 do key[i]:=key[i] xor random(256);
//use AES256 CTR keyed with first 256 bit of d1, first 128 bit of d2 used as IV
for i:=0 to 31 do key256[i]:=d1[i];
for i:=0 to 15 do aes_iv[i]:=d2[i];
AES_CTR_Init(key256, 256, aes_iv, aes_ctx);
AES_CTR_Encrypt(@key, @key, 256, aes_ctx);
//use Twofish256 CTR keyed with first 256 bit of d3, first 128 bit of d4 used as IV
for i:=0 to 31 do key256[i]:=d3[i];
for i:=0 to 15 do tf_iv[i]:=d4[i];
tf_CTR_Init(key256, 256, tf_iv, tf_ctx);
tf_CTR_Encrypt(@key, @key, 256, tf_ctx);
if generate_keyf=INCOMPLETE_FUNCTION then generate_keyf:=SUCCESS;
end;

function get_timers (var tdigest:TSHA512Digest):boolean;                        //give a SHA512 digest as whitened pool of enthropy gathered from timers
{
Please note that subsequent calls to this function decrease the value of real
enthropy gathered.
The function should be called once during program execution, otherwise enthropy
in timers will be bound to the unpredictability of time interval between the two
calls, so it will be partially worthwhile only calling the procedure after an
unpredictable amount of time (either bound to non trivial user input or to complex
computational tasks with great degree of timing indetermination).
The function is useless for generating secret values if the attacker can guess
when the function was called, but will still be suitable to generate unique
pseudorandom values (like for salting).
Please note that all counters featured are more or less directly related.
}
var
   SHA512Context: THashContext;
   Ctr: TCtrRec;
   TS:TTimeStamp;
   {$IFDEF MSWINDOWS}
   i:integer;
   freq,mutime:int64;
   {$ENDIF}
   {$IFDEF LINUX}
   yy,mt,dd,hh,mm,ss,ms,us:word;
   {$ENDIF}
begin
get_timers:=false;
SHA512Init(SHA512Context);
randomize;                                                                      //generate random seed
SHA512Update(SHA512Context, @randseed, sizeof(randseed));
_ReadCounter(Ctr);                                                              //get pentium Time Stamp Counter (CPU cycles from boot), if present
SHA512Update(SHA512Context, @Ctr, sizeof(Ctr));
TS:=datetimetotimestamp(now);                                                   //get date and time with millisecond precision
SHA512Update(SHA512Context, @TS.date, sizeof(TS.date));
SHA512Update(SHA512Context, @TS.time, sizeof(TS.time));
{$IFDEF MSWINDOWS} //Windows high resolution system timer
QueryPerformanceFrequency(freq);//all Windows from 95/NT3.1                     //frequency of the high-resolution performance counter, if present
SHA512Update(SHA512Context, @freq, sizeof(freq));
QueryPerformanceCounter(mutime);//all Windows from 95/NT3.1                     //get microsecond time with high-resolution timer under Windows, if present
SHA512Update(SHA512Context, @mutime, sizeof(mutime));
i:=GetTickCount64;//from Vista+                                                 //ticket count since last boot under Windows (wraps after about 49 days)
SHA512Update(SHA512Context, @i, sizeof(i));
{$ENDIF}
SHA512Final(SHA512Context, tdigest);
get_timers:=true;
end;

function get_memory (var memdigest:TSHA512Digest):boolean;                      //give a SHA512 digest as whitened pool of enthropy gathered from memory statistics (Windows only)
{
Please note that subsequent calls to this function decrease the value of real
enthropy gathered since few memory related variables (mainly free memory) will
change with high frequence.
This procedure actually works only under Windows.
}
var
   SHA512Context: THashContext;
   {$IFDEF MSWINDOWS}MemoryStatus: TMemoryStatus;{$ENDIF}
begin
get_memory:=false;
{$IFDEF MSWINDOWS}
SHA512Init(SHA512Context);
GlobalMemoryStatus(MemoryStatus); //all Windows from 95                          //get memory status
//GlobalMemoryStatusEx could be used for all NTs post Window 2000
SHA512Update(SHA512Context, @MemoryStatus, sizeof(MemoryStatus));
SHA512Final(SHA512Context, memdigest);
get_memory:=true;
{$ENDIF}
end;

function get_fingerprint (var fingerprint:TSHA512Digest; usetempfiles:boolean):boolean; //give SHA512 digest as whitened pool of enthropy from system or session variables
{
Variables chosen for generating that digest are meant to allow each system to
have a quite unique "fingerprint", not necessarily constant during time.
Those variables may be some times easy and some times hard to guess for an attacker,
may either be constant during time (i.e. system name) or tend to vary (i.e. free
disk space, process id) even very quickly (timers, free memory).
Time for getting system's fingerprint may vary, especially if using temporary files,
since operation times may be delayed from difficult to predict factors, especially
on multitasking machines.
}
var
   tdigest,memdigest:TSHA512Digest;
   SHA512Context: THashContext;
   hStatus:THeapStatus;
   flist,fattr_dec:TFoundList;
   fsizes:TFoundListSizes;
   ftimes:TFoundListAges;
   fattr:TFoundListAttrib;
   var_qword,nfiles,ndirs:qword;
   dummy:ansistring;
   i:integer;
begin
get_fingerprint:=false;
SHA512Init(SHA512Context);
if get_timers(tdigest)=true then SHA512Update(SHA512Context, @tdigest, sizeof(tdigest)); //get timers entropy as SHA512 digest
if get_memory(memdigest)=true then SHA512Update(SHA512Context, @memdigest, sizeof(memdigest)); //get memory statistics (true on Windows only) entropy as SHA512 digest
SHA512Update(SHA512Context, @GetProcessID, sizeof(GetProcessID));               //PID assigned
SHA512Update(SHA512Context, @getlastoserror, sizeof(getlastoserror));           //last OS error
hStatus := GetHeapStatus;
SHA512Update(SHA512Context, @hstatus.TotalFree, sizeof(hstatus.TotalFree));     //free heap memory
SHA512Update(SHA512Context, @hstatus.TotalAllocated, sizeof(hstatus.TotalAllocated)); //allocated heap memory
var_qword:=disksize(0);
SHA512Update(SHA512Context, @var_qword, sizeof(var_qword));                     //size of active disk
var_qword:=diskfree(0);
SHA512Update(SHA512Context, @var_qword, sizeof(var_qword));                     //free space on active disk
dummy:=(getcurrentdir);                                               //current dir
for i:=0 to GetEnvironmentVariableCount do dummy:=dummy+(GetEnvironmentString(i)); //environment variables
dummy:=dummy+inttostr(GetEnvironmentVariableCount);                             //count of env. variables, as string
SHA512Update(SHA512Context, @dummy, length(dummy));
dummy:='';
if usetempfiles=true then
   try
   ListDetails ( gettempdir(false),'*',faanyfile,
                 true,
                 true,
                 nfiles,ndirs,
                 flist,fsizes,ftimes,fattr,fattr_dec);
   dummy:=gettempdir(false);
   if nfiles>0 then for i:=0 to nfiles-1 do dummy:=dummy+flist[i]+inttostr(fsizes[i])+inttostr(ftimes[i])+inttostr(fattr[i])+fattr_dec[i];  //qualified names, sizes, fileages and fileattributes of user's temp files
   ListDetails ( gettempdir(true),'*',faanyfile,
                 true,
                 true,
                 nfiles,ndirs,
                 flist,fsizes,ftimes,fattr,fattr_dec);
   dummy:=gettempdir(true);
   if nfiles>0 then for i:=0 to nfiles-1 do dummy:=dummy+flist[i]+inttostr(fsizes[i])+inttostr(ftimes[i])+inttostr(fattr[i])+fattr_dec[i];  //qualified names, sizes, fileages and fileattributes of system's temp files (if accessible, otherwise user temp files again)
   SHA512Update(SHA512Context, @dummy, length(dummy));
   dummy:='';
   except
   SHA512Update(SHA512Context, @getlastoserror, sizeof(getlastoserror));        //in case of unexpected error accessing temp files, check last OS error again
   end;
if get_memory(memdigest)=true then SHA512Update(SHA512Context, @memdigest, sizeof(memdigest));
if get_timers(tdigest)=true then SHA512Update(SHA512Context, @tdigest, sizeof(tdigest)); //get memory and timers again; especially if using temp files the time of accomplishing all tasks may vary in a way quite difficult to predict
SHA512Final(SHA512Context, fingerprint);
get_fingerprint:=true;
end;

function sample_mouse_ent( var ment:THashContext; x,y:integer):boolean;         //update hash context with enthropy gathered by mouse movement, cohordinates and timing
{
Please note that unlike get_* functions, sample_* functions doesn't give a digest
as output but rather update an hash context that has to be initialized and finalized
externally.
In example to sample mouse movements on a form you may init the hash on form create,
call sample_mouse_ent to update enthropy pool on mouse move and let generate_salt
function take care of finalizing the hash i.e. calling it when pressing a button.
}
var
   tdigest,memdigest:TSHA512Digest;
begin
sample_mouse_ent:=false;
if get_timers(tdigest)=true then Whirl_Update(ment, @tdigest, sizeof(tdigest)); //get timers entropy as SHA512 digest
if get_memory(memdigest)=true then Whirl_Update(ment, @memdigest, sizeof(memdigest)); //get memory statistics (true on Windows only) entropy as SHA512 digest
Whirl_Update(ment,@x,4);
Whirl_Update(ment,@y,4);
sample_mouse_ent:=true;
end;

function sample_keyb_ent ( var kent:THashContext; k:byte):boolean;              //update hash context with enthropy gathered by keyboard, key pressed and and timing
{
Applies the same mode of use explained for sample_mouse_ent
}
var
   tdigest,memdigest:TSHA512Digest;
begin
sample_keyb_ent:=false;
if get_timers(tdigest)=true then Whirl_Update(kent, @tdigest, sizeof(tdigest)); //get timers entropy as SHA512 digest
if get_memory(memdigest)=true then Whirl_Update(kent, @memdigest, sizeof(memdigest)); //get memory statistics (true on Windows only) entropy as SHA512 digest
Whirl_Update(kent,@k,1);
sample_keyb_ent:=true;
end;

function sample_file_ent ( var fent:THashContext; fname:ansistring):boolean;    //update hash context with enthropy gathered by a file: content, name and timing
{
Applies the same mode of use explained for sample_mouse_ent
}
var
   tdigest,memdigest:TSHA512Digest;
   bfile:file of byte;
   bbuff:array [0..32767] of byte;
   numread:integer;
begin
sample_file_ent:=false;
if get_timers(tdigest)=true then Whirl_Update(fent, @tdigest, sizeof(tdigest)); //get timers entropy as SHA512 digest
if get_memory(memdigest)=true then Whirl_Update(fent, @memdigest, sizeof(memdigest)); //get memory statistics (true on Windows only) entropy as SHA512 digest
Whirl_Update(fent, @fname, length(fname));
assignfile(bfile,fname);
reset(bfile);
filemode:=0;
numread:=1;
repeat
   blockread (bfile,bbuff,32768,numread);
   Whirl_Update(fent,@bbuff,numread);
until (numread=0);
close(bfile);
if get_memory(memdigest)=true then Whirl_Update(fent, @memdigest, sizeof(memdigest));
if get_timers(tdigest)=true then Whirl_Update(fent, @tdigest, sizeof(tdigest)); //get memory and timers again since times to read the file may not be easily predictable
sample_file_ent:=true;
end;

function word2bytebuf ( inword:word;                                            //word to write in the buffer starting from the given address, please note that size of the buffer is not checked
                        var buf:array of byte;
                        addr:dword
                        ):integer;
begin
word2bytebuf:=INCOMPLETE_FUNCTION;
buf[addr]  :=inword;
buf[addr+1]:=inword shr 8;
if word2bytebuf=INCOMPLETE_FUNCTION then word2bytebuf:=SUCCESS;
end;

function dword2bytebuf ( indword:dword;                                         //dword to write in the buffer starting from the given address, please note that size of the buffer is not checked
                         var buf:array of byte;
                         addr:dword
                         ):integer;
begin
dword2bytebuf:=INCOMPLETE_FUNCTION;
buf[addr]  :=indword;
buf[addr+1]:=indword shr 8;
buf[addr+2]:=indword shr 16;
buf[addr+3]:=indword shr 24;
if dword2bytebuf=INCOMPLETE_FUNCTION then dword2bytebuf:=SUCCESS;
end;

function qword2bytebuf ( inqword:qword;                                         //qword to write in the buffer starting from the given address, please note that size of the buffer is not checked
                         var buf:array of byte;
                         addr:dword
                         ):integer;
begin
qword2bytebuf:=INCOMPLETE_FUNCTION;
buf[addr]  :=inqword;
buf[addr+1]:=inqword shr 8;
buf[addr+2]:=inqword shr 16;
buf[addr+3]:=inqword shr 24;
buf[addr+4]:=inqword shr 32;
buf[addr+5]:=inqword shr 40;
buf[addr+6]:=inqword shr 48;
buf[addr+7]:=inqword shr 56;
if qword2bytebuf=INCOMPLETE_FUNCTION then qword2bytebuf:=SUCCESS;
end;

function decode_pea_error ( err:integer;                                        //error code
                            var s:ansistring                                    //string of error description (constants of pea_utils unit)
                            ):integer;
begin
decode_pea_error:=INCOMPLETE_FUNCTION;
case err of
   0: s:='SUCCESS';
   1: s:='INCOMPLETE_FUNCTION';
   2: s:='NOT_PEA_HEADER';
   3: s:='UNKNOWN_COMPRESSION_ALGORITHM';
   4: s:='UNKNOWN_CONTROL_ALGORITHM';
   5: s:='UNKNOWN_OBJ_CONTROL_ALGORITHM';
   6: s:='UNKNOWN_VOLUME_CONTROL_ALGORITHM';
   7: s:='NON_ACCESSIBLE_KEYFILE';
   8: s:='ERROR_IN_SALT_GENERATION';
   9: s:='PEA_REVISION_NOT_SUPPORTED';
  10: s:='INVALID_ENCRYPTION_HEADER';
  11: s:='NOT_SUPPORTED_COMPRESSION_MODE';
  12: s:='SUBHEADER_NOT_MATCH_HEADER';
  13: s:='FILELIST_EMPTY';
  14: s:='FILELIST_NOT_ACCESSIBLE';
   else s:='unexpected error'
   end;
decode_pea_error:=SUCCESS;
end;

procedure update_pea_filename ( fname:ansistring;                               //name to update
                                i:integer;                                      //counter (updated externally)
                                var newname:ansistring);                        //updated name: input name + 6 digit human readable counter + .pea extension
begin
if i <10 then newname:=fname+'.00000'+inttostr(i)+'.pea'
else
   if i<100 then newname:=fname+'.0000'+inttostr(i)+'.pea'
   else
      if i<1000 then newname:=fname+'.000'+inttostr(i)+'.pea'
      else
         if i<10000 then newname:=fname+'.00'+inttostr(i)+'.pea'
         else
            if i<100000 then newname:=fname+'.0'+inttostr(i)+'.pea'
            else newname:=fname+'.'+inttostr(i)+'.pea';
end;

function read_filelist ( listfile_param:ansistring;                             //name of file containing list of input objects
                         var in_param: TFoundlist                               //array of names of input objects
                         ):integer;
var
   f_list:text;
   dummy:ansistring;
   k:dword;
begin
read_filelist:=INCOMPLETE_FUNCTION;
try
   assignfile(f_list,listfile_param);
   filemode:=0;
   reset(f_list);
   if read_header(f_list)=true then
      begin
      k:=0;
      repeat
         readln(f_list,dummy);
         if dummy<>'' then
            begin
            SetLength(in_param,k+1);
            in_param[k]:=dummy;
            k:=k+1;
            end;
      until eof(f_list);
      closefile(f_list);
      if k=0 then
         begin
         read_filelist:=FILELIST_EMPTY;
         exit;
         end;
      end
   else
      begin
      closefile(f_list);
      //read as ansitext
      assignfile(f_list,listfile_param);
      filemode:=0;
      reset(f_list);
      k:=0;
      repeat
         readln(f_list,dummy);
         if dummy<>'' then
            begin
            SetLength(in_param,k+1);
            in_param[k]:=ansitoutf8(dummy);
            k:=k+1;
            end;
      until eof(f_list);
      closefile(f_list);
      if k=0 then
         begin
         read_filelist:=FILELIST_EMPTY;
         exit;
         end;
      end;
except
   read_filelist:=FILELIST_NOT_ACCESSIBLE;
   exit;
end;
read_filelist:=SUCCESS;
end;

function pw4cl(sw:ansistring; var pw:ansistring):integer;
var
   cdelim:utf8string;
begin
pw4cl:=-1;
cdelim:=correctdelimiter(sw+pw);//Windows = ", Linux and others = ' unless ' is found in the string, in this case swap to "
if pos(cdelim, sw+pw)<>0 then exit;
pw:=cdelim+sw+pw+cdelim;
pw4cl:=0;
end;

procedure evaluate_password ( pw: ansistring;                                   //password
                              var pw_strength:dword);                           //entropy bits evaluation
{
Password strength is expressed as raw exteemed entropy entered by the keyboard,
1 points for each entropy bit.
Please note that the unit doesn't perform a quality check on the password using
dictionaries, but try to implement simple checks or composition best practices:
+1 point for each character
+3 for each unique character
max +20 quality bonus for first use of:
+2 lowercase
+2 uppercase
+2 space (multiple words)
+6 number, non strictly subject to dictionary attacks but may be subject to social engineering
+8 non-alphanumerical character excluding space
}
var
   i,j,pw_len,qbonus:integer;
   bonus_upcase,bonus_lowcase,bonus_nonalph,bonus_num,bonus_phrase:boolean;
   s:ansistring;
   c:char;
begin
if pw='' then
   begin
   pw_strength:=0;
   exit;
   end;
pw_len:=length(pw);
bonus_nonalph:=false;
bonus_num:=false;
bonus_upcase:=false;
bonus_lowcase:=false;
bonus_phrase:=false;
for i:=1 to pw_len do
   begin
   if (bonus_nonalph=true) and (bonus_upcase=true) and (bonus_lowcase=true) and (bonus_num=true)  and (bonus_phrase=true) then break;
   if bonus_phrase=false then
      if (ord(pw[i])=32) then
         bonus_phrase:=true;
   if bonus_nonalph=false then
      if (ord(pw[i])<>32) and ((ord(pw[i])<97) or (ord(pw[i])>122)) and ((ord(pw[i])<65) or (ord(pw[i])>90)) and ((ord(pw[i])<48) or (ord(pw[i])>57)) then
         bonus_nonalph:=true;
   if bonus_upcase=false then
      if ((ord(pw[i])>=65) and (ord(pw[i])<=90)) then
         bonus_upcase:=true;
   if bonus_lowcase=false then
      if ((ord(pw[i])>=97) and (ord(pw[i])<=122)) then
         bonus_lowcase:=true;
   if bonus_num=false then
      if ((ord(pw[i])>=48) and (ord(pw[i])<=57)) then
         bonus_num:=true;
   end;
qbonus:=1;
if bonus_nonalph=true then qbonus:=qbonus+8;
if bonus_num=true then qbonus:=qbonus+6;
if bonus_upcase=true then qbonus:=qbonus+2;
if bonus_lowcase=true then qbonus:=qbonus+2;
if bonus_phrase=true then qbonus:=qbonus+2;
s:=pw;
i:=0;
while length(s)>0 do
   begin
   i:=i+1;
   c:=s[1];
   j:=1;
   while j<=length(s) do
      begin
      if c=s[j] then delete(s,j,1)
      else j:=j+1;
      end;
   end;
pw_strength:=pw_len+qbonus+i*3;
if pw_strength>pw_len*7 then pw_strength:=pw_len*7;//rule out extra bonus if result is over realistic max (useful for short strings)
end;

function prepend_keyfile(var pw:ansistring; keyfilename:ansistring):integer;
var
   sbuf:array [1..32767] of byte;
   n:integer;
   k,f_size:qword;
   SHA256Context:THashContext;
   SHA256Digest:TSHA256Digest;
   f:file of byte;
begin
prepend_keyfile:=-1;
try
   filemode:=0;
   assignfile(f,keyfilename);
   reset(f);
   srcfilesize(keyfilename,f_size);
except
   try
      close(f);
   except
   end;
   exit;
end;
SHA256Init(SHA256Context);
k:=0;
repeat
   blockread(f,sbuf,sizeof(sbuf),n);
   if n<>0 then
      begin
      inc(k,n);
      SHA256Update(SHA256Context,@sbuf,n);
      end;
until n<>sizeof(sbuf);
close(f);
SHA256Final(SHA256Context,SHA256Digest);
pw:=base64str(@SHA256Digest,sizeof(SHA256Digest))+pw;
prepend_keyfile:=0;
end;

end.
