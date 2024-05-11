unit Unit_pea;
{
 DESCRIPTION     :  Unit providing PEA, UnPEA, Raw File Split/Join features.
                    Can either be compiled as a standalone GUI application with
                    parameters passed by Command Line or can be used within
                    another application calling *_lib_procedure procedures
                    with appropriate parameters

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060915  G.Tani
 0.11     20060920  G.Tani
 0.12     20060925  G.Tani
 0.12b    20061130  G.Tani
 0.12c    20070122  G.Tani
 0.12d    20070224  G.Tani
 0.13     20070503  G.Tani
 0.14     20070605  G.Tani
 0.15     20070804  G.Tani
 0.16     20071001  G.Tani
 0.17     20071028  G.Tani
 0.17b    20071124  G.Tani
 0.18     20080124  G.Tani
 0.19     20080318  G.Tani
 0.19b    20080511  G.Tani
 0.20     20080730  G.Tani
 0.21     20080922  G.Tani
 0.22     20081030  G.Tani
 0.23     20081118  G.Tani
 0.24     20090116  G.Tani
 0.25     20090215  G.Tani
 0.26     20090324  G.Tani
 0.27     20090709  G.Tani
 0.28     20091016  G.Tani
 0.29     20091028  G.Tani
 0.30     20091109  G.Tani
 0.31     20100613  G.Tani
 0.32     20101016  G.Tani
 0.33     20101122  G.Tani
 0.34     20101224  G.Tani
 0.35     20110226  G.Tani
 0.36     20110611  G.Tani
 0.37     20110726  G.Tani
 0.38     20110913  G.Tani
 0.39     20111005  G.Tani
 0.40     20120607  G.Tani
 0.41     20120805  G.Tani      Real time approximate calculation of possible compression in advanced List (Info) function
                                Application auto closes accordingly to PeaZip policy for operation needing to automatically close (PEA, UNPEA, file split, file join, secure delete)
          20120805  G.Tani      Uniformed Button Panels design over the application
 0.42     20130221  G.Tani      New theming engine
                                New high resolution application icon
          20130322  G.Tani      Recompiled with Lazarus 1.0.8
 0.43     20130408  G.Tani      Fixed single volume size issue for Pea format on Win64
 0.44     20130617  G.Tani      Code cleanup
          20130718  G.Tani      Recompiled with Lazarus 1.0.10
 0.45     20130928  G.Tani      Secure delete changes system files attribute to allow operation
                                Recompiled with Lazarus 1.0.12
 0.46     20131122  G.Tani      Secure deletion: added VERY_FAST mode (single pass, random pattern) and ZERO (single pass overwriting data with zero)
                                Adds Sanitize function (free space deletion) with ZERO mode and VERY_FAST to VERY_SLOW secure deletion modes
 0.47     20131222  G.Tani      Improved secure file delete and secure free space delete
                                 All modes with 4 or more iterations now uses overwrite with all 0 and overwrite with al 1 (FF byte) for the two first iterations, fasetr and more secure due to most recommendations for secure deletion protocols, as USAF System Security Instruction 5020, Schneier's Algorithm, Communications Security Establishment Canada ITSG-06, British HMG Infosec Standard 5 Enhanced Standard
                                Various minor improvements, messagedlg used for all error/warning messages
 0.48     20140222  G.Tani      Standalone "PeaUtils" GUI for Pea utilities
                                 the GUI is displayed when pea executable is started with no parameter
                                 the GUI can be started pointing to a specific function (from script, command or link) with "peautils" "n-th function" (0 to 11, same order as in the function dropdown menu) parameters, i.e. pea peautils 0 for CRC32; further parameters are ignored as it is mean for interactive use
          20140309  G.Tani      Visual updates, recompiled for Lazarus 1.2.0
 0.49     20140706  G.Tani      Quick delete and Send to recycle bin (Windows) modes added to secure deletion routine
 0.50     20150718  G.Tani      Recompiled with Lazarus 1.4.0
                                Updated libraries: crc_hash_2014-08-25, util_2015-05-04
 0.51     20150729  G.Tani      Aligned span pre-sets sizes with PeaZip values
 0.52     20151121  G.Tani      Improved reporting for file management tools
                                 can sort report by column
                                  helps identifying similar elements, as files with same size, date, checksum/hash, or directories containing same number of files and subdirs / total size
                                 can export to csv file
                                Improved file hashing tool
                                 can now take directory as input to check contained files
                                 operation can be cancelled while running
                                 progress calculation is based on total input size
                                 show 32 character samples of file header and end of file regions (not exported in report as potentially unsafe)
                                 show information about each directory content: dirs, files, total size
                                  can be used to find possibly identical directories (same number of files and subdirs / total size)
                                 show each item (file or folder) % size of total input
                                 produce more stats about total content: larger/smaller file, newer/older file, total potential compression extimate
                                 new preview mode providing only meta information and file samples
                                 new list mode providing only meta information without checksum/hash nor file sample (replaces still available older info/list listfiles function)
                                Improved secure delete
                                 operation can be cancelled while running (already deleted files will not be recovered)
                                 progress calculation based on total input size
                                 new 'header' mode, quick deletion overwriting with random data only file header up to 64 KB
                                Improved secure free space deletion
                                 operation can be cancelled while running
                                 fixed: can now delete free space for system drive
 0.53     20160111  G.Tani      Recompiled for Lazarus 1.6.0 / FPC3
                                 file management functions now full support Unicode file/dir names on Windows
                                 PEA format can now handle Unicode file/dir names on Windows systems
                                Can now display the result report as table or clipboard (toggle using titles line)
                                New Ten theme
                                Various fixes and improvements
 0.54     20160427  G.Tani      Pea file format revision 1.1
                                 introduced support for Twofish and Serpent encryption, EAX mode, 128 and 256 bit (stream -level algorithm)
                                 introduced support for SHA-3 256 and 512 hash (object, volume, and stream -level algorithm)
                                File tools, improved hashing utility
                                 introduced support for SHA-3 256 and 512 hash
                                 added digest of each selected crc/hash (same crc/hash function on crc/hash values) if more than 1 file is analyzed, to allow quick result comparison
                                 various fixes
 0.55     20160618  G.Tani      Fixed errors checking old PEA 1.0 file format version / revision
 0.56     20160909  G.Tani      Various improvements for using the executable as standalone application, to be deployed as PeaUtils 1.0 spin-off package
                                 When used as standalone utility shows hamburger button with popup menu for Run as administaror, online help, updates, and donations
                                 Added CRC64 and hex preview options in standalone operations dropdown menu
 0.57     20160919  G.Tani      Various improvements before release of PeaUtils 1.0 spin-off package
                                 Added Byte to byte compare function
                                 Added Split and Join functions
                                 Replaced List with Analyze files and folders (provides more information)
                                 Reorganized functions dropdown menu
                                 Created Windows installer with most common functions available for context menu integration
 0.58     20161022  G.Tani      Visual updates
 0.59     20161204  G.Tani      Improved DPI awareness, improved PeaUtils layout
 0.60     20170211  G.Tani      Fixes to frontend for PeaUtils 1.1 spin-off package
 0.61     20170321  G.Tani      Updates for PeaUtils 1.2 spin-off package
                                 Checksum/hash now reports duplicate items (uses best selected algorithm, count identical items)
                                 Secure delete now waits the process to exit and updates the input list removing items successfully removed
 0.62     20170423  G.Tani      Improved how 0 byte files are handled in some cases
                                Improved how version is reported in application's title bar
 0.63     20170804  G.Tani      Minor visual update
 0.64     20180209  G.Tani      Recompiled with Lazarus 1.8.0 with updated WE libraries
 0.65     20181203  G.Tani      Updated to Wolfgang Ehrhardt math library util_2018-11-27
 0.66     20191009  G.Tani      Recompiled with LCL scaling and autoscaling graphics
 0.67     20191222  G.Tani      WIPE: fixed reporting number of deleted item with RECYCLE option
 0.68     20200125  G.Tani      Fixed: pea/unpea now allows using keyfiles only as in PeaZip
 0.69     20200406  G.Tani      Minor updates
 0.70     20200423  G.Tani      Added function to save all or each single crc or hash value to file, from context menu of report window
                                Checksum and hash values are now reported also for empty files, as defined by the standard of each function
                                Recompiled with Lazarus 2.0.8
 0.71     20200508  G.Tani      New PEA format revision 1.2
                                 introduced support for BLAKE2S 256 bit and BLAKE2B 512 bit
 0.72     20200514  G.Tani      Improved theming
 0.73     20200805  G.Tani      Visual updates
                                Added button to change case on the fly for checksum/hash (hex and lsbhex)
 0.74     20200905  G.Tani      New PEA format revision 1.3
                                 Introduced support for multiple encryption, cascading encryption with AES, Twofish, and Sepent, 256 bit in EAX mode
                                  Each cipher is separately keyed through PBKDF2, following steps are taken to ensure the 3 keys are statistically independent after key schedule:
                                   key schedule of each cipher is based on a different hash primitive which is run for a different number of iterations
                                    Whirlpool x 25000 for AES, SHA512 x 50000 for Twofish, SHA3-512 x 75000 for Serpent (Whirlpool is significantly slower than SHA512 that is slower than SHA3-512)
                                   key schedule of each cipher is provided a separate 96 byte pseudorandom salt
                                   password is modified when provided as input for key schedule of each cipher
                                    modification are trivial xor with non secret values and counters, with the sole purpose to initialize the key derivation with different values and be a further factor (alongside different salt, and different hash / iteration number) to guarantee keys are a statistically independent
                                  Password verification tag is the xor of the 3 password verification tags of each encryption function, and is written / verified after all 3 key initialization functions are completed before verification
                                  Each block between password verification tag and stream authentication tag is encrypted with all 3 ciphers
                                  A 1..128 bytes block of random data is added after password verification tag in order to mask exact archive size
                                  Each cipher generate its own 128 bit sized stream authentication tag, tags are concatenated and hashed with SHA3-384; the SHA3-384 value is checked for verification, this requires all the 3 tags to match to expected values and does not allow ciphers to be authenticated separately
 0.75     20201206  G.Tani      Recompiled with updated theming
 0.76     20210121  G.Tani      Improved quoting on Unix-like sistems, fixes
 0.77     20210302  G.Tani      Various fixes
 1.00     20210415  G.Tani      Added 512 bit hash functions to file utilities menu
                                Updated theming to allow custom zooming and spacing accordingly to peazip binary
 1.01     20210522  G.Tani      Updated theming consistently with PeaZip 8.0 (allow optional alternate grid colors for readability)
                                Added exit codes for main functions
                                 1 abnormal termination
                                 0 success
                                 -1 incomplete
                                 -2 completed with errors
                                  pea: some files cannot be archived (not found, not readable)
                                  unpea: errors detected in the archive
                                  wipe: some files cannot be deleted (locked, not found)
                                 -3 internal error
                                 -4 cancelled by user
                                Batch and hidden *_report modes now save report to output path without needing user interaction
                                Improved hiding the GUI in HIDDEN mode
                                Improved byte to byte file comparison function
 1.02     20210711  G.Tani      Over 2x improved speed of hex preview, now enabled for files up to 64 MB in size
                                Updated files and free space secure delete functions, new ONE parameter to overwrite all bits with 1
 1.03     20210919  G.Tani      Merged patches for Darwin
 1.04     20211102  G.Tani      Pea binary moved to root folder of PeaZip package, modified to support new internal directory structure and new theming
                                Fixed theming issues
 1.05     20211212  G.Tani      Added BENCH function: single and multi core integer and floating generic arithmetic performances becnchmark
                                Optimized performances of PEA, UNPEA, and checksum / hash routines
 1.06     20220120  G.Tani      Recompiled with Lazarus 2.2.0
                                RECYCLE option of WIPE function ported to macOS, move item(s) to Trash
 1.07     20220402  G.Tani      New UI layout and theme
 1.08     20220620  G.Tani      Updated theme, can now use custom CSV separator, fixes
 1.09     20220808  G.Tani      Updated theming engine
 1.10     20221003  G.Tani      Updated theming engine
 1.11     20221208  G.Tani      Minor fixes
 1.12     20230218  G.Tani      Fixes, added conditional compilation sections for non-Windows systems
 1.13     20230620  G.Tani      Fixes, updated theming engine to support Contrast setting
 1.14     20230807  G.Tani      Added Blue-Ray pre-sets for file split
 1.15     20231020  G.Tani      Added context menu item to chacksum/hash screen to save selected CRC or hash value for all listed files, compatible with Coreutils sha256sum and similar utilities
 1.16     20231209  G.Tani      Fixes, updated theming
 1.17     20240202  G.Tani      Updated theming, added W10+ dark mode, compiled for Lazarus 3.0
                                Improved error detection in PEA archive header data
 1.18     20240422  G.Tani      By default hexadecimal checksum and hash values are written as lowercase
                                Save hash and filename function (context menu in report screen, rightclicking on one of the performed crc or hash functioons) now generates a file fully compatible with sha256sum and similar utilities
                                Compiled for Lazarus 3.2
                                (Windows 10+) Can now be manually forced to light or dark mode regardless system colors, accordingly to peazip app

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
{$IFDEF DARWIN}
{$modeswitch ObjectiveC1}
{$linkframework CoreFoundation}
{$ENDIF}
{$INLINE ON}{$UNITPATH ./we}

interface

uses
{$IFDEF MSWINDOWS}
Windows, activex, ShlObj,
{$ENDIF}
{$IFDEF DARWIN}MacOSAll, CocoaAll,{$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Process, UTF8Process, Spin,
  Buttons, ComCtrls, StdCtrls, Menus, strutils, zuncompr, zcompres,
  hash, adler32, CRC16, CRC24, CRC32, CRC64, ED2K, MD4, MD5, RMD160, SHA1, SHA224,
  SHA256, SHA3_256, SHA384, SHA3_384, SHA512, SHA3_512, Whirl512, Blake2s, Blake2b,
  aes_ctr, tf_ctr, sp_ctr, tf_base, sp_base, AES_Type, AES_EAX, fcrypta, FCAES256,
  tf_eax, fcryptt, fctf256,
  sp_eax, fcrypts, fcsp256,
  mem_util, list_utils, img_utils, pea_utils, rfs_utils, ansiutf8_utils, unit_report, types;

type

  { TForm_pea }

  TForm_pea = class(TForm)
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel9: TBevel;
    ButtonDone1: TBitBtn;
    ButtonPeaExit1: TBitBtn;
    ButtonPW1: TBitBtn;
    ButtonPW2: TBitBtn;
    ButtonPeaExit: TBitBtn;
    ButtonRefSize: TButton;
    ButtonUtilsCancel: TBitBtn;
    ButtonToolsCancel: TBitBtn;
    ButtonUtilsOK: TBitBtn;
    ButtonRFSinteractive: TBitBtn;
    ButtonRFSinteractive1: TBitBtn;
    ButtonUtilsReset: TSpeedButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBoxUnits: TComboBox;
    ComboBoxUtils: TComboBox;
    EditConfirm1: TEdit;
    EditPW1: TEdit;
    ImageUtils: TImage;
    Image7: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    ImageList1: TImageList;
    ImageSplit: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LabelConfirm1: TLabel;
    LabelDecrypt2: TLabel;
    LabelDecrypt3: TLabel;
    LabelDecrypt4: TLabel;
    LabelDecrypt5: TLabel;
    LabelDecrypt6: TLabel;
    LabelE1: TLabel;
    LabelEncrypt2: TLabel;
    LabelEncrypt3: TLabel;
    LabelEncrypt4: TLabel;
    LabelEncrypt5: TLabel;
    LabelEncrypt6: TLabel;
    LabelHint1: TLabel;
    LabelKeyFile1: TLabel;
    LabelLog1: TBitBtn;
    LabelOpen: TBitBtn;
    labelopenfile0: TLabel;
    labelopenfile2: TLabel;
    labelopenfile3: TLabel;
    LabelOut1: TLabel;
    LabelPS1: TLabel;
    LabelPW1: TLabel;
    LabelKeyFileName1: TLabel;
    LabelTools5: TLabel;
    LabelUtilsFun: TLabel;
    LabelSample1: TLabel;
    LabelSample2: TLabel;
    LabelTime1: TLabel;
    LabelTools3: TLabel;
    LabelTools4: TLabel;
    LabelTools2: TLabel;
    LabelUtilsInput: TLabel;
    ListMemo: TMemo;
    MainMenu1: TMainMenu;
    mainmenuhelp: TMenuItem;
    MenuItem1: TMenuItem;
    PanelUtilsTitle: TPanel;
    Panelsp1: TPanel;
    Panelsp0: TPanel;
    Panelsp2: TPanel;
    peautilsbtn: TSpeedButton;
    pmupdates: TMenuItem;
    pmdonations: TMenuItem;
    pmhelp: TMenuItem;
    pmrunasadmin: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PanelDecrypt1: TPanel;
    PanelEncrypt1: TPanel;
    Panel1: TPanel;
    PanelPW1: TPanel;
    PanelUtils: TPanel;
    PanelRFSinteractive: TPanel;
    PanelTools: TPanel;
    peautilsmenu: TPopupMenu;
    ProgressBar1: TProgressBar;
    Shape2: TShape;
    ShapeE1: TShape;
    ShapeE2: TShape;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonDone1Click(Sender: TObject);
    procedure ButtonPeaExitClick(Sender: TObject);
    procedure ButtonPW1Click(Sender: TObject);
    procedure ButtonPW2Click(Sender: TObject);
    procedure ButtonRFSinteractive1Click(Sender: TObject);
    procedure ButtonRFSinteractiveClick(Sender: TObject);
    procedure ButtonToolsCancelClick(Sender: TObject);
    procedure ButtonUtilsCancelClick(Sender: TObject);
    procedure ButtonUtilsResetClick(Sender: TObject);
    procedure ButtonUtilsOKClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBoxUtilsChange(Sender: TObject);
    procedure EditPW1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure ImageUtilsClick(Sender: TObject);
    procedure LabelE1Click(Sender: TObject);
    procedure LabelKeyFile1Click(Sender: TObject);
    procedure LabelLog1Click(Sender: TObject);
    procedure LabelOpenClick(Sender: TObject);
    procedure labelopenfile0Click(Sender: TObject);
    procedure labelopenfile2Click(Sender: TObject);
    procedure mainmenuhelpClick(Sender: TObject);
    procedure pmupdatesClick(Sender: TObject);
    procedure pmdonationsClick(Sender: TObject);
    procedure PanelPW1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure peautilsbtnClick(Sender: TObject);
    procedure pmhelpClick(Sender: TObject);
    procedure pmrunasadminClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  Type fileofbyte = file of byte;

const
  P_RELEASE          = '1.18'; //declares release version for the whole build
  PEAUTILS_RELEASE   = '1.3'; //declares for reference last peautils release
  PEA_FILEFORMAT_VER = 1;
  PEA_FILEFORMAT_REV = 3; //version and revision declared to be implemented must match with the ones in pea_utils, otherwise a warning will be raised (form caption)
  SBUFSIZE           = 65535;//32768;
  {32KB of size for reading small buffers, used for ciphers and hashes}
  WBUFSIZE           = 1048576;
  {1MB of size for reading whide buffers, used for compression.
  Decompression may read arbitrarily sized buffers up to array size used for
  wide buffers -64KB (left for possible data expansion)}
  {$IFDEF MSWINDOWS}
  DEFAULT_THEME = 'main-embedded';
  EXEEXT        = '.exe';
  {$ELSE}
  DEFAULT_THEME = 'main-embedded';
  EXEEXT        = '';
  {$ENDIF}
  BASEBENCH     = 524288;
  WS_EX_LAYERED = $80000;
  LWA_ALPHA     = $2;
  FIRSTDOM      = 'https://peazip.github.io/';
  SECONDDOM     = 'https://peazip.sourceforge.io/';

var
  Form_pea: TForm_pea;
   wbuf1,wbuf2:array[0..1114111] of byte; //>1MB wide buffers (1MB+ 64KB)
   fun,pw,keyfile_name,output,vol_algo,graphicsfolder,caption_build,delimiter,confpath:ansistring;
   vol_size:qword;
   desk_env:byte;
   interacting,control,details,height_set,toolactioncancelled:boolean;
   ment,kent,fent,ment_sample: THashContext;
   mentd: TWhirlDigest;
   mentd_sample: TSHA256Digest;
   fingerprint: TSHA512Digest;
   in_param,in_files,exp_files,status_objects,status_volumes,exp_fattr_dec,fattr_dec:TFoundList;
   status_files:TFoundListBool;
   fsizes,exp_fsizes:TFoundListSizes;
   ftimes,exp_ftimes:TFoundListAges;
   fattr,exp_fattr:TFoundListAttrib;
   obj_tags,exp_obj_tags,volume_tags,exp_volume_tags:TFoundListArray64;
   Bfd,Bmail,Bhd,Bdvd,Binfo,Blog,Bok,Bcancel,Butils,Badmin:TBitmap;
   fshown:boolean;
   //theming
   conf:text;
   opacity,closepolicy,qscale,qscaleimages,pspacing,pzooming,alttabstyle,ensmall,gridaltcolor,highlighttabs,temperature,contrast:integer;
   executable_path,resource_path,binpath,sharepath,persistent_source,color1,color2,color3,color4,color5:string;
   csvsep:ansistring;

{
PEA features can be called using different modes of operation:
INTERACTIVE the form is visible, user's input is requested if needed (can be used only calling PEA from command line, it's not allowed in *_lib_procedure procedures)
BATCH       the form is visible, user's input not requested: if passphrase/keyfile are needed are got from next two parameters of command line
HIDDEN      the form is not visible, user input not requested (as for BATCH)
*_REPORT    can be applied to each mode, the program operates as described for the mode used and then an automated job report is saved at the end of the operation
mode of operation is declared as opmode in *_lib_procedure, then passed to *_procedure as pw_param
INTERACTIVE* modes can be used only for PEA and UnPEA (since only those features may require keying), other modes can be used also for RFS and RFJ
}

//procedure to call pea within another application
procedure pea_lib_procedure ( out_param: ansistring;                            //archive qualified name (without .(volume number).PEA suffix) or AUTONAME
                              ch_size: qword;                                   //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              compr: ansistring;                                //compression scheme to use
                              volume_algo:ansistring;                           //algorithm for volume integrity check
                              obj_algo: ansistring;                             //algorithm for object integrity check
                              algo:ansistring;                                  //algorithm for stream integrity check
                              password,keyf_name:ansistring;                    //password and keyfile qualified name (if needed by stream algo)
                              in_param:TFoundList;                              //array of ansistring containing input qualified names
                              opmode:ansistring);                               //mode of operation

procedure pea_procedure ( out_param: ansistring;
                          ch_size: qword;
                          compr: ansistring;
                          compr_level: byte;
                          volume_algo:ansistring;
                          volume_authsize:byte;
                          obj_algo: ansistring;
                          obj_authsize: byte;
                          algo:ansistring;
                          headersize,authsize: byte;
                          pwneeded: boolean;
                          pw_param,password,keyf_name:ansistring;
                          in_param:TFoundList);

//procedure to call unpea within another application
procedure unpea_lib_procedure ( in_qualified_name,                              //archive qualified name
                                out_param,                                      //dir were extracting the archive (or AUTONAME)
                                date_param,                                     //actually only supported RESETDATE, reset date of extracted files
                                attr_param,                                     //RESETATTR (or SETATTR only on Windows to set object's attributes as on original objects)
                                struct_param,                                   //actually only supported EXTRACT2DIR, create a dir and extract archive in the dir using shortest paths for archived objects
                                password,keyf_name:ansistring;                  //password and keyfile qualified name (if needed)
                                opmode:ansistring);                             //mode of operation

procedure unpea_procedure ( in_qualified_name,
                            out_param,
                            date_param,
                            attr_param,
                            struct_param,
                            pw_param,
                            password,
                            keyf_name:ansistring);

//procedure to call raw file split within another application
procedure rfs_lib_procedure ( out_param:ansistring;                             //qualified name for output volumes (without .(volume number) suffix) or AUTONAME
                              ch_size:qword;                                    //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              volume_algo,                                      //algorithm for volume integrity check
                              in_qualified_name:ansistring;                     //qualified name of input file
                              opmode:ansistring);                               //mode of operation

procedure rfs_procedure ( out_param:ansistring;
                          ch_size:qword;
                          volume_algo:ansistring;
                          volume_authsize:byte;
                          pw_param:ansistring;
                          in_qualified_name:ansistring);

//procedure to call raw file join within another application
procedure rfj_lib_procedure ( in_qualified_name,                                //qualified name of first volume of the split file
                              out_param,                                        //qualified name to give to the output rejoined file (or AUTONAME)
                              opmode:ansistring);                               //mode of operation

procedure rfj_procedure ( in_qualified_name,
                          pw_param,
                          out_param:ansistring);
                          
implementation

{
misc procedures
}

//timing
procedure timing(tsin:TTimeStamp; size:qword);
var tsout:TTimeStamp;
time,speed:qword;
begin
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time<=0 then time:=100000;
speed:=(size * 1000) div time;
Form_pea.LabelTime1.Caption:='Processed '+nicenumber(inttostr(size),0)+' in '+nicetime(inttostr(time))+' @ '+nicenumber(inttostr(speed),0)+'/s';
Form_pea.ButtonDone1.Visible:=true;
end;

//if an error is encountered calling a PEA_utils procedure, show error description then halt, otherwise (error code is 0) continue
procedure test_pea_error ( s:ansistring;
                           err:integer);
var
   decoded_err:ansistring;
begin
if err<>0 then
   begin
   decode_pea_error(err,decoded_err);
   MessageDlg('Error '+s+': '+inttostr(err)+' '+decoded_err, mtError, [mbOK], 0);
   halt(-3);
   end;
end;

//when an internal error is encountered, show error description then halt
procedure internal_error (s:ansistring);
begin
MessageDlg(s, mtError, [mbOK], 0);
halt(-3);
end;

procedure clean_global_vars;
begin
SetLength(in_param,0);
SetLength(in_files,0);
SetLength(exp_files,0);
SetLength(status_objects,0);
SetLength(status_volumes,0);
SetLength(exp_fattr_dec,0);
SetLength(fattr_dec,0);
SetLength(status_files,0);
SetLength(fsizes,0);
SetLength(exp_fsizes,0);
SetLength(ftimes,0);
SetLength(exp_ftimes,0);
SetLength(fattr,0);
SetLength(exp_fattr,0);
SetLength(obj_tags,0);
SetLength(exp_obj_tags,0);
SetLength(volume_tags,0);
SetLength(exp_volume_tags,0);
output:='';
vol_size:=0;
vol_algo:='';
end;

procedure checkspace(outpath:ansistring; chsize:qword);
var size_ok:boolean;
begin
size_ok:=false;
repeat
   if ((chsize>diskfree(0)) and (chsize<>1024*1024*1024*1024*1024)) then
      if MessageDlg('Output path '+outpath+' seems to not have enough free space for an output volume, try to free some space on it or exchange it with an empty one if it''s a removable media. Do you want to test the path another time?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3)
   else size_ok:=true;
until size_ok=true;
end;

procedure checkspacepea(outpath:ansistring; chsize,volume_authsize:qword);
var size_ok:boolean;
begin
size_ok:=false;
repeat
   if ((chsize>diskfree(0)) and (chsize<>1024*1024*1024*1024*1024-volume_authsize)) then
      if MessageDlg('Output path '+outpath+' seems to not have enough free space for an output volume, try to free some space on it or exchange it with an empty one if it''s a removable media. Do you want to test the path another time?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3)
   else size_ok:=true;
until size_ok=true;
end;

procedure check_chunk ( in_folder:ansistring;
                        j:dword;
                        var chunks_ok:boolean);
begin
chunks_ok:=false;
if MessageDlg('The path "'+in_folder+'" seem not containing volume '+inttostr(j)+' (i.e. volumes are on multiple removable media and you have to change the media). Check again?',mtWarning,[mbYes, mbNo],0)=6 then
else internal_error('Impossible to read requested volume(s). Not found volume '+inttostr(j));
end;

procedure read_from_chunks ( in_folder,in_name:ansistring;                      //path and base name of input file; actual PEA filename get updated by update_pea_filename procedure
                             byte_to_read:dword;                                //size to be read from chunks
                             var buf: array of byte;                            //buffer with output data
                             var tmp_buf: array of byte;                        //buffer used to temporarily store the data to compose in the output buffer
                             volume_tag_size:byte;                              //size of volume tag, data to be skipped at the end of each volume;
                             maxsize:dword;                                     //max size to read at once
                             singlevolume:boolean);
var
   i,j,k,ind,numread:dword;
   total:qword;
   chunks_ok:boolean;
   in_file:ansistring;
   f_in:file of byte;
begin
try
j:=1;
ind:=0;
chunks_ok:=true;
in_file:=in_name;
while ((chunks_ok=true) and (ind<byte_to_read)) do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file);
   repeat
      if fileexists(in_folder+in_file) then
         begin
         chunks_ok:=true;
         assignfile(f_in,in_folder+in_file);
         filemode:=0;
         {$I-}reset(f_in);{$I+}
         if IOResult<>0 then internal_error('IO error opening '+in_folder+in_file);
         srcfilesize(in_folder+in_file,total);
         total:=total-volume_tag_size;
         if total<byte_to_read then internal_error('Impossible to read requested data from '+in_file+'. The archive may be invalid or corrupted');
         //total:=system.filesize(f_in)-volume_tag_size;
         while ((total>0) and (ind<byte_to_read)) do
            begin
            if total>maxsize then i:=maxsize else i:=total;
            //try
            blockread (f_in,tmp_buf,i,numread);
            //if singlevolume=true then if numread<byte_to_read then check_chunk(in_folder,j,chunks_ok);
            //except
            //internal_error('IO error reading from '+in_folder+in_file);
            //end;
            dec(total,numread);
            for k:=0 to numread-1 do buf[ind+k]:=tmp_buf[k];
            inc(ind,numread);
            end;
         {$I-}close(f_in);{$I+}
         if IOResult<>0 then internal_error('IO error closing '+in_folder+in_file);
         j:=j+1;
         end
      else check_chunk(in_folder,j,chunks_ok);
   until chunks_ok=true;
   end;
except
internal_error('IO error reading from '+in_folder+in_file);
end;
end;

procedure gen_rand(var arr: array of byte);
var
   ment1,kent1,fent1: THashContext;
begin
ment1:=ment;
kent1:=kent;
fent1:=fent;
generate_keyf (arr,persistent_source,fingerprint,ment1,kent1,fent1);
end;

procedure shl_rand(var arr: array of byte);
var
   randf: file of byte;
   randarr: TKey2048;
   i,j: integer;
begin
try
//read current rand seed file
assignfile(randf,persistent_source);
filemode:=0;
reset(randf);
blockread(randf,randarr,256,j);
closefile(randf);
//left shift by one byte the array of the rand seed
for i:=0 to 254 do arr[i]:=randarr[i+1];
arr[255]:=randarr[0];
except
end;
end;

{
PEA: Pack (archive, compress and split) Encrypt and Authenticate
The program accept n objects (files, dirs) as input, merge them into a single
archive and give m output chunks of desired size.
Number of objects to be archived is actually only memory limited, not format
limited (PEA format allow unlimited input objects); each object can be up to 2^64
byte in size.
PEA file format version 1 revision 0 can create a single stream, optionally
encrypted and authenticated, containing all objects to be archived, keyed by
passphrase and optionally keyfile (two factor authentication).
Metadata associated to archived objects are: qualified name, last modification
time, attributes; if more advanced archiving/restoring/backup features are
needed it's recommended using asynchronously tar or similar programs more focused
on that needs before sending the resulting file to PEA.

Notes:
- W.Ehrhardt's hash and crypto libraries are used for hashes, checksums, ciphers
  and key scheduling (PBKDF2);
- Lazarus paszlib compression libraries were used to build a custom compression
  scheme (PCOMPESS*);
}

procedure PEA;
var
   out_param,compr,volume_algo,obj_algo,algo,pw_param,password,keyf_name,list_param,listfile_param:ansistring;
   ch_size:qword;
   compr_level,volume_authsize,obj_authsize,headersize,authsize:byte;
   pwneeded:boolean;


procedure parse_pea_cl; //exit at first error with descriptive message, including parameters passed if relevant
var i,k:dword;
begin
i:=0;
try
   out_param:=(paramstr(2));
   //// control volume size
   try
      ch_size:=strtoqword(paramstr(3));
      if ch_size=0 then ch_size:=1024*1024*1024*1024*1024;//high(ch_size); set to 1024 TB// if chunk size is set to 0 no chunks will be done
   except
      internal_error('"'+paramstr(3)+'" is not a valid chunk size; values allowed are 1..2^64, 0 to don''t split the input');
   end;
   //get compression algorithm
   compr:=upcase(paramstr(4));
   if decode_compression_algo(compr,compr_level)<>0 then
      internal_error('"'+compr+'" is not a valid compression algorithm, please refer to the documentation for supported ones');
   //get volume control algorithm
   volume_algo:=upcase(paramstr(5));
   if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
      internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
   if ch_size<volume_authsize+10 then ch_size:=volume_authsize+10;//chunk size is set at least 10 byte over volume size, in order to have at least 10 byte of data in the first volume to allow to read archive header at once (needed to know volume authsize in UnPEA)
   ch_size:=ch_size-volume_authsize;
   //get object control algorithm
   obj_algo:=upcase(paramstr(6));
   if decode_obj_control_algo(obj_algo,obj_authsize)<>0 then
      internal_error('"'+obj_algo+'" is not a valid control algorithm for object check, please refer to the documentation for supported ones');
   //get control algorithm
   algo:=upcase(paramstr(7));
   if decode_control_algo(algo,headersize,authsize,pwneeded)<>0 then
      internal_error('"'+algo+'" is not a valid control algorithm, please refer to the documentation for supported ones');
   //get operation mode
   inc(i,1);
   pw_param:=upcase(paramstr(7+i));
   if pwneeded=true then
      begin
      if (pw_param<>'INTERACTIVE') and (pw_param<>'INTERACTIVE_REPORT') then
         begin
         inc(i,1);
         password:=(paramstr(7+i));
         inc(i,1);
         keyf_name:=(paramstr(7+i));
         end
      else
         if (pw_param<>'INTERACTIVE') and (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'INTERACTIVE_REPORT') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
            internal_error('"'+pw_param+'" is not a valid operation mode parameter, please refer to the documentation');
      end;
   //get input list (it will be expanded in pea_procedure)
   list_param:=upcase(paramstr(8+i));
   if paramstr(8+i)<>'' then
      if list_param='FROMCL' then //get input files by CL
         begin
         for k:=0 to paramcount-9-i do
            begin
            SetLength(in_param,k+1);
            in_param[k]:=(paramstr(k+9+i));
            end;
         end
      else
         if list_param='FROMFILE' then //get input files from a list file (an ansi text file containing a list of object names, each object in a line)
            begin
            listfile_param:=(paramstr(9+i));
            case read_filelist(listfile_param,in_param) of
              13: internal_error('The list file '+listfile_param+' is empty');
              14: internal_error('Cannot access the specified list file '+listfile_param);
               end;
            end
         else internal_error('Input method '+list_param+' not allowed')
   else internal_error('No accessible input object found');
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_pea_cl;
pea_procedure(out_param,ch_size,compr,compr_level,volume_algo,volume_authsize,obj_algo,obj_authsize,algo,headersize,authsize,pwneeded,pw_param,password,keyf_name,in_param);
end;

procedure pea_lib_procedure ( out_param: ansistring;                            //archive qualified name (without .(volume number).PEA suffix) or AUTONAME
                              ch_size: qword;                                   //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              compr: ansistring;                                //compression scheme to use
                              volume_algo:ansistring;                           //algorithm for volume integrity check
                              obj_algo: ansistring;                             //algorithm for object integrity check
                              algo:ansistring;                                  //algorithm for stream integrity check
                              password,keyf_name:ansistring;                    //password and keyfile qualified name (if needed by stream algo)
                              in_param:TFoundList;                              //array of ansistring containing input qualified names
                              opmode:ansistring);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:ansistring;
   compr_level,volume_authsize,obj_authsize,headersize,authsize:byte;
   pwneeded:boolean;
begin
//// control volume size
if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
//get compression algorithm
if decode_compression_algo(compr,compr_level)<>0 then
   internal_error('"'+compr+'" is not a valid compression algorithm, please refer to the documentation for supported ones');
//get volume control algorithm
if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
   internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
if ch_size<volume_authsize+1 then ch_size:=volume_authsize+1;
ch_size:=ch_size-volume_authsize;
//get object control algorithm
if decode_obj_control_algo(obj_algo,obj_authsize)<>0 then
   internal_error('"'+obj_algo+'" is not a valid control algorithm for object check, please refer to the documentation for supported ones');
//get control algorithm
if decode_control_algo(algo,headersize,authsize,pwneeded)<>0 then
   internal_error('"'+algo+'" is not a valid control algorithm, please refer to the documentation for supported ones');
//input list (will be expanded in pea_procedure) is jet loaded in in_param, TFoundList (array of ansistring)
//get operation mode
if (upcase(opmode)<>'INTERACTIVE') and (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'INTERACTIVE_REPORT') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter, please refer to the documentation');
if (upcase(opmode)='INTERACTIVE') or (upcase(opmode)='INTERACTIVE_REPORT') then
   internal_error('INTERACTIVE* modes are not allowed calling pea_lib_procedure, use BATCH* or HIDDEN* modes');
pw_param:=upcase(opmode);
pea_procedure(out_param,ch_size,compr,compr_level,volume_algo,volume_authsize,obj_algo,obj_authsize,algo,headersize,authsize,pwneeded,pw_param,password,keyf_name,in_param);
end;

procedure pea_procedure ( out_param: ansistring;
                          ch_size: qword;
                          compr: ansistring;
                          compr_level: byte;
                          volume_algo:ansistring;
                          volume_authsize:byte;
                          obj_algo: ansistring;
                          obj_authsize: byte;
                          algo:ansistring;
                          headersize,authsize: byte;
                          pwneeded: boolean;
                          pw_param,password,keyf_name:ansistring;
                          in_param:TFoundList);
var
   hdr : TFCAHdr;
   fhdr : TFCFHdr;
   shdr : TFCSHdr;
   hdr256 : TFCA256Hdr;
   fhdr256 : TFCF256Hdr;
   shdr256 : TFCS256Hdr;
   cxe : TAES_EAXContext;
   cxf : Ttf_EAXContext;
   cxs : Tsp_EAXContext;
   cxh : TFCA_HMAC_Context;
   randarr: TKey2048;
   auth,auth2,auth3 : array [0..15] of byte; //valid type conversion for TFCA_AuthBlock and TFCA256_AuthBlock
   Blake2sContext,Blake2sContext_obj,Blake2sContext_volume:blake2s_ctx;
   Blake2sDigest,Blake2sDigest_obj,Blake2sDigest_volume:TBlake2sDigest;
   Blake2bDigest,Blake2bDigest_obj,Blake2bDigest_volume:TBlake2bDigest;
   HashContext,HashContext_obj,HashContext_volume: THashContext;
   Whirl512Digest,Whirl512Digest_obj,Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest,SHA512Digest_obj,SHA512Digest_volume: TSHA512Digest;
   SHA256Digest,SHA256Digest_obj,SHA256Digest_volume: TSHA256Digest;
   SHA3_512Digest,SHA3_512Digest_obj,SHA3_512Digest_volume: TSHA3_512Digest;
   SHA3_256Digest,SHA3_256Digest_obj,SHA3_256Digest_volume: TSHA3_256Digest;
   SHA1Digest,SHA1Digest_obj,SHA1Digest_volume: TSHA1Digest;
   RMD160Digest,RMD160Digest_obj,RMD160Digest_volume: TRMD160Digest;
   MD5Digest,MD5Digest_obj,MD5Digest_volume: TMD5Digest;
   crc64,crc64_obj,crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   r: TSearchRec;
   f_in,f_out:file of byte;
   sbuf1,sbuf2:array [0..65534] of byte;
   auth_buf:array [0..63] of byte;
   filename_size,pw_len:word;
   err,adler,crc32,adler_obj,crc32_obj,adler_volume,crc32_volume:longint;
   i,j,k,addr,n_skipped,n_input_files,n_dirs,obj_ok,ch_number_expected,numread,compsize,compsize_d,num_res:dword;
   n_exp,file_size,total,cent_size,prog_size,prog_compsize,in_size,out_size,exp_size,ch_res:qword;
   in_qualified_name,out_file,out_path,out_name,s:ansistring;
   ansi_qualified_name:ansistring;
   inskipped:boolean;
label 1;

procedure clean_variables;
begin
i:=0;
j:=0;
k:=0;
addr:=0;
n_skipped:=0;
n_input_files:=0;
n_dirs:=0;
obj_ok:=0;
ch_number_expected:=0;
numread:=0;
compsize:=0;
compsize_d:=0;
num_res:=0;
n_exp:=0;
file_size:=0;
total:=0;
cent_size:=0;
prog_size:=0;
prog_compsize:=0;
in_size:=0;
out_size:=0;
exp_size:=0;
ch_res:=0;
clean_global_vars;
end;

procedure expand_inputlist;
var i,k:dword;
fh_overhead:qword;
begin
addr:=0;
n_skipped:=0;
in_size:=0;
fh_overhead:=0;
for i:=0 to length(in_param)-1 do
   begin
   if filegetattr(in_param[i]) > 0 then
      if filegetattr(in_param[i]) and faDirectory <>0 then //Object is a dir
         begin
         expand(in_param[i],exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,n_exp);
         SetLength(in_files,length(in_files)+n_exp);
         SetLength(status_files,length(status_files)+n_exp);
         SetLength(fsizes,length(fsizes)+n_exp);
         SetLength(ftimes,length(ftimes)+n_exp);
         SetLength(fattr,length(fattr)+n_exp);
         SetLength(fattr_dec,length(fattr_dec)+n_exp);
         if in_param[i][length(in_param[i])]<>DirectorySeparator then in_param[i]:=in_param[i]+DirectorySeparator;
         for k:=0 to n_exp-1 do
            begin
            in_files[addr+k]:=exp_files[k];
            status_files[addr+k]:=true;
            fsizes[addr+k]:=exp_fsizes[k];
            in_size:=in_size+exp_fsizes[k];
            ftimes[addr+k]:=exp_ftimes[k];
            fattr[addr+k]:=exp_fattr[k];
            if (exp_fattr[k] and faDirectory)=0 then fh_overhead:=fh_overhead+length(exp_files[k])+18
            else fh_overhead:=fh_overhead+length(exp_files[k])+10;
            fattr_dec[addr+k]:=exp_fattr_dec[k];
            end;
         addr:=addr+n_exp;
         end
      else //Object is a file
         begin
         expand(in_param[i],exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,n_exp);
         SetLength(in_files,length(in_files)+1);
         SetLength(status_files,length(status_files)+1);
         SetLength(fsizes,length(fsizes)+1);
         SetLength(ftimes,length(ftimes)+1);
         SetLength(fattr,length(fattr)+1);
         SetLength(fattr_dec,length(fattr_dec)+1);
         in_files[addr]:=in_param[i];
         status_files[addr]:=true;
         fsizes[addr]:=exp_fsizes[0];
         fh_overhead:=fh_overhead+length(exp_files[0])+18;
         in_size:=in_size+exp_fsizes[0];
         ftimes[addr]:=exp_ftimes[0];
         fattr[addr]:=exp_fattr[0];
         fattr_dec[addr]:=exp_fattr_dec[0];
         addr:=addr+1;
         end
   else //Object not accessible
      begin
      SetLength(in_files,length(in_files)+1);
      SetLength(status_files,length(status_files)+1);
      SetLength(fsizes,length(fsizes)+1);
      SetLength(ftimes,length(ftimes)+1);
      SetLength(fattr,length(fattr)+1);
      SetLength(fattr_dec,length(fattr_dec)+1);
      in_files[addr]:=in_param[i];
      status_files[addr]:=false;
      inc(n_skipped,1);
      addr:=addr+1;
      end;
   end;
n_input_files:=addr;
exp_size:=in_size+headersize+authsize+6+fh_overhead;
if n_skipped=n_input_files then internal_error('No valid input found');
end;

//clean keying-related variables
procedure clean_keying_vars;
var
   k:integer;
begin
for k:=0 to SBUFSIZE do sbuf2[k]:=0;
pw:='';
password:='';
keyfile_name:='';
keyf_name:='';
pw_len:=0;
k:=0;
end;

procedure init_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_obj);
'SHA512' : SHA512Init(HashContext_obj);
'SHA256' : SHA256Init(HashContext_obj);
'SHA3_512' : SHA3_512Init(HashContext_obj);
'SHA3_256' : SHA3_256Init(HashContext_obj);
'SHA1' : SHA1Init(HashContext_obj);
'BLAKE2S' : Blake2s_Init(Blake2sContext_obj,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext_obj,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_obj);
'MD5' : MD5Init(HashContext_obj);
'CRC64' : CRC64Init(crc64_obj);
'CRC32' : CRC32Init(crc32_obj);
'ADLER32' : Adler32Init(adler_obj);
end;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA3_512' : SHA3_512Init(HashContext_volume);
'SHA3_256' : SHA3_256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'BLAKE2S' : Blake2s_Init(Blake2sContext_volume,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext_volume,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_control_algo(var buf:array of byte; size:word);
var k:integer;
begin
case upcase(algo) of
'TRIATS':
begin
if FCA_EAX256_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCF_EAX256_encrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCS_EAX256_encrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'TRITSA':
begin
if FCF_EAX256_encrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCS_EAX256_encrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCA_EAX256_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'TRISAT':
begin
if FCS_EAX256_encrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCA_EAX256_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCF_EAX256_encrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'EAX256' : if FCA_EAX256_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'TF256' : if FCF_EAX256_encrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'SP256' : if FCS_EAX256_encrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'EAX' : if FCA_EAX_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'TF' : if FCf_EAX_encrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'SP' : if FCs_EAX_encrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'HMAC' : if FCA_HMAC_encrypt(cxh, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'WHIRLPOOL' : Whirl_Update(HashContext, @buf, size);
'SHA512' : SHA512Update(HashContext, @buf, size);
'SHA256' : SHA256Update(HashContext, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext, @buf, size);
'SHA1' : SHA1Update(HashContext, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext,@buf,size);
'RIPEMD160' : RMD160Update(HashContext, @buf, size);
'MD5' : MD5Update(HashContext, @buf, size);
'CRC64' : CRC64Update(crc64, @buf, size);
'CRC32' : CRC32Update(crc32, @buf, size);
'ADLER32' : Adler32Update(adler, @buf, size);
end;
end;

procedure update_obj_control_algo(buf:array of byte; size:word);
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_obj, @buf, size);
'SHA512' : SHA512Update(HashContext_obj, @buf, size);
'SHA256' : SHA256Update(HashContext_obj, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_obj, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_obj, @buf, size);
'SHA1' : SHA1Update(HashContext_obj, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext_obj,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext_obj,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_obj, @buf, size);
'MD5' : MD5Update(HashContext_obj, @buf, size);
'CRC64' : CRC64Update(crc64_obj, @buf, size);
'CRC32' : CRC32Update(crc32_obj, @buf, size);
'ADLER32' : Adler32Update(adler_obj, @buf, size);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_volume, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext_volume,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext_volume,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_control_algo;
begin
case upcase(algo) of
'TRIATS':
begin
FCA_EAX256_final(cxe, auth);
FCF_EAX256_final(cxf, auth2);
FCS_EAX256_final(cxs, auth3);
end;
'TRITSA':
begin
FCF_EAX256_final(cxf, auth);
FCS_EAX256_final(cxs, auth2);
FCA_EAX256_final(cxe, auth3);
end;
'TRISAT':
begin
FCS_EAX256_final(cxs, auth);
FCA_EAX256_final(cxe, auth2);
FCF_EAX256_final(cxf, auth3);
end;
'EAX256' : FCA_EAX256_final(cxe, auth);
'TF256' : FCF_EAX256_final(cxf, auth);
'SP256' : FCS_EAX256_final(cxs, auth);
'EAX' : FCA_EAX_final(cxe, auth);
'TF' : FCf_EAX_final(cxf, auth);
'SP' : FCs_EAX_final(cxs, auth);
'HMAC' : FCA_HMAC_final(cxh, auth);
'WHIRLPOOL' : Whirl_Final(HashContext,WHIRL512Digest);
'SHA512' : SHA512Final(HashContext,SHA512Digest);
'SHA256' : SHA256Final(HashContext,SHA256Digest);
'SHA3_512' : SHA3_512Final(HashContext,SHA3_512Digest);
'SHA3_256' : SHA3_256Final(HashContext,SHA3_256Digest);
'SHA1' : SHA1Final(HashContext,SHA1Digest);
'BLAKE2S' : blake2s_Final(Blake2sContext,Blake2sDigest);
'BLAKE2B' : blake2b_Final(HashContext,Blake2bDigest);
'RIPEMD160' : RMD160Final(HashContext,RMD160Digest);
'MD5' : MD5Final(HashContext,MD5Digest);
'CRC64' : CRC64Final(crc64);
'CRC32' : CRC32Final(crc32);
'ADLER32' : Adler32Final(adler);
end;
end;

procedure finish_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_obj,WHIRL512Digest_obj);
'SHA512' : SHA512Final(HashContext_obj,SHA512Digest_obj);
'SHA256' : SHA256Final(HashContext_obj,SHA256Digest_obj);
'SHA3_512' : SHA3_512Final(HashContext_obj,SHA3_512Digest_obj);
'SHA3_256' : SHA3_256Final(HashContext_obj,SHA3_256Digest_obj);
'SHA1' : SHA1Final(HashContext_obj,SHA1Digest_obj);
'BLAKE2S' : blake2s_Final(Blake2sContext_obj,Blake2sDigest_obj);
'BLAKE2B' : blake2b_Final(HashContext_obj,Blake2bDigest_obj);
'RIPEMD160' : RMD160Final(HashContext_obj,RMD160Digest_obj);
'MD5' : MD5Final(HashContext_obj,MD5Digest_obj);
'CRC64' : CRC64Final(crc64_obj);
'CRC32' : CRC32Final(crc32_obj);
'ADLER32' : Adler32Final(adler_obj);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA3_512' : SHA3_512Final(HashContext_volume,SHA3_512Digest_volume);
'SHA3_256' : SHA3_256Final(HashContext_volume,SHA3_256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'BLAKE2S' : blake2s_Final(Blake2sContext_volume,Blake2sDigest_volume);
'BLAKE2B' : blake2b_Final(HashContext_volume,Blake2bDigest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure write_volume_check;
var k:dword;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do auth_buf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA256Digest_volume[k];
      'SHA3_512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA3_512Digest_volume[k];
      'SHA3_256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA3_256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA1Digest_volume[k];
      'BLAKE2S' : for k:=0 to volume_authsize-1 do auth_buf[k]:=Blake2sDigest_volume[k];
      'BLAKE2B' : for k:=0 to volume_authsize-1 do auth_buf[k]:=Blake2bDigest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do auth_buf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do auth_buf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,auth_buf,0);
      dword2bytebuf(crc64_volume.hi32,auth_buf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,auth_buf,0);
      'ADLER32' : dword2bytebuf(adler_volume,auth_buf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=auth_buf[k];
   blockwrite (f_out,auth_buf,volume_authsize);
   prog_compsize:=prog_compsize+volume_authsize;
   prog_size:=prog_size+volume_authsize;
   end;
end;

procedure write2chunks ( var num_res: dword;                     //amount of data to write
                         var buf_data: array of byte;            //data buffer
                         var f_out: fileofbyte;                  //output file
                         var out_path,out_name: ansistring;      //name and path for the output;
                         var i: dword;                           //chunk progressive number
                         var ch_size:qword;                      //chunk size
                         var ch_res: qword);                     //residual space in the given chunk
var ci,cj,k,numwritten:dword;
    addr,buf:qword;
    out_file:ansistring;
begin
addr:=0;
numwritten:=0;
while num_res>0 do
   begin
   if num_res<=ch_res then
      begin
      try
      blockwrite (f_out,buf_data,num_res,numwritten);
      except
      internal_error('IO error writing to volume '+inttostr(i));
      end;
      ci:=0;
      while ci<numwritten do
         begin
         if numwritten-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numwritten-ci;
         for k:=0 to cj-1 do sbuf1[k]:=buf_data[ci+k];
         update_volume_control_algo(sbuf1,cj);
         inc(ci,cj);
         end;
      num_res:=num_res-numwritten;
      ch_res:=ch_res-numwritten;
      addr:=0;
      end
   else
      begin
      SetLength(volume_tags,length(volume_tags)+1);
      try
      blockwrite (f_out,buf_data,ch_res,numwritten);
      except
      internal_error('IO error writing to volume '+inttostr(i));
      end;
      ci:=0;
      while ci<numwritten do
         begin
         if numwritten-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numwritten-ci;
         for k:=0 to cj-1 do sbuf1[k]:=buf_data[ci+k];
         update_volume_control_algo(sbuf1,cj);
         inc(ci,cj);
         end;
      finish_volume_control_algo;
      try
      write_volume_check;
      except
      internal_error('IO error writing volume control tag to volume '+inttostr(i));
      end;
      {$I-}close(f_out);{$I+}
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(i));
      i:=i+1;
      update_pea_filename(out_name,i,out_file);
      checkspacepea(out_path,ch_size,volume_authsize);
      assignfile(f_out,out_path+out_file);
      {$I-}rewrite(f_out);{$I+} //it will overwrite orphaned files with same name to preserve name coherence
      if IOResult<>0 then internal_error('IO error opening volume '+inttostr(i));
      init_volume_control_algo;
      num_res:=num_res-numwritten;
      if num_res<ch_size then buf:=num_res else buf:=ch_size;
      addr:=addr+numwritten;
      for k:=0 to buf do buf_data[k]:=buf_data[addr+k];
      ch_res:=ch_size;
      end;
   end;
end;

procedure init_control_algo;
var
  i:integer;
  sbufx,tsbuf2:array [0..65534] of byte;
  tpw_len,verw:Word;
begin
case upcase(algo) of
'TRIATS','TRITSA','TRISAT':
begin
for i:=0 to pw_len-1 do tsbuf2[i]:=sbuf2[i]; tpw_len:=pw_len;
case upcase(algo) of
   'TRIATS': test_pea_error('creating stream crypto subheader with '+algo,pea_eax256_subhdrP (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr256,sbuf1,num_res));
   'TRITSA': test_pea_error('creating stream crypto subheader with '+algo,pea_tfeax256_subhdrP (cxf,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,fhdr256,sbuf1,num_res));
   'TRISAT': test_pea_error('creating stream crypto subheader with '+algo,pea_speax256_subhdrP (cxs,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,shdr256,sbuf1,num_res));
   end;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to tpw_len-1 do sbuf2[i]:=tsbuf2[i]; pw_len:=tpw_len;
for i:=0 to tpw_len-1 do sbuf2[i]:=sbuf2[i] xor (pw_len+i) xor ord(upcase(algo[length(algo)-1]));
case upcase(algo) of
   'TRIATS': test_pea_error('creating stream crypto subheader with '+algo,pea_tfeax256_subhdrP (cxf,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,fhdr256,sbuf1,num_res));
   'TRITSA': test_pea_error('creating stream crypto subheader with '+algo,pea_speax256_subhdrP (cxs,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,shdr256,sbuf1,num_res));
   'TRISAT': test_pea_error('creating stream crypto subheader with '+algo,pea_eax256_subhdrP (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr256,sbuf1,num_res));
   end;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to tpw_len-1 do sbuf2[i]:=tsbuf2[i]; pw_len:=tpw_len;
for i:=0 to tpw_len-1 do sbuf2[i]:=sbuf2[i] xor (pw_len xor i) xor ord(upcase(algo[length(algo)]));
case upcase(algo) of
   'TRIATS': test_pea_error('creating stream crypto subheader with '+algo,pea_speax256_subhdrP (cxs,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,shdr256,sbuf1,num_res));
   'TRITSA': test_pea_error('creating stream crypto subheader with '+algo,pea_eax256_subhdrP (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr256,sbuf1,num_res));
   'TRISAT': test_pea_error('creating stream crypto subheader with '+algo,pea_tfeax256_subhdrP (cxf,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,fhdr256,sbuf1,num_res));
   end;
for i:=0 to tpw_len-1 do tsbuf2[i]:=0; tpw_len:=0;
verw:=hdr256.PW_Ver xor fhdr256.PW_Ver xor shdr256.PW_Ver;
word2bytebuf(verw,sbuf1,14);
verw:=0;
end;
'EAX256' : test_pea_error('creating stream crypto subheader with '+algo,pea_eax256_subhdr (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr256,sbuf1,num_res));
'TF256' : test_pea_error('creating stream crypto subheader with '+algo,pea_tfeax256_subhdr (cxf,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,fhdr256,sbuf1,num_res));
'SP256' : test_pea_error('creating stream crypto subheader with '+algo,pea_speax256_subhdr (cxs,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,shdr256,sbuf1,num_res));
'EAX' : test_pea_error('creating stream crypto subheader with '+algo,pea_eax_subhdr (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr,sbuf1,num_res));
'TF' : test_pea_error('creating stream crypto subheader with '+algo,pea_tfeax_subhdr (cxf,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,fhdr,sbuf1,num_res));
'SP' : test_pea_error('creating stream crypto subheader with '+algo,pea_speax_subhdr (cxs,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,shdr,sbuf1,num_res));
'HMAC' : test_pea_error('creating stream crypto subheader with '+algo,pea_hmac_subhdr (cxh,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr,sbuf1,num_res));
'WHIRLPOOL' : Whirl_Init(HashContext);
'SHA512' : SHA512Init(HashContext);
'SHA256' : SHA256Init(HashContext);
'SHA3_512' : SHA3_512Init(HashContext);
'SHA3_256' : SHA3_256Init(HashContext);
'SHA1' : SHA1Init(HashContext);
'BLAKE2S' : Blake2s_Init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext);
'MD5' : MD5Init(HashContext);
'CRC64' : CRC64Init(crc64);
'CRC32' : CRC32Init(crc32);
'ADLER32' : Adler32Init(adler);
end;
end;

procedure compress_file;
{
PCOMPRESS1..3 is a deflate-based scheme of compression that allows decompression
of single blocks without need of decompressing preceding blocks:
that slightly degrade compression compared to classical schemes but allow fast
access to arbitrary sectors knowing position in input data (feature not used in
this application)
}
var ci,cj,k:dword;
begin
//file data area
while ((numread<>0) and (total<file_size)) do
   begin
   blockread (f_in,wbuf1,WBUFSIZE,numread);
   inc(total,numread);
   inc(prog_size,numread);
   compsize:=numread+65536;
   {leave some room for expansion in compsize (64kb), however expanded blocks
   will be substituted by original blocks and compressed size will be set equal
   to input size, triggering decompression routine to not decompress but rather
   use the block as is (speeding up a bit operations on files that doesn't
   compress well or at all in case of user's misuse)}
   err:=zcompres.compress2(@wbuf2[0], compsize, wbuf1[0], numread, compr_level);
   if (err<>0) or (compsize>=numread) then
      begin
      wbuf2:=wbuf1;
      compsize:=numread;
      end;
   compsize_d:=compsize;
   //check of uncompressed size and data in the order it will be written
   dword2bytebuf(compsize,sbuf1,0);
   update_obj_control_algo(sbuf1,4);
   ci:=0;
   while ci<numread do
      begin
      if numread-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numread-ci;
      for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
      update_obj_control_algo(sbuf1,cj);
      inc(ci,cj);
      end;
   //compressed block size field, dword
   dword2bytebuf(compsize,wbuf1,0);
   //compressed block data field, variable sized
   for k:=0 to compsize_d-1 do wbuf1[k+4]:=wbuf2[k];
   ci:=0;
   while ci<compsize_d+4 do
      begin
      if compsize_d+4-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=compsize_d+4-ci;
      for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
      update_control_algo(sbuf1,cj);
      for k:=0 to cj-1 do wbuf1[ci+k]:=sbuf1[k];
      inc(ci,cj);
      end;
   num_res:=compsize_d+4;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  wbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
// uncompressed size of last buffer field (since it can not match the buffer size), dword
dword2bytebuf(numread,sbuf1,0);
update_obj_control_algo(sbuf1,4);
update_control_algo(sbuf1,4);
num_res:=4;
prog_compsize:=prog_compsize+4;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure nocompress_file;
begin
while ((numread<>0) and (total<file_size)) do
   begin
   blockread (f_in,sbuf1,SBUFSIZE,numread);
   inc(total,numread);
   inc(prog_size,numread);
   update_obj_control_algo(sbuf1,numread);
   update_control_algo(sbuf1,numread);
   num_res:=numread;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
end;

procedure write_eos; //unused in PEA file format 1.0
//write a trigger object that declare the end of the stream
begin
trigger_eos(sbuf1);
update_control_algo(sbuf1,6);
num_res:=6;
prog_size:=prog_size+6;
prog_compsize:=prog_compsize+6;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_eoa;
//write a trigger object that declare the end of the archive (instead of EOS in the last stream of the archive)
begin
trigger_eoa(sbuf1);
update_control_algo(sbuf1,6);
num_res:=6;
prog_size:=prog_size+6;
prog_compsize:=prog_compsize+6;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_auth;
var
  k:dword;
  ct384:THashContext;
  dg384:TSHA3_384Digest;
begin
finish_control_algo;
case upcase(algo) of
   'TRIATS','TRITSA','TRISAT':
   begin
   for k:=0 to 15 do sbuf1[k]:=auth[k];
   for k:=16 to 31 do sbuf1[k]:=auth2[k-16];
   for k:=32 to 47 do sbuf1[k]:=auth3[k-32];
   SHA3_384Init(ct384);
   SHA3_384Update(ct384, @sbuf1, 48);
   SHA3_384Final(ct384, dg384);
   for k:=0 to 47 do sbuf1[k]:=dg384[k];
   end;
   'EAX256','TF256','SP256','EAX','TF','SP','HMAC': for k:=0 to authsize-1 do sbuf1[k]:=auth[k];
   'WHIRLPOOL' : for k:=0 to authsize-1 do sbuf1[k]:=WHIRL512Digest[k];
   'SHA512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA512Digest[k];
   'SHA256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA256Digest[k];
   'SHA3_512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA3_512Digest[k];
   'SHA3_256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA3_256Digest[k];
   'SHA1' : for k:=0 to authsize-1 do sbuf1[k]:=SHA1Digest[k];
   'BLAKE2S' : for k:=0 to authsize-1 do sbuf1[k]:=Blake2sDigest[k];
   'BLAKE2B' : for k:=0 to authsize-1 do sbuf1[k]:=Blake2bDigest[k];
   'RIPEMD160' : for k:=0 to authsize-1 do sbuf1[k]:=RMD160Digest[k];
   'MD5' : for k:=0 to authsize-1 do sbuf1[k]:=MD5Digest[k];
   'CRC64' :
   begin
   dword2bytebuf(crc64.lo32,sbuf1,0);
   dword2bytebuf(crc64.hi32,sbuf1,4);
   end;
   'CRC32' : dword2bytebuf(crc32,sbuf1,0);
   'ADLER32' : dword2bytebuf(adler,sbuf1,0);
   end;
s:='';
num_res:=authsize;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_obj_check;
var k:dword;
begin
if upcase(obj_algo)<>'NOALGO' then
   begin
   case upcase(obj_algo) of
      'WHIRLPOOL' : for k:=0 to obj_authsize-1 do sbuf1[k]:=WHIRL512Digest_obj[k];
      'SHA512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA512Digest_obj[k];
      'SHA256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA256Digest_obj[k];
      'SHA3_512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA3_512Digest_obj[k];
      'SHA3_256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA3_256Digest_obj[k];
      'SHA1' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA1Digest_obj[k];
      'BLAKE2S' : for k:=0 to obj_authsize-1 do sbuf1[k]:=Blake2sDigest_obj[k];
      'BLAKE2B' : for k:=0 to obj_authsize-1 do sbuf1[k]:=Blake2bDigest_obj[k];
      'RIPEMD160' : for k:=0 to obj_authsize-1 do sbuf1[k]:=RMD160Digest_obj[k];
      'MD5' : for k:=0 to obj_authsize-1 do sbuf1[k]:=MD5Digest_obj[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_obj.lo32,sbuf1,0);
      dword2bytebuf(crc64_obj.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32_obj,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler_obj,sbuf1,0);
      end;
   for k:=0 to obj_authsize-1 do obj_tags[i,k]:=sbuf1[k];
   update_control_algo(sbuf1,obj_authsize);
   num_res:=obj_authsize;
   prog_size:=prog_size+num_res;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   end;
end;

procedure first_gui_output;
var i,k:integer;
begin
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelEncrypt2.Caption:='Input: ';
if length(in_param)>4 then k:=4 else k:=length(in_param);
for i:=0 to k-1 do Form_pea.LabelEncrypt2.Caption:=Form_pea.LabelEncrypt2.Caption+in_param[i]+', ';
if length(in_param)>4 then Form_pea.LabelEncrypt2.Caption:=Form_pea.LabelEncrypt2.Caption+' ...';
Form_pea.LabelEncrypt3.Caption:='Output: '+out_param+'.*';
Form_pea.LabelEncrypt4.Caption:='Using: '+compr+'; stream: '+algo+', object(s): '+obj_algo+', volume(s): '+volume_algo;
Form_pea.LabelTime1.Caption:='Creating archive...';
Form_pea.Panel1.visible:=true;
Form_pea.LabelE1.Visible:=true;
end;

procedure evaluate_volumes;
begin
if exp_size>0 then
   begin
   ch_number_expected:=(exp_size div ch_size)+1;
   if (exp_size mod ch_size)=0 then ch_number_expected:=ch_number_expected-1;
   end
else ch_number_expected:=0;
if ch_number_expected>9999 then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Expected '+inttostr(ch_number_expected)+' volumes. It seems a lot! Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3)
   else
      if MessageDlg('Up to '+inttostr(ch_number_expected)+' volumes are expected. It seems a lot, even if the selected compression scheme may reduce the actual number. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3);
if ch_number_expected>0 then
   if (upcase(compr)<>'PCOMPRESS0') then
      if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt5.Caption:='Volume number and total output size may vary due to the compressibility of the input; volume size: '+inttostr(ch_size+volume_authsize)+' B'
      else Form_pea.LabelEncrypt5.Caption:='Expected a single volume archive, output size may vary due to the compressibility of the input'
   else
      if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt5.Caption:='Expected '+inttostr(ch_number_expected)+' volume(s) of '+inttostr(ch_size+volume_authsize)+' B for a total output size of '+inttostr(exp_size)+' B'
      else Form_pea.LabelEncrypt5.Caption:='Expected a single volume archive of '+inttostr(exp_size)+' B of size'
else Form_pea.LabelEncrypt5.Caption:='Unknown number of volumes expected';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_param[0];
out_file:=extractfilename(out_param);
out_path:=extractfilepath(out_param);
if out_file='' then extractdirname(out_param,out_path,out_file); //first input object is a dir, output is set as a file in the same path of the dir and prefixing dir name as name
if out_path='' then out_path:=executable_path;
if setcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_file+'.*';
if exp_size>diskfree(0) then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3)
   else
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media (however the selected compression scheme may reduce the total space needed for the output). Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3);
end;

procedure do_report_PEA;
var
   h,k:dword;
   s:ansistring;
begin
Form_report.InputT.Caption:='Input';
Form_report.OutputT.Caption:='Output';
Form_report.Caption:='Log PEA';
Form_report.StringGrid1.ColCount:=7;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.Cells[3,0]:='Age';
Form_report.StringGrid1.Cells[4,0]:='Attrib';
Form_report.StringGrid1.Cells[5,0]:='Attrib n.';
Form_report.StringGrid1.Cells[6,0]:=obj_algo;
Form_report.StringGrid1.RowCount:=n_input_files+1;
obj_ok:=0;
for k:=0 to n_input_files-1 do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=in_files[k];
    if status_files[k]=true then Form_report.StringGrid1.Cells[1,k+1]:='Archived'
    else
       begin
       inskipped:=true;
       Form_report.StringGrid1.Cells[1,k+1]:='Skipped';
       end;
    if status_files[k]=true then
       begin
       Form_report.StringGrid1.Cells[2,k+1]:=inttostr(fsizes[k]);
       if ftimes[k]<>0 then Form_report.StringGrid1.Cells[3,k+1]:=datetimetostr(filedatetodatetime(ftimes[k]));
       Form_report.StringGrid1.Cells[4,k+1]:=fattr_dec[k];
       Form_report.StringGrid1.Cells[5,k+1]:=inttostr(fattr[k]);
       if upcase(obj_algo)<>'NOALGO' then
          begin
          s:='';
          for h:=0 to obj_authsize-1 do s:=s+hexstr(@obj_tags[k,h],1);
          Form_report.StringGrid1.Cells[6,k+1]:=s;
          end;
       inc(obj_ok,1);
       end;
    end;
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=2;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:=volume_algo;
Form_report.StringGrid2.RowCount:=j+1;
for k:=0 to j-1 do
    begin
    update_pea_filename(out_path+out_name,k+1,s);
    Form_report.StringGrid2.Cells[0,k+1]:=s;
    if upcase(volume_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[1,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
//operation parameters
Form_report.Label1.Caption:=Form_pea.LabelEncrypt4.Caption;
//input
Form_report.Label2.Caption:='Archived '+inttostr(obj_ok)+' objects ('+inttostr(n_dirs)+' dirs, '+inttostr(obj_ok-n_dirs)+' files) of '+inttostr(n_input_files)+' ('+inttostr(n_input_files-obj_ok)+' not found); input '+inttostr(in_size)+' B';
//output
Form_report.Label3.Caption:=Form_pea.LabelEncrypt6.Caption;
//output name
Form_report.Label4.Caption:=Form_pea.LabelEncrypt3.Caption;
end;

procedure last_gui_output;
begin
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_name+'.*; tag: '+s;
if compr<>'PCOMPRESS0' then out_size:=prog_compsize
else out_size:=prog_size;
if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt6.Caption:=inttostr(j)+' volume(s), '+inttostr(ch_size+volume_authsize)+' B; total '+inttostr(out_size)+' B'
else Form_pea.LabelEncrypt6.Caption:='Single volume, '+inttostr(out_size)+' B';
if compr<>'PCOMPRESS0' then if in_size<>0 then Form_pea.LabelEncrypt6.Caption:=Form_pea.LabelEncrypt6.Caption+', '+inttostr((out_size * 100) div (in_size+1))+'% of input';
do_report_PEA;
Form_pea.LabelEncrypt5.Caption:=Form_report.Label2.Caption;
Form_pea.LabelOut1.Caption:=inttostr((out_size * 100) div (in_size+1))+'% of input size';
if ((out_size * 200) div (in_size+1))<16 then Form_pea.ShapeE2.Width:=16
else
   if ((out_size * 200) div (in_size+1))>300 then Form_pea.ShapeE2.Width:=300
   else Form_pea.ShapeE2.Width:=(out_size * 200) div (in_size+1);
end;

begin
exitcode:=-1;
clean_variables;
inskipped:=false;
get_fingerprint (fingerprint,false);
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.Caption:='Pea';
Form_pea.PanelDecrypt1.visible:=false;
Form_pea.PanelEncrypt1.visible:=true;
ts_start:=datetimetotimestamp(now);
i:=0;
//give preliminary information on work status to the GUI
first_gui_output;
{
expand the list of input objects and evaluate input and expected uncompressed
output size (taking overheads in account):
if the object is a file add file name to the list;
if the object is a dir, recursively add the content to the list (any object in
the dir and all subdir will be added to the list; if you want only file sin the
root dir to be added to the list, add them as files, don't add the dir);
if the object is not found mark it as skipped in the status list, otherwise mark
it as ok (the different lists indexes must remain sincronized)
}
expand_inputlist;
cent_size:=(exp_size div 100)+1; //1% of expected output size, used for progress indication
{
evaluate volumes number;
at 9999 objects the program will warn and proceed only after user's permission,
however the program has no sort of problem until 999999 chunks (but the host
system may!)
}
evaluate_volumes;
{
get output path and name;
evaluate if the path has enough free space for expected output.
}
evaluate_output;
//check if output path has room for a chunk of given size (mandatory)
checkspacepea(out_path,ch_size,volume_authsize);
{
start the actual operation routine
1) generate the archive header;
2a) generate the stream header (current implementation allow only a stream for archive)
2b) if using AE as stream check algorithm, initialize the encryption and generate additional header data needed for the encryption (similar to FCA file header);
2c) if compression is used write compressed buffer's size
3) add objects to archive; if the object is a non-empty file write the data to the archive, synchronously doing optional compression and control at stream, object and volume level; write object level control tag at the end of each object
4) generate End Of Archive trigger followed by the appropriate control tag
5) write the optional volume control tag at the end of each volume (starting from an appropriate position before volume end, due to the tag size required)
}
//1) generate archive header
out_name:=out_file;
if ch_size=1024*1024*1024*1024*1024-volume_authsize then
   assignfile(f_out,out_file+'.pea')
else
   assignfile(f_out,out_file+'.000001.pea');//current dir was jet set to out_path
{$I-}rewrite(f_out);{$I+}
if IOResult<>0 then internal_error('IO error opening first volume');
SetLength(volume_tags,length(volume_tags)+1);
init_volume_control_algo;
test_pea_error('creating archive header',pea_archive_hdr(volume_algo,sbuf1,num_res));
j:=1;
ch_res:=ch_size;
prog_size:=num_res;
prog_compsize:=num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to 9 do auth_buf[i]:=sbuf1[i];
//2a) generate stream header
test_pea_error('creating stream header',pea_stream_hdr(compr,algo,obj_algo,sbuf1,num_res));
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to 9 do auth_buf[i+10]:=sbuf1[i];
// 2b) init stream control algorithm, generate crypto subheader if needed
if pwneeded=true then
   begin
   //get password
   if (upcase(pw_param)='INTERACTIVE') or (upcase(pw_param)='INTERACTIVE_REPORT') then
      begin
      //password is pw string that was already entered in EditPW.Text
      //keyfile name is keyfile_name already entered
      end
   else
      begin
      pw:=password; //pw is got from commandline (not recommended)
      keyfile_name:=keyf_name; //keyfile name is got from command line
      end;
   pw_len:=length(pw);
   if pw_len=0 then internal_error('invalid password length');
   for k:=0 to pw_len-1 do sbuf2[k]:=ord(pw[k+1]);//copy password into an array of byte
   //append headers to password's array (sbuf2)
   for i:=0 to 1 do auth_buf[i+20]:=sbuf1[i];
   for k:=0 to 21 do sbuf2[pw_len+k]:=auth_buf[k];
   pw_len:=pw_len+22;
   //append keyfile to password's array (sbuf2)
   if upcase(keyfile_name)<>'NOKEYFILE' then
      test_pea_error('accessing keyfile',use_keyfile(keyfile_name,2048,numread,sbuf2,pw_len));
   end;
init_control_algo;
clean_keying_vars;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
case upcase(algo) of
   'TRIATS','TRITSA','TRISAT': //mask exact archive size extending header 1..128 byte with random data (encrypted)
   begin
   gen_rand(randarr);
   randarr[0]:=randarr[0] div 2;
   for i:=0 to 127 do sbuf1[i]:=randarr[i];
   update_control_algo(sbuf1,randarr[0]+1);
   num_res:=randarr[0]+1;
   prog_size:=prog_size+num_res;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
   for i:=0 to 255 do randarr[i]:=0;
   end;
   end;
if pwneeded=false then update_control_algo(auth_buf,20); //check the archive and stream headers
// 2c) buffer size field (data to compress at once), dword, stream specific
if upcase(compr)<>'PCOMPRESS0' then
   begin
   dword2bytebuf(WBUFSIZE,sbuf1,0);
   update_control_algo(sbuf1,4);
   num_res:=4;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   end;
//3) for each object: if the object is accessible add it to the archive
n_dirs:=0;
for i:=0 to n_input_files-1 do
   begin
   SetLength(obj_tags,length(obj_tags)+1);
   if status_files[i]=false then goto 1; //the object, during creation of the list, was not accessible
   in_qualified_name:=in_files[i];
   addr:=i;
   k:=check_in(f_in,in_qualified_name,status_files,i);
   if k<>0 then
      begin
      inc(n_skipped,1);
      goto 1; //the object is actually not accessible
      end;
   init_obj_control_algo;
   //2 byte (word) sized field for size of the input object qualified name, if = 0 then the object is a trigger
   ansi_qualified_name:=utf8toansi(in_qualified_name);
   filename_size:=length(ansi_qualified_name);//(in_files[i]);
   word2bytebuf(filename_size,sbuf1,0);
   //variable sized field for input object qualified name
   for k:=0 to filename_size-1 do sbuf1[k+2]:=ord(ansi_qualified_name[k+1]);
   //4 byte (dword) sized field for input object last modification time
   if filegetattr(in_files[i]) and faDirectory = 0 then k:=fileage(in_qualified_name)
   else
      begin
      if findfirst(in_files[i]+'.',faDirectory,r) = 0 then k:=r.Time
      else k:=datetimetofiledate(now); //should not happen
      FindClose(r);
      end;
   dword2bytebuf(k,sbuf1,filename_size+2);
   //4 byte (dword) sized field for input object attributes
   k:=filegetattr(in_qualified_name);
   dword2bytebuf(k,sbuf1,filename_size+6);
   if filegetattr(in_qualified_name) and faDirectory <>0 then //the object is a directory
      begin
      update_obj_control_algo(sbuf1,filename_size+10);
      update_control_algo(sbuf1,filename_size+10);
      num_res:=filename_size+10;
      prog_size:=prog_size+num_res;
      prog_compsize:=prog_compsize+num_res;
      inc(n_dirs,1);
      write2chunks ( num_res,
                     sbuf1,
                     f_out,
                     out_path,out_name,
                     j,
                     ch_size,
                     ch_res);
      finish_obj_control_algo;
      write_obj_check;
      end
   else //the object is a file
      begin
      //8 byte (qword) sized field for input file size
      srcfilesize(in_qualified_name,file_size);
      //file_size:=system.filesize(f_in);
      qword2bytebuf(file_size,sbuf1,filename_size+10);
      update_obj_control_algo(sbuf1,filename_size+18);
      update_control_algo(sbuf1,filename_size+18);
      num_res:=filename_size+18;
      prog_size:=prog_size+num_res;
      prog_compsize:=prog_compsize+num_res;
      write2chunks ( num_res,
                     sbuf1,
                     f_out,
                     out_path,out_name,
                     j,
                     ch_size,
                     ch_res);
      if file_size>0 then //non empty file
         begin
         ////// for each file: 3) mangle and write file data
         total:=0;
         numread:=1;
         if upcase(compr)<>'PCOMPRESS0' then compress_file
         else nocompress_file; //no compression
         closefile(f_in);
         end;
      finish_obj_control_algo;
      write_obj_check;
      end;
   1:
   end;
//4) close stream: write trigger of end of archive (since PEA1.0 files contain a single stream) and write authentication tag (if applicable)
write_eoa;
if upcase(algo)<>'NOALGO' then write_auth
else s:='no control tag';
//5) generate last volume control tag
SetLength(volume_tags,length(volume_tags)+1);
finish_volume_control_algo;
write_volume_check;
{$I-}closefile(f_out);{$I+}
if IOResult<>0 then internal_error('IO error closing last volume');
//give final job information to the GUI
last_gui_output;
//calculate operation time
timing(ts_start,in_size);
//make accessible exit button and link to the detailed job log
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.ButtonPeaExit.Visible:=false;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log PEA','txt',upcase(pw_param),out_path);
if inskipped=true then exitcode:=-2 else exitcode:=0;
Sleep(500);
if closepolicy>0 then Form_pea.Close; //error conditions are intercepted before and handled with internal_error procedure
end;

{
UnPEA
Decrypt, authenticate, join, decompress, extract PEA format archives

Error management:
- errors in objects, stream or volumes are checked by strong functions and
  reported in job log, that can be saved, at the end of the job a popup message
  will warn that such errors were encountered;
- errors that prevent the application to work make the application quit with a
  descriptive message, if the error is of unknown nature application will
  autosave a job log allowing further analysis.

Known issues:
- FPC's set object attributes works only on Windows, set object date seem not
actually working (both are currently not supported on *x);
}

procedure unpea;
var
   in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name:ansistring;
   i:integer;

procedure parse_unpea_cl;
begin
i:=0;
try
   in_qualified_name:=(paramstr(2));
   if not(fileexists(in_qualified_name)) then
      internal_error('"'+in_qualified_name+'" not exist');
   out_param:=(paramstr(3));
   date_param:=upcase(paramstr(4)); //how to use file age information: SETDATE (not supported on *x) set the output file date to the input file date, RESETDATE gives new file age
   if date_param<>'RESETDATE' then  //(date_param<>'SETDATE') or
      internal_error('"'+date_param+'" is not a valid parameter for file age metadata: RESETDATE (gives new file age) is actually the only option featured by the program');
   attr_param:=upcase(paramstr(5)); //like the previous, about attribute data: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output object attribute as they are by default for the target system and position
   if not ((attr_param='SETATTR') or (attr_param='RESETATTR')) then
      internal_error('"'+attr_param+'" is not a valid parameter for file attributes metadata: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output objects attributes as they are by default for the target system and position');
   struct_param:=upcase(paramstr(6)); //EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures
   if struct_param<>'EXTRACT2DIR' then
      internal_error('"'+struct_param+'" is not a valid parameter for output structure, the only parameter supported is EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures');
   //get operation mode
   pw_param:=upcase(paramstr(7));
   if (pw_param<>'INTERACTIVE') and (pw_param<>'INTERACTIVE_REPORT') then
      begin
      inc(i,1);
      password:=(paramstr(7+i));
      inc(i,1);
      keyf_name:=(paramstr(7+i));
      end
   else if (pw_param<>'INTERACTIVE') and (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'INTERACTIVE_REPORT') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter, please refer to the documentation');
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_unpea_cl; //parse and validate command line
unpea_procedure(in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name);
end;

procedure unpea_lib_procedure ( in_qualified_name,                              //archive qualified name
                                out_param,                                      //dir were extracting the archive (or AUTONAME)
                                date_param,                                     //actually only supported RESETDATE, reset date of extracted files
                                attr_param,                                     //RESETATTR (or SETATTR only on Windows to set object's attributes as on original objects)
                                struct_param,                                   //actually only supported EXTRACT2DIR, create a dir and extract archive in the dir using shortest paths for archived objects
                                password,keyf_name:ansistring;                  //password and keyfile qualified name (if needed)
                                opmode:ansistring);                             //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:ansistring;
begin
if not(fileexists(in_qualified_name)) then
   internal_error('"'+in_qualified_name+'" not exist');
//how to use file age information: SETDATE (not supported on *x) set the output file date to the input file date, RESETDATE gives new file age
if date_param<>'RESETDATE' then  //(date_param<>'SETDATE') or
   internal_error('"'+date_param+'" is not a valid parameter for file age metadata: RESETDATE (gives new file age) is actually the only option featured by the program');
//like the previous, about attribute data: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output object attribute as they are by default for the target system and position
if not ((attr_param='SETATTR') or (attr_param='RESETATTR')) then
   internal_error('"'+attr_param+'" is not a valid parameter for file attributes metadata: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output objects attributes as they are by default for the target system and position');
//EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures
if struct_param<>'EXTRACT2DIR' then
   internal_error('"'+struct_param+'" is not a valid parameter for output structure, the only parameter supported is EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures');
//get operation mode
if (upcase(opmode)<>'INTERACTIVE') and (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'INTERACTIVE_REPORT') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode, please refer to the documentation');
if (upcase(opmode)='INTERACTIVE') or (upcase(opmode)='INTERACTIVE_REPORT') then
   internal_error('INTERACTIVE* modes are not allowed calling unpea_lib_procedure, use BATCH* or HIDDEN* modes');
pw_param:=upcase(opmode);
unpea_procedure(in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name);
end;

procedure unpea_procedure ( in_qualified_name,
                            out_param,
                            date_param,
                            attr_param,
                            struct_param,
                            pw_param,
                            password,
                            keyf_name:ansistring);
var
   hdr,hdrd : TFCAHdr;
   fhdr,fhdrd : TFCFHdr;
   shdr,shdrd : TFCSHdr;
   hdr256,hdrd256 : TFCA256Hdr;
   fhdr256,fhdrd256 : TFCF256Hdr;
   shdr256,shdrd256 : TFCS256Hdr;
   cxe : TAES_EAXContext;
   cxf : Ttf_EAXContext;
   cxs : Tsp_EAXContext;
   cxh : TFCA_HMAC_Context;
   auth,auth2,auth3 : array [0..15] of byte;//TFCA_AuthBlock;
   HashContext,HashContext_obj,HashContext_volume: THashContext;
   Blake2sContext,Blake2sContext_obj,Blake2sContext_volume:blake2s_ctx;
   Blake2sDigest,Blake2sDigest_obj,Blake2sDigest_volume:TBlake2sDigest;
   Blake2bDigest,Blake2bDigest_obj,Blake2bDigest_volume:TBlake2bDigest;
   Whirl512Digest,Whirl512Digest_obj,Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest,SHA512Digest_obj,SHA512Digest_volume: TSHA512Digest;
   SHA256Digest,SHA256Digest_obj,SHA256Digest_volume: TSHA256Digest;
   SHA3_512Digest,SHA3_512Digest_obj,SHA3_512Digest_volume: TSHA3_512Digest;
   SHA3_256Digest,SHA3_256Digest_obj,SHA3_256Digest_volume: TSHA3_256Digest;
   SHA1Digest,SHA1Digest_obj,SHA1Digest_volume: TSHA1Digest;
   RMD160Digest,RMD160Digest_obj,RMD160Digest_volume: TRMD160Digest;
   MD5Digest,MD5Digest_obj,MD5Digest_volume: TMD5Digest;
   crc64,crc64_obj,crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out:file of byte;
   sbuf1,sbuf2:array [0..65534] of byte;
   tagbuf,exp_auth:array [0..63] of byte;
   compr_level,headersize,authsize,obj_authsize,volume_authsize,archive_datetimeencoding,storead:byte;
   pw_len,fns:word;
   adler,crc32,adler_obj,crc32_obj,adler_volume,crc32_volume:longint;
   i,j,ci,cj,h,k,numread,numwritten,n_chunks,n_dirs,n_input_files,compsize,uncompsize,addr,fage,fattrib,buf_size:dword;
   total,wrk_space,exp_space,cent_size,fs,out_size,qw0,qw1,qw2,qw3,qw4,qw5,qw6,qw7:qword;
   nobj:int64;
   stream_error,obj_error,volume_error,end_of_archive,pwneeded,chunks_ok,filenamed,out_created,no_more_files,readingstream,readingheader,readingfns,readingtrigger,readingfn,readingfs,readingfage,readingfattrib,readingcompsize,fassigned,readingf,readingcompblock,readingobjauth,readingauth,singlevolume:boolean;
   subroot,basedir,s,in_file,in_name,in_folder,out_path,out_file,algo,obj_algo,volume_algo,compr,fn,finpre:ansistring;
label 1;

procedure clean_variables;
begin
i:=0;
j:=0;
h:=0;
k:=0;
numread:=0;
numwritten:=0;
n_chunks:=0;
n_dirs:=0;
n_input_files:=0;
compsize:=0;
uncompsize:=0;
addr:=0;
fage:=0;
fattrib:=0;
total:=0;
cent_size:=0;
wrk_space:=0;
exp_space:=0;
fs:=0;
nobj:=0;
out_size:=0;
clean_global_vars;
end;

procedure evaluate_archive_size(var exp_space:qword; var cent_size:qword); //succeed if all chunks are accessible
var qw:qword;
begin
j:=1;
no_more_files:=false;
exp_space:=0;
while no_more_files=false do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file)
   else no_more_files:=true;
   if fileexists(in_folder+in_file) then
      begin
      assignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      srcfilesize(in_folder+in_file,qw);
      exp_space:=exp_space+qw;
      //exp_space:=exp_space+system.filesize(f_in);
      closefile(f_in);
      j:=j+1;
      end
   else no_more_files:=true;
   end;
n_chunks:=j-1;
cent_size:=(exp_space div 100)+1;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, expected '+inttostr(n_chunks)+' volume(s), total '+inttostr(exp_space)+' B';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_folder+in_name;
out_file:=extractfilename(out_param);
out_path:=extractfilepath(out_param);
if out_file='' then out_file:=in_name; //if no output name is explicitly given, the output name is assumed to be the name of the first input file
if out_path='' then out_path:=in_folder; //if no output path is explicitly given, the output path is assumed to be the path of the first input file
if out_path='' then out_path:=executable_path;
if setcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
if exp_space>diskfree(0) then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3)
   else
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media (however the selected compression scheme may reduce the total space needed for the output). Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else halt(-3);
end;

procedure ansiextract2dir;
var
   afn,fnpath,fnname,aout_path,aout_file:ansistring;
begin
afn:=fn;
aout_path:=utf8toansi(out_path);
aout_file:=utf8toansi(out_file);
if afn[length(afn)]=DirectorySeparator then
   begin
   ansiextractdirname(afn,fnpath,fnname);
   if subroot='' then
      begin
      subroot:=fnpath;
      basedir:=afn;
      end;
   if ansicontainsstr(fnpath,basedir) then
      begin
      s:=copy(fnpath,length(subroot)+1,length(fnpath)-length(subroot)-1);
      end
   else
      begin
      subroot:=fnpath;
      basedir:=afn;
      s:='';
      end;
   try
      {$I-}if s<>'' then mkdir(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname)
      else mkdir(aout_path+aout_file+directoryseparator+fnname);{$I+}
   except
      if IOResult<>0 then internal_error('IO error creating dir '+ansitoutf8(fnname));
   end;
   {$IFDEF MSWINDOWS}
   if attr_param='SETATTR' then filesetattr(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fattrib);
   filesetdate(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fage);
   {$ENDIF}
   readingfns:=true;
   end
else
   begin
   fnname:=extractfilename(afn);
   fnpath:=extractfilepath(afn);
   if subroot='' then
      begin
      subroot:=fnpath;
      s:='';
      end
   else s:=copy(fnpath,length(subroot)+1,length(fnpath)-length(subroot)-1);
      if setcurrentdir(aout_path+aout_file+directoryseparator+s)<>true then s:='';
      h:=0;
      filenamed:=false;
      repeat
         if h=0 then
            if fileexists(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname) then inc(h,1)
            else filenamed:=true
         else
            if fileexists(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname+' - '+inttostr(h)+extractfileext(afn)) then inc(h,1)
            else filenamed:=true;
      until filenamed = true;
      if h>0 then fnname:=fnname+' - '+inttostr(h)+extractfileext(afn);
      try
      assignfile(f_out,aout_path+aout_file+directoryseparator+s+directoryseparator+fnname);
      setcurrentdir(aout_path+aout_file);
      {$I-}rewrite(f_out);{$I+}
      except
      internal_error('IO error creating '+ansitoutf8(fnname));
      end;
      if IOResult<>0 then internal_error('IO error creating '+ansitoutf8(fnname));
      {$IFDEF MSWINDOWS}
      if attr_param='SETATTR' then filesetattr(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fattrib);
      //filesetdate(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fage); fails
      {$ENDIF}
      readingfs:=true;
      fassigned:=true;
      end;
end;

procedure init_AE256_control_algo;
var
   i:integer;
   tsbuf2:array [0..65534] of byte;
   verw:word;
begin
case  upcase(algo) of
'TRIATS':
begin
for i:=0 to pw_len-1 do tsbuf2[i]:=sbuf2[i];
hdr256.FCAsig:=hdr.FCAsig;
hdr256.Flags:=hdr.Flags;
hdr256.Salt:=hdr.Salt;
hdr256.PW_Ver:=hdr.PW_Ver;
hdrd256:=hdr256;
if FCA_EAX256_initP(cxe, @sbuf2, pw_len, hdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
fhdr256.FCfsig:=fhdr.FCfsig;
fhdr256.Flags:=fhdr.Flags;
fhdr256.Salt:=fhdr.Salt;
fhdr256.PW_Ver:=fhdr.PW_Ver;
fhdrd256:=fhdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i] xor (pw_len+i) xor ord(upcase(algo[length(algo)-1]));
if FCf_EAX256_initP(cxf, @sbuf2, pw_len, fhdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
shdr256.FCssig:=shdr.FCssig;
shdr256.Flags:=shdr.Flags;
shdr256.Salt:=shdr.Salt;
shdr256.PW_Ver:=shdr.PW_Ver;
shdrd256:=shdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i];
for i:=0 to pw_len-1 do sbuf2[i]:=sbuf2[i] xor (pw_len xor i) xor ord(upcase(algo[length(algo)]));
if FCs_EAX256_initP(cxs, @sbuf2, pw_len, shdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
verw:=hdrd256.PW_Ver xor fhdrd256.PW_Ver xor shdrd256.PW_Ver;
if shdr256.PW_ver<>verw then internal_error('Wrong password or keyfile');
for i:=0 to pw_len-1 do tsbuf2[i]:=0;
verw:=0;
end;
'TRITSA':
begin
for i:=0 to pw_len-1 do tsbuf2[i]:=sbuf2[i];
fhdr256.FCfsig:=fhdr.FCfsig;
fhdr256.Flags:=fhdr.Flags;
fhdr256.Salt:=fhdr.Salt;
fhdr256.PW_Ver:=fhdr.PW_Ver;
fhdrd256:=fhdr256;
if FCf_EAX256_initP(cxf, @sbuf2, pw_len, fhdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
shdr256.FCssig:=shdr.FCssig;
shdr256.Flags:=shdr.Flags;
shdr256.Salt:=shdr.Salt;
shdr256.PW_Ver:=shdr.PW_Ver;
shdrd256:=shdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i] xor (pw_len+i) xor ord(upcase(algo[length(algo)-1]));
if FCs_EAX256_initP(cxs, @sbuf2, pw_len, shdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
hdr256.FCAsig:=hdr.FCAsig;
hdr256.Flags:=hdr.Flags;
hdr256.Salt:=hdr.Salt;
hdr256.PW_Ver:=hdr.PW_Ver;
hdrd256:=hdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i];
for i:=0 to pw_len-1 do sbuf2[i]:=sbuf2[i] xor (pw_len xor i) xor ord(upcase(algo[length(algo)]));
if FCA_EAX256_initP(cxe, @sbuf2, pw_len, hdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
verw:=hdrd256.PW_Ver xor fhdrd256.PW_Ver xor shdrd256.PW_Ver;
if hdr256.PW_ver<>verw then internal_error('Wrong password or keyfile');
for i:=0 to pw_len-1 do tsbuf2[i]:=0;
verw:=0;
end;
'TRISAT':
begin
for i:=0 to pw_len-1 do tsbuf2[i]:=sbuf2[i];
shdr256.FCssig:=shdr.FCssig;
shdr256.Flags:=shdr.Flags;
shdr256.Salt:=shdr.Salt;
shdr256.PW_Ver:=shdr.PW_Ver;
shdrd256:=shdr256;
if FCs_EAX256_initP(cxs, @sbuf2, pw_len, shdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
hdr256.FCAsig:=hdr.FCAsig;
hdr256.Flags:=hdr.Flags;
hdr256.Salt:=hdr.Salt;
hdr256.PW_Ver:=hdr.PW_Ver;
hdrd256:=hdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i] xor (pw_len+i) xor ord(upcase(algo[length(algo)-1]));
if FCA_EAX256_initP(cxe, @sbuf2, pw_len, hdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
fhdr256.FCfsig:=fhdr.FCfsig;
fhdr256.Flags:=fhdr.Flags;
fhdr256.Salt:=fhdr.Salt;
fhdr256.PW_Ver:=fhdr.PW_Ver;
fhdrd256:=fhdr256;
for i:=0 to pw_len-1 do sbuf2[i]:=tsbuf2[i];
for i:=0 to pw_len-1 do sbuf2[i]:=sbuf2[i] xor (pw_len xor i) xor ord(upcase(algo[length(algo)]));
if FCf_EAX256_initP(cxf, @sbuf2, pw_len, fhdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
verw:=hdrd256.PW_Ver xor fhdrd256.PW_Ver xor shdrd256.PW_Ver;
if fhdr256.PW_ver<>verw then internal_error('Wrong password or keyfile');
for i:=0 to pw_len-1 do tsbuf2[i]:=0;
verw:=0;
end;
'EAX256':
begin
hdr256.FCAsig:=hdr.FCAsig;
hdr256.Flags:=hdr.Flags;
hdr256.Salt:=hdr.Salt;
hdr256.PW_Ver:=hdr.PW_Ver;
hdrd256:=hdr256;
if FCA_EAX256_init(cxe, @sbuf2, pw_len, hdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if hdr256.PW_ver<>hdrd256.PW_ver then internal_error('eax Wrong password or keyfile');
end;
'TF256':
begin
fhdr256.FCfsig:=fhdr.FCfsig;
fhdr256.Flags:=fhdr.Flags;
fhdr256.Salt:=fhdr.Salt;
fhdr256.PW_Ver:=fhdr.PW_Ver;
fhdrd256:=fhdr256;
if FCf_EAX256_init(cxf, @sbuf2, pw_len, fhdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if fhdr256.PW_ver<>fhdrd256.PW_ver then internal_error('Wrong password or keyfile');
end;
'SP256':
begin
shdr256.FCssig:=shdr.FCssig;
shdr256.Flags:=shdr.Flags;
shdr256.Salt:=shdr.Salt;
shdr256.PW_Ver:=shdr.PW_Ver;
shdrd256:=shdr256;
if FCs_EAX256_init(cxs, @sbuf2, pw_len, shdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if shdr256.PW_ver<>shdrd256.PW_ver then internal_error('Wrong password or keyfile');
end;
end;
end;

procedure init_AE128_control_algo;
begin
case upcase(algo) of
'TF':
begin
fhdrd:=fhdr;
if FCf_EAX_init(cxf, @sbuf2, pw_len, fhdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if fhdr.PW_ver<>fhdrd.PW_ver then internal_error('Wrong password or keyfile');
end;
'SP':
begin
shdrd:=shdr;
if FCs_EAX_init(cxs, @sbuf2, pw_len, shdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if shdr.PW_ver<>shdrd.PW_ver then internal_error('Wrong password or keyfile');
end;
else
begin
hdrd:=hdr;
if upcase(algo)='EAX' then if FCA_EAX_init(cxe, @sbuf2, pw_len, hdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if upcase(algo)='HMAC' then if FCA_HMAC_init(cxh, @sbuf2, pw_len, hdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if hdr.PW_ver<>hdrd.PW_ver then internal_error('Wrong password or keyfile');
end;
end;
end;

procedure init_nonAE_control_algo;
begin
case upcase(algo) of
'WHIRLPOOL' : Whirl_Init(HashContext);
'SHA512' : SHA512Init(HashContext);
'SHA256' : SHA256Init(HashContext);
'SHA3_512' : SHA3_512Init(HashContext);
'SHA3_256' : SHA3_256Init(HashContext);
'SHA1' : SHA1Init(HashContext);
'BLAKE2S' : Blake2s_Init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext);
'MD5' : MD5Init(HashContext);
'CRC64' : CRC64Init(crc64);
'CRC32' : CRC32Init(crc32);
'ADLER32' : Adler32Init(adler);
end;
end;

procedure init_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_obj);
'SHA512' : SHA512Init(HashContext_obj);
'SHA256' : SHA256Init(HashContext_obj);
'SHA3_512' : SHA3_512Init(HashContext_obj);
'SHA3_256' : SHA3_256Init(HashContext_obj);
'SHA1' : SHA1Init(HashContext_obj);
'BLAKE2S' : Blake2s_Init(Blake2sContext_obj,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext_obj,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_obj);
'MD5' : MD5Init(HashContext_obj);
'CRC64' : CRC64Init(crc64_obj);
'CRC32' : CRC32Init(crc32_obj);
'ADLER32' : Adler32Init(adler_obj);
end;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA3_512' : SHA3_512Init(HashContext_volume);
'SHA3_256' : SHA3_256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'BLAKE2S' : Blake2s_Init(Blake2sContext_volume,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(HashContext_volume,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_control_algo(var buf:array of byte; size:word);
var k:integer;
begin
case upcase(algo) of
'TRIATS':
begin
if FCS_EAX256_decrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCF_EAX256_decrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCA_EAX256_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'TRITSA':
begin
if FCA_EAX256_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCS_EAX256_decrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCF_EAX256_decrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'TRISAT':
begin
if FCF_EAX256_decrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCA_EAX256_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
if FCS_EAX256_decrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
end;
'EAX256' : if FCA_EAX256_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'TF256' : if FCF_EAX256_decrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'SP256' : if FCS_EAX256_decrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'EAX' : if FCA_EAX_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'TF' : if FCf_EAX_decrypt(cxf, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'SP' : if FCs_EAX_decrypt(cxs, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'HMAC' : if FCA_HMAC_decrypt(cxh, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'WHIRLPOOL' : Whirl_Update(HashContext, @buf, size);
'SHA512' : SHA512Update(HashContext, @buf, size);
'SHA256' : SHA256Update(HashContext, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext, @buf, size);
'SHA1' : SHA1Update(HashContext, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext,@buf,size);
'RIPEMD160' : RMD160Update(HashContext, @buf, size);
'MD5' : MD5Update(HashContext, @buf, size);
'CRC64' : CRC64Update(crc64, @buf, size);
'CRC32' : CRC32Update(crc32, @buf, size);
'ADLER32' : Adler32Update(adler, @buf, size);
end;
end;

procedure update_obj_control_algo(buf:array of byte; size:word);
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_obj, @buf, size);
'SHA512' : SHA512Update(HashContext_obj, @buf, size);
'SHA256' : SHA256Update(HashContext_obj, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_obj, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_obj, @buf, size);
'SHA1' : SHA1Update(HashContext_obj, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext_obj,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext_obj,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_obj, @buf, size);
'MD5' : MD5Update(HashContext_obj, @buf, size);
'CRC64' : CRC64Update(crc64_obj, @buf, size);
'CRC32' : CRC32Update(crc32_obj, @buf, size);
'ADLER32' : Adler32Update(adler_obj, @buf, size);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_volume, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext_volume,@buf,size);
'BLAKE2B' : Blake2b_update(HashContext_volume,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_control_algo;
begin
case upcase(algo) of
'TRIATS':
begin
FCA_EAX256_final(cxe, auth);
FCF_EAX256_final(cxf, auth2);
FCS_EAX256_final(cxs, auth3);
end;
'TRITSA':
begin
FCF_EAX256_final(cxf, auth);
FCS_EAX256_final(cxs, auth2);
FCA_EAX256_final(cxe, auth3);
end;
'TRISAT':
begin
FCS_EAX256_final(cxs, auth);
FCA_EAX256_final(cxe, auth2);
FCF_EAX256_final(cxf, auth3);
end;
'EAX256' : FCA_EAX256_final(cxe, auth);
'TF256' : FCf_EAX256_final(cxf, auth);
'SP256' : FCs_EAX256_final(cxs, auth);
'EAX' : FCA_EAX_final(cxe, auth);
'TF' : FCf_EAX_final(cxf, auth);
'SP' : FCs_EAX_final(cxs, auth);
'HMAC' : FCA_HMAC_final(cxh, auth);
'WHIRLPOOL' : Whirl_Final(HashContext,WHIRL512Digest);
'SHA512' : SHA512Final(HashContext,SHA512Digest);
'SHA256' : SHA256Final(HashContext,SHA256Digest);
'SHA3_512' : SHA3_512Final(HashContext,SHA3_512Digest);
'SHA3_256' : SHA3_256Final(HashContext,SHA3_256Digest);
'SHA1' : SHA1Final(HashContext,SHA1Digest);
'BLAKE2S' : blake2s_Final(Blake2sContext,Blake2sDigest);
'BLAKE2B' : blake2b_Final(HashContext,Blake2bDigest);
'RIPEMD160' : RMD160Final(HashContext,RMD160Digest);
'MD5' : MD5Final(HashContext,MD5Digest);
'CRC64' : CRC64Final(crc64);
'CRC32' : CRC32Final(crc32);
'ADLER32' : Adler32Final(adler);
end;
end;

procedure finish_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_obj,WHIRL512Digest_obj);
'SHA512' : SHA512Final(HashContext_obj,SHA512Digest_obj);
'SHA256' : SHA256Final(HashContext_obj,SHA256Digest_obj);
'SHA3_512' : SHA3_512Final(HashContext_obj,SHA3_512Digest_obj);
'SHA3_256' : SHA3_256Final(HashContext_obj,SHA3_256Digest_obj);
'SHA1' : SHA1Final(HashContext_obj,SHA1Digest_obj);
'BLAKE2S' : blake2s_Final(Blake2sContext_obj,Blake2sDigest_obj);
'BLAKE2B' : blake2b_Final(HashContext_obj,Blake2bDigest_obj);
'RIPEMD160' : RMD160Final(HashContext_obj,RMD160Digest_obj);
'MD5' : MD5Final(HashContext_obj,MD5Digest_obj);
'CRC64' : CRC64Final(crc64_obj);
'CRC32' : CRC32Final(crc32_obj);
'ADLER32' : Adler32Final(adler_obj);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA3_512' : SHA3_512Final(HashContext_volume,SHA3_512Digest_volume);
'SHA3_256' : SHA3_256Final(HashContext_volume,SHA3_256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'BLAKE2S' : blake2s_Final(Blake2sContext_volume,Blake2sDigest_volume);
'BLAKE2B' : blake2b_Final(HashContext_volume,Blake2bDigest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure authenticate_stream;
var
   k:dword;
   tag_match:boolean;
   ct384:THashContext;
   dg384:TSHA3_384Digest;
begin
if upcase(algo)<>'NOALGO' then
   begin
   for k:=0 to authsize-1 do exp_auth[k]:=sbuf1[k];
   case upcase(algo) of
      'TRIATS','TRITSA','TRISAT':
      begin
      for k:=0 to authsize-1 do sbuf1[k]:=auth[k];
      for k:=16 to 31 do sbuf1[k]:=auth2[k-16];
      for k:=32 to 47 do sbuf1[k]:=auth3[k-32];
      SHA3_384Init(ct384);
      SHA3_384Update(ct384, @sbuf1, 48);
      SHA3_384Final(ct384, dg384);
      for k:=0 to 47 do sbuf1[k]:=dg384[k];
      end;
      'EAX256','TF256','SP256','EAX','TF','SP','HMAC' : for k:=0 to authsize-1 do sbuf1[k]:=auth[k];
      'WHIRLPOOL' : for k:=0 to authsize-1 do sbuf1[k]:=WHIRL512Digest[k];
      'SHA512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA512Digest[k];
      'SHA256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA256Digest[k];
      'SHA3_512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA3_512Digest[k];
      'SHA3_256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA3_256Digest[k];
      'SHA1' : for k:=0 to authsize-1 do sbuf1[k]:=SHA1Digest[k];
      'BLAKE2S' : for k:=0 to authsize-1 do sbuf1[k]:=Blake2sDigest[k];
      'BLAKE2B' : for k:=0 to authsize-1 do sbuf1[k]:=Blake2bDigest[k];
      'RIPEMD160' : for k:=0 to authsize-1 do sbuf1[k]:=RMD160Digest[k];
      'MD5' : for k:=0 to authsize-1 do sbuf1[k]:=MD5Digest[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64.lo32,sbuf1,0);
      dword2bytebuf(crc64.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler,sbuf1,0);
      end;
   tag_match:=true;
   for k:=0 to authsize-1 do if sbuf1[k]<>exp_auth[k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=false then
      begin
      Form_pea.LabelDecrypt5.Caption:='The archive''s stream of data seem corrupted or tampered! You should not trust the stream''s content!';
      stream_error:=true;
      end
   else
      begin
      s:='';
      for k:=0 to authsize-1 do s:=s+hexstr(@sbuf1[k],1);
      if (upcase(algo)='TRIATS') or (upcase(algo)='TRITSA') or (upcase(algo)='TRISAT') or
      (upcase(algo)='EAX256') or (upcase(algo)='TF256') or (upcase(algo)='SP256') or
      (upcase(algo)='EAX') or (upcase(algo)='TF') or (upcase(algo)='SP') or (upcase(algo)='HMAC') then Form_pea.LabelDecrypt5.Caption:='Archive''s stream correctly authenticated, tag: '+s
      else Form_pea.LabelDecrypt5.Caption:='Archive''s stream correctly verified';
      end;
   end;
end;

procedure check_obj;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(obj_algo)<>'NOALGO' then
   begin
   for k:=0 to obj_authsize-1 do exp_obj_tags[nobj,k]:=sbuf1[k];
   case upcase(obj_algo) of
      'WHIRLPOOL' : for k:=0 to obj_authsize-1 do sbuf1[k]:=WHIRL512Digest_obj[k];
      'SHA512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA512Digest_obj[k];
      'SHA256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA256Digest_obj[k];
      'SHA3_512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA3_512Digest_obj[k];
      'SHA3_256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA3_256Digest_obj[k];
      'SHA1' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA1Digest_obj[k];
      'BLAKE2S' : for k:=0 to obj_authsize-1 do sbuf1[k]:=Blake2sDigest_obj[k];
      'BLAKE2B' : for k:=0 to obj_authsize-1 do sbuf1[k]:=Blake2bDigest_obj[k];
      'RIPEMD160' : for k:=0 to obj_authsize-1 do sbuf1[k]:=RMD160Digest_obj[k];
      'MD5' : for k:=0 to obj_authsize-1 do sbuf1[k]:=MD5Digest_obj[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_obj.lo32,sbuf1,0);
      dword2bytebuf(crc64_obj.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32_obj,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler_obj,sbuf1,0);
      end;
   for k:=0 to obj_authsize-1 do obj_tags[nobj,k]:=sbuf1[k];
   tag_match:=true;
   for k:=0 to obj_authsize-1 do if obj_tags[nobj,k]<>exp_obj_tags[nobj,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_objects[nobj]:='Object is OK'
   else
      begin
      status_objects[nobj]:='Wrong tag!';
      obj_error:=true;
      end;
   end;
end;

procedure check_volume;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   for k:=0 to volume_authsize-1 do exp_volume_tags[j-1,k]:=tagbuf[k];
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do tagbuf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA256Digest_volume[k];
      'SHA3_512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA3_512Digest_volume[k];
      'SHA3_256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA3_256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA1Digest_volume[k];
      'BLAKE2S' : for k:=0 to volume_authsize-1 do tagbuf[k]:=Blake2sDigest_volume[k];
      'BLAKE2B' : for k:=0 to volume_authsize-1 do tagbuf[k]:=Blake2bDigest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do tagbuf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do tagbuf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,tagbuf,0);
      dword2bytebuf(crc64_volume.hi32,tagbuf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,tagbuf,0);
      'ADLER32' : dword2bytebuf(adler_volume,tagbuf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=tagbuf[k];
   tag_match:=true;
   for k:=0 to volume_authsize-1 do if volume_tags[j-1,k]<>exp_volume_tags[j-1,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_volumes[j-1]:='Volume is OK'
   else
      begin
      status_volumes[j-1]:='Wrong tag!';
      volume_error:=true;
      end;
   end;
end;

procedure do_report_unPEA;
var
   h,k,obj_ok:dword;
   s:ansistring;
   system_datetimeencoding:byte;
begin
get_system_datetimeencoding(system_datetimeencoding);
Form_report.InputT.Caption:='Objects';
Form_report.OutputT.Caption:='Volumes';
Form_report.Caption:='Log UnPEA';
Form_report.StringGrid1.ColCount:=8;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.Cells[3,0]:='Age';
Form_report.StringGrid1.Cells[4,0]:='Attrib';
Form_report.StringGrid1.Cells[5,0]:='Attrib n.';
Form_report.StringGrid1.Cells[6,0]:='calculated ('+obj_algo+')';
Form_report.StringGrid1.Cells[7,0]:='found';
Form_report.StringGrid1.RowCount:=nobj+2;
obj_ok:=0;
for k:=0 to nobj do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=ansitoutf8(in_files[k]);
    Form_report.StringGrid1.Cells[1,k+1]:=status_objects[k];
    Form_report.StringGrid1.Cells[2,k+1]:=inttostr(fsizes[k]);
    if system_datetimeencoding=archive_datetimeencoding then
       begin
       try
          if ftimes[k]<>0 then Form_report.StringGrid1.Cells[3,k+1]:=datetimetostr(filedatetodatetime(ftimes[k]));
       except
          Form_report.StringGrid1.Cells[3,k+1]:='Non valid DateTime';
       end;
       end
    else Form_report.StringGrid1.Cells[3,k+1]:='DateTime conversion not available';
    Form_report.StringGrid1.Cells[4,k+1]:=fattr_dec[k];
    Form_report.StringGrid1.Cells[5,k+1]:=inttostr(fattr[k]);
    if upcase(obj_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to obj_authsize-1 do s:=s+hexstr(@obj_tags[k,h],1);
       Form_report.StringGrid1.Cells[6,k+1]:=s;
       s:='';
       for h:=0 to obj_authsize-1 do s:=s+hexstr(@exp_obj_tags[k,h],1);
       Form_report.StringGrid1.Cells[7,k+1]:=s;
       end;
    inc(obj_ok,1);
    end;
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=4;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:='Status';
Form_report.StringGrid2.Cells[2,0]:='calculated ('+volume_algo+')';
Form_report.StringGrid2.Cells[3,0]:='found';
Form_report.StringGrid2.RowCount:=j;
for k:=0 to j-2 do
    begin
    if singlevolume=false then update_pea_filename((in_name),k+1,s)
    else s:=(in_name);
    Form_report.StringGrid2.Cells[0,k+1]:=s;
    if upcase(volume_algo)<>'NOALGO' then
       begin
       Form_report.StringGrid2.Cells[1,k+1]:=status_volumes[k];
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[2,k+1]:=s;
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@exp_volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[3,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
Form_report.Label1.Caption:=in_folder+in_name+'.* -> '+out_path+out_file+DirectorySeparator;
Form_report.Label2.Caption:=Form_pea.LabelDecrypt4.Caption;
Form_report.Label3.Caption:='Input: '+inttostr(j-1)+' volume(s), '+inttostr(wrk_space)+' B -> Extracted '+inttostr(obj_ok)+' objects ('+inttostr(n_dirs)+' dirs, '+inttostr(obj_ok-n_dirs)+' files) of '+inttostr(n_input_files)+' ('+inttostr(n_input_files-obj_ok)+' not extracted); total output: '+inttostr(out_size)+' B';
Form_report.Label4.Caption:=Form_pea.LabelDecrypt5.Caption+' '+Form_pea.LabelDecrypt6.Caption
end;

//clean keying-related variables
procedure clean_keying_vars;
var
   k:integer;
begin
for k:=0 to pw_len-1 do sbuf2[k]:=0;
pw:='';
password:='';
keyfile_name:='';
keyf_name:='';
pw_len:=0;
k:=0;
end;

function report_errors:integer;
var
   s:ansistring;
begin
result:=0;
if (stream_error=false) and (obj_error=false) and (volume_error=false) then exit;
result:=-1;
s:='Error(s) found in: ';
if stream_error=true then s:=s+'stream; ';
if obj_error=true then s:=s+'object(s); ';
if volume_error=true then s:=s+'volume(s); ';
s:=s+'please check job log!';
MessageDlg(s, mtError, [mbOK], 0);
end;

begin
exitcode:=-1;
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=true;
Form_pea.PanelEncrypt1.visible:=false;
Form_pea.Caption:='UnPea';
ts_start:=datetimetotimestamp(now);
stream_error:=false;
obj_error:=false;
volume_error:=false;
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_qualified_name;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_param;
Form_pea.LabelTime1.Caption:='Opening archive...';
in_folder:=extractfilepath(in_qualified_name);
if in_folder='' then in_folder:=executable_path;
in_file:=extractfilename(in_qualified_name);
if upcase(copy(in_qualified_name,length(in_qualified_name)-10,11))<>'.000001.PEA' then
   begin
   singlevolume:=true;
   end
else
   begin
   singlevolume:=false;
   delete(in_file,length(in_file)-10,11);
   end;
in_name:=in_file;
//try to evaluate archive size (succeed if all chunks are accessible)
evaluate_archive_size(exp_space,cent_size);
//check output name and path
evaluate_output;
//try to check if the path has enough room for the output (formerly guessed archive size is used, actual output size is unknown unless all data is extracted and all headers are parsed)
setcurrentdir(extractfilepath(out_param));
if exp_space>diskfree(0) then
   if MessageDlg('Output path '+extractfilepath(out_param)+' seems to not have enough free space. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else halt(-3);
{blockread 10 byte archive header; since volume tag size is unknown to UnPEA,
PEA set first volume size mandatory at least 10 byte (plus volume tag) in order
to make UnPEA able to blockread the archive header and calculate the volume tag
size}
assignfile(f_in,in_qualified_name);
filemode:=0;
{$I-}reset(f_in);{$I+}
if IOResult<>0 then internal_error('IO error reading from '+in_qualified_name);
blockread (f_in,sbuf1,10,numread);
close(f_in);
test_pea_error('parsing archive header',pea_parse_archive_header(sbuf1,volume_algo,archive_datetimeencoding));
decode_volume_control_algo (volume_algo,volume_authsize);
//read 10 byte archive header plus 10 byte stream header plus other 16 byte crypto subheader (if AE is used) plus 4 byte for compression buffer size (if compression is used)
read_from_chunks ( in_folder,in_name,
                   40,
                   sbuf1,sbuf2,
                   volume_authsize,
                   40,
                   singlevolume);
for i:=0 to 22 do tagbuf[i]:=sbuf1[i]; //write plaintext header
for i:=0 to 29 do sbuf1[i]:=sbuf1[i+10]; //discard 10 byte of archive header
test_pea_error('parsing stream header',pea_parse_stream_header(sbuf1, compr, compr_level, algo, obj_algo));
decode_control_algo ( algo,
                      headersize,
                      authsize,
                      pwneeded);
if compr<>'PCOMPRESS0' then headersize:=headersize+14//stream header size + 10 (archive header size) + 4 (compression buffer field size, if compression is used)
else headersize:=headersize+10;
decode_obj_control_algo (obj_algo,obj_authsize);
for i:=0 to 19 do sbuf1[i]:=sbuf1[i+10]; //discard 10 bytes of stream header
if pwneeded=true then //initialize AE (appending headers to password)
   begin
   //read AE header
   case upcase(algo) of
   'TRIATS','TRITSA','TRISAT':
   begin
   case upcase(algo) of
      'TRIATS': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr));
      'TRITSA': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaderf(sbuf1,fhdr));
      'TRISAT': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaders(sbuf1,shdr));
      end;
   read_from_chunks ( in_folder,in_name,
                   56,
                   sbuf1,sbuf2,
                   volume_authsize,
                   56,
                   singlevolume);
   for i:=0 to 15 do sbuf1[i]:=sbuf1[i+36];
   case upcase(algo) of
      'TRIATS': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaderf(sbuf1,fhdr));
      'TRITSA': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaders(sbuf1,shdr));
      'TRISAT': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr));
      end;
   read_from_chunks ( in_folder,in_name,
                   72,
                   sbuf1,sbuf2,
                   volume_authsize,
                   72,
                   singlevolume);
   for i:=0 to 15 do sbuf1[i]:=sbuf1[i+52];
   case upcase(algo) of
      'TRIATS': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaders(sbuf1,shdr));
      'TRITSA': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr));
      'TRISAT': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaderf(sbuf1,fhdr));
      end;
   end;
   'EAX','EAX256': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr));
   'TF','TF256': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaderf(sbuf1,fhdr));
   'SP','SP256': test_pea_error('parsing crypto subheader',pea_parse_crypto_subheaders(sbuf1,shdr));
   else test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr))
   end;
   if (upcase(pw_param)='INTERACTIVE') or (upcase(pw_param)='INTERACTIVE_REPORT') then
      begin
      //password is pw string that was already entered in EditPW.Text
      //keyfile name is keyfile_name already entered
      end
   else
      begin
      pw:=password; //pw is got from commandline (not recommended)
      keyfile_name:=keyf_name; //keyfile name is got from command line
      end;
   pw_len:=length(pw);
   if pw_len=0 then internal_error('invalid password length');
   for k:=0 to pw_len-1 do sbuf2[k]:=ord(pw[k+1]);//copy password into an array of byte
   //append header to password's array (sbuf2)
   for k:=0 to 21 do sbuf2[pw_len+k]:=tagbuf[k];
   pw_len:=pw_len+22;
   //append keyfile to password's array (sbuf2)
   if upcase(keyfile_name)<>'NOKEYFILE' then
      test_pea_error('accessing keyfile',use_keyfile(keyfile_name,2048,numread,sbuf2,pw_len));
   //initialize AE
   if (upcase(algo)='TRIATS') or (upcase(algo)='TRITSA') or (upcase(algo)='TRISAT') or
      (upcase(algo)='EAX256') or (upcase(algo)='TF256') or (upcase(algo)='SP256') then init_AE256_control_algo
   else init_AE128_control_algo;
   clean_keying_vars;
   case upcase(algo) of
      'TRIATS','TRITSA','TRISAT': //remove masking of exact archive size, 1..128 byte of random data
      begin
      read_from_chunks ( in_folder,in_name,
                   328,
                   sbuf1,sbuf2,
                   volume_authsize,
                   328,
                   singlevolume);
      sbuf1[0]:=sbuf1[68];
      update_control_algo(sbuf1,1);
      storead:=sbuf1[0];
      if storead>0 then
         for i:=0 to storead-1 do sbuf1[i]:=sbuf1[69+i];
      update_control_algo(sbuf1,storead);
      headersize:=headersize+storead+1;
   end;
   end;
   if (upcase(algo)='TRIATS') or (upcase(algo)='TRITSA') or (upcase(algo)='TRISAT') then
      for i:=0 to 3 do sbuf1[i]:=sbuf1[i+69+storead]
   else
      for i:=0 to 3 do sbuf1[i]:=sbuf1[i+16]; //discard 16 bytes of crypto subheader
   storead:=0;
   end
//if AE is not used, initialize other control algorithms (and check headers)
else
   begin
   init_nonAE_control_algo;
   update_control_algo(tagbuf,20);//check the archive and stream headers
   end;
Form_pea.LabelDecrypt4.Caption:='Using: '+compr+', stream: '+algo+', objects: '+obj_algo+', volume(s): '+volume_algo;
out_created:=false;
if upcase(struct_param)='EXTRACT2DIR' then //save objects with shortest path in a dir with archive's name; actually this is the only output method allowed
   begin
   s:=out_file;
   j:=0;
   repeat
     if not(directoryexists(out_path+out_file)) and not(fileexists(out_path+out_file)) then
         try
         forcedirectories(out_path+out_file);
         out_created:=true;
         except
         out_file:=s+'output';
         out_created:=true;
         end
      else
         begin
         j:=j+1;
         out_file:=s+' - '+inttostr(j);
         if j=1000 then //to break recursivity if filename is not valid (ie unsupported character encoding)
            begin
            out_file:=s+'output';
            out_created:=true;
            end;
         end;
      {try //no longer works with Lazarus 0.9.30, exception is not returned
         mkdir(out_path+out_file);
         out_created:=true;
      except
         out_file:=s+' - '+inttostr(j);
         j:=j+1;
      end;}
      until out_created=true;
   setcurrentdir(out_param);
   end;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
//if compression is used, get compression buffer size; since at present revision level a single stream is included in an archive, the stream specific compression buffer size is read as first 4 bytes after the headers area
if compr<>'PCOMPRESS0' then
   begin
   update_control_algo(sbuf1,4);
   buf_size:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
   if buf_size<>WBUFSIZE then internal_error(inttostr(buf_size)+' is not a valid compression buffer size. Stream header is probably corrupted.')
   end;
// process the data
uncompsize:=0;
no_more_files:=false;
chunks_ok:=true;
readingstream:=true;
readingheader:=true;
readingfns:=false;
readingtrigger:=false;
readingfn:=false;
readingfs:=false;
readingfage:=false;
readingfattrib:=false;
readingcompsize:=false;
fassigned:=false;
readingf:=false;
readingcompblock:=false;
readingobjauth:=false;
readingauth:=false;
end_of_archive:=false;
addr:=0;
uncompsize:=0;
j:=1;
n_dirs:=0;
n_input_files:=0;
out_size:=0;
wrk_space:=0;
nobj:=-1;
finpre:='';
init_volume_control_algo;
while (chunks_ok=true) and (end_of_archive=false) do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file);
   repeat
      if fileexists(in_folder+in_file) then
         begin
         try
      chunks_ok:=true;
      if in_folder+in_file=finpre then internal_error('End Of Archive tag seems missing, the archive is probably corrupted.');
      finpre:=in_folder+in_file;
      assignfile(f_in,in_folder+in_file);
      filemode:=0;
      {$I-}reset(f_in);{$I+}
      if IOResult<>0 then internal_error('IO error opening '+in_folder+in_file);
      srcfilesize(in_folder+in_file,total);
      total:=total-volume_authsize;
      //total:=system.filesize(f_in)-volume_authsize;
      while ((total>0) and (readingheader=true)) do //read and discard archive and stream headers
         begin
         if total>headersize-addr then i:=headersize-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         inc(addr,numread);
         if addr>=headersize then
            begin
            addr:=0;
            readingheader:=false;
            readingfns:=true;
            end;
         end;
      1:
      while ((total>0) and (readingfns=true)) do //read filename size;
         begin
         if total>2-addr then i:=2-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=2 then
            begin
            readingfns:=false;
            addr:=0;
            if readingstream=true then
               begin
               init_obj_control_algo;
               update_control_algo(sbuf1,2);
               update_obj_control_algo(sbuf1,2);
               end;
            fns:=sbuf1[0] + (sbuf1[1] shl 8);
            if fns>SBUFSIZE then internal_error('Object name size exceeds '+inttostr(SBUFSIZE));
            {pathnames longer than SBUFSIZE (usually exceeding actual needs,
            SBUFSIZE is originally defined as 32KB), are considered errors}
            if fns=0 then readingtrigger:=true //read a trigger object
            else
               begin
               readingtrigger:=false;
               readingfn:=true;
               inc(nobj,1);
               end;
            end;
         end;
      while ((total>0) and (readingtrigger=true)) do //read 4 byte trigger;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingtrigger:=false;
            addr:=0;
            update_control_algo(sbuf1,4);
            if ((sbuf1[0]=69) and (sbuf1[1]=79) and (sbuf1[2]=65) and (sbuf1[3]=0)) then //EOA
               begin
               if authsize<>0 then readingauth:=true;
               end_of_archive:=true;
               end
            else internal_error('Unrecognized trigger object');
            end;
         end;
      while ((total>0) and (readingfn=true)) do //read object name;
         begin
         if total>fns-addr then i:=fns-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=fns then
            begin
            readingfn:=false;
            readingfage:=true;
            addr:=0;
            fn:='';
            update_control_algo(sbuf1,fns);
            update_obj_control_algo(sbuf1,fns);
            for k:=0 to fns-1 do fn:=fn+char(sbuf1[k]);
            SetLength(in_files,length(in_files)+1);
            SetLength(status_objects,length(in_files)+1);
            SetLength(fsizes,length(in_files)+1);
            SetLength(ftimes,length(in_files)+1);
            SetLength(fattr,length(in_files)+1);
            SetLength(fattr_dec,length(in_files)+1);
            SetLength(obj_tags,length(in_files)+1);
            SetLength(exp_obj_tags,length(in_files)+1);
            in_files[nobj]:=fn;
            end;
         end;
      while ((total>0) and (readingfage=true)) do //read file date and time of last modification;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingfage:=false;
            readingfattrib:=true;
            addr:=0;
            update_control_algo(sbuf1,4);
            update_obj_control_algo(sbuf1,4);
            fage:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
            ftimes[nobj]:=fage;
            end;
         end;
      while ((total>0) and (readingfattrib=true)) do //read file attributes;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingfattrib:=false;
            addr:=0;
            n_input_files:=n_input_files+1;
            update_control_algo(sbuf1,4);
            update_obj_control_algo(sbuf1,4);
            fattrib:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
            fattr[nobj]:=fattrib;
            dword2decodedFileAttributes(fattrib,fattr_dec[nobj]);
            if fassigned=false then
               begin
               //dodirseparators(fn);
               dodirseparators(fn);
               if upcase(struct_param)='EXTRACT2DIR' then
                  begin
                  ansiextract2dir;
                  if (total>0) and (fattrib and faDirectory <> 0) then //object is a dir
                     begin
                     n_dirs:=n_dirs+1;
                     readingobjauth:=true;
                     end;
                  end;
               end;
            end;
         end;
      while ((total>0) and (readingfs=true)) do //read file size;
         begin
         if total>8-addr then i:=8-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=8 then
            begin
            readingfs:=false;
            addr:=0;
            update_control_algo(sbuf1,8);
            update_obj_control_algo(sbuf1,8);
            qw0:=sbuf1[0];
            qw1:=sbuf1[1];
            qw2:=sbuf1[2];
            qw3:=sbuf1[3];
            qw4:=sbuf1[4];
            qw5:=sbuf1[5];
            qw6:=sbuf1[6];
            qw7:=sbuf1[7];
            qw0:=qw0;
            qw1:=qw1 *256;
            qw2:=qw2 *256*256;
            qw3:=qw3 *256*256*256;
            qw4:=qw4 *256*256*256*256;
            qw5:=qw5 *256*256*256*256*256;
            qw6:=qw6 *256*256*256*256*256*256;
            qw7:=qw7 *256*256*256*256*256*256*256;
            fs:=qw0+qw1+qw2+qw3+qw4+qw5+qw6+qw7;
            //fs:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24) + (sbuf1[4] shl 32) + (sbuf1[5] shl 40) + (sbuf1[6] shl 48) + (sbuf1[7] shl 56);
            out_size:=out_size+fs;
            fsizes[nobj]:=fs;
            if fs>0 then
               if compr<>'PCOMPRESS0' then readingcompsize:=true
               else readingf:=true
            else //object is an empty file
               begin
               closefile(f_out);
               fassigned:=false;
               readingobjauth:=true;
               end;
            end;
         end;
      if compr<>'PCOMPRESS0' then //use compression
         begin
         while ((total>0) and (readingcompsize=true)) do
            begin
            if total>4-addr then i:=4-addr else i:=total;
            try
            blockread (f_in,sbuf2,i,numread);
            except
            internal_error('IO error reading from '+in_folder+in_file);
            end;
            update_volume_control_algo(sbuf2,numread);
            dec(total,numread);
            inc(wrk_space,numread);
            for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
            inc(addr,numread);
            if addr>=4 then
               begin
               readingcompsize:=false;
               readingf:=true;
               addr:=0;
               update_control_algo(sbuf1,4);
               update_obj_control_algo(sbuf1,4);
               compsize:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
               end;
            end;
         while ((total>0) and (readingf=true)) do
            begin
            while ((total>0) and (addr<compsize+4)) do //read first compsize field for a compressed byte (buffer size was jet read)
               begin
               readingcompblock:=true;
               if total>compsize+4-addr then i:=compsize+4-addr else i:=total;
               try
               blockread (f_in,wbuf2,i,numread);
               except
               internal_error('IO error reading from '+in_folder+in_file);
               end;
               ci:=0;
               while ci<numread do
                  begin
                  if numread-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numread-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf2[ci+k];
                  update_volume_control_algo(sbuf1,cj);
                  inc(ci,cj);
                  end;
               dec(total,numread);
               inc(wrk_space,numread);
               for k:=0 to i-1 do wbuf1[addr+k]:=wbuf2[k];
               inc(addr,numread);
               if addr=compsize+4 then readingcompblock:=false;
               end;
            if readingcompblock=false then //read a compressed block sized compsize and next 4 byte (next block's compressed size, or uncompressed size for last block)
               begin
               addr:=0;
               ci:=0;
               while ci<compsize+4 do
                  begin
                  if compsize+4-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=compsize+4-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
                  update_control_algo(sbuf1,cj);
                  for k:=0 to cj-1 do wbuf1[ci+k]:=sbuf1[k];
                  inc(ci,cj);
                  end;
               if fs>buf_size then k:=buf_size else k:=fs;
               uncompsize:=k;
               if compsize<k then zuncompr.uncompress(@wbuf2[0], uncompsize, wbuf1[0], compsize)
               else wbuf2:=wbuf1;
               ci:=0;
               while ci<uncompsize do
                  begin
                  if uncompsize-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=uncompsize-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf2[ci+k];
                  update_obj_control_algo(sbuf1,cj);
                  inc(ci,cj);
                  end;
               try
               blockwrite (f_out,wbuf2,uncompsize,numwritten);
               except
               internal_error('IO error writing data');
               end;
               dec(fs,numwritten);
               compsize:=wbuf1[compsize]+(wbuf1[compsize+1] shl 8)+(wbuf1[compsize+2] shl 16)+(wbuf1[compsize+3] shl 24);
               if compsize>WBUFSIZE then internal_error('Decompression error, declared compsize bigger than compression buffer');
               dword2bytebuf(compsize,sbuf1,0);
               update_obj_control_algo(sbuf1,4);
               Form_pea.ProgressBar1.Position:=(wrk_space) div cent_size;
               Application.ProcessMessages;
               end;
            if fs=0 then //end of compressed file, control if uncompsize of last block matches to what expected
               begin
               if compsize<>uncompsize then internal_error('Decompression error, uncompressed size doesn''t match with expected size');
               closefile(f_out);
               fassigned:=false;
               readingf:=false;
               readingobjauth:=true;
               end;
            end;
         end
      else //no compression
         while ((total>0) and (readingf=true)) do
            begin
            if total>SBUFSIZE then i:=SBUFSIZE else i:=total;
            if fs>i then else i:=fs;
            try
            blockread (f_in,sbuf1,i,numread);
            except
            internal_error('IO error reading from '+in_folder+in_file);
            end;
            update_volume_control_algo(sbuf1,numread);
            dec(total,numread);
            inc(wrk_space,numread);
            dec(fs,numread);
            update_control_algo(sbuf1,numread);
            update_obj_control_algo(sbuf1,numread);
            try
            blockwrite (f_out,sbuf1,numread,numwritten);
            except
            internal_error('IO error writing data');
            end;
            Form_pea.ProgressBar1.Position:=(wrk_space) div cent_size;
            Application.ProcessMessages;
            if fs=0 then
               begin
               closefile(f_out);
               fassigned:=false;
               readingf:=false;
               readingobjauth:=true;
               end;
            end;
      //read object check field
      while ((total>0) and (readingobjauth=true)) do
         begin
         if obj_algo='NOALGO' then
            begin
            readingobjauth:=false;
            readingfns:=true;
            addr:=0;
            if total>0 then goto 1;
            end;
         if total>obj_authsize-addr then i:=obj_authsize-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=obj_authsize then
            begin
            update_control_algo(sbuf1,obj_authsize);
            readingobjauth:=false;
            readingfns:=true;
            addr:=0;
            finish_obj_control_algo;
            check_obj;
            if total>0 then goto 1;
            end;
         end;
      //read auth block (if any);
      while (total>0) and (readingauth=true) do
         begin
         if total>authsize-addr then i:=authsize-addr else i:=total;
         try
         blockread (f_in,sbuf2,i,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr=authsize then
            begin
            finish_control_algo;
            authenticate_stream;
            readingfns:=true;
            addr:=0;
            if total>0 then internal_error('Last volume seem to have wrong size');
            end;
         end;
      //read volume check block (if any);
      if (total=0) then
         begin
         SetLength(status_volumes,length(status_volumes)+1);
         SetLength(volume_tags,length(status_volumes)+1);
         SetLength(exp_volume_tags,length(status_volumes)+1);
         try
         blockread (f_in,tagbuf,volume_authsize,numread);
         except
         internal_error('IO error reading from '+in_folder+in_file);
         end;
         finish_volume_control_algo;
         check_volume;
         dec(total,numread);
         inc(wrk_space,numread);
         init_volume_control_algo;
         end;
      {$I-}close(f_in);{$I+}
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(j));
      j:=j+1;
      except
         try
            setcurrentdir(out_path);
            do_report_unpea;
            save_report('error log','txt',upcase(pw_param),out_path);
         except
         end;
      internal_error('Unexpected error working on volume '+inttostr(j)+'; data is either become non accessible or could be corrupted in a way that not allow the current implementation to extract data from the archive (in that case you should try to obtain a new copy of the archive). Tried to extract available output to: '+out_path+out_file+DirectorySeparator+' and to save the error report in: '+out_path);
      end;
      end
      else check_chunk(in_folder,j,chunks_ok);
   until (chunks_ok=true) or (end_of_archive=true);
   end;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, '+inttostr(j-1)+' volume(s), '+inttostr(wrk_space)+' B';
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
Form_pea.LabelDecrypt6.Caption:='Done '+struct_param+' on archive';
Form_pea.ProgressBar1.Position:=100;
setcurrentdir(out_path);
do_report_unpea;
timing(ts_start,wrk_space);
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path+out_file;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.ButtonPeaExit1.Visible:=false;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log UnPEA','txt',upcase(pw_param),out_path);
if report_errors =0 then
   begin
   exitcode:=0;
   sleep(500);
   if closepolicy>0 then Form_pea.Close;
   end
else exitcode:=-2;
end;

{
Raw File Split
Byte split a single input file in volumes of given size
In an optional separate .check file are saved error checking tags of each volume
The code is closely related to PEA, it's kept distinct for better readability
}

procedure rfs;
var
   out_param,volume_algo,in_qualified_name,pw_param:ansistring;
   ch_size:qword;
   volume_authsize:byte;

procedure parse_rfs_cl;
begin
try
   //output
   out_param:=(paramstr(2));
   //control chunk size
   if (upcase(paramstr(3))='ASK') then
      begin
      ch_size:=vol_size;
      volume_algo:=vol_algo;
      end
   else
      begin
      try
         ch_size:=strtoqword(paramstr(3));
         if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
      except
         internal_error('"'+paramstr(3)+'" is not a valid chunk size; values allowed are 1..2^64, 0 to don''t split the input');
      end;
      //get volume control algorithm
      volume_algo:=upcase(paramstr(4));
      end;
   if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
      internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
   //get operation mode
   pw_param:=upcase(paramstr(5));
   if (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter for RFS, please refer to the documentation');
   //input
   if (paramstr(6))<>'' then
      begin
      in_qualified_name:=(paramstr(6));
      if not fileexists(in_qualified_name) then
         internal_error('"'+in_qualified_name+'" file is not accessible');
      end
   else
      begin
      internal_error('No accessible input object found');
      end;
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_rfs_cl;
rfs_procedure(out_param,ch_size,volume_algo,volume_authsize,pw_param,in_qualified_name);
end;

procedure rfs_lib_procedure ( out_param:ansistring;                             //qualified name for output volumes (without .(volume number) suffix) or AUTONAME
                              ch_size:qword;                                    //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              volume_algo,                                      //algorithm for volume integrity check
                              in_qualified_name:ansistring;                     //qualified name of input file
                              opmode:ansistring);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:ansistring;
   volume_authsize:byte;
begin
//control chunk size
if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
//get volume control algorithm
if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
   internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
//input
if in_qualified_name='' then
   internal_error('No accessible input object found');
if not fileexists(in_qualified_name) then
   internal_error('"'+in_qualified_name+'" file is not accessible');
//get operation mode
if (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter for rfs_lib_procedure, please refer to the documentation');
pw_param:=upcase(opmode);
rfs_procedure(out_param,ch_size,volume_algo,volume_authsize,pw_param,in_qualified_name);
end;

procedure rfs_procedure ( out_param:ansistring;
                          ch_size:qword;
                          volume_algo:ansistring;
                          volume_authsize:byte;
                          pw_param:ansistring;
                          in_qualified_name:ansistring);
var
   HashContext_volume: THashContext;
   Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest_volume: TSHA512Digest;
   SHA256Digest_volume: TSHA256Digest;
   SHA3_512Digest_volume: TSHA3_512Digest;
   SHA3_256Digest_volume: TSHA3_256Digest;
   SHA1Digest_volume: TSHA1Digest;
   RMD160Digest_volume: TRMD160Digest;
   MD5Digest_volume: TMD5Digest;
   Blake2sContext:blake2s_ctx;
   Blake2sDigest:TBlake2sDigest;
   Blake2bContext:THashContext;
   Blake2bDigest:TBlake2bDigest;
   crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out,f_check:file of byte;
   sbuf1:array [0..65534] of byte;
   auth_buf:array [0..63] of byte;
   adler_volume,crc32_volume:longint;
   j,ch_number_expected,numread,num_res:dword;
   file_size,total,cent_size,prog_size,in_size,out_size,check_size,exp_size,ch_res:qword;
   out_file,out_path,out_name:ansistring;

procedure clean_variables;
begin
j:=0;
ch_number_expected:=0;
numread:=0;
num_res:=0;
file_size:=0;
total:=0;
cent_size:=0;
prog_size:=0;
in_size:=0;
out_size:=0;
check_size:=0;
exp_size:=0;
ch_res:=0;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA3_512' : SHA3_512Init(HashContext_volume);
'SHA3_256' : SHA3_256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'BLAKE2S' : Blake2s_Init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(Blake2bContext,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_volume, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext,@buf,size);
'BLAKE2B' : Blake2b_update(Blake2bContext,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA3_512' : SHA3_512Final(HashContext_volume,SHA3_512Digest_volume);
'SHA3_256' : SHA3_256Final(HashContext_volume,SHA3_256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'BLAKE2S' : blake2s_Final(Blake2sContext,Blake2sDigest);
'BLAKE2B' : blake2b_Final(Blake2bContext,Blake2bDigest);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure write_volume_check;
var k:dword;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do auth_buf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA256Digest_volume[k];
      'SHA3_512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA3_512Digest_volume[k];
      'SHA3_256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA3_256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA1Digest_volume[k];
      'BLAKE2S' : for k:=0 to volume_authsize-1 do auth_buf[k]:=Blake2sDigest[k];
      'BLAKE2B' : for k:=0 to volume_authsize-1 do auth_buf[k]:=Blake2bDigest[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do auth_buf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do auth_buf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,auth_buf,0);
      dword2bytebuf(crc64_volume.hi32,auth_buf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,auth_buf,0);
      'ADLER32' : dword2bytebuf(adler_volume,auth_buf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=auth_buf[k];
   blockwrite (f_check,auth_buf,volume_authsize);
   check_size:=check_size+volume_authsize;
   end;
end;

procedure write2chunks ( var num_res: dword;                     //amount of data to write
                         var sbuf1: array of byte;               //data buffer
                         var f_out:fileofbyte;                   //output file
                         var out_path,out_name: ansistring;      //name and path for the output;
                         var i: dword;                           //chunk progressive number
                         var ch_size:qword;                      //chunk size
                         var ch_res: qword);                     //residual space in the given chunk
var k,numwritten:dword;
    addr,buf:qword;
    out_file:ansistring;
begin
addr:=0;
numwritten:=0;
while num_res>0 do
   begin
   if num_res<=ch_res then
      begin
      try
      blockwrite (f_out,sbuf1,num_res,numwritten);
      except
      internal_error('IO error writing to volume '+inttostr(i));
      end;
      update_volume_control_algo(sbuf1,numwritten);
      num_res:=num_res-numwritten;
      ch_res:=ch_res-numwritten;
      addr:=0;
      end
   else
      begin
      SetLength(volume_tags,length(volume_tags)+1);
      try
      blockwrite (f_out,sbuf1,ch_res,numwritten);
      except
      internal_error('IO error writing to volume '+inttostr(i));
      end;
      update_volume_control_algo(sbuf1,numwritten);
      finish_volume_control_algo;
      try
      write_volume_check;
      except
      internal_error('IO error writing volume control tag to volume '+inttostr(i));
      end;
      {$I-}close(f_out);{$I+}
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(i));
      i:=i+1;
      update_rfs_filename(out_name,i,out_file);
      checkspace(out_path,ch_size);
      assignfile(f_out,out_path+out_file);
      {$I-}rewrite(f_out);{$I+} //it will overwrite orphaned files with same name to preserve name coherence
      if IOResult<>0 then internal_error('IO error opening volume '+inttostr(i));
      init_volume_control_algo;
      num_res:=num_res-numwritten;
      if num_res<ch_size then buf:=num_res else buf:=ch_size;
      addr:=addr+numwritten;
      for k:=0 to buf do sbuf1[k]:=sbuf1[addr+k];
      ch_res:=ch_size;
      end;
   end;
end;

procedure nocompress_file;
begin
while ((numread<>0) and (total<file_size)) do
   begin
   try
   blockread (f_in,sbuf1,SBUFSIZE,numread);
   except
   internal_error('IO error reading from '+in_qualified_name);
   end;
   inc(total,numread);
   inc(prog_size,numread);
   num_res:=numread;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
end;

procedure first_gui_output;
begin
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelEncrypt2.Caption:='Input: '+in_qualified_name;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_param;
Form_pea.LabelEncrypt4.Caption:='Integrity check algorithm: '+volume_algo;
Form_pea.LabelTime1.Caption:='Splitting file in volumes...';
Form_pea.Panel1.visible:=false;
Form_pea.LabelE1.Visible:=false;
end;

procedure evaluate_volumes;
begin
ch_number_expected:=(in_size div ch_size)+1;
if (exp_size mod ch_size)=0 then ch_number_expected:=ch_number_expected-1;
if ch_number_expected>9999 then
   if MessageDlg('Expected '+inttostr(ch_number_expected)+' volumes. It seems a lot! Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else halt(-3);
if ch_size<>1024*1024*1024*1024*1024 then Form_pea.LabelEncrypt5.Caption:='Expected '+inttostr(ch_number_expected)+' volume(s) of '+inttostr(ch_size+volume_authsize)+' B for a total output size of '+inttostr(exp_size)+' B'
else Form_pea.LabelEncrypt5.Caption:='Expected a single volume of '+inttostr(exp_size)+' B of size';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_qualified_name;
out_file:=extractfilename(out_param);
out_path:=extractfilepath(out_param);
if out_file='' then out_file:=extractfilename(in_qualified_name); //if no output name is explicitly given, the output name is assumed to be the name of the input file
if out_path='' then out_path:=extractfilepath(in_qualified_name); //if no output path is explicitly given, the output path is assumed to be the path of the input file
if out_path='' then out_path:=executable_path;
if setcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_file;
if exp_size>diskfree(0) then
   if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else halt(-3);
end;

procedure do_report_rfs;
var
   k,h:dword;
   s:ansistring;
begin
Form_report.InputT.Caption:='Input';
Form_report.OutputT.Caption:='Output';
Form_report.Caption:='Split file log';
Form_report.StringGrid1.ColCount:=3;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,1]:=in_qualified_name;
Form_report.StringGrid1.Cells[1,1]:='OK';
Form_report.StringGrid1.Cells[2,1]:=inttostr(file_size);
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=2;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:=volume_algo;
Form_report.StringGrid2.RowCount:=j+1;
for k:=0 to j-1 do
    begin
    Form_report.StringGrid2.Cells[0,k+1]:=inttostr(k+1);
    if upcase(volume_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[1,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
//operation parameters
Form_report.Label1.Caption:=Form_pea.LabelEncrypt4.Caption;
//input
Form_report.Label2.Caption:='Split '+in_qualified_name+'; input '+inttostr(file_size)+' B';
//output
Form_report.Label3.Caption:=Form_pea.LabelEncrypt6.Caption;
//output name
Form_report.Label4.Caption:=Form_pea.LabelEncrypt3.Caption;
end;

procedure last_gui_output;
begin
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_name+'.*';
out_size:=prog_size;
if ch_size<>1024*1024*1024*1024*1024 then Form_pea.LabelEncrypt6.Caption:=inttostr(j)+' volume(s) of '+inttostr(ch_size)+' B; total output '+inttostr(out_size)+' B'
else Form_pea.LabelEncrypt6.Caption:='Single volume archive of '+inttostr(out_size)+' B';
if upcase(volume_algo)<>'NOALGO' then Form_pea.LabelEncrypt6.Caption:=Form_pea.LabelEncrypt6.Caption+' + '+inttostr(check_size)+' B (check tags)';
do_report_rfs;
Form_pea.LabelEncrypt5.Caption:=Form_report.Label2.Caption;
Form_pea.LabelEncrypt4.Visible:=true;
Form_pea.LabelEncrypt5.Visible:=true;
Form_pea.LabelEncrypt6.Visible:=true;
end;

begin
exitcode:=-1;
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=false;
Form_pea.PanelEncrypt1.visible:=true;
Form_pea.Caption:='Split file';
ts_start:=datetimetotimestamp(now);
//give preliminary information on work status to the GUI
first_gui_output;
assignfile(f_in,in_qualified_name);
filemode:=0;
{$I-}reset(f_in);{$I+}
if IOResult<>0 then internal_error('IO error opening '+in_qualified_name);
srcfilesize(in_qualified_name,file_size);
//file_size:=system.filesize(f_in);
if file_size=0 then internal_error('The file is empty, cannot be split');
if ch_size>file_size then ch_size:=file_size;
cent_size:=(file_size div 100)+1; //1% of expected output size, used for progress indication
//evaluate volumes number;
//at 9999 objects the program will warn and proceed only after user's permission,
//however the program has no sort of problem until 999999 chunks (but the host
//system may!)
evaluate_volumes;
//get output path and name;
//evaluate if the path has enough free space for expected output.
evaluate_output;
//check if output path has room for a chunk of given size (mandatory)
checkspace(out_path,ch_size);
//start the actual operation routine
out_name:=out_file;
assignfile(f_out,out_file+'.001');//current dir was jet set to out_path
{$I-}rewrite(f_out);{$I+}
if IOResult<>0 then internal_error('IO error creating first output volume');
if upcase(volume_algo)<>'NOALGO' then
   begin
   assignfile(f_check,out_file+'.check');
   {$I-}rewrite(f_check);{$I+}
   if IOResult<>0 then internal_error('IO error creating .check file');
   rfs_create_checkfile_hdr(volume_algo,sbuf1);
   try
   blockwrite(f_check,sbuf1,4);
   except
   internal_error('IO error writing to .check file');
   end;
   check_size:=4;
   init_volume_control_algo;
   end;
j:=1;
//1) split file in chunks
total:=0;
numread:=1;
ch_res:=ch_size;
nocompress_file; //no compression
//last volume check
SetLength(volume_tags,length(volume_tags)+1);
finish_volume_control_algo;
write_volume_check;
if IOResult<>0 then internal_error('IO error writing last volume check');
{$I-}closefile(f_in);{$I+}
if IOResult<>0 then internal_error('IO error closing '+in_qualified_name);
{$I-}closefile(f_out);{$I+}
if IOResult<>0 then internal_error('IO error closing last output volume');
if upcase(volume_algo)<>'NOALGO' then
   begin
   {$I-}closefile(f_check);{$I+}
   if IOResult<>0 then internal_error('IO error closing .check file');
   end;
//give final job information to the GUI
last_gui_output;
//calculate operation time
timing(ts_start,out_size);
//make accessible exit button and link to the detailed job log
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.ButtonPeaExit.Visible:=false;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log Raw File Split','txt',upcase(pw_param),out_path);
exitcode:=0;
Sleep(500);
if closepolicy>0 then Form_pea.Close;
end;

{
Raw File Join
Byte join volumes with same name and progressive counter extension in a single
output file
Optionally error check each volume with information provided by a separate
.check file
The code is closely related to UnPEA, it's kept distinct for better readability
}

procedure rfj;
var
   in_qualified_name,out_param,pw_param:ansistring;

procedure parse_rfj_cl;
begin
try
   in_qualified_name:=(paramstr(2));
   if not(fileexists(in_qualified_name)) then
      internal_error('"'+in_qualified_name+'" not exist');
   //get operation mode
   pw_param:=upcase(paramstr(3));
   if (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter for RFJ, please refer to the documentation');
   out_param:=(paramstr(4));
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_rfj_cl;
rfj_procedure(in_qualified_name,pw_param,out_param);
end;

procedure rfj_lib_procedure ( in_qualified_name,                                //qualified name of first volume of the split file
                              out_param,                                        //qualified name to give to the output rejoined file (or AUTONAME)
                              opmode:ansistring);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:ansistring;
begin
if not(fileexists(in_qualified_name)) then
   internal_error('"'+in_qualified_name+'" not exist');
//get operation mode
if (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter for rfj_lib_procedure, please refer to the documentation');
pw_param:=upcase(opmode);
rfj_procedure(in_qualified_name,pw_param,out_param);
end;

procedure rfj_procedure ( in_qualified_name,
                          pw_param,
                          out_param:ansistring);
var
   HashContext_volume: THashContext;
   Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest_volume: TSHA512Digest;
   SHA256Digest_volume: TSHA256Digest;
   SHA3_512Digest_volume: TSHA3_512Digest;
   SHA3_256Digest_volume: TSHA3_256Digest;
   SHA1Digest_volume: TSHA1Digest;
   RMD160Digest_volume: TRMD160Digest;
   MD5Digest_volume: TMD5Digest;
   Blake2sContext:blake2s_ctx;
   Blake2sDigest:TBlake2sDigest;
   Blake2bContext:THashContext;
   Blake2bDigest:TBlake2bDigest;
   crc64_volume: TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out,f_check:file of byte;
   sbuf1:array [0..65534] of byte;
   tagbuf:array [0..63] of byte;
   volume_authsize:byte;
   adler_volume,crc32_volume:longint;
   i,j,numread,numwritten,n_chunks:dword;
   total,prog_size,wrk_space,exp_space:qword;
   chunks_ok,no_more_files,filenamed:boolean;
   in_file,in_name,in_folder,out_path,out_file,volume_algo:ansistring;

procedure clean_variables;
begin
i:=0;
j:=0;
numread:=0;
numwritten:=0;
n_chunks:=0;
total:=0;
prog_size:=0;
wrk_space:=0;
exp_space:=0;
end;

procedure evaluate_file_size(var exp_space:qword; var prog_size:qword); //succeed if all chunks are accessible
var qw:qword;
begin
j:=1;
no_more_files:=false;
exp_space:=0;
while no_more_files=false do
   begin
   update_rfs_filename(in_name,j,in_file);
   if fileexists(in_folder+in_file) then
      begin
      assignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      srcfilesize(in_folder+in_file,qw);
      exp_space:=exp_space+qw;
      //exp_space:=exp_space+system.filesize(f_in);
      closefile(f_in);
      j:=j+1;
      end
   else no_more_files:=true;
   end;
n_chunks:=j-1;
prog_size:=(exp_space div 100)+1;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, expected '+inttostr(n_chunks)+' volume(s), total '+inttostr(exp_space)+' B';
end;

procedure evaluate_output;
var
   k:integer;
   name_ok:boolean;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_folder+in_name;//the extension was already removed from in_file name
k:=0;
name_ok:=false;
repeat
   if k=0 then
      if fileexists(out_param) or directoryexists(out_param) then inc(k,1)
      else name_ok:=true
   else
      if fileexists(out_param+' - '+inttostr(k)+extractfileext(out_param)) or directoryexists(out_param+' - '+inttostr(k)+extractfileext(out_param)) then inc(k,1)
      else name_ok:=true;
until name_ok = true;
if k>0 then out_param:=out_param+' - '+inttostr(k)+extractfileext(out_param);
out_file:=extractfilename(out_param);
out_path:=extractfilepath(out_param);
if out_file='' then out_file:=extractfilename(in_qualified_name); //if no output name is explicitly given, the output name is assumed to be the name of the input file
if out_path='' then out_path:=extractfilepath(in_qualified_name); //if no output path is explicitly given, the output path is assumed to be the path of the input file
if out_path='' then out_path:=executable_path;
if setcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelDecrypt3.Caption:='Input: '+out_path+out_file;
if exp_space>diskfree(0) then
   if MessageDlg('Output path '+out_path+' seems to not have enough free space. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else halt(-3);
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA3_512' : SHA3_512Init(HashContext_volume);
'SHA3_256' : SHA3_256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'BLAKE2S' : Blake2s_Init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen);
'BLAKE2B' : Blake2b_Init(Blake2bContext,nil,0,BLAKE2B_MaxDigLen);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA3_512' : SHA3_512Update(HashContext_volume, @buf, size);
'SHA3_256' : SHA3_256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'BLAKE2S' : Blake2s_update(Blake2sContext,@buf,size);
'BLAKE2B' : Blake2b_update(Blake2bContext,@buf,size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA3_512' : SHA3_512Final(HashContext_volume,SHA3_512Digest_volume);
'SHA3_256' : SHA3_256Final(HashContext_volume,SHA3_256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'BLAKE2S' : blake2s_Final(Blake2sContext,Blake2sDigest);
'BLAKE2B' : blake2b_Final(Blake2bContext,Blake2bDigest);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure check_volume;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   for k:=0 to volume_authsize-1 do exp_volume_tags[j-1,k]:=tagbuf[k];
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do tagbuf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA256Digest_volume[k];
      'SHA3_512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA3_512Digest_volume[k];
      'SHA3_256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA3_256Digest_volume[k];
      'BLAKE2S' : for k:=0 to volume_authsize-1 do tagbuf[k]:=Blake2sDigest[k];
      'BLAKE2B' : for k:=0 to volume_authsize-1 do tagbuf[k]:=Blake2bDigest[k];
      'SHA1' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA1Digest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do tagbuf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do tagbuf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,tagbuf,0);
      dword2bytebuf(crc64_volume.hi32,tagbuf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,tagbuf,0);
      'ADLER32' : dword2bytebuf(adler_volume,tagbuf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=tagbuf[k];
   tag_match:=true;
   for k:=0 to volume_authsize-1 do if volume_tags[j-1,k]<>exp_volume_tags[j-1,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_volumes[j-1]:='Volume is OK'
   else status_volumes[j-1]:='Wrong tag!';
   end;
end;

procedure do_report_rfj;
var
   h,k:dword;
   s:ansistring;
begin
Form_report.InputT.Caption:='File';
Form_report.OutputT.Caption:='Volumes';
Form_report.Caption:='Log Raw File Join';
Form_report.StringGrid1.ColCount:=2;
Form_report.StringGrid1.Cells[0,0]:='File name';
Form_report.StringGrid1.Cells[1,0]:='Size (B)';
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,1]:=out_param;
Form_report.StringGrid1.Cells[1,1]:=inttostr(exp_space);
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=4;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:='Status';
Form_report.StringGrid2.Cells[2,0]:='calculated ('+volume_algo+')';
Form_report.StringGrid2.Cells[3,0]:='found';
if j>1 then Form_report.StringGrid2.RowCount:=j
else exit;
for k:=0 to j-2 do
    begin
    Form_report.StringGrid2.Cells[0,k+1]:=inttostr(k+1);
    if upcase(volume_algo)<>'NOALGO' then
       begin
       Form_report.StringGrid2.Cells[1,k+1]:=status_volumes[k];
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[2,k+1]:=s;
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@exp_volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[3,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
Form_report.Label1.Caption:=in_qualified_name+' -> '+out_param;
Form_report.Label2.Caption:=Form_pea.LabelDecrypt4.Caption;
Form_report.Label3.Caption:='Total output '+inttostr(wrk_space)+' B';
Form_report.Label4.Caption:='Joined '+inttostr(j-1)+' volume(s)';
end;

begin
exitcode:=-1;
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=true;
Form_pea.PanelEncrypt1.visible:=false;
Form_pea.Caption:='File join';
ts_start:=datetimetotimestamp(now);
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_qualified_name;
if extractfileext(in_qualified_name)<>'.001' then
  begin
  MessageDlg('Please select the file with .001 extension to start joining file parts', mtWarning, [mbOK], 0);
  exit;
  end;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_param;
Form_pea.LabelTime1.Caption:='Joining volumes...';
in_folder:=extractfilepath(in_qualified_name);
in_file:=extractfilename(in_qualified_name);
delete(in_file,length(in_file)-3,4);
in_name:=in_file;
//try to evaluate archive size (succeed if all chunks are accessible)
evaluate_file_size(exp_space,prog_size);
//evaluate output name and if output path has enough free space
evaluate_output;
// process the data
chunks_ok:=true;
wrk_space:=0;
Form_pea.ProgressBar1.Position:=5;
j:=0;
filenamed:=false;
repeat //avoid to overwrite files
   if j=0 then
      if fileexists(out_path+out_file) or directoryexists(out_path+out_file) then inc(j,1)
      else filenamed:=true
   else
      if fileexists(out_path+out_file+' - '+inttostr(j)+extractfileext(out_file)) or directoryexists(out_path+out_file+' - '+inttostr(j)+extractfileext(out_file)) then inc(j,1)
      else filenamed:=true;
   until filenamed = true;
if j>0 then out_file:=out_file+' - '+inttostr(j)+extractfileext(out_file);
assignfile(f_out,out_path+out_file);
{$I-}rewrite(f_out);{$I+}
if IOResult<>0 then internal_error('IO error creating output file '+out_path+out_file);
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file;
j:=1;
try
   assignfile(f_check,in_folder+in_name+'.check');
   filemode:=0;
   {$I-}reset(f_check);{$I+}
   if IOResult<>0 then internal_error('IO error opening check file '+in_folder+in_name+'.check');
   try
   blockread (f_check,sbuf1,4,numread);
   except
   internal_error('IO error reading from check file '+in_folder+in_name+'.check');
   end;
   if rfs_parse_archive_header (sbuf1,volume_algo)<>0 then volume_algo:='NOALGO';
except
   volume_algo:='NOALGO';
end;
decode_rfs_volume_control_algo (volume_algo, volume_authsize);
Form_pea.LabelDecrypt4.Caption:='Integrity check algorithm: '+volume_algo;
{
Since in raw split files there is no extra information about file termination,
the program will assume that the user had copied ALL needed volumes into the same
path
}
while chunks_ok=true do
   begin
   update_rfs_filename(in_name,j,in_file);
   if fileexists(in_folder+in_file) then
      begin
      init_volume_control_algo;
      chunks_ok:=true;
      assignfile(f_in,in_folder+in_file);
      filemode:=0;
      {$I-}reset(f_in);{$I+}
      if IOResult<>0 then internal_error('IO error opening input file '+in_folder+in_file);
      srcfilesize(in_folder+in_file,total);
      //total:=system.filesize(f_in);
      while (total>0) do
         begin
         if total>SBUFSIZE then i:=SBUFSIZE else i:=total;
         try
         blockread (f_in,sbuf1,i,numread);
         except
         internal_error('IO error reading from input file '+in_folder+in_file);
         end;
         update_volume_control_algo(sbuf1,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         try
         blockwrite (f_out,sbuf1,numread,numwritten);
         except
         internal_error('IO error writing to output file '+out_path+out_file);
         end;
         end;
      {$I-}close(f_in);{$I+}
      if IOResult<>0 then internal_error('IO error closing input file '+in_folder+in_file);
      //check volume
      SetLength(status_volumes,length(status_volumes)+1);
      SetLength(volume_tags,length(status_volumes)+1);
      SetLength(exp_volume_tags,length(status_volumes)+1);
      if upcase(volume_algo)<>'NOALGO' then blockread (f_check,tagbuf,volume_authsize,numread);
      finish_volume_control_algo;
      check_volume;
      j:=j+1;
      Form_pea.ProgressBar1.Position:=wrk_space div prog_size;
      Application.ProcessMessages;
      end
   else chunks_ok:=false;
   end;
{$I-}close(f_out);{$I+}
if IOResult<>0 then internal_error('IO error closing output file '+out_path+out_file);
if upcase(volume_algo)<>'NOALGO' then
   begin
   {$I-}closefile(f_check);{$I+}
   if IOResult<>0 then internal_error('IO error closing check file '+in_folder+in_name+'.check');
   end;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, got '+inttostr(j-1)+' volume(s), total '+inttostr(wrk_space)+' B';
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file;
Form_pea.LabelDecrypt5.Caption:='Volumes merged succesfully';
Form_pea.ProgressBar1.Position:=100;
setcurrentdir(extractfilepath(out_param));
do_report_rfj;
timing(ts_start,wrk_space);
Form_pea.LabelOpen.Caption:='Open';
output:=out_path+out_file;
Form_pea.LabelOpen.visible:=true;
Form_pea.LabelLog1.Visible:=true;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.ButtonPeaExit1.Visible:=false;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log Raw File Join','txt',upcase(pw_param),out_path);
exitcode:=0;
Sleep(500);
if closepolicy>0 then Form_pea.Close;
end;

//procedure to wipe files and folders
procedure wipe ( level: ansistring);
//NONE: delete (quick delete: no overwrite, not sent to recycle bin)
//QUICK: alias for NONE
//RECYCLE: move to recycle bin (Windows and macOS)
//ZERO: overwrite with zero, flush, delete
//ONE: overwrite with one, flush, delete
//VERY_FAST: overwrite with random data, flush, delete
//FAST: 2 * overwrite with random data, flush, delete
//MEDIUM: zero delete, one delete, random data overwrite, flush, mask file size <4KB, 3 * (rename, flush), delete
//SLOW: zero delete, one delete, 2 * (random data overwrite, flush), mask file size <4KB*2, 4 * (rename, flush), delete
//VERY_SLOW: zero delete, one delete, 3 * (random data overwrite, flush), mask file size <4KB*3, 5 * (rename, flush), delete
var
   f:file of byte;
   exp_files:TFoundList;
   exp_dirs:TFoundList;
   exp_fsizes:TFoundListSizes;
   exp_ftimes:TFoundListAges;
   exp_fattr:TFoundListAttrib;
   exp_fattr_dec:TFoundList;
   nfound,size,total,ntotalexp,tsize,etsize,nfiles,ndirs,ctsize,speed,numread:qword;
   i,j,k,errors,dfiles,ddirs,nlevel,nleveli,rc,attr,time,numwritten:integer;
   buf:array[0..65534]of byte;
   aes_ctx:TAESContext;
   aes_iv:array[0..15]of byte;
   randomstring,randomstring2,oldrandomstring,in_param,s,end2caption:ansistring;
   sr:TSearchRec;
   tsin,tsout:TTimestamp;

procedure canceldelete;
begin
Form_pea.LabelTools4.Caption:='Operation cancelled by user';
Application.ProcessMessages;
if errors=0 then
   begin
   Sleep(1500);
   if closepolicy>0 then Form_pea.Close;
   end;
end;

procedure wipefixed(b:byte);
begin
assignfile(f,(exp_files[k]));
rewrite(f);
total:=0;
FillByte(buf,sizeof(buf),b);
repeat
   if size-total>65536 then numread:=65536
   else numread:=size-total;
   blockwrite(f,buf,numread,numwritten);
   etsize:=etsize+numread;
   Form_pea.ProgressBar1.Position:=(100*etsize) div tsize;
   tsout:=datetimetotimestamp(now);
   time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
   Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' elapsed';
   Application.ProcessMessages;
   if toolactioncancelled=true then
      begin
      closefile(f);
      try udeletefile(exp_files[k]); except end;
      canceldelete;
      exit;
      end;
   inc(total,numwritten);
until (total>=size);
closefile(f);//causes flush;
end;

{$IFDEF MSWINDOWS}
function recyclefile_fromname(fname:ansistring):integer;
var
   FStruct: TSHFileOpStruct;
   fnamearr: array[0..255] of char;
begin
//file already checked when the function is called
fillchar(fnamearr,sizeof(fnamearr),0) ;
StrPcopy(fnamearr,expandfilename(fname)+#0#0) ;
FStruct.hwnd:=0;
FStruct.wFunc:=FO_DELETE;
FStruct.pFrom:=fnamearr;
FStruct.pTo:=nil;
FStruct.fFlags:= FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT;
FStruct.fAnyOperationsAborted := false;
FStruct.hNameMappings := nil;
Result:=ShFileOperation(FStruct);
end;
{$ENDIF}

{$IFDEF DARWIN}
procedure mac_movetotrash(itemtodelete:AnsiString);
var
  aNSArray : NSMutableArray;
  aNSURL   : NSURL;
begin
  aNSArray := NSMutableArray(NSMutableArray.array_).init;
  aNSURL   := NSURL.fileURLWithPath(NSSTR(pchar(itemtodelete)));
  aNSArray.addObject(aNSURL);
  NSWorkspace.sharedWorkspace.recycleURLs_completionHandler(aNSArray,nil);
end;
{$ENDIF}

begin
exitcode:=-1;
tsin:=datetimetotimestamp(now);
Form_pea.PanelPW1.height:=2;
Form_pea.ButtonToolsCancel.visible:=true;
Form_pea.ButtonToolsCancel.hint:='Cancel will stop deletion but will not recover already deleted elements';
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
level:=upcase(level);
if (level='NONE') or (level='QUICK') or (level='RECYCLE') or (level='HEADER') then
   Form_report.Caption:='Delete'
else
   Form_report.Caption:='Secure delete';
Form_pea.Caption:=Form_report.Caption;
Form_pea.LabelTools2.Caption:=Form_report.Caption+' ('+level+'), '+inttostr(paramcount-2)+' element(s)';
Form_pea.ProgressBar1.Position:=0;
Form_pea.PanelTools.Cursor:=crHourGlass;
errors:=0;
dfiles:=0;
ddirs:=0;
ntotalexp:=paramcount-2;
case level of
   'NONE' : nlevel:=0;
   'QUICK' : nlevel:=0;
   'RECYCLE' : nlevel:=0;
   'HEADER' : nlevel:=1;
   'ZERO' : nlevel:=1;
   'ONE' : nlevel:=1;
   'VERY_FAST' : nlevel:=1;
   'FAST' : nlevel:=2;
   'MEDIUM' : nlevel:=3;
   'SLOW' : nlevel:=4;
   else nlevel:=5;
   end;
nleveli:=nlevel;
tsize:=0;
etsize:=0;
ctsize:=0;
for j:=3 to paramcount do
   begin
   if filegetattr((paramstr(j))) and faDirectory = 0 then
      srcfilesize((paramstr(j)),ctsize)
   else
      rcountsize((paramstr(j))+directoryseparator,'*',faAnyFile,true,nfiles,ndirs,ctsize);
   tsize:=tsize+ctsize;
   end;
Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+', '+nicenumber(inttostr(tsize),0);
end2caption:=Form_pea.LabelTools2.Caption;
Application.ProcessMessages;
tsize:=(tsize*nlevel) + paramcount;
randomize;
for j:=3 to paramcount do
   begin
   if Form_pea.ProgressBar1.Position>=100 then Form_pea.ProgressBar1.Position:=99;
   tsout:=datetimetotimestamp(now);
   time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
   Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
   Application.ProcessMessages;
   try
      if level='RECYCLE' then
               begin
               {$IFDEF MSWINDOWS}//recycle (Windows)
               in_param:=escapefilename(paramstr(j),desk_env);
               findfirst(in_param, faAnyFile, sr);
               s := StrPas(sr.FindData.cAlternateFileName);
               if s='' then s:= extractfilename(in_param);
               s := extractfilepath(in_param) + s;
               FindClose(sr);
               recyclefile_fromname(s);
               {$ENDIF}
               {$IFDEF DARWIN}//move to trash (macOS)
               in_param:=escapefilename(paramstr(j),desk_env);
               findfirst(in_param, faAnyFile, sr);
               s:= extractfilename(in_param);
               s := extractfilepath(in_param) + s;
               FindClose(sr);
               mac_movetotrash(s);
               {$ENDIF}
               end
      else
      begin
      expand((paramstr(j)),exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,nfound);
      if nfound=0 then nfound:=1;
      SetLength(exp_dirs,0);
      ntotalexp:=ntotalexp+nfound-1;
      for k:=0 to nfound-1 do
         begin
         if filegetattr(exp_files[k]) and faDirectory = 0 then //file
            begin
            rc:=Form_report.StringGrid1.RowCount+1;
            Form_report.StringGrid1.RowCount:=rc;
            if nlevel>0 then Form_pea.LabelTools2.Caption:=end2caption+', '+nicenumber(inttostr(etsize div nlevel),0)+' deleted'
            else Form_pea.LabelTools2.Caption:=end2caption;
            Form_pea.LabelTools3.Caption:='Processing item '+inttostr(rc-1)+' of '+inttostr(ntotalexp)+' found';
            application.ProcessMessages;
            try
               {$IFDEF MSWINDOWS}
               upredeletefile(exp_files[k]);
               {$ENDIF}
               assignfile(f,exp_files[k]);
               filemode:=0;
               reset(f);
               srcfilesize(exp_files[k],size);
               closefile(f);
               Form_pea.LabelTools3.Caption:=Form_pea.LabelTools3.Caption+', '+nicenumber(inttostr(size),0)+' file';
               setcurrentdir(extractfilepath((exp_files[k])));
               if toolactioncancelled=true then
                  begin
                  canceldelete;
                  exit;
                  end;
               case level of
               'NONE': udeletefile(exp_files[k]);//quick delete
               'QUICK': udeletefile(exp_files[k]);//quick delete (alias)
               'ZERO': //overwrite with zero
                  begin
                  wipefixed(0);
                  udeletefile(exp_files[k]);
                  end;
               'ONE': //overwrite with one
                  begin
                  wipefixed(255);
                  udeletefile(exp_files[k]);
                  end;
               else // secure delete (and header quick delete)
               begin
               get_fingerprint(fingerprint,false);
               //init encryption
               for i:=0 to 31 do fingerprint[i]:=fingerprint[i];
               for i:=0 to 15 do aes_iv[i]:=fingerprint[i]+random(256);
               AES_CTR_Init(fingerprint, 256, aes_iv, aes_ctx);
               if nlevel>2 then
                  begin
                  nleveli:=nlevel-2;
                  wipefixed(0);
                  sleep(random(250));
                  wipefixed(255);
                  sleep(random(250));
                  end;
               for i:=1 to nleveli do //overwrite nlevel times with random data (AES256 CTR init once by system fingerprint)
                  begin
                  assignfile(f,exp_files[k]);
                  rewrite(f);
                  total:=0;
                  repeat
                     if size-total>65536 then numread:=65536
                     else numread:=size-total;
                     AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
                     blockwrite(f,buf,numread,numwritten);
                     etsize:=etsize+numread;
                     Form_pea.ProgressBar1.Position:=(100*etsize) div tsize;
                     tsout:=datetimetotimestamp(now);
                     time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
                     Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
                     Application.ProcessMessages;
                     if toolactioncancelled=true then
                        begin
                        closefile(f);
                        try udeletefile(exp_files[k]); except end;
                        canceldelete;
                        exit;
                        end;
                     inc(total,numwritten);
                     if level='HEADER' then begin etsize:=etsize+size-numread; break; end;//overwrite only file header up to 64KB
                  until (total>=size);
                  closefile(f);//causes flush;
                  if nleveli>1 then sleep(random(250));
                  end;
               if nlevel>2 then
                  begin
                  numread:=1+random(4096*i);//replace file with random sized block 1B-(4KB*i) to mask original size
                  AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
                  assignfile(f,(exp_files[k]));
                  rewrite(f);
                  blockwrite(f,buf,numread,numwritten);
                  closefile(f);
                  randomstring:=(exp_files[k]);
                  for i:=1 to nleveli do //rename
                     begin
                     oldrandomstring:=randomstring;
                     assignfile(f,randomstring);
                     randomstring:=extractfilepath(randomstring)+inttostr(random(maxint))+'.tmp';
                     renamefile(oldrandomstring,randomstring);
                     end;
                  udeletefile(randomstring);
                  end
               else udeletefile(exp_files[k]);
               end;
               end;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='file successfully deleted';
               dfiles:=dfiles+1;
            except
               try closefile(f); except end;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='<ERROR: FILE NOT DELETED (not writeable/accessible/found)>';
               errors:=errors+1;
            end;
            end
         else
            begin
            if not(fileexists(exp_files[k])) and not(directoryexists(exp_files[k])) then //not found
               begin
               rc:=Form_report.StringGrid1.RowCount+1;
               Form_report.StringGrid1.RowCount:=rc;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='OBJECT NOT FOUND';
               errors:=errors+1;
               end
            else
               begin
               SetLength(exp_dirs,length(exp_dirs)+1);
               exp_dirs[length(exp_dirs)-1]:=exp_files[k];
               end;
            end;
      end;
      setcurrentdir(executable_path);
      if length(exp_dirs)>0 then
         for k:=(length(exp_dirs)-1) downto 0 do
            begin
            rc:=Form_report.StringGrid1.RowCount+1;
            Form_report.StringGrid1.RowCount:=rc;
            Form_pea.LabelTools3.Caption:='Processing item '+inttostr(rc-1)+' of '+inttostr(ntotalexp)+', directory';
            Application.ProcessMessages;
            try
               randomstring:=(exp_dirs[k]);
               for i:=1 to nlevel do //rename
                  begin
                  if randomstring[length(randomstring)]=directoryseparator then setlength(randomstring,length(randomstring)-1);
                  randomstring2:=extractfilepath(randomstring)+inttostr(random(maxint));
                  renamefile(randomstring,randomstring2);
                  randomstring:=randomstring2;
                  end;
               removedir(randomstring);
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_dirs[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='directory successfully deleted';
               ddirs:=ddirs+1;
            except
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_dirs[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='DIRECTORY NOT DELETED (not writeable/accessible/found)';
               errors:=errors+1;
            end;
            end;
      end;
    except
       rc:=Form_report.StringGrid1.RowCount+1;
       Form_report.StringGrid1.RowCount:=rc;
       Form_report.StringGrid1.Cells[0,Form_report.StringGrid1.RowCount-1]:=(paramstr(j));
       Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='OBJECT NOT DELETED';
       errors:=errors+1;
   end;
   end;
Form_pea.ButtonToolsCancel.visible:=false;
Form_pea.ProgressBar1.Position:=100;
Form_pea.PanelTools.Cursor:=crDefault;
Form_report.StringGrid1.Cells[0,0]:='File';
Form_report.StringGrid1.Cells[1,0]:='Result';
Form_report.StringGrid1.AutosizeColumns;
Form_pea.LabelTools2.Caption:=end2caption;
if level='RECYCLE' then
   Form_pea.LabelTools3.Caption:=''
else
   Form_pea.LabelTools3.Caption:='Processed '+inttostr(dfiles)+' files, '+inttostr(ddirs)+' directories, '+inttostr(errors)+' errors';
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time>0 then
   begin
   speed:=(tsize * 1000) div time;
   Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' total time @ '+nicenumber(inttostr(speed),0)+'/s';
   end
else Form_pea.LabelTools4.Caption:='';
Form_report.Label1.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Application.ProcessMessages;
if errors=0 then
   begin
   exitcode:=0;
   Sleep(1500);
   if closepolicy>0 then Form_pea.Close;
   end
else exitcode:=-2;
end;

//procedure to wipe/sanitize free space: write files smaller than 2GB then delete
procedure sanitize ( level: ansistring);
//ZERO: overwrite free space with zero, flush
//ONE: overwrite free space with one, flush
//VERY_FAST: 1 * (random data overwrite filling free space, flush, delete work files)
//FAST: 2 * (random data overwrite filling free space, flush, delete work files)
//MEDIUM: zero delete, one delete, 1 * (random data overwrite filling free space, flush, delete work files)
//SLOW: zero delete, one delete, 2 * (random data overwrite filling free space, flush, delete work files)
//VERY_SLOW: zero delete, one delete, 3* (random data overwrite filling free space, flush, delete work files)

var
   f:file of byte;
   total,gtotal,maxs:qword;
   n,i,j,numread,numwritten,nlevel,nleveli,rc,drivenumber,time:integer;
   buf:array[0..65534]of byte;
   aes_ctx:TAESContext;
   aes_iv:array[0..15]of byte;
   wrkfile,wrkdir,wrktitle,fstype,sdrive,wincomspec,winver,majmin:ansistring;
   bufVolumeName, bufFSName: array[0..255] of Char;
   sn,mc,flags:dword;
   sizefree,sizetotal,rfree,speed:qword;
   tok:boolean;
   tsin,tsout:TTimestamp;

procedure recoverfreespace(n:integer);
var m:integer;
begin
try
   for m:=1 to n do udeletefile(wrkdir+directoryseparator+inttostr(m));
except
   sleep(500);
   try
      for m:=1 to n do udeletefile(wrkdir+directoryseparator+inttostr(m));
   except
      MessageDlg('Cannot delete temporary work files, please manually delete '+wrkdir+directoryseparator+' to recover free space', mtWarning, [mbOK], 0);
   end;
end;
end;

procedure cancelsanitize(n:integer);
begin
Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' operation cancelled by user, recovering free space from '+wrkdir+' ...';
Application.ProcessMessages;
recoverfreespace(n);
removedir(wrkdir);
sleep(1500);
halt(-4);
end;

procedure sanitizefixed(b:byte;j:integer);
begin
   FillByte(buf,sizeof(buf),b);
   tok:=false;
   gtotal:=0;
   n:=0;
   repeat
      total:=0;
      n:=n+1;
      wrkfile:=wrkdir+directoryseparator+inttostr(n);
      assignfile(f,wrkfile);
      rewrite(f);
      try
      repeat
         if maxs-total>65536 then numread:=65536
         else numread:=maxs-total;
         rfree:=diskfree(drivenumber);
         Form_pea.ProgressBar1.Position:=100-((rfree*100) div sizefree);
         Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree),0)+' free, '+nicenumber(inttostr(rfree),0)+' remaining';
         tsout:=datetimetotimestamp(now);
         time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
         Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
         Application.ProcessMessages;
         if toolactioncancelled=true then begin closefile(f); cancelsanitize(n); exit; end;
         if rfree<=65536 then tok:=true
         else
            begin
            blockwrite(f,buf,numread,numwritten);
            inc(total,numwritten);
            end;
      until (tok=true) or (total>=maxs);
      finally
         closefile(f);//causes flush;
         inc(gtotal,total);
      end;
   until tok=true;
   recoverfreespace(n);
   Form_pea.LabelTools2.Caption:='Done drive '+(paramstr(3))+', '+nicenumber(inttostr(sizetotal),0)+', pass '+inttostr(j)+' of '+inttostr(nlevel);
   end;

begin
exitcode:=-1;
{$IFDEF MSWINDOWS}
Form_pea.ButtonToolsCancel.visible:=true;
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
level:=upcase(level);
case level of
   'ZERO' : wrktitle:='Zero delete free space';
   'ONE' : wrktitle:='One delete free space';
   else wrktitle:='Secure delete free space ('+level+')';
   end;
Form_report.Caption:=wrktitle;
Form_pea.Caption:=wrktitle;
Form_pea.ProgressBar1.Position:=0;
Form_pea.PanelTools.Cursor:=crHourGlass;
case level of
   'ZERO' : nlevel:=1;
   'ONE' : nlevel:=1;
   'VERY_FAST' : nlevel:=1;
   'FAST' : nlevel:=2;
   'MEDIUM' : nlevel:=3;
   'SLOW' : nlevel:=4;
   else nlevel:=5;
   end;
nleveli:=nlevel;
randomize;
Form_report.StringGrid1.RowCount:=1;
Application.ProcessMessages;
drivenumber:=ord(upcase(paramstr(3)[1]))-64;
if drivenumber>2 then sizefree:=diskfree(drivenumber);
if drivenumber>2 then sizetotal:=DiskSize(drivenumber);
maxs:=2*1024*1024*1024-1;//2GiB-1B
wrkdir:=paramstr(3)+directoryseparator+'.ptmp';
forcedirectories(wrkdir);

tsin:=datetimetotimestamp(now);

getwinenvadv(wincomspec,winver,majmin);
if (winver='nt6+') or (winver='nt5') then
   begin
   sdrive:=extractfiledrive((paramstr(3)))+'\';
   GetVolumeInformation(Pchar(sdrive),
   @bufVolumeName, sizeof(bufVolumeName),
   @sn, mc, flags,
   @bufFSName, sizeof(bufFSName));
   fstype:=bufFSName;
   end
else fstype:='';

   if fstype<>'' then
      Form_pea.LabelTools2.Caption:='Processing drive '+(paramstr(3))+' ('+fstype+'), '+nicenumber(inttostr(sizetotal),0)
   else
      Form_pea.LabelTools2.Caption:='Processing drive '+(paramstr(3))+', '+nicenumber(inttostr(sizetotal),0);
Application.ProcessMessages;
case level of
   'ZERO': //overwrite with zero
   sanitizefixed(0,1);
   'ONE': //overwrite with one
   sanitizefixed(255,1);
   else // secure delete
   begin
   get_fingerprint(fingerprint,false);
   //init encryption
   for i:=0 to 31 do fingerprint[i]:=fingerprint[i];
   for i:=0 to 15 do aes_iv[i]:=fingerprint[i]+random(256);
   AES_CTR_Init(fingerprint, 256, aes_iv, aes_ctx);
   //overwrite nlevel times with random data (AES256 CTR init once by system fingerprint)
   if nlevel>2 then
      begin
      nleveli:=nleveli-2;
      sanitizefixed(0,1);
      sanitizefixed(255,2);
      end;
   for j:=1 to nleveli do
   begin

   if fstype<>'' then
      Form_pea.LabelTools2.Caption:='Processing drive '+(paramstr(3))+' ('+fstype+'), '+nicenumber(inttostr(sizetotal),0)+', pass '+inttostr(j+nlevel-nleveli)+' of '+inttostr(nlevel)
   else
      Form_pea.LabelTools2.Caption:='Processing drive '+(paramstr(3))+', '+nicenumber(inttostr(sizetotal),0)+', pass '+inttostr(j+nlevel-nleveli)+' of '+inttostr(nlevel);
   Application.ProcessMessages;

   tok:=false;
   gtotal:=0;
   n:=0;
   repeat
      total:=0;
      n:=n+1;
      wrkfile:=wrkdir+directoryseparator+inttostr(n);
      assignfile(f,wrkfile);
      rewrite(f);
      try
      repeat
         if maxs-total>65536 then numread:=65536
         else numread:=maxs-total;
         rfree:=diskfree(drivenumber);
         Form_pea.ProgressBar1.Position:=100-((rfree*100) div sizefree);
         Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree),0)+' free, '+nicenumber(inttostr(rfree),0)+' remaining';
         tsout:=datetimetotimestamp(now);
         time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
         Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
         Application.ProcessMessages;
         if toolactioncancelled=true then begin closefile(f); cancelsanitize(n); exit; end;
         if rfree<=65536 then tok:=true
         else
            begin
            AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
            blockwrite(f,buf,numread,numwritten);
            inc(total,numwritten);
            end;
      until (tok=true) or (total>=maxs);
      finally
         closefile(f);//causes flush;
         inc(gtotal,total);
      end;
   until tok=true;
   recoverfreespace(n);
   Form_pea.LabelTools2.Caption:='Done drive '+(paramstr(3))+', '+nicenumber(inttostr(sizetotal),0)+', pass '+inttostr(j+nlevel-nleveli)+' of '+inttostr(nlevel);
   end;
   end;
end;
Form_pea.ButtonToolsCancel.visible:=false;
removedir(wrkdir);
Form_pea.ProgressBar1.Position:=100;
Form_pea.PanelTools.Cursor:=crDefault;
Form_report.StringGrid1.Cells[0,0]:='File';
Form_report.StringGrid1.Cells[1,0]:='Result';
Form_report.StringGrid1.AutosizeColumns;
rfree:=diskfree(drivenumber);
Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree),0)+' free when task started, '+nicenumber(inttostr(rfree),0)+' currently free, temporary work dir '+wrkdir;
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time>0 then
   begin
   speed:=(sizefree * 1000) div time;
   Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' total time @ '+nicenumber(inttostr(speed),0)+'/s';
   end
else Form_pea.LabelTools4.Caption:='';
Form_report.Label1.Caption:=wrktitle;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Application.ProcessMessages;
{if errors=0 then
   begin
   Sleep(500);
   if closepolicy>0 then Form_pea.Close;
   end;}
{$ENDIF}
exitcode:=0;
end;

//procedure to compare files
procedure compare;
var
   fa,fb:file of byte;
   sfile:ansistring;
   sizea,sizeb,sizemin,total:qword;
   i,d,x,numreada,numreadb,numreadmin,continue:integer;
   bufa,bufb:array[0..65534]of byte;
   stoppedcomparison:boolean;
begin
exitcode:=-1;
Form_pea.PanelPW1.height:=2;
Form_report.visible:=true;
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
Form_report.Caption:='Compare';
Form_pea.Caption:='Byte to byte compare';
Form_pea.LabelTools2.Caption:='Comparing files...';
Form_pea.LabelTools3.Caption:='First file: '+(paramstr(2));
sfile:=paramstr(3);
if sfile='' then
   if Form_pea.OpenDialog2.Execute then
      if Form_pea.OpenDialog2.FileName<>'' then sfile:=Form_pea.OpenDialog2.FileName
      else exit;
Form_pea.LabelTools4.Caption:='Second file: '+(sfile);
Form_pea.ProgressBar1.Position:=0;
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,0]:='Test';
Form_report.StringGrid1.Cells[1,0]:='Result';
continue:=0;
total:=0;
d:=0;
try
assignfile(fa,(paramstr(2)));
filemode:=0;
reset(fa);
srcfilesize((paramstr(2)),sizea);
if sizea=0 then begin internal_error('The file is empty, cannot be compared'); exit; end;
setcurrentdir(extractfilepath((paramstr(2))));
assignfile(fb,sfile);
filemode:=0;
reset(fb);
srcfilesize(sfile,sizeb);
//sizeb:=system.filesize(fb);
if sizea=0 then total:=2;
if sizeb=0 then total:=2;
except
total:=1;
end;
if total<>0 then
   begin
   Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
   Form_report.StringGrid1.Cells[0,1]:='Error';
   Form_report.StringGrid1.Cells[1,1]:='Cannot compare files';
   if paramstr(2)=sfile then
      Form_pea.LabelTools2.Caption:='Cannot compare a file with itself!'
   else
      begin
      Form_pea.LabelTools2.Caption:='Cannot compare files';
      if total=2 then
         begin
         if sizea=0 then Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' first file is empty';
         if sizeb=0 then Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' second file is empty';
         end
      else
         Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' (i.e. not accessible, not files etc)';
      end;
   Form_pea.ProgressBar1.Position:=100;
   Form_report.StringGrid1.AutosizeColumns;
   Form_report.Label1.Caption:=Form_pea.Caption;
   Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
   Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
   Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
   {$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
   Form_pea.ButtonDone1.Visible:=true;
   Form_pea.LabelOpen.Visible:=true;
   Form_pea.LabelOpen.Enabled:=false;
   Form_pea.LabelLog1.Visible:=true;
   exit;
   end;
if sizeb<sizea then sizemin:=sizeb
else sizemin:=sizea;
if sizea=sizeb then
   begin
   Form_report.StringGrid1.Cells[0,1]:='Size comparison';
   Form_report.StringGrid1.Cells[1,1]:='Files have same size: '+inttostr(sizea)+' B';
   x:=1;
   end
else
   begin
   Form_report.StringGrid1.Cells[0,1]:='Size comparison';
   Form_report.StringGrid1.Cells[1,1]:='Files have different sizes';
   Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+3;
   Form_report.StringGrid1.Cells[0,2]:='- First file';
   Form_report.StringGrid1.Cells[1,2]:=inttostr(sizea)+' B';
   Form_report.StringGrid1.Cells[0,3]:='- Second file';
   Form_report.StringGrid1.Cells[1,3]:=inttostr(sizeb)+' B';
   Form_report.StringGrid1.Cells[0,4]:='- Size difference';
   if sizea>sizeb then Form_report.StringGrid1.Cells[1,4]:=inttostr(sizea-sizeb)+' B'
   else Form_report.StringGrid1.Cells[1,4]:=inttostr(sizeb-sizea)+' B';
   x:=4;
   end;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption+' ('+inttostr(sizea)+' B)';
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption+' ('+inttostr(sizeb)+' B)';
Form_pea.Visible:=false;
stoppedcomparison:=false;
Form_report.StringGrid1.BeginUpdate;
repeat
   blockread (fa,bufa,65536,numreada);
   blockread (fb,bufb,65536,numreadb);
   if numreadb<numreada then numreadmin:=numreadb
   else numreadmin:=numreada;
   for i:=0 to (numreadmin - 1) do
      begin
      if bufa[i]=bufb[i] then
      else
         begin
         d:=d+1;
         Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
         Form_report.StringGrid1.Cells[0,d+x]:='Offset '+system.hexstr(((filepos(fa)-numreada+i)div 16)*16,8);
         Form_report.StringGrid1.Cells[1,d+x]:='Byte '+inttostr(filepos(fa)-numreada+i+1)+' is different: Hex '+hexstr(@bufa[i],1)+' vs '+hexstr(@bufb[i],1)+'; Dec '+inttostr(bufa[i])+' vs '+inttostr(bufb[i]);
         if ((d>=100) and (continue=0)) then
            begin
            continue:=1;
            if MessageDlg('More than 100 different bytes, continue anyway?',mtConfirmation,[mbYes, mbNo],0)=6 then continue:=1
            else
               begin
               Form_report.StringGrid1.Cells[0,d+x]:='Comparison terminated by user';
               Form_report.StringGrid1.Cells[1,d+x]:='More than 100 different bytes';
               stoppedcomparison:=true;
               break;
               end;
            end;
         if (d>=10000) then
            begin
            Form_report.StringGrid1.Cells[0,d+x]:='Comparison automatically terminated';
            Form_report.StringGrid1.Cells[1,d+x]:='More than 10000 different bytes';
            stoppedcomparison:=true;
            break;
            end;
         end;
      end;
   inc(total,numreadmin);
   Form_pea.ProgressBar1.Position:=(total*100) div sizemin;
   Application.ProcessMessages;
until ((numreada=0) or (numreadb=0) or (stoppedcomparison=true));
Form_report.StringGrid1.EndUpdate;
closefile(fa);
closefile(fb);
Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
Form_report.StringGrid1.Cells[0,Form_report.StringGrid1.RowCount-1]:='Byte comparison';
if d=0 then
   if sizea=sizeb then
      begin
      Form_pea.Caption:='Files are identical';
      Form_pea.LabelTools2.Caption:='Same size, no different byte';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='No different byte';
      end
   else
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Different size, no different byte in the shortest file ('+inttostr(sizemin)+' B)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='No different byte in the shortest file ('+inttostr(sizemin)+' B)';
      end
else
   if sizea=sizeb then
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Same size, '+inttostr(d)+' different byte(s)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:=inttostr(d)+' different byte(s)';
      end
   else
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Different size, '+inttostr(d)+' different byte(s) in the shortest file ('+inttostr(sizemin)+' B)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:=inttostr(d)+' different byte(s) in the shortest file ('+inttostr(sizemin)+' B)';
      end;
Form_pea.ProgressBar1.Position:=100;
Form_report.StringGrid1.AutosizeColumns;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
exitcode:=0;
end;

procedure bench;
var
  P:array [1..16] of TProcessUTF8;
  cl,shint:AnsiString;
  i:integer;
  tsin,tsout:TTimestamp;
  timem,times,pointm,points:qword;
  pointratio:single;
begin

{$IFDEF CPU32}
MessageDlg('PeaZip 32 bit does not support benchmark, use PeaZip 64 bit instead.', mtWarning, [mbOK], 0); exit;
{$ENDIF}

Form_pea.PanelPW1.height:=2;
Form_pea.LabelTools2.Caption:='Benchmark started...';
shint:='Single core benchmark runs arithmetic and logic calculations over one array of 64K 64 bit integers, and one of 64K double precision floating.'+char($0D)+char($0A)+
'Multi core benchmark runs 16 parallel processes, each with one calculation thread, and up to 5 non-CPU intensive threads for GUI.'+char($0D)+char($0A)+
'Single core result unit is arbitrarily set, multi core result shows how faster is the machine to compute the same workload, Ratio value is multi core / single core result.'+char($0D)+char($0A)+
'Efficient system scheduler, and spare extra cores for non-intensive threads, helps improving multi core results, while thermal throttling degrades performances.'+char($0D)+char($0A)+
'2020 MacBook Air M1 on macOS Monterey for reference scores 100 (single core) / 515 (multi core).';
Form_pea.Image7.Hint:=shint;
Form_pea.LabelTools2.Hint:=shint;
Form_pea.LabelTools3.Hint:=shint;
Form_pea.LabelTools4.Hint:=shint;
cl:=executable_path+'pea'+EXEEXT+' BENCHINT SINGLE 1 1';
if validatecl(cl)<>0 then begin MessageDlg('Operation stopped, potentially dangerous command detected (i.e. command concatenation not allowed within the program): '+cl, mtWarning, [mbOK], 0); exit; end;

//single
tsin:=datetimetotimestamp(now);
P[1]:=TProcessUTF8.Create(nil);
{$IFDEF MSWINDOWS}
P[1].Options := [poWaitOnExit,poNoConsole];
{$ELSE}
P[1].Options := [poWaitOnExit];
{$ENDIF}
P[1].CommandLine:=cl;
P[1].Execute;
P[1].Free;
tsout:=datetimetotimestamp(now);
times:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if times>0 then
   begin
   points:=BASEBENCH div times;
   Form_pea.LabelTools3.Caption:='Single core '+inttostr(points);
   end;

application.ProcessMessages;
sleep(1000);

//multi 16
tsin:=datetimetotimestamp(now);
for i:=1 to 16 do
   begin
   P[i]:=TProcessUTF8.Create(nil);
   {$IFDEF MSWINDOWS}
   P[i].Options := [poNoConsole];
   {$ENDIF}
   cl:=executable_path+'pea'+EXEEXT+' BENCHINT MULTI '+inttostr(i)+' 16';
   P[i].CommandLine:=cl;
   P[i].Execute;
   end;

while (p[1].Running=true)  or
      (p[2].Running=true)  or
      (p[3].Running=true)  or
      (p[4].Running=true)  or
      (p[5].Running=true)  or
      (p[6].Running=true)  or
      (p[7].Running=true)  or
      (p[8].Running=true)  or
      (p[9].Running=true)  or
      (p[10].Running=true) or
      (p[11].Running=true) or
      (p[12].Running=true) or
      (p[13].Running=true) or
      (p[14].Running=true) or
      (p[15].Running=true) or
      (p[16].Running=true) do
      begin application.ProcessMessages; sleep(1); end;

for i:=1 to 16 do P[i].Free;

tsout:=datetimetotimestamp(now);
timem:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if (timem>0) and (times>0) then
   begin
   pointm:=(16*BASEBENCH) div timem;
   Form_pea.LabelTools2.Caption:='Arithmetic benchmark completed';
   Form_pea.LabelTools4.Caption:='Multi core '+inttostr(pointm);
   pointratio:=pointm/points;
   Form_pea.LabelTools4.Caption:=Form_pea.LabelTools4.Caption+' | ratio '+FloatToStrF(pointratio,ffFixed,4,2);
   end
else Form_pea.LabelTools2.Caption:='Benchmark failed';

end;

//procedure for arithmetic benchmark
procedure benchar;
var
   sbuf64:array [0..65534] of int64;
   sbufdouble:array [0..65534] of double;
   max64,i,j,t,tt,biter:int64;
   bmode:AnsiString;
   time:qword;
   tsin,tsout:TTimestamp;

procedure cancelbenchint; //not enabled
begin
Form_pea.LabelTools4.Caption:='Operation cancelled by user, terminating...';
Application.ProcessMessages;
sleep(1500);
halt(-4);
end;

begin
{$IFDEF CPU64}
exitcode:=-1;
tsin:=datetimetotimestamp(now);
Form_pea.PanelPW1.height:=2;
t:=1;
bmode:=UpCase(paramstr(2));
try t:=strtoint(paramstr(3)); except end;
try tt:=strtoint(paramstr(4)); except end;
if bmode='MULTI' then Form_pea.Caption:='Running benchmark segment '+inttostr(t)+'/'+inttostr(tt)
else Form_pea.Caption:='Running benchmark';
biter:=900;
//Form_pea.ButtonToolsCancel.visible:=true;
Form_pea.LabelTools2.Caption:='Benchmark running...';
Form_pea.ProgressBar1.Position:=0;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Randomize;
max64 := High(Int64);
for j:=1 to biter do
   begin
   for i:=0 to 65534 do sbuf64[i]:=random(max64);
   for i:=0 to 65534 do sbuf64[i]:=not(sbuf64[i]);
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] xor sbuf64[i+1];
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] or sbuf64[i+1];
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] and sbuf64[i+1];
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] + sbuf64[i+1];
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] - sbuf64[i+1];
   for i:=0 to 65534 do sbuf64[i]:=sqr(sbuf64[i]);
   for i:=0 to 65533 do sbuf64[i]:=sbuf64[i] * sbuf64[i+1] + sbuf64[i];
   for i:=0 to 65533 do if (sbuf64[i+1]<>0) then sbuf64[i]:=(sbuf64[i] div sbuf64[i+1]) + sbuf64[i+1];
   for i:=0 to 65533 do if (sbuf64[i+1]<>0) then sbuf64[i]:=(sbuf64[i] mod sbuf64[i+1]);
   for i:=0 to 65534 do sbuf64[i]:=sbuf64[i] shl 1;
   for i:=0 to 65534 do sbuf64[i]:=sbuf64[i] shr 1;

   for i:=0 to 65534 do sbufdouble[i]:=random;
   try for i:=0 to 65533 do sbufdouble[i]:=sbufdouble[i] + sbufdouble[i+1]; except end;
   try for i:=0 to 65533 do sbufdouble[i]:=sbufdouble[i] - sbufdouble[i+1]; except end;
   try for i:=0 to 65533 do sbufdouble[i]:=sbufdouble[i] * sbufdouble[i+1] + sbufdouble[i]; except end;
   try for i:=0 to 65533 do if (sbufdouble[i+1]<>0) then sbufdouble[i]:=sbufdouble[i] / sbufdouble[i+1]; except end;
   try for i:=0 to 65534 do sbufdouble[i]:=sqrt(sbufdouble[i]); except end;
   try for i:=0 to 65534 do sbufdouble[i]:=sqr(sbufdouble[i]); except end;

   for i:=0 to 65534 do sbufdouble[i]:=random;
   try for i:=0 to 65534 do sbuf64[i]:=trunc(sbufdouble[i]); except end;
   try for i:=0 to 65534 do sbuf64[i]:=round(sbufdouble[i]); except end;
   for i:=0 to 65534 do sbuf64[i]:=random(max64);
   try for i:=0 to 65534 do sbufdouble[i]:=sqrt(sbuf64[i]); except end;
   try for i:=0 to 65534 do sbufdouble[i]:=sqr(sbuf64[i]); except end;
   try for i:=0 to 65534 do if (sbuf64[i]<>0) then sbufdouble[i]:=(sbuf64[i] / sbuf64[i+1]) + sbufdouble[i] - sbuf64[i+1] except end;
   try for i:=0 to 65534 do if (sbufdouble[i]<>0) then sbufdouble[i]:=sbuf64[i] / sbufdouble[i] except end;

   Form_pea.ProgressBar1.Position:=(100*j) div (biter);
   tsout:=datetimetotimestamp(now);
   time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
   Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' elapsed';
   //if toolactioncancelled=true then begin cancelbenchint; exit; end;
   Application.ProcessMessages;
   end;
//Form_pea.ButtonToolsCancel.visible:=false;
Form_pea.ProgressBar1.Position:=100;
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time>0 then
   begin
   Form_pea.LabelTools2.Caption:='Benchmark completed';
   Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' elapsed (lower, better)';
   end
else Form_pea.LabelTools2.Caption:='Benchmark failed';
//Form_pea.ButtonDone1.Visible:=true;
exitcode:=0;
halt;
{$ENDIF}
end;

//procedure to checksum/hash files
procedure check; //ALL: all algorithms, otherwise specify the algorithm to use followed by "on" which marks the input parameters
var
   sbuf:array [1..65535] of byte;
   n:word;
   i,j,t,td,te,k,dmax,dmin,x,icount,iver,rc,dup,icol:integer;
   f_size,nfiles,ntfiles,ndirs,ctsize,etsize,tsize,nfound,ntotalexp,time,speed,compest,compsize:qword;
   smax,smin:int64;
   exp_files:TFoundList;
   exp_fsizes:TFoundListSizes;
   exp_ftimes:TFoundListAges;
   exp_fattr:TFoundListAttrib;
   exp_fattr_dec:TFoundList;
   tsin,tsout:TTimestamp;
   mode,dummystr,oper,moded,sdig:ansistring;
   dummyansistr:ansistring;
   pgpsig:TPGPDigest;
   Adler:longint;
   CRC16:word;
   CRC24:longint;
   CRC32:longint;
   CRC64:TCRC64;
   ED2KContext:TED2KContext;
   ED2KRes:TED2KResult;
   MD4Context:THashContext;
   MD4Digest:TMD4Digest;
   MD5Context:THashContext;
   MD5Digest:TMD5Digest;
   Blake2sContext:blake2s_ctx;
   Blake2sDigest:TBlake2sDigest;
   Blake2bContext:THashContext;
   Blake2bDigest:TBlake2bDigest;
   RMD160Context:THashContext;
   RMD160Digest:TRMD160Digest;
   SHA1Context:THashContext;
   SHA1Digest:TSHA1Digest;
   SHA224Context:THashContext;
   SHA224Digest:TSHA224Digest;
   SHA256Context:THashContext;
   SHA256Digest:TSHA256Digest;
   SHA3_256Context:THashContext;
   SHA3_256Digest:TSHA3_256Digest;
   SHA384Context:THashContext;
   SHA384Digest:TSHA384Digest;
   SHA512Context:THashContext;
   SHA512Digest:TSHA512Digest;
   SHA3_512Context:THashContext;
   SHA3_512Digest:TSHA3_512Digest;
   WhirlContext:THashContext;
   WhirlDigest:TWhirlDigest;
   Adler32_on,CRC16_on,CRC24_on,CRC32_on,CRC64_on,ED2K_on,MD4_on,MD5_on,RIPEMD160_on,
   SHA1_on,SHA224_on,SHA256_on,SHA384_on,SHA512_on,WHIRLPOOL_on,SHA3_256_on,SHA3_512_on,
   Blake2s_on,Blake2b_on:boolean;
   f:file of byte;

procedure cancelcheck;
begin
Form_pea.LabelTools4.Caption:='Operation cancelled by user, terminating...';
Application.ProcessMessages;
sleep(1500);
halt(-4);
end;

begin
exitcode:=-1;
tsin:=datetimetotimestamp(now);
Form_pea.PanelPW1.height:=2;
Form_pea.ButtonToolsCancel.visible:=true;
Form_report.Notebook1.PageIndex:=0;
Form_pea.LabelTools2.Caption:='Checking file(s)...';
Form_pea.ProgressBar1.Position:=0;
Form_report.InputT.Caption:='Input';
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_report.StringGrid1.ColCount:=31;
Form_report.StringGrid1.Cells[0,0]:='File path and name';
Form_report.StringGrid1.Cells[1,0]:='Name';
Form_report.StringGrid1.Cells[2,0]:='Type';
Form_report.StringGrid1.Cells[3,0]:='Size';
Form_report.StringGrid1.Cells[4,0]:='Bytes';
Form_report.StringGrid1.Cells[5,0]:='Modified';
Form_report.StringGrid1.Cells[6,0]:='Attributes';
Form_report.StringGrid1.Cells[7,0]:='Copies';
Form_report.StringGrid1.Cells[8,0]:='Adler32';
Form_report.StringGrid1.Cells[9,0]:='CRC16';
Form_report.StringGrid1.Cells[10,0]:='CRC24';
Form_report.StringGrid1.Cells[11,0]:='CRC32';
Form_report.StringGrid1.Cells[12,0]:='CRC64';
Form_report.StringGrid1.Cells[13,0]:='eDonkey';
Form_report.StringGrid1.Cells[14,0]:='MD4';
Form_report.StringGrid1.Cells[15,0]:='MD5';
Form_report.StringGrid1.Cells[16,0]:='RIPEMD160';
Form_report.StringGrid1.Cells[17,0]:='SHA1';
//Form_report.StringGrid1.Cells[18,0]:='SHA224';
Form_report.StringGrid1.Cells[18,0]:='BLAKE2S';
Form_report.StringGrid1.Cells[19,0]:='SHA256';
Form_report.StringGrid1.Cells[20,0]:='SHA3_256';
//Form_report.StringGrid1.Cells[21,0]:='SHA384';
Form_report.StringGrid1.Cells[21,0]:='BLAKE2B';
Form_report.StringGrid1.Cells[22,0]:='SHA512';
Form_report.StringGrid1.Cells[23,0]:='SHA3_512';
Form_report.StringGrid1.Cells[24,0]:='Whirlpool';
Form_report.StringGrid1.Cells[25,0]:='(Encoded size)';
Form_report.StringGrid1.Cells[26,0]:='File header';
Form_report.StringGrid1.Cells[27,0]:='End of file';
Form_report.StringGrid1.Cells[28,0]:='Directory content';
Form_report.StringGrid1.Cells[29,0]:='% size';
Form_report.StringGrid1.Cells[30,0]:='(Encoded % size)';
//read output mode HEX or BASE64
if upcase(paramstr(2))='HEX' then mode:='HEX'
else
   if upcase(paramstr(2))='LSBHEX' then mode:='LSBHEX'
   else
      if upcase(paramstr(2))='BASE64' then mode:='BASE64'
      else
         begin
         MessageDlg('Mode '+paramstr(2)+' is not valid, use HEX to see output coded as hexadecimal, LSBHEX for LSB hexadecimal or BASE64 for output coded in BASE64', mtError, [mbOK], 0);
         halt(-3);
         end;
//read algorithms to be used
j:=3;
Adler32_on:=false;
CRC16_on:=false;
CRC24_on:=false;
CRC32_on:=false;
CRC64_on:=false;
ED2K_on:=false;
MD4_on:=false;
MD5_on:=false;
RIPEMD160_on:=false;
Blake2s_on:=false;
Blake2b_on:=false;
SHA1_on:=false;
SHA224_on:=false;
SHA256_on:=false;
SHA3_256_on:=false;
SHA384_on:=false;
SHA512_on:=false;
SHA3_512_on:=false;
WHIRLPOOL_on:=false;
oper:='CRCHASH';
moded:=mode;
repeat
   case upcase(paramstr(j)) of
   'ADLER32': Adler32_on:=true;
   'CRC16': CRC16_on:=true;
   'CRC24': CRC24_on:=true;
   'CRC32': CRC32_on:=true;
   'CRC64': CRC64_on:=true;
   'ED2K': ED2K_on:=true;
   'MD4': MD4_on:=true;
   'MD5': MD5_on:=true;
   'RIPEMD160': RIPEMD160_on:=true;
   'SHA1': SHA1_on:=true;
   'BLAKE2S': Blake2s_on:=true;//'SHA224': SHA224_on:=true;
   'SHA256': SHA256_on:=true;
   'SHA3_256': SHA3_256_on:=true;
   'BLAKE2B': Blake2b_on:=true;//'SHA384': SHA384_on:=true;
   'SHA512': SHA512_on:=true;
   'SHA3_512': SHA3_512_on:=true;
   'WHIRLPOOL': WHIRLPOOL_on:=true;
   'ALL':
   begin
      Adler32_on:=true;
      CRC16_on:=true;
      CRC24_on:=true;
      CRC32_on:=true;
      CRC64_on:=true;
      ED2K_on:=true;
      MD4_on:=true;
      MD5_on:=true;
      RIPEMD160_on:=true;
      SHA1_on:=true;
      Blake2s_on:=true;//SHA224_on:=true;
      SHA256_on:=true;
      SHA3_256_on:=true;
      Blake2b_on:=true;//SHA384_on:=true;
      SHA512_on:=true;
      SHA3_512_on:=true;
      WHIRLPOOL_on:=true;
   end;
   'PREVIEW': begin oper:='PREVIEW'; moded:=oper; end;//no algorithm, only file metadata and header/eof samples
   'LIST': begin oper:='LIST'; moded:=oper; end;//no algorithm, only file metadata, will not assign files (faster)
   end;
   j:=j+1;
until ((upcase(paramstr(j-1))='ON') or (j>paramcount));
if j=4 then
   begin
   MessageDlg('No algorithm received', mtError, [mbOK], 0);
   halt(-3);
   end;
if j>paramcount then
   begin
   MessageDlg('No input file received', mtError, [mbOK], 0);
   halt(-3);
   end;
if oper='CRCHASH' then Form_pea.Caption:='Checksum and hash'
else Form_pea.Caption:='Analyze';
if (oper='CRCHASH') and (mode<>'BASE64') then Form_report.LabelCase.visible:=true;
Form_report.Caption:=Form_pea.Caption;
//get input size
tsize:=0;
etsize:=0;
ctsize:=0;
nfiles:=0;
ntfiles:=0;
Form_pea.LabelTools2.Caption:='Checking ('+moded+') '+inttostr(paramcount-j+1)+' element(s), counting total items and size...';
Application.ProcessMessages;
for i:=j to paramcount do
   begin
   if filegetattr((paramstr(i))) and faDirectory = 0 then
      begin
      srcfilesize((paramstr(i)),ctsize);
      ntfiles:=ntfiles+1;
      end
   else
      begin
      rcountsize((paramstr(i))+directoryseparator,'*',faAnyFile,true,nfiles,ndirs,ctsize);
      ntfiles:=ntfiles+nfiles;
      end;
   tsize:=tsize+ctsize;
   end;
//perform checks
t:=0;
te:=0;
td:=0;
ntotalexp:=0;
compest:=0;
compsize:=0;
dmax:=-1;
dmin:=-1;
smax:=-1;
smin:=-1;
for i:=j to paramcount do
begin
if i=j then setcurrentdir(extractfilepath((paramstr(i)))); //set path same as the first input file (for saving the report in)
expand((paramstr(i)),exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,nfound);
if nfound=0 then nfound:=1;
ntotalexp:=ntotalexp+nfound;
if oper='CRCHASH' then
   if ntotalexp>1 then Form_report.StringGrid1.Rowcount:=ntotalexp+2
   else Form_report.StringGrid1.Rowcount:=ntotalexp+1
else Form_report.StringGrid1.Rowcount:=ntotalexp+1;
for k:=0 to nfound-1 do
   if filegetattr(exp_files[k]) and faDirectory = 0 then
   begin
   filemode:=0;
   t:=t+1;
   try
   if oper<>'LIST' then
   begin
   assignfile(f,(exp_files[k]));
   filemode:=0;
   reset(f);
   end;
   srcfilesize((exp_files[k]),f_size);
   Form_report.StringGrid1.Cells[0,t]:=(exp_files[k]);
   Form_report.StringGrid1.Cells[1,t]:=extractfilename((exp_files[k]));
   Form_report.StringGrid1.Cells[2,t]:=extractfileext((exp_files[k]));
   Form_report.StringGrid1.Cells[3,t]:=nicenumber(inttostr(f_size),0);
   Form_report.StringGrid1.Cells[4,t]:=inttostr(f_size);
   Form_report.StringGrid1.Cells[5,t]:=FormatDateTime('yyyy-mm-dd hh:mm:ss', filedatetodatetime(exp_ftimes[k]));
   Form_report.StringGrid1.Cells[6,t]:=exp_fattr_dec[k];
   Form_report.StringGrid1.Cells[25,t]:=inttostr(length(inttostr(length(Form_report.StringGrid1.Cells[4,t]))))+inttostr(length(Form_report.StringGrid1.Cells[4,t]))+Form_report.StringGrid1.Cells[4,t];
   if tsize>0 then Form_report.StringGrid1.Cells[29,t]:=inttostr((100*f_size) div tsize)+'%';
   if tsize>0 then Form_report.StringGrid1.Cells[30,t]:=inttostr(length(Form_report.StringGrid1.Cells[27,t]))+Form_report.StringGrid1.Cells[27,t];
   compest:=testpcomp(exp_files[k]);
   compsize:=compsize+(f_size*compest);
   if smax=-1 then smax:=exp_fsizes[k];
   if smin=-1 then smin:=exp_fsizes[k];
   if dmax=-1 then dmax:=exp_ftimes[k];
   if dmin=-1 then dmin:=exp_ftimes[k];
   if f_size>smax then smax:=f_size;
   if f_size<smin then smin:=f_size;
   try
   if exp_ftimes[k]>dmax then dmax:=exp_ftimes[k];
   if exp_ftimes[k]<dmin then dmin:=exp_ftimes[k];
   except end;
   {if f_size=0 then
      begin
      if oper<>'LIST' then
      begin closefile(f); end;
      continue;
      end;}
   Form_pea.LabelTools2.Caption:='Checking ('+moded+') '+inttostr(paramcount-j+1)+' element(s), '+nicenumber(inttostr(tsize),0)+', '+nicenumber(inttostr(etsize),0)+' checked';
   Form_pea.LabelTools3.Caption:='Processing item '+inttostr(t)+' of '+inttostr(ntfiles)+', '+nicenumber(inttostr(f_size),0)+' file';
   Application.ProcessMessages;
   except
   Form_report.StringGrid1.Cells[0,t]:=(exp_files[k]);
   Form_report.StringGrid1.Cells[1,t]:=extractfilename((exp_files[k]));
   Form_report.StringGrid1.Cells[2,t]:=extractfileext((exp_files[k]));
   Form_report.StringGrid1.Cells[3,t]:='ERROR';
   te:=te+1;
   continue;
   end;
   if oper<>'LIST' then
   begin
   if RIPEMD160_on then RMD160Init(RMD160Context);
   if SHA1_on then SHA1Init(SHA1Context);
   if SHA256_on then SHA256Init(SHA256Context);
   if SHA3_256_on then SHA3_256Init(SHA3_256Context);
   if Blake2s_on then Blake2s_Init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen); //if SHA224_on then SHA224Init(SHA224Context);
   if Blake2b_on then Blake2b_Init(Blake2bContext,nil,0,BLAKE2B_MaxDigLen); //if SHA384_on then SHA384Init(SHA384Context);
   if SHA512_on then SHA512Init(SHA512Context);
   if SHA3_512_on then SHA3_512Init(SHA3_512Context);
   if Whirlpool_on then Whirl_Init(WhirlContext);
   if ED2K_on then ED2K_Init(ED2KContext);
   if MD4_on then MD4Init(MD4Context);
   if MD5_on then MD5Init(MD5Context);
   if CRC16_on then CRC16Init(CRC16);
   if CRC24_on then CRC24Init(CRC24);
   if CRC32_on then CRC32Init(CRC32);
   if Adler32_on then Adler32Init(adler);
   if CRC64_on then CRC64Init(CRC64);
   repeat
      blockread(f,sbuf,sizeof(sbuf),n);
      if n<>0 then
         begin
         if Adler32_on then Adler32Update(adler,@sbuf,n);
         if CRC16_on then CRC16Update(CRC16,@sbuf,n);
         if CRC24_on then CRC24Update(CRC24,@sbuf,n);
         if CRC32_on then CRC32Update(CRC32,@sbuf,n);
         if CRC64_on then CRC64Update(CRC64,@sbuf,n);
         if ED2K_on then ED2K_Update(ED2KContext,@sbuf,n);
         if MD4_on then MD4Update(MD4Context,@sbuf,n);
         if MD5_on then MD5Update(MD5Context,@sbuf,n);
         if RIPEMD160_on then RMD160Update(RMD160Context,@sbuf,n);
         if SHA1_on then SHA1Update(SHA1Context,@sbuf,n);
         if Blake2s_on then Blake2s_update(Blake2sContext,@sbuf,n);//if SHA224_on then SHA224Update(SHA224Context,@sbuf,n);
         if SHA256_on then SHA256Update(SHA256Context,@sbuf,n);
         if SHA3_256_on then SHA3_256Update(SHA3_256Context,@sbuf,n);
         if Blake2b_on then Blake2b_update(Blake2bContext,@sbuf,n);//if SHA384_on then SHA384Update(SHA384Context,@sbuf,n);
         if SHA512_on then SHA512Update(SHA512Context,@sbuf,n);
         if SHA3_512_on then SHA3_512Update(SHA3_512Context,@sbuf,n);
         if Whirlpool_on then Whirl_Update(WhirlContext,@sbuf,n);
         etsize:=etsize+n;
         Form_pea.ProgressBar1.Position:=(100*etsize) div (tsize+paramcount);
         tsout:=datetimetotimestamp(now);
         time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
         Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' elapsed';
         if toolactioncancelled=true then begin closefile(f); cancelcheck; exit; end;
         Application.ProcessMessages;
         end;
   until n<>sizeof(sbuf);
   seek(f,0);
   blockread(f,sbuf,32,n);
   if n<>0 then
      begin
      dummystr:='';
      SetString(dummystr, PChar(@sbuf[1]), n);
      Form_report.StringGrid1.Cells[26,t]:=utf8encode(dummystr);
      end;
   if f_size>=32 then seek(f,f_size-32) else seek(f,0);
   blockread(f,sbuf,32,n);
   if n<>0 then
      begin
      dummystr:='';
      SetString(dummystr, PChar(@sbuf[1]), n);
      Form_report.StringGrid1.Cells[27,t]:=utf8encode(dummystr);
      end;
   close(f);
   if Adler32_on then Adler32Final(adler);
   if CRC16_on then CRC16Final(CRC16);
   if CRC24_on then CRC24Final(CRC24);  Long2PGP(CRC24, pgpsig);
   if CRC32_on then CRC32Final(CRC32);
   if CRC64_on then CRC64Final(CRC64);
   if ED2K_on then ED2K_Final(ED2KContext,ED2KRes);
   if MD4_on then MD4Final(MD4Context,MD4Digest);
   if MD5_on then MD5Final(MD5Context,MD5Digest);
   if RIPEMD160_on then RMD160Final(RMD160Context,RMD160Digest);
   if SHA1_on then SHA1Final(SHA1Context,SHA1Digest);
   if Blake2s_on then blake2s_Final(Blake2sContext,Blake2sDigest);//if SHA224_on then SHA224Final(SHA224Context,SHA224Digest);
   if SHA256_on then SHA256Final(SHA256Context,SHA256Digest);
   if SHA3_256_on then SHA3_256Final(SHA3_256Context,SHA3_256Digest);
   if Blake2b_on then blake2b_Final(Blake2bContext,Blake2bDigest);//if SHA384_on then SHA384Final(SHA384Context,SHA384Digest);
   if SHA512_on then SHA512Final(SHA512Context,SHA512Digest);
   if SHA3_512_on then SHA3_512Final(SHA3_512Context,SHA3_512Digest);
   if Whirlpool_on then Whirl_Final(WhirlContext,WhirlDigest);
   if ((mode='HEX') or (mode='LSBHEX')) then
      begin
      if mode ='HEX' then
         begin
         if CRC16_on then CRC16 := swap(CRC16);
         if CRC24_on then Form_report.StringGrid1.Cells[10,t]:=hexstr(@pgpsig,sizeof(pgpsig));
         CRC32 := (CRC32 shr 24) or ((CRC32 shr 8) and $FF00) or ((CRC32 shl 8) and $FF0000) or (CRC32 shl 24);
         Adler := (Adler shr 24) or ((Adler shr 8) and $FF00) or ((Adler shl 8) and $FF0000) or (Adler shl 24);
         end
      else
         begin
         if CRC24_on then Form_report.StringGrid1.Cells[10,t]:=hexstr(@CRC24,sizeof(CRC24));
         end;
      if Adler32_on then Form_report.StringGrid1.Cells[8,t]:=lowercase(hexstr(@adler,sizeof(Adler)));
      if CRC16_on then Form_report.StringGrid1.Cells[9,t]:=lowercase(hexstr(@CRC16,sizeof(CRC16)));
      if CRC32_on then Form_report.StringGrid1.Cells[11,t]:=lowercase(hexstr(@CRC32,sizeof(CRC32)));
      if CRC64_on then Form_report.StringGrid1.Cells[12,t]:=lowercase(hexstr(@CRC64,sizeof(CRC64)));
      if ED2K_on then
         begin
         Form_report.StringGrid1.Cells[13,t]:=lowercase(hexstr(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey)));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[13,t]:=Form_report.StringGrid1.Cells[13,t]+' / eMule: '+lowercase(hexstr(@ED2KRes.eMule, sizeof(ED2KRes.eMule)));
         end;
      if MD4_on then Form_report.StringGrid1.Cells[14,t]:=lowercase(hexstr(@MD4Digest,sizeof(MD4Digest)));
      if MD5_on then Form_report.StringGrid1.Cells[15,t]:=lowercase(hexstr(@MD5Digest,sizeof(MD5Digest)));
      if RIPEMD160_on then Form_report.StringGrid1.Cells[16,t]:=lowercase(hexstr(@RMD160Digest,sizeof(RMD160Digest)));
      if SHA1_on then Form_report.StringGrid1.Cells[17,t]:=lowercase(hexstr(@SHA1Digest,sizeof(SHA1Digest)));
      if Blake2s_on then Form_report.StringGrid1.Cells[18,t]:=lowercase(hexstr(@Blake2sDigest,sizeof(Blake2sDigest)));//if SHA224_on then Form_report.StringGrid1.Cells[18,t]:=upcase(hexstr(@SHA224Digest,sizeof(SHA224Digest)));
      if SHA256_on then Form_report.StringGrid1.Cells[19,t]:=lowercase(hexstr(@SHA256Digest,sizeof(SHA256Digest)));
      if SHA3_256_on then Form_report.StringGrid1.Cells[20,t]:=lowercase(hexstr(@SHA3_256Digest,sizeof(SHA3_256Digest)));
      if Blake2b_on then Form_report.StringGrid1.Cells[21,t]:=lowercase(hexstr(@Blake2bDigest,sizeof(Blake2bDigest)));//if SHA384_on then Form_report.StringGrid1.Cells[21,t]:=upcase(hexstr(@SHA384Digest,sizeof(SHA384Digest)));
      if SHA512_on then Form_report.StringGrid1.Cells[22,t]:=lowercase(hexstr(@SHA512Digest,sizeof(SHA512Digest)));
      if SHA3_512_on then Form_report.StringGrid1.Cells[23,t]:=lowercase(hexstr(@SHA3_512Digest,sizeof(SHA3_512Digest)));
      if Whirlpool_on then Form_report.StringGrid1.Cells[24,t]:=lowercase(hexstr(@WhirlDigest,sizeof(WhirlDigest)));
      end
   else
      begin
      if Adler32_on then Form_report.StringGrid1.Cells[8,t]:=base64str(@adler,sizeof(Adler));
      if CRC16_on then Form_report.StringGrid1.Cells[9,t]:=base64str(@CRC16,sizeof(CRC16));
      if CRC24_on then Form_report.StringGrid1.Cells[10,t]:=base64str(@pgpsig,sizeof(CRC24));
      if CRC32_on then Form_report.StringGrid1.Cells[11,t]:=base64str(@CRC32,sizeof(CRC32));
      if CRC64_on then Form_report.StringGrid1.Cells[12,t]:=base64str(@CRC64,sizeof(CRC64));
      if ED2K_on then
         begin
         Form_report.StringGrid1.Cells[13,t]:=base64str(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[13,t]:=Form_report.StringGrid1.Cells[13,t]+' / eMule: '+base64str(@ED2KRes.eMule, sizeof(ED2KRes.eMule));
         end;
      if MD4_on then Form_report.StringGrid1.Cells[14,t]:=base64str(@MD4Digest,sizeof(MD4Digest));
      if MD5_on then Form_report.StringGrid1.Cells[15,t]:=base64str(@MD5Digest,sizeof(MD5Digest));
      if RIPEMD160_on then Form_report.StringGrid1.Cells[16,t]:=base64str(@RMD160Digest,sizeof(RMD160Digest));
      if SHA1_on then Form_report.StringGrid1.Cells[17,t]:=base64str(@SHA1Digest,sizeof(SHA1Digest));
      if Blake2s_on then Form_report.StringGrid1.Cells[18,t]:=base64str(@Blake2sDigest,sizeof(Blake2sDigest)); //if SHA224_on then Form_report.StringGrid1.Cells[18,t]:=base64str(@SHA224Digest,sizeof(SHA224Digest));
      if SHA256_on then Form_report.StringGrid1.Cells[19,t]:=base64str(@SHA256Digest,sizeof(SHA256Digest));
      if SHA3_256_on then Form_report.StringGrid1.Cells[20,t]:=base64str(@SHA3_256Digest,sizeof(SHA3_256Digest));
      if Blake2b_on then Form_report.StringGrid1.Cells[21,t]:=base64str(@Blake2bDigest,sizeof(Blake2bDigest)); //if SHA384_on then Form_report.StringGrid1.Cells[21,t]:=base64str(@SHA384Digest,sizeof(SHA384Digest));
      if SHA512_on then Form_report.StringGrid1.Cells[22,t]:=base64str(@SHA512Digest,sizeof(SHA512Digest));
      if SHA3_512_on then Form_report.StringGrid1.Cells[23,t]:=base64str(@SHA3_512Digest,sizeof(SHA3_512Digest));
      if Whirlpool_on then Form_report.StringGrid1.Cells[24,t]:=base64str(@WhirlDigest,sizeof(WhirlDigest));
      end;
   end
   else //list operation, do not assign file
      begin
      etsize:=etsize+f_size;
      Form_pea.ProgressBar1.Position:=(100*etsize) div (tsize+paramcount);
      tsout:=datetimetotimestamp(now);
      time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
      Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' elapsed';
      if toolactioncancelled=true then begin cancelcheck; exit; end;
      end;
   if etsize>0 then Form_pea.LabelTools4.Caption:='Potential compression '+inttostr((etsize*100 - compsize) div etsize)+'%';
   end
   else//directory
    begin
    t:=t+1;
    td:=td+1;
    dummystr:=copy((exp_files[k]),0,length((exp_files[k]))-1);
    Form_report.StringGrid1.Cells[0,t]:=(exp_files[k]);
    Form_report.StringGrid1.Cells[1,t]:=extractfilename(dummystr);
    Form_report.StringGrid1.Cells[2,t]:=' [folder]';
    Form_report.StringGrid1.Cells[3,t]:='';
    Form_report.StringGrid1.Cells[4,t]:='';
    Form_report.StringGrid1.Cells[5,t]:=FormatDateTime('yyyy-mm-dd hh:mm:ss', filedatetodatetime(exp_ftimes[k]));
    Form_report.StringGrid1.Cells[6,t]:=exp_fattr_dec[k];
    ctsize:=0;
    nfiles:=0;
    ndirs:=0;
    rcountsize((exp_files[k]),'*',faAnyFile,true,nfiles,ndirs,ctsize);
    Form_report.StringGrid1.Cells[28,t]:='('+inttostr(ctsize)+' B) '+(inttostr(ndirs-1))+' dir(s), '+(inttostr(nfiles))+' file(s), '+nicenumber(inttostr(ctsize),0);
    if tsize>0 then Form_report.StringGrid1.Cells[29,t]:=inttostr((100*ctsize) div tsize)+'%';
    if tsize>0 then Form_report.StringGrid1.Cells[30,t]:=inttostr(length(Form_report.StringGrid1.Cells[27,t]))+Form_report.StringGrid1.Cells[27,t];
    end;
end;
Form_report.StringGrid1.AutosizeColumns;
if Adler32_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      Adler32Init(Adler);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[8,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            Adler32Update(Adler,@sbuf,sizeof(sdig));
            end;
         end;
      Adler32Final(Adler);
      case mode of
         'HEX':
         begin
         Adler := (Adler shr 24) or ((Adler shr 8) and $FF00) or ((Adler shl 8) and $FF0000) or (Adler shl 24);
         Form_report.StringGrid1.Cells[8,t+1]:=upcase(hexstr(@Adler,sizeof(Adler)));
         end;
         'LSBHEX': Form_report.StringGrid1.Cells[8,t+1]:=upcase(hexstr(@Adler,sizeof(Adler)));
         'BASE64':Form_report.StringGrid1.Cells[8,t+1]:=base64str(@Adler,sizeof(Adler));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[8]:=0;
if CRC16_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      CRC16Init(CRC16);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[9,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            CRC16Update(CRC16,@sbuf,sizeof(sdig));
            end;
         end;
      CRC16Final(CRC16);
      case mode of
         'HEX':
         begin
         CRC16 := swap(CRC16);
         Form_report.StringGrid1.Cells[9,t+1]:=upcase(hexstr(@CRC16,sizeof(CRC16)));
         end;
         'LSBHEX': Form_report.StringGrid1.Cells[9,t+1]:=upcase(hexstr(@CRC16,sizeof(CRC16)));
         'BASE64':Form_report.StringGrid1.Cells[9,t+1]:=base64str(@CRC16,sizeof(CRC16));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[9]:=0;
if CRC24_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      CRC24Init(CRC24);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[10,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            CRC24Update(CRC24,@sbuf,sizeof(sdig));
            end;
         end;
      CRC24Final(CRC24);
      Long2PGP(CRC24, pgpsig);
      case mode of
         'HEX':
         begin
         Form_report.StringGrid1.Cells[10,t+1]:=hexstr(@pgpsig,sizeof(pgpsig));
         Form_report.StringGrid1.Cells[10,t+1]:=upcase(hexstr(@CRC24,sizeof(CRC24)));
         end;
         'LSBHEX': Form_report.StringGrid1.Cells[10,t+1]:=hexstr(@CRC24,sizeof(CRC24));
         'BASE64': Form_report.StringGrid1.Cells[10,t+1]:=base64str(@pgpsig,sizeof(CRC24))
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[10]:=0;
if CRC32_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      CRC32Init(CRC32);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[11,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            CRC32Update(CRC32,@sbuf,sizeof(sdig));
            end;
         end;
      CRC32Final(CRC32);
      case mode of
         'HEX':
         begin
         CRC32 := (CRC32 shr 24) or ((CRC32 shr 8) and $FF00) or ((CRC32 shl 8) and $FF0000) or (CRC32 shl 24);
         Form_report.StringGrid1.Cells[11,t+1]:=upcase(hexstr(@CRC32,sizeof(CRC32)));
         end;
         'LSBHEX': Form_report.StringGrid1.Cells[11,t+1]:=upcase(hexstr(@CRC32,sizeof(CRC32)));
         'BASE64':Form_report.StringGrid1.Cells[11,t+1]:=base64str(@CRC32,sizeof(CRC32));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[11]:=0;
if CRC64_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      CRC64Init(CRC64);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[12,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            CRC64Update(CRC64,@sbuf,sizeof(sdig));
            end;
         end;
      CRC64Final(CRC64);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[12,t+1]:=upcase(hexstr(@CRC64,sizeof(CRC64)));
         'BASE64':Form_report.StringGrid1.Cells[12,t+1]:=base64str(@CRC64,sizeof(CRC64));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[12]:=0;
if ED2K_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      ED2K_Init(ED2KContext);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[13,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            ED2K_Update(ED2KContext,@sbuf,sizeof(sdig));
            end;
         end;
      ED2K_Final(ED2KContext,ED2KRes);
      case mode of
         'HEX','LSBHEX':
         begin
         Form_report.StringGrid1.Cells[13,t+1]:=upcase(hexstr(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey)));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[13,t+1]:=Form_report.StringGrid1.Cells[13,t+1]+' / eMule: '+upcase(hexstr(@ED2KRes.eMule, sizeof(ED2KRes.eMule)));
         end;
         'BASE64':
         begin
         Form_report.StringGrid1.Cells[13,t+1]:=base64str(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[13,t+1]:=Form_report.StringGrid1.Cells[13,t+1]+' / eMule: '+base64str(@ED2KRes.eMule, sizeof(ED2KRes.eMule));
         end;
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[13]:=0;
if MD4_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      MD4Init(MD4Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[14,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            MD4Update(MD4Context,@sbuf,sizeof(sdig));
            end;
         end;
      MD4Final(MD4Context,MD4Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[14,t+1]:=upcase(hexstr(@MD4Digest,sizeof(MD4Digest)));
         'BASE64':Form_report.StringGrid1.Cells[14,t+1]:=base64str(@MD4Digest,sizeof(MD4Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[14]:=0;
if MD5_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      MD5Init(MD5Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[15,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            MD5Update(MD5Context,@sbuf,sizeof(sdig));
            end;
         end;
      MD5Final(MD5Context,MD5Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[15,t+1]:=upcase(hexstr(@MD5Digest,sizeof(MD5Digest)));
         'BASE64':Form_report.StringGrid1.Cells[15,t+1]:=base64str(@MD5Digest,sizeof(MD5Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[15]:=0;
if RIPEMD160_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      RMD160Init(RMD160Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[16,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            RMD160Update(RMD160Context,@sbuf,sizeof(sdig));
            end;
         end;
      RMD160Final(RMD160Context,RMD160Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[16,t+1]:=upcase(hexstr(@RMD160Digest,sizeof(RMD160Digest)));
         'BASE64':Form_report.StringGrid1.Cells[16,t+1]:=base64str(@RMD160Digest,sizeof(RMD160Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[16]:=0;
if SHA1_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA1Init(SHA1Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[17,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA1Update(SHA1Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA1Final(SHA1Context,SHA1Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[17,t+1]:=upcase(hexstr(@SHA1Digest,sizeof(SHA1Digest)));
         'BASE64':Form_report.StringGrid1.Cells[17,t+1]:=base64str(@SHA1Digest,sizeof(SHA1Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[17]:=0;
if Blake2s_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      Blake2s_init(Blake2sContext,nil,0,BLAKE2S_MaxDigLen);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[18,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            blake2s_update(Blake2sContext,@sbuf,sizeof(sdig));
            end;
         end;
      blake2s_Final(Blake2sContext,Blake2sDigest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[18,t+1]:=upcase(hexstr(@Blake2sDigest,sizeof(Blake2sDigest)));
         'BASE64':Form_report.StringGrid1.Cells[18,t+1]:=base64str(@Blake2sDigest,sizeof(Blake2sDigest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[18]:=0;
{if SHA224_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA224Init(SHA224Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[18,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA224Update(SHA224Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA224Final(SHA224Context,SHA224Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[18,t+1]:=upcase(hexstr(@SHA224Digest,sizeof(SHA224Digest)));
         'BASE64':Form_report.StringGrid1.Cells[18,t+1]:=base64str(@SHA224Digest,sizeof(SHA224Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[18]:=0;}
if SHA256_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA256Init(SHA256Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[19,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA256Update(SHA256Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA256Final(SHA256Context,SHA256Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[19,t+1]:=upcase(hexstr(@SHA256Digest,sizeof(SHA256Digest)));
         'BASE64':Form_report.StringGrid1.Cells[19,t+1]:=base64str(@SHA256Digest,sizeof(SHA256Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[19]:=0;
if SHA3_256_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA3_256Init(SHA3_256Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[20,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA3_256Update(SHA3_256Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA3_256Final(SHA3_256Context,SHA3_256Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[20,t+1]:=upcase(hexstr(@SHA3_256Digest,sizeof(SHA3_256Digest)));
         'BASE64':Form_report.StringGrid1.Cells[20,t+1]:=base64str(@SHA3_256Digest,sizeof(SHA3_256Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[20]:=0;
if Blake2b_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      Blake2b_init(Blake2bContext,nil,0,BLAKE2B_MaxDigLen);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[21,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            blake2b_update(Blake2bContext,@sbuf,sizeof(sdig));
            end;
         end;
      blake2b_Final(Blake2bContext,Blake2bDigest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[21,t+1]:=upcase(hexstr(@Blake2bDigest,sizeof(Blake2bDigest)));
         'BASE64':Form_report.StringGrid1.Cells[21,t+1]:=base64str(@Blake2bDigest,sizeof(Blake2bDigest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[21]:=0;
{if SHA384_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA384Init(SHA384Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[21,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA384Update(SHA384Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA384Final(SHA384Context,SHA384Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[21,t+1]:=upcase(hexstr(@SHA384Digest,sizeof(SHA384Digest)));
         'BASE64':Form_report.StringGrid1.Cells[21,t+1]:=base64str(@SHA384Digest,sizeof(SHA384Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[21]:=0;}
if SHA512_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA512Init(SHA512Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[22,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA512Update(SHA512Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA512Final(SHA512Context,SHA512Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[22,t+1]:=upcase(hexstr(@SHA512Digest,sizeof(SHA512Digest)));
         'BASE64':Form_report.StringGrid1.Cells[22,t+1]:=base64str(@SHA512Digest,sizeof(SHA512Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[22]:=0;
if SHA3_512_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      SHA3_512Init(SHA3_512Context);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[23,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            SHA3_512Update(SHA3_512Context,@sbuf,sizeof(sdig));
            end;
         end;
      SHA3_512Final(SHA3_512Context,SHA3_512Digest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[23,t+1]:=upcase(hexstr(@SHA3_512Digest,sizeof(SHA3_512Digest)));
         'BASE64':Form_report.StringGrid1.Cells[23,t+1]:=base64str(@SHA3_512Digest,sizeof(SHA3_512Digest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[23]:=0;
if Whirlpool_on then
   if (t-td-te)>1 then
      begin
      Form_report.StringGrid1.Cells[0,t+1]:='* Digest *';
      Whirl_Init(WhirlContext);
      for i:=1 to t do
         begin
         sdig:=Form_report.StringGrid1.Cells[24,i];
         if sdig<>'' then
            begin
            for j:=1 to sizeof(sdig) do sbuf[j]:=byte(sdig[j]);
            Whirl_Update(WhirlContext,@sbuf,sizeof(sdig));
            end;
         end;
      Whirl_Final(WhirlContext,WhirlDigest);
      case mode of
         'HEX','LSBHEX': Form_report.StringGrid1.Cells[24,t+1]:=upcase(hexstr(@WhirlDigest,sizeof(WhirlDigest)));
         'BASE64':Form_report.StringGrid1.Cells[24,t+1]:=base64str(@WhirlDigest,sizeof(WhirlDigest));
         end;
      end
   else
else Form_report.StringGrid1.ColWidths[24]:=0;
Form_report.StringGrid1.ColWidths[25]:=0;
if oper='LIST' then
   begin
   Form_report.StringGrid1.ColWidths[26]:=0;
   Form_report.StringGrid1.ColWidths[27]:=0;
   end;
Form_report.StringGrid1.ColWidths[30]:=0;
if Form_report.StringGrid1.ColWidths[0]>320 then Form_report.StringGrid1.ColWidths[0]:=320;
if Form_report.StringGrid1.ColWidths[1]>200 then Form_report.StringGrid1.ColWidths[1]:=200;
///check duplicates
icol:=0;
for icount:=8 to 23 do
   if Form_report.StringGrid1.ColWidths[icount]<>0 then icol:=icount;
if icol<>0 then
begin
Form_pea.ProgressBar1.Position:=95;
rc:=Form_report.StringGrid1.RowCount-1;
if rc>1 then rc:=rc-1;//digest
Form_pea.LabelTools2.Caption:='Checking for duplicates in '+inttostr(rc)+' items';
Form_pea.LabelTools3.Caption:='This passage may take some time';
Application.ProcessMessages;
for icount:=1 to rc do
   begin
   dup:=0;
   if (Form_report.StringGrid1.Cells[2,icount]<>' [folder]') and (Form_report.StringGrid1.Cells[4,icount]<>'0') then
      begin
      for iver:=1 to rc do
         if Form_report.StringGrid1.Cells[icol,iver]<>Form_report.StringGrid1.Cells[icol,icount] then else dup:=dup+1;
      Form_report.StringGrid1.Cells[7,icount]:=inttostr(dup);
      end
   else Form_report.StringGrid1.Cells[7,icount]:='-';
   if icount>1024 then
      if (icount and 127) = 0 then
         begin
         Form_pea.LabelTools2.Caption:='Checking for duplicates of '+Form_report.StringGrid1.Cells[1,icount];
         Form_pea.LabelTools3.Caption:='Item '+inttostr(icount)+' of '+inttostr(rc);
         Application.ProcessMessages;
         end;
   end;
end
else Form_report.StringGrid1.ColWidths[7]:=0;
///
Form_report.StringGrid1.PopupMenu:=Form_report.PopupMenu1;
Form_pea.ButtonToolsCancel.visible:=false;
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelTools2.Caption:='Checked ('+moded+') '+inttostr(paramcount-j+1)+' element(s), '+nicenumber(inttostr(tsize),0)+' ['+inttostr(tsize)+' B]';
Form_pea.LabelTools3.Caption:='Processed '+inttostr(t)+' of '+inttostr(ntotalexp)+' items: '+inttostr(t-td-te)+' files, '+inttostr(td)+' directories, '+inttostr(te)+' errors';
try
if t>1 then Form_pea.LabelTools4.Caption:=
   'Larger '+nicenumber(inttostr(smax),0)+' smaller '+nicenumber(inttostr(smin),0)+
   ', newer '+FormatDateTime('yyyy-mm-dd hh:mm:ss', filedatetodatetime(dmax))+
   ' older '+FormatDateTime('yyyy-mm-dd hh:mm:ss', filedatetodatetime(dmin))
else
   Form_pea.LabelTools4.Caption:='Size '+nicenumber(inttostr(smax),0)+', date '+FormatDateTime('yyyy-mm-dd hh:mm:ss', filedatetodatetime(dmax));
except end;
if etsize>0 then Form_pea.LabelTools4.Caption:=Form_pea.LabelTools4.Caption+', potential compression '+inttostr((etsize*100 - compsize) div etsize)+'%';
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time>0 then
   begin
   speed:=(tsize * 1000) div time;
   Form_pea.LabelTools5.Caption:=nicetime(inttostr(time))+' total time @ '+nicenumber(inttostr(speed),0)+'/s';
   end
else Form_pea.LabelTools5.Caption:='';
Form_report.Label1.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools4.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools5.Caption;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Form_report.Visible:=true;
Form_pea.Visible:=false;
exitcode:=0;
end;

//procedure to display environment variables strings
procedure envstr;
var
   i:integer;
begin
exitcode:=-1;
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.Caption:='Environment variables';
Form_pea.Caption:='Environment variables';
Form_pea.LabelTools2.Caption:='';
Form_pea.ProgressBar1.Position:=0;
Form_report.InputT.Caption:='Environment variables';
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_report.StringGrid1.ColCount:=2;
Form_report.StringGrid1.Cells[0,0]:='N';
Form_report.StringGrid1.Cells[1,0]:='Variable';
Form_report.StringGrid1.Rowcount:=GetEnvironmentVariableCount+2;
for i:=0 to GetEnvironmentVariableCount do
   begin
   Form_report.StringGrid1.Cells[0,i+1]:=inttostr(i);
   Form_report.StringGrid1.Cells[1,i+1]:=GetEnvironmentString(i);
   end;
Form_report.StringGrid1.AutosizeColumns;
Form_pea.ProgressBar1.Position:=100;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Form_report.Visible:=true;
Form_pea.Visible:=false;
exitcode:=0;
end;

procedure listfiles; //list files: mode INFO gives detailed information, LIST plain list
var
   s,mode:ansistring;
   h,k,i:integer;
   exp_files:TFoundList;
   exp_fsizes:TFoundListSizes;
   exp_ftimes:TFoundListAges;
   exp_fattr:TFoundListAttrib;
   exp_fattr_dec:TFoundList;
   nfound,nsize,smax,smin,compsize:qword;
   dmax,dmin,compest:integer;
begin
exitcode:=-1;
mode:=(paramstr(2));
s:=(paramstr(3));
Form_pea.Caption:=mode;
Form_pea.LabelTools2.Caption:='Listing '+s+' may take some time, please wait...';
Form_pea.ProgressBar1.Position:=5;
Application.ProcessMessages;
expand(s,exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,nfound);
Form_pea.ProgressBar1.Position:=50;
Application.ProcessMessages;
nsize:=0;
compsize:=0;
compest:=0;
dmax:=exp_ftimes[0];
dmin:=exp_ftimes[0];
smax:=exp_fsizes[0];
smin:=exp_fsizes[0];
if nfound=0 then nfound:=1;
Form_pea.LabelTools3.Caption:='Found: '+inttostr(nfound);
Form_pea.ProgressBar1.Position:=60;
Application.ProcessMessages;
if upcase(mode)='INFO' then
   for i:=0 to nfound-1 do
      begin
      nsize:=nsize+exp_fsizes[i];
      compest:=testpcomp(exp_files[i]);
      compsize:=compsize+(exp_fsizes[i]*compest);
      if exp_fsizes[i]>smax then smax:=exp_fsizes[i];
      if exp_fsizes[i]<smin then smin:=exp_fsizes[i];
      try
      if exp_ftimes[i]>dmax then dmax:=exp_ftimes[i];
      if exp_ftimes[i]<dmin then dmin:=exp_ftimes[i];
      except end;
      end;
Form_pea.LabelTools4.Caption:='Total size: '+inttostr(nsize)+' B';
Form_pea.ProgressBar1.Position:=70;
Application.ProcessMessages;
Form_report.InputT.Caption:='Input';
if upcase(mode)='INFO' then Form_report.Caption:='Info'
else Form_report.Caption:='List';
Form_report.StringGrid1.ColCount:=4;
Form_report.StringGrid1.Cells[0,0]:='Name';
Form_report.StringGrid1.Cells[1,0]:='Size (B)';
Form_report.StringGrid1.Cells[2,0]:='Date/time';
Form_report.StringGrid1.Cells[3,0]:='Attributes';
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_report.StringGrid1.RowCount:=nfound+1;
for k:=0 to nfound-1 do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=exp_files[k];
    Form_report.StringGrid1.Cells[1,k+1]:=inttostr(exp_fsizes[k]);
    try Form_report.StringGrid1.Cells[2,k+1]:=datetimetostr(filedatetodatetime(exp_ftimes[k])); except Form_report.StringGrid1.Cells[2,k+1]:='unknown'; end;
    Form_report.StringGrid1.Cells[3,k+1]:=exp_fattr_dec[k];
    end;
Form_pea.ProgressBar1.Position:=100;
Application.ProcessMessages;
Form_report.StringGrid1.AutosizeColumns;
Form_report.Label1.Caption:=s;
Form_report.Label2.Caption:='';
Form_report.Label3.Caption:='';
Form_report.Label4.Caption:='';
if upcase(mode)='INFO' then
   begin
   try Form_report.Label2.Caption:='Found: '+inttostr(nfound)+' objects (newer: '+datetimetostr(filedatetodatetime(dmax))+', older: '+datetimetostr(filedatetodatetime(dmin))+')'; except Form_report.Label2.Caption:='Found: '+inttostr(nfound)+' objects'; end;
   Form_report.Label3.Caption:='Total size: '+nicenumber(inttostr(nsize),0)+' (larger: '+nicenumber(inttostr(smax),0)+', smaller: '+nicenumber(inttostr(smin),0)+');';
   if nsize<>0 then Form_report.Label3.Caption:=Form_report.Label3.Caption+' potential compression: '+inttostr((nsize*100 - compsize) div nsize)+'%';//+nicenumber(inttostr(compsize div 100))+' ('+inttostr(compsize div nsize)+'%)';
   end;
Form_report.Visible:=true;
Form_pea.Visible:=false;
exitcode:=0;
end;

//hex preview: slow, limited to 64 MB
procedure hexpreview;
var
   hexs,hexs1,astr,offs,s:ansistring;
   fa:file of byte;
   sizea,total:qword;
   i,x,y,numreada,nrows,prows,noffs,wrbytes:integer;
   bufa:array[0..65534]of byte;
   bufhex:array[0..15,0..4095]of byte;
begin
exitcode:=-1;
if directoryexists(paramstr(2)) then
   begin
   Form_pea.LabelTools2.Caption:=paramstr(2)+' is a directory, cannot be previewed';
   exit;
   end;
Form_report.StringGrid1.BeginUpdate;
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.Caption:='Hex preview';
Form_pea.Caption:='Hex preview';
Form_pea.LabelTools2.Caption:=(paramstr(2));
Form_pea.LabelTools3.Caption:='';
Form_pea.LabelTools4.Caption:='';
Form_pea.ProgressBar1.Position:=0;
Form_report.StringGrid1.RowCount:=1;
Form_report.StringGrid1.ColCount:=3;
Form_report.StringGrid1.Cells[0,0]:='Offset';
Form_report.StringGrid1.Cells[1,0]:='Hex';
Form_report.StringGrid1.Cells[2,0]:='Possible UTF8';
Form_report.StringGrid1.Font.Name:='Courier';
Form_report.StringGrid1.Font.Size:=10;
Form_report.StringGrid1.ColWidths[0]:=96;
Form_report.StringGrid1.ColWidths[1]:=460;
Form_report.StringGrid1.ColWidths[2]:=180;
sizea:=0;
try
assignfile(fa,(paramstr(2)));
filemode:=0;
reset(fa);
srcfilesize((paramstr(2)),sizea);
if sizea=0 then begin internal_error('The file is empty, cannot be previewed'); exit; end;
setcurrentdir(extractfilepath((paramstr(2))));
except
MessageDlg((paramstr(2))+' is not accessible (or not a file)', mtError, [mbOK], 0);
halt(-3);
exit;
end;
if sizea>64*1024*1024 then
  begin
  MessageDlg('Hex preview is currently limited to small files, up to 64 MB', mtWarning, [mbOK], 0);
  exit;
  end;
Form_pea.LabelTools3.Caption:='Size '+nicenumber(inttostr(sizea),0)+' ('+inttostr(sizea)+' B)';
Form_report.StringGrid1.RowCount:=(sizea div 16) +2;
total:=0;
prows:=1;
wrbytes:=0;
repeat
   numreada:=0;
   blockread (fa,bufa,65536,numreada);
   i:=0;
   y:=0;
   repeat
      for x:=0 to 15 do
         begin
         bufhex[x,y]:=bufa[i];
         i:=i+1;
         if i=numreada then break;
         end;
      y:=y+1;
   until i>=numreada;
   nrows:=y;
   i:=0;
   for y:=0 to nrows-1 do
      begin
      noffs:=y+prows-1;
      offs:=inttohex(noffs*16,8);
      Form_report.StringGrid1.Cells[0,y+prows]:=offs;
      astr:='';
      for x:=0 to 15 do
         begin
         i:=i+1;
         if i=numreada then break;
         end;
      astr:=chr(bufhex[0,y])+chr(bufhex[1,y])+chr(bufhex[2,y])+chr(bufhex[3,y])+chr(bufhex[4,y])+chr(bufhex[5,y])+chr(bufhex[6,y])+chr(bufhex[7,y])+
            chr(bufhex[8,y])+chr(bufhex[9,y])+chr(bufhex[10,y])+chr(bufhex[11,y])+chr(bufhex[12,y])+chr(bufhex[13,y])+chr(bufhex[14,y])+chr(bufhex[15,y]);
      SetLength(astr, x+1);
      hexs:='';
      hexs1:='';
      SetLength(hexs, Length(astr)*2);
      BinToHex(@astr[1], @hexs[1], Length(astr));
      hexs1:=hexs[1]+hexs[2]+' '+hexs[3]+hexs[4]+' '+hexs[5]+hexs[6]+' '+hexs[7]+hexs[8]+' '+
             hexs[9]+hexs[10]+' '+hexs[11]+hexs[12]+' '+hexs[13]+hexs[14]+' '+hexs[15]+hexs[16]+' '+
             hexs[17]+hexs[18]+' '+hexs[19]+hexs[20]+' '+hexs[21]+hexs[22]+' '+hexs[23]+hexs[24]+' '+
             hexs[25]+hexs[26]+' '+hexs[27]+hexs[28]+' '+hexs[29]+hexs[30]+' '+hexs[31]+hexs[32];
      //setlength(hexs,length(hexs)-1);
      wrbytes:=wrbytes+16;
      Form_report.StringGrid1.Cells[1,y+prows]:=hexs1;
      Form_report.StringGrid1.Cells[2,y+prows]:=ansitoutf8(astr);
      end;
   ///
   inc(total,numreada);
   prows:=prows+nrows;
   Form_pea.ProgressBar1.Position:=(total*100) div sizea;
   Application.ProcessMessages;
until (numreada=0) or (total>=sizea);
//Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid1.EndUpdate;
closefile(fa);
Form_pea.Visible:=false;
Form_report.visible:=true;
Form_pea.ProgressBar1.Position:=100;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:='';
{$IFDEF MSWINDOWS}Form_report.OutputT.TabVisible:=false;{$ENDIF}Form_report.Notebook1.ShowTabs:=false;
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
exitcode:=0;
end;

{
GUI procedures
}

procedure parse_action;
begin
case upcase(paramstr(1))of
'PEA' : pea;
'UNPEA' : unpea;
'RFS' : rfs;
'RFJ' : rfj;
'WIPE' : wipe(paramstr(2));
'SANITIZE' : sanitize(paramstr(2));
'COMPARE' : compare;
'BENCH' : bench;
'BENCHINT' : benchar;
'CHECK' : check;
'ENVSTR' : envstr;
'LIST' : listfiles;
'HEXPREVIEW' : hexpreview;
else internal_error('Incorrect request for Pea, the action "'+paramstr(1)+'" is not supported');
end;
end;

procedure call_pea;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
Form_pea.Panel1.Visible:=true;
Form_pea.LabelE1.Visible:=true;
if (upcase(paramstr(7))='TRIATS') or (upcase(paramstr(7))='TRITSA') or (upcase(paramstr(7))='TRISAT') or
   (upcase(paramstr(7))='EAX256') or (upcase(paramstr(7))='TF256') or (upcase(paramstr(7))='SP256') or
   (upcase(paramstr(7))='EAX') or (upcase(paramstr(7))='TF') or (upcase(paramstr(7))='SP') or (upcase(paramstr(7))='HMAC') then
   if (upcase(paramstr(8))='INTERACTIVE') or (upcase(paramstr(8))='INTERACTIVE_REPORT') then
      begin
      Form_pea.Visible:=true;
      Form_pea.PanelDecrypt1.visible:=false;
      Form_pea.PanelEncrypt1.visible:=true;
      Form_pea.PanelPW1.Visible:=true;
      Whirl_Init(ment); //only for PEA called as executable (otherwise passphrase/keyfile is passed from main executable): improve seed generation trough mouse movements sampling while entering passwphrase/keyfile
      Form_pea.LabelConfirm1.Visible:=true;
      Form_pea.EditConfirm1.Visible:=true;
      Form_pea.LabelHint1.Visible:=true;
      Form_pea.LabelSample1.Visible:=true;
      Form_pea.LabelSample2.Visible:=true;
      Form_pea.Image5.Visible:=true;
      exit;
      end;
interacting:=false;
end;

procedure call_unpea;
var
   f_in:file of byte;
   in_folder,in_file,in_name,in_qualified_name,compr,algo,obj_algo,volume_algo:ansistring;
   buf,tmp_buf:array [0..19] of byte;
   pwneeded,singlevolume:boolean;
   compr_level,headersize,authsize,volume_authsize,archive_datetimeencoding:byte;
   i,numread:integer;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
//parse archive to see if password is needed
in_qualified_name:=(paramstr(2));
if not(fileexists(in_qualified_name)) then internal_error('"'+in_qualified_name+'" not exists');
in_folder:=extractfilepath(in_qualified_name);
in_file:=extractfilename(in_qualified_name);
if upcase(copy(in_qualified_name,length(in_qualified_name)-10,11))<>'.000001.PEA' then
   begin
   singlevolume:=true;
   end
else
   begin
   singlevolume:=false;
   delete(in_file,length(in_file)-10,11);
   end;
in_name:=in_file;
{blockread 10 byte archive header; since volume tag size is unknown to UnPEA,
PEA set first volume size mandatory at least 10 byte (plus volume tag) in order
to make UnPEA able to blockread the archive header and calculate the volume tag
size}
assignfile(f_in,in_qualified_name);
filemode:=0;
reset(f_in);
try
blockread (f_in,buf,10,numread);
except
internal_error('IO error reading from '+in_qualified_name);
end;
close(f_in);
test_pea_error('parsing archive header',pea_parse_archive_header(buf,volume_algo,archive_datetimeencoding));
decode_volume_control_algo (volume_algo,volume_authsize);
read_from_chunks ( in_folder,in_name,
                   20,
                   buf,tmp_buf,
                   volume_authsize,
                   20,
                   singlevolume);
for i:=0 to 9 do buf[i]:=buf[i+10];
pea_parse_stream_header(buf, compr, compr_level, algo, obj_algo);
decode_control_algo ( algo,
                      headersize,
                      authsize,
                      pwneeded);
//if password is needed, open the password panel
if pwneeded=true then
   if (upcase(paramstr(7))='INTERACTIVE') or (upcase(paramstr(7))='INTERACTIVE_REPORT') then
      begin
      Form_pea.Visible:=true;
      Form_pea.PanelDecrypt1.visible:=true;
      Form_pea.PanelEncrypt1.visible:=false;
      Form_pea.PanelPW1.Visible:=true;
      exit;
      end;
interacting:=false;
end;

procedure call_rfs;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
if upcase(paramstr(3))='ASK' then
   begin
   Form_pea.PanelRFSinteractive.visible:=true;
   Form_pea.Caption:='Split file';
   exit;
   end;
interacting:=false;
end;

procedure call_rfj;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
interacting:=false;
end;

procedure call_wipe;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_sanitize;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_compare;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_bench;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_benchint;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_check;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_envstr;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_list;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_hexpreview;
begin
Form_pea.Visible:=true;
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

{ TForm_pea }

procedure TForm_pea.ButtonDone1Click(Sender: TObject);
begin
Close;
end;

procedure TForm_pea.ButtonPeaExitClick(Sender: TObject);
begin
halt(-4);
end;

procedure TForm_pea.ButtonPW1Click(Sender: TObject);
begin
if (EditPW1.Text='') and (EditConfirm1.Text='') then pw:='default'
else
   if (upcase(paramstr(1))='PEA') and (EditPW1.Text<>EditConfirm1.Text) then
      begin
      MessageDlg('Passwords doesn''t match, please retype "Passphrase" and "Confirm" fields', mtWarning, [mbOK], 0);
      exit;
      end
   else pw:=EditPW1.Text;
if LabelKeyFileName1.Caption='<none>' then keyfile_name:='NOKEYFILE'
else keyfile_name:=LabelKeyFileName1.Caption;
PanelPW1.Visible:=false;
interacting:=false;
end;

procedure TForm_pea.ButtonPW2Click(Sender: TObject);
begin
Form_pea.Close;
end;

procedure TForm_pea.ButtonRFSinteractive1Click(Sender: TObject);
begin
Form_pea.Close;
end;

procedure TForm_pea.ButtonRFSinteractiveClick(Sender: TObject);
begin
   case ComboBox1.ItemIndex of
      0: begin
         try
            vol_size:=SpinEdit1.Value;
            case ComboBox2.ItemIndex of
               1: vol_size:=vol_size*1024;
               2: vol_size:=vol_size*1024*1024;
               3: vol_size:=vol_size*1024*1024*1024;
               end;
         except
            MessageDlg('Cannot get volume size', mtWarning, [mbOK], 0);
            exit;
         end;
         end;
      1: vol_size:=1457664;//FD
      2: vol_size:=10*1024*1024;//limit for attachment size of some mail services
      3: vol_size:=25*1024*1024;//limit for attachment size of most mail services
      4: vol_size:=650*1024*1024;//CD 650 MB
      5: vol_size:=700*1024*1024;//CD 700 MB
      6: vol_size:=4092*1024*1024;//max file size for FAT32 filesystem
      7: vol_size:=4480*1024*1024;//size DVD+R
      8: vol_size:=8128*1024*1024;//size for DVD-R DL
      9: vol_size:=23040*1024*1024;//size for Blu-Ray
      10: vol_size:=23040*1024*1024*2;//size for Blu-Ray DL
      11: vol_size:=23040*1024*1024*4;//size for Blu-Ray XL3
      12: vol_size:=23040*1024*1024*5;//size for Blu-Ray XL4
      end;
   case ComboBox3.ItemIndex of //volume checks
      0: vol_algo:='WHIRLPOOL';
      1: vol_algo:='SHA3_512';
      2: vol_algo:='SHA512';
      3: vol_algo:='BLAKE2B';
      4: vol_algo:='SHA3_256';
      5: vol_algo:='SHA256';
      6: vol_algo:='BLAKE2S';
      7: vol_algo:='RIPEMD160';
      8: vol_algo:='SHA1';
      9: vol_algo:='MD5';
      10: vol_algo:='CRC64';
      11: vol_algo:='CRC32';
      12: vol_algo:='ADLER32';
      13: vol_algo:='NOALGO';
      end;
   interacting:=false;
   PanelRFSinteractive.visible:=false;
end;

procedure TForm_pea.ButtonToolsCancelClick(Sender: TObject);
begin
toolactioncancelled:=true;
end;

procedure TForm_pea.ButtonUtilsCancelClick(Sender: TObject);
begin
Form_pea.Close;
end;

procedure TForm_pea.ButtonUtilsResetClick(Sender: TObject);
begin
ListMemo.Clear;
end;

procedure getunits;
var
   s:ansistring;
   nunits,i:qword;
   drivestr:array[1..255] of char;
begin
{$IFDEF MSWINDOWS}
Form_pea.ComboBoxUnits.Clear;
GetLogicalDriveStrings(255,@drivestr);
i:=1;
nunits:=0;
repeat
   s:='';
   nunits:=nunits+1;
   while (i<=255) and (drivestr[i]<>#00) do
      begin
      s:=s+char(drivestr[i]);
      inc(i);
      end;
   inc(i);
   if s<>'' then Form_pea.ComboBoxUnits.Items.Add(s);
until length(s)=0;
nunits:=nunits-1;
{$ENDIF}
end;

procedure TForm_pea.ButtonUtilsOKClick(Sender: TObject);
var
  cl,bin_name,in_param:ansistring;
  i:integer;
  P:TProcessUTF8;
begin
Form_report.StringGrid1.PopupMenu:=nil;
bin_name:=stringdelim(escapefilename(executable_path,desk_env)+'pea'+EXEEXT);
in_param:='';
cl:='';
for i:=0 to ListMemo.Lines.Count do
   if length(ListMemo.Lines[i])>1 then
      in_param:=in_param+stringdelim(ListMemo.Lines[i])+' ';
case ComboBoxUtils.ItemIndex of
   20: begin end;
   21: begin end;
   22: begin end;
   else if in_param='' then exit;
   end;
case ComboBoxUtils.ItemIndex of
   0: cl:=bin_name+' CHECK HEX CRC32 ON '+in_param;
   1: cl:=bin_name+' CHECK HEX CRC64 ON '+in_param;
   2: cl:=bin_name+' CHECK HEX MD5 ON '+in_param;
   3: cl:=bin_name+' CHECK HEX RIPEMD160 ON '+in_param;
   4: cl:=bin_name+' CHECK HEX SHA1 ON '+in_param;
   5: cl:=bin_name+' CHECK HEX BLAKE2S ON '+in_param;
   6: cl:=bin_name+' CHECK HEX SHA256 ON '+in_param;
   7: cl:=bin_name+' CHECK HEX SHA3_256 ON '+in_param;
   8: cl:=bin_name+' CHECK HEX BLAKE2B ON '+in_param;
   9: cl:=bin_name+' CHECK HEX SHA512 ON '+in_param;
   10: cl:=bin_name+' CHECK HEX SHA3_512 ON '+in_param;
   11: cl:=bin_name+' CHECK HEX WHIRLPOOL ON '+in_param;
   12: cl:=bin_name+' CHECK HEX CRC32 CRC64 MD5 RIPEMD160 SHA1 BLAKE2S SHA256 SHA3_256 ON '+in_param;
   13: cl:=bin_name+' CHECK HEX ALL ON '+in_param;
   14: cl:=bin_name+' CHECK HEX LIST ON '+in_param;
   15: cl:=bin_name+' RFS AUTONAME ASK NONE BATCH '+stringdelim(ListMemo.Lines[0]); //one file
   16: cl:=bin_name+' RFJ '+stringdelim(ListMemo.Lines[0])+' BATCH AUTONAME'; //one file (strictly)
   17: cl:=bin_name+' COMPARE '+in_param; //two files or one file (ask for second file, ignores more files)
   18: cl:=bin_name+' HEXPREVIEW '+stringdelim(ListMemo.Lines[0]);  //one file
   19: begin
      if MessageDlg('Do you want to securely delete selected file(s)? The operation can''t be undone and files will be not recoverable', mtWarning, [mbYes,mbNo], 0)=6 then
         begin
         cl:=bin_name+' WIPE MEDIUM '+in_param;
         P:=TProcessUTF8.Create(nil);
         {$IFDEF MSWINDOWS}
         P.Options := [poNoConsole,poWaitOnExit];
         {$ELSE}
         P.Options := [poWaitOnExit];
         {$ENDIF}
         cl:=(cl);
         P.CommandLine:=cl;
         if validatecl(cl)<>0 then begin MessageDlg('Operation stopped, potentially dangerous command detected (i.e. command concatenation not allowed within the program): '+cl, mtWarning, [mbOK], 0); exit; end;
         P.Execute;
         P.Free;
         i:=0;
         repeat
         if length(ListMemo.Lines[i])>1 then
            if not fileexists(ListMemo.Lines[i]) then ListMemo.Lines.Delete(i)
            else i:=i+1;
         until i>=ListMemo.Lines.Count;
         exit;
         end
      else exit;
      end;
   20: begin
      {$IFDEF MSWINDOWS}
      in_param:=ComboBoxUnits.Caption;
      if MessageDlg('The operation can take some time, depending on the size of the disk, continue?', mtInformation, [mbYes,mbNo], 0)=6 then
         cl:=bin_name+' SANITIZE FAST '+in_param
      else exit;
      {$ELSE}
      MessageDlg('Sorry, function not supported on current system', mtInformation, [mbOK], 0);
      exit;
      {$ENDIF}
      end;
   21: begin
      {$IFDEF MSWINDOWS}
      in_param:=ComboBoxUnits.Caption;
      if MessageDlg('The operation can take some time, depending on the size of the disk, continue?', mtInformation, [mbYes,mbNo], 0)=6 then
         cl:=bin_name+' SANITIZE ZERO '+in_param
      else exit;
      {$ELSE}
      MessageDlg('Sorry, function not supported on current system', mtInformation, [mbOK], 0);
      exit;
      {$ENDIF}
      end;
   22: cl:=bin_name+' ENVSTR';
end;
P:=TProcessUTF8.Create(nil);
{$IFDEF MSWINDOWS}
P.Options := [poNoConsole];
{$ENDIF}
cl:=(cl);
P.CommandLine:=cl;
if validatecl(cl)<>0 then begin MessageDlg('Operation stopped, potentially dangerous command detected (i.e. command concatenation not allowed within the program): '+cl, mtWarning, [mbOK], 0); exit; end;
P.Execute;
P.Free;
end;

procedure change_imagesplit;
begin
with Form_pea do
   begin
   case ComboBox1.ItemIndex of
      0: ImageSplit.Picture.Bitmap:=nil;
      1: ImageSplit.Picture.Bitmap:=Bfd;
      2: ImageSplit.Picture.Bitmap:=Bmail;
      3: ImageSplit.Picture.Bitmap:=Bmail;
      4: ImageSplit.Picture.Bitmap:=Bdvd;
      5: ImageSplit.Picture.Bitmap:=Bdvd;
      6: ImageSplit.Picture.Bitmap:=Bhd;
      7: ImageSplit.Picture.Bitmap:=Bdvd;
      8: ImageSplit.Picture.Bitmap:=Bdvd;
      9: ImageSplit.Picture.Bitmap:=Bdvd;
      10: ImageSplit.Picture.Bitmap:=Bdvd;
      11: ImageSplit.Picture.Bitmap:=Bdvd;
      12: ImageSplit.Picture.Bitmap:=Bdvd;
      end;
   end;
end;

procedure ComboBox1_onchange;
begin
with Form_pea do
begin
change_imagesplit;
if ComboBox1.ItemIndex = 0 then
   begin
   SpinEdit1.Visible:=true;
   ComboBox2.Visible:=true;
   end
else
   begin
   SpinEdit1.Visible:=false;
   ComboBox2.Visible:=false;
   end;
end;
end;

procedure TForm_pea.ComboBox1Change(Sender: TObject);
begin
ComboBox1_onchange;
end;

procedure enabledropmenu;
begin
with Form_pea do
begin
ListMemo.Enabled:=True;
ListMemo.Visible:=True;
LabelUtilsInput.Visible:=True;
LabelOpenFile0.Visible:=True;
LabelOpenFile2.Visible:=True;
LabelOpenFile3.Visible:=True;
ButtonUtilsReset.Enabled:=True;
ComboBoxUnits.Visible:=false;
end;
end;

procedure disabledropmenu;
begin
with Form_pea do
begin
ListMemo.Enabled:=False;
ListMemo.Clear;
ListMemo.Visible:=False;
LabelUtilsInput.Visible:=False;
LabelOpenFile0.Visible:=False;
LabelOpenFile2.Visible:=False;
LabelOpenFile3.Visible:=False;
ButtonUtilsReset.Enabled:=False;
if ComboBoxUtils.ItemIndex<>22 then
   begin
   ComboBoxUnits.Visible:=true;
   getunits;
   ComboBoxUnits.ItemIndex:=0;
   end
else ComboBoxUnits.Visible:=false;
end;
end;

procedure TForm_pea.ComboBoxUtilsChange(Sender: TObject);
begin
case ComboBoxUtils.ItemIndex of
   0: enabledropmenu;
   1: enabledropmenu;
   2: enabledropmenu;
   3: enabledropmenu;
   4: enabledropmenu;
   5: enabledropmenu;
   6: enabledropmenu;
   7: enabledropmenu;
   8: enabledropmenu;
   9: enabledropmenu;
   10: enabledropmenu;
   11: enabledropmenu;
   12: enabledropmenu;
   13: enabledropmenu;
   14: enabledropmenu;
   15: enabledropmenu;
   16: enabledropmenu;
   17: enabledropmenu;
   18: enabledropmenu;
   19: enabledropmenu;
   20: disabledropmenu;
   21: disabledropmenu;
   22: disabledropmenu;
end;
end;

procedure TForm_pea.EditPW1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
   pw,pr:ansistring;
   pw_strength:dword;
   pw_rating:byte;
begin
if LabelKeyFileName1.Caption[1]<>'<' then
   begin
   Shape2.Width:=240;
   Shape2.Brush.Color:=PBLUE;
   LabelPS1.Caption:='Using KeyFile';
   end
else
   begin
   pw:=EditPW1.Text;
   evaluate_password(pw,pw_strength);
   if (pw_strength>8) then
      if (pw_strength<240) then Shape2.Width:=pw_strength
      else Shape2.Width:=240
   else Shape2.Width:=8;
   if pw_strength<24 then Shape2.Brush.Color:=PRED
   else
      if pw_strength<48 then Shape2.Brush.Color:=PYELLOW
      else
         if pw_strength<72 then Shape2.Brush.Color:=PLGREEN
         else Shape2.Brush.Color:=PGREEN;
   case Shape2.Brush.Color of
      PRED: pr:='Weak';
      PYELLOW: pr:='Poor';
      PLGREEN: pr:='Adequate';
      PGREEN: pr:='Good';
      end;
   LabelPS1.Caption:=pr+' ('+inttostr(pw_strength)+')';
   end;
end;

procedure TForm_pea.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   randf: file of byte;
   randarr: TKey2048;
begin
try
shl_rand(randarr); //read and leftshift of 1 byte data from persistent random seed file
gen_rand(randarr); //create new reandom seed file
assignfile(randf,persistent_source); //write keyfile as new seed file
rewrite(randf);
blockwrite(randf,randarr,256);
closefile(randf);
except
end;
end;

procedure TForm_pea.LabelE1Click(Sender: TObject);
begin
if details=true then
   begin
   Panel1.visible:=true;
   LabelE1.Caption:='+';
   details:=false;
   end
else
   begin
   Panel1.visible:=false;
   LabelE1.Caption:='-';
   details:=true;
   end;
end;

procedure TForm_pea.LabelKeyFile1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
   if OpenDialog1.FileName<>'' then
      begin
      LabelKeyFileName1.Caption:=OpenDialog1.FileName;
      Shape2.Width:=240;
      Shape2.Brush.Color:=PBLUE;
      LabelPS1.Caption:='Using KeyFile';
      end;
end;

procedure getthemepath(var thpath:ansistring);
var
   theme_name,s:ansistring;
begin
s:=graphicsfolder;
if s<>'' then setlength(s,length(s)-1);
theme_name:=extractfilename(s);
//default and no graphic themes are in application's path, custom themes are in configuration path (application's path for portable versions, user's home/application data for installable versions)
if (upcase(theme_name)<>upcase(DEFAULT_THEME)) and (upcase(theme_name)<>'NOGRAPHIC') then thpath:=confpath
else thpath:=sharepath;
end;

procedure load_icons; //load icons from bitmaps
var
   thpath:ansistring;
   i16res:integer;
begin
getthemepath(thpath);
i16res:=(qscaleimages*16) div 100;
//valorize all captions, hints, TStrings Items
   try
   Bfd:=TBitmap.Create;
   Bmail:=TBitmap.Create;
   Bhd:=TBitmap.Create;
   Bdvd:=TBitmap.Create;
   Binfo:=TBitmap.Create;
   Blog:=TBitmap.Create;
   Bok:=TBitmap.Create;
   Bcancel:=TBitmap.Create;
   Butils:=TBitmap.Create;
   Badmin:=TBitmap.Create;
   if graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      Form_pea.imagelist1.getbitmap(1,Bfd);
      Form_pea.imagelist1.getbitmap(2,Bmail);
      Form_pea.imagelist1.getbitmap(3,Bhd);
      Form_pea.imagelist1.getbitmap(4,Bdvd);
      Form_pea.imagelist1.getbitmap(5,Binfo);
      Form_pea.imagelist1.getbitmap(6,Blog);
      Form_pea.imagelist1.getbitmap(7,Bok);
      Form_pea.imagelist1.getbitmap(8,Bcancel);
      Form_pea.imagelist1.getbitmap(9,Butils);
      Form_pea.imagelist1.getbitmap(10,Badmin);
      end;
   if graphicsfolder='themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      Form_pea.imagelist1.getbitmap(0,Bfd);
      Form_pea.imagelist1.getbitmap(0,Bmail);
      Form_pea.imagelist1.getbitmap(0,Bhd);
      Form_pea.imagelist1.getbitmap(0,Bdvd);
      Form_pea.imagelist1.getbitmap(0,Binfo);
      Form_pea.imagelist1.getbitmap(0,Blog);
      Form_pea.imagelist1.getbitmap(0,Bok);
      Form_pea.imagelist1.getbitmap(0,Bcancel);
      Form_pea.imagelist1.getbitmap(0,Butils);
      Form_pea.imagelist1.getbitmap(0,Badmin);
      end;
   if (graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator) and (graphicsfolder<>'themes'+directoryseparator+'main-embedded'+directoryseparator) then
      begin
      getthemedbitmap(Binfo,thpath+graphicsfolder+'16'+directoryseparator+'16-info.png');
      getthemedbitmap(Blog,thpath+graphicsfolder+'16'+directoryseparator+'16-paste.png');
      getthemedbitmap(Bok,thpath+graphicsfolder+'16'+directoryseparator+'16-test.png');
      getthemedbitmap(Bcancel,thpath+graphicsfolder+'16'+directoryseparator+'16-stop.png');
      end;
   setpbitmap(Bfd,i16res);
   setpbitmap(Bmail,i16res);
   setpbitmap(Bhd,i16res);
   setpbitmap(Bdvd,i16res);
   setpbitmap(Binfo,i16res);
   setpbitmap(Blog,i16res);
   setpbitmap(Bok,i16res);
   setpbitmap(Bcancel,i16res);
   setpbitmap(Butils,i16res);
   setpbitmap(Badmin,i16res);
   Form_pea.peautilsbtn.Glyph:=Butils;
   Form_pea.pmrunasadmin.Bitmap:=Badmin;
   Form_pea.Image3.Picture.Bitmap:=Binfo;
   Form_pea.Image4.Picture.Bitmap:=Binfo;
   Form_pea.Image5.Picture.Bitmap:=Binfo;
   Form_pea.Image7.Picture.Bitmap:=Binfo;
   Form_pea.ImageUtils.Picture.Bitmap:=Binfo;
   Form_pea.buttonpw1.Glyph:=Bok;
   Form_pea.buttonpw2.Glyph:=Bcancel;
   Form_pea.ButtonPeaExit.Glyph:=Bcancel;
   Form_pea.ButtonPeaExit1.Glyph:=Bcancel;
   Form_pea.buttonrfsinteractive.Glyph:=Bok;
   Form_pea.buttonrfsinteractive1.Glyph:=Bcancel;
   Form_pea.buttonutilsok.Glyph:=Bok;
   Form_pea.buttonutilscancel.Glyph:=Bcancel;
   Form_pea.buttontoolscancel.Glyph:=Bcancel;
   except
   //MessageDlg('some icons not found', mtWarning, [mbOK], 0);  //it's deactivated in final compilation to allow the program to work outside of PeaZip package
   end;
end;

function wingetappdata(var s:ansistring):integer;
{$IFDEF MSWINDOWS}
var
  pidl: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
wingetappdata:=-1;
{$IFDEF MSWINDOWS}
try
   if Succeeded(ShGetSpecialFolderLocation(Form_pea.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
      if ShGetPathfromIDList(pidl, Buf ) then
         begin
         s:=(Buf)+'\PeaZip\';
         CoTaskMemFree(pidl);
         wingetappdata:=0;
         end
      else CoTaskMemFree(pidl);
except
end;
{$ENDIF}
end;

procedure TForm_pea.FormCreate(Sender: TObject);

procedure readconf_relativeline(nlines:integer; var dummy:ansistring);
var
   i:integer;
begin
for i:=1 to nlines do readln(conf,dummy);
end;

var
   s,dummy:ansistring;
   dummyint:integer;
begin
fshown:=false;
//valorize application's paths
executable_path:=Application.location;
if executable_path='' then extractfilepath(paramstr(0));
if executable_path<>'' then
   if executable_path[length(executable_path)]<>directoryseparator then executable_path:=executable_path+directoryseparator;
setcurrentdir(executable_path);
{$IFDEF Darwin}
   resource_path:=executable_path+'../Resources/';
{$ELSE}
   resource_path:=executable_path+'res'+directoryseparator;
{$ENDIF}
SetFocusedControl(EditPW1);
getdesk_env(desk_env,caption_build,delimiter);
height_set:=false;
toolactioncancelled:=false;
Form_pea.Caption:='PEA '+P_RELEASE+' ('+PEAUTILS_RELEASE+') / specs '+inttostr(PEA_FILEFORMAT_VER)+'.'+inttostr(PEA_FILEFORMAT_REV);
if (PEA_FILEFORMAT_VER <> pea_utils.PEA_FILEFORMAT_VER) or (PEA_FILEFORMAT_REV <> pea_utils.PEA_FILEFORMAT_REV) then
   Form_pea.Caption:='PEA '+P_RELEASE+' ('+PEAUTILS_RELEASE+') / Warning: inconsistent internal specs level!';
try
   if FileExists(resource_path+'portable') then //if file exists, assume portable version
      begin
      confpath:=resource_path+'conf'+directoryseparator; //configuration (portable)
      binpath:=resource_path+'bin'+directoryseparator;//binaries, architecture dependant
      sharepath:=resource_path+'share'+directoryseparator;//non binaries resources, non-architecture dependant
      end
   else
      begin
      binpath:=resource_path+'bin'+directoryseparator;//binaries, architecture dependant
      sharepath:=resource_path+'share'+directoryseparator;//non binaries resources, non-architecture dependant
      {$IFDEF MSWINDOWS}
      if wingetappdata(confpath)<>0 then confpath:=(GetEnvironmentVariable('APPDATA'))+'\PeaZip\'; //if wingetappdata fails use env variables
      {$ELSE}
      s:=GetEnvironmentVariable('XDG_CONFIG_HOME');
      if s<>'' then confpath:=s+'/peazip/'
      else
         begin
         get_home_path(s);
         confpath:=s+'/.config/peazip/';
         end;
      {$ENDIF}
      if not(directoryexists(confpath)) then forcedirectories(confpath);
      confpath:=ExpandFileName(confpath);
      if confpath<>'' then
         if confpath[length(confpath)]<>directoryseparator then confpath:=confpath+directoryseparator;
      if not(directoryexists(confpath)) then confpath:=resource_path+'conf'+directoryseparator; //if alternative configuration directory does not exist or is not accessible, use res path
      end;
   persistent_source:=confpath+'rnd';
   {$IFDEF DARWIN}
   persistent_source:=confpath+'rnd';
   {$ENDIF}
   assignfile(conf,(confpath+'conf.txt'));
   filemode:=0;
   reset(conf);
   readln(conf,dummy);
   readln(conf,graphicsfolder);
   if graphicsfolder[1]='r' then graphicsfolder:='themes'+directoryseparator+DEFAULT_THEME+directoryseparator;
   DoDirSeparators(graphicsfolder);
   readln(conf,dummy); opacity:=strtoint(dummy);
   readln(conf,color1);
   readln(conf,color2);
   readln(conf,color3);
   readln(conf,color4);
   readln(conf,color5);
   readln(conf,dummy); decodebintheming(dummy,gridaltcolor,highlighttabs,dummyint,dummyint,dummyint,dummyint,alttabstyle,ensmall,contrast);
   readln(conf,dummy); pzooming:=strtoint(dummy);
   readln(conf,dummy); pspacing:=strtoint(dummy);
   readln(conf,dummy); temperature:=strtoint(dummy);
   readconf_relativeline(3,dummy); csvsep:=dummy;
   readconf_relativeline(2,dummy); closepolicy:=strtoint(dummy);
   CloseFile(conf);
   if opacity<0 then opacity:=0;
   if opacity>100 then opacity:=100;
   if (closepolicy<0) or (closepolicy>4) then closepolicy:=1;
   if color1='' then color1:=ColorToString(PAPPCOL);
   if color2='' then color2:=colortostring(clWindow);
   if color3='' then color3:=ColorToString(PTACOL);
   if color4='' then color4:='$00669999';
   if color5='' then color5:=colortostring(clWindowText);
except
   persistent_source:=resource_path+'rnd';
   graphicsfolder:='themes'+directoryseparator+DEFAULT_THEME+directoryseparator;
   dodirseparators(graphicsfolder);
   opacity:=100;
   color1:=ColorToString(PAPPCOL);
   color2:=colortostring(clWindow);
   color3:=ColorToString(PTACOL);
   color4:='$00669999';
   color5:=colortostring(clWindowText);
   closepolicy:=1;
   pzooming:=100;
   pspacing:=4;
   gridaltcolor:=0;
   highlighttabs:=0;
   alttabstyle:=2;
   ensmall:=0;
end;
Unit_report.color1:=color1;
Unit_report.color2:=color2;
Unit_report.csvsep:=csvsep;
Unit_report.alttabstyle:=alttabstyle;
Unit_report.highlighttabs:=highlighttabs;
Form_pea.LabelOpen.visible:=false;
if (opacity<100) then
   begin
   Form_pea.AlphaBlend:=true;
   Form_pea.AlphaBlendValue:=255+opacity-100;
   end;
PanelDecrypt1.visible:=false;
PanelEncrypt1.visible:=false;
PanelPW1.visible:=false;
PanelRFSinteractive.visible:=false;
PanelTools.visible:=false;
LabelE1.visible:=false;
Panel1.visible:=false;
details:=false;
control:=false;
interacting:=true;
end;

procedure TForm_pea.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var i:integer;
begin
if Form_pea.PanelUtils.visible=false then exit;
if ListMemo.Enabled=false then exit;
for i := 0 to High(FileNames) do
     begin
     ListMemo.Append(FileNames[i]);
     end;
end;

procedure set_items_height;
var
   refsize,rowheight,tabheightl,tabheight,tablabelheight,pbarhsmall:integer;
begin
with Form_pea do
begin
refsize:=ButtonRefSize.Height;
get_pformscaling(refsize,qscale,qscaleimages);
qscale:=(qscale*pzooming) div 100;
qscaleimages:=(qscaleimages*pzooming) div 100;
Width:=560*qscale div 100;
Height:=270*qscale div 100;
Form_report.Width:=800*qscale div 100;
Form_report.Height:=420*qscale div 100;
//tabs
tabheight:=36*qscale div 100;
if (alttabstyle=2) or (alttabstyle=5) then tablabelheight:=30
else tablabelheight:=24;
tablabelheight:=(tablabelheight*qscale) div 100;
tabheightl:=48*qscale div 100;
Form_report.PanelTitleRep.Height:=tabheight;
Form_report.ShapeTitleRepb1.Height:=tablabelheight;
Form_report.ShapeTitleRepb2.Height:=tablabelheight;
Form_report.Panelsp0.Height:=tabheightl;
PanelUtilsTitle.Height:=tabheight;
Panelsp0.Height:=tabheightl;
Panelsp1.Height:=tabheightl;
Panelsp2.Height:=tabheightl;
pbarhsmall:=4*qscale div 100;
Form_report.Shapelinkrep1.Height:=pbarhsmall+2;
Form_report.Shapelinkrep2.Height:=pbarhsmall+2;
//grid
rowheight:=((16+2+pspacing) * qscale) div 100;
Form_report.StringGrid1.DefaultRowHeight:=rowheight;
Form_report.StringGrid2.DefaultRowHeight:=rowheight;
end;
end;

procedure TForm_pea.FormShow(Sender: TObject);
var
  kfun,funutil,i:integer;
begin
if fshown=true then exit;
fshown:=true;
Form_pea.Visible:=false;
{$IFNDEF MSWINDOWS}
menuitem1.visible:=false;
pmrunasadmin.visible:=false;
{$ENDIF}
set_items_height;
if color3='clForm' then color3:=ColorToString(PTACOL);
getpcolors(StringToColor(color1),StringToColor(color2),StringToColor(color3),temperature,contrast);
img_utils.relwindowcolor:=stringtocolor(color2);
load_icons;
Form_pea.Color:=StringToColor(color2);
Form_pea.LabelE1.Font.Color:=pgray;
Form_pea.labelopenfile2.Font.Color:=ptextaccent;
Form_pea.labelopenfile0.Font.Color:=ptextaccent;
Form_report.Color:=StringToColor(color2);
Form_report.ShapeTitleREPb1.Brush.Color:=StringToColor(colhigh);
Form_report.ShapeTitleREPb2.Brush.Color:=StringToColor(colmid);
Form_report.LabelSaveTxt.Font.Color:=ptextaccent;
Form_report.LabelSaveTxt1.Font.Color:=ptextaccent;
case highlighttabs of
   0: begin
      PanelUtilsTitle.Color:=stringtocolor(color2);
      Form_report.PanelTitleREP.Color:=stringtocolor(color2);
      Unit_report.tabpencol:=StringToColor(color2);
      Unit_report.tablowcol:=psilver;
      Unit_report.tabbrushcol:=StringToColor(colmid);
      Unit_report.tabbrushhighcol:=StringToColor(colvlow);
      end;
   1: begin
      PanelUtilsTitle.Color:=stringtocolor(color2);
      Form_report.PanelTitleREP.Color:=stringtocolor(color2);
      Unit_report.tabpencol:=StringToColor(color2);
      Unit_report.tablowcol:=plblue;
      Unit_report.tabbrushcol:=pvvlblue;
      Unit_report.tabbrushhighcol:=pvvvlblue;
      end;
   2: begin
      PanelUtilsTitle.Color:=stringtocolor(collow);
      Form_report.PanelTitleREP.Color:=stringtocolor(collow);
      Unit_report.tabpencol:=StringToColor(collow);
      Unit_report.tablowcol:=psilver;
      Unit_report.tabbrushcol:=StringToColor(colmid);
      Unit_report.tabbrushhighcol:=StringToColor(colvlow);
      end;
   3: begin
      PanelUtilsTitle.Color:=stringtocolor(colmid);
      Form_report.PanelTitleREP.Color:=stringtocolor(colmid);
      Unit_report.tabpencol:=StringToColor(colmid);
      Unit_report.tablowcol:=psilver;
      Unit_report.tabbrushcol:=StringToColor(colhigh);
      Unit_report.tabbrushhighcol:=StringToColor(collow);
      end;
   4: begin
      PanelUtilsTitle.Color:=pvvvlblue;
      Form_report.PanelTitleREP.Color:=pvvvlblue;
      Unit_report.tabpencol:=pvvvlblue;
      Unit_report.tablowcol:=plblue;
      Unit_report.tabbrushcol:=pvvlblue;
      Unit_report.tabbrushhighcol:=pvvvvlblue;
      end;
   5: begin
      PanelUtilsTitle.Color:=pvvlblue;
      Form_report.PanelTitleREP.Color:=pvvlblue;
      Unit_report.tabpencol:=pvvlblue;
      Unit_report.tablowcol:=plblue;
      Unit_report.tabbrushcol:=pvlblue;
      Unit_report.tabbrushhighcol:=pvvvlblue;
      end;
   end;
if gridaltcolor=1 then
   begin
   Form_report.StringGrid1.AlternateColor:=stringtocolor(collow);
   Form_report.StringGrid2.AlternateColor:=stringtocolor(collow);
   end
else
   begin
   Form_report.StringGrid1.AlternateColor:=stringtocolor(color2);
   Form_report.StringGrid2.AlternateColor:=stringtocolor(color2);
   end;
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_rep(Form_report.LabelTitleREP1,Form_report.ShapeTitleREPb1) else clicklabel_rep(Form_report.LabelTitleREP1,Form_report.ShapeLinkREP1);
if paramcount>0 then
   begin
   if upcase(paramstr(1))='PEAUTILS' then
      begin
      try
      kfun:=strtoint(paramstr(2));
      Form_pea.Visible:=True;
      Form_pea.ComboBoxUtils.ItemIndex:=kfun;
      Form_pea.ComboBoxUtilsChange(nil);
      for i:=3 to paramcount do
         Form_pea.ListMemo.Append(paramstr(i));
      except
      Form_pea.ComboBoxUtilsChange(nil);
      end;
      end
   else
      begin
      funutil:=0;
      case upcase(paramstr(1)) of
         'PEA' : call_pea;
         'UNPEA' : call_unpea;
         'RFS' : call_rfs;
         'RFJ' : call_rfj;
         'WIPE' : call_wipe;
         'SANITIZE' : call_sanitize;
         'COMPARE' : call_compare;
         'BENCH' : call_bench;
         'BENCHINT' : call_benchint;
         'CHECK' : call_check;
         'ENVSTR' : call_envstr;
         'LIST' : call_list;
         'HEXPREVIEW' : call_hexpreview;
      else funutil:=1;//internal_error('Incorrect request for Pea, the action "'+paramstr(1)+'" is not supported');
      end;
      if funutil=0 then Form_pea.PanelUtils.visible:=false
      else
         begin
         kfun:=12;
         Form_pea.Visible:=True;
         Form_pea.ComboBoxUtils.ItemIndex:=kfun;
         Form_pea.ComboBoxUtilsChange(nil);
         for i:=1 to paramcount do
            Form_pea.ListMemo.Append(paramstr(i));
         end;
      end;
   end
else
   begin
   Form_pea.Visible:=True;
   Form_pea.ComboBoxUtilsChange(nil);
   end;
end;

procedure TForm_pea.ImageUtilsClick(Sender: TObject);
begin
MessageDlg('Select a function (e.g. hash files) and drag here, or type/paste (one per line), a list of input files/folders if applicable to selected feature.', mtInformation, [mbOK], 0);
end;

procedure TForm_pea.LabelLog1Click(Sender: TObject);
begin
Form_report.Visible:=true;
Form_report.WindowState:=wsNormal;
end;

function cp_open(s:ansistring; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s<>'' then
   {$IFDEF MSWINDOWS}
   w:=utf8decode(s);
   cp_open:=ShellExecuteW(Form_pea.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), 1);//all Windows from 95 and NT3.1
   if cp_open<33 then
      cp_open:=shellexecuteW(Form_pea.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), 1);
   {$ENDIF}
   {$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
   {$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF OPENBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF DARWIN}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure cp_search(desk_env:byte);
begin
{$IFDEF MSWINDOWS}
shellexecutew(Form_pea.handle, PWideChar('find'), PWideChar(''), PWideChar(''), PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_search_linuxlike(desk_env);{$ENDIF}//try to search via Gnome or KDE
{$IFDEF FREEBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF OPENBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF DARWIN}cp_search_linuxlike(desk_env);{$ENDIF}
end;

procedure TForm_pea.LabelOpenClick(Sender: TObject);
begin
cp_open(output,desk_env);
end;

procedure TForm_pea.labelopenfile0Click(Sender: TObject);
begin
cp_search(desk_env);
end;

procedure TForm_pea.labelopenfile2Click(Sender: TObject);
var i:integer;
begin
   if OpenDialog2.execute then
      if OpenDialog2.FileName<>'' then
         begin
         for i := 0 to OpenDialog2.Files.Count-1 do
            begin
            ListMemo.Append(OpenDialog2.Files[i]);
            end;
         end;
end;

procedure TForm_pea.mainmenuhelpClick(Sender: TObject);
begin
cp_open(FIRSTDOM+'peazip-help.html',desk_env);
end;

procedure TForm_pea.pmupdatesClick(Sender: TObject);
begin
cp_open(FIRSTDOM,desk_env);
end;

procedure TForm_pea.pmdonationsClick(Sender: TObject);
begin
cp_open(FIRSTDOM+'donations.html',desk_env);
end;

procedure TForm_pea.PanelPW1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   i:integer;
   st:string;
begin
sample_mouse_ent(ment,x,y);
ment_sample:=ment;
SHA256Final(ment_sample,mentd_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@mentd_sample[i],1);
LabelSample1.Caption:=st;
end;

procedure TForm_pea.peautilsbtnClick(Sender: TObject);
var
   pp:TPoint;
begin
      pp.x:=peautilsbtn.Left+peautilsbtn.Width;
      pp.y:=peautilsbtn.top+peautilsbtn.height;
      pp:=clienttoscreen(pp);
      peautilsmenu.Alignment:=paRight;
      peautilsmenu.PopUp(pp.x,pp.y);
      peautilsmenu.Alignment:=paLeft;
end;

procedure TForm_pea.pmhelpClick(Sender: TObject);
begin
cp_open(FIRSTDOM+'peazip-help.html',desk_env);
end;

procedure TForm_pea.pmrunasadminClick(Sender: TObject);
var
   w:widestring;
begin
{$IFDEF MSWINDOWS}
w:=utf8decode('"'+executable_path+'pea.exe"');
ShellExecuteW(Form_pea.Handle, PWideChar ('runas'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
Close;
{$ENDIF}
end;

procedure TForm_pea.Timer1Timer(Sender: TObject); //gives the time to draw the UI before the CPU intensive task begin
begin
if control=true then exit;
if interacting=true then exit;
control:=true;
parse_action;
end;

initialization
  {$I unit_pea.lrs}
  
  {$IFDEF MSWINDOWS}
  OleInitialize(nil);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  OleUninitialize
  {$ENDIF}
  
end.
