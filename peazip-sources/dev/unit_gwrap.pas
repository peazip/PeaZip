unit Unit_gwrap;
{
 DESCRIPTION     :  PeaLauncher, component launching 7za/7z console application,
                    providing GUI for:
                    displaying output of console using pipes;
                    displaying and explain job exitcode.

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20061102  G.Tani      Initial version
 0.11     20061130  G.Tani
 0.12     20070117  G.Tani
 0.13     20070130  G.Tani
 0.14     20070226  G.Tani
 0.15     20070403  G.Tani
 0.16     20070607  G.Tani
 0.17     20070716  G.Tani
 0.18     20070804  G.Tani
 0.19     20071001  G.Tani
 0.20     20071030  G.Tani
 0.20b    20071124  G.Tani
 0.21     20080319  G.Tani
 0.22     20080510  G.Tani
 0.23     20080730  G.Tani
 0.24     20080922  G.Tani
 0.25     20081027  G.Tani
 0.26     20081118  G.Tani
 0.27     20090124  G.Tani
 0.28     20090215  G.Tani
 0.29     20090330  G.Tani
 0.30     20090508  G.Tani
 0.31     20090713  G.Tani
 0.32     20090825  G.Tani
 0.33     20090913  G.Tani
 0.34     20091016  G.Tani
 0.35     20091103  G.Tani
 0.36     20091109  G.Tani
 0.37     20091125  G.Tani
 0.38     20100108  G.Tani
 0.39     20100125  G.Tani
 0.40     20100207  G.Tani
 0.41     20100217  G.Tani
 0.42     20100313  G.Tani
 0.43     20100424  G.Tani
 0.44     20100602  G.Tani
          20100907  G.Tani
 0.45     20101014  G.Tani
 0.46     20101113  G.Tani
 0.47     20101224  G.Tani
 0.48     20110402  G.Tani
 0.49     20110426  G.Tani
 0.50     20110727  G.Tani
 0.51     20110813  G.Tani
 0.52     20110915  G.Tani
 0.53     20111016  G.Tani
 0.54     20111110  G.Tani
          20111224  G.Tani
 0.55     20120115  G.Tani
 0.56     20120315  G.Tani
          20120515  G.Tani
 0.57     20120807  G.Tani      (Windows) Added semaphore to perform one operation at time in order to optimize disk performances for some operations
          20120818  G.Tani      Uniformed Button Panels design over the application
 0.58     20120916  G.Tani      More information given on extraction, list and test jobs
 0.59     20121104  G.Tani      PeaLauncher (in standard mode) inherits previous instances minimized/normal status
                                New high definition Windows icon
 0.60     20130220  G.Tani      New theming engine
 0.61     20130310  G.Tani      Minor fixes in fallback if text file is not found
 0.62     20130617  G.Tani      Code cleanup, various usability improvements (new layout, input and output pats are linked)
 0.63     20130718  G.Tani      (Linux) Fixed opening input/output paths
                                Recompiled with Lazarus 1.0.10
 0.64     20130823  G.Tani      Various minor fixes
 0.65     20130922  G.Tani      Various minor fixes, recompiled with Lazarus 1.0.12
 0.66     20131106  G.Tani      Added context menu for input/output links featuring entries to explore and search the system, and pause/stop the task
 0.67     20140222  G.Tani      Improved standalone extraction launcher GUI
 0.68     20140403  G.Tani      New progress bar, visual updates
                                New synthetic Info mode
                                Recompiled with Lazarus 1.2.0
 0.69     20140416  G.Tani      Info function error handling
 0.70     20140703  G.Tani      New, lighter layout meant to better focus on relevant information
                                Various minor fixes
 0.71     20140810  G.Tani      Visual update for lighter layout but preserving tabs for quicker switching
 0.72     20141015  G.Tani      Priority setting moved to dropdown menu near OK/Cancel buttons
                                Improved stopping tasks
                                 Cancel all option in dropdown menu allows to stop all following tasks
                                 Cancel action reports to application's form rather than using a separate message box
 0.73     20141220  G.Tani      Improved system benchmark output presentation
 0.74     20150108  G.Tani      Fixed bug in Info function with newer 7z versions
 0.75     20150311  G.Tani      Improved usability as standalone application (PeaExtractor 1.0)
                                Added menu for online resources (updates, help...) and other options
                                Added ability to set program's localization and system context menu language when used as standalone application
 0.76     20150322  G.Tani      Fixed access to online help and resources when compiled as PeaExtractor
                                Improved Open file dialog adding custom file extension filters for common archive types
 0.77     20150425  G.Tani      Introduced support for ZPAQ 7.05 and BCM 1.0
 0.78     20150618  G.Tani      Updated Info function for new 7z/p7zip backend versions
 0.79     20150718  G.Tani      Recompiled for PeaZip 5.7 with Lazarus 1.4.0
 0.80     20150915  G.Tani      Improved UTF8 compatibility, recompiled for PeaZip 5.8
 0.81     20151010  G.Tani      Fixed use clWindow for keeping the application consistent with system's visual preferences
 0.82     20151121  G.Tani      Updated to use p7zip 15.x line, minor fixes, recompiled for PeaZip 5.9
 0.83     20160221  G.Tani      Recompiled for Lazarus 1.6.0 / FPC3 with full support for Unicode file/dir names in Windows
                                Code cleanup to remove unnecessary character encoding handling functions
                                New Ten theme set as default
 0.84     20160809  G.Tani      Various updates for standalone PeaExtractor 1.2
 0.85     20160909  G.Tani      Various updates for standalone PeaExtractor 1.3
 0.86     20161022  G.Tani      Various visual updates for standalone PeaExtractor 1.3
 0.87     20161124  G.Tani      DPI awareness improvements
 0.88     20170325  G.Tani      Recompiled for PeaZip 6.4, visual updates
 0.89     20170415  G.Tani      UI fixes
 0.90     20170930  G.Tani      Recompiled for 6.5, translated message dialogs, new progress evaluation algorithm, visual updates
 0.91     20180209  G.Tani      Recompiled with Lazarus 1.8.0 and updated WE libs
                                Minor updates
 0.92     20180503  G.Tani      Recompiled for 6.6
 0.93     20181024  G.Tani      Various fixes
 0.94     20190203  G.Tani      Updated to Wolfgang Ehrhardt math library util_2018-11-27
                                Moved from standalone executable to form of PeaZip
                                 Can now accept single or multiple tasks in same instance
                                 (Windows) when elevation is required only the backend task
                                 is elevated, and each task is elevated individually with a
                                 separate prompt (unless the main application itself is running as elevated)
 0.95     20190226  G.Tani      Added "Cancel all" button
                                Various fixes and improvements
 0.96     20190326  G.Tani      Added ability to reduce to rtay and control from context menu
                                Moved both progress bars on the bottom
 0.97     20190422  G.Tani      Various improvements
 0.98     20190422  G.Tani      Launcher now save position and size
 0.99     20190613  G.Tani      Code cleanup, improved progress bar, improved test/list speed, various fixes
 1.00     20190815  G.Tani      Added estimated remaining time
 1.01     20191102  G.Tani      Improved progress bar and displaying of size of the current / total task
                                Fixed benchmark
                                Various fixes
 1.02     20200127  G.Tani      Added support for Brotli and Zstandard in compression, extraction amd test mode
                                Various fixes for displaying operations of single file compression formats
 1.03     20200415  G.Tani      Fixes
 1.04     20200509  G.Tani      Cancel all button (and options dropdown menu) are no longer hidden when task ends
 1.05     20200528  G.Tani      Improved progress bar accuracy for multiple tasks
 1.06     20200902  G.Tani      Fixed wrong error 127 report for some cases, fixed typos
 1.07     20210305  G.Tani      Supports Windows 7+ progress bar in application's status bar icon
 1.08     20210706  G.Tani      Fixed window stealing focus at each new task, occurring on some Linux machines
                                Optimized speed for operations on archives containing large number of files
 1.09     20210923  G.Tani      Merged patches for Darwin support
                                Optimized memory usage for progress and report streams
 1.10     20240204  G.Tani      Improved translations loading

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com

The program is released under GNU LGPL http://www.gnu.org/licenses/lgpl.txt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, activex, shellapi, ShlObj, comobj, Win32Int, InterfaceBase, LCLIntf,
  {$ENDIF}
  Unit7, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, strutils,
  StdCtrls, Buttons, Process, UTF8Process, Menus,
  pea_utils, list_utils, ansiutf8_utils, img_utils, Grids, Math;

type

  { TForm_gwrap }

  TForm_gwrap = class(TForm)
    ButtonStop1: TSpeedButton;
    ButtonStopAll: TBitBtn;
    cbAutoOpen: TCheckBox;
    CheckBoxHalt: TCheckBox;
    ImageButton2: TLabel;
    ImageSavePJ: TLabel;
    ImageKeep: TLabel;
    Button1: TBitBtn;
    ButtonStop: TBitBtn;
    ButtonPause: TBitBtn;
    Imagestatus: TImage;
    l1: TLabel;
    l2: TLabel;
    l3: TLabel;
    l4: TLabel;
    l5: TLabel;
    l6: TLabel;
    l7spc: TLabel;
    Label1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelInfo3: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Labeli: TLabel;
    LabelInfo1: TLabel;
    LabelInfo2: TLabel;
    Labelspac: TLabel;
    Labelo: TLabel;
    LabelTitle1: TLabel;
    LabelTitle2: TLabel;
    LabelTitle3: TLabel;
    LabelTitle4: TLabel;
    LabelWarning1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    pmet: TMenuItem;
    pm2et: TMenuItem;
    PanelTitlePLTabAlign: TPanel;
    pm2pause: TMenuItem;
    pm2cancel: TMenuItem;
    pm2cancelall: TMenuItem;
    pm2search: TMenuItem;
    pm2explore: TMenuItem;
    pm2eo: TMenuItem;
    pm2ei: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    pm2restore: TMenuItem;
    N1: TMenuItem;
    pmbackground: TMenuItem;
    OpenDialog2: TOpenDialog;
    PanelTitle: TPanel;
    Notebook1: TPanel;
    Page1: TPanel;
    Page2: TPanel;
    Page3: TPanel;
    Page4: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelBench: TPanel;
    pmei: TMenuItem;
    pmeo: TMenuItem;
    pmexplore: TMenuItem;
    pmsearch: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ShapeGlobalProgress: TPanel;
    Shapelink1: TShape;
    Shapelink2: TShape;
    Shapelink3: TShape;
    Shapelink4: TShape;
    ShapeProgress: TPanel;
    ShapeTitleb1: TShape;
    ShapeTitleb2: TShape;
    ShapeTitleb3: TShape;
    ShapeTitleb4: TShape;
    StringGrid1: TStringGrid;
    Timer2: TTimer;
    TrayIcon1: TTrayIcon;
    procedure ButtonStop1Click(Sender: TObject);
    procedure ButtonStopAllClick(Sender: TObject);
    procedure cbAutoOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageKeepClick(Sender: TObject);
    procedure ImageSavePJClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ImageButton2Click(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure l2Click(Sender: TObject);
    procedure l4Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure labelopenfile0Click(Sender: TObject);
    procedure LabelTitle1Click(Sender: TObject);
    procedure LabelTitle1MouseEnter(Sender: TObject);
    procedure LabelTitle1MouseLeave(Sender: TObject);
    procedure LabelTitle2Click(Sender: TObject);
    procedure LabelTitle2MouseEnter(Sender: TObject);
    procedure LabelTitle2MouseLeave(Sender: TObject);
    procedure LabelTitle3Click(Sender: TObject);
    procedure LabelTitle3MouseEnter(Sender: TObject);
    procedure LabelTitle3MouseLeave(Sender: TObject);
    procedure LabelTitle4Click(Sender: TObject);
    procedure LabelTitle4MouseEnter(Sender: TObject);
    procedure LabelTitle4MouseLeave(Sender: TObject);
    procedure LabelWarning1Click(Sender: TObject);
    procedure pm2cancelallClick(Sender: TObject);
    procedure pm2cancelClick(Sender: TObject);
    procedure pm2eiClick(Sender: TObject);
    procedure pm2eoClick(Sender: TObject);
    procedure pm2etClick(Sender: TObject);
    procedure pm2exploreClick(Sender: TObject);
    procedure pm2pauseClick(Sender: TObject);
    procedure pm2restoreClick(Sender: TObject);
    procedure pm2searchClick(Sender: TObject);
    procedure pmbackgroundClick(Sender: TObject);
    procedure pmeiClick(Sender: TObject);
    procedure pmeoClick(Sender: TObject);
    procedure pmetClick(Sender: TObject);
    procedure pmexploreClick(Sender: TObject);
    procedure pmsearchClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { private declarations }
    {$IFDEF MSWINDOWS}
    FTaskBarList: ITaskbarList3;
    AppHandle: THandle;
    {$ENDIF}
  public
    { public declarations }
  end;

procedure gwraplaunch;
procedure settheme;
procedure deselectlabels_launcher;
procedure explore_out(pt:ansistring);

const
  WS_EX_LAYERED = $80000;
  LWA_ALPHA     = $2;
  STR_STOPALL   = '.pstopalltmp';
  {$IFDEF MSWINDOWS}
  DEFAULT_THEME = 'main-embedded';
  {$ELSE}
  DEFAULT_THEME = 'main-embedded';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  EXEEXT        = '.exe';
  {$ELSE}
  EXEEXT        = '';
  {$ENDIF}

var
  Form_gwrap: TForm_gwrap;
  pprogn,pjobtype,ptsize,ppsize,pinputfile,poutname,poutnamet,pcl,paction,pcapt,pbackground,psubfun,pfun:ansistring;
  pprogbar,pprogbarprev,perrors,iperc,ipercp,remtime,temperature,contrast,alttabstyle,highlighttabs,
  modeofuse,max_l,ppriority,autoopen,exit_code,ws_gw_top,ws_gw_left,ws_gw_height,ws_gw_width,
  pbarh,pbarhsmall,pjobcommentrar,pjobuserar:integer;
  pproglast,pprogfirst,pfromnativedrag,runelevated,pgook,perrignore,pcanignore,launched,
  stopped,ended,ppause,pstarted,launchwithsemaphore,gocancelall, needinteraction,
  exbackground,pldesigned,okseven:boolean;
  Binfo,Bp1,Bp2,Bp3,Bp4,Bp5,Bp6,Bp7,Bp8,Bsuccess,Berror: TBitmap;
  cl,cl1,outpath,executable_path,resource_path,binpath,sharepath,graphicsfolder,dummy,Color1,Color2,Color3,
  Color4,Color5,caption_build,delimiter,confpath,peazippath,in_name,peazipver,rarcomment:ansistring;
  insize,progress,pinsize:qword;
  opacity,desk_env,pcount,optype,filesizebase,pautoclose:byte;
  T,conf:text;
  f:file of byte;
  tsin:TTimestamp;
  activelabel_launcher :TLabel;
  //imported strings
  txt_7_4_recover,txt_rr,txt_7_8_dd,txt_8_2_keep:ansistring;
  //translations
  txt_6_9_remaining,txt_6_5_abort,txt_6_5_error,txt_6_5_no,txt_6_5_yes,txt_6_5_yesall,txt_6_5_warning,
  txt_5_6_update,txt_5_6_cml,txt_5_6_donations,txt_5_6_localization,txt_5_6_runasadmin,
  txt_5_6_help,
  txt_5_5_cancelall,
  txt_5_3_details,txt_5_3_files,txt_5_3_folders,txt_5_3_os,txt_5_3_ps,txt_5_3_info,
  txt_5_3_list,txt_5_3_test,
  txt_5_0_extract,txt_5_0_from,txt_5_0_in,txt_5_0_to,
  txt_4_5_search,
  txt_4_0_dragorselect,txt_4_0_drag,txt_4_0_select,
  txt_3_6_selectdir,
  txt_3_5_close,
  txt_3_0_details,txt_3_0_hints,txt_3_0_arc,txt_3_0_ext,
  txt_2_8_oop,
  txt_2_7_validatecl,txt_2_7_validatefn,
  txt_2_6_open,
  txt_2_5_ace_missing,
  txt_2_3_pw_errorchar_gwrap,txt_2_3_cancel,txt_2_3_encryption,txt_2_3_extinnew,
  txt_2_3_keyfile,txt_2_3_kf_not_found_gwrap,txt_2_3_moreoptions,txt_2_3_nopaths,
  txt_2_3_pw,txt_2_3_skipexisting,txt_2_3_overexisting,txt_2_3_renameextracted,
  txt_2_3_renameexisting,txt_2_3_options,
  txt_status,txt_jobstatus,txt_rating,txt_threads,txt_input,
  txt_output,txt_time,txt_isrunning,txt_autoclose,txt_halt,txt_report,txt_console,
  txt_explore,txt_ok,txt_stop,txt_pause,txt_rt,txt_high,txt_normal,txt_idle,txt_priority,
  txt_savejob,txt_savelog,txt_bench,txt_saveas,txt_job_success,txt_job1,txt_job2,
  txt_job7,txt_job8,txt_job127,txt_job255,txt_job_unknown,txt_benchscale,txt_lt,txt_extto,
  txt_create,txt_nocl,txt_job_started,txt_jobstopped,txt_jstopped,txt_jpaused,txt_jresumed,
  txt_p_realtime,txt_p_high,txt_p_normal,txt_p_idle,txt_paused,txt_running,txt_speedscale,
  txt_crscale,txt_done,txt_halted,txt_error,txt_hardware,txt_software,txt_resume,
  txt_stdjob,txt_benchmarkjob,txt_defragjob,txt_consolejob,lang_file,lver,wincomspec,
  winver:ansistring;
  tabpencol,tablowcol,tabbrushcol,tabbrushhighcol:tcolor;
  {$IFDEF MSWINDOWS}
  //semaphore
  psem: THandle;
  {$ENDIF}

implementation

///launcher

procedure setpanel_launcher(i:integer);
begin
with Form_gwrap do
begin
imagebutton2.visible:=false;
imagesavepj.visible:=false;
imagekeep.visible:=false;
case i of
   1: begin
      Page1.Visible:=true;
      Page2.Visible:=false;
      Page3.Visible:=false;
      Page4.Visible:=false;
      if ImageKeep.Caption=txt_8_2_keep then ImageKeep.visible:=true;
      end;
   2: begin
      Page1.Visible:=false;
      Page2.Visible:=true;
      Page3.Visible:=false;
      Page4.Visible:=false;
      imagebutton2.visible:=true;
      end;
   3: begin
      Page1.Visible:=false;
      Page2.Visible:=false;
      Page3.Visible:=true;
      Page4.Visible:=false;
      imagesavepj.visible:=true;
      end;
   4: begin
      Page1.Visible:=false;
      Page2.Visible:=false;
      Page3.Visible:=false;
      Page4.Visible:=true;
      end;
   end;
end;
end;

procedure exitlabel_launcher(var a: TLabel; var b:TShape);
begin
if activelabel_launcher=a then exit;
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   b.Brush.Color:=tabbrushcol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end
else
   begin
   b.Brush.Color:=tabpencol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end;
if (highlighttabs=1) or (highlighttabs=4) or (highlighttabs=5) then a.Font.Color:=clDefault else a.Font.Color:=pGray;
end;

procedure deselectlabels_launcher;
begin
with Form_gwrap do
begin
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   exitlabel_launcher(LabelTitle1,ShapeTitleb1);
   exitlabel_launcher(LabelTitle2,ShapeTitleb2);
   exitlabel_launcher(LabelTitle3,ShapeTitleb3);
   exitlabel_launcher(LabelTitle4,ShapeTitleb4);
   end
else
   begin
   exitlabel_launcher(LabelTitle1,Shapelink1);
   exitlabel_launcher(LabelTitle2,Shapelink2);
   exitlabel_launcher(LabelTitle3,Shapelink3);
   exitlabel_launcher(LabelTitle4,Shapelink4);
   end;
end;
end;

procedure setlabelpanel_launcher(var a: Tlabel);
begin
with Form_gwrap do
begin
if a = LabelTitle1 then setpanel_launcher(1);
if a = LabelTitle2 then setpanel_launcher(2);
if a = LabelTitle3 then setpanel_launcher(3);
if a = LabelTitle4 then setpanel_launcher(4);
end;
end;

procedure clicklabel_launcher(var a: TLabel; var b:TShape);
begin
activelabel_launcher:=a;
deselectlabels_launcher;
if (alttabstyle<>1) and (alttabstyle<>4) then
   begin
   b.Brush.Color:=StringToColor(color2);
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end
else
   begin
   b.Brush.Color:=tablowcol;
   b.Pen.Color:=tabpencol;
   b.Pen.Style:=psSolid;
   end;
if ((alttabstyle=1) or (alttabstyle=4)) and ((highlighttabs=1) or (highlighttabs=4) or (highlighttabs=5)) then a.Font.Color:=ptextaccent else a.Font.Color:=clDefault;
setlabelpanel_launcher(a);
end;

procedure enterlabel_launcher(var a: TLabel; var b:TShape);
begin
if activelabel_launcher=a then exit;
b.Brush.Color:=tabbrushhighcol;
b.Pen.Color:=tabpencol;
b.Pen.Style:=psSolid;
a.Font.Color:=clDefault;
end;

///

procedure conf_critical_error_msg; //hardcoded
begin
pMessageErrorOK('Cannot find or parse critical configuration files (probably because deleted, moved or corrupted); PeaZip should be reinstalled');
end;

procedure lang_critical_error_msg; //hardcoded
begin
pMessageErrorOK('Cannot parse language file '+lang_file+' and will now try to fall back to default language file default.txt');
end;

function valorize_text:integer;
var
   s:ansistring;
begin
valorize_text:=-1;
try
readln(t,s);
readln(t,s); txt_6_9_remaining:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_abort:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_error:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_no:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_warning:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_yes:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_6_5_yesall:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_update:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_cml:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_donations:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_localization:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_runasadmin:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_6_help:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_5_cancelall:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_details:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_files:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_folders:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_info:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_list:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_os:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_ps:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_3_test:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_extract:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_from:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_in:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_to:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_5_search:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_drag:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_dragorselect:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_select:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_6_selectdir:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_5_close:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_details:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_hints:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_arc:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_ext:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_8_oop:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_7_validatefn:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_7_validatecl:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_6_open:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_5_ace_missing:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_pw_errorchar_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_renameexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_renameextracted:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_cancel:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_encryption:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_extinnew:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_keyfile:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_kf_not_found_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_moreoptions:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_nopaths:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_options:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_overexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_pw:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_skipexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_unknown:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_stdjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_benchmarkjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_defragjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_consolejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job1:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job127:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job2:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job255:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job7:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job8:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_autoclose:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_crscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_console:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_benchscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_create:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_done:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_nocl:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_error:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_explore:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_extto:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_halt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_halted:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_hardware:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_input:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jpaused:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jresumed:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_started:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jobstatus:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jobstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_success:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_lt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_ok:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_output:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_pause:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_paused:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_realtime:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_rating:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_rt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_report:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_resume:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_priority:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_running:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_isrunning:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_saveas:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_savejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_savelog:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_software:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_speedscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_status:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_stop:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_bench:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_threads:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_time:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s);
readln(t,s); if s<>'=== end PeaLauncher text group ===' then exit;
valorize_text:=0;
except
valorize_text:=-1;
end;
end;

function valorize_headers:integer;
var
   s:ansistring;
begin
valorize_headers:=-1;
readln(t,s);//translator(s)
readln(t,s);//last revision's translator(s)
readln(t,s);//last revision date
readln(t,s);
readln(t,s);//peazip text group
if s<>'=== PeaZip text group ===' then exit
else valorize_headers:=0;
end;

procedure prepare_FormDlg;
begin
Unit7.txt_info:=txt_5_3_info;
Unit7.txt_2_7_ok:=txt_ok;
Unit7.txt_2_7_cancel:=txt_2_3_cancel;
Unit7.txt_6_5_warning:=txt_6_5_warning;
Unit7.txt_6_5_error:=txt_6_5_error;
Unit7.txt_6_5_yesall:=txt_6_5_yesall;
Unit7.txt_6_5_yes:=txt_6_5_yes;
Unit7.txt_no:=txt_6_5_no;
Unit7.txt_6_5_abort:=txt_6_5_abort;
end;

procedure assign_guitext;
begin
with Form_gwrap do
begin
ImageKeep.Caption:='';
labelWarning1.Caption:=txt_3_0_hints;
LabelTitle1.Caption:='      '+txt_status+'      ';
LabelTitle2.Caption:='      '+txt_report+'      ';
LabelTitle3.Caption:='      '+txt_console+'      ';
LabelTitle4.Caption:='      '+txt_2_3_options+'      ';
Label1.Caption:=txt_isrunning+', ';
Labeli.Caption:=txt_input+' ';
Labelo.Caption:=txt_output+' ';
CheckBoxHalt.Caption:=txt_halt;
cbAutoOpen.Caption:=txt_2_8_oop;
Button1.Caption:='   '+txt_ok+'   ';
ButtonStop.Caption:='   '+txt_2_3_cancel+'   ';
ButtonStopAll.Caption:='   '+txt_5_5_cancelall+'   ';
ButtonPause.Caption:='   '+txt_pause+'   ';
ImageSavePJ.Caption:=txt_savejob;
ImageButton2.Caption:=txt_savelog;
Label2.Hint:=txt_benchscale;
OpenDialog1.Title:=txt_2_6_open;
SaveDialog1.Title:=txt_saveas;
SaveDialog2.Title:=txt_saveas;
SelectDirectoryDialog1.Title:=txt_3_6_selectdir;
pmexplore.caption:=txt_explore+'...';
pmsearch.caption:=txt_4_5_search;
pm2explore.caption:=txt_explore+'...';
pm2search.caption:=txt_4_5_search;
pm2cancel.Caption:=txt_2_3_cancel;
pm2cancelall.Caption:=txt_5_5_cancelall;
pm2pause.Caption:=txt_pause;
pmbackground.Caption:=pbackground;
//prepare_FormDlg;
end
end;

function load_texts(lang:ansistring):integer; //valorize localized text strings
var
   s:ansistring;
   i:integer;
begin
load_texts:=-1;
try
   assignfile(t,(sharepath+'lang'+Directoryseparator+lang));
   filemode:=0;
   reset(t);
   read_header(t);
   readln(t,s); //declaration
   if (s<>'=== PeaZip language file ===') and (s<>'== PeaZip language file ===') then
      begin
      closefile(t);
      exit;
      end;
   readln(t,s);//language
   readln(t,lver);//version
   i:=valorize_headers;
   repeat //skip until about text group
      readln(t,s);
   until (eof(t)) or (s='=== end PeaZip text group ===');
   if eof(t)=true then
      begin
      closefile(t);
      exit;
      end;
   readln(t,s);//empty
   readln(t,s);//declaration
   if s<>'=== PeaLauncher text group ===' then
      begin
      closefile(t);
      exit;
      end;
   if i=0 then i:=valorize_text;
   if i=0 then assign_guitext
   else
      begin
      closefile(t);
      exit;
      end;
   closefile(t);
   load_texts:=0;
except
   try
   closefile(t);
   except
   end;
   load_texts:=-1;
end;
end;

procedure load_default_texts;
begin
txt_6_9_remaining:='remaining';
txt_6_5_abort:='Abort';
txt_6_5_error:='Error';
txt_6_5_no:='No';
txt_6_5_warning:='Warning';
txt_6_5_yes:='Yes';
txt_6_5_yesall:='Yes to all';
txt_5_6_update:='Check for updates';
txt_5_6_cml:='System context menu language';
txt_5_6_donations:='Donations';
txt_5_6_localization:='Localization';
txt_5_6_runasadmin:='Run as administrator';
txt_5_6_help:='Online support';
txt_5_5_cancelall:='Cancel all';
txt_5_3_details:='Details';
txt_5_3_files:='files';
txt_5_3_folders:='folders';
txt_5_3_info:='Info';
txt_5_3_list:='List';
txt_5_3_os:='Original size';
txt_5_3_ps:='Packed size';
txt_5_3_test:='Test';
txt_5_0_extract:='Extract';
txt_5_0_from:='from';
txt_5_0_in:='in';
txt_5_0_to:='to';
txt_4_5_search:='Search';
txt_4_0_drag:='Drag here the archive to extract, or ';
txt_4_0_dragorselect:='Drag here or select file';
txt_4_0_select:='select file';
txt_3_6_selectdir:='Select directory';
txt_3_5_close:='Close';
txt_3_0_details:='For more details please see "Report" tab for the full task''s log, and "Console" tab for task definition as command line.';
txt_3_0_hints:='Hints about the error';
txt_3_0_arc:='Possible causes of the error may be non readable input files (locked, not accessible, corrupted, missing volumes in multipart archive...), or full or not accessible output path.';
txt_3_0_ext:='The archive may require a different password for the current operation.';
txt_2_8_oop:='Open output path when task completes';
txt_2_7_validatefn:='Operation stopped, invalid file name detected:';
txt_2_7_validatecl:='Operation stopped, potentially dangerous command detected (i.e. command concatenation not allowed within the program):';
txt_2_6_open:='Open archive';
txt_2_5_ace_missing:='UNACE plugin is missing; for handling ACE archives you can download the plugin form PeaZip''s website (being UNACE closed source, the plugin is not featured in base package)';
txt_2_3_pw_errorchar_gwrap:='quote character cannot be used by PeaLauncher in passwords under current system, please change password or chose Console mode in Backend binaries user interface in Options > Settings';
txt_2_3_renameexisting:='Auto rename existing files';
txt_2_3_renameextracted:='Auto rename extracted files';
txt_2_3_cancel:='Cancel';
txt_2_3_encryption:='Encryption';
txt_2_3_extinnew:='Extract in new folder';
txt_2_3_keyfile:='Keyfile';
txt_2_3_kf_not_found_gwrap:='Keyfile cannot be found or read. Please chose a different Keyfile.';
txt_2_3_moreoptions:='More options...';
txt_2_3_nopaths:='No paths';
txt_2_3_options:='Options';
txt_2_3_overexisting:='Overwrite existing files';
txt_2_3_pw:='Password';
txt_2_3_skipexisting:='Skip existing files';
txt_job_unknown:=': Unknown error encountered';
txt_stdjob:='[animation will stop at task''s completion]';
txt_benchmarkjob:='[system will respond slowly while running the benchmark (some minutes)]';
txt_defragjob:='[will not respond while defragmenting, you can let it run in background]';
txt_consolejob:='[task''s feedback and detailed progress available in console window]';
txt_job1:='1: Warning, non fatal error(s); i.e. some files missing or locked';
txt_job127:='127: Cannot execute requested operation';
txt_job2:='2: Fatal error occurred';
txt_job255:='255: Task halted by the user';
txt_job7:='7: Error, got incorrect command line';
txt_job8:='8: Error, not enough memory for requested operation';
txt_autoclose:='Close this window when task completes';
txt_crscale:='Compression ratio (lower, better):';
txt_console:='Console';
txt_benchscale:='Core 2 Duo 6600 rating, equivalent MHz speed.';
txt_create:='Create';
txt_done:='Done:';
txt_nocl:='Empty command line';
txt_error:='Error:';
txt_explore:='Explore';
txt_extto:='Extract to';
txt_halt:='Halt system when task completes';
txt_halted:='Halted:';
txt_hardware:='hardware';
txt_high:='high priority';
txt_idle:='idle priority';
txt_input:='Input:';
txt_jpaused:='Task paused';
txt_jresumed:='Task resumed';
txt_job_started:='Task started';
txt_jobstatus:='Task status:';
txt_jstopped:='Task halted by the user';
txt_jobstopped:='Task halted by the user; you can inspect the partial outcome clicking output path link.';
txt_job_success:='Task successfully completed';
txt_lt:='List/test';
txt_normal:='normal priority';
txt_ok:='OK';
txt_output:='Output:';
txt_pause:='Pause';
txt_paused:='Paused,';
txt_p_high:='Priority set to high';
txt_p_idle:='Priority set to idle';
txt_p_normal:='Priority set to normal';
txt_p_realtime:='Priority set to real time';
txt_rating:='Rating:';
txt_rt:='real time priority';
txt_report:='Report';
txt_resume:='Resume';
txt_priority:='Click to set task''s priority';
txt_running:='Running,';
txt_isrunning:='Running...';
txt_saveas:='Save as';
txt_savejob:='Save task definition as script';
txt_savelog:='Save task''s log';
txt_software:='software';
txt_speedscale:='Speed, logarithmic scale (higher, better):';
txt_status:='Status';
txt_stop:='Stop';
txt_bench:='System benchmark';
txt_threads:='Threads:';
txt_time:='Time:';
end;

function texts(lang:ansistring):integer;
begin
if load_texts(lang)<>0 then //try to load language file
   begin
   load_default_texts;
   assign_guitext;
   texts:=0;
   end
else texts:=0;
end;

function cp_open(s:ansistring; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s='' then exit;
if validatecl(s)<>0 then begin pMessageWarningOK(txt_2_7_validatecl+' '+s); exit; end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(Form_gwrap.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(Form_gwrap.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF OPENBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF DARWIN}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure save_report;
var
   s,p:ansistring;
   i:integer;
begin
s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+'job_log.txt';
Form_gwrap.SaveDialog1.FileName:=s;
if outpath<>'' then
   if checkdirexists(outpath) then Form_gwrap.SaveDialog1.InitialDir:=outpath
   else Form_gwrap.SaveDialog1.InitialDir:=extractfilepath(outpath);
if Form_gwrap.SaveDialog1.Execute then
   begin
   s:=Form_gwrap.SaveDialog1.FileName;
   assignfile(t,(s));
   rewrite(t);
   write_header(t);
   for i:=0 to Form_gwrap.StringGrid1.Rowcount-1 do writeln(t,Form_gwrap.StringGrid1.Cells[0,i]);
   writeln(t,'');
   writeln(t,Form_gwrap.Label1.Caption);
   if modeofuse=2 then
      begin
      writeln(t,'');
      writeln(t,Form_gwrap.Label2.Caption);
      writeln(t,Form_gwrap.Label3.Caption);
      writeln(t,Form_gwrap.Label7.Caption);
      writeln(t,Form_gwrap.Label8.Caption);
      end;
   closefile(t);
   p:=(getcurrentdir);
   if p<>'' then
      if p[length(p)]<>DirectorySeparator then p:=p+DirectorySeparator;
   end;
end;

procedure save_cl;
var
s,p:ansistring;
begin
s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+'job_definition.txt';
Form_gwrap.SaveDialog1.FileName:=s;
if outpath<>'' then
   if checkdirexists(outpath) then Form_gwrap.SaveDialog1.InitialDir:=outpath
   else Form_gwrap.SaveDialog1.InitialDir:=extractfilepath(outpath);
if Form_gwrap.SaveDialog1.Execute then
   begin
   s:=Form_gwrap.SaveDialog1.FileName;
   assignfile(t,(s));
   rewrite(t);
   write_header(t);
   writeln(t,cl);
   closefile(t);
   p:=(getcurrentdir);
   if p<>'' then
      if p[length(p)]<>DirectorySeparator then p:=p+DirectorySeparator;
   end;
end;

procedure decode_exitcode(i:integer; var s:ansistring);
begin
case i of
   0: s:=txt_job_success;
   1: s:=txt_job1;
   2: s:=txt_job2;
   7: s:=txt_job7+' '+cl;
   8: s:=txt_job8;
   127: s:=txt_job127;
   255: s:=txt_job255;
   else s:=inttostr(i)+txt_job_unknown;
   end;
if stopped=true then s:=txt_jstopped;
end;

procedure progress10; //progress counter
var
   outsize,percentout,i,iprog,incstep,refsize,gperc:qword;
   tdiff,speed,umode:integer;
   tsout:TTimeStamp;
   tpath:ansistring;
begin
with Form_gwrap do
begin
percentout:=0;
outsize:=0;
speed:=0;

tsout:=datetimetotimestamp(now);
tdiff:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if tdiff<=0 then tdiff:=100000;

if (pfun<>'UN7Z') and (pfun<>'7Z') then umode:=0 else umode:=1;
if runelevated=true then umode:=0;

//On external drives NTFS seems not updating output file size, even using SHChangeNotify

if umode=0 then
   begin
   tpath:=outpath;
   if (optype=1) and (modeofuse=0) then
     if fileexists(tpath+'.tmp') then tpath:=outpath+'.tmp';
   try
   if fileexists(tpath) then
      if not(checkdirexists(tpath)) then
         begin
         srcfilesize_multipart(tpath,outsize);
         if insize<outsize then
            if insize<>0 then percentout:=1000000
         else
            if insize<>0 then percentout:=(outsize*1000000) div insize;
         if outsize>0 then
            begin
            Form_gwrap.Labelo.Visible:=true;
            Form_gwrap.LabelInfo2.Visible:=true;
            if (percentout>0) then Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize),filesizebase)+' ('+inttostr(percentout div 10000)+'%)'
            else Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize),filesizebase);
            if tdiff<>0 then speed:=outsize div tdiff * 1000;
            if speed>0 then
               Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' @ '+nicenumber(inttostr(speed),filesizebase)+'/s';
            end
         else
            begin
            Form_gwrap.Labelo.Visible:=false;
            Form_gwrap.LabelInfo2.Visible:=false;
            end;
         end;
   except
   end;
   if optype=1 then percentout:=percentout * 2;
   if percentout>1000000 then percentout:=1000000;
   if insize<>0 then refsize:=(insize+outsize) div 2
   else refsize:=outsize;
   if refsize>10000000 then
      begin
      incstep:=400000000000 div refsize; //time-bound incremental step
      if incstep<2 then incstep:=2;
      end
   else incstep:=40000;
   if optype=0 then incstep:=incstep * 4;
   if optype=0 then
      if insize<>0 then
         if outsize>insize then incstep:=incstep * 2;
   if optype=1 then
      if insize<>0 then
         if outsize>(insize div 2) then incstep:=incstep * 2;
   if progress<250000 then progress:=progress+(incstep div 2)+1
      else
      if progress<500000 then progress:=progress+incstep
      else
         if progress<700000 then progress:=progress+(incstep div 4)+1
         else
            if progress<800000 then progress:=progress+(incstep div 16)+1
            else
               if progress<900000 then progress:=progress+(incstep div 64)+1
               else
                  if progress<950000 then progress:=progress+(incstep div 256)+1
                  else
                     if progress<1000000 then progress:=progress+1;
   if percentout>0 then i:=(progress+percentout) div 2
   else i:=progress;
   iprog:=(Form_gwrap.Width*i div 1000000);
   if iprog<3 then iprog:=3;
   ShapeProgress.Width:=iprog;
   if Form_gwrap.Width<>0 then
      if ShapeProgress.Width<>0 then
         iperc:=(ShapeProgress.Width *100) div Form_gwrap.Width;
   end
else
   begin
   if Form_gwrap.Width<>0 then
      if (iperc>0) and (iperc<100) then
         ShapeProgress.Width:=(Form_gwrap.Width*iperc) div 100;
   tpath:=outpath;
   if (optype=1) and (modeofuse=0) then
      if fileexists(tpath+'.tmp') then tpath:=outpath+'.tmp';
   try
      if fileexists(tpath) then
         if not(checkdirexists(tpath)) then
            begin
            srcfilesize_multipart(tpath,outsize);
            if outsize>0 then
               begin
               Form_gwrap.Labelo.Visible:=true;
               Form_gwrap.LabelInfo2.Visible:=true;
               Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize),filesizebase);

               if pfun='7Z' then
                  if (insize>0) and (iperc>0) then
                     begin
                     percentout:=(((outsize*100) div iperc)*1000000) div insize;
                     Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' ('+inttostr(percentout div 10000)+'%)';
                     end;

               if tdiff<>0 then speed:=outsize div tdiff * 1000;
               if speed>0 then
                  Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' @ '+nicenumber(inttostr(speed),filesizebase)+'/s';
               end
            else
               begin
               Form_gwrap.Labelo.Visible:=false;
               Form_gwrap.LabelInfo2.Visible:=false;
               end;
            end;
   except
   end;
   end;

if ShapeGlobalProgress.visible=true then ShapeGlobalProgress.Width:=pprogbarprev+(((pprogbar-pprogbarprev)*iperc) div 100);
if iperc>0 then
   begin
   l5.Caption:=inttostr(iperc)+'% ';
   Form_gwrap.Caption:=pprogn+' '+l5.Caption+pcapt;
   Form_gwrap.TrayIcon1.Hint:=Form_gwrap.Caption;
   Form_gwrap.pm2restore.Caption:=Form_gwrap.Caption;
   end;

if (pfun<>'UN7Z') and (pfun<>'7Z') then
   LabelInfo3.Caption:=nicetime(inttostr(tdiff))
else
   begin
   if (iperc>0) and (iperc<100) then
      if iperc>ipercp then
         remtime:=(tdiff*(100-iperc)) div iperc;
   if remtime>0 then LabelInfo3.Caption:=nicetime(inttostr(tdiff))+', '+txt_6_9_remaining+' '+nicetime(inttostr(remtime))
   else LabelInfo3.Caption:=nicetime(inttostr(tdiff));
   end;
if (iperc>0) and (iperc<100) then ipercp:=iperc;
if ShapeGlobalProgress.visible=true then gperc:=(ShapeGlobalProgress.Width * 100) div Form_gwrap.Width
else gperc:=iperc;
{$IFDEF MSWINDOWS}
if okseven=true then
   try
   FTaskBarList.SetProgressState(AppHandle, TBPF_Normal);
   FTaskBarList.SetProgressValue(AppHandle, gperc, 100);
   except
   end;
{$ENDIF}
end;
end;

procedure setiomenu;
begin
if Form_gwrap.l2.caption='' then Form_gwrap.pmei.visible:=false else Form_gwrap.pmei.visible:=true;
if poutnamet='' then Form_gwrap.pmet.visible:=false else Form_gwrap.pmet.visible:=true;
if Form_gwrap.l4.caption='' then Form_gwrap.pmeo.visible:=false else Form_gwrap.pmeo.visible:=true;
Form_gwrap.pm2ei.visible:=Form_gwrap.pmei.visible;
Form_gwrap.pm2et.visible:=Form_gwrap.pmet.visible;
Form_gwrap.pm2eo.visible:=Form_gwrap.pmeo.visible;
Form_gwrap.pmei.caption:=txt_explore+' '+Form_gwrap.l2.caption;
Form_gwrap.pmet.caption:=txt_explore+' TMP';
Form_gwrap.pmeo.caption:=txt_explore+' '+Form_gwrap.l4.caption;
Form_gwrap.pm2ei.caption:=Form_gwrap.pmei.caption;
Form_gwrap.pm2et.caption:=Form_gwrap.pmet.caption;
Form_gwrap.pm2eo.caption:=Form_gwrap.pmeo.caption;
end;

procedure set_form_title;
var s,s1,s2:ansistring;
begin
Form_gwrap.l1.Visible:=false;
Form_gwrap.l2.Visible:=false;
Form_gwrap.l3.Visible:=false;
Form_gwrap.l4.Visible:=false;
Form_gwrap.l1.Caption:='';
Form_gwrap.l2.Caption:='';
Form_gwrap.l3.Caption:='';
Form_gwrap.l4.Caption:='';
Form_gwrap.l6.Caption:='';
Form_gwrap.l2.Hint:='';
Form_gwrap.l4.Hint:='';
case modeofuse of
   1 : s1:=txt_5_3_test+' '+extractfilename(in_name);
   4 : s1:=txt_5_3_info+' '+extractfilename(in_name);
   5 : s1:=txt_5_3_list+' '+extractfilename(in_name);
   end;
case modeofuse of
   3 :
   begin
   pcapt:=cl;
   exit;
   end;
   2 :
   begin
   pcapt:=txt_bench;
   exit;
   end;
   1,4,5 :
   begin
   if outpath<>'' then
      if outpath[length(outpath)]=directoryseparator then s2:=copy(outpath,1,length(outpath)-1)
      else s2:=outpath;
   {$IFDEF MSWINDOWS}if length(s2) = 2 then else{$ENDIF} s2:=extractfilename(s2);
   Form_gwrap.l1.Caption:=(s1+' '+txt_5_0_in+' ');
   Form_gwrap.l4.Caption:=(s2);
   Form_gwrap.l4.Hint:=(extractfilepath(outpath));
   pcapt:=Form_gwrap.l1.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   setiomenu;
   exit;
   end;
   end;
if extractfilename(outpath)='' then optype:=0
else optype:=1;//0 extract, 1 archive
if psubfun='extract' then optype:=0;
if psubfun='archive' then optype:=1;
case pfun of
   'UN7Z','UNARC','UNPAQ','UNLPAQ','UNZPAQ','UNQUAD','UNACE','UNBROTLI','UNZSTD': optype:=0;
end;
if (psubfun='repair') or (psubfun='rrec') then optype:=1;
case optype of
0:
   begin
   if outpath<>'' then
      if outpath[length(outpath)]=directoryseparator then s2:=copy(outpath,1,length(outpath)-1)
      else s2:=outpath;
   {$IFDEF MSWINDOWS}if length(s2) = 2 then else{$ENDIF} s2:=extractfilename(s2);
   s:=extractfilepath(in_name);
   s:=copy(s,1,length(s)-1);
   {$IFDEF MSWINDOWS}if length(s) = 2 then else{$ENDIF} s:=extractfilename(s);
   s1:=txt_5_0_extract+' '+extractfilename(in_name)+' '+txt_5_0_from+' ';
   Form_gwrap.l1.Caption:=(s1);
   Form_gwrap.l2.Caption:=(s);
   Form_gwrap.l2.Hint:=(extractfilepath(in_name));
   Form_gwrap.l3.Caption:=' '+txt_5_0_to+' ';
   Form_gwrap.l4.Caption:=(s2);
   Form_gwrap.l4.Hint:=(extractfilepath(outpath));
   pcapt:=Form_gwrap.l1.Caption+Form_gwrap.l2.Caption+' '+Form_gwrap.l3.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l2.Visible:=true;
   Form_gwrap.l3.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   end
else
   begin
   case psubfun of
      'repair': s1:=txt_7_4_recover+' '+extractfilename(outpath);
      'rrec': s1:=txt_rr+' '+extractfilename(outpath);
      else s1:=txt_create+' '+extractfilename(outpath); //don't work properly for arc since needs no directoryseparator after outpath
   end;
   s2:=extractfilepath(outpath);
   s2:=copy(s2,1,length(s2)-1);
   {$IFDEF MSWINDOWS}if length(s2) = 2 then else{$ENDIF} s2:=extractfilename(s2);
   Form_gwrap.l1.Caption:=(s1+' '+txt_5_0_in+' ');
   Form_gwrap.l4.Caption:=(s2);
   Form_gwrap.l4.Hint:=(extractfilepath(outpath));
   pcapt:=Form_gwrap.l1.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   end;
end;
Form_gwrap.l6.Caption:=paction;
if pfromnativedrag=true then
   begin
   Form_gwrap.l3.Visible:=false;
   Form_gwrap.l4.Visible:=false;
   Form_gwrap.l6.Caption:=txt_7_8_dd;
   Form_gwrap.l6.Visible:=true;
   Form_gwrap.pmeo.Visible:=false;
   Form_gwrap.pm2eo.Visible:=false;
   end;
if Form_gwrap.l6.Caption<>'' then Form_gwrap.l6.visible:=true else Form_gwrap.l6.visible:=false;
setiomenu;
end;

procedure explore_out(pt:ansistring);
var
   s,outpath2,inoutpath:ansistring;
   {$IFNDEF MSWINDOWS}
   i:integer;
   {$ENDIF}
begin
inoutpath:=outpath;
if pt<>'' then outpath:=pt;
{$IFDEF MSWINDOWS}
outpath2:=outpath;
if (modeofuse=1) or (modeofuse=4) or (modeofuse=5) then outpath2:=in_name;
if (optype=1) and (modeofuse=0) then
   if fileexists(outpath+'.tmp') then outpath2:=outpath+'.tmp';
s:=outpath2;
if s='' then exit;
winexplorepath(s);
{$ELSE}
{$IFDEF DARWIN}
outpath2:=outpath;
if (modeofuse=1) or (modeofuse=4) or (modeofuse=5) then outpath2:=in_name;
if (optype=1) and (modeofuse=0) then
   if fileexists(outpath+'.tmp') then outpath2:=outpath+'.tmp';
s:=outpath2;
if s='' then exit;
macexplorepath(s);
{$ENDIF}
i:=filegetattr(outpath);
if (i and faDirectory) = 0 then s:=extractfilepath(outpath)
else s:=outpath;
if s='' then exit;
cp_open(s,desk_env);
{$ENDIF}
outpath:=inoutpath;
end;

procedure explore_in;
var
   s:ansistring;
   {$IFNDEF MSWINDOWS}
   i:integer;
   {$ENDIF}
begin
{$IFDEF MSWINDOWS}
s:=in_name;
if s='' then exit;
s:=(s);
winexplorepath(s);
{$ELSE}
{$IFDEF DARWIN}
s:=in_name;
if s='' then exit;
s:=(s);
macexplorepath(s);
{$ENDIF}
i:=filegetattr(in_name);
if (i and faDirectory) = 0 then s:=extractfilepath(in_name)
else s:=in_name;
if s='' then exit;
cp_open(s,desk_env);
{$ENDIF}
end;

procedure setlabel1text;
begin
with Form_gwrap do
   begin
   if ppause=true then Label1.Caption:=txt_paused+' '
   else Label1.Caption:=txt_running+' ';
   case ppriority of
      0: Label1.Caption:=Label1.Caption+txt_rt+', ';
      1: Label1.Caption:=Label1.Caption+txt_high+', ';
      2: Label1.Caption:=Label1.Caption+txt_normal+', ';
      3: Label1.Caption:=Label1.Caption+txt_idle+', ';
      end;
   case modeofuse of
      3 : Label5.Caption:=txt_defragjob;//unused
      20 : Label5.Caption:=txt_consolejob;
      end;
   if modeofuse<>2 then
      if label5.caption<>'' then Label5.Visible:=true else Label5.Visible:=false;
   end;
end;

procedure updatereport(M:TMemoryStream; var stri1:ansistring);
var
   i:integer;
   stri2:ansistring;
begin
with Form_gwrap do
begin
SetString(stri2, M.Memory, M.Size);
stri2:=AnsiReverseString(stri2);
stri2:=copy(stri2,pos(char($0A)+char($0D),stri2)+2,length(stri2)-(pos(char($0A)+char($0D),stri2)));
stri1:=copy(stri2,1,pos(char($0A)+char($0D),stri2)-1);
stri1:=ReverseString(stri1);
stri2:=ReverseString(stri2);
Form_gwrap.Memo1.Clear;
Form_gwrap.Memo1.Append(stri2);
Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.Memo1.Lines.Count;
for i:=0 to Form_gwrap.Memo1.Lines.Count-1 do Form_gwrap.StringGrid1.Cells[0,i]:=Form_gwrap.Memo1.Lines[i];
Form_gwrap.Memo1.Clear;
Form_gwrap.StringGrid1.RowCount:=Form_gwrap.StringGrid1.Rowcount-1; //last row may be incomplete
Form_gwrap.StringGrid1.Row:=Form_gwrap.StringGrid1.Rowcount-1;
//Form_gwrap.StringGrid1.AutosizeColumns;
end;
end;

procedure updatereportl(var stri1:ansistring);
var
   i,l,previperc:integer;
begin
with Form_gwrap do
begin
previperc:=iperc;
l:=length(stri1);
for i:=l downto 1 do
    begin
    if i<(l-16000) then break;
    if stri1[i]='%' then
       begin
       try
       iperc:=strtoint(copy(stri1,i-3,3));
       if (iperc<100) then break;
       except
       end;
       end;
    end;
if iperc<0 then iperc:=previperc;
if iperc>100 then iperc:=previperc;
end;
end;

procedure displayinfo(exit_code:integer; exit_string:ansistring);
var
   s,s1,dummystr:ansistring;
   i,rc:integer;
   osize,psize,cratio:qword;
   stopinfo:boolean;
begin
Form_gwrap.Visible:=false;
if exit_code=0 then
begin
try
//details (not localized)
stopinfo:=false;
s:='';
if pjobuserar=1 then i:=2 else i:=8;
rc:=Form_gwrap.StringGrid1.Rowcount;
while stopinfo=false do
begin
i:=i+1;
if i>=rc-1 then stopinfo:=true;
dummystr:=Form_gwrap.StringGrid1.Cells[0,i];
if pjobuserar=0 then
   if dummystr='------------------- ----- ------------ ------------  ------------------------' then stopinfo:=true
   else s:=s+dummystr+char($0D)//char($0A)+char($0D)
else s:=s+dummystr+char($0D);
end;
//comment
if pjobuserar=1 then  //unused, localized Rar.exe does not allow to easily detect "Comment not present" case
   begin
   rarcomment:=copy(s,1,Length(s)-2);
   end
else
   begin
   if length(s)>58 then s:=copy(s,1,Length(s)-1-58);
   i:=pos('Comment',s)+10;
   rarcomment:='';
   if i>10 then rarcomment:=copy(s,i,Length(s)-i);
   if length(rarcomment)>4 then
      if (rarcomment[1]=char($0D)) and (rarcomment[2]='{') then rarcomment:=copy(rarcomment,4,Length(rarcomment)-5);
   end;
if pjobcommentrar=1 then
   begin
   gocancelall:=false;
   Form_gwrap.Close;
   exit;
   end;
//original size
stopinfo:=false;
dummystr:=Form_gwrap.StringGrid1.Cells[0,rc-2];
{$IFDEF MSWINDOWS}dummystr:=copy(dummystr,22,length(dummystr)-21);{$ENDIF}
while stopinfo=false do
begin
if length(dummystr)<2 then break;
if dummystr[1]=' ' then dummystr:=copy(dummystr,2,length(dummystr)-1)
else stopinfo:=true;
end;
s1:=txt_5_3_os+' '+nicenumber(copy(dummystr,1,pos(' ',dummystr)-1),filesizebase)+char($0A)+char($0D);
osize:=strtoqword(copy(dummystr,1,pos(' ',dummystr)-1));
dummystr:=copy(dummystr,pos(' ',dummystr),length(dummystr)-pos(' ',dummystr)+1);
//packed size
stopinfo:=false;
while stopinfo=false do
begin
if length(dummystr)<2 then break;
if dummystr[1]=' ' then dummystr:=copy(dummystr,2,length(dummystr)-1)
else stopinfo:=true;
end;
s1:=s1+txt_5_3_ps+' '+nicenumber(copy(dummystr,1,pos(' ',dummystr)-1),filesizebase);
psize:=strtoqword(copy(dummystr,1,pos(' ',dummystr)-1));
if osize<>0 then
   begin
   cratio:=(100*psize) div osize;
   s1:=s1+' ('+inttostr(cratio)+'%)'+char($0A)+char($0D);
   end
else s1:=s1+char($0A)+char($0D);
dummystr:=copy(dummystr,pos(' ',dummystr),length(dummystr)-pos(' ',dummystr)+1);
//files number
stopinfo:=false;
while stopinfo=false do
begin
if length(dummystr)<2 then break;
if dummystr[1]=' ' then dummystr:=copy(dummystr,2,length(dummystr)-1)
else stopinfo:=true;
end;
s1:=s1+copy(dummystr,1,pos(' ',dummystr)-1)+' '+txt_5_3_files;
dummystr:=copy(dummystr,pos(' ',dummystr),length(dummystr)-pos(' ',dummystr)+1);
//dirs number
i:=pos('folder',dummystr)-1;
if i>1 then
   s1:=s1+', '+copy(dummystr,pos(', ',dummystr)+2,i-pos(', ',dummystr)-2)+' '+txt_5_3_folders+char($0A)+char($0D)
else
   s1:=s1+char($0A)+char($0D)
except
   if rc>4 then
   s:=Form_gwrap.StringGrid1.Cells[0,rc-4]+char($0A)+char($0D)+
   Form_gwrap.StringGrid1.Cells[0,rc-3]+char($0A)+char($0D)+
   Form_gwrap.StringGrid1.Cells[0,rc-2]+char($0A)+char($0D)+
   Form_gwrap.StringGrid1.Cells[0,rc-1]
   else s:='';
   s1:='';
end;
//presentation
s:=extractfilename(in_name)+char($0A)+char($0D)+char($0A)+char($0D)+s1+char($0A)+char($0D)+txt_5_3_details+char($0A)+char($0D)+'------'+char($0A)+char($0D)+s;
end
else
   begin
   if pjobcommentrar=1 then
      begin
      rarcomment:='';
      gocancelall:=false;
      Form_gwrap.Close;
      exit;
      end;
   s:=exit_string;
   end;
pMessageInfoOK(s);
gocancelall:=false;
Form_gwrap.Close;
end;

procedure setbuttonsnormal;
begin
Form_gwrap.Button1.Visible:=false;
Form_gwrap.ButtonPause.Visible:=true;
Form_gwrap.ButtonStop.Visible:=true;
if pprogn<>'' then
   Form_gwrap.ButtonStopAll.Visible:=true
else
   Form_gwrap.ButtonStopAll.Visible:=false;
if Form_gwrap.ButtonStopAll.visible=true then
   begin
   Form_gwrap.ButtonStopAll.AnchorSideRight.Control:=Form_gwrap.ButtonStop;
   Form_gwrap.ButtonStop1.AnchorSideRight.Control:=Form_gwrap.ButtonStopAll;
   end
else
   Form_gwrap.ButtonStop1.AnchorSideRight.Control:=Form_gwrap.ButtonStop;
Form_gwrap.pm2cancelall.Visible:=Form_gwrap.ButtonStopAll.Visible;
end;

procedure setbuttonsclose;
begin
Form_gwrap.Button1.Visible:=true;
Form_gwrap.ButtonPause.Visible:=false;
Form_gwrap.ButtonStop.Visible:=false;
if Form_gwrap.ButtonStopAll.visible=true then
   begin
   Form_gwrap.ButtonStopAll.AnchorSideRight.Control:=Form_gwrap.Button1;
   Form_gwrap.ButtonStop1.AnchorSideRight.Control:=Form_gwrap.ButtonStopAll;
   end
else
   Form_gwrap.ButtonStop1.AnchorSideRight.Control:=Form_gwrap.Button1;
end;

procedure savegwpos;
begin
if Form_gwrap.Visible=true then
   if Form_gwrap.WindowState=wsNormal then
      begin
      ws_gw_left:=Form_gwrap.left;
      ws_gw_top:=Form_gwrap.top;
      ws_gw_width:=Form_gwrap.width;
      ws_gw_height:=Form_gwrap.height;
      end;
end;

procedure showandhide;
begin
needinteraction:=false;
application.ProcessMessages;
savegwpos;
if (pproglast=true) or (Form_gwrap.ShapeGlobalProgress.Visible=false) then
   begin
   sleep(500);
   savegwpos;
   Form_gwrap.Visible:=false;
   exbackground:=false;
   Form_gwrap.TrayIcon1.visible:=false;
   end;
end;

procedure showandwait;
begin
gocancelall:=false;
setbuttonsclose;
if Form_gwrap.visible=false then Form_gwrap.visible:=true;
if Form_gwrap.WindowState=wsMinimized then Form_gwrap.WindowState:=wsNormal;
if Form_gwrap.Visible=true then Form_gwrap.SetFocus;
application.ProcessMessages;
exbackground:=false;
Form_gwrap.TrayIcon1.visible:=false;
needinteraction:=true;
repeat
   savegwpos;
   application.ProcessMessages;
   sleep(100);
until needinteraction=false;
end;

function getalignw(ntabs:integer):integer;
begin
result:=Form_gwrap.ShapeTitleb1.Width+
Form_gwrap.ShapeTitleb2.Width+
Form_gwrap.ShapeTitleb3.Width+
Form_gwrap.LabelTitle1.BorderSpacing.Left+Form_gwrap.LabelTitle1.BorderSpacing.Left+
Form_gwrap.LabelTitle2.BorderSpacing.Left+
Form_gwrap.LabelTitle3.BorderSpacing.Left;
if ntabs=4 then result:=result+Form_gwrap.ShapeTitleb4.Width+Form_gwrap.LabelTitle4.BorderSpacing.Left;
end;

procedure launch_cl;
var
   P: TProcessUTF8;
   tsout:TTimeStamp;
   tdiff,speed1,speed2,i,j,BytesRead,BytesRead2:integer;
   outsize,cratio:qword;
   s,stri,astri,bstri,wd,m2s:ansistring;
   fe:ansistring;
   {$IFDEF MSWINDOWS}
   w0,w1:WideString;
   sei: TShellExecuteInfoW;
   seiexit:dword;
   {$ENDIF}
   prevpause:boolean;
   M,M2:TmemoryStream;
begin
pstarted:=true;
Form_gwrap.Panel2.Visible:=true;
Form_gwrap.PanelBench.Visible:=false;
Form_gwrap.Shapeprogress.visible:=true;
Form_gwrap.ShapeProgress.Width:=3;
progress:=0;
Form_gwrap.Memo2.Clear;
Form_gwrap.Memo2.Lines.Append(cl);
Form_gwrap.LabelTitle4.Visible:=true;
ipercp:=0;
iperc:=0;
remtime:=0;
if cl='' then
   begin
   pMessageErrorOK(txt_nocl);
   Application.Terminate;
   end;
if pprogfirst=true then
   begin
   ppause:=false;
   Form_gwrap.ButtonPause.Caption:='   '+txt_pause+'   ';
   prevpause:=false;
   end;
if ppause=false then
   begin
   Form_gwrap.ShapeProgress.Color:=PGREEN;
   Form_gwrap.ShapeGlobalProgress.Color:=PGREEN;
   end;
Form_gwrap.LabelTitle1.caption:='      '+txt_isrunning+'      ';
Form_gwrap.PanelTitlePLTabAlign.Width:=getalignw(4);
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(Form_gwrap.LabelTitle1,Form_gwrap.ShapeTitleb1) else clicklabel_launcher(Form_gwrap.LabelTitle1,Form_gwrap.Shapelink1);
if insize>0 then
   if (pinsize>0) and (pinsize<>insize) then Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(pinsize),filesizebase)+' / '+nicenumber(inttostr(insize),filesizebase)
   else Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(insize),filesizebase)
else Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(pinsize),filesizebase);
set_form_title;
if Form_gwrap.Visible=true then Application.ProcessMessages;
if runelevated=false then
   begin
   P:=TProcessUTF8.Create(nil);
   P.CommandLine:=(cl);
   M := TMemoryStream.Create;
   BytesRead := 0;
   M.setsize(32*1024);
   M2 := TMemoryStream.Create;
   BytesRead2 := 0;
   M2.setsize(128);
   if modeofuse=20 then
   else
      {$IFDEF MSWINDOWS}
      P.Options := [poUsePipes, poNoConsole];
      {$ELSE}
      P.Options := [poUsePipes];
      {$ENDIF}
   end;
try
if validatecl(cl)<>0 then begin pMessageWarningOK(txt_2_7_validatecl+' '+cl); ended:=true; exit; end;
tsin:=datetimetotimestamp(now);
pcount:=1;
stri:='';
astri:='';
bstri:='';
if modeofuse<>2 then Form_gwrap.l5.Caption:='0% ' else Form_gwrap.l5.Caption:='';
Form_gwrap.Caption:=pprogn+' '+pcapt;
Form_gwrap.TrayIcon1.Hint:=Form_gwrap.Caption;
Form_gwrap.pm2restore.Caption:=Form_gwrap.Caption;
Form_gwrap.StringGrid1.Rowcount:=1;
if runelevated=true then
   begin
   {$IFDEF MSWINDOWS}
   fe:='0';
   seiexit:=127;
   wd:=copy(cl,1,pos('.exe',cl)+4);
   w0:=utf8decode(stringdelim(wd));
   wd:=copy(cl,pos('.exe',cl)+6,length(cl)-pos('.exe',cl)-4);
   cl:=wd;
   w1:=utf8decode(cl);
   FillChar(sei, SizeOf(sei), 0);
   sei.cbSize := SizeOf(sei);
   sei.Wnd := 0;
   sei.fMask := SEE_MASK_NOCLOSEPROCESS;
   sei.lpVerb := PWideChar ('runas');
   sei.lpFile := PWideChar (w0);
   sei.lpParameters := PWideChar (w1);
   sei.nShow := SW_HIDE;
   ShellExecuteExW(@sei);
   Form_gwrap.Timer2.enabled:=true;
   while WaitForSingleObject(sei.hProcess, 50) <> WAIT_OBJECT_0 do
      begin
      fe:=inttostr(GetLastError);
      if fe='6' then break;
      Application.ProcessMessages;
      if stopped=true then TerminateProcess(sei.hProcess,255);
      end;
   GetExitCodeProcess(sei.hProcess, seiexit);
   exit_code:=seiexit;
   Form_gwrap.StringGrid1.Rowcount:=1;
   {$ENDIF}
   end
else
   begin
   Form_gwrap.Timer2.enabled:=true;
   case ppriority of
      0: begin
         P.Priority:=ppRealTime;
         end;
      1: begin
         P.Priority:=ppHigh;
         end;
      2: begin
         P.Priority:=ppNormal;
         end;
      3: begin
         P.Priority:=ppIdle;
         end;
      end;
   P.Execute;
   while P.Running do
   begin
   Application.ProcessMessages;
   if stopped=true then
      begin
      P.Terminate(255);
      {$IFDEF MSWINDOWS}
      if okseven=true then
         try
         Form_gwrap.FTaskBarList.SetProgressState(Form_gwrap.AppHandle, TBPF_ERROR);
         except
         end;
      {$ENDIF}
      Form_gwrap.ShapeProgress.Color:=PRED;
      Form_gwrap.ShapeGlobalProgress.Color:=PRED;
      break;
      end;
   if ppause=true then
      begin
      if ppause<>prevpause then
         begin
         P.Suspend;
         prevpause:=true;
         end;
      end
   else
      begin
      if ppause<>prevpause then
         begin
         P.Resume;
         prevpause:=false;
         end;
      end;
   setlabel1text;
   if modeofuse>=20 then Sleep(100)
   else
      begin
      if P.output.NumBytesAvailable>0 then
         begin
         if BytesRead+max_l>=M.Size then M.SetSize(BytesRead + 8*1024*1024);
         i := P.Output.Read((M.Memory + BytesRead)^, max_l);
         end
      else i:=0;
      if i > 0 then Inc(BytesRead, i);
      if P.Stderr.NumBytesAvailable>0 then
         begin
         if (pfun='UN7Z') or (pfun='7Z') then
            M2.SetSize(P.Stderr.NumBytesAvailable)
         else
            M2.SetSize(BytesRead2 + max_l);
         if (pfun='UN7Z') or (pfun='7Z') then
            j := P.Stderr.Read((M2.Memory)^, P.Stderr.NumBytesAvailable)
         else
            j := P.Stderr.Read((M2.Memory + BytesRead2)^, max_l);
         if j>0 then
            if (pfun='UN7Z') or (pfun='7Z') then
               begin
               astri:=bstri;
               SetString(stri, M2.Memory, M2.Size);
               bstri:=stri;
               if astri<>bstri then
                  begin
                  updatereportl(stri);
                  end;
               end;
         end
      else j:=0;
      if j > 0 then Inc(BytesRead2, j);
      if (i=0) and (j=0) then sleep(100);
      end;
   end;
   M2.Free;
   end;
Form_gwrap.Timer2.enabled:=false;
Form_gwrap.l5.caption:='100% ';
iperc:=100;
Form_gwrap.Caption:=pprogn+' '+pcapt;
Form_gwrap.TrayIcon1.Hint:=Form_gwrap.Caption;
Form_gwrap.pm2restore.Caption:=Form_gwrap.Caption;
pstarted:=false;
Form_gwrap.Imagestatus.Cursor:=crDefault;
Form_gwrap.Imagestatus.Hint:='';
tsout:=datetimetotimestamp(now);
Form_gwrap.ShapeProgress.Width:=Form_gwrap.Width-6;//process terminated
if pproglast=true then Form_gwrap.ShapeGlobalProgress.Width:=Form_gwrap.Width-6;
Application.ProcessMessages;
if runelevated=false then
   begin
      if modeofuse>=20 then
         begin
            exit_code:=P.ExitStatus;
            P.Free;
         end
      else
         begin
            repeat
               if BytesRead+max_l>=M.Size then M.SetSize(BytesRead + 8*1024*1024);
               i := P.Output.Read((M.Memory + BytesRead)^, max_l);
               if i>0 then
                  begin
                  astri:=bstri;
                  SetString(stri, M.Memory, M.Size);
                  bstri:=stri;
                  if astri<>bstri then
                     begin
                     updatereport(M,stri);
                     end;
                  end;
               if i > 0 then Inc(BytesRead, i)
               else Sleep(100);
            until i <= 0;
            M.SetSize(BytesRead);
            exit_code:=P.ExitStatus;
            P.Free;
            Form_gwrap.Memo1.Clear;
            Form_gwrap.Memo1.Lines.LoadFromStream(M);
            SetString(stri, M.Memory, M.Size);
            stri:=AnsiReverseString(stri);
            stri:=copy(stri,pos(char($0A)+char($0D),stri)+2,length(stri)-(pos(char($0A)+char($0D),stri)));
            stri:=copy(stri,1,pos(char($0A)+char($0D),stri)-1);
            stri:=ReverseString(stri);
            M.Free;
            Form_gwrap.StringGrid1.BeginUpdate;
            if Form_gwrap.Memo1.Lines.Count>0 then
               begin
               Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.Memo1.Lines.Count+1;
               for i:=0 to Form_gwrap.Memo1.Lines.Count-1 do Form_gwrap.StringGrid1.Cells[0,i]:=Form_gwrap.Memo1.Lines[i];//(Form_gwrap.Memo1.Lines[i]);
               Form_gwrap.Memo1.Clear;
               i:=i+1;
               Form_gwrap.StringGrid1.Cells[0,i]:='';
               if Form_gwrap.StringGrid1.RowCount<128*1024 then Form_gwrap.StringGrid1.AutosizeColumns;
               Form_gwrap.StringGrid1.Row:=Form_gwrap.StringGrid1.Rowcount-1;
               end
            else
               begin
               Form_gwrap.StringGrid1.Rowcount:=1;
               Form_gwrap.StringGrid1.Cells[0,0]:='';
               end;
            Form_gwrap.StringGrid1.EndUpdate;
         end;
   end;
Form_gwrap.Stringgrid1.Cursor:=crDefault;
Form_gwrap.ShapeProgress.Width:=Form_gwrap.Width-3; //read from memory stream, if used
if pproglast=true then Form_gwrap.ShapeGlobalProgress.Width:=Form_gwrap.Width-3;
Application.ProcessMessages;
except
exit_code:=127; //"cannot execute" error
end;
tdiff:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if tdiff<=0 then tdiff:=100000;
if pproglast=true then if exit_code<>0 then Form_gwrap.LabelTitle1.caption:='      '+txt_status+'      ';
Form_gwrap.PanelTitlePLTabAlign.Width:=getalignw(4);
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(Form_gwrap.LabelTitle1,Form_gwrap.ShapeTitleb1) else clicklabel_launcher(Form_gwrap.LabelTitle1,Form_gwrap.Shapelink1);
{$IFNDEF MSWINDOWS}
if stopped=true then exit_code:=255;
{$ENDIF}

decode_exitcode(exit_code,s);
if exit_code<>0 then perrors:=perrors+1;

if modeofuse=4 then begin displayinfo(exit_code,s); exit; end;

Form_gwrap.Label1.Caption:=s+', ';
Form_gwrap.Label5.Caption:='';
Form_gwrap.Label6.Caption:='';
Form_gwrap.Label5.Visible:=false;
Form_gwrap.Label6.Visible:=false;
outsize:=0;
try
if fileexists((outpath)) then
   begin
   assignfile(f,(outpath));
   filemode:=0;
   reset(f);
   srcfilesize_multipart(outpath,outsize);
   closefile(f);
   end;
except
end;
if (outsize>0) then
   if (insize>0) and (Form_gwrap.labelo.visible=true) then
      begin
      cratio:=outsize * 100;
      cratio:=cratio div insize;
      Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize),filesizebase)+' ('+inttostr(cratio)+'%)';
      end
   else
      Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize),filesizebase);
Form_gwrap.Imagestatus.Picture.Bitmap:=Binfo;
//speed
speed1:=0;
speed2:=0;
if tdiff<>0 then speed1:=pinsize div tdiff * 1000;
if tdiff<>0 then speed2:=outsize div tdiff * 1000;
Form_gwrap.LabelInfo3.Caption:=nicetime(inttostr(tdiff));
if speed1>0 then Form_gwrap.LabelInfo1.Caption:=Form_gwrap.LabelInfo1.Caption+' @ '+nicenumber(inttostr(speed1),filesizebase)+'/s';
if speed2>0 then Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' @ '+nicenumber(inttostr(speed2),filesizebase)+'/s';
if modeofuse>=20 then
   begin
   Form_gwrap.StringGrid1.Rowcount:=5;
   Form_gwrap.StringGrid1.Cells[0,0]:=datetimetostr(timestamptodatetime(tsout))+' - '+s;
   Form_gwrap.StringGrid1.Cells[0,1]:='';
   Form_gwrap.StringGrid1.Cells[0,2]:=Form_gwrap.Labeli.Caption+' '+Form_gwrap.LabelInfo1.Caption;
   Form_gwrap.StringGrid1.Cells[0,3]:=Form_gwrap.Labelo.Caption+' '+Form_gwrap.LabelInfo2.Caption;
   Form_gwrap.StringGrid1.Cells[0,4]:=Form_gwrap.LabelInfo3.Caption;
   //Form_gwrap.StringGrid1.AutosizeColumns;
   end;
if exit_code=0 then
   begin
   Form_gwrap.Imagestatus.Picture.Bitmap:=Bsuccess;
   end
else
   begin
   Form_gwrap.Imagestatus.Picture.Bitmap:=Berror;
   {$IFDEF MSWINDOWS}
   if okseven=true then
      try
      Form_gwrap.FTaskBarList.SetProgressState(Form_gwrap.AppHandle, TBPF_ERROR);
      except
      end;
   {$ENDIF}
   Form_gwrap.ShapeProgress.Color:=PRED;
   Form_gwrap.ShapeGlobalProgress.Color:=PRED;
   end;

case exit_code of
   0: Form_gwrap.Caption:=pprogn+' '+txt_done+' '+pcapt;
   255: Form_gwrap.Caption:=pprogn+' '+txt_halted+' '+pcapt;
   else
      begin
      Form_gwrap.Caption:=pprogn+' '+txt_error+' '+pcapt;
      if (modeofuse<>2) and (modeofuse<>3) then
         begin
         Form_gwrap.LabelWarning1.Visible:=true;
         Form_gwrap.labelspac.visible:=true;
         end;
      end;
   end;
Form_gwrap.TrayIcon1.Hint:=Form_gwrap.Caption;
Form_gwrap.pm2restore.Caption:=Form_gwrap.Caption;
if Form_gwrap.LabelInfo2.width>Form_gwrap.LabelInfo3.width then i:=Form_gwrap.LabelInfo2.width
else i:=Form_gwrap.LabelInfo3.width;
case modeofuse of
   0: begin
      end;
   1,4,5: begin
      if (pfun<>'UNBROTLI') and (pfun<>'UNZSTD') then
            if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(Form_gwrap.LabelTitle2,Form_gwrap.ShapeTitleb2) else clicklabel_launcher(Form_gwrap.LabelTitle2,Form_gwrap.Shapelink2);
      end;
   2: begin
      Form_gwrap.Panel2.Visible:=false;
      Form_gwrap.PanelBench.Visible:=true;
      try
      Form_gwrap.Label2.Caption:=Form_gwrap.Stringgrid1.Cells[0,6];
      Form_gwrap.Label3.Caption:=copy(Form_gwrap.Stringgrid1.Cells[0,9],26,26);
      Form_gwrap.Label8.Caption:='Rating: '+copy(Form_gwrap.Stringgrid1.Cells[0,Form_gwrap.Stringgrid1.RowCount-2],28,7);
      Form_gwrap.Label7.Caption:='';
      except
      end;
      end;
   3: begin
      if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(Form_gwrap.LabelTitle2,Form_gwrap.ShapeTitleb2) else clicklabel_launcher(Form_gwrap.LabelTitle2,Form_gwrap.Shapelink2);
      end;
   end;
Form_gwrap.ShapeProgress.Width:=Form_gwrap.Width;
if Form_gwrap.ShapeGlobalProgress.visible=true then Form_gwrap.ShapeGlobalProgress.Width:=pprogbar;
if pproglast=true then Form_gwrap.ShapeGlobalProgress.Width:=Form_gwrap.Width;
ended:=true;
Application.ProcessMessages;
//Form_gwrap.StringGrid1.AutosizeColumns;

if exit_code<>0 then
   if pcanignore=true then
      begin
      Form_gwrap.ImageKeep.Visible:=true;
      Form_gwrap.ImageKeep.Caption:=txt_8_2_keep;
      end;

//final actions, options
if (pproglast=true) or (pprogn='') then
begin
if Form_gwrap.CheckBoxHalt.State=cbChecked then
   begin
   P:=TProcessUTF8.Create(nil);
   {$IFDEF MSWINDOWS}
   P.Options := [poNoConsole];
   P.Executable:='shutdown';
   P.Parameters.Add('/s');
   P.Parameters.Add('/t 10');
   {$ELSE}
   P.Executable:='halt';
   {$ENDIF}
   if Form_gwrap.Visible=true then Application.ProcessMessages;
   P.Execute;
   P.Free;
   end;
if autoopen=1 then
   if (modeofuse<>1) and (modeofuse<>4) and (modeofuse<>5) and (modeofuse<>2) then
      if (psubfun<>'extract') and (psubfun<>'convert') then explore_out(''); //deferred (in peach unit) to let run move after extraction operations if needed
Form_gwrap.LabelTitle4.Visible:=false;
Form_gwrap.PanelTitlePLTabAlign.Width:=getalignw(3);
end;

case pautoclose of
   0: showandwait;
   1: begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>4) and (modeofuse<>5) and (modeofuse<>2) then
            showandhide
         else
            showandwait
      else
         showandwait;
      end;
   2: begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>4) and (modeofuse<>5) and (modeofuse<>2) then
            showandhide
         else
            showandwait
      else
         showandwait;
      end;
   3: begin
      if exit_code=0 then
         if (modeofuse<>4) and (modeofuse<>5) and (modeofuse<>2) then
            showandhide
         else
            showandwait
      else
         showandwait;
      end;
   4: showandhide;
   else
      begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>4) and (modeofuse<>5) and (modeofuse<>2) then
            showandhide
         else
            showandwait
      else
         showandwait;
      end;
   end;
end;

{ TForm_gwrap }

procedure TForm_gwrap.Timer2Timer(Sender: TObject);
begin
if stopped=true then exit;
if ppause=true then exit;
if ended=true then exit;
if pstarted=true then
   begin
      if modeofuse<20 then progress10;
      case pcount of
         1: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp2; pcount:=2; end;
         2: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp3; pcount:=3; end;
         3: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp4; pcount:=4; end;
         4: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp5; pcount:=5; end;
         5: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp6; pcount:=6; end;
         6: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp7; pcount:=7; end;
         7: begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp8; pcount:=8; end;
         else begin Form_gwrap.Imagestatus.Picture.Bitmap:=Bp1; pcount:=1; end;
         end;
   end;
end;

procedure TForm_gwrap.TrayIcon1Click(Sender: TObject);
begin
exbackground:=false;
TrayIcon1.visible:=false;
Form_gwrap.visible:=true;
end;

procedure TForm_gwrap.TrayIcon1DblClick(Sender: TObject);
begin
exbackground:=false;
TrayIcon1.visible:=false;
Form_gwrap.visible:=true;
end;

procedure go_ok;
begin
gocancelall:=false;
pgook:=true;
Form_gwrap.Close;
end;

procedure TForm_gwrap.Button1Click(Sender: TObject);
begin
go_ok;
end;

procedure TForm_gwrap.ImageSavePJClick(Sender: TObject);
begin
save_cl;
end;

procedure TForm_gwrap.ButtonStop1Click(Sender: TObject);
var
   p:TPoint;
begin
p.x:=ButtonStop1.Left;
p.y:=ButtonStop1.top+ButtonStop1.Height;
p:=clienttoscreen(p);
popupmenu1.popup(p.x,p.y);
end;

procedure gostopall;
var
  pstopfile:file of byte;
begin
Form_gwrap.ButtonStopAll.visible:=false;
Form_gwrap.Button1.visible:=true;
Form_gwrap.ButtonPause.Visible:=false;
Form_gwrap.ButtonStop.Visible:=false;
Form_gwrap.ButtonStop1.AnchorSideRight.Control:=Form_gwrap.Button1;
assignfile(pstopfile,fget_usrtmp_path+STR_STOPALL);
rewrite(pstopfile);
closefile(pstopfile);
stopped:=true;
if ended=true then go_ok;
end;

procedure TForm_gwrap.ButtonStopAllClick(Sender: TObject);
begin
gostopall;
end;

procedure TForm_gwrap.cbAutoOpenClick(Sender: TObject);
begin
if autoopen=1 then autoopen:=0
else autoopen:=1;
end;

procedure TForm_gwrap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
needinteraction:=false;
if gocancelall=true then
   if pMessageErrorYesNo(txt_5_5_cancelall)=6 then gostopall
   else CloseAction:=caNone;
end;

procedure TForm_gwrap.FormDestroy(Sender: TObject);
begin
{$IFDEF MSWINDOWS}CloseHandle(psem);{$ENDIF}
ExitCode:=exit_code;
end;

procedure TForm_gwrap.FormResize(Sender: TObject);
var respre,resperc:integer;
begin
if Form_gwrap.Width <> 0 then
   begin
   respre:=ShapeProgress.Width;
   ShapeProgress.Width:=(Form_gwrap.Width*iperc) div 100;
   if ShapeGlobalProgress.Visible=true then
      if respre<>0 then
         begin
         resperc:=(ShapeProgress.Width * 100 ) div respre;
         ShapeGlobalProgress.Width:=(ShapeGlobalProgress.Width * resperc) div 100;
         end;
   end;
end;

procedure settheme;
begin
Form_gwrap.Imagestatus.Picture.Bitmap:=Bp1;
if color3='clForm' then color3:=ColorToString(PTACOL);
getpcolors(stringtocolor(color1),stringtocolor(color2),stringtocolor(color3),temperature,contrast);
Form_gwrap.ShapeTitleb1.Brush.Color:=StringToColor(COLLOW);
Form_gwrap.ShapeTitleb2.Brush.Color:=StringToColor(COLLOW);
Form_gwrap.ShapeTitleb3.Brush.Color:=StringToColor(COLLOW);
Form_gwrap.ShapeTitleb4.Brush.Color:=StringToColor(COLLOW);
Form_gwrap.Label1.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.Label5.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.l6.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.Label6.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.Labeli.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.Labelo.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.LabelInfo1.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.LabelInfo2.Font.Color:=pGray;//clInactiveCaptionText;
Form_gwrap.LabelInfo3.Font.Color:=pGray;//clInactiveCaptionText;
if (opacity<100) then
   begin
   Form_gwrap.AlphaBlend:=true;
   Form_gwrap.AlphaBlendValue:=255+opacity-100;
   end;
end;

procedure TForm_gwrap.FormShow(Sender: TObject);
begin
if pldesigned=true then exit;
getdesk_env(desk_env,caption_build,delimiter);
//executable_path, binpath, confpath, sharepath are passed from unit peach prepare_Form_gwrap;
setcurrentdir(executable_path);
peazippath:=executable_path;
{$IFDEF MSWINDOWS}getwinenv(wincomspec,winver);{$ENDIF}
texts(lang_file);
settheme;
pldesigned:=true;
end;

procedure TForm_gwrap.ImageKeepClick(Sender: TObject);
begin
if pcanignore=true then perrignore:=true;
go_ok;
end;

procedure TForm_gwrap.ImageButton2Click(Sender: TObject);
begin
save_report;
end;

procedure TForm_gwrap.ButtonStopClick(Sender: TObject);
begin
stopped:=true;
end;

procedure toggle_pause;
begin
ppause:=not(ppause);
if ppause=false then
   begin
   {$IFDEF MSWINDOWS}
   if okseven=true then
      try
      Form_gwrap.FTaskBarList.SetProgressState(Form_gwrap.AppHandle, TBPF_Normal);
      except
      end;
   {$ENDIF}
   Form_gwrap.ButtonPause.Caption:='   '+txt_pause+'   ';
   Form_gwrap.pm2pause.Caption:=txt_pause;
   Form_gwrap.LabelTitle1.caption:='      '+txt_isrunning+'      ';
   Form_gwrap.PanelTitlePLTabAlign.Width:=getalignw(4);
   Form_gwrap.ShapeProgress.Color:=PGREEN;
   Form_gwrap.ShapeGlobalProgress.Color:=PGREEN;
   end
else
   begin
   {$IFDEF MSWINDOWS}
   if okseven=true then
      try
      Form_gwrap.FTaskBarList.SetProgressState(Form_gwrap.AppHandle, TBPF_PAUSED);
      except
      end;
   {$ENDIF}
   Form_gwrap.ButtonPause.Caption:='   '+txt_resume+'   ';
   Form_gwrap.pm2pause.Caption:=txt_resume;
   Form_gwrap.LabelTitle1.caption:='      '+txt_status+'      ';
   Form_gwrap.PanelTitlePLTabAlign.Width:=getalignw(4);
   Form_gwrap.ShapeProgress.Color:=PYELLOW;
   Form_gwrap.ShapeGlobalProgress.Color:=PYELLOW;
   end;
end;

procedure TForm_gwrap.ButtonPauseClick(Sender: TObject);
begin
toggle_pause;
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
   if Succeeded(ShGetSpecialFolderLocation(Form_gwrap.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
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

procedure clearstopsequencefile;
begin
if fileexists(fget_usrtmp_path+STR_STOPALL) then
try
udeletefile (fget_usrtmp_path+STR_STOPALL);
except
sleep(500);
udeletefile (fget_usrtmp_path+STR_STOPALL);
end;
end;

procedure gwraplaunch;
begin
launched:=true;
pstarted:=false;
ended:=false;
launchwithsemaphore:=false;
gocancelall:=true;
setbuttonsnormal;
Form_gwrap.ImageKeep.Visible:=false;
Form_gwrap.ImageKeep.Caption:='';
Form_gwrap.Label5.Visible:=false;
Form_gwrap.Label6.Visible:=false;
Form_gwrap.LabelWarning1.Visible:=false;
Form_gwrap.labelspac.visible:=false;
pcapt:='';
Form_gwrap.l5.Caption:='';
if pprogn<>'' then
   begin
   Form_gwrap.ShapeGlobalProgress.visible:=true;
   Form_gwrap.ShapeGlobalProgress.height:=pbarhsmall;
   Form_gwrap.ShapeProgress.height:=pbarhsmall;
   end
else
   begin
   Form_gwrap.ShapeGlobalProgress.visible:=false;
   Form_gwrap.ShapeProgress.height:=pbarh;
   end;
iperc:=1;
if exbackground=false then
   if (pprogfirst=true) or (pprogn='') or (pgook=true) then Form_gwrap.Visible:=True;
pgook:=false;
//get job type
//0 archive/extract
//1 test;
//2 benchmark; (3 defrag, unused);
//4 info (list and give message);
//5 list and verbose list;
//20 archive/extract, not using pipes, visible console (mode 10 was removed)
//same + 1000 if launched using semaphore (removed in modeofuse)
modeofuse:=strtoint(pjobtype);
insize:=0;
if modeofuse>=1000 then
   begin
   launchwithsemaphore:=true;
   modeofuse:=modeofuse-1000;
   end;

case modeofuse of
   0: max_l:=4*1024;
   1,4,5: max_l:=4*1024;
   2: begin max_l:=32; insize:=0; ptsize:='0'; end;
   3: max_l:=32;
   end;

//get partial and total input size
pinsize:=0;
try
insize:=strtoqword(ptsize);
if pinputfile<>'na' then
   begin
   in_name:=pinputfile;
   if pprogn='' then
      pinsize:=insize
   else
      srcfilesize_multipart(pinputfile,pinsize);
   end
else
   begin
   in_name:='';
   if pprogn='' then
      pinsize:=insize
   else
      pinsize:=strtoqword(ppsize);
   end;
except
insize:=0;
end;
{$IFDEF MSWINDOWS}
if okseven=true then
   try
   Form_gwrap.FTaskBarList.SetProgressState(Form_gwrap.AppHandle, TBPF_NOPROGRESS);
   except
   end;
{$ENDIF}
if insize=0 then
   begin
   Form_gwrap.Labeli.Visible:=false;
   Form_gwrap.LabelInfo1.Visible:=false;
   end;
Form_gwrap.Labelo.Visible:=false;
Form_gwrap.LabelInfo2.Visible:=false; //set visible during progress
//get output path (finalized only when invoked)
outpath:=poutname;
//get cl
cl:=pcl;

clearstopsequencefile;
stopped:=false;
launched:=false;
launch_cl;
end;

procedure checkseven;
{$IFDEF MSWINDOWS}
var
  osVerInfo: TOSVersionInfo;
{$ENDIF}
begin
okseven:=false;
{$IFDEF MSWINDOWS}
osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
if GetVersionEx(osVerInfo) then
   if osVerInfo.dwMajorVersion>6 then okseven:=true
   else
      if (osVerInfo.dwMajorVersion=6) and (osVerInfo.dwMinorVersion>=1) then okseven:=true;
{$ENDIF}
end;

procedure TForm_gwrap.FormCreate(Sender: TObject);
begin
launched:=true;
needinteraction:=false;
exbackground:=false;
pldesigned:=false;
pgook:=false;
{$IFDEF MSWINDOWS}
checkseven;
if okseven=true then
   try
   AppHandle := Form_gwrap.Handle;
   FTaskBarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList3;
   except
   end;
{$ENDIF}
end;

procedure TForm_gwrap.l2Click(Sender: TObject);
begin
explore_in;
end;

procedure TForm_gwrap.l4Click(Sender: TObject);
begin
explore_out('');
end;

procedure TForm_gwrap.Label4Click(Sender: TObject);
var
   P:TProcessUTF8;
   bin_name,in_param:ansistring;
begin
P:=TProcessUTF8.Create(nil);
in_param:=stringdelim(escapefilename(cl,desk_env));
bin_name:=stringdelim(escapefilename(peazippath,desk_env)+'peazip'+EXEEXT);
{$IFDEF MSWINDOWS}P.Options := [poNoConsole];{$ELSE}P.Options := [poWaitOnExit];{$ENDIF}
cl:=bin_name+' -ext2open '; //ext2open handles a single input in open interface
P.Parameters.Add('-ext2open');
cl:=cl+in_param;//(cl was not transformed in utf8 before)
P.Parameters.Add(in_param);
P.Executable:=bin_name;
if validatecl(cl)<>0 then begin pMessageWarningOK(txt_2_7_validatecl+' '+cl); exit; end;
P.Execute;
P.Free;
Application.Terminate;
end;

procedure cp_search(desk_env:byte);
begin
{$IFDEF MSWINDOWS}
if winver='nt6+' then
   shellexecutew(Form_gwrap.handle, PWideChar('find'), PWideChar(''), PWideChar(''), PWideChar (''), SW_SHOWNORMAL)
else
   cp_open(sharepath+'empty.fnd',desk_env);
{$ENDIF}
{$IFDEF LINUX}cp_search_linuxlike(desk_env);{$ENDIF}//try to search via Gnome or KDE
{$IFDEF FREEBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF OPENBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF DARWIN}cp_search_linuxlike(desk_env);{$ENDIF}
end;

procedure TForm_gwrap.labelopenfile0Click(Sender: TObject);
begin
cp_search(desk_env);
end;

procedure TForm_gwrap.LabelTitle1Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(LabelTitle1,ShapeTitleb1) else clicklabel_launcher(LabelTitle1,Shapelink1);
end;

procedure TForm_gwrap.LabelTitle1MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_launcher(LabelTitle1,ShapeTitleb1) else enterlabel_launcher(LabelTitle1,Shapelink1);
end;

procedure TForm_gwrap.LabelTitle1MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_launcher(LabelTitle1,ShapeTitleb1) else exitlabel_launcher(LabelTitle1,Shapelink1);
end;

procedure TForm_gwrap.LabelTitle2Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(LabelTitle2,ShapeTitleb2) else clicklabel_launcher(LabelTitle2,Shapelink2);
end;

procedure TForm_gwrap.LabelTitle2MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_launcher(LabelTitle2,ShapeTitleb2) else enterlabel_launcher(LabelTitle2,Shapelink2);
end;

procedure TForm_gwrap.LabelTitle2MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_launcher(LabelTitle2,ShapeTitleb2) else exitlabel_launcher(LabelTitle2,Shapelink2);
end;

procedure TForm_gwrap.LabelTitle3Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(LabelTitle3,ShapeTitleb3) else clicklabel_launcher(LabelTitle3,Shapelink3);
end;

procedure TForm_gwrap.LabelTitle3MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_launcher(LabelTitle3,ShapeTitleb3) else enterlabel_launcher(LabelTitle3,Shapelink3);
end;

procedure TForm_gwrap.LabelTitle3MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_launcher(LabelTitle3,ShapeTitleb3) else exitlabel_launcher(LabelTitle3,Shapelink3);
end;

procedure TForm_gwrap.LabelTitle4Click(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(LabelTitle4,ShapeTitleb4) else clicklabel_launcher(LabelTitle4,Shapelink4);
end;

procedure TForm_gwrap.LabelTitle4MouseEnter(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then enterlabel_launcher(LabelTitle4,ShapeTitleb4) else enterlabel_launcher(LabelTitle4,Shapelink4);
end;

procedure TForm_gwrap.LabelTitle4MouseLeave(Sender: TObject);
begin
if (alttabstyle<>1) and (alttabstyle<>4) then exitlabel_launcher(LabelTitle4,ShapeTitleb4) else exitlabel_launcher(LabelTitle4,Shapelink4);
end;

procedure TForm_gwrap.LabelWarning1Click(Sender: TObject);
var s:ansistring;
begin
if (alttabstyle<>1) and (alttabstyle<>4) then clicklabel_launcher(LabelTitle2,ShapeTitleb2) else clicklabel_launcher(LabelTitle2,Shapelink2);
if (modeofuse=1) or (modeofuse=4) or (modeofuse=5) then s:=txt_3_0_arc+char($0D)+char($0A)+txt_3_0_ext
else
   if extractfilename(outpath)='' then s:=txt_3_0_arc+char($0D)+char($0A)+txt_3_0_ext
   else s:=txt_3_0_arc;
s:=s+char($0D)+char($0A)+char($0D)+char($0A)+txt_3_0_details;
if stopped=true then s:=txt_jobstopped+char($0D)+char($0A)+char($0D)+char($0A)+txt_3_0_details;
pMessageWarningOK(s);
end;

procedure TForm_gwrap.pm2cancelallClick(Sender: TObject);
begin
exbackground:=false;
TrayIcon1.visible:=false;
gostopall;
end;

procedure TForm_gwrap.pm2cancelClick(Sender: TObject);
begin
exbackground:=false;
TrayIcon1.visible:=false;
stopped:=true;
end;

procedure TForm_gwrap.pm2eiClick(Sender: TObject);
begin
explore_in;
end;

procedure TForm_gwrap.pm2eoClick(Sender: TObject);
begin
explore_out('');
end;

procedure TForm_gwrap.pm2etClick(Sender: TObject);
begin
explore_out(poutnamet);
end;

procedure do_explorepath;
begin
{$IFDEF MSWINDOWS}
if winver<>'nt6+' then
   ShellExecuteW(Form_gwrap.Handle, PWideChar  ('open'), PWideChar ('Explorer'), PWideChar  (''), PWideChar  (''), SW_SHOWNORMAL)
else
   ShellExecuteW(Form_gwrap.Handle, PWideChar  ('open'), PWideChar ('Explorer'), PWideChar  ('/E,::{20D04FE0-3AEA-1069-A2D8-08002B30309D}'), PWideChar  (''), SW_SHOWNORMAL);
{$ELSE}
cp_open('/',desk_env);
{$ENDIF}
end;

procedure TForm_gwrap.pm2exploreClick(Sender: TObject);
begin
do_explorepath;
end;

procedure TForm_gwrap.pm2pauseClick(Sender: TObject);
begin
toggle_pause;
end;

procedure TForm_gwrap.pm2restoreClick(Sender: TObject);
begin
exbackground:=false;
TrayIcon1.visible:=false;
Form_gwrap.visible:=true;
end;

procedure TForm_gwrap.pm2searchClick(Sender: TObject);
begin
cp_search(desk_env);
end;

procedure TForm_gwrap.pmbackgroundClick(Sender: TObject);
begin
exbackground:=true;
TrayIcon1.visible:=true;
Form_gwrap.visible:=false;
end;

procedure TForm_gwrap.pmeiClick(Sender: TObject);
begin
explore_in;
end;

procedure TForm_gwrap.pmeoClick(Sender: TObject);
begin
explore_out('');
end;

procedure TForm_gwrap.pmetClick(Sender: TObject);
begin
explore_out(poutnamet);
end;

procedure TForm_gwrap.pmexploreClick(Sender: TObject);
begin
do_explorepath;
end;

procedure TForm_gwrap.pmsearchClick(Sender: TObject);
begin
cp_search(desk_env);
end;

initialization
  {$I unit_gwrap.lrs}

  {$IFDEF MSWINDOWS}
  OleInitialize(nil);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  OleUninitialize
  {$ENDIF}

end.

