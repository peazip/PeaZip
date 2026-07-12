unit UnitImgViewer; //Image viewer form: mzoom in and out 5-1000% or fit to screen height/width, switch to immersive mode, browse next previous first last item in the list received from the file browser, replicate delete and rename file browser's functions

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ActnList, Grids, Menus, Process, StdCtrls, UTF8Process, LCLType, LCLIntf,
  UnitDlg, UnitInput, img_utils, list_utils, externalprograms;

type

  { TFormImgViewer }

  TFormImgViewer = class(TForm)
    renamevieweritem: TAction;
    mdelete: TMenuItem;
    mrename: TMenuItem;
    mquickdelete: TMenuItem;
    mzerodelete: TMenuItem;
    msecuredelete: TMenuItem;
    mtrash: TMenuItem;
    mupdate: TMenuItem;
    mzoomfit: TMenuItem;
    mzoom300: TMenuItem;
    mzoom400: TMenuItem;
    mzoom500: TMenuItem;
    mzoomfitw: TMenuItem;
    mzoomfith: TMenuItem;
    mzoom20: TMenuItem;
    mzoom25: TMenuItem;
    mzoom33: TMenuItem;
    mzoom50: TMenuItem;
    mzoom100: TMenuItem;
    mzoom200: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    mzoomout: TMenuItem;
    mzoomin: TMenuItem;
    mzoom: TMenuItem;
    Separator2: TMenuItem;
    mexplorepath: TMenuItem;
    mproperties: TMenuItem;
    mrefresh: TMenuItem;
    Separator1: TMenuItem;
    mfirst: TMenuItem;
    mlast: TMenuItem;
    mfwd: TMenuItem;
    mback: TMenuItem;
    OpenNextarrow: TAction;
    OpenPrevarrow: TAction;
    PopupViewer: TPopupMenu;
    StringGridImgViewer: TStringGrid;
    Zoom33n: TAction;
    Zoom25n: TAction;
    Zoom20n: TAction;
    Zoom50n: TAction;
    ZoomFitn: TAction;
    Zoom200n: TAction;
    Zoom300n: TAction;
    Zoom400n: TAction;
    Zoom500n: TAction;
    Zoom100n: TAction;
    ExitImmersive: TAction;
    EnterImmersive: TAction;
    ZoomInplusn: TAction;
    ZoomOutplusn: TAction;
    ZoomInplus: TAction;
    ZoomOutplus: TAction;
    ActionListImageViewer: TActionList;
    ImageViewer: TImage;
    procedure EnterImmersiveExecute(Sender: TObject);
    procedure ExitImmersiveExecute(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure ImageViewerClick(Sender: TObject);
    procedure ImageViewerDblClick(Sender: TObject);
    procedure ImageViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mbackClick(Sender: TObject);
    procedure mexplorepathClick(Sender: TObject);
    procedure mfirstClick(Sender: TObject);
    procedure mlastClick(Sender: TObject);
    procedure mfwdClick(Sender: TObject);
    procedure mpropertiesClick(Sender: TObject);
    procedure mquickdeleteClick(Sender: TObject);
    procedure mrefreshClick(Sender: TObject);
    procedure mrenameClick(Sender: TObject);
    procedure msecuredeleteClick(Sender: TObject);
    procedure mtrashClick(Sender: TObject);
    procedure mupdateClick(Sender: TObject);
    procedure mzerodeleteClick(Sender: TObject);
    procedure mzoom100Click(Sender: TObject);
    procedure mzoom200Click(Sender: TObject);
    procedure mzoom20Click(Sender: TObject);
    procedure mzoom25Click(Sender: TObject);
    procedure mzoom300Click(Sender: TObject);
    procedure mzoom33Click(Sender: TObject);
    procedure mzoom400Click(Sender: TObject);
    procedure mzoom500Click(Sender: TObject);
    procedure mzoom50Click(Sender: TObject);
    procedure mzoomfitClick(Sender: TObject);
    procedure mzoomfithClick(Sender: TObject);
    procedure mzoomfitwClick(Sender: TObject);
    procedure mzoominClick(Sender: TObject);
    procedure mzoomoutClick(Sender: TObject);
    procedure OpenNextarrowExecute(Sender: TObject);
    procedure OpenPrevarrowExecute(Sender: TObject);
    procedure renamevieweritemExecute(Sender: TObject);
    procedure Zoom20nExecute(Sender: TObject);
    procedure Zoom25nExecute(Sender: TObject);
    procedure Zoom33nExecute(Sender: TObject);
    procedure Zoom50nExecute(Sender: TObject);
    procedure Zoom100nExecute(Sender: TObject);
    procedure Zoom200nExecute(Sender: TObject);
    procedure Zoom300nExecute(Sender: TObject);
    procedure Zoom400nExecute(Sender: TObject);
    procedure Zoom500nExecute(Sender: TObject);
    procedure ZoomFitnExecute(Sender: TObject);
    procedure ZoomInplusExecute(Sender: TObject);
    procedure ZoomInplusnExecute(Sender: TObject);
    procedure ZoomOutplusExecute(Sender: TObject);
    procedure ZoomOutplusnExecute(Sender: TObject);
  private

  public

  end;

var
  FormImgViewer: TFormImgViewer;
  txt_10_7_errload,txt_10_7_errfile,executable_path,txt_2_7_validatecl,
  txt_delete,txt_wipe,txt_5_2_zerodelete,txt_4_7_recycle,txt_reset,
  txt_5_5_lower,txt_5_5_upper,txt_timestamp,txt_6_4_appdirn,txt_6_4_prepdirn,
  txt_rename,txt_name_provide,txt_checkname_failed,imageviewername,fun:ansistring;
  imgviewerzoom,imgviewersizeh,imgviewersizew,imgviewersrc:integer;
  filesizebase,desk_env,erasepasses:byte;
  viewerbusy,formissnapped:boolean;

procedure applyzoom(mode:ansistring);

implementation

{ TFormImgViewer }

procedure errorloadimg(errn:integer);
begin
FormImgViewer.ImageViewer.Visible:=False;
FormImgViewer.Caption:=FormImgViewer.StringGridImgViewer.Cells[1,UnitImgViewer.imgviewersrc]+
 ' '+FormImgViewer.StringGridImgViewer.Cells[5,UnitImgViewer.imgviewersrc]+
 ' '+nicenumber(FormImgViewer.StringGridImgViewer.Cells[3,UnitImgViewer.imgviewersrc],filesizebase);
case errn of
  0: FormImgViewer.Caption:=FormImgViewer.Caption+' '+UpCase(txt_10_7_errfile);
  1: FormImgViewer.Caption:=FormImgViewer.Caption+' '+UpCase(txt_10_7_errload);
  end;
Application.ProcessMessages;
viewerbusy:=false;
end;

procedure snaptosize;
begin
if formissnapped=true then exit;
if (FormImgViewer.BorderStyle<>bsNone) and (FormImgViewer.WindowState<>wsMaximized) then
   begin
   FormImgViewer.Height:=FormImgViewer.ImageViewer.Height;
   FormImgViewer.Width:=FormImgViewer.ImageViewer.Width;
   formissnapped:=true;
   end;
end;

procedure applyzoom(mode:ansistring);
var
  BImageViewer:TBitmap;
  zoomedimgviewersizeh,zoomedimgviewersizew,sheight,swidth:integer;
  zoomstring,pfinfo,mby,sizetimestr:ansistring;
begin
try
if viewerbusy=true then exit;
viewerbusy:=true;
if not FileExists(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]) then
   begin
   errorloadimg(0);
   exit;
   end;
formissnapped:=false;

//detect usable vertical and horizontal size in windowed mode
if FormImgViewer.BorderStyle<>bsNone then
   begin
   sheight:=GetSystemMetrics(SM_CYFULLSCREEN);
   swidth:=GetSystemMetrics(SM_CXFULLSCREEN);
   end
else
   begin
   sheight:=Screen.Height;
   swidth:=Screen.Width;
   end;

case mode of
 'in': if imgviewerzoom<22 then imgviewerzoom:=imgviewerzoom+1 else begin viewerbusy:=false; exit; end;
 'out': if imgviewerzoom>0 then imgviewerzoom:=imgviewerzoom-1 else begin viewerbusy:=false; exit; end;
 '100': imgviewerzoom:=10;
 '200': imgviewerzoom:=15;
 '300': imgviewerzoom:=17;
 '400': imgviewerzoom:=18;
 '500': imgviewerzoom:=19;
 '50': imgviewerzoom:=6;
 '33': imgviewerzoom:=4;
 '25': imgviewerzoom:=3;
 '20': imgviewerzoom:=2;
 'fit': imgviewerzoom:=30; //fit to smallest screen dimension
 'fith': imgviewerzoom:=31;
 'fitw': imgviewerzoom:=32;
 'open': imgviewerzoom:=33; //if larger fit to smallest screen dimension, else 100%
 end;
//FormImgViewer.ImageViewer.Visible:=False;
sizetimestr:=trim(nicenumber(FormImgViewer.StringGridImgViewer.Cells[3,UnitImgViewer.imgviewersrc],filesizebase))+
 ' '+trim(FormImgViewer.StringGridImgViewer.Cells[5,UnitImgViewer.imgviewersrc]);
FormImgViewer.Caption:=FormImgViewer.StringGridImgViewer.Cells[1,UnitImgViewer.imgviewersrc]+' '+sizetimestr+ ' ...';
Application.ProcessMessages;
BImageViewer:=TBitmap.Create;
BImageViewer.Transparent:=true;
getthemedbitmap(BImageViewer,FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]);
mby:=getmagicbytes_img(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]);
imgviewersizeh:=BImageViewer.Height;
imgviewersizew:=BImageViewer.Width;
case BImageViewer.PixelFormat of
    pfDevice: pfinfo:='Device';
    pf1bit: pfinfo:='1';
    pf4bit: pfinfo:='4';
    pf8bit: pfinfo:='8';
    pf15bit: pfinfo:='15';
    pf16bit: pfinfo:='16';
    pf24bit: pfinfo:='24';
    pf32bit: pfinfo:='32';
    pfCustom: pfinfo:='Custom';
    else pfinfo:='Unknown';
end;
case imgviewerzoom of
 0:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh) div 20;
   zoomedimgviewersizew:=(imgviewersizew) div 20;
   zoomstring:='5%';
   end;
 1:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh) div 10;
   zoomedimgviewersizew:=(imgviewersizew) div 10;
   zoomstring:='10%';
   end;
 2:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*20) div 100;
   zoomedimgviewersizew:=(imgviewersizew*20) div 100;
   zoomstring:='20%';
   end;
 3:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh) div 4;
   zoomedimgviewersizew:=(imgviewersizew) div 4;
   zoomstring:='25%';
   end;
 4:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*33) div 100;
   zoomedimgviewersizew:=(imgviewersizew*33) div 100;
   zoomstring:='33%';
   end;
 5:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*40) div 100;
   zoomedimgviewersizew:=(imgviewersizew*40) div 100;
   zoomstring:='40%';
   end;
 6:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh) div 2;
   zoomedimgviewersizew:=(imgviewersizew) div 2;
   zoomstring:='50%';
   end;
 7:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*67) div 100;
   zoomedimgviewersizew:=(imgviewersizew*67) div 100;
   zoomstring:='67%';
   end;
 8:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*80) div 100;
   zoomedimgviewersizew:=(imgviewersizew*80) div 100;
   zoomstring:='80%';
   end;
 9:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*87) div 100;
   zoomedimgviewersizew:=(imgviewersizew*87) div 100;
   zoomstring:='87%';
   end;
 10:
   begin
   zoomedimgviewersizeh:=imgviewersizeh;
   zoomedimgviewersizew:=imgviewersizew;
   zoomstring:='100%';
   end;
 11:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*115) div 100;
   zoomedimgviewersizew:=(imgviewersizew*115) div 100;
   zoomstring:='115%';
   end;
 12:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*125) div 100;
   zoomedimgviewersizew:=(imgviewersizew*125) div 100;
   zoomstring:='125%';
   end;
 13:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*3) div 2;
   zoomedimgviewersizew:=(imgviewersizew*3) div 2;
   zoomstring:='150%';
   end;
 14:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*175) div 100;
   zoomedimgviewersizew:=(imgviewersizew*175) div 100;
   zoomstring:='175%';
   end;
 15:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*2);
   zoomedimgviewersizew:=(imgviewersizew*2);
   zoomstring:='200%';
   end;
 16:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*5) div 2;
   zoomedimgviewersizew:=(imgviewersizew*5) div 2;
   zoomstring:='250%';
   end;
 17:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*3);
   zoomedimgviewersizew:=(imgviewersizew*3);
   zoomstring:='300%';
   end;
 18:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*4);
   zoomedimgviewersizew:=(imgviewersizew*4);
   zoomstring:='400%';
   end;
 19:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*5);
   zoomedimgviewersizew:=(imgviewersizew*5);
   zoomstring:='500%';
   end;
 20:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*6);
   zoomedimgviewersizew:=(imgviewersizew*6);
   zoomstring:='600%';
   end;
 21:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*8);
   zoomedimgviewersizew:=(imgviewersizew*8);
   zoomstring:='800%';
   end;
 22:
   begin
   zoomedimgviewersizeh:=(imgviewersizeh*10);
   zoomedimgviewersizew:=(imgviewersizew*10);
   zoomstring:='1000%';
   end;
 30: begin
     if ((swidth*100) div imgviewersizew) < ((sheight*100) div imgviewersizeh) then
        begin
        zoomedimgviewersizew:=swidth;
        zoomedimgviewersizeh:=(imgviewersizeh*swidth) div imgviewersizew;
        end
     else
        begin
        zoomedimgviewersizeh:=sheight;
        zoomedimgviewersizew:=(imgviewersizew*sheight) div imgviewersizeh;
        end;
     zoomstring:=FormImgViewer.mzoomfit.Caption;
     imgviewerzoom:=10;
     end;
 31:
   begin
   zoomedimgviewersizeh:=sheight;
   zoomedimgviewersizew:=(imgviewersizew*sheight) div imgviewersizeh;
   zoomstring:=FormImgViewer.mzoomfith.Caption;
   imgviewerzoom:=10;
   end;
 32:
   begin
   zoomedimgviewersizew:=swidth;
   zoomedimgviewersizeh:=(imgviewersizeh*swidth) div imgviewersizew;
   zoomstring:=FormImgViewer.mzoomfitw.Caption;
   imgviewerzoom:=10;
   end;
  33:
    begin
    if ((swidth*100) div imgviewersizew) < ((sheight*100) div imgviewersizeh) then
       begin
       zoomedimgviewersizew:=swidth;
       zoomedimgviewersizeh:=(imgviewersizeh*swidth) div imgviewersizew;
       end
    else
       begin
       zoomedimgviewersizeh:=sheight;
       zoomedimgviewersizew:=(imgviewersizew*sheight) div imgviewersizeh;
       end;
    if (imgviewersizew<zoomedimgviewersizew) and (imgviewersizeh<zoomedimgviewersizeh) then
       begin
       zoomedimgviewersizew:=imgviewersizew;
       zoomedimgviewersizeh:=imgviewersizeh;
       zoomstring:='100%';
       end
    else
       zoomstring:=FormImgViewer.mzoomfit.Caption;
    imgviewerzoom:=10;
    end;
 end;
FormImgViewer.Caption:=FormImgViewer.StringGridImgViewer.Cells[1,UnitImgViewer.imgviewersrc]+' '+sizetimestr+
 ' | '+inttostr(imgviewersizew)+'*'+inttostr(imgviewersizeh)+'@'+pfinfo+' '+mby+' | '+zoomstring;
if zoomedimgviewersizeh<2 then zoomedimgviewersizeh:=2;
if zoomedimgviewersizew<2 then zoomedimgviewersizew:=2;
Application.ProcessMessages;
if zoomstring<>'100%' then resize_bitmap(BImageViewer,zoomedimgviewersizew,zoomedimgviewersizeh);
FormImgViewer.ImageViewer.Transparent:=true;
FormImgViewer.ImageViewer.Height:=BImageViewer.Height;
FormImgViewer.ImageViewer.Width:=BImageViewer.Width;
FormImgViewer.ImageViewer.Picture.Bitmap:=BImageViewer;
if FormImgViewer.ImageViewer.Visible=False then FormImgViewer.ImageViewer.Visible:=True;
snaptosize;

//the default fullscreen windowed mode is forced to show the horizontal scroll bar if needed (autoscroll seems forcing only the vertical one)
if (FormImgViewer.WindowState=wsMaximized) and (FormImgViewer.BorderStyle<>bsNone) and (zoomedimgviewersizew>Screen.Width) then
   begin
   FormImgViewer.AutoScroll:=true;
   FormImgViewer.ImageViewer.AnchorSideLeft.Side:=asrLeft;
   FormImgViewer.HorzScrollBar.position:=(FormImgViewer.HorzScrollBar.Range - ((FormImgViewer.HorzScrollBar.Range*Screen.Width) div zoomedimgviewersizew)) div 2;
   end
else
   FormImgViewer.ImageViewer.AnchorSideLeft.Side:=asrCenter;

//avoid placing the window's controls outside the screen
if (FormImgViewer.WindowState<>wsMaximized) then
   begin
   if FormImgViewer.Top<0 then FormImgViewer.Top:=0;
   if FormImgViewer.Left<0 then FormImgViewer.Left:=0;
   end;

BImageViewer.Free;
Application.ProcessMessages;
viewerbusy:=false;
except
errorloadimg(1);
end;
end;

procedure setimmersiveviewer;
begin
if FormImgViewer.BorderStyle<>bsNone then
   begin
   FormImgViewer.BorderStyle:=bsNone;
   FormImgViewer.WindowState:=wsMaximized;
   end
else
   begin
   FormImgViewer.BorderStyle:=bsSizeable;
   FormImgViewer.WindowState:=wsMaximized;
   end;
applyzoom('open'); //restore default zoom when toggling the immersive mode (windowed mode has usable screen size different from full screen mode)
end;

procedure open_first_item;
var
  i:integer;
begin
if fun<>'FILEBROWSER' then exit;
if viewerbusy=true then exit;
for i:=1 to FormImgViewer.StringGridImgViewer.RowCount-1 do
   if supportedimgtype(FormImgViewer.StringGridImgViewer.Cells[2,i])=true then
      begin
      if not FileExists(FormImgViewer.StringGridImgViewer.Cells[12,i]) then Continue;
      imgviewersrc:=i;
      applyzoom('open');
      exit;
      end;
end;

procedure open_last_item;
var
  i:integer;
begin
if fun<>'FILEBROWSER' then exit;
if viewerbusy=true then exit;
for i:=FormImgViewer.StringGridImgViewer.RowCount-1 downto 1 do
   if supportedimgtype(FormImgViewer.StringGridImgViewer.Cells[2,i])=true then
      begin
      if not FileExists(FormImgViewer.StringGridImgViewer.Cells[12,i]) then Continue;
      imgviewersrc:=i;
      applyzoom('open');
      exit;
      end;
applyzoom('open');//if no match (e.g. no valid item) refresh to show no content
end;

procedure open_next_item;
var
  i:integer;
begin
if fun<>'FILEBROWSER' then exit;
if viewerbusy=true then exit;
for i:=imgviewersrc+1 to FormImgViewer.StringGridImgViewer.RowCount-1 do
   if supportedimgtype(FormImgViewer.StringGridImgViewer.Cells[2,i])=true then
      begin
      if not FileExists(FormImgViewer.StringGridImgViewer.Cells[12,i]) then Continue;
      imgviewersrc:=i;
      applyzoom('open');
      exit;
      end;
open_last_item;//if no match (e.g. delete wile on last item) try to open last valid item
end;

procedure open_prev_item;
var
  i:integer;
begin
if fun<>'FILEBROWSER' then exit;
if viewerbusy=true then exit;
for i:=imgviewersrc-1 downto 1 do
   if supportedimgtype(FormImgViewer.StringGridImgViewer.Cells[2,i])=true then
      begin
      if not FileExists(FormImgViewer.StringGridImgViewer.Cells[12,i]) then Continue;
      imgviewersrc:=i;
      applyzoom('open');
      exit;
      end;
end;

procedure erase_fromname(erasemode: integer);//not enabled from within archives
//erase modes: 0 quick, 1 secure (get erase passes from global variable set in settings), 2 overwrite with zero, 3 send to Trash
var
   P:tprocessutf8;
   bin_name,eraselevel,pstr,cl,in_param,in_name:ansistring;
begin
if fun<>'FILEBROWSER' then exit;
in_param:=stringdelim(escapefilename(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc],desk_env));
if in_param='' then exit; //input parameter must be already set from the calling procedure
in_name:=FormImgViewer.StringGridImgViewer.Cells[1,UnitImgViewer.imgviewersrc];
bin_name:=stringdelim(escapefilename(executable_path,desk_env)+'pea'+EXEEXT);
case erasemode of
   0: pstr:=txt_delete;
   1: pstr:=txt_wipe;
   2: pstr:=txt_5_2_zerodelete;
   3: pstr:=txt_4_7_recycle;
   end;
case erasepasses of
   0: eraselevel:='VERY_FAST';
   1: eraselevel:='FAST';
   2: eraselevel:='MEDIUM';
   3: eraselevel:='SLOW';
   4: eraselevel:='VERY_SLOW';
   end;
if erasemode=0 then eraselevel:='QUICK';
if erasemode=2 then eraselevel:='ZERO';
if erasemode=3 then eraselevel:='RECYCLE';//enabled only in Windows and macOS
if pMessageWarningYesNo(pstr+char($0D)+char($0A)+char($0D)+char($0A)+in_name)=6 then
   begin
   cl:=bin_name+' WIPE '+eraselevel+' '+in_param;
   if validatecl(cl)<>0 then begin pMessageWarningOK(txt_2_7_validatecl+' '+cl); exit; end;
   P:=tprocessutf8.Create(nil);
   {$IFDEF MSWINDOWS}P.Options := [poNoConsole, poWaitOnExit];{$ELSE}P.Options := [poWaitOnExit];{$ENDIF}
   peapexecute(P,cl);
   P.Free;
   open_next_item;
   end;
end;

procedure TFormImgViewer.ZoomInplusnExecute(Sender: TObject);
begin
applyzoom('in');
end;

procedure TFormImgViewer.ZoomInplusExecute(Sender: TObject);
begin
applyzoom('in');
end;

procedure TFormImgViewer.Zoom100nExecute(Sender: TObject);
begin
applyzoom('100');
end;

procedure TFormImgViewer.Zoom200nExecute(Sender: TObject);
begin
applyzoom('200');
end;

procedure TFormImgViewer.Zoom300nExecute(Sender: TObject);
begin
applyzoom('300');
end;

procedure TFormImgViewer.Zoom400nExecute(Sender: TObject);
begin
applyzoom('400');
end;

procedure TFormImgViewer.Zoom500nExecute(Sender: TObject);
begin
applyzoom('500');
end;

procedure TFormImgViewer.ZoomFitnExecute(Sender: TObject);
begin
applyzoom('fit');
end;

procedure TFormImgViewer.FormDblClick(Sender: TObject);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then
else
   if tmppt.x>FormImgViewer.Width-64 then
   else setimmersiveviewer;
except
end;
end;

procedure TFormImgViewer.ImageViewerDblClick(Sender: TObject);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then
else
   if tmppt.x>FormImgViewer.Width-64 then
   else setimmersiveviewer;
except
end;
end;

procedure TFormImgViewer.EnterImmersiveExecute(Sender: TObject);
begin
setimmersiveviewer;
end;

procedure TFormImgViewer.ExitImmersiveExecute(Sender: TObject);
begin
if FormImgViewer.BorderStyle<>bsNone then
   FormImgViewer.Close
else
   begin
   FormImgViewer.BorderStyle:=bsSizeable;
   FormImgViewer.WindowState:=wsMaximized;
   end;
end;

procedure TFormImgViewer.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then FormImgViewer.Cursor:=crHandPoint
else
   if tmppt.x>FormImgViewer.Width-64 then FormImgViewer.Cursor:=crHandPoint
   else FormImgViewer.Cursor:=crDefault;
ImageViewer.Cursor:=FormImgViewer.Cursor;
except
end;
end;

procedure TFormImgViewer.FormResize(Sender: TObject);
begin
if (FormImgViewer.BorderStyle<>bsNone) and (FormImgViewer.WindowState<>wsMaximized) then
   snaptosize
else
   formissnapped:=false;
end;

procedure TFormImgViewer.ImageViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then ImageViewer.Cursor:=crHandPoint
else
   if tmppt.x>FormImgViewer.Width-64 then ImageViewer.Cursor:=crHandPoint
   else ImageViewer.Cursor:=crDefault;
FormImgViewer.Cursor:=ImageViewer.Cursor;
except
end;
end;

procedure TFormImgViewer.FormClick(Sender: TObject);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then open_prev_item
else
   if tmppt.x>FormImgViewer.Width-64 then open_next_item;
except
end;
end;

procedure TFormImgViewer.ImageViewerClick(Sender: TObject);
var
   tmppt:tpoint;
begin
try
tmppt:=mouse.CursorPos;
tmppt:=FormImgViewer.ScreenToClient(tmppt);
if tmppt.x<64 then open_prev_item
else
   if tmppt.x>FormImgViewer.Width-64 then open_next_item;
except
end;
end;

procedure TFormImgViewer.FormCreate(Sender: TObject);
begin
viewerbusy:=false;
end;

procedure TFormImgViewer.mbackClick(Sender: TObject);
begin
open_prev_item;
end;

procedure cp_explorepath_viewer(s:AnsiString; desk_env: byte);
begin
{$IFDEF MSWINDOWS}
winexplorepath(s);
{$ELSE}
{$IFDEF DARWIN}
macexplorepath(s);
{$ENDIF}
cp_open_linuxlike(extractfilepath(s),desk_env);
{$ENDIF}
end;

procedure TFormImgViewer.mexplorepathClick(Sender: TObject);
begin
cp_explorepath_viewer(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc],desk_env);
end;

procedure TFormImgViewer.mfirstClick(Sender: TObject);
begin
open_first_item;
end;

procedure TFormImgViewer.mlastClick(Sender: TObject);
begin
open_last_item;
end;

procedure TFormImgViewer.mfwdClick(Sender: TObject);
begin
open_next_item;
end;

procedure TFormImgViewer.mpropertiesClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
spars:TStringList;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
spars:=TStringList.Create;
spars.Add(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]);
ShowFileProperties(spars, 0);
spars.Free;
{$ENDIF}
end;

procedure TFormImgViewer.mquickdeleteClick(Sender: TObject);
begin
erase_fromname(0);
end;

procedure TFormImgViewer.mrefreshClick(Sender: TObject);
begin
applyzoom('open');
end;


function pviewerInputQuery ( const FCaption, FPrompt, Fwarning : ansistring; var UserValue : ansistring) : Boolean;
var
   cbflag,i:integer;
begin
result:=false;
UnitInput.tsstyle:=tsstyle;
UnitInput.dirn:=extractfilepath(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]);
UnitInput.instr:=UserValue;
UnitInput.moveclick:=0;
FormInput.LabelMoveTo.Visible:=false;
FormInput.Labelinputspacer3.Visible:=false;
FormInput.CheckBoxInputRecoursive.Visible:=false;
FormInput.BitBtnReset.Visible:=false;
FormInput.BitBtnBrowse.Visible:=false;
FormInput.Labelrenamereset.Caption:=txt_reset;
FormInput.LabelLow.Caption:=txt_5_5_lower;
FormInput.LabelUp.Caption:=txt_5_5_upper;
FormInput.LabelTimestamp.Caption:=txt_timestamp;
FormInput.Labelappdirn.Caption:=txt_6_4_appdirn;
FormInput.Labelprepdirn.Caption:=txt_6_4_prepdirn;
FormInput.ButtonPanelInput.OKButton.Caption:=txt_2_7_ok;
FormInput.ButtonPanelInput.CancelButton.Caption:=txt_2_7_cancel;
FormInput.LabelLow.Visible:=true;
FormInput.Labelinputspacer1.Visible:=true;
FormInput.LabelUp.Visible:=true;
FormInput.Labelinputspacer2.Visible:=true;
FormInput.LabelTimestamp.Visible:=true;
FormInput.Labelappdirn.Visible:=true;
FormInput.Labelinputspacer4.Visible:=true;
FormInput.Labelprepdirn.Visible:=true;
FormInput.Labelrenamereset.Visible:=true;
FormInput.caption:=FCaption;
FormInput.Editinputquery.Clear;
FormInput.editinputquery.hint:=FPrompt;
FormInput.EditInputQuery.Text:=UserValue;
FormInput.Labelwarning.caption:=FWarning;
FormInput.Showmodal;
case FormInput.ModalResult of
   mrOk:
      begin
      UserValue:=FormInput.EditInputQuery.Text;
      result:=true;
      end;
   mrCancel:
      begin
      UserValue:='';
      end;
   mrAbort:
      begin
      UserValue:='';
      end;
   end;
end;

procedure viewerrename; //not enabled within archives
var
   s:ansistring;
begin
if fun<>'FILEBROWSER' then exit;
s:=extractfilename(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc]);
FormInput.LabelMoveTo.Visible:=true;
FormInput.Labelinputspacer3.Visible:=true;
if not pviewerInputQuery(txt_rename, txt_name_provide, '', s) then exit;
if checkfilename(s)<>0 then
   begin
   pMessageWarningOK('"'+s+'" '+txt_checkname_failed+char($0D)+char($0A)+txt_name_provide);
   exit;
   end;
renamefile(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc],extractfilepath(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc])+s);
imageviewername:=extractfilepath(FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc])+s;
FormImgViewer.ModalResult:=mrRetry;
end;

procedure TFormImgViewer.mrenameClick(Sender: TObject);
begin
viewerrename;
end;

procedure TFormImgViewer.msecuredeleteClick(Sender: TObject);
begin
erase_fromname(1);
end;

procedure TFormImgViewer.mtrashClick(Sender: TObject);
begin
erase_fromname(3);
end;

procedure TFormImgViewer.mupdateClick(Sender: TObject); //not needed/enabled within archives
begin
if fun<>'FILEBROWSER' then exit;
imageviewername:=FormImgViewer.StringGridImgViewer.Cells[12,UnitImgViewer.imgviewersrc];
FormImgViewer.ModalResult:=mrRetry;
end;

procedure TFormImgViewer.mzerodeleteClick(Sender: TObject);
begin
erase_fromname(2);
end;

procedure TFormImgViewer.mzoom100Click(Sender: TObject);
begin
applyzoom('100');
end;

procedure TFormImgViewer.mzoom200Click(Sender: TObject);
begin
applyzoom('200');
end;

procedure TFormImgViewer.mzoom20Click(Sender: TObject);
begin
applyzoom('20');
end;

procedure TFormImgViewer.mzoom25Click(Sender: TObject);
begin
applyzoom('25');
end;

procedure TFormImgViewer.mzoom300Click(Sender: TObject);
begin
applyzoom('300');
end;

procedure TFormImgViewer.mzoom33Click(Sender: TObject);
begin
applyzoom('33');
end;

procedure TFormImgViewer.mzoom400Click(Sender: TObject);
begin
applyzoom('400');
end;

procedure TFormImgViewer.mzoom500Click(Sender: TObject);
begin
applyzoom('500');
end;

procedure TFormImgViewer.mzoom50Click(Sender: TObject);
begin
applyzoom('50');
end;

procedure TFormImgViewer.mzoomfitClick(Sender: TObject);
begin
applyzoom('fit');
end;

procedure TFormImgViewer.mzoomfithClick(Sender: TObject);
begin
applyzoom('fith');
end;

procedure TFormImgViewer.mzoomfitwClick(Sender: TObject);
begin
applyzoom('fitw');
end;

procedure TFormImgViewer.mzoominClick(Sender: TObject);
begin
applyzoom('in');
end;

procedure TFormImgViewer.mzoomoutClick(Sender: TObject);
begin
applyzoom('out');
end;

procedure TFormImgViewer.OpenNextarrowExecute(Sender: TObject);
begin
open_next_item;
end;

procedure TFormImgViewer.OpenPrevarrowExecute(Sender: TObject);
begin
open_prev_item;
end;

procedure TFormImgViewer.renamevieweritemExecute(Sender: TObject);
begin
viewerrename;
end;

procedure TFormImgViewer.Zoom20nExecute(Sender: TObject);
begin
applyzoom('20');
end;

procedure TFormImgViewer.Zoom25nExecute(Sender: TObject);
begin
applyzoom('25');
end;

procedure TFormImgViewer.Zoom33nExecute(Sender: TObject);
begin
applyzoom('33');
end;

procedure TFormImgViewer.Zoom50nExecute(Sender: TObject);
begin
applyzoom('50');
end;

procedure TFormImgViewer.ZoomOutplusExecute(Sender: TObject);
begin
applyzoom('out');
end;

procedure TFormImgViewer.ZoomOutplusnExecute(Sender: TObject);
begin
applyzoom('out');
end;

initialization
  {$I unitimgviewer.lrs}

end.

