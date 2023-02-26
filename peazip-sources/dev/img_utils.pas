unit img_utils;
{
 DESCRIPTION     :  Unit providing routines related to handling images

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20101030  G.Tani      Initial version
 0.11     20110816  G.Tani      Improved getting string with image details
 0.12     20130215  G.Tani      Improced decorations, improved some cases of error handling if images are not correctly loaded
 0.13     20130602  G.Tani      Code cleanup, fixed recognition of uncommon jpeg extensions
 0.14     20140924  G.Tani      Fixes; Added conversion to TIFF, PPM, and XPM formats
 0.15     20160718  G.Tani      Added code to show exe icons (Windows)
 0.16     20170423  G.Tani      Fixed color modification modcolor, can now either make the color darker or lighter
                                Added proportional color modification modpropcolor
                                Added color darkness evaluation function evalcolor
 0.17     20181206  G.Tani      Can now convert images to ico format
 0.18     20190821  G.Tani      New resize_bitmap function
 0.19     20191125  G.Tani      Improved and extended functions related to load and resize transparent bitmaps
 0.20     20211012  G.Tani      Added temperature parameter to modpropcolor to create warmer or colder shades of color
 0.21     20220810  G.Tani      Modified color temperature scaling

(C) Copyright 2010 Giorgio Tani giorgio.tani.software@gmail.com
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

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, Forms, ExtCtrls, Graphics {$IFDEF MSWINDOWS}, Shellapi, Activex{$ENDIF};

type
   TFoundList = array of ansistring; //dynamic array of ansistring, each time a new file is found new space is allocated (function rList)
   TFoundListBool = array of boolean;
   TFoundListAges = array of dword;
   TFoundListAttrib = array of dword;
   TFoundListSizes = array of qword;
   TFoundListArray64 = array of array [0..63] of byte;
   TFileOfByte = File of Byte;

procedure setsize_bitmap(var abitmap:tbitmap; isize, deco:integer);
procedure loadlargeicon(srcbitmap:TBitmap; var destbitmap:Tbitmap; destsize:integer);
function load_bitmap(var abitmap:Tbitmap; s:ansistring; isize, deco:integer; var imginfo:ansistring):integer;
function load_imagefiletopicture(var apicture:Tpicture; s:ansistring):integer;
function rotate_picture(var apicture:Tpicture; rfun:ansistring): integer;
function resize_picture(var apicture:Tpicture; wsize,hsize:integer): integer;
function resize_bitmap(var abitmap:Tbitmap; wsize,hsize:integer): integer;
procedure get_pformscaling(var refsize, qscale,qscaleimages:integer);
function setpbitmap(var abitmap:TBitmap; virtualsize:integer):integer;
function resize_bitmap_tobitmap(var srcbitmap,destbitmap:Tbitmap; wsize,hsize:integer): integer;
function setpbitmap_tobitmap(var srcbitmap,destbitmap:TBitmap; virtualsize:integer):integer;
function getthemedbitmap(var abitmap:TBitmap; imgname:ansistring):integer;
function crop_picture(var apicture:Tpicture; ctop,cbottom,cleft,cright:integer): integer;
function save_picturetoimagefile(var apicture:Tpicture; s:ansistring):integer;
function saveconvert_picturetoimagefile(var apicture:Tpicture; s,convext:ansistring; convopt:integer):integer;
//GUI theming function: create a darker shade of specified color
function modcolor(col:TColor; dr,dg,db:Single):TColor; //change each channel multiplying it to an independent modification parameter, until saturation of the channel (255)
function modpropcolor(col:TColor; prop,temperature:integer):TColor; //change all channels of a color porportionally, creating lighter or darker, colder or warmer shades of same color
function evalcolor(col:TColor):byte;//evaluate darkness of a color combining rgb channels 0 darkest 255 lightest

const
DECO_NONE = 0; //no decoration, use all available space for rendering the image
DECO_SHADOW = 1; //light frame around the image, plus shadow
DECO_FRAME = 2; //light frame 6px away from the image
DECO_BORDER = 3; //light frame around the full icon
DECO_SPACE = 4; //6 px empty border

var
relwindowcolor: TColor;

implementation

procedure autoscale_image(aform:Tform; var aimage:Timage; var ascale,iscale:double);
var
  iwidth,iheight : Integer;
  rect : TRect;
begin
with aform do
   begin
   iwidth:=aimage.Picture.Bitmap.Width;
   iheight:=aimage.Picture.Bitmap.Height;
   iscale:=iscale*ascale;
   rect:=aimage.BoundsRect;
   rect.Right:=rect.Left+Round(iwidth*iscale);
   rect.Bottom:=rect.Top+Round(iheight*iscale);
   aimage.BoundsRect:=rect;
   aimage.Stretch:=True;
   aimage.left:=(aform.width-aimage.width)div 2;
   aimage.top:=(aform.height-aimage.height)div 2;
   end;
end;

procedure autosize_image(aform:tform; var aimage:timage; var iscale:double);
var
   wscale,hscale,ascale:double;
begin
with aform do
   begin
   iscale:=1.0;
   if aimage.Picture.Bitmap.Width<>0 then wscale:=aform.Width / aimage.Picture.Bitmap.Width else wscale:=100;
   if aimage.Picture.Bitmap.Height<>0 then hscale:=aform.Height / aimage.Picture.Bitmap.Height else hscale:=100;
   if wscale<hscale then ascale:=wscale
   else ascale:=hscale;
   autoscale_image(aform, aimage, ascale, iscale);
   end;
end;

function load_image_auto(aform:Tform; var aimage:Timage; s:ansistring; var iscale:double):integer;
begin
load_image_auto:=-1;
Try
//iscale:=1.0;
aimage.Picture.LoadFromFile(s);
aform.Caption:=s;
//image_loaded:=1;
autosize_image(aform, aimage, iscale);
load_image_auto:=0;
Except
load_image_auto:=1;
end
end;

procedure scale_bitmap(var abitmap:tbitmap; isize, deco:integer; var ascale:double);
//scale image (keeping in account border size), place the bitmap centered in a square of given size
var
  iwidth,iheight : Integer;
  rect : TRect;
  bbitmap:tbitmap;
begin
   bbitmap:=tbitmap.Create;
   bbitmap.width:=isize;
   bbitmap.height:=isize;
   bbitmap.Transparent:=false;
   bbitmap.canvas.Brush.Color:=relwindowcolor;
   if deco=DECO_BORDER then bbitmap.canvas.Pen.Color:=clbtnface
   else bbitmap.canvas.Pen.Color:=relwindowcolor;
   bbitmap.canvas.Rectangle(0,0,isize,isize);

   iwidth:=abitmap.Width;
   iheight:=abitmap.Height;
   rect.Left := (isize-round(iwidth*ascale)) div 2;
   rect.Top := (isize-round(iheight*ascale)) div 2;
   rect.Right:=rect.Left+Round(iwidth*ascale);
   rect.Bottom:=rect.Top+Round(iheight*ascale);

   if deco=DECO_SHADOW then
   begin
   bbitmap.canvas.Brush.Color:=clnone;
   bbitmap.canvas.Pen.Color:=clbtnface;
   bbitmap.canvas.Rectangle(rect.Left-1,rect.Top-1,rect.Right+1,rect.Bottom+1);
   bbitmap.canvas.Brush.Color:=$00c0c0c0;
   bbitmap.canvas.Pen.Color:=$00e0e0e0;
   bbitmap.canvas.Rectangle(rect.Left+1,rect.Top+1,rect.Right+2,rect.Bottom+2);
   end;

   if deco=DECO_FRAME then
   begin
   bbitmap.canvas.Brush.Color:=clnone;
   bbitmap.canvas.Pen.Color:=clbtnface;
   bbitmap.canvas.Rectangle(rect.Left-6,rect.Top-6,rect.Right+6,rect.Bottom+6);
   end;

   bbitmap.Canvas.StretchDraw(rect, abitmap);

   abitmap.Assign(bbitmap);
   bbitmap.free;
end;

procedure setsize_bitmap(var abitmap:tbitmap; isize, deco:integer);
var
   wscale,hscale,ascale:double;
   csize:integer;
begin
   case deco of
       DECO_FRAME: csize:=isize-12;
       DECO_BORDER: csize:=isize-12;
       DECO_SHADOW: csize:=isize-4;
   else csize:=isize; //let room for borders
   end;
   if abitmap.Width<>0 then wscale:=csize / abitmap.Width else wscale:=100;
   if abitmap.Height<>0 then hscale:=csize / abitmap.Height else hscale:=100;
   if wscale<hscale then ascale:=wscale
   else ascale:=hscale;
   if ascale>1 then ascale:=1;
   scale_bitmap(abitmap, isize, deco, ascale)
end;

procedure getimageinfo(aimage:TImage; var imginfo:ansistring);
begin
try imginfo:=inttostr(aimage.Picture.Bitmap.Width)+'*'+inttostr(aimage.Picture.Bitmap.height)+'@';
except imginfo:=''; end;
case aimage.Picture.Bitmap.PixelFormat of
    pfDevice: imginfo:=imginfo+'Device';
    pf1bit: imginfo:=imginfo+'1';
    pf4bit: imginfo:=imginfo+'4';
    pf8bit: imginfo:=imginfo+'8';
    pf15bit: imginfo:=imginfo+'15';
    pf16bit: imginfo:=imginfo+'16';
    pf24bit: imginfo:=imginfo+'24';
    pf32bit: imginfo:=imginfo+'32';
    pfCustom: imginfo:=imginfo+'Custom';
end;
end;

procedure loadlargeicon(srcbitmap:TBitmap; var destbitmap:Tbitmap; destsize:integer);
begin
destbitmap.Assign(srcbitmap);
setsize_bitmap(destbitmap,destsize,DECO_NONE);
destbitmap.TransparentColor:=$00FFFFFF;
destbitmap.Transparent:=true;
end;

function load_bitmap(var abitmap:Tbitmap; s:ansistring; isize, deco:integer; var imginfo:ansistring):integer;
var
   aimage:TImage;
begin
load_bitmap:=-1;
Try
aimage:=TImage.Create(nil);
aimage.Parent:=nil;
aimage.Picture.LoadFromFile(s);
getimageinfo(aimage, imginfo);
abitmap.assign(aimage.Picture.Bitmap);
setsize_bitmap(abitmap, isize, deco);
aimage.free;
load_bitmap:=0;
Except
load_bitmap:=1;
end
end;

function load_imagefiletopicture(var apicture:Tpicture; s:ansistring):integer;
begin
load_imagefiletopicture:=-1;
Try
apicture:=Tpicture.Create;
apicture.LoadFromFile(s);
load_imagefiletopicture:=0;
Except
load_imagefiletopicture:=1;
end
end;

function rotate_picture(var apicture:Tpicture; rfun:ansistring): integer;
var
   x,y: Integer;
   rlh,rlw: Integer;
   bpicture:Tpicture;
begin
result:=-1;
rlw:=apicture.Width;
rlh:=apicture.Height;
bpicture:=Tpicture.Create;

case rfun of
   'right':
   with bpicture do
   begin
   bitmap.Width:=rlh;
   bitmap.Height:=rlw;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlh-y-1,x]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'left':
   with bpicture do
   begin
   bitmap.Width:=rlh;
   bitmap.Height:=rlw;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[y,rlw-x-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   '180':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlw-x-1,rlh-y-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'flip':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[x,rlh-y-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'mirror':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlw-x-1,y]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;
   end;
apicture.assign(bpicture);
bpicture.Free;
result:=1;
end;

function resize_picture(var apicture:Tpicture; wsize,hsize:integer): integer;
var
   rect : TRect;
   bpicture:Tpicture;
begin
result:=-1;
bpicture:=tpicture.Create;
bpicture.bitmap.width:=wsize;
bpicture.bitmap.height:=hsize;
rect.Left := 0;
rect.Top := 0;
rect.Right:=wsize;
rect.Bottom:=hsize;
bpicture.bitmap.Canvas.StretchDraw(rect, apicture.bitmap);
apicture.assign(bpicture);
bpicture.Free;
result:=1;
end;

function resize_bitmap(var abitmap:Tbitmap; wsize,hsize:integer): integer; //resize transparent bitmap, works with 32bit bitmaps
var
  rect : TRect;
  bbitmap:tbitmap;
begin
result:=-1;
bbitmap:=tbitmap.Create;
{$IFDEF MSWINDOWS}
bbitmap.PixelFormat:=abitmap.PixelFormat;
{$ELSE}
{$IFNDEF LCLGTK2}
bbitmap.PixelFormat:=abitmap.PixelFormat;
{$ENDIF}
{$ENDIF}
bbitmap.width:=wsize;
bbitmap.height:=hsize;
{$IFDEF MSWINDOWS}
bbitmap.Transparent:=true;
bbitmap.TransparentColor:=relwindowcolor;
bbitmap.canvas.Brush.Color:=relwindowcolor;
bbitmap.canvas.Pen.Color:=relwindowcolor;
bbitmap.canvas.Rectangle(0,0,wsize,hsize);
{$ELSE}
{$IFDEF LCLGTK2}
bbitmap.Transparent:=true;
bbitmap.TransparentColor:=relwindowcolor;
bbitmap.canvas.Brush.Color:=relwindowcolor;
bbitmap.canvas.Pen.Color:=relwindowcolor;
bbitmap.canvas.Rectangle(0,0,wsize,hsize);
{$ENDIF}
{$ENDIF}
rect.Left := 0;
rect.Top := 0;
rect.Right:=wsize;
rect.Bottom:=hsize;
bbitmap.Canvas.StretchDraw(rect, abitmap);
abitmap.Assign(bbitmap);
bbitmap.free;
result:=0;
end;

procedure get_pformscaling(var refsize, qscale, qscaleimages:integer);
begin
   if refsize<3 then refsize:=25;
   qscale:=(100000*refsize) div 25000;
   if qscale<110 then qscale:=100 //small icons = 16px
   else
      if qscale<135 then qscale:=125 //20
      else
         if qscale<165 then qscale:=150 //24
         else
            if qscale<220 then qscale:=200 //32
            else
               if qscale<270 then qscale:=250 //40
               else
                  if qscale<330 then qscale:=300 //48
                  else
                     if qscale<440 then qscale:=400 //64
                     else
                        if qscale<550 then qscale:=500 //80
                        else
                           if qscale<660 then qscale:=600 //96
                           else
                              if qscale<880 then qscale:=800 //128
                              else qscale:=1000; //160
qscaleimages:=qscale;
case qscaleimages of //avoid some multiples that usually does not scale well, falling back to alternative similar scaling factor
   125: qscaleimages:=115;
   250: qscaleimages:=200;
end;
end;

function setpbitmap(var abitmap:TBitmap; virtualsize:integer):integer;//wrapper for resize_bitmap for square icons
begin
result:=resize_bitmap(abitmap, virtualsize, virtualsize);
end;

function resize_bitmap_tobitmap(var srcbitmap,destbitmap:Tbitmap; wsize,hsize:integer): integer; //copy transparent srcbitmap to destbitmap of new size, works with 32bit bitmaps
var
   rect : TRect;
begin
result:=-1;
destbitmap.Clear;
destbitmap.PixelFormat:=srcbitmap.PixelFormat;
destbitmap.width:=wsize;
destbitmap.height:=hsize;
destbitmap.Transparent:=true;
{destbitmap.Transparent:=false;
destbitmap.canvas.Brush.Color:=relwindowcolor;
destbitmap.canvas.Pen.Color:=relwindowcolor;
destbitmap.canvas.Rectangle(0,0,wsize,hsize); }
rect.Left:=0;
rect.Top:=0;
rect.Right:=wsize;
rect.Bottom:=hsize;
destbitmap.Canvas.StretchDraw(rect, srcbitmap);
result:=1;
end;

function setpbitmap_tobitmap(var srcbitmap,destbitmap:TBitmap; virtualsize:integer):integer;//wrapper for resize_bitmap for square icons
begin
result:=resize_bitmap_tobitmap(srcbitmap, destbitmap, virtualsize, virtualsize);
end;

function getthemedbitmap(var abitmap:TBitmap; imgname:ansistring):integer;
var
   aimage:TImage;
begin
result:=-1;
aimage:=TImage.Create(nil); ;
aimage.Picture.LoadFromFile(imgname);
abitmap.Assign(aimage.Picture.Bitmap);
aimage.Free;
result:=1;
end;

function crop_picture(var apicture:Tpicture; ctop,cbottom,cleft,cright:integer): integer;
var
   x,y: Integer;
   rlh,rlw: Integer;
   bpicture:Tpicture;
begin
result:=-1;
rlw:=apicture.bitmap.Width;
rlh:=apicture.bitmap.Height;
bpicture:= Tpicture.Create;
with bpicture do
   begin
   bitmap.Width:=rlw-cleft-cright;
   bitmap.Height:=rlh-ctop-cbottom;
   for x:=0 to rlw-cleft-cright-1 do
      for y:=0 to rlh-ctop-cbottom-1 do
         bitmap.Canvas.Pixels[x,y]:=apicture.bitmap.Canvas.Pixels[x+cleft,y+ctop];
   end;
apicture.assign(bpicture);
bpicture.Free;
result:=1;
end;

function save_picturetoimagefile(var apicture:Tpicture; s:ansistring):integer;
begin
try
save_picturetoimagefile:=-1;
apicture.SaveToFile(s);
apicture.Free;
save_picturetoimagefile:=0;
except
save_picturetoimagefile:=1;
try apicture.Free; except end;
end;
end;

function saveconvert_picturetoimagefile(var apicture:Tpicture; s,convext:ansistring; convopt:integer):integer;
var
   ajpeg:TJpegImage;
   atiff:TTiffImage;
   apng:TPortableNetworkGraphic;
   aico:TIcon;
   appm:TPortableAnyMapGraphic;
   axpm:TPixmap;
begin
try
saveconvert_picturetoimagefile:=-1;
convext:=LowerCase(convext);
case convext of
'.jpg', '.jpeg', '.jpe', '.jif', '.jfif', '.jfi', 'jpeg', 'jpg':
   begin
   ajpeg:=TJPEGImage.Create;
   ajpeg.CompressionQuality:=convopt;
   ajpeg.Assign(apicture.Bitmap);
   ajpeg.SaveToFile(s+'.'+convext);
   ajpeg.free;
   end;
'tiff':
   begin
   atiff:=TTiffImage.Create;
   atiff.Assign(apicture.Bitmap);
   atiff.SaveToFile(s+'.'+convext);
   atiff.free;
   end;
'png':
   begin
   apng:=TPortableNetworkGraphic.Create;
   apng.Assign(apicture.Bitmap);
   apng.SaveToFile(s+'.'+convext);
   apng.free;
   end;
'ico':
   begin
   aico:=TIcon.Create;
   aico.Assign(apicture.Bitmap);
   aico.SaveToFile(s+'.'+convext);
   aico.free;
   end;
'ppm':
   begin
   appm:=TPortableAnyMapGraphic.Create;
   appm.Assign(apicture.Bitmap);
   appm.SaveToFile(s+'.'+convext);
   appm.free;
   end;
'xpm':
   begin
   axpm:=TPixmap.Create;
   axpm.Assign(apicture.Bitmap);
   axpm.SaveToFile(s+'.'+convext);
   axpm.free;
   end;
else apicture.SaveToFile(s+'.'+convext);
end;
apicture.Free;
saveconvert_picturetoimagefile:=0;
except
saveconvert_picturetoimagefile:=1;
try apicture.Free; except end;
end;
end;

function modcolor(col:TColor; dr,dg,db:Single):TColor;
var
  r, g, b: Byte;
  rr,gg,bb:integer;
begin
  col := ColorToRGB(col);
  r := BYTE(col);
  g := BYTE((WORD(col)) shr 8);
  b := BYTE(col shr 16);
  rr := trunc( r * dr); if rr>255 then rr:=255;
  gg := trunc( g * dg); if gg>255 then gg:=255;
  bb := trunc( b * db); if bb>255 then bb:=255;
  r:=rr;
  g:=gg;
  b:=bb;
  result := DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
end;

function modpropcolor(col:TColor; prop,temperature:integer):TColor;
var
  r, g, b: Byte;
  rint,gint,bint:integer;
  rr,gg,bb,rprop,gprop,bprop:integer;
begin
  col := ColorToRGB(col);
  r := BYTE(col);
  g := BYTE((WORD(col)) shr 8);
  b := BYTE(col shr 16);
  if prop<-255 then prop:=-255;
  if prop>255 then prop:=255;
  rprop:=prop+temperature*2;
  gprop:=prop;
  bprop:=prop;//-temperature;
  if rprop<-255 then rprop:=-255;
  if rprop>255 then rprop:=255;
  if bprop<-255 then bprop:=-255;
  if bprop>255 then bprop:=255;
  if prop>=0 then
     begin
     rr := (255-r) * rprop div 255;
     gg := (255-g) * gprop div 255;
     bb := (255-b) * bprop div 255;
     end
  else
     begin
     rr := (r) * rprop div 255;
     gg := (g) * gprop div 255;
     bb := (b) * bprop div 255;
     end;
  rint:=r+rr;
  gint:=g+gg;
  bint:=b+bb;
  if rint<0 then rint:=0;
  if rint>255 then rint:=255;
  if gint<0 then gint:=0;
  if gint>255 then gint:=255;
  if bint<0 then bint:=0;
  if bint>255 then bint:=255;
  r:=rint;
  g:=gint;
  b:=bint;
  result := DWORD(((DWORD(BYTE(r))) or ((DWORD(WORD(g))) shl 8)) or ((DWORD(BYTE(b))) shl 16));
end;

function evalcolor(col:TColor):byte;
var
  r, g, b: Byte;
  cbtot: integer;
begin
  col := ColorToRGB(col);
  r := BYTE(col);
  g := BYTE((WORD(col)) shr 8);
  b := BYTE(col shr 16);
  cbtot:=(r+g+b) div 3;
  result := cbtot;
end;

end.

