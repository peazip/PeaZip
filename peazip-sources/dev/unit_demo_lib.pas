unit Unit_demo_lib;
{
 DESCRIPTION     :  Demo application using PEA, UnPEA and Raw File Split/Join
                    calling *_lib_procedure procedures with hardcoded parameters,
                    showing different operation modes

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060822  G.Tani      Initial version
 0.11     20080704  G.Tani      Removed dependency for WinXP package, recompiled with utf8 enabled IDE

(C) Copyright 2006 Giorgio Tani giorgiotani@interfree.it
official pea-peach site http://sourceforge.net/projects/pea-peach/
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

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  unit_pea, list_utils;

type

  { TForm_demo_lib }

  TForm_demo_lib = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form_demo_lib: TForm_demo_lib;
  
implementation

{ TForm_demo_lib }

procedure TForm_demo_lib.Button1Click(Sender: TObject); //archive sourcecode .pas files
var flist:TFoundList;
begin
SetLength(flist,8);
flist[0]:='list_utils.pas';
flist[1]:='pea_utils.pas';
flist[2]:='peach.pas';
flist[3]:='rfs_utils.pas';
flist[4]:='unit1.pas'; //does not exist, used to demonstrate skipping feature
flist[5]:='unit_demo_lib.pas';
flist[6]:='unit_pea.pas';
flist[7]:='unit_report.pas';
pea_lib_procedure('units',25000,'PCOMPRESS2','WHIRLPOOL','SHA256','EAX','this is the passphrase','NOKEYFILE',flist,'BATCH');
end;

procedure TForm_demo_lib.Button2Click(Sender: TObject); //restore sourcecode .pas files in units folder
begin
unpea_lib_procedure('units.000001.pea','AUTONAME','RESETDATE','SETATTR','EXTRACT2DIR','this is the passphrase','NOKEYFILE','HIDDEN_REPORT');
end;

procedure TForm_demo_lib.Button3Click(Sender: TObject); //split present unit in 1000 byte chunks
begin
rfs_lib_procedure ( 'test',1000,'SHA512','unit_demo_lib.pas','HIDDEN_REPORT');
end;

procedure TForm_demo_lib.Button4Click(Sender: TObject); //join chunks of present unit in a new, autonamed, file
begin
rfj_lib_procedure ( 'test.001','AUTONAME','BATCH_REPORT');
end;

initialization
  {$I unit_demo_lib.lrs}

end.

