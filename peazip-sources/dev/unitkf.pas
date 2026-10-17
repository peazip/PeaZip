unit UnitKF; //Form to create keyfile and random password, sampling enthropy from system and user input

{$mode objfpc}{$H+}{$R+}

interface

uses
  UnitDlg, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls,
  hash, sha512, whirl512, sha1, sha256, mem_util,
  pea_utils, Buttons, ButtonPanel, ActnList;

type

  { TFormKF }

  TFormKF = class(TForm)
    ActionKFExit: TAction;
    ActionListKF: TActionList;
    ButtonKF: TButton;
    ButtonKFLoadFile: TButton;
    ButtonPanelKF: TButtonPanel;
    ButtonSuggestPW: TButton;
    ButtonPWfromhash: TButton;
    CheckBoxSuggestPW: TCheckBox;
    EditEnt: TEdit;
    EditSuggestPW: TEdit;
    EditPWfromhash: TEdit;
    GroupBoxKF: TGroupBox;
    ImageInfoKF: TImage;
    OpenDialogKF: TOpenDialog;
    PanelKeyfile: TPanel;
    ProgressBarKF: TProgressBar;
    SaveDialogKF: TSaveDialog;
    ShapeKF1: TShape;
    ShapeKF2: TShape;
    SpinEditSuggestPW: TSpinEdit;
    procedure ActionKFExitExecute(Sender: TObject);
    procedure ButtonKFClick(Sender: TObject);
    procedure ButtonKFLoadFileClick(Sender: TObject);
    procedure ButtonPWfromhashClick(Sender: TObject);
    procedure ButtonSuggestPWClick(Sender: TObject);
    procedure EditEntKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure GroupBoxKFMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageInfoKFClick(Sender: TObject);
    procedure PanelKeyfileMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Shape3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormKF: TFormKF;
  persistent_source,txt_keyfile_notcreated,txt_error_openfile,st:ansistring;
  dkflimit:integer;
  SHA512Context: THashContext;
  SHA512Digest: TSHA512Digest;
  WhirlContext_File: THashContext;
  WhirlDigest_File: TWhirlDigest;
  SHA512ContextSample:THashContext;
  SHA512DigestSample: TSHA512Digest;
  WhirlContextSample: THashContext;
  WhirlDigestSample: TWhirlDigest;
  mentd,kentd,fentd: TWhirlDigest;
  fingerprint:TSHA512Digest;
  d_sample: TSHA256Digest;
  ment,kent,fent,sample: THashContext;

implementation

procedure read_rand(var arr: array of byte);
var
   randf: file of byte;
begin
try
//read current rand seed file
assignfile(randf,(persistent_source));
filemode:=0;
reset(randf);
blockread(randf,arr,256);
closefile(randf);
except
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
   i: integer;
begin
try
//read current rand seed file
assignfile(randf,(persistent_source));
filemode:=0;
reset(randf);
blockread(randf,randarr,256);
closefile(randf);
//left shift by one byte the rand seed (in randarr)
for i:=0 to 254 do arr[i]:=randarr[i+1];
arr[255]:=randarr[0];
except
end;
end;

procedure gen_pw(keyarr:TKey2048; pwsize:integer; var pw:ansistring);
var
   i,j:integer;
   spchar:boolean;
begin
setlength(pw,pwsize);
for i:=0 to pwsize do pw[i]:=char(0);
if FormKF.CheckBoxSuggestPW.State=cbChecked then spchar:=false else spchar:=true;
i:=1;
j:=0;
repeat
   keyarr[j]:=keyarr[j] xor random(256);
   if spchar=false then //strictly char, upcase char and number as required by some services
      if ((keyarr[j]>47) and (keyarr[j]<58)) or ((keyarr[j]>64) and (keyarr[j]<91)) or ((keyarr[j]>96) and (keyarr[j]<123)) then
         begin
         pw[i]:=char(keyarr[j]);
         i:=i+1;
         end
      else begin end
   else //allow symbols and special characters
      if (keyarr[j]>31) and (keyarr[j]<127) and (keyarr[j]<>34) and (keyarr[j]<>39) then
         begin
         pw[i]:=char(keyarr[j]);
         i:=i+1;
         end;
   j:=j+1;
until (i=pwsize+1) or (j=256);
while i<pwsize+1 do
   begin
   j:=random(128);
   if spchar=false then
      if ((j>47) and (j<58)) or ((j>64) and (j<91)) or ((j>96) and (j<123)) then
         begin
         pw[i]:=char(j);
         i:=i+1;
         end
      else begin end
   else
      if ((j>31) and (j<127)) then
         begin
         pw[i]:=char(j);
         i:=i+1;
         end;
   end;
end;

{ TFormKF }

procedure TFormKF.ButtonKFClick(Sender: TObject);
//create keyfile, recreate the system's fingerprint the entropy bar is zeroed since less entropy is introduced than the first time the system fingerprint was taken (at application startup, when entropy bar was set to 16)
var
   keyf: file of byte;
   keyarr: TKey2048;
   keyf_name:ansistring;
   numwritten:integer;
begin
if SaveDialogKF.execute then
   begin
   if SaveDialogKF.Filename<>'' then keyf_name:=SaveDialogKF.Filename
   else keyf_name:='KeyFile';
   assignfile(keyf,keyf_name);
   rewrite(keyf);
   end
else
   begin
   pMessageWarningOK(txt_keyfile_notcreated);
   exit;
   end;
read_rand(keyarr);
gen_rand(keyarr);
blockwrite(keyf,keyarr,256,numwritten); //write 256 byte (2048 bit) keyarr to file
closefile(keyf);
get_fingerprint (fingerprint,false);
ProgressBarKF.Position:=0;
end;

procedure TFormKF.ActionKFExitExecute(Sender: TObject);
begin
ModalResult:=mrCancel;
end;

procedure TFormKF.ButtonKFLoadFileClick(Sender: TObject);
var
   fse,i:integer;
   f:file of byte;
begin //conservative entropy evaluation: 2 bit for timing (file selection takes longer than mouse or keyboard operations) 1 bit for filename character file content enthropy is quite hazardous to evaluate, 1 bit is assigned for each byte of size up to 512 bit (digest size).
try
if OpenDialogKF.Execute then
   if OpenDialogKF.Filename<>'' then
      begin
      sample_file_ent(fent,OpenDialogKF.Filename);
      sample:=fent;
      SHA256Final(sample,d_sample);
      st:='';
      for i:=0 to 3 do st:=st+hexstr(@d_sample[i],1);
      assignfile(f,OpenDialogKF.Filename);
      filemode:=0;
      reset(f);
      if system.filesize(f)>512 then fse:=512
      else fse:=system.filesize(f);
      closefile(f);
      ProgressBarKF.Position:=ProgressBarKF.Position+2+length(OpenDialogKF.Filename)+fse;
      end;
except
pMessageErrorOK(txt_error_openfile);
end;
end;

procedure TFormKF.ButtonPWfromhashClick(Sender: TObject);
var
  hpw:ansistring;
begin //load password from hash
try
if OpenDialogKF.Execute then
   if OpenDialogKF.Filename<>'' then
      begin
      hpw:='';
      prepend_keyfile(hpw,OpenDialogKF.Filename,dkflimit);
      EditPWfromhash.Caption:=hpw;
      end;
except
pMessageErrorOK(txt_error_openfile);
end;

end;

procedure TFormKF.ButtonSuggestPWClick(Sender: TObject);
var
   keyarr: TKey2048;
   pw:ansistring;
begin
read_rand(keyarr);
gen_rand(keyarr);
gen_pw(keyarr,SpinEditSuggestPW.Value,pw);
EditSuggestPW.Text:=pw;
get_fingerprint (fingerprint,false);
ProgressBarKF.Position:=0;
end;

procedure TFormKF.EditEntKeyPress(Sender: TObject; var Key: char);
//conservative entropy evaluation: 1 for the character +1 bit for the keypress delta time (memory sampling is done only under Windows so is no taken in account for this conservative exteem)
var
   i:integer;
begin
sample_keyb_ent(kent,ord(Key));
sample:=kent;
SHA256Final(sample,d_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@d_sample[i],1);
ProgressBarKF.Position:=ProgressBarKF.Position+2;
end;

procedure TFormKF.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   randf: file of byte;
   randarr: TKey2048;
begin
try
   shl_rand(randarr); //read and leftshift of 1 byte data from persistent random seed file
   gen_rand(randarr); //create keyfile starting from data of previous seed file shifted of 1 byte
   assignfile(randf,persistent_source); //write keyfile as new seed file
   rewrite(randf);
   blockwrite(randf,randarr,256);
   closefile(randf);
except
end;
end;

procedure TFormKF.GroupBoxKFMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   i:integer;
begin //conservative entropy evaluation: 3 bit per movement, 1 for each of the two axis + 1 for delta timer
sample_mouse_ent(ment,x,y);
sample:=ment;
SHA256Final(sample,d_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@d_sample[i],1);
ProgressBarKF.Position:=ProgressBarKF.Position+3;
end;

procedure TFormKF.ImageInfoKFClick(Sender: TObject);
begin
pMessageInfoOK(ImageInfoKF.Hint);
end;

procedure TFormKF.PanelKeyfileMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//conservative entropy evaluation: 3 bit per movement, 1 for each of the two axis + 1 for delta timer
var
   i:integer;
begin
sample_mouse_ent(ment,x,y);
sample:=ment;
SHA256Final(sample,d_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@d_sample[i],1);
ProgressBarKF.Position:=ProgressBarKF.Position+3;
end;

procedure TFormKF.Shape3MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//conservative entropy evaluation: 3 bit per movement, 1 for each of the two axis + 1 for delta timer
var
   i:integer;
begin
sample_mouse_ent(ment,x,y);
sample:=ment;
SHA256Final(sample,d_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@d_sample[i],1);
ProgressBarKF.Position:=ProgressBarKF.Position+3;
end;

initialization
  {$I unitkf.lrs}

end.

