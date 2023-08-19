unit Unit14;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  list_utils;

type

  { TFormComment }

  TFormComment = class(TForm)
    Button1: TButton;
    LabelSavetofile: TLabel;
    LabelReset: TLabel;
    LabelLoadfromfile: TLabel;
    Labelspac: TLabel;
    Labelspac1: TLabel;
    MemoComment: TMemo;
    OpenDialogFC: TOpenDialog;
    SaveDialogFC: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure LabelLoadfromfileClick(Sender: TObject);
    procedure LabelResetClick(Sender: TObject);
    procedure LabelSavetofileClick(Sender: TObject);
  private

  public

  end;

var
  FormComment: TFormComment;
  incomment,pdesk:ansistring;

implementation

{ TFormComment }

procedure TFormComment.Button1Click(Sender: TObject);
begin
ModalResult:=mrOK;
end;

procedure commentfromfile(commentfile:ansistring);
var
   dummy,commstr: AnsiString;
   fcomment: text;
   fsize:qword;
begin
if commentfile='' then
   begin
   FormComment.OpenDialogFC.InitialDir:=pdesk;
   if FormComment.OpenDialogFC.Execute then
      if FormComment.OpenDialogFC.FileName<>'' then commentfile:=FormComment.OpenDialogFC.FileName;
   end;
if commentfile<>'' then
   begin
   try
   srcfilesize(commentfile,fsize);
   if fsize=0 then
      begin
      FormComment.MemoComment.Caption:='';
      exit;
      end;
   commstr:='';
   assignfile(fcomment,commentfile);
   filemode:=0;
   reset(fcomment);
   repeat
      readln(fcomment,dummy);
      commstr:=commstr+dummy+char($0D)+char($0A);
   until eof(fcomment);
   closefile(fcomment);
   if length(commstr)>2 then SetLength(commstr,length(commstr)-2);
   if length(commstr)>64*1024-16 then SetLength(commstr,64*1024-16);//full 64K does not work with Rar.exe backend
   FormComment.MemoComment.Caption:=commstr;
   except
   try closefile(fcomment); except end;
   end;
   end;
end;

procedure TFormComment.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  commentfromfile(filenames[0]);
end;

procedure TFormComment.LabelLoadfromfileClick(Sender: TObject);
begin
  commentfromfile('');
end;

procedure TFormComment.LabelResetClick(Sender: TObject);
begin
MemoComment.Text:=incomment;
end;

procedure TFormComment.LabelSavetofileClick(Sender: TObject);
var
   t: text;
   commstr: AnsiString;
begin
SaveDialogFC.DefaultExt:='.txt';
SaveDialogFC.InitialDir:=pdesk;
if SaveDialogFC.Execute then
   if SaveDialogFC.FileName<>'' then
      begin
      try
      commstr:=MemoComment.Caption;
      if length(commstr)>64*1024-16 then SetLength(commstr,64*1024-16);//full 64K does not work with Rar.exe backend
      assignfile(t,SaveDialogFC.FileName );
      rewrite(t);
      write(t,commstr);
      closefile(t);
      except
      try closefile(t); except end;
      end;
      end;
end;

initialization
  {$I unit14.lrs}

end.

