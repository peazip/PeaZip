{-Test prog for fio unit, we Apr. 2006}

program t_fio;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  fio;
var
  fn : string;
  ex : boolean;
  Err: word;
  fs : longint;
  f  : file;
begin;
  fn := paramstr(1);
  if (fn<>'') then begin
    ex := fio_FileExists(fn);
    writeln(fn, ' exists: ', ex);
    if ex then begin
      system.assign(f,fn);
      fio_Reset(f, 1, true, Err);
      if Err=0 then begin
        fs := fio_Filesize(f, Err);
        if Err=0 then writeln('Filesize: ', fs)
        else writeln('fio_Filesize error: ', Err);
      end
      else writeln('fio_reset error: ', Err);
      if Err=0 then begin
        fio_seek(f,100000, Err);
        writeln('fio_seek return code: ', Err);
        fs := fio_filepos(f,Err);
        writeln('Filepos: ',fs, '   Err=',Err);
      end;
    end;
  end;
end.
