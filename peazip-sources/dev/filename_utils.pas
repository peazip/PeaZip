unit filename_utils;

{
  UI-neutral filename and command delimiter helpers.
}

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  SUCCESS = 0;

procedure cutextension(var s: ansistring);
function cutext(var s: ansistring): ansistring;
function checkfiledirname(s: ansistring): integer;
function checkfiledirname_acceptblank(s: ansistring): integer;
function checkfilename(s: ansistring): integer;
function checkfilename_acceptblank(s: ansistring): integer;
function correctdelimiter(s: ansistring): ansistring;
function stringdelim(s: ansistring): ansistring;
function stringundelim(s: ansistring): ansistring;

implementation

procedure cutextension(var s: ansistring);
var
  sext: ansistring;
begin
  sext := ExtractFileExt(s);
  if sext = '' then exit;
  if pos(' ', sext) <> 0 then exit;
  if length(sext) > 6 then exit;
  setlength(s, length(s) - length(ExtractFileExt(s)));
end;

function cutext(var s: ansistring): ansistring;
var
  s1: ansistring;
begin
  s1 := s;
  cutextension(s1);
  result := s1;
end;

function checkfiledirname(s: ansistring): integer;
var
  sf: ansistring;
  i: integer;
begin
  result := -1;
  if s = '' then exit;
  for i := 0 to 31 do
    if pos(char(i), s) <> 0 then exit;
  if pos('*', s) <> 0 then exit;
  if pos('?', s) <> 0 then exit;
  if pos('<', s) <> 0 then exit;
  if pos('>', s) <> 0 then exit;
  if pos('|', s) <> 0 then exit;
  if pos('       ', s) <> 0 then exit;
  {$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then exit;
  {$ENDIF}
  sf := ExtractFileName(s);
  if pos('\', sf) <> 0 then exit;
  if pos('/', sf) <> 0 then exit;
  if pos(':', sf) <> 0 then exit;
  {$IFDEF MSWINDOWS}
  cutextension(sf);
  sf := UpCase(sf);
  if (sf = 'CON') or (sf = 'PRN') or (sf = 'AUX') or (sf = 'NUL') or
     (sf = 'COM1') or (sf = 'COM2') or (sf = 'COM3') or (sf = 'COM4') or
     (sf = 'COM5') or (sf = 'COM6') or (sf = 'COM7') or (sf = 'COM8') or
     (sf = 'COM9') or (sf = 'LPT1') or (sf = 'LPT2') or (sf = 'LPT3') or
     (sf = 'LPT4') or (sf = 'LPT5') or (sf = 'LPT6') or (sf = 'LPT7') or
     (sf = 'LPT8') or (sf = 'LPT9') then
    exit;
  {$ENDIF}
  result := SUCCESS;
end;

function checkfiledirname_acceptblank(s: ansistring): integer;
begin
  result := -1;
  if s = '' then result := SUCCESS
  else result := checkfiledirname(s);
end;

function checkfilename(s: ansistring): integer;
var
  {$IFDEF MSWINDOWS}
  s1: ansistring;
  {$ENDIF}
  i: integer;
begin
  result := -1;
  if (s = '') or (s = '.') or (s = '..') then exit;
  for i := 0 to 31 do
    if pos(char(i), s) <> 0 then exit;
  if pos('\', s) <> 0 then exit;
  if pos('/', s) <> 0 then exit;
  if pos(':', s) <> 0 then exit;
  if pos('*', s) <> 0 then exit;
  if pos('?', s) <> 0 then exit;
  if pos('<', s) <> 0 then exit;
  if pos('>', s) <> 0 then exit;
  if pos('|', s) <> 0 then exit;
  if pos('       ', s) <> 0 then exit;
  {$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then exit;
  s1 := ExtractFileName(s);
  cutextension(s1);
  s1 := UpCase(s1);
  if (s1 = 'CON') or (s1 = 'PRN') or (s1 = 'AUX') or (s1 = 'NUL') or
     (s1 = 'COM1') or (s1 = 'COM2') or (s1 = 'COM3') or (s1 = 'COM4') or
     (s1 = 'COM5') or (s1 = 'COM6') or (s1 = 'COM7') or (s1 = 'COM8') or
     (s1 = 'COM9') or (s1 = 'LPT1') or (s1 = 'LPT2') or (s1 = 'LPT3') or
     (s1 = 'LPT4') or (s1 = 'LPT5') or (s1 = 'LPT6') or (s1 = 'LPT7') or
     (s1 = 'LPT8') or (s1 = 'LPT9') then
    exit;
  {$ENDIF}
  result := SUCCESS;
end;

function checkfilename_acceptblank(s: ansistring): integer;
begin
  result := -1;
  if s = '' then result := SUCCESS
  else result := checkfilename(s);
end;

function correctdelimiter(s: ansistring): ansistring;
begin
  result := '''';
  {$IFDEF MSWINDOWS}
  result := '"';
  {$ELSE}
  if pos('''', s) <> 0 then result := '"';
  {$ENDIF}
end;

function stringdelim(s: ansistring): ansistring;
var
  cdelim: ansistring;
begin
  cdelim := correctdelimiter(s);
  result := cdelim + s + cdelim;
end;

function stringundelim(s: ansistring): ansistring;
var
  cdelim: ansistring;
  st: ansistring;
begin
  st := s;
  cdelim := correctdelimiter(st);
  if length(st) > 1 then if st[1] = cdelim then st := copy(st, 2, length(st) - 1);
  if length(st) > 1 then if st[length(st)] = cdelim then st := copy(st, 1, length(st) - 1);
  result := st;
end;

end.
