program list_utils_smoke;

{$mode objfpc}{$H+}

uses
  SysUtils,
  filename_utils;

const
  PEAZIP_SMOKE_TEST = true;

procedure Check(const Condition: boolean; const MessageText: string);
begin
  if not Condition then
  begin
    Writeln(stderr, 'FAIL: ', MessageText);
    Halt(1);
  end;
end;

begin
  Check(PEAZIP_SMOKE_TEST, 'smoke test marker is enabled');
  Check(checkfilename('file.txt') = SUCCESS, 'simple filename is valid');
  Check(checkfilename('') <> SUCCESS, 'empty filename is invalid');
  Check(checkfilename('.') <> SUCCESS, 'single-dot filename is invalid');
  Check(checkfilename('..') <> SUCCESS, 'double-dot filename is invalid');
  Check(checkfilename('dir/file') <> SUCCESS, 'slash is invalid in filename');
  Check(checkfilename('dir\file') <> SUCCESS, 'backslash is invalid in filename');
  Check(checkfilename('bad:name') <> SUCCESS, 'colon is invalid in filename');
  Check(checkfilename('bad' + #1 + 'name') <> SUCCESS, 'control char is invalid in filename');
  Check(checkfilename_acceptblank('') = SUCCESS, 'blank filename is accepted by acceptblank helper');
  Check(stringdelim('abc') = '''abc''', 'simple string uses strong quotes on Unix-like systems');
  Check(stringundelim(stringdelim('a''b')) = 'a''b', 'double-quoted string delimiter round-trip works');
  {$IFDEF MSWINDOWS}
  Check(checkfilename('CON') <> SUCCESS, 'CON is reserved on Windows');
  Check(checkfilename('NUL.txt') <> SUCCESS, 'NUL is reserved on Windows');
  Check(checkfilename('COM1') <> SUCCESS, 'COM1 is reserved on Windows');
  {$ENDIF}
  Writeln('list_utils smoke tests passed');
end.
