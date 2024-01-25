unit uImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

function FindImportLibrary(hModule: THandle; pLibName: PAnsiChar): PPointer;
function FindImportFunction(pLibrary: PPointer; pFunction: Pointer): PPointer;
function ReplaceImportFunction(pOldFunction: PPointer; pNewFunction: Pointer): Pointer;

function FindDelayImportLibrary(hModule: THandle; pLibName: PAnsiChar): Pointer;
function FindDelayImportFunction(hModule: THandle; pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR; pFuncName: PAnsiChar): PPointer;
procedure ReplaceDelayImportFunction(hModule: THandle; pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR; pFuncName: PAnsiChar; pNewFunction: Pointer);

implementation

type
{$IFDEF WIN64}
  PIMAGE_NT_HEADERS = PIMAGE_NT_HEADERS64;
{$ELSE}
  PIMAGE_NT_HEADERS = PIMAGE_NT_HEADERS32;
{$ENDIF}

function FindImageDirectory(hModule: THandle; Index: Integer; out DataDir: PIMAGE_DATA_DIRECTORY): Pointer;
var
  pNTHeaders: PIMAGE_NT_HEADERS;
  pModule: PByte absolute hModule;
  pDosHeader: PIMAGE_DOS_HEADER absolute hModule;
begin
  if pDosHeader^.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    pNTHeaders := @pModule[pDosHeader^.e_lfanew];
    if pNTHeaders^.Signature = IMAGE_NT_SIGNATURE then
    begin
      DataDir := @pNTHeaders^.OptionalHeader.DataDirectory[Index];
      Result := @pModule[DataDir^.VirtualAddress];
      Exit;
    end;
  end;
  Result := nil;
end;

function FindImportLibrary(hModule: THandle; pLibName: PAnsiChar): PPointer;
var
  pEnd: PByte;
  pImpDir: PIMAGE_DATA_DIRECTORY;
  pImpDesc: PIMAGE_IMPORT_DESCRIPTOR;
  pModule: PAnsiChar absolute hModule;
begin
  pImpDesc := FindImageDirectory(hModule, IMAGE_DIRECTORY_ENTRY_IMPORT, pImpDir);
  if pImpDesc = nil then Exit(nil);

  pEnd := PByte(pImpDesc) + pImpDir^.Size;

  while (PByte(pImpDesc) < pEnd) and (pImpDesc^.FirstThunk <> 0) do
  begin
    if StrIComp(@pModule[pImpDesc^.Name], pLibName) = 0 then
    begin
      Result := @pModule[pImpDesc^.FirstThunk];
      Exit;
    end;
    Inc(pImpDesc);
  end;
  Result := nil;
end;

function FindImportFunction(pLibrary: PPointer; pFunction: Pointer): PPointer;
begin
  while Assigned(pLibrary^) do
  begin
    if pLibrary^ = pFunction then Exit(pLibrary);
    Inc(pLibrary);
  end;
  Result := nil;
end;

function ReplaceImportFunction(pOldFunction: PPointer; pNewFunction: Pointer): Pointer;
var
  dwOldProtect: DWORD = 0;
begin
  if VirtualProtect(pOldFunction, SizeOf(Pointer), PAGE_READWRITE, dwOldProtect) then
  begin
    Result := pOldFunction^;
    pOldFunction^ := pNewFunction;
    VirtualProtect(pOldFunction, SizeOf(Pointer), dwOldProtect, dwOldProtect);
  end;
end;

function FindDelayImportLibrary(hModule: THandle; pLibName: PAnsiChar): Pointer;
var
  pEnd: PByte;
  pImpDir: PIMAGE_DATA_DIRECTORY;
  pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR;
  pModule: PAnsiChar absolute hModule;
begin
  pImpDesc := FindImageDirectory(hModule, IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT, pImpDir);
  if pImpDesc = nil then Exit(nil);

  pEnd := PByte(pImpDesc) + pImpDir^.Size;

  while (PByte(pImpDesc) < pEnd) and (pImpDesc^.DllNameRVA > 0) do
  begin
    if StrIComp(@pModule[pImpDesc^.DllNameRVA], pLibName) = 0 then
      Exit(pImpDesc);

    Inc(pImpDesc);
  end;
  Result := nil;
end;

function FindDelayImportFunction(hModule: THandle;
  pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR; pFuncName: PAnsiChar): PPointer;
var
  pImpName: PIMAGE_IMPORT_BY_NAME;
  pImgThunkName: PIMAGE_THUNK_DATA;
  pImgThunkAddr: PIMAGE_THUNK_DATA;
  pModule: PAnsiChar absolute hModule;
begin
  pImgThunkName:= @pModule[pImpDesc^.ImportNameTableRVA];
  pImgThunkAddr:= @pModule[pImpDesc^.ImportAddressTableRVA];

  while (pImgThunkName^.u1.Ordinal <> 0) do
  begin
    if not (IMAGE_SNAP_BY_ORDINAL(pImgThunkName^.u1.Ordinal)) then
    begin
      pImpName:= @pModule[pImgThunkName^.u1.AddressOfData];
      if (StrIComp(pImpName^.Name, pFuncName) = 0) then
        Exit(PPointer(@pImgThunkAddr^.u1._Function));
    end;
    Inc(pImgThunkName);
    Inc(pImgThunkAddr);
  end;
  Result:= nil;
end;

procedure ReplaceDelayImportFunction(hModule: THandle;
  pImpDesc: PIMAGE_DELAYLOAD_DESCRIPTOR; pFuncName: PAnsiChar;
  pNewFunction: Pointer);
var
  pOldFunction: PPointer;
begin
  pOldFunction:= FindDelayImportFunction(hModule, pImpDesc, pFuncName);
  if Assigned(pOldFunction) then ReplaceImportFunction(pOldFunction, pNewFunction);
end;

end.

