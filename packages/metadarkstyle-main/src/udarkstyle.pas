{
  Double Commander
  -------------------------------------------------------------------------
  Dark mode support unit (Windows 10 + Qt5).

  Copyright (C) 2019-2021 Richard Yu
  Copyright (C) 2019-2022 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit uDarkStyle;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows;

var
  g_buildNumber: DWORD = 0;
  //g_darkModeEnabled: bool = false;
  g_darkModeSupported: bool = false;
  //gAppMode: integer = 1;

{$IF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
procedure ApplyDarkStyle;
{$ENDIF}

procedure RefreshTitleBarThemeColor(hWnd: HWND);
function AllowDarkModeForWindow(hWnd: HWND; allow: bool): bool;
procedure InitDarkMode;

implementation

uses
  UxTheme, JwaWinUser, FileInfo, uDarkStyleParams
  {$IF DEFINED(LCLQT5)}
  ,Qt5
  {$ENDIF}
  {$IF DEFINED(LCLQT6)}
  ,Qt6
  {$ENDIF}
  ;
  
var
  AppMode: TPreferredAppMode;

var
  RtlGetNtVersionNumbers: procedure(major, minor, build: LPDWORD); stdcall;
  DwmSetWindowAttribute: function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  // 1809 17763
  _ShouldAppsUseDarkMode: function(): bool; stdcall; // ordinal 132
  _AllowDarkModeForWindow: function(hWnd: HWND; allow: bool): bool; stdcall; // ordinal 133
  _AllowDarkModeForApp: function(allow: bool): bool; stdcall; // ordinal 135, removed since 18334
  _RefreshImmersiveColorPolicyState: procedure(); stdcall; // ordinal 104
  _IsDarkModeAllowedForWindow: function(hWnd: HWND): bool; stdcall; // ordinal 137
  // Insider 18334
  _SetPreferredAppMode: function(appMode: TPreferredAppMode): TPreferredAppMode; stdcall; // ordinal 135, since 18334

function AllowDarkModeForWindow(hWnd: HWND; allow: bool): bool;
begin
  if (g_darkModeSupported) then
    Result:= _AllowDarkModeForWindow(hWnd, allow)
  else
    Result:= false;
end;

function IsHighContrast(): bool;
var
  highContrast: HIGHCONTRASTW;
begin
  highContrast.cbSize:= SizeOf(HIGHCONTRASTW);
  if (SystemParametersInfoW(SPI_GETHIGHCONTRAST, SizeOf(highContrast), @highContrast, 0)) then
    Result:= (highContrast.dwFlags and HCF_HIGHCONTRASTON <> 0)
  else
    Result:= false;
end;

function ShouldAppsUseDarkMode: Boolean;
var
  bb:bool;
begin
  bb:=_ShouldAppsUseDarkMode();
  Result:= (_ShouldAppsUseDarkMode() or (AppMode = pamForceDark)) and not IsHighContrast();
end;

procedure RefreshTitleBarThemeColor(hWnd: HWND);
const
  DWMWA_USE_IMMERSIVE_DARK_MODE_OLD = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE_NEW = 20;
var
  dark: BOOL;
  dwAttribute: DWORD;
begin
  dark:= (_IsDarkModeAllowedForWindow(hWnd) and ShouldAppsUseDarkMode);

  if (Win32BuildNumber < 19041) then
    dwAttribute:= DWMWA_USE_IMMERSIVE_DARK_MODE_OLD
  else begin
    dwAttribute:= DWMWA_USE_IMMERSIVE_DARK_MODE_NEW;
  end;

  DwmSetWindowAttribute(hwnd, dwAttribute, @dark, SizeOf(dark));
end;

procedure AllowDarkModeForApp(allow: bool);
begin
  if Assigned(_AllowDarkModeForApp) then
    _AllowDarkModeForApp(allow)
  else if Assigned(_SetPreferredAppMode) then
  begin
    if (allow) then
      _SetPreferredAppMode(AppMode)
    else
      _SetPreferredAppMode(pamDefault);
  end;
end;

{$IF DEFINED(LCLQT5) OR DEFINED(LCLQT6)}
procedure ApplyDarkStyle;
const
  StyleName: WideString = 'Fusion';
var
  AColor: TQColor;
  APalette: QPaletteH;

  function QColor(R: Integer; G: Integer; B: Integer; A: Integer = 255): PQColor;
  begin
    Result:= @AColor;
    QColor_fromRgb(Result, R, G, B, A);
  end;

begin
  //g_darkModeEnabled:= True;

  QApplication_setStyle(QStyleFactory_create(@StyleName));

  APalette:= QPalette_Create();

  // Modify palette to dark
  if (AppMode = pamForceDark) then
  begin
    // DarkMode Pallete
    QPalette_setColor(APalette, QPaletteWindow, QColor(53, 53, 53));
    QPalette_setColor(APalette, QPaletteWindowText, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteWindowText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteBase, QColor(42, 42, 42));
    QPalette_setColor(APalette, QPaletteAlternateBase, QColor(66, 66, 66));
    QPalette_setColor(APalette, QPaletteToolTipBase, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteToolTipText, QColor(53, 53, 53));
    QPalette_setColor(APalette, QPaletteText, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteDark, QColor(35, 35, 35));
    QPalette_setColor(APalette, QPaletteLight, QColor(66, 66, 66));
    QPalette_setColor(APalette, QPaletteShadow, QColor(20, 20, 20));
    QPalette_setColor(APalette, QPaletteButton, QColor(53, 53, 53));
    QPalette_setColor(APalette, QPaletteButtonText, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteButtonText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteBrightText, QColor(255, 0, 0));
    QPalette_setColor(APalette, QPaletteLink, QColor(42, 130, 218));
    QPalette_setColor(APalette, QPaletteHighlight, QColor(42, 130, 218));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlight, QColor(80, 80, 80));
    QPalette_setColor(APalette, QPaletteHighlightedText, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlightedText, QColor(127, 127, 127));
  end
  else
  begin
    // LightMode Pallete
    QPalette_setColor(APalette, QPaletteWindow, QColor(240, 240, 240));
    QPalette_setColor(APalette, QPaletteWindowText, QColor(0, 0, 0));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteWindowText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteBase, QColor(225, 225, 225));
    QPalette_setColor(APalette, QPaletteAlternateBase, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteToolTipBase, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteToolTipText, QColor(0, 0, 0));
    QPalette_setColor(APalette, QPaletteText, QColor(0, 0, 0));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteDark, QColor(200, 200, 200));
    QPalette_setColor(APalette, QPaletteLight, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteShadow, QColor(220, 220, 220));
    QPalette_setColor(APalette, QPaletteButton, QColor(240, 240, 240));
    QPalette_setColor(APalette, QPaletteButtonText, QColor(0, 0, 0));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteButtonText, QColor(127, 127, 127));
    QPalette_setColor(APalette, QPaletteBrightText, QColor(255, 0, 0));
    QPalette_setColor(APalette, QPaletteLink, QColor(42, 130, 218));
    QPalette_setColor(APalette, QPaletteHighlight, QColor(42, 130, 218));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlight, QColor(200, 200, 200));
    QPalette_setColor(APalette, QPaletteHighlightedText, QColor(255, 255, 255));
    QPalette_setColor(APalette, QPaletteDisabled, QPaletteHighlightedText, QColor(127, 127, 127));
  end;
  
  QApplication_setPalette(APalette);
end;
{$ENDIF}

const
  LOAD_LIBRARY_SEARCH_SYSTEM32 = $800;

function CheckBuildNumber(buildNumber: DWORD): Boolean; inline;
begin
  Result := (buildNumber = 17763) or // Win 10: 1809
            (buildNumber = 18362) or // Win 10: 1903 & 1909
            (buildNumber = 19041) or // Win 10: 2004 & 20H2 & 21H1 & 21H2
            (buildNumber = 22000) or // Win 11: 21H2
            (buildNumber > 22000);   // Win 11: Insider Preview
end;

function GetBuildNumber(Instance: THandle): DWORD;
begin
  try
    with TVersionInfo.Create do
    try
      Load(Instance);
      Result:= FixedInfo.FileVersion[2];
    finally
      Free;
    end;
  except
    Exit(0);
  end;
end;

procedure InitDarkMode();
var
  hUxtheme: HMODULE;
  major, minor, build: DWORD;
begin
  @RtlGetNtVersionNumbers := GetProcAddress(GetModuleHandleW('ntdll.dll'), 'RtlGetNtVersionNumbers');
  if Assigned(RtlGetNtVersionNumbers) then
  begin
    RtlGetNtVersionNumbers(@major, @minor, @build);

    if (major = 10) and (minor = 0) then
    begin
      hUxtheme := LoadLibraryExW('uxtheme.dll', 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
      if (hUxtheme <> 0) then
      begin
        g_buildNumber:= GetBuildNumber(hUxtheme);

        if CheckBuildNumber(g_buildNumber) then
        begin
          @_RefreshImmersiveColorPolicyState := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(104));
          @_ShouldAppsUseDarkMode := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(132));
          @_AllowDarkModeForWindow := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(133));

          if (g_buildNumber < 18362) then
            @_AllowDarkModeForApp := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(135))
          else
            @_SetPreferredAppMode := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(135));

          @_IsDarkModeAllowedForWindow := GetProcAddress(hUxtheme, MAKEINTRESOURCEA(137));

          @DwmSetWindowAttribute := GetProcAddress(LoadLibrary('dwmapi.dll'), 'DwmSetWindowAttribute');

          if Assigned(_RefreshImmersiveColorPolicyState) and
             Assigned(_ShouldAppsUseDarkMode) and
             Assigned(_AllowDarkModeForWindow) and
             (Assigned(_AllowDarkModeForApp) or Assigned(_SetPreferredAppMode)) and
             Assigned(_IsDarkModeAllowedForWindow) then
          begin
            g_darkModeSupported := true;
            AppMode := PreferredAppMode;
            if AppMode <> pamForceLight then
            begin
              AllowDarkModeForApp(true);
              _RefreshImmersiveColorPolicyState();
              IsDarkModeEnabled := ShouldAppsUseDarkMode;
              if IsDarkModeEnabled then AppMode := pamForceDark;
            end;
          end;
        end;
      end;
    end;
  end;
end;

initialization
end.

