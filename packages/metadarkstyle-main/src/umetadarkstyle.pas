{
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}

unit uMetaDarkStyle;

interface

{$IFDEF WINDOWS}
uses
    {IF DEFINED(LCLQT5)}
      uDarkStyle,
    {ENDIF}
    uDarkStyleParams,
    {$IFDEF LCLWIN32}
    uWin32WidgetSetDark,
    {$ENDIF}
    uDarkStyleSchemesLoader;
{$ENDIF}

{$IFDEF WINDOWS}
procedure ApplyMetaDarkStyle(const CS:TDSColors);
{$ENDIF}
procedure MetaDarkFormChanged(Form: TObject);

implementation

{$IFDEF WINDOWS}
procedure ApplyMetaDarkStyle(const CS:TDSColors);
begin
  InitDarkMode;
  Initialize(CS);
  ApplyDarkStyle;
end;
{$ENDIF}

procedure MetaDarkFormChanged(Form: TObject);
begin
  {$IFDEF LCLWIN32}DarkFormChanged(Form);{$ENDIF}
end;
end.
