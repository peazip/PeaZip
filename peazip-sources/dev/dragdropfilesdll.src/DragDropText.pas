unit DragDropText;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropText
// Description:     Implements Dragging and Dropping of different text formats.
// Version:         5.2
// Date:            17-AUG-2010
// Target:          Win32, Delphi 5-2010
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//
// Lazarus adaption 10/2017 Michael Köcher / six1
// -----------------------------------------------------------------------------

{.$define DROPSOURCE_TEXTSCRAP}

interface

uses
  DragDrop,
  DropTarget,
  DropSource,
  DragDropFormats,
  ActiveX,
  Windows,
  Classes;

{$include DragDrop.inc}

type
////////////////////////////////////////////////////////////////////////////////
//
//              TCustomAnsiTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for simple ansi text based clipboard formats.
////////////////////////////////////////////////////////////////////////////////
  TCustomAnsiTextClipboardFormat = class(TCustomAnsiStringClipboardFormat)
  private
  protected
    function GetSize: integer; override;
    property Text: AnsiString read GetString write SetString;
  public
    constructor Create; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomUnicodeTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for simple string clipboard formats storing the data in a
// UniCode (wide) string.
////////////////////////////////////////////////////////////////////////////////
  TCustomUnicodeTextClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FText: UnicodeString;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;

    function GetText: UnicodeString;
    procedure SetText(const Value: UnicodeString);
    property Text: UnicodeString read FText write FText;
  public
    procedure Clear; override;
    function HasData: boolean; override;
  end;

  TCustomWideTextClipboardFormat = TCustomUnicodeTextClipboardFormat deprecated;

////////////////////////////////////////////////////////////////////////////////
//
//              TCustomTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TCustomTextClipboardFormat = TCustomUnicodeTextClipboardFormat;
{$else}
  TCustomTextClipboardFormat = TCustomAnsiTextClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Note: The clipboard automatically synthesizes the CF_TEXT format from the
// CF_OEMTEXT format. On Windows NT/2000 the clipboard also synthesizes from the
// CF_UNICODETEXT format.
////////////////////////////////////////////////////////////////////////////////
  TAnsiTextClipboardFormat = class(TCustomAnsiTextClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Text;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Note: On Windows NT/2000 the clipboard automatically synthesizes the
// CF_UNICODETEXT format from the CF_TEXT and CF_OEMTEXT formats.
////////////////////////////////////////////////////////////////////////////////
  TUnicodeTextClipboardFormat = class(TCustomUnicodeTextClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Text;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TTextClipboardFormat = TUnicodeTextClipboardFormat;
{$else}
  TTextClipboardFormat = TAnsiTextClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//              TOEMTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
// Note: The clipboard automatically synthesizes the CF_OEMTEXT format from the
// CF_TEXT format. On Windows NT/2000 the clipboard also synthesizes from the
// CF_UNICODETEXT format.
////////////////////////////////////////////////////////////////////////////////
  TOEMTextClipboardFormat = class(TCustomAnsiTextClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Text;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TRichTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TRichTextClipboardFormat = class(TCustomAnsiTextClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Text;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCSVClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TCSVClipboardFormat = class(TCustomStringListClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    property Lines;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TLocaleClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TLocaleClipboardFormat = class(TCustomDWORDClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    function HasData: boolean; override;
    property Locale: DWORD read GetValueDWORD write SetValueDWORD;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TTextDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TTextDataFormat = class(TCustomDataFormat)
  private
    FUnicodeText: UnicodeString;
    FAnsiText: AnsiString;
    FNeedAnsi, FNeedUnicode: boolean;
    FLocale: DWORD;
  protected
    class procedure RegisterCompatibleFormats; override;
    function GetAnsiText: AnsiString;
    procedure SetAnsiText(const Value: AnsiString);
    function GetUnicodeText: UnicodeString;
    procedure SetUnicodeText(const Value: UnicodeString);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetLocale: DWORD;
    procedure SetLocale(const Value: DWORD);
    function GetCodePage: integer;
    property CodePage: integer read GetCodePage;
  public
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property AnsiText: AnsiString read GetAnsiText write SetAnsiText;
    property Text: string read GetText write SetText;
    property UnicodeText: UnicodeString read GetUnicodeText write SetUnicodeText;
    property Locale: DWORD read GetLocale write SetLocale;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropTextTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropTextTarget = class(TCustomDropMultiTarget)
  private
    FTextFormat: TTextDataFormat;
  protected
    function GetText: string;
    function GetAnsiText: AnsiString;
    function GetLocale: DWORD;
    function GetUnicodeText: UnicodeString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: string read GetText;
    property AnsiText: AnsiString read GetAnsiText;
    property UnicodeText: UnicodeString read GetUnicodeText;
    property Locale: DWORD read GetLocale;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropTextSource
//
////////////////////////////////////////////////////////////////////////////////
  TDropTextSource = class(TCustomDropMultiSource)
  private
    FTextFormat: TTextDataFormat;
  protected
    function GetText: string;
    function GetAnsiText: AnsiString;
    function GetLocale: DWORD;
    function GetUnicodeText: UnicodeString;
    procedure SetText(const Value: string);
    procedure SetAnsiText(const Value: AnsiString);
    procedure SetLocale(const Value: DWORD);
    procedure SetUnicodeText(const Value: UnicodeString);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property AnsiText: AnsiString read GetAnsiText write SetAnsiText;
    property UnicodeText: UnicodeString read GetUnicodeText write SetUnicodeText;
  published
    property Locale: DWORD read GetLocale write SetLocale default 0;
    property Text: string read GetText write SetText;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//              Misc.
//
////////////////////////////////////////////////////////////////////////////////
function IsRTF(const s: string): boolean;
function MakeRTF(const s: string): AnsiString;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//                      IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
  ShlObj,
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//
//              Utilities
//
////////////////////////////////////////////////////////////////////////////////
function IsRTF(const s: string): boolean;
begin
  // This probably isn't a valid test, but it will have to do until I have
  // time to research the RTF specifications.
  { TODO -oanme -cImprovement : Need a solid test for RTF format. }
  Result := (AnsiStrLIComp(PChar(s), '{\rtf', 5) = 0);
end;

{ TODO -oanme -cImprovement : Needs RTF to text conversion. Maybe ITextDocument can be used. }

function MakeRTF(const s: string): AnsiString;
type
  PSmallint = ^Smallint;
const
  sRtfHeader: AnsiString = '{\rtf1\ansi {\*\generator Drag and Drop Component Suite}';
var
  p: PChar;
  c: Char;
  cAnsi: AnsiChar;
begin
  { DONE -oanme -cImprovement : Needs to escape \ in text to RTF conversion. }
  { DONE -oanme -cImprovement : Needs better text to RTF conversion. }
  if (not IsRTF(s)) then
  begin
    Result := sRtfHeader;
    p := PChar(s);
    while (ord(p^) <> 0) do
    begin
      c := p^;
      cAnsi := AnsiChar(c);
      if (Ord(c) <= 127) then // Char is ANSI
      begin
        case cAnsi of
        '\', '{', '}':
          Result := Result + '\' + cAnsi;
        Chr(9):
          Result := Result + '\tab ';
        Chr(13):
          Result := Result + '\par ';
        else
          if (Ord(cAnsi) > 31) and (Ord(cAnsi) < 127) then
            Result := Result + cAnsi
          else
            Result := Result + AnsiString(Format('\''%.2x', [ord(c)]));
        end;
      end else // Char is Unicode
      begin
        if (cAnsi < ' ') or (cAnsi in ['0'..'9', '\', '{', '}']) then
          cAnsi := '?';
        Result := Result + AnsiString(Format('\u%d%s', [Smallint(Ord(c)), cAnsi]));
      end;

      inc(p);
    end;
    Result := Result + '}'
  end else
    Result := AnsiString(s);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomAnsiTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TCustomAnsiTextClipboardFormat.Create;
begin
  inherited Create;
  TrimZeroes := True;
end;

function TCustomAnsiTextClipboardFormat.GetSize: integer;
begin
  Result := inherited GetSize;
  // Unless the data is already zero terminated, we add a byte to include
  // the string's implicit terminating zero.
  if (ord(Data[Result]) <> 0) then
    inc(Result);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TAnsiTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TAnsiTextClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_TEXT;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TCustomUnicodeTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TCustomUnicodeTextClipboardFormat.Clear;
begin
  FText := '';
end;

function TCustomUnicodeTextClipboardFormat.HasData: boolean;
begin
  Result := (FText <> '');
end;

function TCustomUnicodeTextClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  SetLength(FText, Size div 2);
  Move(Value^, PWideChar(FText)^, Size);
  Result := True;
end;

function TCustomUnicodeTextClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size <= (Length(FText)+1)*2);
  if (Result) then
    Move(PWideChar(FText)^, Value^, Size);
end;

function TCustomUnicodeTextClipboardFormat.GetSize: integer;
begin
  Result := Length(FText);
  // Unless the data is already zero terminated, we add two bytes to include
  // the string's implicit terminating zero.
  if (FText[Result] <> #0) then
    inc(Result);

  Result := Result*SizeOf(WideChar);
end;

function TCustomUnicodeTextClipboardFormat.GetText: UnicodeString;
begin
  Result := FText;
end;

procedure TCustomUnicodeTextClipboardFormat.SetText(const Value: UnicodeString);
begin
  FText := Value;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TUnicodeTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TUnicodeTextClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_UNICODETEXT;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TOEMTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TOEMTextClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_OEMTEXT;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TRichTextClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_RTF: TClipFormat = 0;

function TRichTextClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  // Note: The string 'Rich Text Format', is also defined in the RichEdit
  // unit as CF_RTF
  if (CF_RTF = 0) then
    CF_RTF := RegisterClipboardFormat('Rich Text Format'); // *** DO NOT LOCALIZE ***
  Result := CF_RTF;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TCSVClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_CSV: TClipFormat = 0;

function TCSVClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_CSV = 0) then
    CF_CSV := RegisterClipboardFormat('CSV'); // *** DO NOT LOCALIZE ***
  Result := CF_CSV;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TLocaleClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
function TLocaleClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_LOCALE;
end;

function TLocaleClipboardFormat.HasData: boolean;
begin
  Result := (Locale <> 0);
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TTextDataFormat
//
////////////////////////////////////////////////////////////////////////////////

function TTextDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TAnsiTextClipboardFormat) then
  begin
    FAnsiText := TAnsiTextClipboardFormat(Source).Text;
    FNeedAnsi := False;
    FNeedUnicode := FNeedUnicode or ((FAnsiText <> '') and (FUnicodeText = ''));
  end

  else if (Source is TUnicodeTextClipboardFormat) then
  begin
    FUnicodeText := TUnicodeTextClipboardFormat(Source).Text;
    FNeedUnicode := False;
    FNeedAnsi := FNeedAnsi or ((FUnicodeText <> '') and (FAnsiText = ''));
  end

  else if (Source is TLocaleClipboardFormat) then
    Locale := TLocaleClipboardFormat(Source).Locale


  else
    Result := inherited Assign(Source);
end;

function TTextDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
var
  s: AnsiString;
begin
  Result := True;

  if (Dest is TAnsiTextClipboardFormat) then
  begin
    TAnsiTextClipboardFormat(Dest).Text := AnsiText
  end else

  if (Dest is TRichTextClipboardFormat) then
  begin
    TRichTextClipboardFormat(Dest).Text := MakeRTF(Text);
  end else

  if (Dest is TUnicodeTextClipboardFormat) then
  begin
    TUnicodeTextClipboardFormat(Dest).Text := UnicodeText;
  end else

  if (Dest is TOEMTextClipboardFormat) then
  begin
    // Synthesize OEM text
    if (HasData) then
    begin
      // Convert source string to OEM string.
      SetLength(s, Length(Text));
      CharToOemBuff(PChar(Text), PAnsiChar(s), Length(s));
      TOEMTextClipboardFormat(Dest).Text := s;
    end else
      TOEMTextClipboardFormat(Dest).Text := '';
  end else

  if (Dest is TLocaleClipboardFormat) then
  begin
    TLocaleClipboardFormat(Dest).Locale := Locale
  end else

    Result := inherited AssignTo(Dest);
end;

procedure TTextDataFormat.Clear;
begin
  Changing;
  FAnsiText := '';
  FUnicodeText := '';
  FNeedAnsi := True;
  FNeedUnicode := True;
end;

function TTextDataFormat.GetAnsiText: AnsiString;
begin
  if (FNeedAnsi) and (not FNeedUnicode) then
  begin
    SetLength(FAnsiText, Length(FUnicodeText));
    // Take Locale into account and synthesize ANSI text from Unicode text.
    WideCharToMultiByte(CodePage, 0, PWideChar(FUnicodeText), Length(FUnicodeText),
      PAnsiChar(FAnsiText), Length(FAnsiText), nil, nil);
    // FAnsiText := FUnicodeText;
  end;
  Result := FAnsiText;
end;

function TTextDataFormat.GetCodePage: integer;
const
  CP_ACP = 0;                                // system default code page
  LOCALE_IDEFAULTANSICODEPAGE = $00001004;   // default ansi code page
var
  ResultCode: Integer;
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(Locale, LOCALE_IDEFAULTANSICODEPAGE, Buffer, Length(Buffer));
  Val(Buffer, Result, ResultCode);
  if ResultCode <> 0 then
    Result := CP_ACP;
end;

function TTextDataFormat.GetLocale: DWORD;
begin
  if (FLocale = 0) and not(csDesigning in Owner.ComponentState) then
    Result := GetThreadLocale
  else
    Result := FLocale;
end;

function TTextDataFormat.GetText: string;
begin
{$ifdef UNICODE}
  Result := UnicodeText;
{$else}
  Result := AnsiText;
{$endif}
end;

function TTextDataFormat.GetUnicodeText: UnicodeString;
begin
  if (FNeedUnicode) and (not FNeedAnsi) then
  begin
    SetLength(FUnicodeText, Length(FAnsiText));
    // Take Locale into account and synthesize Unicode text from ANSI text.
    MultiByteToWideChar(CodePage, 0, PAnsiChar(FAnsiText), Length(FAnsiText),
      PWideChar(FUnicodeText), Length(FUnicodeText));
    // FUnicodeText := FAnsiText;
  end;
  Result := FUnicodeText;
end;

procedure TTextDataFormat.SetText(const Value: string);
begin
{$ifdef UNICODE}
  SetUnicodeText(Value);
{$else}
  SetAnsiText(Value);
{$endif}
end;

procedure TTextDataFormat.SetAnsiText(const Value: AnsiString);
begin
  Changing;
  FAnsiText := Value;
  FUnicodeText := '';
  FNeedAnsi := False;
  FNeedUnicode := True;
  if (FLocale = 0) and not(csDesigning in Owner.ComponentState) then
    FLocale := GetThreadLocale;
end;

procedure TTextDataFormat.SetUnicodeText(const Value: UnicodeString);
begin
  Changing;
  FUnicodeText := Value;
  FAnsiText := '';
  FNeedUnicode := False;
  FNeedAnsi := True;
end;

procedure TTextDataFormat.SetLocale(const Value: DWORD);
begin
  Changing;
  FLocale := Value;
end;

function TTextDataFormat.HasData: boolean;
begin
  Result := ((not FNeedAnsi) and (FAnsiText <> '')) or
    ((not FNeedUnicode) and (FUnicodeText <> ''));
end;

function TTextDataFormat.NeedsData: boolean;
begin
  Result := (FNeedAnsi and (FAnsiText = '')) or
    (FNeedUnicode and (FUnicodeText = ''));
end;


class procedure TTextDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TAnsiTextClipboardFormat, AnsiBoost);
  RegisterDataConversion(TLocaleClipboardFormat, AnsiBoost);
  RegisterDataConversion(TUnicodeTextClipboardFormat, UnicodeBoost);
  RegisterDataConversion(TOEMTextClipboardFormat, AnsiBoost);
  RegisterDataConversion(TRichTextClipboardFormat);
end;

////////////////////////////////////////////////////////////////////////////////
//
//              TDropTextTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropTextTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextFormat := TTextDataFormat.Create(Self);
end;

destructor TDropTextTarget.Destroy;
begin
  FTextFormat.Free;
  inherited Destroy;
end;

function TDropTextTarget.GetAnsiText: AnsiString;
begin
  Result := FTextFormat.AnsiText;
end;

function TDropTextTarget.GetLocale: DWORD;
begin
  Result := FTextFormat.Locale;
end;

function TDropTextTarget.GetText: string;
begin
  Result := FTextFormat.Text;
end;

function TDropTextTarget.GetUnicodeText: UnicodeString;
begin
  Result := FTextFormat.UnicodeText;
end;


////////////////////////////////////////////////////////////////////////////////
//
//              TDropTextSource
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropTextSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTextFormat := TTextDataFormat.Create(Self);
end;

destructor TDropTextSource.Destroy;
begin
  FTextFormat.Free;
  inherited Destroy;
end;

function TDropTextSource.GetAnsiText: AnsiString;
begin
  Result := FTextFormat.AnsiText;
end;

function TDropTextSource.GetLocale: DWORD;
begin
  Result := FTextFormat.Locale;
end;

function TDropTextSource.GetText: string;
begin
  Result := FTextFormat.Text;
end;

function TDropTextSource.GetUnicodeText: UnicodeString;
begin
  Result := FTextFormat.UnicodeText;
end;

procedure TDropTextSource.SetAnsiText(const Value: AnsiString);
begin
  FTextFormat.AnsiText := Value;
end;

procedure TDropTextSource.SetLocale(const Value: DWORD);
begin
  FTextFormat.Locale := Value;
end;

procedure TDropTextSource.SetText(const Value: string);
begin
  FTextFormat.Text := Value;
end;

procedure TDropTextSource.SetUnicodeText(const Value: UnicodeString);
begin
  FTextFormat.UnicodeText := Value;
end;

////////////////////////////////////////////////////////////////////////////////
//
//              Initialization/Finalization
//
////////////////////////////////////////////////////////////////////////////////

initialization
  // Data format registration
  TTextDataFormat.RegisterDataFormat;
  // Clipboard format registration
  TAnsiTextClipboardFormat.RegisterFormat;
  TUnicodeTextClipboardFormat.RegisterFormat;
  TRichTextClipboardFormat.RegisterFormat;
  TOEMTextClipboardFormat.RegisterFormat;
  TCSVClipboardFormat.RegisterFormat;
  TLocaleClipboardFormat.RegisterFormat;

finalization

end.
