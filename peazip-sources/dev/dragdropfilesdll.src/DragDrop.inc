// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDrop.inc
// Description:     This include file contains common defines used by the
//                  library.
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-2010 Anders Melander
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//                      Detect compiler version
//                      Supports D1 - D2010, Win32
// -----------------------------------------------------------------------------

// Delphi 1.x
{$IFDEF VER80}
  {$DEFINE VER8_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 2.x
{$IFDEF VER90}
  {$DEFINE VER9x}
  {$DEFINE VER9_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++Builder 1.x
{$IFDEF VER93}
  {$DEFINE BCB}
  {$DEFINE VER9x}
  {$DEFINE VER93_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++Builder 3.x
{$IFDEF VER110}
  {$DEFINE BCB}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++Builder 4.x
{$IFDEF VER125}
  {$DEFINE BCB}
  {$DEFINE VER125_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 5.x
// C++Builder 5.x
{$IFDEF VER130}
  {$DEFINE VER13_PLUS}
  {$IFDEF BCB}
    {$DEFINE VER135_PLUS}
  {$ENDIF}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 6.x
// C++Builder 6.x
{$IFDEF VER140}
  {$DEFINE VER14_PLUS}
  {$IFDEF BCB}
    {$DEFINE VER145_PLUS}
  {$ENDIF}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 7.x
{$IFDEF VER150}
  {$DEFINE VER15_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 8.x
{$IFDEF VER160}
  {$DEFINE VER16_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 9.x (Delphi 2005)
{$IFDEF VER170}
  {$DEFINE VER17_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 10.x (Delphi 2006)
{$IFDEF VER180}
  {$DEFINE VER18_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 2007
// Note: D2007/win32 defines VER180 and VER185. D2007/.NET defines VER190
{$IFDEF VER185}
  {$DEFINE VER185_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}
{$IFDEF VER190}
  {$DEFINE VER19_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 2009
{$IFDEF VER200}
  {$DEFINE VER20_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 2010
{$IFDEF VER210}
  {$DEFINE VER21_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Unknown compiler version - assume D2009 compatible for Unicode or D2007 for Ansi
{$IFNDEF VER_OK}
  {$ifdef UNICODE}
    {$DEFINE VER20_PLUS}
  {$else}
    {$DEFINE VER19_PLUS}
  {$endif}
{$ELSE}
  {$UNDEF VER_OK}
{$ENDIF}

{$IFDEF VER21_PLUS}
  {$DEFINE VER20_PLUS}
{$ENDIF}
{$IFDEF VER20_PLUS}
  {$DEFINE VER19_PLUS}
{$ENDIF}
{$IFDEF VER19_PLUS}
  {$DEFINE VER185_PLUS}
{$ENDIF}
{$IFDEF VER185_PLUS}
  {$DEFINE VER18_PLUS}
{$ENDIF}
{$IFDEF VER18_PLUS}
  {$DEFINE VER17_PLUS}
{$ENDIF}
{$IFDEF VER17_PLUS}
  {$DEFINE VER16_PLUS}
{$ENDIF}
{$IFDEF VER16_PLUS}
  {$DEFINE VER15_PLUS}
{$ENDIF}
{$IFDEF VER15_PLUS}
  {$DEFINE VER145_PLUS}
{$ENDIF}
{$IFDEF VER145_PLUS}
  {$DEFINE VER14_PLUS}
{$ENDIF}
{$IFDEF VER14_PLUS}
  {$DEFINE VER135_PLUS}
{$ENDIF}
{$IFDEF VER135_PLUS}
  {$DEFINE VER13_PLUS}
{$ENDIF}
{$IFDEF VER13_PLUS}
  {$DEFINE VER125_PLUS}
{$ENDIF}
{$IFDEF VER125_PLUS}
  {$DEFINE VER12_PLUS}
{$ENDIF}
{$IFDEF VER12_PLUS}
  {$DEFINE VER11_PLUS}
{$ENDIF}
{$IFDEF VER11_PLUS}
  {$DEFINE VER10_PLUS}
{$ENDIF}
{$IFDEF VER10_PLUS}
  {$DEFINE VER93_PLUS}
{$ENDIF}
{$IFDEF VER93_PLUS}
  {$DEFINE VER9_PLUS}
{$ENDIF}
{$IFDEF VER9_PLUS}
  {$DEFINE VER8_PLUS}
{$ENDIF}


// -----------------------------------------------------------------------------
//              Required compiler directives
// -----------------------------------------------------------------------------

{$BOOLEVAL OFF}         // Short circuit boolean evaluation.
{$EXTENDEDSYNTAX ON}    // Enable Delphi Pascal extensions.
{$LONGSTRINGS ON}       // String = AnsiString.
{$ALIGN ON}             // Aligned data. Required by COM.
{$ifdef BCB}
  {$ObjExportAll ON}    // Required for C++ Builder
{$endif}

// The following are not realy nescessary, but they are good practice.
{$TYPEDADDRESS ON}      // @ operator returns typed pointer.
{$WRITEABLECONST OFF}   // Typed consts are R/O.

// Disable platform warnings. This library is only supported on Windows.
{$ifdef VER14_PLUS}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$endif}
// Disable "unsafe" warnings. Let's assume I know what I'm doing.
{$ifdef VER15_PLUS}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$endif}

