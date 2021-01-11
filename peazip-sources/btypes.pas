unit BTypes;

{Common basic type definitions}


interface


{$i STD.INC}

(*************************************************************************

 DESCRIPTION     :  Common basic type definitions

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D12/D17-D22, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.04.06  W.Ehrhardt  Initial version
 0.11     15.04.06  we          With $ifdef HAS_XTYPES
 0.12     15.04.06  we          FPC1_0 and pShortInt
 0.13     09.09.08  we          UInt32 = cardinal $ifdef HAS_CARD32
 0.14     12.11.08  we          Str127, Ptr2Inc
 0.15     14.11.08  we          BString, char8
 0.16     21.11.08  we          __P2I: type cast pointer to integer for masking etc
 0.17     02.12.08  we          Use pchar and pAnsiChar for pchar8 if possible
 0.18     27.02.09  we          pBoolean
 0.19     14.02.12  we          extended = double $ifdef SIMULATE_EXT64
 0.20     06.05.14  we          extended = double $ifdef SIMULATE_EXT64 OR EXT64
 0.21     25.04.15  we          With $ifdef HAS_INTXX, HAS_PINTXX
*************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006-2015 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

{$ifdef BIT16}
type
  Int8   = ShortInt;                { 8 bit   signed integer}
  Int16  = Integer;                 {16 bit   signed integer}
  Int32  = Longint;                 {32 bit   signed integer}
  UInt8  = Byte;                    { 8 bit unsigned integer}
  UInt16 = Word;                    {16 bit unsigned integer}
  UInt32 = Longint;                 {32 bit unsigned integer}

  Smallint    = Integer;
  Shortstring = string;

  pByte       = ^Byte;
  pBoolean    = ^Boolean;
  pShortInt   = ^ShortInt;
  pWord       = ^Word;
  pSmallInt   = ^SmallInt;
  pLongint    = ^Longint;

{$else}

  {$ifndef HAS_INTXX}
  type
    Int8   = ShortInt;                { 8 bit   signed integer}
    Int16  = SmallInt;                {16 bit   signed integer}
    Int32  = Longint;                 {32 bit   signed integer}
    UInt8  = Byte;                    { 8 bit unsigned integer}
    UInt16 = Word;                    {16 bit unsigned integer}
    {$ifdef HAS_CARD32}
    UInt32 = Cardinal;                {32 bit unsigned integer}
    {$else}
    UInt32 = Longint;                 {32 bit unsigned integer}
    {$endif}
  {$endif}

  {$ifndef HAS_XTYPES}
  type
    pByte     = ^Byte;
    pBoolean  = ^Boolean;
    pShortInt = ^ShortInt;
    pWord     = ^Word;
    pSmallInt = ^SmallInt;
    pLongint  = ^Longint;
  {$endif}
  {$ifdef FPC} {$ifdef VER1_0}
  type
    pBoolean  = ^Boolean;
    pShortInt = ^ShortInt;
  {$endif} {$endif}

{$endif} {BIT16}

type
  Str255  = string[255];   {Handy type to avoid problems with 32 bit and/or unicode}
  Str127  = string[127];

type
{$ifndef HAS_PINTXX}
  pInt8   = ^Int8;
  pInt16  = ^Int16;
  pInt32  = ^Int32;
  pUInt8  = ^UInt8;
  pUInt16 = ^UInt16;
  pUInt32 = ^UInt32;
{$endif}
  pStr255 = ^Str255;
  pStr127 = ^Str127;

{$ifdef BIT16}
  {$ifdef V7Plus}
    type
      BString  = string[255];   {String of 8 bit characters}
      pBString = ^BString;
      char8    = char;          {8 bit characters}
      pchar8   = pchar;
  {$else}
    type
      BString  = string[255];   {String of 8 bit characters}
      pBString = ^BString;
      char8    = char;          {8 bit characters}
      pchar8   = ^char;
  {$endif}
{$else}
  {$ifdef UNICODE}
    type
      BString  = AnsiString;    {String of 8 bit characters}
      pBString = pAnsiString;
      char8    = AnsiChar;      {8 bit characters}
      pchar8   = pAnsiChar;
    {$else}
    type
      BString  = AnsiString;    {String of 8 bit characters}
      pBString = pAnsiString;
      char8    = AnsiChar;      {8 bit characters}
      pchar8   = pAnsiChar;
  {$endif}
{$endif}


{$ifdef V7Plus}
type
  Ptr2Inc = pByte;         {Type cast to increment untyped pointer}
{$else}
type
  Ptr2Inc = Longint;       {Type cast to increment untyped pointer}
{$endif}


{$ifdef FPC}
  {$ifdef VER1}
    type __P2I = longint;  {Type cast pointer to integer for masking etc}
  {$else}
    type __P2I = PtrUInt;  {Type cast pointer to integer for masking etc}
  {$endif}
{$else}
  {$ifdef BIT64}
    type __P2I = NativeInt;  {Type cast pointer to integer for masking etc}
  {$else}
    type __P2I = longint;    {Type cast pointer to integer for masking etc}
  {$endif}
{$endif}


{$ifdef EXT64}
  type extended = double;    {Force 64-bit 'extended'}
{$else}
  {$ifdef SIMULATE_EXT64}
    type extended = double;  {Debug simulation EXT64}
  {$endif}
{$endif}


implementation

end.
