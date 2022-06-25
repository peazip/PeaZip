{-Test prog for BTypes unit, we Feb. 2012}

program T_BTypes;

{$i std.inc}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifdef delphi}
  {$hints off}
{$endif}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  BTypes;


var
  v_integer     : integer;
  v_longint     : longint;
  v_pointer     : pointer;
  v_char        : char;
  v_string      : string;
  v_char8       : char8;
  v_Int8        : Int8;
  v_Int16       : Int16;
  v_Int32       : Int32;
  v_UInt8       : UInt8;
  v_UInt16      : UInt16;
  v_UInt32      : UInt32;
  v_Str127      : Str127;
  v_Str255      : Str255;
  v_pInt8       : pInt8;
  v_pchar8      : pchar8;
  v_pInt16      : pInt16;
  v_pInt32      : pInt32;
  v_pUInt8      : pUInt8;
  v_pUInt16     : pUInt16;
  v_pUInt32     : pUInt32;
  v_pStr127     : pStr127;
  v_pStr255     : pStr255;
  v_BString     : BString;
  v_smallint    : smallint;
  v_shortstring : shortstring;
  v_pByte       : pByte;
  v_pBoolean    : pBoolean;
  v_pLongint    : pLongint;
  v_pWord       : pWord;
  v_pSmallInt   : pSmallInt;
  v_pShortInt   : pShortInt;
  v_Ptr2Inc     : Ptr2Inc;
  v__P2I        : __P2I;
  v_sgl         : single;
  v_dbl         : double;
  v_ext         : extended;
  v_comp        : comp;

begin
  writeln('t_btypes  - Simple test program for BTypes unit');
  writeln('sizeof char        : ', sizeof(v_char       ));
  writeln('sizeof string      : ', sizeof(v_string     ));
  writeln('sizeof integer     : ', sizeof(v_integer    ));
  writeln('sizeof longint     : ', sizeof(v_longint    ));
  writeln('sizeof pointer     : ', sizeof(v_pointer    ));
  writeln('sizeof char8       : ', sizeof(v_char8      ));
  writeln('sizeof Int8        : ', sizeof(v_Int8       ));
  writeln('sizeof Int16       : ', sizeof(v_Int16      ));
  writeln('sizeof Int32       : ', sizeof(v_Int32      ));
  writeln('sizeof UInt8       : ', sizeof(v_UInt8      ));
  writeln('sizeof UInt16      : ', sizeof(v_UInt16     ));
  writeln('sizeof UInt32      : ', sizeof(v_UInt32     ));
  writeln('sizeof Str127      : ', sizeof(v_Str127     ));
  writeln('sizeof Str255      : ', sizeof(v_Str255     ));
  writeln('sizeof BString     : ', sizeof(v_BString    ));
  writeln('sizeof pchar8      : ', sizeof(v_pchar8     ));
  writeln('sizeof pInt8       : ', sizeof(v_pInt8      ));
  writeln('sizeof pInt16      : ', sizeof(v_pInt16     ));
  writeln('sizeof pInt32      : ', sizeof(v_pInt32     ));
  writeln('sizeof pUInt8      : ', sizeof(v_pUInt8     ));
  writeln('sizeof pUInt16     : ', sizeof(v_pUInt16    ));
  writeln('sizeof pUInt32     : ', sizeof(v_pUInt32    ));
  writeln('sizeof pStr127     : ', sizeof(v_pStr127    ));
  writeln('sizeof pStr255     : ', sizeof(v_pStr255    ));
  writeln('sizeof smallint    : ', sizeof(v_smallint   ));
  writeln('sizeof shortstring : ', sizeof(v_shortstring));
  writeln('sizeof pByte       : ', sizeof(v_pByte      ));
  writeln('sizeof pBoolean    : ', sizeof(v_pBoolean   ));
  writeln('sizeof pLongint    : ', sizeof(v_pLongint   ));
  writeln('sizeof pWord       : ', sizeof(v_pWord      ));
  writeln('sizeof pSmallInt   : ', sizeof(v_pSmallInt  ));
  writeln('sizeof pShortInt   : ', sizeof(v_pShortInt  ));
  writeln('sizeof Ptr2Inc     : ', sizeof(v_Ptr2Inc    ));
  writeln('sizeof __P2I       : ', sizeof(v__P2I       ));
  writeln('sizeof single      : ', sizeof(v_sgl        ));
  writeln('sizeof double      : ', sizeof(v_dbl        ));
  writeln('sizeof extended    : ', sizeof(v_ext        ));
  writeln('sizeof comp        : ', sizeof(v_comp       ));
end.
