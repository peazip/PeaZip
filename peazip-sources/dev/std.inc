(*************************************************************************

 DESCRIPTION     :  Standard definitions and options

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D12/D14-D26, FPC, VP, (TPW1.0/1.5,BCB3/4)

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     05.10.03  W.Ehrhardt  Initial version
 1.01     05.10.03  we          X_OPT, removed TP4
 1.02     30.10.03  we          WINCRT
 1.03     09.12.03  we          {$R+,S+} {$ifdef debug}
 1.04     26.12.03  we          VP: {&Optimise+,SmartLink+,Speed+} ifndef debug
 1.05     28.12.03  we          DELPHI = Delphi32 (no Delphi 1!)
 1.06     12.04.04  we          Delphi 7
 1.07     26.09.04  we          Record starting values of important options
 1.08     10.10.04  we          RESULT for Result pseudo variable
 1.09     02.01.05  we          BIT16: default $F-
 1.10     26.02.05  we          StrictLong
 1.11     05.05.05  we          D9 aka Delphi 2005
 1.12     22.05.05  we          StrictLong for FPC 2.0
 1.13     27.05.05  we          {$goto on} for FPC
 1.14     27.05.05  we          moved {$goto on} to default settings
 1.15     29.05.05  we          HAS_INT64, HAS_MSG, _STD_INC_
 1.16     06.08.05  we          J_OPT, N_OPT, HAS_INLINE
 1.17     17.08.05  we          HAS_ASSERT
 1.18     08.11.05  we          APPCONS, partial TMT,TPW15 support
 1.19     20.11.05  we          Default option {$B-}
 1.20     08.01.06  we          ABSTRACT/DEFAULT
 1.21     08.02.06  we          Fix Scanhelp quirk
 1.22     11.02.06  we          VER5X
 1.23     15.04.06  we          HAS_XTYPES
 1.24     08.05.06  we          D10 aka Delphi 2006
 1.25     25.05.06  we          Define RESULT if FPC_OBJFPC is defined
 1.26     08.09.06  we          Define RESULT/DEFAULT if FPC_DELPHI is defined
 1.27     14.11.06  we          HAS_ASSERT for FPC VER1 and VER2
 1.28     28.11.06  we          HAS_UNSAFE, $warn SYMBOL_../UNSAFE_.. OFF
 1.29     25.05.07  we          D11 aka Delphi 2007, FPC2.1.4
 1.30     23.06.07  we          FPC_ProcVar: Helper for procedure variables
 1.31     18.09.07  we          HAS_INLINE for FPC VER2
 1.32     04.10.07  we          FPC Intel ASMmode only if CPUI386 is defined
 1.33     22.11.07  we          Record value of $X option, undef RESULT if $X-
 1.34     19.05.08  we          HAS_UINT64
 1.35     21.06.08  we          V7PLUS, HAS_UINT64 for FPC VER2_2
 1.36     07.09.08  we          HAS_CARD32
 1.37     21.11.08  we          D12 aka D2009
 1.38     19.02.09  we          TPW 1.0 adjustments
 1.39     05.07.09  we          D12Plus
 1.40     17.10.09  we          BASM (BASM16 or Bit32)
 1.41     21.10.09  we          HAS_OVERLOAD
 1.42     07.04.10  we          HAS_DENORM_LIT (Denormalised extended literals, e.g. -1.23e-4942)
 1.43     20.06.10  we          D14 (VER210)
 1.45     16.10.10  we          WIN16
 1.46     05.11.10  we          FPC VER2_4
 1.47     12.11.11  we          FPC VER2_6
 1.48     01.01.12  we          HAS_UINT64 for FPC VER2_6
 1.49     12.01.12  we          BIT64, WIN32or64, Bit32or64
 1.50     13.01.12  we          EXT64 (64 bit extended = double)
 1.51     19.01.12  we          Define EXT64 if SIMULATE_EXT64
 1.52     05.09.12  we          Basic support for D14, D15(XE), D16(XE2), D17(XE3)
 1.53     01.12.12  we          Simplified FPC 2.X.Y definitions
 1.54     17.12.12  we          UNIT_SCOPE (D16/D17)
 1.55     25.12.12  we          J_OPT for BIT64
 1.56     25.04.13  we          D18/XE4 (VER250)
 1.57     28.09.13  we          Basic support for D19/XE5 (VER260)
 1.58     17.04.14  we          Basic support for D20/XE6 (VER270)
 1.59     06.05.14  we          FPC/CPUARM: $define EXT64, i.e. no FP 80-bit extended
 1.60     13.09.14  we          Basic support for D21/XE7 (VER280)
 1.61     22.10.14  we          HAS_OUT
 1.62     13.01.15  we          FPC VER3 (FPC3.0.1/3.1.1), FPC2Plus, FPC271or3
 1.63     22.04.15  we          Basic support for D22/XE8 (VER290)
 1.64     25.04.15  we          HAS_INTXX, HAS_PINTXX
 1.65     01.09.15  we          Basic support for D23 (VER300) 'Seattle'
 1.66     26.04.16  we          Basic support for D24 (VER310) 'Berlin'
 1.67     17.03.17  we          Define PurePascal for FPC/CPUARM
 1.68     11.04.17  we          Basic support for D25 (VER320) 'Tokyo'
 1.69     03.12.17  we          D22PLUS - Delphi 22 / VER290 / XE8 or higher: has system.hash
 1.70     02.08.18  we          HAS_FP_CAST (typecast for FP constants, e.g. double(1))
 1.71     04.10.18  we          Symbol FPC32Plus, avoid silly FPC32+ warnings
 1.72     08.10.18  we          Symbol FPC3Plus, avoid silly FPC3+ warnings
 1.73     24.10.18  we          Symbol D15Plus (for FormatSettings.DecimalSeparator)
 1.74     22.11.18  we          Basic support for D26 (VER330) 'Rio'
 1.75     23.11.18  we          Define DELPHI ifdef CONDITIONALEXPRESSIONS
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2018 Wolfgang Ehrhardt

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

{$ifndef _STD_INC_}

{$define _STD_INC_}      {include STD.INC only once}

{.$undef BIT16}          {16 Bit code, Pascal / D1}
{.$undef BIT32}          {32 Bit code}
{.$undef BIT64}          {64 Bit code}
{.$undef DELPHI}         {Delphi2+ and BCB++}
{.$undef G_OPT}          {G+ option support}
{.$undef D4PLUS}         {Delphi 4 or higher}
{.$undef BASM16}         {16 Bit BASM}
{.$undef LoadArgs}       {Register params}
{.$undef WINCRT}         {Use WinCRT for console}
{.$undef WIN16}          {Compiler for 16-bit windows}
{.$undef WIN32or64}      {Compiler for 32/64-bit windows}
{.$undef RESULT}         {Result pseudo variable}
{.$undef StrictLong}     {Warning for longint const with MS bit}
{.$undef HAS_INT64}      { int64 integer type available}
{.$undef HAS_UINT64}     {uint64 integer type available}
{.$undef HAS_CARD32}     {Has 32 bit cardinal}
{.$undef HAS_MSG}        {Has message directive}
{.$undef HAS_INLINE}     {Has inline procs/funcs (D9)}
{.$undef HAS_OUT}        {Has OUT parameters: D3+, FPC2+ Delphi/ObjFPC}
{.$undef ABSTRACT}       {Has abstract methods}
{.$undef DEFAULT}        {Support default parameters}
{.$undef VER5X}          {TP5 or TP55}
{.$undef HAS_XTYPES}     {Xtra types in system: pByte, pLongint etc}
{.$undef HAS_UNSAFE}     {UNSAFE warnings}
{.$undef APPCONS}        {Needs "Apptype console" for console application}
{.$undef FPC_ProcVar}    {FPC handling of @ and proc variables}
{.$undef FPC2Plus}       {FPC 2 or newer}
{.$undef FPC3Plus}       {FPC 3 or newer}
{.$undef FPC32Plus}      {FPC 3.2 or newer}
{.$undef FPC271or3}      {FPC 271 or 3 (less accurate for 64 bit or SSE2)}
{.$undef D12PLUS}        {Delphi 12 or higher}
{.$undef D15Plus}        {Delphi 21 / VER280, Formatsettings.create}
{.$undef D22PLUS}        {Delphi 22 / VER290 / XE8 or higher, has system.hash}
{.$undef HAS_OVERLOAD}   {Overloading of procedures and functions}
{.$undef HAS_DENORM_LIT} {Denormalised (extended) literals, e.g. -1.23e-4942}
{.$undef EXT64}          {64 bit extended = double}
{.$undef UNIT_SCOPE}     {Unit scope name, D16+}
{.$undef HAS_INTXX}      {Int8 .. Int32, UInt8 .. UInt32}
{.$undef HAS_PINTXX}     {pInt8 .. pInt32, pUInt8 .. pUInt32}
{.$undef HAS_FP_CAST}    {typecast for FP constants, e.g. double(1))}


{$define CONST}     {const in proc declaration}
{$define Q_OPT}     {Q- option support}
{$define X_OPT}     {X+ option support}
{$define N_OPT}     {N+ option support}
{$define BASM}      {BASM16 or BIT32}
{$define V7PLUS}    {TP7 or higher}


{$ifdef VER10}        {TPW 1.0}
  {$define BIT16}
  {$define BASM16}
  {$define WINCRT}
  {$define G_OPT}
  {$undef  CONST}
  {$undef  Q_OPT}
  {$undef  V7PLUS}
{$endif}

{$ifdef VER15}        {TPW 1.5}
  {$define BIT16}
  {$define BASM16}
  {$define WINCRT}
  {$define G_OPT}
  {$undef  CONST}
  {$undef  Q_OPT}
  {$undef  V7PLUS}
{$endif}

{$ifdef VER50 }
  {$define BIT16}
  {$define VER5X}
  {$undef  BASM}
  {$undef  CONST}
  {$undef  Q_OPT}
  {$undef  X_OPT}
  {$undef  V7PLUS}
{$endif}

{$ifdef VER55 }
  {$define BIT16}
  {$define VER5X}
  {$undef  BASM}
  {$undef  CONST}
  {$undef  Q_OPT}
  {$undef  X_OPT}
  {$undef  V7PLUS}
{$endif}

{$ifdef VER60 }
  {$define BIT16}
  {$undef  CONST}
  {$undef  Q_OPT}
  {$define G_OPT}
  {$define BASM16}
  {$undef  V7PLUS}
{$endif}

{$ifdef VER70 }
  {$define BIT16}
  {$define G_OPT}
  {$define BASM16}
{$endif}

{$ifdef VER80}
  {.$define DELPHI}   {D1} {*we V1.05}
  {$define BIT16 }
  {$define G_OPT }
  {$define BASM16}
  {$define WINCRT}
  {$define RESULT}
{$endif}

{$ifdef VER90 }
  {$define DELPHI}  {D2}
{$endif}

{$ifdef VER93 }
  {$define DELPHI}  {BCB++1}
{$endif}

{$ifdef VER100}
  {$define DELPHI}  {D3}
  {$define HAS_ASSERT}
  {$define HAS_OUT}
{$endif}

{$ifdef VER110}
  {$define DELPHI}  {BCB3}
  {$define HAS_OUT}
{$endif}

{$ifdef VER120}
  {$define DELPHI}  {D4}
  {$define D4PLUS}
{$endif}

{$ifdef VER125}
  {$define DELPHI}  {BCB4}
  {$define D4PLUS}
{$endif}

{$ifdef VER130}
  {$define DELPHI}  {D5}
  {$define D4PLUS}
{$endif}

{$ifdef VER140}
  {$define DELPHI}  {D6}
  {$define D4PLUS}
{$endif}

{$ifdef VER150}
  {$define DELPHI}  {D7}
  {$define D4PLUS}
  {$define HAS_UNSAFE}
  {$define HAS_UINT64}
{$endif}

{$ifdef VER170}
  {$define DELPHI}  {D9}
  {$define D4PLUS}
  {$define HAS_INLINE}
  {$define HAS_UNSAFE}
  {$define HAS_UINT64}
{$endif}

{$ifdef VER180}
  {$define DELPHI}  {D10, D11 ifdef VER185}
  {$define D4PLUS}
  {$define HAS_INLINE}
  {$define HAS_UNSAFE}
  {$define HAS_UINT64}
{$endif}

{$ifdef VER200}
  {$define DELPHI}  {D12}
  {$define D12PLUS}
{$endif}

{$ifdef VER210}
  {$define DELPHI}  {D14}
  {$define D12PLUS}
{$endif}

{$ifdef VER220}
  {$define DELPHI}  {D15 - XE}
  {$define D15PLUS}
{$endif}

{$ifdef VER230}
  {$define DELPHI}  {D16 - XE2}
  {$define D15PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER240}
  {$define DELPHI}  {D17 - XE3}
  {$define D15PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER250}
  {$define DELPHI}  {D18 - XE4}
  {$define D15PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER260}
  {$define DELPHI}  {D19 - XE5}
  {$define D15PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER270}
  {$define DELPHI}  {D20 - XE6}
  {$define D15PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER280}
  {$define DELPHI}  {D21 - XE7}
  {$define D15Plus}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER290}
  {$define DELPHI}  {D22 - XE8}
  {$define D22PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER300}
  {$define DELPHI}  {D23}
  {$define D22PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER310}
  {$define DELPHI}  {D24}
  {$define D22PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef VER320}
  {$define DELPHI}  {D25}
  {$define D22PLUS}
  {$define UNIT_SCOPE}
  {$define HAS_FP_CAST}
{$endif}

{$ifdef VER330}
  {$define DELPHI}  {D25}
  {$define D12PLUS}
  {$define D22PLUS}
  {$define UNIT_SCOPE}
{$endif}

{$ifdef CONDITIONALEXPRESSIONS}  {D6+}
  {$ifndef D4PLUS}
    {$define D4PLUS}
  {$endif}
  {$define HAS_MSG}
  {$define HAS_XTYPES}
  {$ifdef CPUX64}
    {$define BIT64}
  {$endif}
  {$define DELPHI}
{$endif}

{$ifdef D15Plus}
  {$define D12PLUS}
{$endif}

{$ifdef D22Plus}
  {$define D12PLUS}
  {$define D15Plus}
{$endif}

{$ifdef VER70}
  {$ifdef windows}
    {$define WINCRT}
  {$endif}
{$endif}

{$ifdef VirtualPascal}
  {$define G_OPT}
  {$define RESULT}
  {$define LoadArgs}
{$endif}

{$ifdef WIN32}
  {$define J_OPT}
{$endif}

{$ifdef BIT64}
  {$define J_OPT}
{$endif}

{$ifdef FPC}
  {$define FPC_ProcVar}
  {$define ABSTRACT}
  {$define HAS_XTYPES}
  {$define HAS_OVERLOAD}
  {$define HAS_FP_CAST}
  {$undef N_OPT}
  {$ifdef VER1}
    {$undef  J_OPT}
    {$define HAS_INT64}
    {$define HAS_CARD32}
    {$define HAS_MSG}
    {$define HAS_ASSERT}
    {$ifndef VER1_0}
      {FPC 1.9.x}
      {$define StrictLong}
    {$else}
      {$define LoadArgs}
    {$endif}
  {$endif}
  {$ifdef VER2}
    {$define FPC2Plus}
    {$define HAS_ASSERT}
    {$define HAS_INT64}
    {$define HAS_CARD32}
    {$define HAS_MSG}
    {$define HAS_INLINE}   {Remember to use -Si}
    {$define StrictLong}
    {$ifdef FPC_OBJFPC}
      {$define DEFAULT}
    {$endif}
    {$ifdef FPC_DELPHI}
      {$define DEFAULT}
    {$endif}
    {$ifndef VER2_0}
      {$ifndef VER2_1}
        {$define HAS_UINT64}    {2.2+}
      {$endif}
      {$define HAS_DENORM_LIT}  {2.1+}
    {$endif}
    {$ifdef VER2_7_1}
      {$define FPC271or3}
    {$endif}
    {$ifdef VER2_6_2}
      {$define HAS_INTXX}
    {$endif}
    {$ifdef VER2_6_4}
      {$define HAS_INTXX}
      {$define HAS_PINTXX}
    {$endif}
  {$endif}
  {$ifdef VER3}
    {$define FPC2Plus}
    {$define FPC3Plus}
    {$define FPC271or3}
    {$define HAS_ASSERT}
    {$define HAS_INT64}
    {$define HAS_CARD32}
    {$define HAS_MSG}
    {$define HAS_INLINE}
    {$define HAS_UINT64}
    {$define HAS_DENORM_LIT}
    {$define StrictLong}
    {$define HAS_INTXX}
    {$define HAS_PINTXX}
    {$ifdef FPC_OBJFPC}
      {$define DEFAULT}
    {$endif}
    {$ifdef FPC_DELPHI}
      {$define DEFAULT}
    {$endif}
    {$ifdef VER3_2_0}
      {$define FPC32Plus}
    {$endif}
    {$ifdef VER3_3_1}
      {$define FPC32Plus}
    {$endif}
  {$endif}

  {Note: Mode detection does not work for -Sxxx and version < 2.0.2}
  {$ifdef FPC_OBJFPC}
    {$define RESULT}
    {$define HAS_OUT}
  {$endif}
  {$ifdef FPC_DELPHI}
    {$define RESULT}
    {$define HAS_OUT}
    {$undef FPC_ProcVar}
    {$undef HAS_FP_CAST}  {Typecast allowed but buggy}
  {$endif}
  {$ifdef FPC_TP}
    {$undef FPC_ProcVar}
    {$undef HAS_FP_CAST}  {Typecast allowed but buggy}
  {$endif}
  {$ifdef FPC_GPC}
    {$undef FPC_ProcVar}
  {$endif}
  {$ifdef CPU64}
    {$define BIT64}
  {$endif}
  {$ifdef CPUARM}
    {$define EXT64}       {No extended for ARM}
    {$define PurePascal}
  {$endif}
{$endif}

{$ifdef __TMT__}
  {$undef N_OPT}
  {$define RESULT}
  {$define HAS_INT64}
  {$define LoadArgs}
  {$ifdef __WIN32__}
    {$define WIN32}
  {$endif}
{$endif}

{$ifndef BIT16}
  {$define Bit32or64}
  {$ifndef BIT64}
    {$define BIT32}
  {$endif}
{$endif}

{$ifdef BIT16}
  {$ifdef WINDOWS}
    {$define WIN16}
  {$endif}
{$endif}

{$ifdef Delphi}
  {$define RESULT}
  {$define ABSTRACT}
  {$define HAS_DENORM_LIT}
{$endif}

{$ifdef D12Plus}
  {$ifndef D4PLUS}
    {$define D4PLUS}
  {$endif}
  {$define HAS_INLINE}
  {$define HAS_UNSAFE}
  {$define HAS_UINT64}
  {$define HAS_INTXX}
{$endif}

{$ifdef D4Plus}
  {$define HAS_OUT}
  {$define HAS_INT64}
  {$define HAS_CARD32}
  {$define StrictLong}
  {$define HAS_ASSERT}
  {$define DEFAULT}
  {$define HAS_OVERLOAD}
{$endif}

{$ifdef WIN32}
  {$define WIN32or64}
  {$ifndef VirtualPascal}
    {$define APPCONS}
  {$endif}
{$endif}

{$ifdef WIN64}
  {$define BIT64}
  {$define WIN32or64}
  {$define EXT64}
  {$define APPCONS}
{$endif}

{$ifdef BIT64}
  {$undef BASM}
{$endif}


{-- Default options --}

{$ifndef FPC}
  {$B-} {short-circuit boolean expression evaluation, FPC has always B-!}
{$endif}

{$ifdef FPC}
  {$ifdef CPUI386}
    {$ASMmode intel}
  {$endif}
  {$goto on}
{$endif}

{$ifdef VirtualPascal}
  {$ifndef debug}
    {&Optimise+,SmartLink+,Speed+}
  {$endif}
{$endif}

{$ifdef G_OPT}
  {$G+}
{$endif}

{$ifdef Q_OPT}
  {Most Crypto and CRC/Hash units need Q-, define Q+ locally if needed}
  {$Q-}
{$endif}

{$ifdef debug}
  {$R+,S+}  {Note: D9+ needs $R- for StrictLong setting!}
{$else}
  {$R-,S-}
{$endif}

{$ifdef SIMULATE_EXT64}
  {$define EXT64}
{$endif}

{$ifdef BIT16}
  {$F-}
{$endif}

{-- Record the starting values of important local options --}
{$ifopt A+} {$define Align_on}           {$endif}
{$ifopt B+} {$define BoolEval_on}        {$endif}
{$ifopt D+} {$define DebugInfo_on}       {$endif}
{$ifopt I+} {$define IOChecks_on}        {$endif}
{$ifopt R+} {$define RangeChecks_on}     {$endif}
{$ifopt V+} {$define VarStringChecks_on} {$endif}


{$ifdef Q_OPT}
{$ifopt P+} {$define OpenStrings_on}     {$endif}
{$ifopt Q+} {$define OverflowChecks_on}  {$endif}
{$endif}

{-- Note that X option is GLOBAL --}
{$ifdef X_OPT}
{$ifopt X+} {$define ExtendedSyntax_on}  {$endif}
{$ifopt X-} {$undef  RESULT}             {$endif}
{$endif}

{$ifdef CONDITIONALEXPRESSIONS}
  {$warn SYMBOL_PLATFORM OFF}
  {$warn SYMBOL_DEPRECATED OFF}
  {$warn SYMBOL_LIBRARY OFF}
  {$warn UNIT_DEPRECATED OFF}
  {$warn UNIT_LIBRARY OFF}
  {$warn UNIT_PLATFORM OFF}
  {$ifdef HAS_UNSAFE}
    {$warn UNSAFE_TYPE OFF}
    {$warn UNSAFE_CODE OFF}
    {$warn UNSAFE_CAST OFF}
  {$endif}
{$endif}

{$ifdef FPC3Plus}
  {$ifndef debug}
    {These directives are sometimes not working if used immediately    }
    {before the code, so they are used 'globally' if not in debug mode.}
    {Without them e.g. some setlength() will produce a 5089 warning    }
    {For FPC < 3.2 this happens with optimization -O3 or -O4           }
   {$warn 5036 OFF} {Local variable "??" does not seem to be initialized}
   {$warn 5037 OFF} {Variable "??" does not seem to be initialized      }
   {$warn 5089 OFF} {Suppress silly warning: Local variable "??" of a managed type does not seem to be initialized}
  {$endif}
{$endif}

{$else}

  {$ifdef HAS_MSG}
    {$message 'std.inc included more than once'}
  {$endif}

{$endif}

