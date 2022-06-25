unit CompVers;


{Return compiler version as string or symbol}


interface


(*************************************************************************

 DESCRIPTION     :  Return compiler version as string or symbol

 REQUIREMENTS    :  TP4-7, D1-D7/D9-D12/D17-D26, FPC1/2/3, VP, (and others)

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  D8, D11, D14, D15, D16, D19-D24, BCB5 are untested


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.05.05  W.Ehrhardt  Initial version
 0.11     27.05.05  we          FPC 2.0
 0.12     21.11.05  we          BCB3/4, Fix BP7-DMPI
 0.13     22.03.06  we          D10 aka D2006
 0.14     13.04.06  we          BCB5, BCB6, D8, FPC202
 0.15     05.11.06  we          FPC204
 0.15     05.11.06  we          FPC204
 0.16     25.05.07  we          D11 aka Delphi 2007, FPC2.1.4
 0.17     12.09.07  we          FPC 2.2.0
 0.18     19.06.08  we          FPC 2.2.2
 0.19     04.10.08  we          D12 aka D2009, TCompString
 0.20     22.03.09  we          FPC 2.2.4
 0.21     15.12.09  we          D14, FPC 2.4.0
 0.22     19.11.10  we          FPC 2.4.2
 0.23     24.05.11  we          FPC 2.4.4
 0.24     01.01.12  we          FPC 2.6.0
 0.25     25.12.12  we          D15, D16, D17, FPC 2.6.2
 0.26     14.05.13  we          D18
 0.27     28.09.13  we          D19
 0.28     04.01.14  we          FPC 2.6.4
 0.29     17.04.14  we          D20
 0.30     13.09.14  we          D21
 0.31     22.10.14  we          Add ?? for unknown FPC
 0.32     17.01.15  we          FPC 2.7.1, FPC 3.0.1, FPC 3.1.1
 0.33     25.04.15  we          D22
 0.34     25.08.15  we          FPC 3.0.0
 0.35     01.09.15  we          D23
 0.36     26.04.16  we          D24
 0.37     11.04.17  we          FPC 3.0.2, D25
 0.38     11.04.17  we          FPC 3.0.4
 0.39     02.10.18  we          FPC 3.2.0, 3.3.1
 0.40     13.10.18  we          Removed conditional define 'unknown'
 0.41     22.11.18  we          D26
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2005-2018 Wolfgang Ehrhardt

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

type
  TCompiler = ( _Unknown,
                _TP4, _TP5, _TP55, _TP6, _BP7, _BP7DPMI, _BP7WIN, _TPW10, _TPW15,
                _D1, _D2, _D3, _D4, _D5, _D6, _D7, _D9,
                _BCB3, _BCB4,
                _VP21, _FPC, _FPC10, _FPC19, _FPC20, _FPC202,
                _D8, _D10, _D11,
                _BCB5, _BCB6, _BCB7,
                _FPC204, _FPC214, _FPC220, _FPC222,
                _D12,
                _FPC224,
                _D14,
                _FPC240, _FPC242, _FPC244, _FPC260, _FPC262,
                _D15, _D16, _D17, _D18, _D19,
                _FPC264,
                _D20,_D21,
                _FPC271, _FPC301, _FPC311,
                _D22, _FPC300, _D23, _D24, _FPC302, _D25, _FPC304,
                _FPC320, _FPC331, _D26
              );

type
  TCompString = string[15];

function Compiler_Sym: TCompiler;
  {-Return compiler version as symbol}

function Compiler_Str: TCompString;
  {-Return compiler version as string}


implementation


{$undef nowarn}

{$ifdef FPC}
  {$define nowarn}
{$endif}
{$ifdef CONDITIONALEXPRESSIONS} {D6+}
  {$define nowarn}
{$endif}
{$ifdef WIN32}
  {$ifndef VirtualPascal}
    {$define nowarn}
  {$endif}
{$endif}

{$ifdef nowarn}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$endif}


{---------------------------------------------------------------------------}
function Compiler_Sym: TCompiler;
  {-Return compiler version as symbol}
begin
  {$ifdef FPC}
    Compiler_Sym := _FPC;
  {$else}
    Compiler_Sym := _Unknown;
  {$endif}

  {$ifdef VER10}
    Compiler_Sym := _TPW10;
  {$endif}

  {$ifdef VER15}
    Compiler_Sym := _TPW15;
  {$endif}

  {$ifdef VER40}
    Compiler_Sym := _TP4;
  {$endif}

  {$ifdef VER50}
    Compiler_Sym := _TP5;
  {$endif}

  {$ifdef VER55}
    Compiler_Sym := _TP55;
  {$endif}

  {$ifdef VER60}
    Compiler_Sym := _TP6;
  {$endif}

  {$ifdef VER70}
    {$ifdef windows}
      Compiler_Sym := _BP7WIN;
    {$else}
      {$ifdef DPMI}
        Compiler_Sym := _BP7DPMI;
      {$else}
        Compiler_Sym := _BP7;
      {$endif}
    {$endif}
  {$endif}

  {$ifdef VER80}
    Compiler_Sym := _D1;
  {$endif}

  {$ifdef VER90}
    Compiler_Sym := _D2;
  {$endif}

  {$ifdef VER100}
    Compiler_Sym := _D3;
  {$endif}

  {$ifdef VER110}
    Compiler_Sym := _BCB3;
  {$endif}

  {$ifdef VER120}
    Compiler_Sym := _D4;
  {$endif}

  {$ifdef VER125}
    Compiler_Sym := _BCB4;
  {$endif}

  {$ifdef VER130}
    {$ifdef BCB}
      Compiler_Sym := _BCB5;
    {$else}
      Compiler_Sym := _D5;
    {$endif}
  {$endif}

  {$ifdef VER140}
    {$ifdef BCB}
      Compiler_Sym := _BCB6;
    {$else}
      Compiler_Sym := _D6;
    {$endif}
  {$endif}

  {$ifdef VER150}
    Compiler_Sym := _D7;
  {$endif}

  {$ifdef VER160}
    Compiler_Sym := _D8;
  {$endif}

  {$ifdef VER170}
    Compiler_Sym := _D9;
  {$endif}

  {$ifdef Ver180}
    {$ifdef Ver185}
      Compiler_Sym := _D11;
    {$else}
      Compiler_Sym := _D10;
    {$endif}
  {$endif}

  {$ifdef Ver190}
    Compiler_Sym := _D11;  {D11/2007 for .NET}
  {$endif}

  {$ifdef Ver200}
    Compiler_Sym := _D12;
  {$endif}

  {$ifdef Ver210}
    Compiler_Sym := _D14;
  {$endif}

  {$ifdef Ver220}
    Compiler_Sym := _D15;
  {$endif}

  {$ifdef Ver230}
    Compiler_Sym := _D16;
  {$endif}

  {$ifdef Ver240}
    Compiler_Sym := _D17;
  {$endif}

  {$ifdef Ver250}
    Compiler_Sym := _D18;
  {$endif}

  {$ifdef Ver260}
    Compiler_Sym := _D19;
  {$endif}

  {$ifdef Ver270}
    Compiler_Sym := _D20;
  {$endif}

  {$ifdef Ver280}
    Compiler_Sym := _D21;
  {$endif}

  {$ifdef Ver290}
    Compiler_Sym := _D22;
  {$endif}

  {$ifdef Ver300}
    Compiler_Sym := _D23;
  {$endif}

  {$ifdef Ver310}
    Compiler_Sym := _D24;
  {$endif}

  {$ifdef Ver320}
    Compiler_Sym := _D25;
  {$endif}

  {$ifdef Ver330}
    Compiler_Sym := _D26;
  {$endif}

  {$ifdef VirtualPascal}
    Compiler_Sym := _VP21;
  {$endif}

  {$ifdef FPC}
    {$ifdef VER1}
      {$ifndef VER1_0}
        Compiler_Sym := _FPC19;
      {$else}
        Compiler_Sym := _FPC10;
      {$endif}
    {$else}
      {$ifdef VER2}
        {$ifdef VER2_0_2}
          Compiler_Sym := _FPC202;
        {$endif}
        {$ifdef VER2_0_4}
          Compiler_Sym := _FPC204;
        {$endif}
        {$ifdef VER2_1_4}
          Compiler_Sym := _FPC214;
        {$endif}
        {$ifdef VER2_2_0}
          Compiler_Sym := _FPC220;
        {$endif}
        {$ifdef VER2_2_2}
          Compiler_Sym := _FPC222;
        {$endif}
        {$ifdef VER2_2_4}
          Compiler_Sym := _FPC224;
        {$endif}
        {$ifdef VER2_4_0}
          Compiler_Sym := _FPC240;
        {$endif}
        {$ifdef VER2_4_2}
          Compiler_Sym := _FPC242;
        {$endif}
        {$ifdef VER2_4_4}
          Compiler_Sym := _FPC244;
        {$endif}
        {$ifdef VER2_6_0}
          Compiler_Sym := _FPC260;
        {$endif}
        {$ifdef VER2_6_2}
          Compiler_Sym := _FPC262;
        {$endif}
        {$ifdef VER2_6_4}
          Compiler_Sym := _FPC264;
        {$endif}
        {$ifdef VER2_7_1}
          Compiler_Sym := _FPC271;
        {$endif}
      {$else}
        {$ifdef VER3}
          {$ifdef VER3_0_0}
            Compiler_Sym := _FPC300;
          {$endif}
          {$ifdef VER3_0_1}
            Compiler_Sym := _FPC301;
          {$endif}
          {$ifdef VER3_0_2}
            Compiler_Sym := _FPC302;
          {$endif}
          {$ifdef VER3_0_4}
            Compiler_Sym := _FPC304;
          {$endif}
          {$ifdef VER3_1_1}
            Compiler_Sym := _FPC311;
          {$endif}
          {$ifdef VER3_2_0}
            Compiler_Sym := _FPC320;
          {$endif}
          {$ifdef VER3_3_1}
            Compiler_Sym := _FPC331;
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
end;


{---------------------------------------------------------------------------}
function Compiler_Str: TCompString;
  {-Return compiler version as string}
begin
  {$ifdef FPC}
    Compiler_Str := 'FPC??';
  {$else}
    Compiler_Str := '(unknown)';
  {$endif}

  {$ifdef VER10}
    Compiler_Str := 'TPW10';
  {$endif}

  {$ifdef VER15}
    Compiler_Str := 'TPW15';
  {$endif}

  {$ifdef VER40}
    Compiler_Str := 'TP4';
  {$endif}

  {$ifdef VER50}
    Compiler_Str := 'TP5';
  {$endif}

  {$ifdef VER55}
    Compiler_Str := 'TP55';
  {$endif}

  {$ifdef VER60}
    Compiler_Str := 'TP6';
  {$endif}

  {$ifdef VER70}
    {$ifdef windows}
      Compiler_Str := 'BP7WIN';
    {$else}
      {$ifdef DPMI}
        Compiler_Str := 'BP7DMPI';
      {$else}
        Compiler_Str := 'BP70';
      {$endif}
    {$endif}
  {$endif}

  {$ifdef VER80}
    Compiler_Str := 'D1';
  {$endif}

  {$ifdef VER90}
    Compiler_Str := 'D2';
  {$endif}

  {$ifdef VER100}
    Compiler_Str := 'D3';
  {$endif}

  {$ifdef VER110}
    Compiler_Str := 'BCB3';
  {$endif}

  {$ifdef VER120}
    Compiler_Str := 'D4';
  {$endif}

  {$ifdef VER125}
    Compiler_Str := 'BCB4';
  {$endif}

  {$ifdef VER130}
    {$ifdef BCB}
      Compiler_Str := 'BCB5';
    {$else}
      Compiler_Str := 'D5';
    {$endif}
  {$endif}

  {$ifdef VER140}
    {$ifdef BCB}
      Compiler_Str := 'BCB6';
    {$else}
      Compiler_Str := 'D6';
    {$endif}
  {$endif}

  {$ifdef VER150}
    Compiler_Str := 'D7';
  {$endif}

  {$ifdef VER160}
    Compiler_Str := 'D8';
  {$endif}

  {$ifdef VER170}
    Compiler_Str := 'D9';      {2005}
  {$endif}

  {$ifdef Ver180}
    {$ifdef Ver185}
      Compiler_Str := 'D11';   {2007.Win32}
    {$else}
      Compiler_Str := 'D10';   {BDS2006}
    {$endif}
  {$endif}

  {$ifdef VER190}
    Compiler_Str := 'D11';     {2007.NET}
  {$endif}

  {$ifdef VER200}
    Compiler_Str := 'D12';     {2009}
  {$endif}

  {$ifdef VER210}
    Compiler_Str := 'D14';     {2010}
  {$endif}

  {$ifdef Ver220}
    Compiler_Str := 'D15';     {XE}
  {$endif}

  {$ifdef Ver230}
    Compiler_Str := 'D16';     {XE2}
  {$endif}

  {$ifdef Ver240}
    Compiler_Str := 'D17';     {XE3}
  {$endif}

  {$ifdef Ver250}
    Compiler_Str := 'D18';     {XE4}
  {$endif}

  {$ifdef Ver260}
    Compiler_Str := 'D19';     {XE5}
  {$endif}

  {$ifdef Ver270}
    Compiler_Str := 'D20';     {XE6}
  {$endif}

  {$ifdef Ver280}
    Compiler_Str := 'D21';     {XE7}
  {$endif}

  {$ifdef Ver290}
    Compiler_Str := 'D22';     {XE8}
  {$endif}

  {$ifdef Ver300}
    Compiler_Str := 'D23';     {'Seattle'}
  {$endif}

  {$ifdef Ver310}
    Compiler_Str := 'D24';     {'Berlin'}
  {$endif}

  {$ifdef Ver320}
    Compiler_Str := 'D25';     {'Tokyo'}
  {$endif}

  {$ifdef Ver330}
    Compiler_Str := 'D26';     {'Rio'}
  {$endif}

  {$ifdef VirtualPascal}
    Compiler_Str := 'VP21';
  {$endif}

  {$ifdef FPC}
    {$ifdef VER1}
      {$ifndef VER1_0}
         Compiler_Str := 'FPC19x';
      {$else}
         Compiler_Str := 'FPC10';
      {$endif}
    {$else}
      {$ifdef VER2}
        {$ifdef VER2_0_2}
          Compiler_Str := 'FPC202';
        {$endif}
        {$ifdef VER2_0_4}
          Compiler_Str := 'FPC204';
        {$endif}
        {$ifdef VER2_1_4}
          Compiler_Str := 'FPC214';
        {$endif}
        {$ifdef VER2_2_0}
          Compiler_Str := 'FPC220';
        {$endif}
        {$ifdef VER2_2_2}
          Compiler_Str := 'FPC222';
        {$endif}
        {$ifdef VER2_2_4}
          Compiler_Str := 'FPC224';
        {$endif}
        {$ifdef VER2_4_0}
          Compiler_Str := 'FPC240';
        {$endif}
        {$ifdef VER2_4_2}
          Compiler_Str := 'FPC242';
        {$endif}
        {$ifdef VER2_4_4}
          Compiler_Str := 'FPC244';
        {$endif}
        {$ifdef VER2_6_0}
          Compiler_Str := 'FPC260';
        {$endif}
        {$ifdef VER2_6_2}
          Compiler_Str := 'FPC262';
        {$endif}
        {$ifdef VER2_6_4}
          Compiler_Str := 'FPC264';
        {$endif}
        {$ifdef VER2_7_1}
          Compiler_Str := 'FPC271';
        {$endif}
      {$else}
        {$ifdef VER3}
          {$ifdef VER3_0_0}
            Compiler_Str := 'FPC300';
          {$endif}
          {$ifdef VER3_0_1}
            Compiler_Str := 'FPC301';
          {$endif}
          {$ifdef VER3_0_2}
            Compiler_Str := 'FPC302';
          {$endif}
          {$ifdef VER3_0_4}
            Compiler_Str := 'FPC304';
          {$endif}
          {$ifdef VER3_1_1}
            Compiler_Str := 'FPC311';
          {$endif}
          {$ifdef VER3_2_0}
            Compiler_Str := 'FPC320';
          {$endif}
          {$ifdef VER3_3_1}
            Compiler_Str := 'FPC331';
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
end;

end.


