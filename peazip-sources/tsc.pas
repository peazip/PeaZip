unit TSC;

{Access to Time Stamp Counter (RDTSC)}

interface

{$i STD.INC}

(*************************************************************************

 DESCRIPTION     :  Access to Time Stamp Counter (RDTSC)
                    General counter: TSC if present and _HasRDTSC is true,
                    GetTickCount or SysTick otherwise for Windows/DOS,
                    milliseconds since midnight on non-Windows 32 bit systems.


 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP, WDOSX

 EXTERNAL DATA   :  _tsc.obj for 16 bit compilers

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REMARK          :  CPUID/RDTSC assumed to be supported for BIT64

 REFERENCES      :  ---


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     06.12.03  W.Ehrhardt  Initial version from old hrtimer
 0.20     06.12.03  we          with _HasRDTSC: boolean
 0.30     06.12.03  we          with _ReadCounter
 0.31     06.12.03  we          $J+ fÅr D4Plus
 0.32     17.12.03  we          interface _check.. routines
 0.33     02.01.04  we          Second cpuid after RDTSC
 0.34     11.04.04  we          Delphi 7
 0.35     28.03.08  we          _ReadCounter via sysutils.time on non-Windows 32 bit systems
 0.36     15.01.12  we          _RDTSC for BIT64, assumes cpuid and rdtsc available
 0.37     19.01.12  we          _ReadCounter for BIT64
 0.38     18.03.12  we          BIT64: _RDTSC uses new assembler function TSC64
 0.39     17.12.12  we          D17 adjustment
 0.40     20.03.13  we          FPC64: {$asmmode intel} for function TSC64
 0.41     28.09.13  we          BASM16: _CheckCPUID, _CheckRDTSC, _RDTSC
 0.42     17.03.17  we          Simple routines for FPC/CPUARM
 0.43     30.03.17  we          FPC/CPUARM uses fpgettimeofday
 0.44     11.11.17  we          Use time for FPC/WINCE
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2002-2017 Wolfgang Ehrhardt

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
  TCtrRec = packed record
              LDW, HDW: longint;
            end;                  {64 bit TSC counter record}

var
  _HasRDTSC: boolean;  {will be true if RDTSC instruction is supported}

procedure _ReadTSC(var Ctr: TCtrRec);
  {-Read Time Stamp Counter}

procedure _ReadCounter(var Ctr: TCtrRec);
  {-Read TSC if present, else GetTickCount/SysTick/milliseconds since midnight}

function  _CheckCPUID: boolean;
  {-check if CPUID supported}

function  _CheckRDTSC: boolean;
  {-check if RDTSC supported, check CPUID first!!}


implementation

{$ifdef CPUARM}

{$ifdef WINCE}

  uses
    sysutils;

  {---------------------------------------------------------------------------}
  procedure _ReadTSC(var Ctr: TCtrRec);
    {-Read Time Stamp Counter}
  begin
    _ReadCounter(Ctr);
  end;

  {---------------------------------------------------------------------------}
  procedure _ReadCounter(var Ctr: TCtrRec);
    {-Use milliseconds since midnight}
  begin
    Ctr.LDW := round(86400000*time);
    Ctr.HDW := 0;
  end;

  {---------------------------------------------------------------------------}
  function  _CheckCPUID: boolean;
    {-check if CPUID supported}
  begin
    _CheckCPUID := false;
  end;

  {---------------------------------------------------------------------------}
  function  _CheckRDTSC: boolean;
    {-check if RDTSC supported, check CPUID first!!}
  begin
    _CheckRDTSC := false;
  end;

{$else}
  {Simple routines for ARM, assuming that TSC is not available}
  {and that unit unix is available}

  uses
    sysutils,unix;


  {---------------------------------------------------------------------------}
  procedure _ReadTSC(var Ctr: TCtrRec);
    {-Read Time Stamp Counter}
  begin
    _ReadCounter(Ctr);
  end;

  {---------------------------------------------------------------------------}
  procedure _ReadCounter(var Ctr: TCtrRec);
    {-Read TSC if present, else GetTickCount/SysTick/milliseconds since midnight}
  var
    t: timeval;
  begin
    fpgettimeofday(@t,nil);
    {Build a 64 bit microsecond tick from the seconds and microsecond longints}
    int64(Ctr) := (int64(t.tv_sec) * 1000000) + t.tv_usec;
  end;

  {---------------------------------------------------------------------------}
  function  _CheckCPUID: boolean;
    {-check if CPUID supported}
  begin
    _CheckCPUID := false;
  end;

  {---------------------------------------------------------------------------}
  function  _CheckRDTSC: boolean;
    {-check if RDTSC supported, check CPUID first!!}
  begin
    _CheckRDTSC := false;
  end;
{$endif}


{$else}


{$ifdef BIT32}
  {$ifdef WIN32}
    {$ifdef UNIT_SCOPE}
      uses winapi.windows;
    {$else}
      uses windows;
    {$endif}
  {$else}
    {$ifdef UNIT_SCOPE}
      uses winapi.windows;  {Delphi 64}
    {$else}
      uses sysutils;        {FPC 64}
    {$endif}
  {$endif}
{$endif}


{$ifdef BIT32or64}

{$ifdef BIT64}

{---------------------------------------------------------------------------}
function _CheckCPUID: boolean;
  {-CPUID assumed to be supported for BIT64}
begin
  _CheckCPUID := true;
end;

{---------------------------------------------------------------------------}
function _CheckRDTSC: boolean;
  {-RDTSC assumed to be supported for BIT64}
begin
  _CheckRDTSC := true;
end;

{$ifdef FPC}
{$asmmode intel}
{$endif}

{---------------------------------------------------------------------------}
function TSC64: int64; assembler;
  {-Read Time Stamp Counter as int64}
asm
  push  rbx
  xor   eax,eax
  xor   ebx,ebx
  xor   ecx,ecx
  xor   edx,edx
  cpuid            {serialize}
  rdtsc
  sub   rcx,rcx
  mov   ecx,eax
  shl   rdx,32
  or    rdx,rcx
  push  rdx
  xor   eax,eax
  cpuid            {serialize}
  pop   rax        {return value}
  pop   rbx
end;


{---------------------------------------------------------------------------}
procedure _RDTSC(var Ctr: TCtrRec);
  {-Read Time Stamp Counter}
begin
  int64(Ctr) := TSC64;
end;

{$else}

{---------------------------------------------------------------------------}
function _CheckCPUID: boolean; assembler;
  {-check if CPUID supported}
asm
  pushfd
  pushfd
  pop      eax
  mov      ecx,eax
  xor      eax,$200000
  push     eax
  popfd
  pushfd
  pop      eax
  popfd
  xor      eax,ecx
  setnz    al
end;


{---------------------------------------------------------------------------}
function _CheckRDTSC: boolean; assembler;
  {-check if RDTSC supported, check CPUID first!!}
asm
  push   ebx
  mov    eax,1
  db     $0f,$a2          {cpuid}
  test   dx,$10           {test RDTSC flag in Features}
  setnz  al
  pop    ebx
end;


{---------------------------------------------------------------------------}
procedure _RDTSC(var Ctr: TCtrRec);
  {-Read Time Stamp Counter}
begin
  asm
    push  ebx
    xor   eax, eax
    xor   ebx, ebx
    xor   ecx, ecx
    xor   edx, edx
    db    $0f,$a2    {cpuid}
    db    $0f,$31    {rdtsc}
    mov   ecx,[Ctr]
    mov   [ecx],eax
    mov   [ecx+4],edx
    xor   eax, eax
    db    $0f,$a2    {cpuid}
    pop   ebx
  end;
end;
{$endif}

{$else}

{16 bit compiler}

{$ifdef BASM16}
{$f+}
{---------------------------------------------------------------------------}
function _CheckCPUID: boolean; assembler;
  {-check if CPUID supported}
asm
  db $66;  pushf                      {pushfd              }
  db $66;  pushf                      {pushfd              }
  db $66;  pop    ax                  {pop      eax        }
  db $66;  mov    cx,ax               {mov      ecx,eax    }
  db $66;  db     $35,$00,$00,$20,$00 {xor      eax,$200000}
  db $66;  push   ax                  {push     eax        }
  db $66;  popf                       {popfd               }
  db $66;  pushf                      {pushfd              }
  db $66;  pop    ax                  {pop      eax        }
  db $66;  popf                       {popfd               }
  db $66;  xor    ax,cx               {xor      eax,ecx    }
           db     $0F,$95,$C0         {setnz    al         }
end;


{---------------------------------------------------------------------------}
function _CheckRDTSC: boolean; assembler;
  {-check if RDTSC supported, check CPUID first!!}
asm
  db $66; push   bx
  db $66; xor    ax,ax            {set eax=1}
          inc    ax
          db     $0f,$a2          {cpuid}
          test   dx,$10           {test RDTSC flag in Features}
          db     $0F,$95,$C0      {setnz  al}
  db $66; pop    bx
end;


{---------------------------------------------------------------------------}
procedure _RDTSC(var Ctr: TCtrRec);
  {-Read Time Stamp Counter}
begin
  asm
    db $66; xor   ax, ax
    db $66; xor   bx, bx
    db $66; xor   cx, cx
    db $66; xor   dx, dx
            db    $0f,$a2    {cpuid}
            db    $0f,$31    {rdtsc}
            les   di,[Ctr]
    db $66; mov	  es:[di],ax
    db $66; mov	  es:[di+4],dx
    db $66; xor   ax, ax
            db    $0f,$a2    {cpuid}
  end;
end;

{$else}
  {$f+}
  function  _CheckCPUID: boolean; external;
    {-check if CPUID supported}

  function  _CheckRDTSC: boolean; external;
    {-check if RDTSC supported, check CPUID first!!}

  procedure _RDTSC(var Ctr: TCtrRec); external;
    {-Read Time Stamp Counter}
  {$l _tsc}
{$endif}


{$endif}


{---------------------------------------------------------------------------}
procedure _ReadTSC(var Ctr: TCtrRec);
  {-Read Time Stamp Counter}
begin
  if _HasRDTSC then _RDTSC(Ctr) else fillchar(Ctr, sizeof(Ctr),0);
end;


{---------------------------------------------------------------------------}
procedure _ReadCounter(var Ctr: TCtrRec);
  {-Read TSC if present, else GetTickCount/SysTick/milliseconds since midnight}
begin
  {$ifdef BIT64}
    _RDTSC(Ctr);
  {$else}
    if _HasRDTSC then _RDTSC(Ctr)
    else begin
      Ctr.HDW := 0;
      {$ifdef WIN32}
        Ctr.LDW := GetTickCount;
      {$else}
        {$ifdef BIT16}
          {$ifdef DPMI}
            Ctr.LDW := MemL[Seg0040:$6c];
          {$else}
            Ctr.LDW := MemL[$40:$6c];
          {$endif}
        {$else}
           {Return milliseconds since midnight on non-Windows 32Bit systems }
           {Thanks to Giorgio Tani for reporting the problem that MemL is   }
           {not defined on BSD and Linux systems with FPC/Lazarus. Note that}
           {FPC for DOS target now also uses this conditional branch.       }
           {$ifdef debug}
             {$ifdef HAS_MSG}
               {$message '_ReadCounter via sysutils.time'}
             {$endif}
           {$endif}
           Ctr.LDW := round(86400000*time);
        {$endif}
      {$endif}
    end;
  {$endif}
end;

{$endif}

begin
  if _CheckCPUID then _HasRDTSC := _CheckRDTSC
  else _HasRDTSC := false;
end.
