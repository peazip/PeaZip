@echo off

rem generic compile batch file for all console compilers
rem pause after each compiler if %1 not empty
rem call self on low env space condition
rem (c) 2003-2005 W.Ehrhardt

rem test file
rem =========

set SRC=t_fio


rem log file (may be con or nul)
rem ============================
::set LOG=nul
set LOG=%SRC%.LOG


rem parameters for test file
rem ========================
set PARA=t_fio.pas

rem build options
rem ========================
::set OPT=


rem test whether enough space in environment
rem ========================================
set PCB=A_rather_long_environment_string_for_testing
if  (%PCB%)==(A_rather_long_environment_string_for_testing) goto OK


rem call self with 4096 byte env
rem ============================
set PCB=
%COMSPEC% /E:4096 /C %0 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto ende


:OK

echo Test %SRC% for all compilers (not BPW, D1) >%LOG%

set PCB=bpc -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=bpc -CP -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=fpc -B -TGO32V2
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=fpc -B
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc22 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc222 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=vpc -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\TP5\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\TP55\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\TP6\TPC.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M2\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M3\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M4\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M5\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M6\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%


set PCB=D:\DMX\M7\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M9\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

call wdosx %SRC%.exe
echo. >>%LOG%
echo Results for WDOSX >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M10\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %OPT% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

echo.
echo **** Log file: %LOG%

:ende

set PCB=
set SRC=
set LOG=
set PARA=
set OPT=

