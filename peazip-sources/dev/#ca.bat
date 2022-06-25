@echo off

set SRC=t_all
::set PARA=-T

set LOG=%SRC%.log

echo Test %SRC% for most console compilers >%LOG%
ver  >>%LOG%

set PCB=bpc -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=bpc -CP -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc1 -B -TGO32V2
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc2 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc22 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc222 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc224 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc240 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc242 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc244 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc260 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc262 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc264d -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc264 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc300 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if errorlevel 1 pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc302 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if errorlevel 1 pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc304 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if errorlevel 1 pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call fpc311 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if errorlevel 1 pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call vpc -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call p5 -b -l
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call p55 -b -l
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=call p6 -b -l
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M2\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M3\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M4\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M5\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M6\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M7\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M9\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

call wdosx %SRC%.exe
echo. >>%LOG%
echo Results for WDOSX >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M10\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M12\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M17\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if errorlevel 1 pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%

set PCB=D:\DMX\M18\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not {%1%}=={} pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%


echo.
echo **** Log file: %LOG%

set PCB=
set SRC=
set LOG=
set PARA=

