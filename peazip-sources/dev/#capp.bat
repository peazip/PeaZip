@echo off

::set SRC=t_1mio_a
set SRC=t_all
::set SRC=t_all_xl
::set SRC=t_hmac
::set SRC=t_kdf
::set SRC=t_hmacob
::set SRC=t_cmodel
::set SRC=t_hmac2
::set SRC=t_ed2kv
::set SRC=t_oid

set LOG=%SRC%.log
if not (%1%)==() pause

echo Test %SRC% for all compilers (with int64) >%LOG%
ver  >>%LOG%

set PCB=call fpc1 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc2 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc22 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc222 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc224 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc240 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc242 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc244 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc260 -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=call fpc260d -B
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M4\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M5\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M6\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M7\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M9\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

call wdosx %SRC%.exe
echo. >>%LOG%
echo Results for WDOSX >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M10\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe >>%LOG%

set PCB=D:\DMX\M12\DCC32.EXE -b
del %SRC%.exe >nul
%PCB% %SRC%.pas
if not (%1%)==() pause
echo. >>%LOG%
echo Results for %PCB% >>%LOG%
%SRC%.exe %PARA% >>%LOG%


echo.
echo **** Log file: %LOG%

set PCB=
set SRC=
set LOG=

