@echo off

set LOG=util_d10x32.log

ver >%LOG%ver >%LOG%

echo. >>%LOG%
echo ==================================== >>%LOG%
echo ======= Compiler: D25 / 32-bit >>%LOG%


rem -------------------------------

set CC=D:\DMX\M25\DCC32.EXE -b
%CC% --version >>%LOG%
echo. >>%LOG%

set SRC=t_hrt
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_bitar3
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_btypes
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_base2n
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_bas2na
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_base64
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_cv
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_dates
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_sort
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_mstat
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%


rem -------------------------------
set CC=D:\DMX\M26\DCC32.EXE -b

echo. >>%LOG%
echo ==================================== >>%LOG%
echo ======= Compiler: D26 / 32-bit >>%LOG%

set CC=D:\DMX\M26\DCC32.EXE -b
%CC% --version >>%LOG%
echo. >>%LOG%


set SRC=t_hrt
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_bitar3
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_btypes
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_base2n
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_bas2na
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_base64
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_cv
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_dates
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_sort
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set SRC=t_mstat
%CC% %SRC%.pas
if errorlevel 1 pause
%SRC%.exe   >>%LOG%

set CC=
set SRC=
