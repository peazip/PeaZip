@echo off
echo Test t_allxl for all WIN32 compilers >t_allxl.log

set PCB=fpc -B
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=vpc -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M2\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M3\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M4\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M5\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M6\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M7\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M9\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=D:\DMX\M10\DCC32.EXE -b
del t_allxl.exe >nul
%PCB% t_allxl.pas
echo. >>t_allxl.log
echo Results for %PCB% >>t_allxl.log
t_allxl.exe >>t_allxl.log

set PCB=
