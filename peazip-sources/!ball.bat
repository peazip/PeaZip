@echo off

echo Test T_ALL for all compilers (not BPW, D1) >T_ALL.LOG

set PCB=bpc -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=bpc -CP -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=fpc -B -TGO32V2
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=fpc -B
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=vpc -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\TP5\TPC.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\TP55\TPC.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\TP6\TPC.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M2\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M3\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M4\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M5\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M6\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M7\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M9\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG

set PCB=D:\DMX\M10\DCC32.EXE -b
del t_all.exe >nul
%PCB% t_all.pas
echo. >>T_ALL.LOG
echo Results for %PCB% >>T_ALL.LOG
t_all.exe >>T_ALL.LOG


set PCB=
