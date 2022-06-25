@echo off

set OUT=$$
::set OUT=con

::set PCB=bpc -b
::set PCB=bpc -CP -b
::set PCB=bpc -CW -b
::set PCB=fpc -B -Fe$fpc -TGO32V2 -dRELEASE
::set PCB=fpc -B -Fe$fpc -dRELEASE
::set PCB=vpc -b
::set PCB=D:\DMX\TP6\TPC.EXE -b
::set PCB=D:\DMX\TP5\TPC.EXE -b
::set PCB=D:\DMX\TP55\TPC.EXE -b
::set PCB=D:\DMX\M1\DCC.EXE -b
::set PCB=D:\DMX\M2\DCC32.EXE -b
::set PCB=D:\DMX\M3\DCC32.EXE -b
::set PCB=D:\DMX\M4\DCC32.EXE -b
::set PCB=D:\DMX\M5\DCC32.EXE -b
::set PCB=D:\DMX\M6\DCC32.EXE -b
::set PCB=D:\DMX\M7\DCC32.EXE -b
set PCB=D:\DMX\M9\DCC32.EXE -b
::set PCB=dcc -B

del $fpc >nul

%PCB% t_aes_ws.pas
%PCB% t_aescrp.pas
%PCB% t_aescfb.pas
%PCB% t_aesctr.pas
%PCB% t_aesofb.pas
%PCB% t_aescbc.pas
%PCB% t_aesecb.pas
%PCB% t_cbccts.pas
%PCB% t_ecbcts.pas
%PCB% T_AESTAB.PAS
%PCB% t_decrsp.pas
%PCB% t_encrsp.pas
%PCB% t_cycenc.pas
%PCB% t_cyccnt.pas
%PCB% t_fbmodi.pas

::goto ende

del %OUT% >nul
echo Results for %PCB% >>%OUT%
echo. >>%OUT%


t_aes_ws TEST >>%OUT%
t_aescrp      >>%OUT%
t_aescfb      >>%OUT%
t_aesctr      >>%OUT%
t_aesofb      >>%OUT%
t_aescbc      >>%OUT%
t_aesecb      >>%OUT%
t_cbccts      >>%OUT%
t_ecbcts      >>%OUT%
::t_decrsp      >>%OUT%
::t_encrsp      >>%OUT%
::t_cycenc      >>%OUT%
t_cyccnt      >>%OUT%
t_fbmodi      >>%OUT%

if %OUT%==con goto ende
echo.
echo ---------------------------------------------------------------------
echo ***** Output written to %out% *****

:ende
set PCB=
set OUT=


