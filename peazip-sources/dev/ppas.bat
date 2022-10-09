@echo off
SET THEFILE=C:\DragDrop\- src\PeaZip\dev\peazip.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o "C:\DragDrop\- src\PeaZip\dev\peazip.exe" "C:\DragDrop\- src\PeaZip\dev\link11648.res"
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
