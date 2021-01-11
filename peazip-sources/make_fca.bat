@echo off
Echo Make dual OS version of fca    (c) 2009 W.Ehrhardt

::comment next line if upx not in path, use -3 for upx 3.03+
set pack=upx
set packopt=-3 -qq

::make fca
::compile 16 bit DOS stub
call bpc -b -q fca
if errorlevel 1 goto end
if not (%pack%)==() %pack% %packopt% fca.exe
move fca.exe fca16.exe

::compile 32 bit PE
:: fca.def must be in current dir
echo STUB 'fca16.exe' >fca.def
call vpc -b -q fca.pas
if not (%pack%)==() %pack% %packopt% fca.exe
del fca16.exe
del fca.def

::goto end

fca -eax  test1234 dates.pas ##EAX.enc
fca -d    test1234 ##EAX.enc ##EAX.out
fca -hmac test1234 dates.pas ##HMA.enc
fca -d    test1234 ##HMA.enc ##HMA.out

:end
