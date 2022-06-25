@echo off
Echo Make dual OS version of fca    (c) 2009 W.Ehrhardt

::comment next line if upx not in path, use -3 for upx 3.03+
set pack=upx
set packopt=-3 -qq

::make fca
::compile 16 bit DOS stub
bpc -b -q fca256.pas
if errorlevel 1 goto end

if not (%pack%)==() %pack% %packopt% fca256.exe
move fca256.exe fca16.exe
::compile 32 bit PE
:: fca.def must be in current dir
echo STUB 'fca16.exe' >fca256.def
call vpc -b -q fca256.pas
if not (%pack%)==() %pack% %packopt% fca256.exe
del fca16.exe
del fca256.def

::goto end

fca256 -eax  test1234 dates.pas ##EAX.enc
fca256 -d    test1234 ##EAX.enc ##EAX.out
fca256 -hmac test1234 dates.pas ##HMA.enc
fca256 -d    test1234 ##HMA.enc ##HMA.out

:end
