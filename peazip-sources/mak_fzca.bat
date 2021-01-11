@echo off
Echo Make dual OS version of fzca    (c) 2009 W.Ehrhardt

::BUILDFIXED will result in less than 2K smaller combined exe
::set dyntable=-dBUILDFIXED

::comment next line if upx not in path, use -3 for upx 3.03+
set pack=upx
set packopt=-3 -qq

::make fzca
::compile 16 bit DOS stub
bpc -b -q %dyntable% fzca
if errorlevel 1 goto end

if not (%pack%)==() %pack% %packopt% fzca.exe
move fzca.exe fzca16.exe
::compile 32 bit PE
:: fzca.def must be in current dir
echo STUB 'fzca16.exe' >fzca.def
call vpc -b -q  %dyntable%  fzca.pas
if not (%pack%)==() %pack% %packopt% fzca.exe
del fzca16.exe
del fzca.def

::goto end

fzca -eax  test1234 whirl512.pas ##EAX.enc
fzca -d    test1234 ##EAX.enc    ##EAX.out
fzca -hmac test1234 whirl512.pas ##HMA.enc -z
fzca -d    test1234 ##HMA.enc    ##HMA.out

:end
