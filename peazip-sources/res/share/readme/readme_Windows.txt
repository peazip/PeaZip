WHAT IS PEAZIP

PeaZip is a free, Open Source file and archive manager.
Create 7Z, ARC, BR, BZ2, GZ, *PAQ, PEA, TAR, UPX, WIM, ZIP, ZST...
Extract over 200 file formats as ACE, CAB, DMG, ISO, RAR, UDF, ZIPX...

https://peazip.github.io

PeaZip allows to create, convert and extract multiple archives at once, create self-extracting archives, bookmark archives and folders, apply powerful search filters, scan and open files with custom applications, apply strong encryption, split/join files, secure data deletion, checksum and hash.


LANGUAGE

To change application's language, use Options > Localization.


CUSTOMIZATION, SYSTEM INTEGRATION AND SCRIPTING

PeaZip executables doesn't need installation and can run from any location, a writeable path is strongly recommended to allow update of configuration and of persistent randomness collector.
For better system integration please read "Customisation and scripting" chapter in documentation.
It is possible to invoke most used PeaZip's functions from basically any mean capable of passing parameters as explained in application's documentation, like:
- registry entries, i.e. for integration with context menu and the new Windows 11 mini-context menu
- links, i.e. in SendTo menu
- scripts
The application is usually installed in C:\Program Files\PeaZip folder.
Scription and system integration examples are available in (peazip)\res\share\batch folder.

Due to it's frontend/backend architecture, backend executables can be freely replaced when a newer version is available (i.e. 64 bit version) as long as the new version supports the known syntax

Status of porting of single components to 64-bit in WIN64 packages:
peazip			64 bit
pea			64 bit
dragdropfilesdll.dll	64 bit
7z			64 bit
brotli			64 bit
zstd			64 bit
arc			32 bit
lpaq			32 bit
paq			32 bit
zpaq			64 bit
quad			32 bit
balz			32 bit
bcm			64 bit
unace			32 bit
upx			64 bit