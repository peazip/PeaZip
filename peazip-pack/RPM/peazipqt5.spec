Name: peazip
Summary: PeaZip file manager and archiver (Qt5)
Version: 8.3.0.LINUX.Qt5
Release: 1
License: LGPLv3
URL: https://peazip.github.io
Group: Archiving
Packager: Giorgio Tani
BuildRoot: /home/user/rpmbuild/BUILDROOT/%{name}-%{version}

%description
PeaZip (Qt5) is a cross-platform portable file and archive manager, released under LGPLv3.

Create 7z, ARC, Brotli, BZ2, GZ, *PEA, split, sfx, TAR, UPX, WIM, XZ, ZIP, ZPAQ, Zstandard

Open over 200 formats: ACE, ARJ, CAB, COMPOUND (MSI, DOC, XLS, PPT), CPIO, ISO, Java (JAR, EAR, WAR), Linux (DEB, PET/PUP, RPM, SLP), LHA/LZH, LZMA, MSIX, NSIS, OOo, RAR / RAR5, U3P, WIM, XPI, Z, ZIPX ...

PeaZip create, convert and extract multiple archives at once, edit existing archives, create self-extracting archives. As browser, can bookmark archives and folders, apply powerful multiple search filters to archive's content. As archive manager it can export job definition as command line, save archive's layouts, use custom compressors and extractors.
 
Other features: strong encryption, encrypted password manager, batch file rename, split/join files (file span), secure data deletion, compare, checksum and hash files, system benchmark, generate random passwords and keyfiles.

%files
%defattr(-,root,root,-)
/usr/bin/peazip
/usr/lib/peazip/*
/usr/lib/libQt5Pas.so.1
/usr/share/applications/peazip.desktop
/usr/share/doc/peazip/*
/usr/share/peazip/*
/usr/share/pixmaps/peazip.png
/usr/share/pixmaps/peazip_add.png
/usr/share/pixmaps/peazip_extract.png
