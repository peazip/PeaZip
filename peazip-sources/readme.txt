PeaZip free archiver project sources
https://peazip.github.io/

To compile sources you need Lazarus IDE (https://sourceforge.net/projects/lazarus).
Open .lpi project files and do "build all" to compile the executables.

Basically, you need to compile project_peach.lpi (PeaZip) and project_pea.lpi (Pea), on Windows you also need to compile dragdropfilesdll.lpi and copy dragdropfilesdll.dll in the same directory of PeaZip.

PeaZip project packages follows this naming convention: name-version.architecture
except for DEB packages following Debian naming convention: name_version_architecture

"name" states the pakage (i.e. PeaZip, PeaZip Portable, sources...)

"version" field starts with version number x.y.z, then states target OS (LINUX, WINDOWS, WIN64); on Linux systems is then declared the target widgetset (i.e. GKT2, Qt5), and finally the release number (usually -1).

"architecture" on Linux declare target architecture (on Debian amd64 is used in place of x86_64) and is omitted on Windows.

Please read the following documentation to understand what is contained in the source package and please see precompiled program's packages to know what third parts executables (7z, arc, paq...) are needed by PeaZip.


Source package content:


SOURCES:

- project_pea.lpi: PEA, the actual engine for PEA file format support; 
- project_peach.lpi: originally PEACH, PEAlaunCHer, that compiles to the main executable 
  PeaZip and act as GUI frontend for PEA, 7z and other utilities;
- project_demo_lib.lpi: a demo application using PEA source as a library.

/dev/dragdropfilesdll directory contains sources to build dragdropfilesdll.dll, which provides application-to-system files drag&drop functions under Windows systems, sources in this path requires installation of optional Lazarus package DragDropLazarus5.2 (or newer) to be compiled, which is based on work of Angus Johnson & Anders Melander (on Delphi), and Michael Köcher / six (on Lazarus).
The package is available in Lazarus Online Package Manager or from https://packages.lazarus-ide.org/DragDrop.zip

/dev/installer path contains InnoSetup script files creating Windows installers with file associations and menu integration for PeaZip.

.res and resulting .rc files are used on Windows platform to give to the application's executables manifest and binaries information (author, version etc)


MEDIA AND DOCUMENTATION:

Readme_*.txt files contain hints for the Windows and Linux users.

copying.txt is the license file for PeaZip project sources, released under LGPL.

/res/share/icons and /res/share/themes paths contains graphic for PeaZip project.

/res/share/lang path contains featured translations of application's text.

/res/share/lang-wincontext path contains .reg files to localize app's context menus in Windows

/res/share/batch path contains sample scipts to use PeaZip from command line, scripts, and .desktop files, and "freedesktop_integration" subpath contain files for integration in desktop environments compliant with freedesktop standars (i.e. Gnome, KDE, and other common Linux DE)


THIRD PARTS:

Units from Wolfgang Ehrhardt's crypto and utilities library, are intellectual 
property of Wolfgang Ehrhardt, released uner Zlib license.
The unit FCAES256.PAS is developed with the contribution of both me and, mainly,
of Wolfgang Ehrhardt.

Latest Wolfgang Ehrhardt's libraries used:
aes_2017-11-17.zip
crc_hash_2018-01-01.zip
fca_2017-11-17.zip
serpent_2017-11-17.zip
tf_2017-11-17.zip
util_2018-11-27.zip

7z (LGPL), 7-Zip-zstd codecs (LGPL), Brotli (MIT License), Zstandard (Dual license BSD / GPLv2), ARC (GPL), LPAQ/PAQ8* (GPL), UnACE (royalty free), QUAD (LGPL), BALZ (public domain), strip and UPX (GPL) binaries are needed to support mainstream file formats, they are not included in source package (but are included in the program's precompiled packages) and are intellectual property of respective Authors.

In PeaZip interface are used some icons inspired by Tango Desktop Project, Crystal/Crystal Clear, and NuoveXT, which are originally released under Creative Commons Attribution Share-Alike and LGPL licenses; more icons are available in /res/share subpaths.


How to package PeaZip for distribution:

PeaZip Portable should be built compressing a directory containing 
 pea and peazip binaries compiled from sources in src package for the target system, and dragdropfilesdll.dll on Windows
 res folder, which contains resources divided by type, and 'portable', an empty file marking the package as portable
  bin architecture dependent binaries, that should be compiled by respective third party sources for the target system
  share non-architecture dependent data such as texts, sample scripts, media, documentation
  conf configuration files
(for further subfolders in each res subpath plesase refer to an existing up to date Portable package)

Installer packages should be built starting from portable package
1) Removing (peazip)/res/portable file, which causes the application to write configuration file to appropriate user specific directory for the known systems, $XDG_CONFIG_HOME/peazip or $HOME/.config/peazip for non_Windows systems following Open Deskptop standard.
2) Taking care to place resources in proper paths depending on the filesystem standards of the target system, e.g. (peazip)/res/bin and (peazip)/res/share should be replaced to links to pointing to (path)/peazip directory created in proper branches of the filesystem for the type of resources
This mechanism can be used, on systems not supporting Open Desktop standards, also to redirect configuraton folder, letting (peazip)/res/portable file, which tells the application to write configuration to (peazip)/res/conf which is, in this case, replaced by a link.
3) On Windows installable packages, place "Configure peazip.exe" compiled from InnoSetup script peazip-setup_script_WIN*-configure.iss in (peazip)/res/bin directory