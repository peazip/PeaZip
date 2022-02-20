PeaZip free archiver project sources
https://peazip.github.io/

To compile sources you need Lazarus IDE https://www.lazarus-ide.org/


CONTENT

project_pea.lpi: PEA, the actual engine for PEA file format support
project_peach.lpi: originally PEACH, PEAlaunCHer, that compiles to the main executable PeaZip and act as GUI frontend for PEA, 7z and other utilities
project_demo_lib.lpi: a demo application using PEA source as a library

.res and resulting .rc files are used on Windows platform to give to the application's executables manifest and binaries information (author, version etc)

(peazip sources)/dev/dragdropfilesdll directory contains sources to build dragdropfilesdll.dll, which provides application-to-system files drag&drop functions under Windows systems.
It requires installation of optional Lazarus package DragDropLazarus5.2 (or newer) to be compiled, which is based on work of Angus Johnson & Anders Melander (on Delphi), and Michael Köcher / six (on Lazarus). The package is available in Lazarus Online Package Manager or from https://packages.lazarus-ide.org/DragDrop.zip

(peazip sources)/dev/installer path contains InnoSetup script files creating Windows installers with file associations and menu / SendTo integration for PeaZip.


COMPILE AND BUILD 

MANUAL
Open .lpi project files and use main menu Run > Build all to compile and build the binaries:
project_peach.lpi builds peazip, the main app executable, file and archive manager
project_pea.lpi builds pea, which provides pea format support, file management tools, encryption and hashing routines
On Windows systems you will also need to compile (peazip sources)/dev/dragdropfilesdll/dragdropfilesdll.lpi and copy dragdropfilesdll.dll in the same directory of peazip.exe

AUTO
Replace lazbuild with full qualified path to lazbuild binary if needed.
Replace (peazip sources) with fill qualified path to PeaZip source's directory.
Run:
lazbuild (peazip sources)/project_peach.lpi
lazbuild (peazip sources)/project_pea.lpi
lazbuild (peazip sources)/dev/dragdropfilesdll/dragdropfilesdll.lpi //only needed for Windows systems


ASSEMBLE PACKAGES

NAMING
PeaZip project packages follows this naming convention: name-version.architecture 
DEB packages are an exception to this rule, following Debian naming convention: name_version_architecture
"name" states the pakage (i.e. PeaZip, PeaZip Portable, sources...)
"version" field starts with version number x.y.z, then states target OS (LINUX, WINDOWS, WIN64); on Linux systems is then declared the target widgetset (i.e. GKT2, Qt5), and finally the release number (usually -1).
"architecture" on Linux declare target architecture (on Debian amd64 is used in place of x86_64) and is omitted on Windows.

PORTABLE PACKAGES
PeaZip Portable should be built compressing a directory here referred as (peazip), containing: 
pea and peazip binaries compiled for the target system (and dragdropfilesdll.dll on Windows)
(peazip)/res directory which contains resources divided by type
(peazip)/res/portable empty file marking the package as portable
(peazip)/res/bin directory containing architecture dependent binaries, that should be compiled by respective third party sources for the target system: refer to THIRD PARTIES section below, or refer to the content of an existing package
(peazip)/res/share directory containing non-architecture dependent data such as texts, sample scripts, media, documentation: refer to DETAILS ABOUT RES/SHARE section for further information
(peazip)/res/conf directory containing configuration files, and user-created custom scripts, compression settings, archive layouts
For details on content of each res subpath plesase refer to (peazip sources)/res, and possibly to an existing up to date Portable package.

INSTALLERS
Installer packages can be built starting from portable package
1) Removing (peazip)/res/portable file, in this way the application will write configuration file to appropriate user specific directory for the known systems, $XDG_CONFIG_HOME/peazip or $HOME/.config/peazip for non_Windows systems following Open Deskptop standard.
2) Taking care to place resources in proper paths depending on the filesystem standards of the target system, e.g. (peazip)/res/bin and (peazip)/res/share should be replaced to links to pointing to (proper path)/peazip directory created in proper branches of the filesystem for the type of resources.
In example, in current Linux packages, (peazip) is written to /usr/lib/peazip, with (peazip)/res/bin being /usr/lib/peazip/res/bin directory, and (peazip)/res/share being a link to /usr/share/peazip directory.
This mechanism can be used, on systems not supporting Open Desktop standards, also to redirect configuraton folder, letting (peazip)/res/portable file, which tells the application to write configuration to (peazip)/res/conf which will be, in this case, replaced by a link.
3) On Windows packages, place "Configure peazip.exe" compiled from InnoSetup script peazip-setup_script_WIN*-configure.iss in (peazip)/res/bin directory

peazip-x.y.z.pack.zip package provides examples of the directory structures used to create DEB and RPM packages for the current release, in order to help developers in assembling packages for distribution with different package managers.
It is not needed to compile sources, it is only meant as additional help to assemble packages.

DETAILS ABOUT RES/SHARE

(peazip)/res/share directory, containing non-architecture dependent resources, can be used "as is" on all systems and types of packages.
If desired, system dependent resources can be omitted to reduce the size of the package, i.e. in non-Windows packages it can be safely removed sample .bat and .reg files, being of no use.
This directory should contain (or link to) peazip_help.pdf in its root in order to provide offline help to users.

(peazip)/res/share/batch subfolder contains sample scripts to use PeaZip from command line scripts, .desktop files, SendTo examples for Windows, macOS .workflow service menus, and freedesktop_integration subpath which contains files for integration in desktop environments compliant with freedesktop standars (i.e. Gnome, KDE, and other common Linux DE)

(peazip)/res/share/copying contains copying.txt (the license file for PeaZip project sources, released under LGPLv3) and third-parties directory containing thir party licenses.

(peazip)/res/share/icons contains Windows icons (.png icons for non-Windows systems are available in batch/freedesktop_integration forlder)

(peazip)/res/share/lang path contains translations of application's text, most of which are contributed by third parties translators

(peazip)/res/share/lang-wincontext path contains .reg files to localize app's context menus in Windows

(peazip)/res/share/presets contains preset files for PeaZip defining custom compression settings. Those files are editable, but user-defined custom compression settings should be saved in configuration directory consistently with PeaZip's behavior

(peazip)/res/share/readme files contains a minimal system-dependent explanation about the application, for Windows and Linux users

(peazip)/res/share/themes path contains themes for PeaZip project; themes are .7z files (with .theme suffix before .7z extension) containing graphic resources and the theme file, an UTF-8 with BOM file containing theming options


THIRD PARTIES

The units of crypto and math utilities library used in PeaZip are derived from Wolfgang Ehrhardt's crypto library (originally aes_2017-11-17.zip, crc_hash_2018-01-01.zip, fca_2017-11-17.zip, serpent_2017-11-17.zip, tf_2017-11-17.zip, and util_2018-11-27.zip libraries), released under Zlib license by Wolfgang Ehrhardt.
Libraries are kept up to date, in example were patched to allow build on aarch64 architecture.

7z (LGPL), p7zip (LGPL), 7-Zip-zstd codecs (LGPL), Brotli (MIT License), Zstandard (Dual license BSD / GPLv2), ARC (GPL), ZPAQ/LPAQ/PAQ8* (GPL), QUAD (LGPL) / BALZ (public domain) / BCM (Apache v2), strip and UPX (GPL) binaries are needed to support mainstream file formats, they are not included in source package (but are included in the program's precompiled packages) and are intellectual property of respective Authors.
Up to date links to aforementione projects are:
https://www.7-zip.org/download.html 7-Zip's 7z (Windows, Linux, and macOS), should be placed in (peazip)/res/bin/7z
http://p7zip.sourceforge.net/ vanilla p7zip, can be placed as alternative to 7z in (peazip)/res/bin/7z
https://github.com/szcnick/p7zip p7zip fork with extra algorithms, can be placed as alternative in (peazip)/res/bin/7z
https://github.com/mcmilk/7-Zip-zstd codecs for 7Z (Windows only), should be placed in (peazip)/res/bin/7z/Codecs
http://freearc.sourceforge.net/ FreeArc, should be placed in (peazip)/res/bin/arc
https://github.com/google/brotli Brotli, should be placed in (peazip)/res/bin/brotli
http://mattmahoney.net/dc/ *PAQ, should be placed in (peazip)/res/bin/lpaq, /paq, and /zpaq
https://github.com/encode84/bcm / http://www.encode.su/ / http://sourceforge.net/projects/bcm/ Ilia Muraviev's QUAD/BALZ/BCM, should be placed in (peazip)/res/bin/quad
https://www.gnu.org/software/binutils/ strip (needed only on Windows), should be placed in (peazip)/res/bin/upx
https://upx.github.io/ UPX, should be placed in (peazip)/res/bin/upx
http://facebook.github.io/zstd/ Zstandard, should be placed in (peazip)/res/bin/zstd

PeaZip can also support other binaries, which MUST not be included in base packages due closed source nature
WinAce UnACE (royalty free) can be supported as back-end binary if placed in (peazip)/res/bin/unace, but must not be included in packages because non-free license.
RarLab UnRar (royalty free) can be supported as back-end binary if placed in (peazip)/res/bin/unrar, but must not be included in packages because non-free license.
RarLab Rar.exe (shareware) can be supported as back-end binary if installed on the same system (PeaZip can auto-configure itself to use it to create RAR archives), but must not included in packages because it is non-free software AND has non royalty free license.

From main menu, Options > Settings, Advanced tab it is possible to set PeaZip to NOT use non-free software or formats.
This behavior can be hardcoded setting HLIBRE_DIR constant in peazip (project_peach.lpi) at compile time:
0 not hardcoded,, read from configuration; 
1 hardcoded to allow only using known Free Software components 
2 hardcoded to allow only using known Free Software components and known open archive formats (not encumbered by patents for read nor write)

In PeaZip GUI interface are used some icons inspired by Tango Desktop Project, Crystal/Crystal Clear, and NuoveXT, which are originally released under Creative Commons Attribution Share-Alike and LGPL licenses; more icons are available in /res/share subpaths.
