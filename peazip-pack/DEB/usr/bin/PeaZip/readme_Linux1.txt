INSTALLATION
PeaZip is a natively portable application, it means that no installation procedure is strictly required to prepare the system to run it.
The program is released in two ways: as installable package authomatically handling system integration, for ease of use, and as standalone .tar.gz package (peazip_portable).
To install the latter, unpack the content of the .tar.gz package in the directory you prefer; you can move the program's folder in another directory whenever you want (even on network, cloud, or removable paths).

USAGE
To start PeaZip doubleclick (or launch by console) "peazip" binary. 
If the system finds unresolved dependencies while launching peazip binary for the first time, please follow the instructions of your installer manager to install the needed libraries (may be standard gtk/gdk related libraries, most of times libgdk_pixbuf library). 
If the program does not start and the system doesn't give any warning, try launching peazip from a console to get more detailed feedback.

SYSTEM INTEGRATION
Installable packages authomatically handles system integration in Gnome and KDE.
If you are using peazip_portable or if you want to customize the system integration in your desktop environment please follow the instructions in FreeDesktop_integration folder, which contains example for integrating the application in Gnome (.desktop files and Nautilus scripts) and KDE (.desktop files for Konqueror's Action service menu).

COMPILE PEAZIP
PeaZip is developed in FreePascal using Lazarus development environment; to compile the application from source package and for any other information please refer to application's website https://peazip.github.io , https://peazip.org or https://peazip.sourceforge.io

QT VERSION
Qt versions requires Qt 4.3.4 or more recent, and needs libQt4Pas (provided with the packages) installed in /usr/local/lib or equivalent directory (copy the file and run ldconfig; automatic in installable packages), such as /usr/lib or /usr/lib32 on some 64 bit distributions.
