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
Qt widgetset packages requires Qt5 (Qt 4.3 for legacy 32 bit Linux packages), and needs libQt5Pas.so.1.2.6 often referred as libQt5Pas.so.1 (libqt4intf.so and libqt4pas5 for 32 bit) or equivalent binary installed in /usr/local/lib or equivalent directory, such as /usr/lib or /usr/lib32.
You can get up to date missing libraries using distribution-specific ways, i.e. apt-get install package:architecture, or copy the file manually and run ldconfig
