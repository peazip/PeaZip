PeaZip is a natively portable standalone application, requiring none or minimal installation on the target system.
This is an advantage when it's desired to have a portable application to just unpack and run, or when a library freeze is desired and shared libraries should not be furtherly changed (i.e. on some production system), or when it's desired to have a single application with almost identical look and feel and features in an environment with mixed systems or desktop managers.
One obvious shortcoming of this approach is scarce system integration on target environments; FreeDesktop standards (http://freedesktop.org/wiki/Standards/menu-spec) however offers powerful and easy to use methods, using .desktop file scripting, for integration of applications in desktop environments following FreeDesktop standard guidelines (notably, Gnome and KDE).

With following instructions you can easily integrate PeaZip with Gnome and KDE:

1) Install a PeaZip installable package, or extract PeaZip portable to the desired path and link peazip binary in one of the system's binary folder i.e. /bin, /sbin, /usr/bin (recommended), /usr/local/bin 

2a) Gnome
- Nautilus scripts
Manually copy 'Archiving' folder, which is contained in 'nautilus-scripts', to your user's profile '.gnome2/nautilus-scripts' folder; that folder is hidden, so you can either find it flagging 'View>Show hidden files' in Nautilus, or clicking on 'Scripts>Open scripts folder' in Gnome's context menu.
This integration is NOT automatically performed by PeaZip installer, because nautilus-scripts path location is currently not widely standardized.
- .desktop files
Copy peazip.desktop files in /usr/share/applications
NOTE Gnome is often configured to use /usr/share/application and /usr/share/pixmaps or /usr/share/icons paths (i.e. on Ubuntu), but the paths may be different on some distribution or in some specific installations.
This integration is automatically performed by the installer.

2b) KDE:
- KDE start menu and 'Open with' dialog (.desktop files)
Copy peazip.desktop in (kde directory)/share/applications/kde if you want to add PeaZip in KDE start menu (and 'Open with' dialog).
This integration is automatically performed by the installer.
- Konqueror (KDE 3.x) Action service menu (.desktop files)
Copy .desktop files from "kde3-konqueror" folder to (kde directory)/share/apps/konqueror/servicemenus
Please note that KDE directory may vary from distribution to distribution (or can even be customized by the user at KDE installation); /opt/kde3 is usually a common place for installing KDE and is used, in example, in Suse and OpenSuse. The (kde directory) is specified in $KDEDIR in environment variable and can be overridden by $KDEDIRS.
This integration is automatically performed by the installer.
- Dolphin (KDE 4.x) service menu
Copy .desktop files from "kde4-dolphin" to /usr/share/kde4/services/ServiceMenus/
Please note that some distribution may use different paths.
This integration is automatically performed by the installer.

3) optionally you can customize .desktop files in order to use PeaZip's icons for Linux (see peazip_icons_linux.zip package on Google Code): place the icons in (kde directory)/share/icons or usr/share/icons and change consequently the "Icon=tar" entry in .desktop files.

peazip.desktop associates PeaZip with some of most common supported file types, in this folder you can find an alternative .desktop file for PeaZip application to associate it with all mimetypes to get benefit of the ability of the program to handle custom filetypes.
Both files can be used as templates to edit custom filetype associations and other application's properties.
