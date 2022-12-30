PeaZip is a natively portable standalone application, requiring none or minimal installation on the target system.

This is an advantage when it's desired to have a portable application to just unpack and run, or when a library freeze is desired and shared libraries should not be furtherly changed (i.e. on some production system), or when it's desired to have a single application with almost identical look and feel and features in an environment with mixed systems or desktop managers.

One obvious shortcoming of this approach is natively scarce system integration on target environments, which is addressed by Linux installers using same methods here explained in order to automatically integrate out-of-the box the application in different distributions.

With following instructions you can easily integrate PeaZip with Gnome, KDE, and other DE following Freedesktop guidelines.


1 INSTALL PEAZIP

1A Install a PeaZip installable package, or

1B Extract PeaZip portable to the desired path (i.e. in /opt, or in user's home subpath) and link peazip binary in the binaries folder most suited for your system i.e. /bin, /sbin, /usr/bin, /usr/local/bin [you will need root unless you have a ~/bin folder configured in user's home]
If you can not or want not to link peazip in binaries folder, you can edit the script files here provided using the absolute path of peazip binary instead of "peazip" directory.


2 SYSTEM INTEGRATION

App list, open with, and context menu can be customised using .desktop files provided in this folder: 
- peazip.desktop file provides standard system integration to main application; it is automatically installed with PeaZip installable packages
- other alternative .desktop files provided here features direct access to specific fuctions (extraction, archiving, force open as archive...) even to DE non supporting .desktop file Actions

Desired .desktop file(s) needs to be copied to /usr/share/applications or /usr/local/share/applications (system-wide) ~/.local/share/applications/ (user-specific)

Icon .png file(s) needs to be copied to /usr/share/icons or /usr/local/share/icons (system-wide) ~/.local/share/icons/ (user-specific)
More icon files are available in (peazip)/res/share/icons directory.
The icons of the app can be modified if desired, and the .desktop files can be edited to use a different icon.

Please note those locations may be different in some distributions and desktop environments, or could have been further configured differently by user's choice [you will need root to write to system-wide directories].

Notes for specific Desktop Environments:

2a) Gnome 2/3, Mate, Cinnamon, Budgie

Some versions may have been configured to use */pixmaps instad of */icons path.

Gnome-derived DE generally allows to customise context menu further use Nautilus scripts; copy 'Archiving' folder from 'Nautilus-scripts' directory, to
Gnome 2 .gnome2/nautilus-scripts 
Gnome 3 ~/.local/share/nautilus/scripts

With Gome 3 user can alternatively use nautilus-actions extension to customise app's system intergation

2b) KDE 3/4/5

Some versions of KDE may have been configured to use (kde directory)/share/applications/kde instead of standard paths.
The (kde directory) is specified in $KDEDIR in environment variable and can be overridden by $KDEDIRS.

To customise context menu use Service menus .desktop files from KDE(version) subfolders
Copy .desktop files from KDE(version) subfolders into, usually:
KDE 3 (kde directory)/share/apps/konqueror/servicemenus (kde directry usually being /opt/kde3)
KDE 4 /usr/share/kde4/services/ServiceMenus
KDE 5 /usr/share/kio/servicemenus or ~/.local/share/kio/servicemenus

2c) Xfce, LXDE 

Copy peazip.desktop file or other .desktop files to standard locations specified at point 2).
For Xfce, Xfdesktop 4.5 or higher is required


3) OPTIONAL CUSTOMISATION

.desktop files can be modified to implement functions described "Customisation and scripting" chapter of PeaZip help file.

Also, .desktop files can be edited to translate text, replace icons, and implement different features as described in Freedesktop specification documentation https://specifications.freedesktop.org/desktop-entry-spec/latest/