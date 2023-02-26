ADD AND REMOVE PEAZIP FROM WINDOWS 11 MINI CONTEXT MENU

add PeaZip *.reg 
Adds most commonly used PeaZip functions to Windows 11 mini context menu.
The scripts assume PeaZip is installed in C:\Program Files\PeaZip: change destination if the application is installed in a different path, or if you want to use the scripts for PeaZip Portable.
The scripts add items to Windows 11 mini context menu: you can comment (pre-pending ; character) the entries you want to not apply.
It is possible to localize the language of the menu entries changing the text in MUIVerb entries.

remove PeaZip entries.reg
Removes PeaZip entries form Windows 11 mini context menu.

The scripts use whitelisted ID SetDesktopWallpaper: you can use different whitelisted IDs if you prefer, and if made available by Microsoft.

Please note all context menu and SenTo menu entries are available from "Show more options", or pressing Shift+F10, in the Windows 11 full context menu, and are not affected by those scripts.

Read PeaZip documentation for the full list of supported functions and please note commands containing "multi" particle are natively capable to pass multiple items to a single program's instance.

For quick reference, most relevant PeaZip commands are:

-add2multi Add item(s) to archive allowing to set format, options, password, etc
-add2multi7z Add item(s) to 7Z archive and immediately starts compression
-add2multizip Add item(s) to ZIP archive and immediately starts compression
-add2multiconvert Add item(s) to archive conversion screen

-ext2openasarchive Try to open a file of arbitrary type as archive; if input is a directory, open PeaZip in that directory 

-ext2multi Extract item(s) allowing to set destination, options, password, etc
-ext2multihere Extract item(s) in current directory
-ext2multismart Extract item(s) in current directory, creating a new folder if needed

