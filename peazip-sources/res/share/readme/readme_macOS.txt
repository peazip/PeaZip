TO INSTALL PEAZIP ON MACOS

1) Open PeaZip's DMG and drop peazip.app in Applications directory
2) Open Terminal, paste and run the following command:

xattr -dr com.apple.quarantine /Applications/peazip.app
or
sudo xattr -dr com.apple.quarantine /Applications/peazip.app
if current user is not allowed to use xattr command

Running the aforementioned xattr command is needed to remove the "quarantine" attribute which is automatically applied by Safari to unsigned app packages downloaded from the web; otherwise running PeaZip will result in one of the following error messages:

"peazip.app is damaged and can’t be opened"
"peazip.app cannot be opened because the developer cannot be verified" 


OPTIONAL: MACOS SERVICE MENUS

The directory "macOS service menus" contains .workflow scripts of context menu items to quickly send items selected in Finder to to main PeaZip functions: add to archive, extract (with full options), extract here (without further interaction), and open file or folder with PeaZip.
To install a .workflow item, double-click on it.
To uninstall a .workflow item, follow system's instruction from context menu customization entry or simply delete it from ˜/Library/Services/ directory.


MORE INFORMATION

PeaZip is available built for aarch64 and Intel architectures on https://peazip.github.io/peazip-macos.html
The application's binaries are not signed by a registered Apple developer account: M1 version is simply ad-hoc signed, Intel version is not signed.
M1 SoC can run bot aarch64 (native) and Intel (using Rosetta 2 emulation layer), Intel-based Macintosh systems can only run Intel version.

PeaZip sources https://peazip.github.io/peazip-sources.html can be built with Lazarus for macOS, sources are tested for Cocoa widget set.
To manually select the target architecture open main menu, Project > Project Options, Configuration and Target, set Darwin as destination OS, and aarch64 as destination CPU family if compiling for machines with M1 Apple Silicon chip, or x86_64 if compiling for Intel-based Apple machines.