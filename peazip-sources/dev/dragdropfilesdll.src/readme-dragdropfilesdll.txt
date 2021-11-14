To compile sources you need Lazarus IDE (https://sourceforge.net/projects/lazarus).
Open .lpi project files and do "build all" to compile the sources.

dragdropfilesdll directory contains sources to build dragdropfilesdll.dll, which provides
application-to-system files drag&drop functions under Windows systems, sources in this path
requires installation of optional Lazarus package DragDropLazarus5.2 (or newer) to be 
compiled, which is based on work of Angus Johnson & Anders Melander (on Delphi), and Michael 
Köcher / six (on Lazarus).

Author: Giorgio Tani
License: LGPLv3
Purpose: Drag and Drop files from application to Windows system either using as source already 
existing files, or waiting for files being created