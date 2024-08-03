
Drag and Drop Component Suite Version 5.2
Released 17-aug-2010
© 1997-2010 Anders Melander
http://melander.dk/delphi/dragdrop/

2017 Fork for Lazarus Michael Köcher (six1) http://forum.lazarus.freepascal.org/index.php/topic,38362.0.html

Installation Lazarus:
Open "/Source/DragDropLazarus.lpk" with Lazarus and compile and Install


Andres Melanders hints:
-------------------------------------------
Table of Contents:
-------------------------------------------
1. Supported platforms
2. Installation
3. Uninstallation
4. Known problems
5. Support and feedback


-------------------------------------------
1. Supported platforms:
-------------------------------------------
This version of the library has been tested with Delphi 5, Delphi 7, Delphi 2007,
Delphi 2009 and Delphi 2010 on Windows XP Professional SP3.
Other versions of Delphi, C++ Builder and Windows may be supported.


-------------------------------------------
2. Installation:
-------------------------------------------
1. If you are using a previous version of the Drag and Drop Component Suite, uninstall that
   version first.

2. Unzip the package to a folder of your choice.

3. In the Packages folder, find the design time package that matches your version of Delphi.
   Open it in Delphi, Compile and Install.

4. Locate the Library sub-folder that matches your version of Delphi. Add it to the Delphi
   library search path.

5. Optional: Add the Source folder to the Delphi browsing path.


-------------------------------------------
3. Uninstallation:
-------------------------------------------
1. Open Delphi and uninstall the Drag and Drop Component Suite design time package.

2. Remove the Library folder from the Delphi library search path.

3. Remove the Source folder from the Delphi browsing path.

4. Locate the folder where you installed The Drag and Drop Component Suite and delete that
   folder.

-------------------------------------------
4. Known problems:
-------------------------------------------
* When the demo applications are compiled with Delphi 7, some of them
  will probably emit a lot of "Unsafe type", "Unsafe code" etc. warnings.
  This doesn't mean that there's anything wrong with the demos. It just
  means that Borland, at that time, wanted you to know that they were
  moving to .NET and would like you to do the same (so you can buy the
  next version of Delphi).
  You can turn the warnings of in the project options.

* Delphi's and C++Builder's HWND and THandle types are not compatible.
  For this reason it might be nescessary to cast C++Builder's HWND values to
  Delphi's THandle type when a HWND is passed to a function. E.g.:

    if (DragDetectPlus(reinterpret_cast<THandle>(Handle), Point(X, Y))) {
      ...
    }

* Virtual File Stream formats can only be pasted from the clipboard with live
  data (i.e. FlushClipboard/OleFlushClipboard hasn't been called on the data
  source). This problem affects TFileContentsStreamOnDemandClipboardFormat and
  the VirtualFileStream demo.
  This is believed to be a bug in the Windows clipboard and a work around hasn't
  been found yet.

* When TDropFileTarget.GetDataOnEnter is set to True, the component doesn't work
  with WinZip.
  Although the file names are received correctly by TDropFileTarget, WinZip
  doesn't extract the files and the files thus can't be copied/moved.
  This is caused by a quirk in WinZip; Apparently WinZip doesn't like
  IDataObject.GetData to be called before IDropTarget.Drop is called.

-------------------------------------------
5. Support and feedback:
-------------------------------------------
You are welcome to give me feedback by posting a comment on my site, but I no longer
provide free support.