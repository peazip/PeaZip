zpaq705.zip, Apr. 17, 2015.

zpaq is a journaling archiver optimized for user-level incremental
backup of directory trees in Windows and *nix. It supports AES-256
encryption, 5 multi-threaded compression levels, and content-aware
file fragment level deduplication. For backups it adds only files
whose date has changed, and keeps both old and new versions. You can roll
back the archive date to restore from old versions of the archive.
The default compression level is faster than zip usually with better
compression. zpaq uses a self-describing compressed format to allow
for future improvements without breaking compatibility with older
versions of the program. Contents:

File          Ver.   Description
-----------   ----   -----------
zpaq.exe      7.05   Archiver, 32 bit Windows.
zpaq64.exe    7.05   Archiver, 64 bit Windows.
zpaq.cpp      7.05   zpaq source code.
zpaq.pod      7.03   zpaq documentation and man source.
libzpaq.h     7.03   libzpaq API documentation and header.
libzpaq.cpp   7.05   libzpaq API source code.
Makefile             To compile in Linux: make

All versions of this software can be found at
http://mattmahoney.net/dc/zpaq.html
Please report bugs to Matt Mahoney at mattmahoneyfl@gmail.com

Code from divsufsort.c is embedded in libzpaq.cpp.
divsufsort.c is (C) 2003-2008 Yuta Mori, MIT license (see source code).
It is also available from libdivsufsort-lite 2.0 from
http://code.google.com/p/libdivsufsort/
All remaining code is public domain.

zpaq.exe can run under either 32 or 64 bit Windows. zpaq64.exe runs only
under 64 bit Windows. The 32 bit version defaults to using at most
2 cores (you can select more) and can only use 2 GB memory.

zpaq is a command line program. For a brief description of the commands,
type "zpaq" with no arguments. See zpaq.pod for details.

TO COMPILE

Normally you can use "make" to compile for Unix, Linux, or Mac OS/X
or compile like this:

  g++ -O3 -march=native -Dunix zpaq.cpp libzpaq.cpp -pthread -o zpaq

To compile for non x86 or x86-64 hardware use option -DNOJIT
Some compilers complain about "-march=native" option. If so, take it out.

zpaq for Windows was compiled with MinGW g++ 4.8.1 (MinGW-W64 project,
rev5, sjlj, Windows threads) as follows:

  g++ -O3 -s -m64 -static zpaq.cpp libzpaq.cpp -o zpaq64
  g++ -O3 -s -m32 -static -DXP zpaq.cpp libzpaq.cpp -o zpaq

To compile using Visual Studio:
(tested with ver. 10.0 (2010), cl version 16.00.30319.01 for 80x86)

  cl /O2 /EHsc zpaq.cpp libzpaq.cpp advapi32.lib

To generate a man page in Linux or Cygwin:

  pod2man zpaq.pod > zpaq.man

Options have the following meanings:

-Dunix   = select Unix or Linux target in zpaq and libzpaq. The default is
           Windows. Some Linux compilers automatically define unix.
-DXP     = turn off features not supported in Windows XP (alternate streams).
-DDEBUG  = turn on run time checks.
-DNOJIT  = turn off run time optimization of ZPAQL to 32 or 64 bit x86
           in libzpaq. Use this for a non-x86 processor, or old
           processors not supporting SSE2 (mostly before 2001).
-pthread = link to pthread library (required in unix/Linux).

General options:

-O3 or /O2   = optimize for speed.
/EHsc        = enable C++ exception handling (VC++).
-s           = strip debugging symbols. (Some compilers ignore this).
-m32 or -m64 = select a 32 or 64 bit executable.
-static      = use this if you plan to run the program on a different
               machine than you compiled it on. Makes the executable bigger.
