{$ifdef BASM16}

 {Helper include file for BASM16 dword alignment     (c) 2007 W.Ehrhardt}

 {Note: proper dword alignment for BASM16 is a bit tricky. It depends on}
 {the unit order and is therefore application depended. Use DumpAlign to}
 {see the order of initialization and the offsets mod 4 (should be 0!). }
 {If alignment for a specific application is done, use $undef DumpAlign.}

 {align.inc is used in CRC24 ... CRC64; if more than one of these units }
 {are used, it may be necessary to iterate the alignment procedure.     }

 {$define NoAlign}        {define to disable the alignment definitions}

 {$ifndef NoAlign}
   {$define DumpAlign}    {set to dump align info in initialization}
   {$define A4_CRC24}     {insert a dummy word in CRC24 }
   {.$define A4_CRC32}    {insert a dummy word in CRC32 }
   {.$define A4_FCRC32}   {insert a dummy word in FCRC32}
   {.$define A4_CRC64}    {insert a dummy word in CRC64 }
 {$endif}

{$endif BASM16}
