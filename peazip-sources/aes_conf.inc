(*************************************************************************

 DESCRIPTION     :  AES configuration include file

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10, FPC, VP (Undef BASM16 for 286)

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version
 0.11     09.07.06  we          Common defines for encrypt/decryt tables
 0.12     19.07.06  we          Cond. defines AES_Diag, AES_Encr/Decr_DummyAlign
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2006 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)


{Use additional 1K expanded able Te4/Td4 for last encryption round}
{.$define AES_LONGBOX}

{Use 2K tables TCe/TCd instead of four 1K tables Te0 .. Te3, Td0 .. Td3}
{.$define AES_ComprTab}

{Interface some diagnostic data, eg Enc/Dec table offsets mod 16}
{$define AES_Diag}

{Use to align TCe to 8 byte boundary, inserts dummy longint}
{Inspect the map file and/or use AES_Diag/TCe_Diag}
{.$define AES_Encr_DummyAlign}

{Use to align TCd to 8 byte boundary, inserts dummy longint}
{Inspect the map file and/or use AES_Diag/TCd_Diag}
{.$define AES_Decr_DummyAlign}


{---------------------------------------------------------------------------}
{Consistency check - do not change!}
{$ifdef AES_ComprTab}
  {$ifdef AES_LONGBOX}
    {$undef AES_LONGBOX}
  {$endif}
{$endif}

