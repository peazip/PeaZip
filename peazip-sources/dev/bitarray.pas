unit BitArray;


{Bit array context with max 524160 bits}


interface


(*************************************************************************

 DESCRIPTION     :  Bit array context with max 524160=8*$FFF0 bits
                    (spin off from mpint prime sieve, see t_bitarr)

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  max 64K heap for one context array

 DISPLAY MODE    :  ---

 ACKNOWLEDGEMENT :  Inspired by Michael Braun's TBitArray OBJECT


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     17.10.05  W.Ehrhardt  Initial version: BP7
 0.11     17.10.05  we          D1-D7/9, FPC, VP
 0.12     17.10.05  we          TP5/5.5
 0.13     18.10.05  we          Clear all bits in BA_Init
 0.14     18.10.05  we          Changed type name to TBitArray
 0.15     18.10.05  we          pbits now PByteArr, helper types in interface
 0.16     26.11.05  we          Renamed types: TBABytes, PBABytes
 0.17     11.09.07  we          FPC: use ReturnNilIfGrowHeapFails in malloc
 0.18     21.01.12  we          Fix for BIT64
 0.19     18.04.14  we          uses memh unit
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2005-2014 Wolfgang Ehrhardt

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


{$i STD.INC}

type
  TBABytes = array[0..$FFF0-1] of byte;  {Helper types}
  PBABytes = ^TBABytes;

type
  TBitArray  = record             {bit array context record  }
                 pbits: PBABytes; {pointer to allocated bytes}
                 used : longint;  {number of used bits       }
                 alloc: word;     {number of allocated bytes }
               end;

procedure BA_ClearBit(var BA: TBitArray; i: longint);
  {-Clear bit i of bit array}

procedure BA_ClearAll(var BA: TBitArray);
  {-Clear all bits of bit array}

procedure BA_Free(var BA: TBitArray);
  {-Deallocate/free bit array}

procedure BA_Init(var BA: TBitArray; NumBits: longint; var OK: boolean);
  {-Allocate/Initialize bit array with NumBits bits}

procedure BA_SetAll(var BA: TBitArray);
  {-Set all bits of bit array}

procedure BA_SetBit(var BA: TBitArray; i: longint);
  {-Set bit i of bit array}

function  BA_TestBit(var BA: TBitArray; i: longint): boolean;
  {-Test if bit i of bit array is set}

procedure BA_ToggleBit(var BA: TBitArray; i: longint);
  {-Toggle bit i of bit array}


implementation


uses memh;

const
  BitMask  : array[0..7] of byte = ($01,$02,$04,$08,$10,$20,$40,$80);


{---------------------------------------------------------------------------}
procedure BA_Init(var BA: TBitArray; NumBits: longint; var OK: boolean);
  {-Allocate/Initialize bit array with NumBits bits}
begin
  OK := false;
  fillchar(BA, sizeof(BA), 0);
  with BA do begin
    if NumBits > 524160 then exit
    else begin
      alloc := (NumBits+7) shr 3;
      pbits := malloc(alloc);
      OK := pbits<>nil;
      if OK then begin
        used := NumBits;
        fillchar(pbits^, alloc, 0);
      end;
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure BA_Free(var BA: TBitArray);
  {-Deallocate/free bit array}
begin
  with BA do begin
    mfree(pbits, alloc);
    used := 0;
    if (pbits<>nil) and (alloc>0) then freemem(pbits, alloc);
    alloc := 0;
    pbits := nil;
  end;
end;


{---------------------------------------------------------------------------}
procedure BA_SetBit(var BA: TBitArray; i: longint);
  {-Set bit i of bit array}
var
  k: word;
begin
  with BA do begin
    if (i>=0) and (i<used) then begin
      k := i shr 3;
      pbits^[k] := pbits^[k] or BitMask[i and 7];
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure BA_ClearBit(var BA: TBitArray; i: longint);
  {-Clear bit i of bit array}
var
  k: word;
begin
  with BA do begin
    if (i>=0) and (i<used) then begin
      k := i shr 3;
      pbits^[k] := pbits^[k] and (not BitMask[i and 7]);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure BA_ToggleBit(var BA: TBitArray; i: longint);
  {-Toggle bit i of bit array}
var
  k: word;
begin
  with BA do begin
    if (i>=0) and (i<used) then begin
      k := i shr 3;
      pbits^[k] := pbits^[k] xor BitMask[i and 7];
    end;
  end;
end;


{---------------------------------------------------------------------------}
function BA_TestBit(var BA: TBitArray; i: longint): boolean;
  {-Test if bit i of bit array is set}
begin
  with BA do begin
    if (i>=0) and (i<used) then begin
      BA_TestBit := pbits^[i shr 3] and BitMask[i and 7] <> 0;
    end
    else BA_TestBit := false;
  end;
end;


{---------------------------------------------------------------------------}
procedure BA_ClearAll(var BA: TBitArray);
  {-Clear all bits of bit array}
begin
  with BA do fillchar(pbits^, alloc, 0);
end;


{---------------------------------------------------------------------------}
procedure BA_SetAll(var BA: TBitArray);
  {-Set all bits of bit array}
begin
  with BA do fillchar(pbits^, alloc, $FF);
end;


end.
