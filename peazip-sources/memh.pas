unit memh;

{Basic portable heap memory allocation functions}

interface

{$i STD.INC}

(*************************************************************************

 DESCRIPTION   :  Basic portable heap memory allocation functions

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12/D17-D18, FPC, VP, WDOSX

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REMARK        :  16-bit compilers and sizes >= $10000: alloc functions
                  return nil, free procedures do nothing!


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.04.14  W.Ehrhardt  Initial version with malloc/mfree
 0.11     16.04.14  we          calloc/cfree
 0.12     16.04.14  we          use untyped var p in free routines
 0.13     17.04.14  we          ialloc with longint size
 0.14     17.04.14  we          long versions
 0.15     18.04.14  we          remove word versions, rename long versions
**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2014 Wolfgang Ehrhardt

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


function malloc(size: longint): pointer;
  {-Allocate heap, return nil if error}

function calloc(size: longint): pointer;
  {-Allocate heap, return nil if error, clear allocated memory to 0}

procedure mfree(var p; size: longint);
  {-Deallocate heap if p<>nil, p will be set to nil}

procedure cfree(var p; size: longint);
  {-Deallocate heap if p<>nil, p will be set to nil, memory set to 0}


implementation


{$ifdef BIT16}
{$F+}
{--------------------------------------------------------------------------}
function HeapFunc(Size: word): integer;
  {-Forces nil return values instead of runtime error if out of memory}
begin
  if size>0 then HeapFunc := 1;
end;


{---------------------------------------------------------------------------}
function ialloc(size: longint; set0: boolean): pointer;
  {-Allocate heap, return nil if error, clear allocated memory to 0 if set0}
var
  p, SaveHeapError : pointer;
  wsize: word absolute size;
type
  LH = packed record L,H: word; end;
begin
  if LH(size).H<>0 then ialloc := nil
  else begin
    SaveHeapError := HeapError;
    HeapError := @HeapFunc;
    getmem(p, wsize);
    HeapError := SaveHeapError;
    if (p<>nil) and set0 then fillchar(p^,wsize,0);
    ialloc  := p;
  end;
end;


{---------------------------------------------------------------------------}
procedure ifree(var p; size: longint; set0: boolean);
  {-Dellocate heap if p<>nil, set p=nil, clear allocated memory to 0 if set0}
var
  pp: pointer absolute p;
  wsize: word absolute size;
type
  LH = packed record L,H: word; end;
begin
  if (pp<>nil) and (LH(size).H=0) then begin
    if set0 then fillchar(pp^, wsize, 0);
    freemem(pp, wsize);
    pp := nil;
  end;
end;

{$else}


{---------------------------------------------------------------------------}
procedure ifree(var p; size: longint; set0: boolean);
  {-Dellocate heap if p<>nil, set p=nil, clear allocated memory to 0 if set0}
var
  pp: pointer absolute p;
begin
  if pp<>nil then begin
    if set0 then fillchar(pp^, size, 0);
    freemem(pp, size);
    pp := nil;
  end;
end;


{$ifdef FPC}

{---------------------------------------------------------------------------}
function ialloc(size: longint; set0: boolean): pointer;
  {-Allocate heap, return nil if error, clear allocated memory to 0 if set0}
var
  p: pointer;
  sh: boolean;
begin
  sh := ReturnNilIfGrowHeapFails;
  ReturnNilIfGrowHeapFails := true;
  getmem(p, size);
  ReturnNilIfGrowHeapFails := sh;
  if (p<>nil) and set0 then fillchar(p^,size,0);
  ialloc := p;
end;

{$else}

{---------------------------------------------------------------------------}
function ialloc(size: longint; set0: boolean): pointer;
  {-Allocate heap, return nil if error, clear allocated memory to 0 if set0}
var
  p: pointer;
begin
  try
    getmem(p, size);
  except
    p := nil;
  end;
  if (p<>nil) and set0 then fillchar(p^,size,0);
  ialloc := p;
end;

{$endif}

{$endif}


{---------------------------------------------------------------------------}
function malloc(size: longint): pointer;
  {-Allocate heap, return nil if error}
begin
  malloc := ialloc(size,false);
end;


{---------------------------------------------------------------------------}
function calloc(size: longint): pointer;
  {-Allocate heap, return nil if error, clear allocated memory to 0}
begin
  calloc := ialloc(size,true);
end;


{---------------------------------------------------------------------------}
procedure mfree(var p; size: longint);
  {-Deallocate heap if p<>nil, p will be set to nil}
begin
  ifree(p,size,false);
end;


{---------------------------------------------------------------------------}
procedure cfree(var p; size: longint);
  {-Deallocate heap if p<>nil, p will be set to nil, memory set to 0}
begin
  ifree(p,size,true);
end;


end.
