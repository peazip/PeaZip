unit BJL3;

{Bob Jenkins' 32-bit hash lookup3 / hashlittle}


interface


(*************************************************************************

 DESCRIPTION     :  Bob Jenkins' 32-bit hash lookup3 / hashlittle

 REQUIREMENTS    :  TP5-7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  - http://burtleburtle.net/bob/hash/doobs.html
                    - http://burtleburtle.net/bob/c/lookup3.c
                    - https://en.wikipedia.org/wiki/Jenkins_hash_function


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     02.12.17  W.Ehrhardt  Initial BP7 version
 0.11     02.12.17  we          Mix/Final for 32 bit
 0.12     02.12.17  we          Selftest with data from BJ and D25
 0.13     03.12.17  we          Separate unit BJL3
 0.14     04.12.17  we          Init/Update/Final/Full procedures

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2017 Wolfgang Ehrhardt

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


{$i std.inc}


function  hashlittle(msg: pointer; mlen, initval: longint): longint;
  {-Hash "mlen" bytes from "msg" using Bob Jenkins' lookup3/hashlittle}

procedure BJL3Init(var BJHash: longint);
  {-Bob Jenkins' lookup3 initialization, initval=0}

procedure BJL3Update(var BJHash: longint; Msg: pointer; Len: longint);
  {-update Bob Jenkins' lookup3 with Msg data}

procedure BJL3Final(var BJHash: longint);
  {-finalize Bob Jenkins' lookup3 calculation}

procedure BJL3Full(var BJHash: longint; Msg: pointer; Len: longint);
  {-Bob Jenkins' lookup3 of Msg with init/update/final, initval=0}

function  BJL3Selftest: boolean;
  {-Selftest for  Bob Jenkins' lookup3}


implementation


uses
  BTypes;

(* Notes:

1. This is no cryptographic hash function, it was designed for table lookup.

2. All len bytes of a message are updated with a single call, if the hash is
   used with chaining like this (this seems to be Delphi's update procedure)

   h := 0;
   h := hashlittle(msg1, len1, h);
   h := hashlittle(msg2, len2, h);

   the hashing is not associative like the other CRC/Hash hash function,
   hashlittle('aaa',3,0) is not equal to the value from the loop below
   because the length of a message is used during initialization:

   h := 0; for i:=1 to 3 do h := hashlitte('a',1,h);

3. This is a clean-room Pascal implementation of lookup3 for little endian,
   and does not use Delphi of FPC source code.

4. The purpose of this unit is to make available the non-cryptographic
   hash function from Delphi's system.hash unit.
*)

{$ifdef BIT16}

{$ifdef BASM}

(** TP6-7/D1 **)

{---------------------------------------------------------------------------}
function RotL(X: longint; c: word): longint;
inline(
  $59/              {pop    cx     }
  $66/$58/          {pop    eax    }
  $66/$D3/$C0/      {rol    eax,cl }
  $66/$8B/$D0/      {mov    edx,eax}
  $66/$C1/$EA/$10); {shr    edx,16 }

{$else}

{** T5/5.5 **}

{---------------------------------------------------------------------------}
function RotL(X: longint; c: word): longint;
  {-Rotate left}
inline(
  $59/           {  pop    cx    }
  $58/           {  pop    ax    }
  $5A/           {  pop    dx    }

  $83/$F9/$10/   {  cmp    cx,16 }
  $72/$06/       {  jb     S     }
  $92/           {  xchg   dx,ax }
  $83/$E9/$10/   {  sub    cx,16 }
  $74/$09/       {  je     X     }

  $2B/$DB/       {S:sub    bx,bx }
  $D1/$D0/       {L:rcl    ax,1  }
  $D1/$D2/       {  rcl    dx,1  }
  $13/$C3/       {  adc    ax,bx }
  $49/           {  dec    cx    }
  $75/$F7);      {  jne    L     }
                 {X:             }
{$endif}
{$endif}


{---------------------------------------------------------------------------}
function hashlittle(msg: pointer; mlen, initval: longint): longint;
  {-Hash "mlen" bytes from "msg" using Bob Jenkins' lookup3/hashlittle}
type
  T3L = array[0..2] of longint;
  P3L = ^T3L;
var
  pm: P3L;
  a, b, c, len: longint;
  last: T3L;
begin

  pm := P3L(msg);
  len:= mlen;

  a  := longint($deadbeef) + len + initval;
  b  := a;
  c  := a;

  {Optimal speed if msg^ is dword aligned, otherwise there may be some}
  {speed penalty, I don't if using byte access will be faster, but the}
  {resulting code is much more complicated.                           }

  while len > 12 do begin
    inc(a, pm^[0]);
    inc(b, pm^[1]);
    inc(c, pm^[2]);
    dec(len, 12);
    inc(Ptr2Inc(pm),12);

    {mix 3 32-bit values reversibly}
    {$ifdef BIT16}
      dec(a, c);  a := a xor RotL(c, 4);  inc(c, b);
      dec(b, a);  b := b xor RotL(a, 6);  inc(a, c);
      dec(c, b);  c := c xor RotL(b, 8);  inc(b, a);
      dec(a, c);  a := a xor RotL(c,16);  inc(c, b);
      dec(b, a);  b := b xor RotL(a,19);  inc(a, c);
      dec(c, b);  c := c xor RotL(b, 4);  inc(b, a);
    {$else}
      dec(a, c);  a := a xor ((c shl  4) or (c shr (32- 4)));  inc(c, b);
      dec(b, a);  b := b xor ((a shl  6) or (a shr (32- 6)));  inc(a, c);
      dec(c, b);  c := c xor ((b shl  8) or (b shr (32- 8)));  inc(b, a);
      dec(a, c);  a := a xor ((c shl 16) or (c shr (32-16)));  inc(c, b);
      dec(b, a);  b := b xor ((a shl 19) or (a shr (32-19)));  inc(a, c);
      dec(c, b);  c := c xor ((b shl  4) or (b shr (32- 4)));  inc(b, a);
    {$endif}
  end;

  {Process final bytes}
  if len>0 then begin
    {Simply move the last bytes of msg into local variable and pad with 0}
    {Bob Jenkins uses a case construct with fall through and lots of code}
    last[0] := 0;
    last[1] := 0;
    last[2] := 0;
    move(pm^,last,len);
    inc(a, last[0]);
    inc(b, last[1]);
    inc(c, last[2]);
    {final mixing of 3 32-bit values (a,b,c) into c}
    {$ifdef BIT16}
      c := c xor b;  dec(c, RotL(b,14));
      a := a xor c;  dec(a, RotL(c,11));
      b := b xor a;  dec(b, RotL(a,25));
      c := c xor b;  dec(c, RotL(b,16));
      a := a xor c;  dec(a, RotL(c, 4));
      b := b xor a;  dec(b, RotL(a,14));
      c := c xor b;  dec(c, RotL(b,24));
    {$else}
      c := c xor b;  dec(c, (b shl 14) or (b shr (32-14)) );
      a := a xor c;  dec(a, (c shl 11) or (c shr (32-11)) );
      b := b xor a;  dec(b, (a shl 25) or (a shr (32-25)) );
      c := c xor b;  dec(c, (b shl 16) or (b shr (32-16)) );
      a := a xor c;  dec(a, (c shl  4) or (c shr (32- 4)) );
      b := b xor a;  dec(b, (a shl 14) or (a shr (32-14)) );
      c := c xor b;  dec(c, (b shl 24) or (b shr (32-24)) );
    {$endif}
  end;
  hashlittle := c;
end;


{---------------------------------------------------------------------------}
function BJL3Selftest: boolean;
  {-Selftest for Bob Jenkins' lookup3/hashlittle}
const
  msg: array[0..29] of char8 = 'Four score and seven years ago';
  a17: array[0..16] of char8 = 'aaaaaaaaaaaaaaaaa';
var
  h,i: longint;
begin
  BJL3Selftest := false;
  {tests from lookup3}
  if hashlittle(@msg, sizeof(msg), 0) <> longint($17770551) then exit;
  if hashlittle(@msg, sizeof(msg), 1) <> longint($cd628161) then exit;
  if hashlittle(@msg, 0, 0)           <> longint($deadbeef) then exit;
  {tests computed with D25 Tokyo Starter}
  if hashlittle(@a17, sizeof(a17), 0) <> 503005453 then exit;
  h := 0;
  for i:=1 to sizeof(a17) do h := hashlittle(@a17, 1, h);
  if h <> 294676632 then exit;
  {more to come?}
  BJL3Selftest := true;
end;


{---------------------------------------------------------------------------}
procedure BJL3Init(var BJHash: longint);
  {-Bob Jenkins' lookup3 initialization, initval=0}
begin
  BJHash := 0;
end;


{---------------------------------------------------------------------------}
procedure BJL3Update(var BJHash: longint; Msg: pointer; Len: longint);
  {-update Bob Jenkins' lookup3 with Msg data}
begin
  BJHash := hashlittle(Msg, Len, BJHash);
end;


{---------------------------------------------------------------------------}
procedure BJL3Full(var BJHash: longint; Msg: pointer; Len: longint);
  {-Bob Jenkins' lookup3 of Msg with init/update/final}
begin
  BJHash := hashlittle(Msg, Len, 0);
end;


{---------------------------------------------------------------------------}
procedure BJL3Final(var BJHash: longint);
  {-finalize Bob Jenkins' lookup3 calculation}
begin
  {Dummy}
end;


end.
