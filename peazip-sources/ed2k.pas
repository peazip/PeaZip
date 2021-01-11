unit ED2K;

{eDonkey 2000 - 128 bit MD4-based hash function}


interface

(*************************************************************************

 DESCRIPTION     :  eDonkey 2000 - 128 bit MD4-based hash function
                    (used in eDonkey/eMule and other P2P)

 REQUIREMENTS    :  TP6/7, D1-D7/D9-D10/D12/D17-D18/D25S, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REMARK          :  For messages length n*CHUNKSIZE bytes (n>0) there are
                    different methods for the total hash. This unit always
                    calculates both the eDonkey and the eMule digests.

 REFERENCES      :  [1] http://en.wikipedia.org/wiki/EDonkey_network
                    [2] http://ed2k-tools.sourceforge.net/ed2k_hash.shtml

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     19.02.07  W.Ehrhardt  Initial version
 0.11     19.02.07  we          lastdig, special case chunkcnt=1 and cbytecnt=0
 0.12     19.02.07  we          clear context in ED2K_Final
 0.13     20.02.07  we          ED2K_Final without goto/label
 0.14     21.02.07  we          Clear lastdig in ED2K_Init
 0.15     22.02.07  we          $ifdef use_emule_final
 0.16     22.02.07  we          emule parameter
 0.17     23.02.07  we          calculate eDonkey AND eMule, type TED2KResult
 0.18     04.03.07  we          eMule=MD4 for zero length
 0.19     12.11.08  we          uses BTypes, Ptr2Inc and/or Str255/Str127
 0.20     19.07.09  we          D12 fix: assign with typecast string(fname)
 0.21     10.03.12  we          ED2K_File: {$ifndef BIT16} instead of {$ifdef WIN32}
 0.22     16.08.15  we          Removed $ifdef DLL / stdcall
 0.23     29.11.17  we          ED2K_File - fname: string

**************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2007-2017 Wolfgang Ehrhardt

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

uses
  BTypes,Hash,md4;

type
  TED2KResult  = packed record
                   eDonkey: TMD4Digest;  {eDonkey method hash}
                     eMule: TMD4Digest;  {eMule   method hash}
                    differ: boolean;     {eDonkey and eMule differ}
                 end;

type
  TED2KContext = packed record
                   total_ctx: THashContext;  {outer total context}
                   chunk_ctx: THashContext;  {inner chunk context}
                   lastdig  : TMD4Digest;    {hash of completed inner chunk}
                   cbytecnt : longint;       {byte count within chunk}
                   chunkcnt : longint;       {chunk count}
                 end;


procedure ED2K_Init(var Context: TED2KContext);
  {-initialize context}

procedure ED2K_Update(var Context: TED2KContext; Msg: pointer; Len: word);
  {-update context with Msg data}

procedure ED2K_UpdateXL(var Context: TED2KContext; Msg: pointer; Len: longint);
  {-update context with Msg data}

procedure ED2K_Final(var Context: TED2KContext; var ResRec: TED2KResult);
  {-finalize eDonkey/eMule hash calculation, clear context}

function  ED2K_SelfTest: boolean;
  {-eDonkey hash self test for two small strings}

procedure ED2K_Full(var ResRec: TED2KResult; Msg: pointer; Len: word);
  {-eDonkey/eMule hash of Msg with init/update/final}

procedure ED2K_FullXL(var ResRec: TED2KResult; Msg: pointer; Len: longint);
  {-eDonkey/eMule hash of Msg with init/update/final}

procedure ED2K_File({$ifdef CONST} const {$endif} fname: string;
                    var ResRec: TED2KResult; var buf; bsize: word; var Err: word);
  {-eDonkey/eMule hash of file, buf: buffer with at least bsize bytes}


implementation


{$ifdef BIT16}
  {$F-}
{$endif}


const
  CHUNKSIZE = 9728000;  {Chunk size for inner MD4 = 9500*1024}

const {MD4 of a zero length message, used for "buggy" eMule finalization}
  MD4Zero: TMD4Digest = ($31,$d6,$cf,$e0,$d1,$6a,$e9,$31,$b7,$3c,$59,$d7,$e0,$c0,$89,$c0);


{---------------------------------------------------------------------------}
procedure ED2K_Init(var Context: TED2KContext);
  {-initialize context}
begin
  with Context do begin
    MD4Init(total_ctx);
    MD4Init(chunk_ctx);
    fillchar(lastdig, sizeof(lastdig), 0);
    cbytecnt := 0;
    chunkcnt := 0;
  end;
end;


{---------------------------------------------------------------------------}
procedure ED2K_UpdateXL(var Context: TED2KContext; Msg: pointer; Len: longint);
   {-update context with Msg data}
var
  rem: longint;
begin
  with Context do begin
    while Len>0 do begin
      {get remaining bytes in chunk}
      rem := CHUNKSIZE-cbytecnt;
      if Len < rem then begin
        {current chunk will not be filled with these Len bytes}
        {therefore update chunk hash only and then exit}
        MD4UpdateXL(chunk_ctx, Msg, Len);
        inc(cbytecnt, Len);
        exit;
      end;
      {process rem bytes, ie complete the current chunk}
      MD4UpdateXL(chunk_ctx, Msg, rem);
      inc(Ptr2Inc(Msg), rem);
      dec(Len, rem);
      {calculate chunk hash}
      MD4Final(chunk_ctx, TMD4Digest(lastdig));
      {Update total hash with chunk hash}
      MD4UpdateXL(total_ctx, @lastdig, sizeof(lastdig));
      {update number of blocks processed}
      inc(chunkcnt);
      {Re-init chunk hash and number of bytes in chunk}
      cbytecnt := 0;
      MD4Init(chunk_ctx);
    end;
  end;
end;


{---------------------------------------------------------------------------}
procedure ED2K_Update(var Context: TED2KContext; Msg: pointer; Len: word);
   {-update context with Msg data}
begin
  ED2K_UpdateXL(Context, Msg, Len);
end;


{---------------------------------------------------------------------------}
procedure ED2K_Final(var Context: TED2KContext; var ResRec: TED2KResult);
  {-finalize eDonkey/eMule hash calculation, clear context}
begin
  {For messages length n*CHUNKSIZE bytes there a different methods for the}
  {total hash. Both eDonkey and eMule digests are calculated. Background info:}
  {http://forums.shareaza.com/showthread.php?s=d1f25f5e820941388008c6b250442cb7&threadid=49009&goto=nextoldest}
  with Context do begin
    if cbytecnt>0 then begin
      {if not at chunk boundary the hashes do not differ}
      ResRec.differ := false;
      {partial last chunk}
      MD4Final(chunk_ctx, lastdig);
      if chunkcnt=0 then begin
        {only one chunk: return chunk digest}
        ResRec.eDonkey := lastdig
      end
      else begin
        MD4UpdateXL(total_ctx, @lastdig, sizeof(lastdig));
        MD4Final(total_ctx, ResRec.eDonkey);
      end;
      ResRec.eMule := ResRec.eDonkey;
    end
    else if chunkcnt=0 then begin
      {zero length}
      ResRec.differ  := false;
      ResRec.eDonkey := MD4Zero;
      ResRec.eMule   := MD4Zero;
    end
    else begin
      ResRec.differ := true;
      if chunkcnt=1 then begin
        {special case: msg length = CHUNKSIZE. Here the inner chunk hash is}
        {already calculated in update, but must be used as the total eDonkey}
        ResRec.eDonkey := lastdig;
        MD4UpdateXL(total_ctx, @MD4Zero, sizeof(MD4Zero));
        MD4Final(total_ctx, ResRec.eMule);
      end
      else begin
        {Use chunk_ctx for a copy of total_ctx}
        chunk_ctx := total_ctx;
        {finalize eDonkey calculation}
        MD4Final(total_ctx, ResRec.eDonkey);
        {for eMule add a Zero-Message-Digest}
        MD4UpdateXL(chunk_ctx, @MD4Zero, sizeof(MD4Zero));
        MD4Final(chunk_ctx, ResRec.eMule);
      end;
    end;
  end;
  fillchar(Context,sizeof(Context),0);
end;


{---------------------------------------------------------------------------}
function ED2K_SelfTest: boolean;
  {-eDonkey hash self test for two small strings}
const
  s1: string[ 3] = 'abc';
  s2: string[62] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  D1: TMD4Digest= ($a4,$48,$01,$7a,$af,$21,$d8,$52,$5f,$c1,$0a,$e8,$7a,$a6,$72,$9d);
  D2: TMD4Digest= ($04,$3f,$85,$82,$f2,$41,$db,$35,$1c,$e6,$27,$e1,$53,$e7,$f0,$e4);

  function SingleTest(s: Str127; TDig: TMD4Digest): boolean;
    {-do a single test, const not allowed for VER<7}
    { Two sub tests: 1. whole string, 2. one update per char}
  var
    Context: TED2KContext;
    ResRec: TED2KResult;
    i: integer;
  begin
    {1. Hash complete string}
    ED2K_Full(ResRec, @s[1],length(s));
    {Compare with known value}
    for i:=0 to sizeof(TDig)-1 do begin
      if ResRec.eDonkey[i]<>TDig[i] then begin
        SingleTest := false;
        exit;
      end;
    end;
    {2. one update call for all chars}
    ED2K_Init(Context);
    for i:=1 to length(s) do ED2K_Update(Context,@s[i],1);
    ED2K_Final(Context,ResRec);
    {Compare with known value}
    for i:=0 to sizeof(TDig)-1 do begin
      if ResRec.eDonkey[i]<>TDig[i] then begin
        SingleTest := false;
        exit;
      end;
    end;
    SingleTest := true;
  end;

begin
  {Note 1: the digests of the test strings are taken from}
  {fsum from slavasoft.com and ed2k_hash.exe from Ref.[2]}

  {Note 2: the digests of the two test strings are nothing but MD4 digests!}
  {A true test for ED2K should use buffers and/or files with at least the}
  {following sizes: n*9728000+k bytes; n=0,1,2; k=-1,0,1; see t_ed2kv.pas}
  ED2K_SelfTest := SingleTest(s1, D1) and SingleTest(s2, D2);
end;


{---------------------------------------------------------------------------}
procedure ED2K_FullXL(var ResRec: TED2KResult; Msg: pointer; Len: longint);
  {-eDonkey/eMule hash of Msg with init/update/final}
var
  Context: TED2KContext;
begin
  ED2K_Init(Context);
  ED2K_UpdateXL(Context, Msg, Len);
  ED2K_Final(Context, ResRec);
end;


{---------------------------------------------------------------------------}
procedure ED2K_Full(var ResRec: TED2KResult; Msg: pointer; Len: word);
  {-eDonkey/eMule hash of Msg with init/update/final}
begin
  ED2K_FullXL(ResRec, Msg, Len);
end;


{$i-} {Force I-}
{---------------------------------------------------------------------------}
procedure ED2K_File({$ifdef CONST} const {$endif} fname: string;
                    var ResRec: TED2KResult; var buf; bsize: word; var Err: word);
  {-eDonkey/eMule hash of file, buf: buffer with at least bsize bytes}
var
  {$ifdef VirtualPascal}
    fms: word;
  {$else}
    fms: byte;
  {$endif}
  {$ifndef BIT16}
    L: longint;
  {$else}
    L: word;
  {$endif}
  Context: TED2KContext;
  f: file;
begin
  fms := FileMode;
  {$ifdef VirtualPascal}
    FileMode := $40; {open_access_ReadOnly or open_share_DenyNone;}
  {$else}
    FileMode := 0;
  {$endif}
  system.assign(f,{$ifdef D12Plus} string {$endif} (fname));
  system.reset(f,1);
  Err := IOResult;
  FileMode := fms;
  if Err<>0 then exit;
  ED2K_Init(Context);
  L := bsize;
  while (Err=0) and (L=bsize) do begin
    system.blockread(f,buf,bsize,L);
    Err := IOResult;
    ED2K_UpdateXL(Context, @buf, L);
  end;
  system.close(f);
  if IOResult=0 then;
  ED2K_Final(Context, ResRec);
end;

end.
