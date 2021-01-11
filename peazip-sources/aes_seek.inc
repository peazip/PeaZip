
(*************************************************************************
 Include file for AES CTR Seek routines. These routines are in a separate inc
 file because they are included in aes_ctr.pas for non-dlls AND in the dll
 interface units AES_Intv/AES_Intf. This is done in order to make tests like
 @IncProc=@AES_IncMSBPart work without additional overhead in programs using
 the dll.

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     31.07.10  W.Ehrhardt  Initial version
**************************************************************************)

(****** (C) Copyright 2010  Wolfgang Ehrhardt -- see copying_we.txt ******)


{---------------------------------------------------------------------------}
function  AES_CTR_Seek({$ifdef CONST}const{$else}var{$endif} iCTR: TAESBlock;
                       SOL, SOH: longint; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SOH*2^32+SOL,}
  { SOH >= 0. iCTR is the initial CTR for offset 0, i.e. the same as in AES_CTR_Init.}
var
  i,pt: integer;
  carry: word;
  TC: TAESBlock;
type
  TWA4  = packed array[0..3] of longint;      {AES block as array of longint}
  TBA4  = packed array[0..3] of byte;         {AES "word" as array of byte  }
begin

  {WARNING: CTR mode demands that the same key / iCTR pair is never reused  }
  {for encryption. This requirement is especially important for the CTR_Seek}
  {function. If different data is written to the same position there will be}
  {leakage of information about the plaintexts. Therefore CTR_Seek should   }
  {normally be used for random reads only.}

  if SOH < 0 then begin
    AES_CTR_Seek := AES_Err_CTR_SeekOffset;
    exit;
  end
  else with ctx do begin
    blen := word(SOL) and $0F;
    {64 bit shift right (SOH, SOL) 4 bits}
    SOL := (SOL shr 4) or ((SOH and $0F) shl 28);
    SOH := (SOH shr 4);
    {Check if known IncProc}
    {$ifdef FPC_ProcVar}
      if (IncProc=nil) or (IncProc=@AES_IncMSBFull) then pt := 1
      else if IncProc=@AES_IncMSBPart then pt := 2
      else if IncProc=@AES_IncLSBFull then pt := 3
      else if IncProc=@AES_IncLSBPart then pt := 4
      else pt := 0;
    {$else}
      if (@IncProc=nil) or (@IncProc=@AES_IncMSBFull) then pt := 1
      else if @IncProc=@AES_IncMSBPart then pt := 2
      else if @IncProc=@AES_IncLSBFull then pt := 3
      else if @IncProc=@AES_IncLSBPart then pt := 4
      else pt := 0;
    {$endif}
    IV := iCTR;
    if (SOL or SOH) <> 0 then begin
      if pt=0 then begin
        {No shortcut calculation for user-defined IncProcs. Note: SOH is }
        {positive here even if the sign bit of the original SOH was set. }

        {The execution of this loop may be very time-consuming because the }
        {IncProc is called many times. If the user is able to calculate the}
        {value IVo of the iCTR after calling IncProc (offset div 16) times,}
        {invoking the function with  AES_CTR_Seek(IVo, SOL and 15, 0, ctx) }
        {will completely skip the IncProc calculation, but set the correct }
        {values for ctx.IV, ctx.buf, and ctx.blen.}
        if SOL=0 then dec(SOH);
        repeat
          repeat
            IncProc(IV);
            dec(SOL);
          until SOL=0;
          dec(SOH);
        until SOH<=0;
      end
      else begin
        fillchar(TC, sizeof(TC), 0);
        carry := 0;
        if (pt=1) or (pt=2) then begin
          {MSB functions, first fill 128 bit offset vector}
          for i:=0 to 3 do begin
            TC[15-i] := TBA4(SOL)[i];
            TC[11-i] := TBA4(SOH)[i];
          end;
          {64 bit addition}
          for i:=15 downto 8 do begin
            carry := carry + TC[i] + IV[i];
            IV[i] := carry and $FF;
            carry := carry shr 8;
          end;
          if (pt=1) and (carry<>0) then begin
            {"Full" function: propagate carry through remaining bytes}
            for i:=7 downto 0 do begin
              carry := carry + IV[i];
              IV[i] := carry and $FF;
              carry := carry shr 8;
              {$ifdef CONST}
                if carry=0 then break;
              {$endif}
            end;
          end;
        end
        else begin
          {LSB functions, first fill 128 bit offset vector}
          TWA4(TC)[0] := SOL;
          TWA4(TC)[1] := SOH;
          {64 bit addition}
          for i:=0 to 7 do begin
            carry := carry + TC[i] + IV[i];
            IV[i] := carry and $FF;
            carry := carry shr 8;
          end;
          if (pt=3) and (carry<>0) then begin
            {"Full" function: propagate carry through remaining bytes}
            for i:=8 to 15 do begin
              carry := carry + IV[i];
              IV[i] := carry and $FF;
              carry := carry shr 8;
              {$ifdef CONST}
                if carry=0 then break;
              {$endif}
            end;
          end;
        end;
      end;
    end;
    AES_Encrypt(ctx, IV, buf);
    AES_CTR_Seek := 0;
  end;
end;


{$ifdef HAS_INT64}
{$ifndef DLL}
{-----------------------------------------------------------------------------}
function AES_CTR_Seek64(const iCTR: TAESBlock; SO: int64; var ctx: TAESContext): integer;
  {-Setup ctx for random access crypto stream starting at 64 bit offset SO >= 0;}
  { iCTR is the initial CTR value for offset 0, i.e. the same as in AES_CTR_Init.}
type
  LH = packed record L,H: longint; end;
begin
  AES_CTR_Seek64 := AES_CTR_Seek(iCTR, LH(SO).L, LH(SO).H, ctx);
end;
{$endif}
{$endif}
