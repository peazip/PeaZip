(*************************************************************************
 Include file for AES_ENCR.PAS - AES_Encrypt for BIT32/Full tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version from AES_ENCR.PAS
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)


{ 32 Bit code: Alternative versions can be found in options.zip
  enc_full.inc - fully unrolled version for highest speed
  enc_ptr.inc  - pointer version (may be faster on some systems)
}


{---------------------------------------------------------------------------}
procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
var
  r: integer;              {round loop countdown counter}
  pK: PWA4;                {pointer to loop rount key   }
  s3,s0,s1,s2: longint;    {TAESBlock s as separate variables}
  t: TWA4;
begin
  {Setup key pointer}
  pK := PWA4(@ctx.RK);
  {Initialize with input block}
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];
  inc(pK);
  {perform encryption rounds}
  for r:=1 to ctx.Rounds-1 do begin
    t[0] := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[0];
    t[1] := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[1];
    t[2] := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[2];
    s3   := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[3];
    s0   := t[0];
    s1   := t[1];
    s2   := t[2];
    inc(pK);
  end;

{$ifdef AES_LONGBOX}
  {Use expanded longint SBox table Te4 from [2]}
  TWA4(BO)[0] := (Te4[s0        and $ff] and X000000ff) xor
                 (Te4[s1 shr  8 and $ff] and X0000ff00) xor
                 (Te4[s2 shr 16 and $ff] and X00ff0000) xor
                 (Te4[s3 shr 24 and $ff] and Xff000000) xor pK^[0];
  TWA4(BO)[1] := (Te4[s1        and $ff] and X000000ff) xor
                 (Te4[s2 shr  8 and $ff] and X0000ff00) xor
                 (Te4[s3 shr 16 and $ff] and X00ff0000) xor
                 (Te4[s0 shr 24 and $ff] and Xff000000) xor pK^[1];
  TWA4(BO)[2] := (Te4[s2        and $ff] and X000000ff) xor
                 (Te4[s3 shr  8 and $ff] and X0000ff00) xor
                 (Te4[s0 shr 16 and $ff] and X00ff0000) xor
                 (Te4[s1 shr 24 and $ff] and Xff000000) xor pK^[2];
  TWA4(BO)[3] := (Te4[s3        and $ff] and X000000ff) xor
                 (Te4[s0 shr  8 and $ff] and X0000ff00) xor
                 (Te4[s1 shr 16 and $ff] and X00ff0000) xor
                 (Te4[s2 shr 24 and $ff] and Xff000000) xor pK^[3];
{$else}
  {Uses Sbox and shl, needs type cast longint() for}
  {16 bit compilers: here Sbox is byte, Te4 is longint}
  TWA4(BO)[0] := (longint(SBox[s0        and $ff])        xor
                  longint(SBox[s1 shr  8 and $ff]) shl  8 xor
                  longint(SBox[s2 shr 16 and $ff]) shl 16 xor
                  longint(SBox[s3 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := (longint(SBox[s1        and $ff])        xor
                  longint(SBox[s2 shr  8 and $ff]) shl  8 xor
                  longint(SBox[s3 shr 16 and $ff]) shl 16 xor
                  longint(SBox[s0 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := (longint(SBox[s2        and $ff])        xor
                  longint(SBox[s3 shr  8 and $ff]) shl  8 xor
                  longint(SBox[s0 shr 16 and $ff]) shl 16 xor
                  longint(SBox[s1 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := (longint(SBox[s3        and $ff])        xor
                  longint(SBox[s0 shr  8 and $ff]) shl  8 xor
                  longint(SBox[s1 shr 16 and $ff]) shl 16 xor
                  longint(SBox[s2 shr 24])         shl 24    ) xor pK^[3];

{$endif}

end;

