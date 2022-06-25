(*************************************************************************
 Include file for AES_DECR.PAS - AES_Decrypt for BIT32/Full tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version from AES_DECR.PAS
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)

{ 32 Bit code: Alternative versions can be found in options.zip
  dec_full.inc - fully unrolled version for highest speed
  dec_ptr.inc  - pointer version (may be faster on some systems)
}


{---------------------------------------------------------------------------}
procedure AES_Decrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-decrypt one block (in ECB mode)}
var
  r: integer;              {round loop countdown counter}
  pK: PWA4;                {pointer to loop rount key   }
  s0,s1,s2,s3: longint;    {TAESBlock s as separate variables}
  t: TWA4;
begin

  {Setup key pointer}
  pK := PWA4(@ctx.RK[ctx.Rounds]);

  {Initialize with input block}
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];

  dec(pK);
  {perform encryption rounds}
  for r:=1 to ctx.Rounds-1  do begin
    t[3] := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[3];
    t[2] := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[2];
    t[1] := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[1];
    s0   := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[0];
    s1   := t[1];
    s2   := t[2];
    s3   := t[3];
    dec(pK);
  end;

{$ifdef AES_LONGBOX}
  {Use expanded longint InvSBox table Td4 from [2]}
  TWA4(BO)[0] := (Td4[s0        and $ff] and X000000ff) xor
                 (Td4[s3 shr  8 and $ff] and X0000ff00) xor
                 (Td4[s2 shr 16 and $ff] and X00ff0000) xor
                 (Td4[s1 shr 24        ] and Xff000000) xor pK^[0];
  TWA4(BO)[1] := (Td4[s1        and $ff] and X000000ff) xor
                 (Td4[s0 shr  8 and $ff] and X0000ff00) xor
                 (Td4[s3 shr 16 and $ff] and X00ff0000) xor
                 (Td4[s2 shr 24        ] and Xff000000) xor pK^[1];
  TWA4(BO)[2] := (Td4[s2 and $ff       ] and X000000ff) xor
                 (Td4[s1 shr  8 and $ff] and X0000ff00) xor
                 (Td4[s0 shr 16 and $ff] and X00ff0000) xor
                 (Td4[s3 shr 24        ] and Xff000000) xor pK^[2];
  TWA4(BO)[3] := (Td4[s3 and $ff       ] and X000000ff) xor
                 (Td4[s2 shr  8 and $ff] and X0000ff00) xor
                 (Td4[s1 shr 16 and $ff] and X00ff0000) xor
                 (Td4[s0 shr 24        ] and Xff000000) xor pK^[3];
{$else}
  {Uses InvSbox and shl, needs type cast longint() for   }
  {16 bit compilers: here InvSbox is byte, Td4 is longint}
  TWA4(BO)[0] := (longint(InvSBox[s0        and $ff])        xor
                  longint(InvSBox[s3 shr  8 and $ff]) shl  8 xor
                  longint(InvSBox[s2 shr 16 and $ff]) shl 16 xor
                  longint(InvSBox[s1 shr 24        ]) shl 24    ) xor pK^[0];
  TWA4(BO)[1] := (longint(InvSBox[s1        and $ff])        xor
                  longint(InvSBox[s0 shr  8 and $ff]) shl  8 xor
                  longint(InvSBox[s3 shr 16 and $ff]) shl 16 xor
                  longint(InvSBox[s2 shr 24        ]) shl 24    ) xor pK^[1];
  TWA4(BO)[2] := (longint(InvSBox[s2 and $ff       ])        xor
                  longint(InvSBox[s1 shr  8 and $ff]) shl  8 xor
                  longint(InvSBox[s0 shr 16 and $ff]) shl 16 xor
                  longint(InvSBox[s3 shr 24        ]) shl 24    ) xor pK^[2];
  TWA4(BO)[3] := (longint(InvSBox[s3 and $ff       ])        xor
                  longint(InvSBox[s2 shr  8 and $ff]) shl  8 xor
                  longint(InvSBox[s1 shr 16 and $ff]) shl 16 xor
                  longint(InvSBox[s0 shr 24        ]) shl 24    ) xor pK^[3];
{$endif}

end;


{---------------------------------------------------------------------------}
procedure MakeDecrKey(var ctx: TAESContext);
  {-Calculate decryption key from encryption key}
var
  i: integer;
  p: PLong;
  x: longint;
begin
  p := PLong(@ctx.RK[1]);
  for i:=1 to 4*(ctx.Rounds-1) do begin
    x  := p^;
    p^ := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]] xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    inc(p);
  end;
end;

