(*************************************************************************
 Include file for AES_DECR.PAS - AES_Decrypt for BIT32/Compressed tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version for compressed tables
 0.11     09.07.06  we          Removed AES_LONGBOX code
 0.12     13.07.06  we          Uses TCd box byte instead of InvSBox
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)


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
    t[3] := Td[s3 and $ff].D0.L xor Td[s2 shr 8 and $ff].D1.L xor Td[s1 shr 16 and $ff].D2.L xor Td[s0 shr 24].D3.L xor pK^[3];
    t[2] := Td[s2 and $ff].D0.L xor Td[s1 shr 8 and $ff].D1.L xor Td[s0 shr 16 and $ff].D2.L xor Td[s3 shr 24].D3.L xor pK^[2];
    t[1] := Td[s1 and $ff].D0.L xor Td[s0 shr 8 and $ff].D1.L xor Td[s3 shr 16 and $ff].D2.L xor Td[s2 shr 24].D3.L xor pK^[1];
    s0   := Td[s0 and $ff].D0.L xor Td[s3 shr 8 and $ff].D1.L xor Td[s2 shr 16 and $ff].D2.L xor Td[s1 shr 24].D3.L xor pK^[0];
    s1   := t[1];
    s2   := t[2];
    s3   := t[3];
    dec(pK);
  end;

  {Uses InvSbox byte from Td and shl, needs type cast longint() for 16 bit compilers}
  TWA4(BO)[0] := (longint(Td[s0        and $ff].D0.box)        xor
                  longint(Td[s3 shr  8 and $ff].D0.box) shl  8 xor
                  longint(Td[s2 shr 16 and $ff].D0.box) shl 16 xor
                  longint(Td[s1 shr 24        ].D0.box) shl 24    ) xor pK^[0];
  TWA4(BO)[1] := (longint(Td[s1        and $ff].D0.box)        xor
                  longint(Td[s0 shr  8 and $ff].D0.box) shl  8 xor
                  longint(Td[s3 shr 16 and $ff].D0.box) shl 16 xor
                  longint(Td[s2 shr 24        ].D0.box) shl 24    ) xor pK^[1];
  TWA4(BO)[2] := (longint(Td[s2 and $ff       ].D0.box)        xor
                  longint(Td[s1 shr  8 and $ff].D0.box) shl  8 xor
                  longint(Td[s0 shr 16 and $ff].D0.box) shl 16 xor
                  longint(Td[s3 shr 24        ].D0.box) shl 24    ) xor pK^[2];
  TWA4(BO)[3] := (longint(Td[s3 and $ff       ].D0.box)        xor
                  longint(Td[s2 shr  8 and $ff].D0.box) shl  8 xor
                  longint(Td[s1 shr 16 and $ff].D0.box) shl 16 xor
                  longint(Td[s0 shr 24        ].D0.box) shl 24    ) xor pK^[3];

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
    p^ := Td[SBox[x shr 24]].D3.L xor Td[SBox[x shr 16 and $ff]].D2.L xor
          Td[SBox[x shr 8 and $ff]].D1.L xor Td[SBox[x and $ff]].D0.L;
    inc(p);
  end;
end;

