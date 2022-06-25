
(*************************************************************************
 Include file for AES_ENCR.PAS - AES_Encrypt for BIT32/Compressed tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version for compressed tables
 0.11     09.07.06  we          Removed AES_LONGBOX code
 0.12     13.07.06  we          Uses TCe box byte instead of SBox
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)


{---------------------------------------------------------------------------}
procedure AES_Encrypt(var ctx: TAESContext; const BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
var
  r: integer;              {round loop countdown counter}
  pK: PWA4;                {pointer to loop round key   }
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
    t[0] := Te[s0 and $ff].E0.L xor Te[s1 shr 8 and $ff].E1.L xor Te[s2 shr 16 and $ff].E2.L xor Te[s3 shr 24].E3.L xor pK^[0];
    t[1] := Te[s1 and $ff].E0.L xor Te[s2 shr 8 and $ff].E1.L xor Te[s3 shr 16 and $ff].E2.L xor Te[s0 shr 24].E3.L xor pK^[1];
    t[2] := Te[s2 and $ff].E0.L xor Te[s3 shr 8 and $ff].E1.L xor Te[s0 shr 16 and $ff].E2.L xor Te[s1 shr 24].E3.L xor pK^[2];
    s3   := Te[s3 and $ff].E0.L xor Te[s0 shr 8 and $ff].E1.L xor Te[s1 shr 16 and $ff].E2.L xor Te[s2 shr 24].E3.L xor pK^[3];
    s0   := t[0];
    s1   := t[1];
    s2   := t[2];
    inc(pK);
  end;
  {Uses Sbox byte from Te and shl, needs type cast longint() for 16 bit compilers}
  TWA4(BO)[0] := (longint(Te[s0        and $ff].E0.box)        xor
                  longint(Te[s1 shr  8 and $ff].E0.box) shl  8 xor
                  longint(Te[s2 shr 16 and $ff].E0.box) shl 16 xor
                  longint(Te[s3 shr 24        ].E0.box) shl 24    ) xor pK^[0];
  TWA4(BO)[1] := (longint(Te[s1        and $ff].E0.box)        xor
                  longint(Te[s2 shr  8 and $ff].E0.box) shl  8 xor
                  longint(Te[s3 shr 16 and $ff].E0.box) shl 16 xor
                  longint(Te[s0 shr 24        ].E0.box) shl 24    ) xor pK^[1];
  TWA4(BO)[2] := (longint(Te[s2        and $ff].E0.box)        xor
                  longint(Te[s3 shr  8 and $ff].E0.box) shl  8 xor
                  longint(Te[s0 shr 16 and $ff].E0.box) shl 16 xor
                  longint(Te[s1 shr 24        ].E0.box) shl 24    ) xor pK^[2];
  TWA4(BO)[3] := (longint(Te[s3        and $ff].E0.box)        xor
                  longint(Te[s0 shr  8 and $ff].E0.box) shl  8 xor
                  longint(Te[s1 shr 16 and $ff].E0.box) shl 16 xor
                  longint(Te[s2 shr 24        ].E0.box) shl 24    ) xor pK^[3];

end;

