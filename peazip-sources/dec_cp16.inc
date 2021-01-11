
(*************************************************************************
 Include file for AES_DECR.PAS - AES_Decrypt for Pascal16/Compressed tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version for compressed tables
 0.11     13.07.06  we          Uses TDe box byte instead of InvSBox
 0.12     15.11.08  we          Use Ptr2Inc from BTypes
**************************************************************************)

(**** (C) Copyright 2002-2008 Wolfgang Ehrhardt -- see copying_we.txt ****)


{Normally used for TP5/5.5 (and during development BP7)}

{---------------------------------------------------------------------------}
procedure AES_Decrypt(var ctx: TAESContext; {$ifdef CONST} const {$else} var {$endif} BI: TAESBlock; var BO: TAESBlock);
  {-decrypt one block (in ECB mode)}
label done;
var
  r: integer;
  pK: PWA4;       {pointer to loop rount key   }
  s,t: TAESBlock;
begin
  {Setup key pointer}
  pK := PWA4(@ctx.RK[ctx.Rounds]);
  {Initialize with input block}
  TWA4(s)[0] := TWA4(BI)[0] xor pK^[0];
  TWA4(s)[1] := TWA4(BI)[1] xor pK^[1];
  TWA4(s)[2] := TWA4(BI)[2] xor pK^[2];
  TWA4(s)[3] := TWA4(BI)[3] xor pK^[3];
  dec(Ptr2Inc(pK), 4*sizeof(longint));
  r := ctx.Rounds-1;
  while true do begin
    TWA4(t)[3] := Td[s[3*4+0]].D0.L xor Td[s[2*4+1]].D1.L xor Td[s[1*4+2]].D2.L xor Td[s[0*4+3]].D3.L xor pK^[3];
    TWA4(t)[2] := Td[s[2*4+0]].D0.L xor Td[s[1*4+1]].D1.L xor Td[s[0*4+2]].D2.L xor Td[s[3*4+3]].D3.L xor pK^[2];
    TWA4(t)[1] := Td[s[1*4+0]].D0.L xor Td[s[0*4+1]].D1.L xor Td[s[3*4+2]].D2.L xor Td[s[2*4+3]].D3.L xor pK^[1];
    TWA4(t)[0] := Td[s[0*4+0]].D0.L xor Td[s[3*4+1]].D1.L xor Td[s[2*4+2]].D2.L xor Td[s[1*4+3]].D3.L xor pK^[0];
    dec(Ptr2Inc(pK), 4*sizeof(longint));
    dec(r);
    if r<1 then goto done;
    TWA4(s)[3] := Td[t[3*4+0]].D0.L xor Td[t[2*4+1]].D1.L xor Td[t[1*4+2]].D2.L xor Td[t[0*4+3]].D3.L xor pK^[3];
    TWA4(s)[2] := Td[t[2*4+0]].D0.L xor Td[t[1*4+1]].D1.L xor Td[t[0*4+2]].D2.L xor Td[t[3*4+3]].D3.L xor pK^[2];
    TWA4(s)[1] := Td[t[1*4+0]].D0.L xor Td[t[0*4+1]].D1.L xor Td[t[3*4+2]].D2.L xor Td[t[2*4+3]].D3.L xor pK^[1];
    TWA4(s)[0] := Td[t[0*4+0]].D0.L xor Td[t[3*4+1]].D1.L xor Td[t[2*4+2]].D2.L xor Td[t[1*4+3]].D3.L xor pK^[0];
    dec(Ptr2Inc(pK), 4*sizeof(longint));
    dec(r);
  end;

done:

  s[00] := Td[t[0*4+0]].D0.box;
  s[01] := Td[t[3*4+1]].D0.box;
  s[02] := Td[t[2*4+2]].D0.box;
  s[03] := Td[t[1*4+3]].D0.box;
  s[04] := Td[t[1*4+0]].D0.box;
  s[05] := Td[t[0*4+1]].D0.box;
  s[06] := Td[t[3*4+2]].D0.box;
  s[07] := Td[t[2*4+3]].D0.box;
  s[08] := Td[t[2*4+0]].D0.box;
  s[09] := Td[t[1*4+1]].D0.box;
  s[10] := Td[t[0*4+2]].D0.box;
  s[11] := Td[t[3*4+3]].D0.box;
  s[12] := Td[t[3*4+0]].D0.box;
  s[13] := Td[t[2*4+1]].D0.box;
  s[14] := Td[t[1*4+2]].D0.box;
  s[15] := Td[t[0*4+3]].D0.box;

  TWA4(BO)[0] := TWA4(s)[0] xor pK^[0];
  TWA4(BO)[1] := TWA4(s)[1] xor pK^[1];
  TWA4(BO)[2] := TWA4(s)[2] xor pK^[2];
  TWA4(BO)[3] := TWA4(s)[3] xor pK^[3];
end;


{---------------------------------------------------------------------------}
procedure MakeDecrKey(var ctx: TAESContext);
  {-Calculate decryption key from encryption key}
var
  i: integer;
  x: longint;
  t: TBA4 absolute x;
begin
  with ctx do begin
    for i:=4 to 4*Rounds-1 do begin
      {Inverse MixColumns transformation: use Sbox and}
      {implicit endian conversion compared with [2]   }
      x := TAWK(RK)[i];
      TAWK(RK)[i] := Td[SBox[t[3]]].D3.L xor Td[SBox[t[2]]].D2.L xor Td[SBox[t[1]]].D1.L xor Td[SBox[t[0]]].D0.L;
    end;
  end;
end;

