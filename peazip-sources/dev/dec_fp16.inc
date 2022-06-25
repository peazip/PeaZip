(*************************************************************************
 Include file for AES_DECR.PAS - AES_Decrypt for Pascal16/Full tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version from AES_DECR.PAS
 0.11     15.11.08  we          Use Ptr2Inc from BTypes
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
    TWA4(t)[3] := Td0[s[3*4+0]] xor Td1[s[2*4+1]] xor Td2[s[1*4+2]] xor Td3[s[0*4+3]] xor pK^[3];
    TWA4(t)[2] := Td0[s[2*4+0]] xor Td1[s[1*4+1]] xor Td2[s[0*4+2]] xor Td3[s[3*4+3]] xor pK^[2];
    TWA4(t)[1] := Td0[s[1*4+0]] xor Td1[s[0*4+1]] xor Td2[s[3*4+2]] xor Td3[s[2*4+3]] xor pK^[1];
    TWA4(t)[0] := Td0[s[0*4+0]] xor Td1[s[3*4+1]] xor Td2[s[2*4+2]] xor Td3[s[1*4+3]] xor pK^[0];
    dec(Ptr2Inc(pK), 4*sizeof(longint));
    dec(r);
    if r<1 then goto done;
    TWA4(s)[3] := Td0[t[3*4+0]] xor Td1[t[2*4+1]] xor Td2[t[1*4+2]] xor Td3[t[0*4+3]] xor pK^[3];
    TWA4(s)[2] := Td0[t[2*4+0]] xor Td1[t[1*4+1]] xor Td2[t[0*4+2]] xor Td3[t[3*4+3]] xor pK^[2];
    TWA4(s)[1] := Td0[t[1*4+0]] xor Td1[t[0*4+1]] xor Td2[t[3*4+2]] xor Td3[t[2*4+3]] xor pK^[1];
    TWA4(s)[0] := Td0[t[0*4+0]] xor Td1[t[3*4+1]] xor Td2[t[2*4+2]] xor Td3[t[1*4+3]] xor pK^[0];
    dec(Ptr2Inc(pK), 4*sizeof(longint));
    dec(r);
  end;

done:

  s[00] := InvSBox[t[0*4+0]];
  s[01] := InvSBox[t[3*4+1]];
  s[02] := InvSBox[t[2*4+2]];
  s[03] := InvSBox[t[1*4+3]];
  s[04] := InvSBox[t[1*4+0]];
  s[05] := InvSBox[t[0*4+1]];
  s[06] := InvSBox[t[3*4+2]];
  s[07] := InvSBox[t[2*4+3]];
  s[08] := InvSBox[t[2*4+0]];
  s[09] := InvSBox[t[1*4+1]];
  s[10] := InvSBox[t[0*4+2]];
  s[11] := InvSBox[t[3*4+3]];
  s[12] := InvSBox[t[3*4+0]];
  s[13] := InvSBox[t[2*4+1]];
  s[14] := InvSBox[t[1*4+2]];
  s[15] := InvSBox[t[0*4+3]];

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
      TAWK(RK)[i] := Td3[SBox[t[3]]] xor Td2[SBox[t[2]]] xor Td1[SBox[t[1]]] xor Td0[SBox[t[0]]];
    end;
  end;
end;


