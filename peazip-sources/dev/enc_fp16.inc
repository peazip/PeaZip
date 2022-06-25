(*************************************************************************
 Include file for AES_ENCR.PAS - AES_Encrypt for Pascal16/Full tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version from AES_ENCR.PAS
 0.11     16.11.08  we          Use Ptr2Inc from BTypes
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)


{Normally used for TP5/5.5 (and during development BP7)}

{---------------------------------------------------------------------------}
procedure AES_Encrypt(var ctx: TAESContext; {$ifdef CONST} const {$else} var {$endif} BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
label done;
var
  pK: PWA4;       {pointer to loop rount key}
  r: integer;
  t,s: TAESBlock;
begin
  {Setup key pointer}
  pK := PWA4(@ctx.RK);
  {Initialize with input block}
  TWA4(s)[0] := TWA4(BI)[0] xor pK^[0];
  TWA4(s)[1] := TWA4(BI)[1] xor pK^[1];
  TWA4(s)[2] := TWA4(BI)[2] xor pK^[2];
  TWA4(s)[3] := TWA4(BI)[3] xor pK^[3];
  inc(Ptr2Inc(pK), 4*sizeof(longint));
  r := 1;
  while true do begin
    TWA4(t)[0] := Te0[s[0*4+0]] xor Te1[s[1*4+1]] xor Te2[s[2*4+2]] xor Te3[s[3*4+3]] xor pK^[0];
    TWA4(t)[1] := Te0[s[1*4+0]] xor Te1[s[2*4+1]] xor Te2[s[3*4+2]] xor Te3[s[0*4+3]] xor pK^[1];
    TWA4(t)[2] := Te0[s[2*4+0]] xor Te1[s[3*4+1]] xor Te2[s[0*4+2]] xor Te3[s[1*4+3]] xor pK^[2];
    TWA4(t)[3] := Te0[s[3*4+0]] xor Te1[s[0*4+1]] xor Te2[s[1*4+2]] xor Te3[s[2*4+3]] xor pK^[3];
    inc(Ptr2Inc(pK), 4*sizeof(longint));
    inc(r);
    if r>=ctx.rounds then goto done;
    TWA4(s)[0] := Te0[t[0*4+0]] xor Te1[t[1*4+1]] xor Te2[t[2*4+2]] xor Te3[t[3*4+3]] xor pK^[0];
    TWA4(s)[1] := Te0[t[1*4+0]] xor Te1[t[2*4+1]] xor Te2[t[3*4+2]] xor Te3[t[0*4+3]] xor pK^[1];
    TWA4(s)[2] := Te0[t[2*4+0]] xor Te1[t[3*4+1]] xor Te2[t[0*4+2]] xor Te3[t[1*4+3]] xor pK^[2];
    TWA4(s)[3] := Te0[t[3*4+0]] xor Te1[t[0*4+1]] xor Te2[t[1*4+2]] xor Te3[t[2*4+3]] xor pK^[3];
    inc(Ptr2Inc(pK), 4*sizeof(longint));
    inc(r);
  end;

done:

  s[00] := SBox[t[0*4+0]];
  s[01] := SBox[t[1*4+1]];
  s[02] := SBox[t[2*4+2]];
  s[03] := SBox[t[3*4+3]];
  s[04] := SBox[t[1*4+0]];
  s[05] := SBox[t[2*4+1]];
  s[06] := SBox[t[3*4+2]];
  s[07] := SBox[t[0*4+3]];
  s[08] := SBox[t[2*4+0]];
  s[09] := SBox[t[3*4+1]];
  s[10] := SBox[t[0*4+2]];
  s[11] := SBox[t[1*4+3]];
  s[12] := SBox[t[3*4+0]];
  s[13] := SBox[t[0*4+1]];
  s[14] := SBox[t[1*4+2]];
  s[15] := SBox[t[2*4+3]];

  TWA4(BO)[0] := TWA4(s)[0] xor pK^[0];
  TWA4(BO)[1] := TWA4(s)[1] xor pK^[1];
  TWA4(BO)[2] := TWA4(s)[2] xor pK^[2];
  TWA4(BO)[3] := TWA4(s)[3] xor pK^[3];
end;
