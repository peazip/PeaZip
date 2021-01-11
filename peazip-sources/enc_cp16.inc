(*************************************************************************
 Include file for AES_ENCR.PAS - AES_Encrypt for Pascal16/Compressed tables

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version for compressed tables
 0.11     13.07.06  we          Uses TCe box byte instead of SBox
 0.12     15.11.08  we          Use Ptr2Inc from BTypes
**************************************************************************)

(**** (C) Copyright 2002-2008 Wolfgang Ehrhardt -- see copying_we.txt ****)


{Normally used for TP5/5.5 (and during development BP7)}

{---------------------------------------------------------------------------}
procedure AES_Encrypt(var ctx: TAESContext; {$ifdef CONST} const {$else} var {$endif} BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
label done;
var
  pK: PWA4;        {pointer to loop round key}
  r: integer;      {round loop counter}
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
    TWA4(t)[0] := Te[s[0*4+0]].E0.L xor Te[s[1*4+1]].E1.L xor Te[s[2*4+2]].E2.L xor Te[s[3*4+3]].E3.L xor pK^[0];
    TWA4(t)[1] := Te[s[1*4+0]].E0.L xor Te[s[2*4+1]].E1.L xor Te[s[3*4+2]].E2.L xor Te[s[0*4+3]].E3.L xor pK^[1];
    TWA4(t)[2] := Te[s[2*4+0]].E0.L xor Te[s[3*4+1]].E1.L xor Te[s[0*4+2]].E2.L xor Te[s[1*4+3]].E3.L xor pK^[2];
    TWA4(t)[3] := Te[s[3*4+0]].E0.L xor Te[s[0*4+1]].E1.L xor Te[s[1*4+2]].E2.L xor Te[s[2*4+3]].E3.L xor pK^[3];
    inc(Ptr2Inc(pK), 4*sizeof(longint));
    inc(r);
    if r>=ctx.rounds then goto done;
    TWA4(s)[0] := Te[t[0*4+0]].E0.L xor Te[t[1*4+1]].E1.L xor Te[t[2*4+2]].E2.L xor Te[t[3*4+3]].E3.L xor pK^[0];
    TWA4(s)[1] := Te[t[1*4+0]].E0.L xor Te[t[2*4+1]].E1.L xor Te[t[3*4+2]].E2.L xor Te[t[0*4+3]].E3.L xor pK^[1];
    TWA4(s)[2] := Te[t[2*4+0]].E0.L xor Te[t[3*4+1]].E1.L xor Te[t[0*4+2]].E2.L xor Te[t[1*4+3]].E3.L xor pK^[2];
    TWA4(s)[3] := Te[t[3*4+0]].E0.L xor Te[t[0*4+1]].E1.L xor Te[t[1*4+2]].E2.L xor Te[t[2*4+3]].E3.L xor pK^[3];
    inc(Ptr2Inc(pK), 4*sizeof(longint));
    inc(r);
  end;

done:

  s[00] := Te[t[0*4+0]].E0.box;
  s[01] := Te[t[1*4+1]].E0.box;
  s[02] := Te[t[2*4+2]].E0.box;
  s[03] := Te[t[3*4+3]].E0.box;
  s[04] := Te[t[1*4+0]].E0.box;
  s[05] := Te[t[2*4+1]].E0.box;
  s[06] := Te[t[3*4+2]].E0.box;
  s[07] := Te[t[0*4+3]].E0.box;
  s[08] := Te[t[2*4+0]].E0.box;
  s[09] := Te[t[3*4+1]].E0.box;
  s[10] := Te[t[0*4+2]].E0.box;
  s[11] := Te[t[1*4+3]].E0.box;
  s[12] := Te[t[3*4+0]].E0.box;
  s[13] := Te[t[0*4+1]].E0.box;
  s[14] := Te[t[1*4+2]].E0.box;
  s[15] := Te[t[2*4+3]].E0.box;

  TWA4(BO)[0] := TWA4(s)[0] xor pK^[0];
  TWA4(BO)[1] := TWA4(s)[1] xor pK^[1];
  TWA4(BO)[2] := TWA4(s)[2] xor pK^[2];
  TWA4(BO)[3] := TWA4(s)[3] xor pK^[3];
end;
