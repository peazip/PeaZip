{-Test prog for CRC24, we 02.04.06}

program t_crc24c;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  mem_util, crc24;

type
 TTestRec = record
              hs : string[80];
              c24: longint;
            end;

{Public domain Botan CRC24 test vectors from (validate.dat)}
{Botan Version 1.5.5, Feb 2006, http://botan.randombit.net/}

const
  anztst = 32;
  tests  : array[1..anztst] of TTestRec= (
             (hs:''; c24:$B704CE),
             (hs:'A6'; c24:$47CC72),
             (hs:'E5B0'; c24:$5AB692),
             (hs:'C52385'; c24:$9B469A),
             (hs:'8E69FB27'; c24:$2B8005),
             (hs:'4A87C90369'; c24:$452CCE),
             (hs:'DC10A8E4AC5D'; c24:$60375E),
             (hs:'6B48125403AF14'; c24:$D8948C),
             (hs:'0BDE9946163348C4'; c24:$6DC026),
             (hs:'384EB1EC12064AED51'; c24:$29B27C),
             (hs:'AF4F16B80F0AC808B595'; c24:$1158E2),
             (hs:'0C571D708F3CB0A234196C'; c24:$24D7C1),
             (hs:'7FE77AEAD433AD02533EFEFD'; c24:$E25DC7),
             (hs:'D990831F658D306DEF60A10D5C'; c24:$E0C845),
             (hs:'826CE458F0983CB46E68BA73D27A'; c24:$BB86BD),
             (hs:'995BFA6433F4185E76DC67F6D30C35'; c24:$AEC6AB),
             (hs:'C7688868D06251C906A919F011ED7FFE'; c24:$B9B9EF),
             (hs:'AA1E9FECA8E4FE3CE31928FC262A4015B8'; c24:$ED74AD),
             (hs:'5273E2B97EC0AB37B1F3004C1BF69E7A0C77'; c24:$BE8A43),
             (hs:'2F68AA285719E8F8B6C1C01227EC83F2C6ABCB'; c24:$A852E2),
             (hs:'CFD75FCCDAF1980008AD554AC7FAEE2E60317121'; c24:$D6F9B5),
             (hs:'29AD5BCF5F4FD710F0CD859DF62AD678F66DEFC0FF'; c24:$3C15F4),
             (hs:'C70CE8FB338D808338FA51A0125FA9537206EB6961C9'; c24:$A210AE),
             (hs:'141240BD7DFAF34FB8AA5ED107A899AE8CC29201B4CF32'; c24:$822081),
             (hs:'10C4F0E357A90F7FAC078282A92E9E10E347D356199427B9'; c24:$D7C1A5),
             (hs:'A982A5BEB78D751C6999B8F487F2A186CE587A0B558F3BD39D'; c24:$1AC0DF),
             (hs:'D6AA6D70B1E5DC2985D0952FCCDB6227F1AEB01CE24B8B2C2F3B'; c24:$6BF464),
             (hs:'C95155A2CB524B40F1F7DE80D2E0718406B309FE818AF6580B88EC'; c24:$BB641B),
             (hs:'6E68D0055FA2B284183000188682F36D31B26F8D04950E381F965DBE'; c24:$C62516),
             (hs:'260D0ECAA0E8E9F595C5AC8E1D7EEA2DA7603A96BF83251B3122C80D07'; c24:$876037),
             (hs:'80E559D244C4188ED223ACF70E358CDE3110B6161F70C3289266873E7802'; c24:$B8184D),
             (hs:'637C9217C88420128CB3CACFF911FAAA2257CB093715B882AE7B708E2EFE46'; c24:$E39818)
           );

(*
RFC 2440                 OpenPGP Message Format            November 1998
6.6. Example of an ASCII Armored Message
-----BEGIN PGP MESSAGE-----
Version: OpenPrivacy 0.99
yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzSvBSFjNSiVHsuAA==
=njUN
-----END PGP MESSAGE-----
*)

const
  msg1: string[99] = 'yDgBO22WxBHv7O8X7O/jygAEzol56iUKiXmV+XmpCtmpqQUKiQrFqclFqUDBovzSvBSFjNSiVHsuAA==';
  bcrc: string[4]  = 'njUN';


(* File contain 3 chars 'abc' encrypted with password 'test'
-----BEGIN PGP MESSAGE-----
Version: GnuPG v1.4.2.2 (MingW32)
jA0EAwMCPVVYKXUJFatgyRqm3Jgt7IcM5lT8+jXjJRQnsIkU31XK7A813Q==
=ZsCC
-----END PGP MESSAGE-----
*)

const
  msg2: string[99] = 'jA0EAwMCPVVYKXUJFatgyRqm3Jgt7IcM5lT8+jXjJRQnsIkU31XK7A813Q==';

var
  s: string[255];
  i,f: integer;
  n: word;
  crc: longint;
  pgpdig: TPGPDigest;
  buf: array[0..127] of byte;
begin
  writeln('T_CRC24C - PGP CRC24 test program    (C) 2006 W.Ehrhardt');

  writeln('* CRC24 self test passed: ',CRC24SelfTest);

  s := Base64DecStr(msg1);
  CRC24Full(crc, @s[1], length(s));
  Long2PGP(crc, pgpdig);
  s := Base64Str(@pgpdig, sizeof(pgpdig));
  writeln('* Test example from RFC 2440 passed: ', s=bcrc);

  writeln('* Test PGP Ascii armor: File with 3 chars "abc" encrypted with password "test"');
  s := Base64DecStr(msg2);
  writeln('  Base64 text "', msg2,'"');
  CRC24Full(crc, @s[1], length(s));
  Long2PGP(crc, pgpdig);
  writeln('  CRC24    HEX-LSB: ', HexStr(@crc, 3));
  writeln('  CRC24    HEX-MSB: ', HexStr(@pgpdig, 3));
  writeln('  CRC24 Base64-LSB: ', Base64Str(@crc, 3));
  writeln('  CRC24 Base64-MSB: ', Base64Str(@pgpdig, sizeof(pgpdig)));
  writeln('  GnuPG Base64-CRC: ', 'ZsCC');

  writeln('* Test vectors from Botan project');
  f := 0;
  for i:=1 to anztst do begin
    Hex2Mem(tests[i].hs, @buf, sizeof(buf), n);
    CRC24Full(crc, @buf, n);
    if crc<>tests[i].c24 then begin
      write(i:3);
      inc(f);
    end;
  end;
  if f=0 then writeln('  All ',anztst,' tests passed.')
  else begin
    writeln;
    writeln('  ', f,' tests failed, ',anztst-f,' tests passed.');
  end;
end.
