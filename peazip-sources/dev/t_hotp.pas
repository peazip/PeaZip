{-Test prog for HOTP functions, we 03.10}

program t_hotp;

{$i std.inc}

{$ifdef APPCONS}
{$apptype console}
{$endif}


uses
  hotp;

{Data from Simon Josefsson HOTP Toolkit, http://www.nongnu.org/hotp-toolkit/}
const
  tv6: array[0..19] of string[6] = (
         '755224', '287082', '359152', '969429',
         '338314', '254676', '287922', '162583',
         '399871', '520489', '403154', '481090',
         '868912', '736127', '229903', '436521',
         '186581', '447589', '903435', '578337');
  tv7: array[0..19] of string[7] = (
         '4755224', '4287082', '7359152', '6969429',
         '0338314', '8254676', '8287922', '2162583',
         '3399871', '5520489', '2403154', '3481090',
         '7868912', '3736127', '5229903', '3436521',
         '2186581', '4447589', '1903435', '1578337');
  tv8: array[0..19] of string[8] = (
         '84755224', '94287082', '37359152', '26969429',
         '40338314', '68254676', '18287922', '82162583',
         '73399871', '45520489', '72403154', '43481090',
         '47868912', '33736127', '35229903', '23436521',
         '22186581', '94447589', '71903435', '21578337');

const
  secret: string[20] = '12345678901234567890';
var
  count: THOTPCount;
  i,err: integer;
  s: string[8];
begin
  writeln('Selftest: ', hotp_selftest);
  hotp_set_count32(count,0,0);
  err := 0;
  for i:=0 to 19 do begin
    s := hotp_generate_otps(secret, count, 6, -1);
    if s<>tv6[i] then begin
      inc(err);
      writeln('Error: digits 6, i=',i:2,':  TV = ', tv6[i]:8, '  HOTP result = ', s:8);
    end;
    s := hotp_generate_otps(secret, count, 7, -1);
    if s<>tv7[i] then begin
      inc(err);
      writeln('Error: digits 7, i=',i:2,':  TV = ', tv7[i]:8, '  HOTP result = ', s:8);
    end;
    s := hotp_generate_otps(secret, count, 8, -1);
    if s<>tv8[i] then begin
      inc(err);
      writeln('Error: digits 8, i=',i:2,':  TV = ', tv8[i]:8, '  HOTP result = ', s:8);
    end;
    hotp_inc_count(count);
  end;
  if err=0 then writeln('Test OK')
  else writeln(' ** failed: ',err, ' errors!');
end.
