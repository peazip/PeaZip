{-Test prog for hash OID vectors, we 2007-2015}

program t_pbkdf;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  Hash,
  {include hash units in order to register the hash descriptors}
  MD4, MD5, RMD160, SHA1, SHA224, SHA256, SHA384,
  SHA512, SHA5_224, SHA5_256, Whirl512,
  SHA3_224, SHA3_256, SHA3_384, SHA3_512,
  Blaks224, Blaks256, Blakb384, Blakb512,
  Mem_Util;


{---------------------------------------------------------------------------}
procedure OID_Check;
  {-Simple check OID1 .. OIDLen >=0, remaining < 0}
var
  algo : THashAlgorithm;
  phash: PHashDesc;
  i,ne: word;
begin
  for algo := C_MinHash to C_MaxHash do begin
    phash := findhash_by_id(algo);
    if phash=nil then writeln('Hash #',ord(algo),' not registered/found [FALSE]')
    else with phash^ do begin
      write(HName:12);
      ne := 0;
      for i:=1 to HLenOID do begin
        if HPtrOID^[i]<0 then begin
          write(i:3,'! ');
          inc(ne);
        end;
      end;
      for i:=HLenOID+1 to MaxOIDLen do begin
        if HPtrOID^[i]>=0 then begin
          write(i:3,'* ');
          inc(ne);
        end;
      end;
      writeln(' - ',ne=0);
    end;
  end;
end;

begin
  writeln('==== sanitiy check OID vectors ======');
  OID_Check;
end.
