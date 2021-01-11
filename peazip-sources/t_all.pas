{-Test prog for CRC/HASH, we 30.08.03}

program t_all;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  hash,
  crc64,
  rmd160,
  sha224,
  sha256,
  sha384,
  sha512,
  sha5_224,
  sha5_256,
  sha3_224,
  sha3_256,
  sha3_384,
  sha3_512,
  whirl512,
  blaks224,
  blaks256,
  blakb384,
  blakb512,
  sha1,
  ed2k,
  md4,
  md5,
  adler32,
  bjl3,
  fcrc32,
  crc32,
  crc24,
  crc16,
  crc_sick,
  bcrc32,
  bcrc64;

begin
  writeln('CRC/Hash test program    (c) 2002-2017 W.Ehrhardt');
  writeln('CRC-Sick    self test passed: ', CRC_Sick_SelfTest);
  writeln('CRC16       self test passed: ', CRC16SelfTest);
  writeln('CRC24       self test passed: ', CRC24SelfTest);
  writeln('CRC32       self test passed: ', CRC32SelfTest);
  writeln('FCRC32      self test passed: ', FCRC32SelfTest);
  writeln('bCRC32      self test passed: ', bCRC32SelfTest);
  writeln('Adler32     self test passed: ', Adler32SelfTest);
  writeln('BJ lookup3  self test passed: ', BJL3SelfTest);
  writeln('CRC64       self test passed: ', CRC64SelfTest);
  writeln('bCRC64      self test passed: ', bCRC64SelfTest);
  writeln('eDonkey     self test passed: ', ED2K_SelfTest);
  writeln('MD4         self test passed: ', MD4SelfTest);
  writeln('MD5         self test passed: ', MD5SelfTest);
  writeln('RIPEMD160   self test passed: ', RMD160SelfTest);
  writeln('SHA1        self test passed: ', SHA1SelfTest);
  writeln('SHA224      self test passed: ', SHA224SelfTest);
  writeln('SHA256      self test passed: ', SHA256SelfTest);
  writeln('SHA384      self test passed: ', SHA384SelfTest);
  writeln('SHA512      self test passed: ', SHA512SelfTest);
  writeln('SHA512/224  self test passed: ', SHA5_224SelfTest);
  writeln('SHA512/256  self test passed: ', SHA5_256SelfTest);
  writeln('Whirlpool   self test passed: ', Whirl_SelfTest);
  writeln('SHA3-224    self test passed: ', SHA3_224SelfTest);
  writeln('SHA3-256    self test passed: ', SHA3_256SelfTest);
  writeln('SHA3-384    self test passed: ', SHA3_384SelfTest);
  writeln('SHA3-512    self test passed: ', SHA3_512SelfTest);
  writeln('Blake2s-224 self test passed: ', Blaks224SelfTest);
  writeln('Blake2s-256 self test passed: ', Blaks256SelfTest);
  writeln('Blake2b-384 self test passed: ', Blakb384SelfTest);
  writeln('Blake2b-512 self test passed: ', Blakb512SelfTest);
end.
