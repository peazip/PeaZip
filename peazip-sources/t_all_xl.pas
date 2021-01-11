{-Test prog for CRC/HASH-XL, we 20.08.03}
{ Aug 2006: print error parameters}

program t_all_xl;

{$i STD.INC}

{$ifdef APPCONS}
 {$apptype console}
{$endif}


uses
  hash,
  crc64,
  rmd160,
  sha224,
  sha256,
  sha384,
  sha512,
  whirl512,
  sha1,
  ed2k,
  md4,
  md5,
  adler32,
  fcrc32,
  crc32,
  crc24,
  crc16,
  bcrc32,
  bcrc64,
  Mem_Util;

const
  fname = '#bigrand';
  bsize = $F000;

var
  f: file;
  tst: packed array[1..$30000] of byte;
  i: longint;
  Err: word;
  fbuf: array[1..bsize] of byte;
  passed: boolean;


procedure Test_CRC16;
var
  fc,bc: word;
begin
  CRC16File(fname, fc, fbuf, bsize, Err);
  CRC16FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then writeln('CRC16     test passed : ', passed)
  else begin
    writeln('** CRC16, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_CRC24;
var
  fc,bc: longint;
begin
  CRC24File(fname, fc, fbuf, bsize, Err);
  CRC24FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then writeln('CRC24     test passed : ', passed)
  else begin
    writeln('** CRC24, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_CRC32;
var
  fc,bc: longint;
begin
  CRC32File(fname, fc, fbuf, bsize, Err);
  CRC32FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then writeln('CRC32     test passed : ', passed)
  else begin
    writeln('** CRC32, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_FCRC32;
var
  fc,bc: longint;
begin
  FCRC32File(fname, fc, fbuf, bsize, Err);
  FCRC32FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then writeln('FCRC32    test passed : ', passed)
  else begin
    writeln('** FCRC32, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_bCRC32;
var
  fc,bc: longint;
begin
  bCRC32File(fname, fc, fbuf, bsize, Err);
  bCRC32FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then writeln('bCRC32    test passed : ', passed)
  else begin
    writeln('** bCRC32, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_bCRC64;
var
  fc,bc: TCRC64b;
begin
  bCRC64File(fname, fc, fbuf, bsize, Err);
  bCRC64FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('bCRC64    test passed : ',passed)
  else begin
    writeln('** bCRC64, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_CRC64;
var
  fc,bc: TCRC64;
begin
  CRC64File(fname, fc, fbuf, bsize, Err);
  CRC64FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('CRC64     test passed : ',passed)
  else begin
    writeln('** CRC64, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_Adler32;
var
  fc,bc: longint;
begin
  Adler32File(fname, fc, fbuf, bsize, Err);
  Adler32FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and (bc=fc);
  if passed then  writeln('Adler32   test passed : ', passed)
  else begin
    writeln('** Adler32, Err=',Err);
    writeln('   bc=',bc);
    writeln('   fc=',fc);
  end;
end;


procedure Test_md5;
var
  fc,bc: TMD5Digest;
begin
  MD5File(fname, fc, fbuf, bsize, Err);
  MD5FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('MD5       test passed : ',  passed)
  else begin
    writeln('** MD5, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_MD4;
var
  fc,bc: TMD4Digest;
begin
  MD4File(fname, fc, fbuf, bsize, Err);
  MD4FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('MD4       test passed : ',  passed)
  else begin
    writeln('** MD4, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_ED2K;
var
  fc,bc: TED2KResult;
begin
  ED2K_File(fname, fc, fbuf, bsize, Err);
  ED2K_FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('eDonkey   test passed : ',  passed)
  else begin
    writeln('** eDonkey, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_rmd160;
var
  fc,bc: Trmd160Digest;
begin
  rmd160File(fname, fc, fbuf, bsize, Err);
  rmd160FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('RIPEMD160 test passed : ',  passed)
  else begin
    writeln('** RIPEMD160, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_sha1;
var
  fc,bc: TSHA1Digest;
begin
  SHA1File(fname, fc, fbuf, bsize, Err);
  SHA1FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('SHA1      test passed : ',  passed)
  else begin
    writeln('** SHA1, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_sha224;
var
  fc,bc: TSHA224Digest;
begin
  SHA224File(fname, fc, fbuf, bsize, Err);
  SHA224FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('SHA224    test passed : ',  passed)
  else begin
    writeln('** SHA224, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_sha256;
var
  fc,bc: TSHA256Digest;
begin
  SHA256File(fname, fc, fbuf, bsize, Err);
  SHA256FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('SHA256    test passed : ',  passed)
  else begin
    writeln('** SHA256, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_sha384;
var
  fc,bc: TSHA384Digest;
begin
  SHA384File(fname, fc, fbuf, bsize, Err);
  SHA384FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('SHA384    test passed : ',  passed)
  else begin
    writeln('** SHA384, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_sha512;
var
  fc,bc: TSHA512Digest;
begin
  SHA512File(fname, fc, fbuf, bsize, Err);
  SHA512FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('SHA512    test passed : ',  passed)
  else begin
    writeln('** SHA512, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;


procedure Test_Whirl;
var
  fc,bc: TWhirlDigest;
begin
  Whirl_File(fname, fc, fbuf, bsize, Err);
  Whirl_FullXL(bc, @tst, sizeof(tst));
  passed := (Err=0) and CompMem(@bc, @fc, sizeof(fc));
  if passed then writeln('Whirlpool test passed : ',  passed)
  else begin
    writeln('** Whirlpool, Err=',Err);
    writeln('   bc=',HexStr(@bc,sizeof(fc)));
    writeln('   fc=',HexStr(@fc,sizeof(fc)));
  end;
end;



begin
  RandSeed := 0;
  for i:=1 to sizeof(tst) do tst[i] := random(256);
  assign(f, fname);
  rewrite(f,1);
  blockwrite(f,tst,sizeof(tst));
  close(f);
  writeln('CRC/Hash test program    (c) 2002-2008 W.Ehrhardt');
  writeln;
  Test_CRC16;
  Test_CRC24;
  Test_CRC32;
  Test_bCRC32;
  Test_FCRC32;
  Test_CRC64;
  Test_bCRC64;
  Test_Adler32;
  writeln;
  Test_ed2k;
  Test_md4;
  Test_md5;
  Test_sha1;
  Test_rmd160;
  Test_sha224;
  Test_sha256;
  Test_sha384;
  Test_sha512;
  Test_Whirl;
end.
