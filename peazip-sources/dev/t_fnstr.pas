{Test filename type string}

program t_fnstr;


{$i STD.INC}

{$ifdef BIT16}
  {$ifdef DPMI}
    {$M $1000}
  {$else}
    {$M $4000,0,655360}
  {$endif}
{$endif}


{$I-,V-}

{$ifndef FPC}
  {$B-,N-}
{$endif}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  CRC16, CRC24, CRC32, Adler32, CRC64, hash, MD5, RMD160,
  SHA1, SHA256, SHA224, SHA384, SHA512, SHA5_224, SHA5_256,
  SHA3_224, SHA3_256, SHA3_384, SHA3_512,
  Whirl512,MD4,ED2K,Blaks224, Blaks256, Blakb384, Blakb512,
  BTypes, mem_util;

{$ifdef J_OPT}
{$J+}
{$endif}

var
  buf: array[1..$200] of byte;


{---------------------------------------------------------------------------}
procedure Process1File({$ifdef CONST} const {$endif} FName: string);
  {-Process a single file}
var
  n, Err: Word;

  CRC16: word;
  CRC24: longint; pgpsig: TPGPDigest;
  CRC32: longint;
  Adler: longint;
  CRC64: TCRC64;
  RMD160Digest: TRMD160Digest;
  SHA1Digest: TSHA1Digest;
  SHA256Digest: TSHA256Digest;
  ED2KRes: TED2KResult;
  MD4Digest: TMD4Digest;
  MD5Digest: TMD5Digest;
  SHA224Digest: TSHA224Digest;
  SHA384Digest: TSHA384Digest;
  SHA512Digest: TSHA512Digest;
  WhirlDigest: TWhirlDigest;
  SHA5_224Digest: TSHA5_224Digest;
  SHA5_256Digest: TSHA5_256Digest;
  SHA3_224Digest: TSHA3_224Digest;
  SHA3_256Digest: TSHA3_256Digest;
  SHA3_384Digest: TSHA3_384Digest;
  SHA3_512Digest: TSHA3_512Digest;
  Blaks_224Digest: TBlake2S_224Digest;
  Blaks_256Digest: TBlake2S_256Digest;
  Blakb_384Digest: TBlake2B_384Digest;
  Blakb_512Digest: TBlake2B_512Digest;

  {----------------------------------------------------------------------}
  function RB(A: longint): longint;
    {-rotate byte of longint}
  begin
    RB := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
  end;

  procedure CheckErr;
  begin
    inc(n);
    if Err <> 0 then writeln('Alg ',n, ',  error = ',Err);
  end;

begin
  {$ifdef bit32}
    {ShareDenyNone to avoid error if redirected output is processed}
    FileMode := $40;
  {$else}
    FileMode := $0;
  {$endif}
  writeln(Fname);
  n := 0;

  RMD160File(fname, RMD160Digest, buf, sizeof(buf), err); CheckErr;
  SHA1File(fname, SHA1Digest, buf, sizeof(buf), err); CheckErr;;
  SHA256File(fname, SHA256Digest, buf, sizeof(buf), err); CheckErr;
  SHA224File(fname, SHA224Digest, buf, sizeof(buf), err); CheckErr;
  SHA384File(fname, SHA384Digest, buf, sizeof(buf), err); CheckErr;
  SHA512File(fname, SHA512Digest, buf, sizeof(buf), err); CheckErr;
  SHA5_224File(fname, SHA5_224Digest, buf, sizeof(buf), err); CheckErr;
  SHA5_256File(fname, SHA5_256Digest, buf, sizeof(buf), err); CheckErr;
  SHA3_224File(fname, SHA3_224Digest, buf, sizeof(buf), err); CheckErr;
  SHA3_256File(fname, SHA3_256Digest, buf, sizeof(buf), err); CheckErr;;
  SHA3_384File(fname, SHA3_384Digest, buf, sizeof(buf), err); CheckErr;;
  SHA3_512File(fname, SHA3_512Digest, buf, sizeof(buf), err); CheckErr;;
  Blaks224File(fname, Blaks_224Digest, buf, sizeof(buf), err); CheckErr;;
  Blaks256File(fname, Blaks_256Digest, buf, sizeof(buf), err); CheckErr;;
  Blakb384File(fname, Blakb_384Digest, buf, sizeof(buf), err); CheckErr;;
  Blakb512File(fname, Blakb_512Digest, buf, sizeof(buf), err); CheckErr;;
  Whirl_File(fname, WhirlDigest, buf, sizeof(buf), err); CheckErr;
  ED2K_File(fname, ED2KRes, buf, sizeof(buf), err); CheckErr;
  MD4File(fname, MD4Digest, buf, sizeof(buf), err); CheckErr;
  MD5File(fname, MD5Digest, buf, sizeof(buf), err); CheckErr;
  CRC16File(fname, CRC16, buf, sizeof(buf), err); CheckErr;;
  CRC24File(fname, CRC24, buf, sizeof(buf), err); CheckErr;
  CRC32File(fname, CRC32, buf, sizeof(buf), err); CheckErr;
  Adler32File(fname, adler, buf, sizeof(buf), err); CheckErr;
  CRC64File(fname, CRC64, buf, sizeof(buf), err); CheckErr;

  {swap bytes: display shall look like word / longint}
  {but HexStr constructs LSB first}
  CRC16 := swap(CRC16);
  CRC32 := RB(CRC32);
  Adler := RB(Adler);
  writeln('      CRC16: '+HexStr(@CRC16, sizeof(CRC16)));
  {special case 3 byte CRC24 use CRC24 variable or pgpsig}
  Long2PGP(CRC24, pgpsig);
  writeln('      CRC24: '+HexStr(@pgpsig, 3));
  writeln('      CRC32: '+HexStr(@CRC32, sizeof(CRC32)));
  writeln('    Adler32: '+HexStr(@adler, sizeof(adler)));
  writeln('      CRC64: '+HexStr(@CRC64, sizeof(CRC64)));
  writeln('    eDonkey: '+HexStr(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey)));
 if ED2KRes.differ then begin
  writeln('      eMule: '+HexStr(@ED2KRes.eMule, sizeof(ED2KRes.eMule)));
 end;
  writeln('        MD4: '+HexStr(@MD4Digest, sizeof(MD4Digest)));
  writeln('        MD5: '+HexStr(@MD5Digest, sizeof(MD5Digest)));
  writeln('  RIPEMD160: '+HexStr(@RMD160Digest, sizeof(RMD160Digest)));
  writeln('       SHA1: '+HexStr(@SHA1Digest, sizeof(SHA1Digest)));
  writeln('     SHA224: '+HexStr(@SHA224Digest, sizeof(SHA224Digest)));
  writeln('     SHA256: '+HexStr(@SHA256Digest, sizeof(SHA256Digest)));
  writeln('     SHA384: '+HexStr(@SHA384Digest, sizeof(SHA384Digest)));
  writeln('     SHA512: '+HexStr(@SHA512Digest, sizeof(SHA512Digest)));
  writeln(' SHA512/224: '+HexStr(@SHA5_224Digest,sizeof(SHA5_224Digest)));
  writeln(' SHA512/256: '+HexStr(@SHA5_256Digest,sizeof(SHA5_256Digest)));
  writeln('  Whirlpool: '+HexStr(@WhirlDigest, sizeof(WhirlDigest)));
  writeln('   SHA3-224: '+HexStr(@SHA3_224Digest, sizeof(SHA3_224Digest)));
  writeln('   SHA3-256: '+HexStr(@SHA3_256Digest, sizeof(SHA3_256Digest)));
  writeln('   SHA3-384: '+HexStr(@SHA3_384Digest, sizeof(SHA3_384Digest)));
  writeln('   SHA3-512: '+HexStr(@SHA3_512Digest, sizeof(SHA3_512Digest)));
  writeln('Blake2s-224: '+HexStr(@Blaks_224Digest, sizeof(Blaks_224Digest)));
  writeln('Blake2s-256: '+HexStr(@Blaks_256Digest, sizeof(Blaks_256Digest)));
  writeln('Blake2b-384: '+HexStr(@Blakb_384Digest, sizeof(Blakb_384Digest)));
  writeln('Blake2b-512: '+HexStr(@Blakb_512Digest, sizeof(Blakb_512Digest)));
  writeln;
end;


{---------------------------------------------------------------------------}
procedure usage;
begin
  writeln('Usage: t_fnstr [file1] ... [fileN]');
  halt;
end;


{---------------------------------------------------------------------------}
var
  i,n: integer;
begin
  writeln('t_fnstr test filename type string   (c) 2017 W.Ehrhardt');
  n := 0;
  for i:=1 to Paramcount do begin
    Process1File(paramstr(i));
    inc(n);
  end;
  if n=0 then usage;
end.
