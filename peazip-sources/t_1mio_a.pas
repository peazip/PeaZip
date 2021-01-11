{-Test prog for Hash functions (10^6 'a') , we 2004-2015}

program t_1Mio_a;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  hash,
  whirl512,
  sha512,
  sha256,
  sha224,
  sha384,
  sha3_224,
  sha3_256,
  sha3_384,
  sha3_512,
  sha1,
  rmd160,
  ed2k,
  md4,
  md5,
  mem_util;

var
  ED2KContext    : TED2KContext;    ED2KRes       : TED2KResult;
  MD4Context     : THashContext;    MD4Digest     : TMD4Digest;
  MD5Context     : THashContext;    MD5Digest     : TMD5Digest;
  SHA1Context    : THashContext;    SHA1Digest    : TSHA1Digest;
  RMD160Context  : THashContext;    RMD160Digest  : TRMD160Digest;
  SHA224Context  : THashContext;    SHA224Digest  : TSHA224Digest;
  SHA256Context  : THashContext;    SHA256Digest  : TSHA256Digest;
  SHA384Context  : THashContext;    SHA384Digest  : TSHA384Digest;
  SHA512Context  : THashContext;    SHA512Digest  : TSHA512Digest;
  SHA3_224Context: THashContext;    SHA3_224Digest: TSHA3_224Digest;
  SHA3_256Context: THashContext;    SHA3_256Digest: TSHA3_256Digest;
  SHA3_384Context: THashContext;    SHA3_384Digest: TSHA3_384Digest;
  SHA3_512Context: THashContext;    SHA3_512Digest: TSHA3_512Digest;
  WhirlContext   : THashContext;    WhirlDigest   : TWhirlDigest;

var
  buf: array[1..1000] of byte;
  i: integer;

{ test values for 100000 'a' for all hashes  }
{ SHAx: values are from the specs}
{ RMD160: http://homes.esat.kuleuven.be/~bosselae/ripemd160.html}
{ Whirlpool: iso-test-vectors.txt in Whirlpool.zip}
{ MD5:  is agreed by all availables md5 summers}
{ MD4:  TSD, FSUM, CryptoBench/Crypto++}
{ EK2K: FSUM, ed2k_hash}
{ SHA-3: David Ireland's 'Test vectors for SHA-1, SHA-2 and SHA-3'}

const
  CED2K  : TMD4Digest = (
             $bb,$ce,$80,$cc,$6b,$b6,$5e,$5c,$67,$45,$e3,$0d,$4e,$ec,$a9,$a4);

  CMD4   : TMD4Digest = (
             $bb,$ce,$80,$cc,$6b,$b6,$5e,$5c,$67,$45,$e3,$0d,$4e,$ec,$a9,$a4);

  CMD5   : TMD5Digest = (
             $77,$07,$d6,$ae,$4e,$02,$7c,$70,$ee,$a2,$a9,$35,$c2,$29,$6f,$21);

  CRMD160: TRMD160Digest = (
             $52,$78,$32,$43,$c1,$69,$7b,$db,$e1,$6d,$37,$f9,$7f,$68,$f0,$83,$25,$dc,$15,$28);

  CSHA1  : TSHA1Digest = (
             $34,$aa,$97,$3c,$d4,$c4,$da,$a4,$f6,$1e,$eb,$2b,$db,$ad,$27,$31,$65,$34,$01,$6f);

  CSHA224: TSHA224Digest = (
             $20,$79,$46,$55,$98,$0c,$91,$d8,$bb,$b4,$c1,$ea,$97,$61,$8a,$4b,
             $f0,$3f,$42,$58,$19,$48,$b2,$ee,$4e,$e7,$ad,$67);

  CSHA256: TSHA256Digest = (
             $cd,$c7,$6e,$5c,$99,$14,$fb,$92,$81,$a1,$c7,$e2,$84,$d7,$3e,$67,
             $f1,$80,$9a,$48,$a4,$97,$20,$0e,$04,$6d,$39,$cc,$c7,$11,$2c,$d0);

  CSHA384: TSHA384Digest = (
             $9d,$0e,$18,$09,$71,$64,$74,$cb,$08,$6e,$83,$4e,$31,$0a,$4a,$1c,
             $ed,$14,$9e,$9c,$00,$f2,$48,$52,$79,$72,$ce,$c5,$70,$4c,$2a,$5b,
             $07,$b8,$b3,$dc,$38,$ec,$c4,$eb,$ae,$97,$dd,$d8,$7f,$3d,$89,$85);

  CSHA512: TSHA512Digest = (
             $e7,$18,$48,$3d,$0c,$e7,$69,$64,$4e,$2e,$42,$c7,$bc,$15,$b4,$63,
             $8e,$1f,$98,$b1,$3b,$20,$44,$28,$56,$32,$a8,$03,$af,$a9,$73,$eb,
             $de,$0f,$f2,$44,$87,$7e,$a6,$0a,$4c,$b0,$43,$2c,$e5,$77,$c3,$1b,
             $eb,$00,$9c,$5c,$2c,$49,$aa,$2e,$4e,$ad,$b2,$17,$ad,$8c,$c0,$9b);

  CWhirl: TWhirlDigest = (
             $0c,$99,$00,$5b,$eb,$57,$ef,$f5,$0a,$7c,$f0,$05,$56,$0d,$df,$5d,
             $29,$05,$7f,$d8,$6b,$20,$bf,$d6,$2d,$ec,$a0,$f1,$cc,$ea,$4a,$f5,
             $1f,$c1,$54,$90,$ed,$dc,$47,$af,$32,$bb,$2b,$66,$c3,$4f,$f9,$ad,
             $8c,$60,$08,$ad,$67,$7f,$77,$12,$69,$53,$b2,$26,$e4,$ed,$8b,$01);

  CSHA3_224: TSHA3_224Digest = (
             $d6,$93,$35,$b9,$33,$25,$19,$2e,$51,$6a,$91,$2e,$6d,$19,$a1,$5c,
             $b5,$1c,$6e,$d5,$c1,$52,$43,$e7,$a7,$fd,$65,$3c);

  CSHA3_256: TSHA3_256Digest = (
             $5c,$88,$75,$ae,$47,$4a,$36,$34,$ba,$4f,$d5,$5e,$c8,$5b,$ff,$d6,
             $61,$f3,$2a,$ca,$75,$c6,$d6,$99,$d0,$cd,$cb,$6c,$11,$58,$91,$c1);

  CSHA3_384: TSHA3_384Digest = (
             $ee,$e9,$e2,$4d,$78,$c1,$85,$53,$37,$98,$34,$51,$df,$97,$c8,$ad,
             $9e,$ed,$f2,$56,$c6,$33,$4f,$8e,$94,$8d,$25,$2d,$5e,$0e,$76,$84,
             $7a,$a0,$77,$4d,$db,$90,$a8,$42,$19,$0d,$2c,$55,$8b,$4b,$83,$40);

  CSHA3_512: TSHA3_512Digest = (
             $3c,$3a,$87,$6d,$a1,$40,$34,$ab,$60,$62,$7c,$07,$7b,$b9,$8f,$7e,
             $12,$0a,$2a,$53,$70,$21,$2d,$ff,$b3,$38,$5a,$18,$d4,$f3,$88,$59,
             $ed,$31,$1d,$0a,$9d,$51,$41,$ce,$9c,$c5,$c6,$6e,$e6,$89,$b2,$66,
             $a8,$aa,$18,$ac,$e8,$28,$2a,$0e,$0d,$b5,$96,$c9,$0b,$0a,$7b,$87);


begin

  writeln('Test 10^6 repetitions of "a"');
  writeln('----------------------------');
  fillchar(buf, sizeof(buf), $61 {='a'});

  {Initialize all contexts}
  ED2K_Init(ED2KContext);
  MD4Init(MD4Context);
  MD5Init(MD5Context);
  SHA1Init(SHA1Context);
  RMD160Init(RMD160Context);
  SHA3_224Init(SHA3_224Context);
  SHA3_256Init(SHA3_256Context);
  SHA3_384Init(SHA3_384Context);
  SHA3_512Init(SHA3_512Context);
  SHA224Init(SHA224Context);
  SHA256Init(SHA256Context);
  SHA384Init(SHA384Context);
  SHA512Init(SHA512Context);
  WHirl_Init(WhirlContext);

  {mix 1000*1000 'a'}
  for i:=1 to 1000 do begin
    ED2K_Update(ED2KContext,@buf,1000);
    MD4Update(MD4Context,@buf,1000);
    MD5Update(MD5Context,@buf,1000);
    SHA1Update(SHA1Context,@buf,1000);
    RMD160Update(RMD160Context,@buf,1000);
    SHA224Update(SHA224Context,@buf,1000);
    SHA256Update(SHA256Context,@buf,1000);
    SHA384Update(SHA384Context,@buf,1000);
    SHA512Update(SHA512Context,@buf,1000);
    SHA3_224Update(SHA3_224Context,@buf,1000);
    SHA3_256Update(SHA3_256Context,@buf,1000);
    SHA3_384Update(SHA3_384Context,@buf,1000);
    SHA3_512Update(SHA3_512Context,@buf,1000);
    Whirl_Update(WhirlContext,@buf,1000);
  end;

  {calculate digests}
  ED2K_Final(ED2KContext,   ED2KRes     );
  MD4Final(MD4Context,      MD4Digest   );
  MD5Final(MD5Context,      MD5Digest   );
  SHA1Final(SHA1Context,    SHA1Digest  );
  RMD160Final(RMD160Context,RMD160Digest);
  SHA224Final(SHA224Context,SHA224Digest);
  SHA256Final(SHA256Context,SHA256Digest);
  SHA384Final(SHA384Context,SHA384Digest);
  SHA512Final(SHA512Context,SHA512Digest);
  Whirl_Final(WhirlContext, WhirlDigest );
  SHA3_224Final(SHA3_224Context,SHA3_224Digest);
  SHA3_256Final(SHA3_256Context,SHA3_256Digest);
  SHA3_384Final(SHA3_384Context,SHA3_384Digest);
  SHA3_512Final(SHA3_512Context,SHA3_512Digest);

  {write test results}
  writeln('   eDonkey: ',  compmem(@ED2KRes.eDonkey, @CED2K  , sizeof(CED2K)));
  writeln('       MD4: ',  compmem(@MD4Digest   , @CMD4   , sizeof(MD4Digest   )));
  writeln('       MD5: ',  compmem(@MD5Digest   , @CMD5   , sizeof(MD5Digest   )));
  writeln('      SHA1: ',  compmem(@SHA1Digest  , @CSHA1  , sizeof(SHA1Digest  )));
  writeln(' RIPEMD160: ',  compmem(@RMD160Digest, @CRMD160, sizeof(RMD160Digest)));
  writeln('    SHA224: ',  compmem(@SHA224Digest, @CSHA224, sizeof(SHA224Digest)));
  writeln('    SHA256: ',  compmem(@SHA256Digest, @CSHA256, sizeof(SHA256Digest)));
  writeln('    SHA384: ',  compmem(@SHA384Digest, @CSHA384, sizeof(SHA384Digest)));
  writeln('    SHA512: ',  compmem(@SHA512Digest, @CSHA512, sizeof(SHA512Digest)));
  writeln('  SHA3_224: ',  compmem(@SHA3_224Digest, @CSHA3_224, sizeof(SHA3_224Digest)));
  writeln('  SHA3_256: ',  compmem(@SHA3_256Digest, @CSHA3_256, sizeof(SHA3_256Digest)));
  writeln('  SHA3_384: ',  compmem(@SHA3_384Digest, @CSHA3_384, sizeof(SHA3_384Digest)));
  writeln('  SHA3_512: ',  compmem(@SHA3_512Digest, @CSHA3_512, sizeof(SHA3_512Digest)));
  writeln(' Whirlpool: ',  compmem(@WhirlDigest , @CWhirl , sizeof(WhirlDigest )));

end.
