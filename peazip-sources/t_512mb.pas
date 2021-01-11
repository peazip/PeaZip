{-Test prog for 512MB, we 03.10.07}

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT} WinCRT, {$endif}
  mem_util, hash,
  CRC32, Adler32, MD5, RMD160, SHA1, SHA256, SHA384, SHA512,
  Whirl512, MD4, ED2K;


(*************************************************************************
From http://www.cryptosys.net (SHA-256 test vectors)

"Data = One million repetitions of 'a'
CDC76E5C 9914FB92 81A1C7E2 84D73E67 F1809A48 A497200E 046D39CC C7112CD0

Data = 2^29 repetitions of 'a'
B9045A71 3CAED5DF F3D3B783 E98D1CE5 778D8BC3 31EE4119 D7070723 12AF06A7

These agree with an implementation created by Aaron Gifford who has many more
additional vectors of SHA-256 (and SHA-384 and SHA-512) on his site as well as
open-source versions of his source code.

I think the test with 2^29 (0x20000000) byte repetitions is important because
it makes the bit count "overflow" into the high 32-bit word. That would seem to
me to be a key place where an implementation of any of the Secure Hash
Algorithms might fall over."
**************************************************************************)


(*
Test vectors 2^29 repetitions of "a"
1=SlavaSoft Optimizing Checksum Utility - fsum 2.51
2=Dominik Reichl's rehash V0.2
3=Jesse Kornblum's whirlpooldeep 1.12
4=LTC 1.17 hashsum
5=ed2k_hash v0.3
*)

{CRC32
0aef26ca   (1)
0aef26ca   (2)}
const vcrc32: longint = ($0aef26ca);


{ADLER32
0955ea9c  (1)
0955ea9c  (2)}
const vadler: longint = ($0955ea9c);


{MD4
e5d23bef981385a93e50d60940266d96  (1)
e5d23bef981385a93e50d60940266d96  (2)}
const vmd4: array[0.. 15] of byte = ($e5,$d2,$3b,$ef,$98,$13,$85,$a9,
                                     $3e,$50,$d6,$09,$40,$26,$6d,$96);


{EDONKEY
14faf9c24c55a7aa82e44ebc932463ba  (1)
125de2df92ee36d4f154f56dfada74b0  (2)  !!!
14faf9c24c55a7aa82e44ebc932463ba  (5)}
const ved2k: array[0.. 15] of byte = ($14,$fa,$f9,$c2,$4c,$55,$a7,$aa,
                                      $82,$e4,$4e,$bc,$93,$24,$63,$ba);


{MD5
31e4d9c6d74cd592b78f77f72965d6ab  (1)
31e4d9c6d74cd592b78f77f72965d6ab  (2)}
const vmd5: array[0.. 15] of byte = ($31,$e4,$d9,$c6,$d7,$4c,$d5,$92,
                                     $b7,$8f,$77,$f7,$29,$65,$d6,$ab);


{RIPEMD160
2d253ceb06fb13e1fa3c7756e37bb5562b1bf3ba  (1)
2d253ceb06fb13e1fa3c7756e37bb5562b1bf3ba  (2)}
const vrmd: array[0.. 19] of byte = ($2d,$25,$3c,$eb,$06,$fb,$13,$e1,
                                     $fa,$3c,$77,$56,$e3,$7b,$b5,$56,
                                     $2b,$1b,$f3,$ba);



{SHA1
0ea59bfe8787939816796610c73deb1c625e03ed  (1)
0ea59bfe8787939816796610c73deb1c625e03ed  (2)}
const vsha1: array[0.. 19] of byte = ($0e,$a5,$9b,$fe,$87,$87,$93,$98,
                                      $16,$79,$66,$10,$c7,$3d,$eb,$1c,
                                      $62,$5e,$03,$ed);


{SHA256
b9045a713caed5dff3d3b783e98d1ce5778d8bc331ee4119d707072312af06a7  (1)
b9045a713caed5dff3d3b783e98d1ce5778d8bc331ee4119d707072312af06a7  (2)}
const vsha256: array[0.. 31] of byte = ($b9,$04,$5a,$71,$3c,$ae,$d5,$df,
                                        $f3,$d3,$b7,$83,$e9,$8d,$1c,$e5,
                                        $77,$8d,$8b,$c3,$31,$ee,$41,$19,
                                        $d7,$07,$07,$23,$12,$af,$06,$a7);


{SHA384
5793041712680b72e846c9eeb67d5d7131653407fa17bb3dc7987bf1a3bb35eb185d4bb0c2b48e9f6a84144792b2b23c  (1)
5793041712680b72e846c9eeb67d5d7131653407fa17bb3dc7987bf1a3bb35eb185d4bb0c2b48e9f6a84144792b2b23c  (2)}
const vsha384: array[0.. 47] of byte = ($57,$93,$04,$17,$12,$68,$0b,$72,
                                        $e8,$46,$c9,$ee,$b6,$7d,$5d,$71,
                                        $31,$65,$34,$07,$fa,$17,$bb,$3d,
                                        $c7,$98,$7b,$f1,$a3,$bb,$35,$eb,
                                        $18,$5d,$4b,$b0,$c2,$b4,$8e,$9f,
                                        $6a,$84,$14,$47,$92,$b2,$b2,$3c);


{SHA512
cdda1bb7e0152b72261c5fbb3684c6e783e65825bf608333f32ead70b8d93058e5
    416a425b834be114ec45c6cdd931b5bcbf5590819400d5bdcbd91697fa3bfd  (1)
cdda1bb7e0152b72261c5fbb3684c6e783e65825bf608333f32ead70b8d93058e5
    416a425b834be114ec45c6cdd931b5bcbf5590819400d5bdcbd91697fa3bfd  (2)}
const vsha512: array[0.. 63] of byte = ($cd,$da,$1b,$b7,$e0,$15,$2b,$72,
                                        $26,$1c,$5f,$bb,$36,$84,$c6,$e7,
                                        $83,$e6,$58,$25,$bf,$60,$83,$33,
                                        $f3,$2e,$ad,$70,$b8,$d9,$30,$58,
                                        $e5,$41,$6a,$42,$5b,$83,$4b,$e1,
                                        $14,$ec,$45,$c6,$cd,$d9,$31,$b5,
                                        $bc,$bf,$55,$90,$81,$94,$00,$d5,
                                        $bd,$cb,$d9,$16,$97,$fa,$3b,$fd);



{Whirlpool
b4fd5f954c987ee4e71b6c126df4b3acc3874771d611e76e666466099bfa2c7cb0
    4417c63f8959e61046dec1877255517f049802078ddbe246620aad102eb87f  (3)
b4fd5f954c987ee4e71b6c126df4b3acc3874771d611e76e666466099bfa2c7cb0
    4417c63f8959e61046dec1877255517f049802078ddbe246620aad102eb87f  (4)}
const vwhirl: array[0.. 63] of byte = ($b4,$fd,$5f,$95,$4c,$98,$7e,$e4,
                                       $e7,$1b,$6c,$12,$6d,$f4,$b3,$ac,
                                       $c3,$87,$47,$71,$d6,$11,$e7,$6e,
                                       $66,$64,$66,$09,$9b,$fa,$2c,$7c,
                                       $b0,$44,$17,$c6,$3f,$89,$59,$e6,
                                       $10,$46,$de,$c1,$87,$72,$55,$51,
                                       $7f,$04,$98,$02,$07,$8d,$db,$e2,
                                       $46,$62,$0a,$ad,$10,$2e,$b8,$7f);


const
  n : longint = 1024*1024;           {2^20}
  k = 64;

var
  tCRC32: longint;
  tAdler: longint;
  RMD160Context: THashContext;  RMD160Digest: TRMD160Digest;
  SHA1Context  : THashContext;    SHA1Digest: TSHA1Digest;
  SHA256Context: THashContext;  SHA256Digest: TSHA256Digest;
  ED2KContext  : TED2KContext;       ED2KRes: TED2KResult;
  MD4Context   : THashContext;     MD4Digest: TMD4Digest;
  MD5Context   : THashContext;     MD5Digest: TMD5Digest;
  SHA384Context: THashContext;  SHA384Digest: TSHA384Digest;
  SHA512Context: THashContext;  SHA512Digest: TSHA512Digest;
  WhirlContext : THashContext;   WhirlDigest: TWhirlDigest;

var
  buf: array[1..k*512] of byte;      {2^15 byte}
  i,imax: longint;
  progress: boolean;
begin
  progress := paramcount>0;

  fillchar(buf, sizeof(buf), $61 {='a'});
  writeln('2^29 repetitions of "a"');

  RMD160Init(RMD160Context);
  SHA1Init(SHA1Context);
  SHA256Init(SHA256Context);
  SHA384Init(SHA384Context);
  SHA512Init(SHA512Context);
  Whirl_Init(WhirlContext);
  ED2K_Init(ED2KContext);
  MD4Init(MD4Context);
  MD5Init(MD5Context);
  CRC32Init(tCRC32);
  Adler32Init(tAdler);

  imax := n div k;
  for i:=1 to imax do begin
    if progress then write(100*i/imax:4:1,'%'#13);
    RMD160Update(RMD160Context, @buf, sizeof(buf));
    SHA1Update(SHA1Context, @buf, sizeof(buf));
    SHA256Update(SHA256Context, @buf, sizeof(buf));
    ED2K_Update(ED2KContext, @buf, sizeof(buf));
    MD4Update(MD4Context, @buf, sizeof(buf));
    MD5Update(MD5Context, @buf, sizeof(buf));
    Adler32Update(TAdler, @buf, sizeof(buf));
    CRC32Update(tCRC32, @buf, sizeof(buf));
    SHA384Update(SHA384Context, @buf, sizeof(buf));
    SHA512Update(SHA512Context, @buf, sizeof(buf));
    Whirl_Update(WhirlContext, @buf, sizeof(buf));
  end;

  RMD160Final(RMD160Context,RMD160Digest);
  SHA1Final(SHA1Context,SHA1Digest);
  SHA256Final(SHA256Context,SHA256Digest);
  ED2K_Final(ED2KContext,ED2KRes);
  MD4Final(MD4Context,MD4Digest);
  MD5Final(MD5Context,MD5Digest);
  CRC32Final(tCRC32);
  Adler32Final(tAdler);
  SHA384Final(SHA384Context,SHA384Digest);
  SHA512Final(SHA512Context,SHA512Digest);
  Whirl_Final(WhirlContext,WhirlDigest);

  writeln('Tests passed:');
  writeln('CRC32    : ', compmem(@tCRC32       , @vCRC32    , sizeof(tCRC32      )));
  writeln('Adler32  : ', compmem(@tAdler       , @vAdler    , sizeof(tAdler      )));
  writeln('RMD160   : ', compmem(@RMD160Digest , @vRMD      , sizeof(RMD160Digest)));
  writeln('SHA1     : ', compmem(@SHA1Digest   , @vSHA1     , sizeof(SHA1Digest  )));
  writeln('SHA256   : ', compmem(@SHA256Digest , @vSHA256   , sizeof(SHA256Digest)));
  writeln('ED2K     : ', compmem(@ED2KRes.eMule, @vED2K     , sizeof(vED2K       )));
  writeln('MD4      : ', compmem(@MD4Digest    , @vMD4      , sizeof(MD4Digest   )));
  writeln('MD5      : ', compmem(@MD5Digest    , @vMD5      , sizeof(MD5Digest   )));
  writeln('SHA384   : ', compmem(@SHA384Digest , @vSHA384   , sizeof(SHA384Digest)));
  writeln('SHA512   : ', compmem(@SHA512Digest , @vSHA512   , sizeof(SHA512Digest)));
  writeln('Whirlpool: ', compmem(@WhirlDigest  , @vWhirl    , sizeof(WhirlDigest )));

end.
