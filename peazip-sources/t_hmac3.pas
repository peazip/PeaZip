{-Test prog for HMACs using Bit-API, we 05.05.2008}

program t_hmac3;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


uses
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  hash,hmac,rmd160,sha1,sha224,sha256,sha384,sha512,
  mem_util;


var
  psha1,prmd160,psha224,psha256,psha384,psha512: PHashDesc;
  ctx: THMAC_Context;
  mac: THashDigest;


{---------------------------------------------------------------------------}
procedure Check1(phash: PHashDesc;
                 key: pointer; klen: word;
                 dig: pointer; dlen: word);
begin
  hmac_init(ctx,phash,key,klen);
  hmac_final_bits(ctx,mac,0,1);
  writeln(phash^.HName:10, ' ',compmem(@mac,dig,dlen));
end;


{[1]: NESSIE-TV}
{[2]: ShaTest RFC4634}

{---------------------------------------------------------------------------}
procedure Testcase1;
const
  Key : array[0..63] of byte = ($00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
                                $01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
                                $01,$23,$45,$67,$89,$ab,$cd,$ef,
                                $00,$11,$22,$33,$44,$55,$66,$77,
                                $88,$99,$aa,$bb,$cc,$dd,$ee,$ff);

const {from [1]}
  HRMD: array[0..19] of byte = ($58,$87,$ca,$80,$ec,$dd,$c6,$9b,
                                $d7,$6c,$4c,$94,$06,$75,$76,$66,
                                $e0,$b2,$83,$6d);

const {from [1] and [2]}
  HSHA: array[0..19] of byte = ($f8,$a7,$f9,$1b,$dc,$3e,$83,$d8,
                                $9b,$32,$ee,$6c,$5e,$a4,$5e,$b2,
                                $eb,$5b,$b1,$23);

const {from [2]}
  H224: array[0..27] of byte = ($65,$f1,$6f,$00,$1f,$e0,$48,$5c,
                                $2a,$7d,$93,$22,$72,$39,$41,$85,
                                $d2,$93,$30,$70,$c7,$4a,$b7,$4f,
                                $96,$fc,$69,$b8);

const {from [1] and [2]}
  H256: array[0..31] of byte = ($ca,$61,$f4,$b7,$45,$80,$5a,$32,
                                $94,$bd,$45,$38,$2e,$d2,$a6,$68,
                                $a2,$7e,$96,$97,$d5,$a9,$5c,$36,
                                $71,$ff,$45,$88,$3c,$6f,$fd,$ac);

const {from [2], values from [1] are different}
  H384: array[0..47] of byte = ($ed,$e9,$4e,$99,$b2,$af,$89,$ee,
                                $fb,$ac,$1b,$f1,$d4,$4d,$c3,$23,
                                $09,$c1,$a0,$d5,$a4,$0c,$c9,$a7,
                                $e3,$35,$28,$0c,$43,$14,$98,$fa,
                                $f0,$8b,$82,$e7,$65,$86,$80,$55,
                                $c8,$97,$e8,$47,$84,$53,$8a,$ae);

const {from [2], values from [1] are different}
  H512: array[0..63] of byte = ($81,$26,$b3,$5a,$9e,$f3,$e2,$71,
                                $01,$83,$9a,$84,$19,$18,$dd,$84,
                                $67,$17,$88,$e9,$d7,$10,$d2,$f7,
                                $5f,$ab,$75,$b9,$eb,$51,$c5,$4e,
                                $83,$50,$e6,$28,$ac,$9a,$e5,$f4,
                                $de,$18,$45,$c9,$74,$76,$93,$07,
                                $82,$14,$3d,$f9,$9b,$f1,$7e,$2d,
                                $cc,$e0,$e3,$b0,$56,$f4,$7a,$d6);

begin
  writeln('Test case 1');
  Check1(psha1,  @key,sizeof(key),@HSHA,sizeof(HSHA));
  Check1(prmd160,@key,sizeof(key),@HRMD,sizeof(HRMD));
  Check1(psha224,@key,sizeof(key),@H224,sizeof(H224));
  Check1(psha256,@key,sizeof(key),@H256,sizeof(H256));
  Check1(psha384,@key,sizeof(key),@H384,sizeof(H384));
  Check1(psha512,@key,sizeof(key),@H512,sizeof(H512));
end;


{---------------------------------------------------------------------------}
procedure FindHashDescriptors;
  {-Find Hash descriptors for all SHAxxx}
  procedure Find1(AlgoName: THashName; var ph: PHashDesc);
  begin
    ph :=  FindHash_by_Name(AlgoName);
    if ph=nil then begin
      writeln('Hash descriptor not found for ', AlgoName);
      writeln('May be unit is not listed in uses statement');
      halt;
    end;
  end;
begin
  Find1('sha1',      psha1);
  Find1('ripemd160', prmd160);
  Find1('sha224',    psha224);
  Find1('sha256',    psha256);
  Find1('sha384',    psha384);
  Find1('sha512',    psha512);
end;


begin
  {$ifdef WINCRT}
    ScreenSize.Y := 50;  {D1: 50 lines screen}
  {$endif}
  writeln('HMAC test cases for Bit-API');
  FindHashDescriptors;
  TestCase1;
end.
