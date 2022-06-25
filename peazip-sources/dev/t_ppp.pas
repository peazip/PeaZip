{Test program for PPP unit (GRC's Perfect Paper Passwords)}

program t_ppp;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{.$define usesha256}  {demo: use sha256/hash for a Sequence Key calculation}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  {$ifdef usesha256}
    hash, sha256,
  {$endif}
  mem_util, BTypes, ppp;

{Some tests: pppNET v0.9.1 from http://sourceforge.net/projects/pppnet/}

{---------------------------------------------------------------------------}
procedure dotest;
var
  pctx: TPPPctx;
  SeqKey: TPPPKey;
  kl: word;
  err,i, dcnt: integer;
  si: str255;
const
  testdef: array[0..69] of string[4] = (  {GRC}
            '32YT', '65!@', 'S3mg', 'skAf', 'wVmK', 'nSge', 'MsXd',
            'DzRA', 't%#f', 'vxDa', 'v!nz', '?S9G', 'u9Um', 'HA72',
            '944=', 'Rgai', 'pNv=', 'n5FU', 'SUKU', 'C+wp', 'C+7G',
            'jsKV', 'uSGn', 'EH?F', 'R3pW', 'EMd?', 'k=vv', '@+rC',
            't5yt', 'c:xD', 'BmeV', 'cex4', 'Zh4t', 'J:oK', 'nUxV',
            'EbA@', 'BHn%', 'G9Sa', 'Fo:i', 'MM97', '@Urg', 'fkPL',
            '%EU+', 'U8GF', 'F%fY', 'dxXE', 'H5M%', ':%B7', '4YDR',
            'vGYq', 'uL%5', '7#cE', 'hi+6', '99bS', '5FVh', 'ZhNr',
            '#DnV', '8sr7', 'Dnj3', 'xf=U', '4%a%', 'J#sE', 'pS?e',
            'CsCU', 'iYGg', 'KPFV', 'j8@2', 'dsLf', '3#yE', 'BWbj');

  testext: array[0..69] of string[4] = (  {pppNET}
            '(EON', 't.ix', 'L>:?', 'u|[&', '<e=T', 'nTZE', 'O)fY',
            'r%Y"', 'o<bB', 'XE7]', 'm?{,', 'e=_)','''#BT', 'f/&>',
            '>J[i', 'm|sk', 'H#hm', ',u:F', ';NUP', 'iSD}', '}Y6&',
           'bN''c', 'pVMk', '7e^{', ':HP8', '-CMu', 'ivN:', '-(Mr',
            '"L?G', 'kpru', 'q.Kk', 'NP7j', 'x6?%', 'vK!<', '>[5}',
            '@WCX', 'E}qG', 'az=W', 'nLHa', '{-6G', ')Xjg', '>iW6',
            'K\pU', '85E8', '9:D!', '+2-_', 'UYR5', ';LvV', 'v&[O',
            '&TwS', 'mNPO', 'q6%E', 'm9gf', 'Jc@{', 'vb?R', 'L.]*',
            'h<@2', '8-ef', '^Cdy', 'qjGr', 'o^Aa','''[3r', 'DRdv',
            '$\?p', '=:(<', 'RHBR','G''o>', ')~Mx', 'U:D,', 'ero9');
  testcst: array[0..69] of string[4] = (  {pppNET}
            'hvRG', '*sXy', 'NS)X', 'nkT$', '*mu(', '=NSz', 'JisZ',
            'JBW%', '!g=X', '_TJS', '~Eyh', 'whp@', '^)jr', 'uuE=',
            'RiaP', 'uy*K', '?L>&', 'Tua~', 'Pttw', '(KUc', 'xhP&',
            '&=e_', '*ah&', 'P*Hh', 'MDD>', 'YC_A', 'UrTh', 'UhAh',
            'W!=h', 'tdqH', 'hfJH', 's$GE', 'a$FU', 'LtsG', 'Ae&+',
            'FcJk', 'nF_k', '^ZrC', 'VefW', '<noq', '@t!i', 'b+Bc',
            '*FHw', 'u@Ct', '>wq-', 'nNFx', 'Nkzd', 'a*Sv', 'r=mG',
            'pV%P', '_j$d', 'G_<L', 'rNNC', ')<HH', 'RtRg', 'V%tB',
            'bq<?', 'oP=%', 'wunA', '*j)p', 'LwJ&', '+htR', 'Kq*$',
            '%^fK', 'JcFg', 'hMxT', '%LVh', 'CLpT', 'Li?(', '!wkV');
  test1400: array[0..69] of string[4] = ( {pppNET}
            'Vv?u', 'zC7u', 'cX+N', 'Vvtf', 'JthW', '?c9v', '@WVL',
            'jcpw', 'icjD', '=wCV', 'HZGB', 'cV2Z', 'V=5G', 'Mwau',
            '2Xm@', 't4g%', 'g@5V', 'GuDF', 'oAVo', 'ewPR', 'NgtD',
            'Dbj#', 'C6Xm', 'i2gT', '3pDV', '8BZ%', 'ATJJ', 'nKZ8',
            '?9b%', '7B3k', 'fJ6C', 'KYyJ', ':s@m', 'z2c6', 'T577',
            'Jdnj', '6K4K', 'y4u7', 'yort', 'Wzvw', '9W7j', 'LxwC',
            '8=SF', '%AnU', '%WgA', 'uFpm', 'Ao7t', 'E3?%', 'VNyF',
            'Dz!W', '#D!m', 'jYL8', '8uvr', 'maR?', 'De=K', 'ENd2',
            'iHv3', '%Aae', 'fpCM', '6j2d', 'ovo#', 'cmsP', '9e?M',
            'zpSm', '@N!m', '@rT7', '4apF', 'dMaG', '?f=D', 'DRh#');

  test6c3: array[0..49] of string[6] = (  {GRC}
            '+2WVHb',  '#m6se+',  'JeCgXS',  '=ksxW9',  '9ssSqG',
            'nHjKZx',  'PxNpY4',  'B6nYjq',  'S@:%M+',  '5NvX=r',
            'WhhF:4',  'ydw?Jj',  '=U:7oL',  'p=eZEf',  'bnooy=',
            'FG+2fn',  'dA!?gY',  'NojUov',  'fR8NPM',  'toJYch',
            '2ifS9d',  'Jcn89N',  'G:Y!Gj',  'VFbCbZ',  'KbNV:L',
            'PdoRzX',  'ZgJq5G',  'N4zLqd',  'y+Ec6A',  'B=BDTR',
            '@FVRCb',  '+:cJT+',  'KBw#w7',  'DRzFbY',  'JGmAy:',
            'fw3oxk',  'dKoCag',  'pNeYEs',  'Y=2Beu',  '#C+abd',
            'CGi?8n',  '5AMXDW',  'Th982q',  '+gaP7i',  '3J7tTZ',
            'ifX4UF',  'NUX?B=',  'Ysr8g?',  '5mAv%D',  'xWxBA!');
{$ifdef usesha256}
const
  zombie: packed array[0..5] of char8 = 'zombie';
{$endif}

const
  key1h = '53303f97ddcf91ed74391fc5c366124632427e1c93c1a2e2836d006fa2653dc1';
  key2h = '49460b7bbbd3aad3f2cba09864f5e8b01a220ea8c077e9fa996de367e7984af0'; {sha256('zombie')}
  cmap  = '~!@#$%^&*()_+-=<>?ABCDEFGHJKLMNPRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

  procedure ShowDiff(ts: str255);
  begin
    inc(dcnt);
    writeln('** Diff! `',si,'` should be `',ts,'`');
  end;

begin
  dcnt := 0;

  Hex2Mem(key1h, @SeqKey, sizeof(SeqKey), kl);
  PPP_Init4Standard(pctx, SeqKey, Err);
  if Err<>0 then begin
    writeln('  *** PPP_Init4Standard = ', Err);
  end
  else begin
    writeln('-------------------------------------');
    writeln('Test 1 with Standard 64-character set');
    writeln('-------------------------------------');
     si := PPP_First32(pctx,0);
    write(si,' ');
    i:=0;
    if si<>testdef[i] then ShowDiff(testdef[i]);
    for i:=1 to 69 do begin
      si := PPP_Next(pctx);
      write(si,' ');
      if si<>testdef[i] then ShowDiff(testdef[i]);
      if i mod 7 = 6 then writeln;
    end;
    writeln;
  end;

  Hex2Mem(key1h, @SeqKey, sizeof(SeqKey), kl);
  PPP_Init(pctx, SeqKey, map64, 6, Err);
  if Err<>0 then begin
    writeln('  *** PPP_Init error = ', Err);
  end
  else begin
    PPP_SetCodesPerCard(pctx, 50);
    writeln('-------------------------------------');
    writeln('Test 2 with Standard 64-character set');
    writeln('-------------------------------------');
    si := PPP_FirstCard(pctx,3);
    write(si,' ');
    i:=0;
    if si<>test6c3[i] then ShowDiff(test6c3[i]);
    for i:=1 to 49 do begin
      si := PPP_Next(pctx);
      write(si,' ');
      if si<>test6c3[i] then ShowDiff(test6c3[i]);
      if i mod 5 = 4 then writeln;
    end;
    writeln;
  end;

  {$ifdef usesha256}
    {demo: simple sha256('zombie')}
    SHA256Full(TSHA256Digest(SeqKey), @zombie, sizeof(zombie));
  {$else}
    Hex2Mem(key2h, @SeqKey, sizeof(SeqKey), kl);
  {$endif}

  PPP_Init4Extended(pctx, SeqKey, Err);
  if Err<>0 then begin
    writeln('  *** PPP_Init4Extended error = ', Err);
  end
  else begin
    writeln('-------------------------------------');
    writeln('Test 3 with Extended 88-character set');
    writeln('-------------------------------------');
    si := PPP_FirstCard(pctx,3);
    write(si,' ');
    i:=0;
    if si<>testext[i] then ShowDiff(testext[i]);
    for i:=1 to 69 do begin
      si := PPP_Next(pctx);
      write(si,' ');
      if si<>testext[i] then ShowDiff(testext[i]);
      if i mod 7 = 6 then writeln;
    end;
    writeln;
  end;

  Hex2Mem(key2h, @SeqKey, sizeof(SeqKey), kl);
  PPP_Init(pctx, SeqKey, cmap, 4, Err);
  if Err<>0 then begin
    writeln('  *** PPP_Init error = ', Err);
  end
  else begin
    writeln('-------------------------------------');
    writeln('Test 4 with a custom 66-character set');
    writeln('-------------------------------------');
    si := PPP_FirstCard(pctx,2);
    write(si,' ');
    i:=0;
    if si<>testcst[i] then ShowDiff(testcst[i]);
    for i:=1 to 69 do begin
      si := PPP_Next(pctx);
      write(si,' ');
      if si<>testcst[i] then ShowDiff(testcst[i]);
      if i mod 7 = 6 then writeln;
    end;
    writeln;
  end;

  Hex2Mem(key2h, @SeqKey, sizeof(SeqKey), kl);
  PPP_Init4Standard(pctx, SeqKey, Err);
  if Err<>0 then begin
    writeln('  *** PPP_Init4Standard error = ', Err);
  end
  else begin
    writeln('-------------------------------------');
    writeln('Test 5: CardNr 1400 with standard set');
    writeln('-------------------------------------');
    si := PPP_FirstCard(pctx,1400);
    write(si,' ');
    i:=0;
    if si<>test1400[i] then ShowDiff(test1400[i]);
    for i:=1 to 69 do begin
      si := PPP_Next(pctx);
      write(si,' ');
      if si<>test1400[i] then ShowDiff(test1400[i]);
      if i mod 7 = 6 then writeln;
    end;
    writeln;
  end;
  if dcnt=0 then writeln('All tests passed.')
  else writeln('*** test failed, ',dcnt,' differences found!')
end;


begin
  writeln('T_PPP - Test PPP unit [Perfect Paper Passwords]  (c) 2010 W.Ehrhardt');
  writeln;
  dotest;
end.

