{-Test prog for AES encryption speed, we 2003}

{Gladman AES.DLL: 2.69s}
{ D3:  5.33s}
{VPC:  4.67s}
{BPW: 19.22s}
{BP7: 17.02s}

program T_ENCRSP;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  AES_Type,AES_Encr,mem_util;

{$ifdef VER70}
  const CNT=10000000 div 5;
{$else}
  const CNT=10000000;
{$endif}

var
  key: array[0..31] of byte;
  plain,ct: TAESBlock;
  ctx: TAESContext;
  i: longint;
begin
  fillchar(key,sizeof(key),0);
  fillchar(plain,sizeof(plain),0);
  if AES_Init_Encr(Key, 16*8, ctx)<>0 then halt;
  for i:=1 to CNT do begin
    AES_Encrypt(ctx, plain, ct);
  end;
  {$ifndef WINCRT}
    writeln(HexStr(@ct,sizeof(ct)));
  {$endif}

end.
