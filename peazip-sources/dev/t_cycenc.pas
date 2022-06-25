{-Test prog for AES encryption speed, we 2003}

{Gladman AES.DLL: 2.69s}
{ D3:  5.38s}
{VPC:  4.67s}
{BPW: 19.22s}
{BP7: 17.02s}

program T_CYCENC;

{$i STD.INC}

{$ifdef win32}
  {$ifndef VirtualPascal}
    {$apptype console}
  {$endif}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}
uses
  {$ifdef WINCRT}
    wincrt,
  {$endif}
  AES_Type, AES_ECB, HRTimer;


{$ifdef VER80}
const
  NUMROUNDS = 3000;
  NUMBLOCKS = 500;
{$else}
const
  NUMROUNDS = 1000;
  NUMBLOCKS = 1500;
{$endif}

var
  key: array[0..31] of byte;
  plain,ct: array[1..NUMBLOCKS] of TAESBlock;
  ctx: TAESContext;
  i: longint;
  HR: THRTimer;

{---------------------------------------------------------------------------}
procedure ShowResult(name: string);
var
  MB,sec: double;
  cnt,diff: comp;
begin
  diff:= ReadCycles(HR);
  cnt := 1.0*NUMBLOCKS*NUMROUNDS;
  MB  := cnt/1E6*sizeof(TAESBlock);
  sec := diff/CPUFrequency;
  writeln(name:10,'   Cnt/Block: ', diff/cnt:7:1, ',     MB/s: ',MB/sec:7:2);
end;


begin
  fillchar(key,sizeof(key),0);
  fillchar(plain,sizeof(plain),0);
  if  AES_ECB_Init_Encr(Key, 128, ctx)<>0 then begin
    writeln('*** Error ECB');
    exit;
  end;
  StartTimer(HR);
  for i:=1 to NUMROUNDS do begin
    if AES_ECB_Encrypt(@plain, @ct, sizeof(plain), ctx)<>0 then begin
      writeln('*** Error ECB');
      exit;
    end;
  end;
  ShowResult('');
end.
