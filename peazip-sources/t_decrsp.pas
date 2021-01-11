{-Test prog for AES decryption speed, we 2003}

{D3 :  10.76  (42)
       10.16  (43)
        9.94  (44)
VPC:    9.17  (55)
BP7:    9.95  (43)      
        9.67  (44)
        7.03  (46)
        5.66  (50)
        4.06  (51)
}             

program T_DECRSP;

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
  aes_type, aes_decr;

var
  Context: TAESContext;
  Block  : TAESBlock;


const
  Key128  : array[0..15] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f);
var
  Err : integer;
  i,N : longint;
{---------------------------------------------------------------------------}
procedure CheckError;
begin
  if Err<>0 then writeln('Error ',Err);
end;

begin
{$ifdef BIT16}
  N := 2000000;
{$else}
  N := 2*10000000;
{$endif}
  fillchar(Block, sizeof(Block),0);
  Err := AES_Init_Decr(Key128, 8*sizeof(Key128), Context);
  CheckError;
  for i:=1 to N do begin
    AES_Decrypt(Context, Block, Block);
  end;
end.
