{-Test prog for Whirlpool, reproduce first part od NESSIE Bit-API test vectors, we 05.08}

program t_whirlb;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT} WinCRT,{$endif}
  mem_util, hash, whirl512;

var
  Context: THashContext;
  Digest : TWhirlDigest;
  data   : array[0..127] of byte;
  L: integer;
begin
  HexUpper := true;  {Hex strings in uppercase}
  fillchar(data, sizeof(data), 0);

  {Calculate first part of nessie-test-vectors.txt}
  writeln('Message digests of strings of 0-bits and length L:');
  for L:=0 to 1023 do begin
    Whirl_Init(Context);
    {update byte part}
    Whirl_Update(Context, @data, L shr 3);
    {update/finalize remaining bits}
    Whirl_FinalBits(Context, Digest, 0, L and 7);
    writeln('    L = ', L:4, ': ', HexStr(@Digest, sizeof(Digest)));
  end;
end.
