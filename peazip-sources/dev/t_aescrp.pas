{-Test prog for AES encrypt/decrypt, we 2003}

program T_AESCRP;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  aes_type, aes_encr, aes_decr, mem_util;

var
  Context: TAESContext;


const
  Plain: TAESBlock = ($0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f);
  Key128  : array[0..15] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f);
  Key192  : array[0..23] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
                                    $10, $11, $12, $13, $14, $15, $16, $17);
  Key256  : array[0..31] of byte = ($00, $01, $02, $03, $04, $05, $06, $07,
                                    $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
                                    $10, $11, $12, $13, $14, $15, $16, $17,
                                    $18, $19, $1a, $1b, $1c, $1d, $1e, $1f);

  CT128 : TAESBlock = ($0A, $94, $0B, $B5, $41, $6E, $F0, $45, $F1, $C3, $94, $58, $C6, $53, $EA, $5A);
  CT192 : TAESBlock = ($00, $60, $BF, $FE, $46, $83, $4B, $B8, $DA, $5C, $F9, $A6, $1F, $F2, $20, $AE);
  CT256 : TAESBlock = ($5A, $6E, $04, $57, $08, $FB, $71, $96, $F0, $2E, $55, $3D, $02, $C3, $A6, $92);

var
  Err : integer;
  OK  : boolean;

{---------------------------------------------------------------------------}
procedure CheckError;
begin
  if Err<>0 then writeln('Error ',Err);
end;

{---------------------------------------------------------------------------}
procedure DoTests;
var
  Block: TAESBlock;  {16 Bit: force Block in stack for debugging}
begin
  writeln('------------------------------------');

  Err := AES_Init_Encr(Key128, 8*sizeof(Key128), Context);
  CheckError;
  writeln('Plaintext : ', HexStr(@Plain, sizeof(Plain)));

  writeln;
  writeln('Key       : ', HexStr(@key128, sizeof(key128)));
  AES_Encrypt(Context, Plain, Block);
  OK := CompMem(@CT128, @Block, sizeof(Block));
  writeln('Encrypted : ', HexStr(@Block, sizeof(Block)), OK:8);

  Err := AES_Init_Decr(Key128, 8*sizeof(Key128), Context);
  CheckError;
  AES_Decrypt(Context, Block, Block);
  OK := CompMem(@Plain, @Block, sizeof(Block));
  writeln('Decrypted : ', HexStr(@Block, sizeof(Block)), OK:8);

  writeln;
  Err := AES_Init_Encr(Key192, 8*sizeof(Key192), Context);
  CheckError;
  writeln('Key       : ', HexStr(@key192, sizeof(key192)));
  AES_Encrypt(Context, Plain, Block);
  OK := CompMem(@CT192, @Block, sizeof(Block));
  writeln('Encrypted : ', HexStr(@Block, sizeof(Block)), OK:8);
  Err := AES_Init_Decr(Key192, 8*sizeof(Key192), Context);
  CheckError;
  AES_Decrypt(Context, Block, Block);
  OK := CompMem(@Plain, @Block, sizeof(Block));
  writeln('Decrypted : ', HexStr(@Block, sizeof(Block)), OK:8);

  writeln;
  Err := AES_Init_Encr(Key256, 8*sizeof(Key256), Context);
  CheckError;
  writeln('Key       : ', HexStr(@key256, sizeof(key256)));
  AES_Encrypt(Context, Plain, Block);
  OK := CompMem(@CT256, @Block, sizeof(Block));
  writeln('Encrypted : ', HexStr(@Block, sizeof(Block)), OK:8);
  Err := AES_Init_Decr(Key256, 8*sizeof(Key256), Context);
  CheckError;
  AES_Decrypt(Context, Block, Block);
  OK := CompMem(@Plain, @Block, sizeof(Block));
  writeln('Decrypted : ', HexStr(@Block, sizeof(Block)), OK:8);
end;


begin
  DoTests;
end.
