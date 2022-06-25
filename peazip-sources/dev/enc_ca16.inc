
(*************************************************************************
 Include file for AES_ENCR.PAS - AES_Encrypt for BASM16/Compressed table

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     09.07.06  W.Ehrhardt  Initial version for compressed table
 0.11     10.07.06  we          Removed bx in TCe[bx+si+?]
 0.13     13.07.06  we          Uses TCe box byte instead of SBox
**************************************************************************)

(**** (C) Copyright 2002-2006 Wolfgang Ehrhardt -- see copying_we.txt ****)


{16 bit BASM used for TP6, BP7, Delphi1}

{---------------------------------------------------------------------------}
procedure AES_Encrypt(var ctx: TAESContext; {$ifdef CONST} const {$else} var {$endif} BI: TAESBlock; var BO: TAESBlock);
  {-encrypt one block, not checked: key must be encryption key}
var
  s,t: TAESBlock;
  rnd: integer;
  pK: pointer;
begin
  rnd := ctx.rounds;
  pK := @ctx.RK;
  asm

   db $66;  pusha

{AES_XorBlock(BI, ctx.RK[0], s);}
            les  si,[BI]
   db $66;  mov  ax,es:[si]
   db $66;  mov  bx,es:[si+4]
   db $66;  mov  cx,es:[si+8]
   db $66;  mov  dx,es:[si+12]

            les  di,[pK]
   db $66;  xor  ax,es:[di]
   db $66;  xor  bx,es:[di+4]
   db $66;  xor  cx,es:[di+8]
   db $66;  xor  dx,es:[di+12]

   db $66;  mov  word ptr [s],ax
   db $66;  mov  word ptr [s+4],bx
   db $66;  mov  word ptr [s+8],cx
   db $66;  mov  word ptr [s+12],dx

            add  di,16      {di->ctx.RK[1]}
            mov  cx,[rnd]
            dec  cx

{  *Note* in the following round loop        }
{  op   eax, mem[8*ebx] is calculated as     }
{  lea  esi, [edx+8*ebx] $66,$67,$8D,$34,$DA }
{  op   eax, mem[esi]                        }
    db $66; sub  bx,bx      {clear ebx}
    db $66; sub  dx,dx      {clear edx}

@@1:

{TWA4(t)[0] := Te0[s[0*4+0]] xor Te1[s[1*4+1]] xor Te2[s[2*4+2]] xor Te3[s[3*4+3]] xor TWA4(ctx.RK[r])[0];}
            mov  bl,byte ptr s[0*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr s[1*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr s[2*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr s[3*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di]
    db $66; mov  word ptr t[0],ax

{TWA4(t)[1] := Te0[s[1*4+0]] xor Te1[s[2*4+1]] xor Te2[s[3*4+2]] xor Te3[s[0*4+3]] xor TWA4(ctx.RK[r])[1];}
            mov  bl,byte ptr s[1*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr s[2*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr s[3*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr s[0*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+4]
    db $66; mov  word ptr t[4],ax

{TWA4(t)[2] := Te0[s[2*4+0]] xor Te1[s[3*4+1]] xor Te2[s[0*4+2]] xor Te3[s[1*4+3]] xor TWA4(ctx.RK[r])[2];}
            mov  bl,byte ptr s[2*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr s[3*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr s[0*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr s[1*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+8]
    db $66; mov  word ptr t[8],ax

{TWA4(t)[3] := Te0[s[3*4+0]] xor Te1[s[0*4+1]] xor Te2[s[1*4+2]] xor Te3[s[2*4+3]] xor TWA4(ctx.RK[r])[3];}
            mov  bl,byte ptr s[3*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr s[0*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr s[1*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr s[2*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+12]
    db $66; mov  word ptr t[12],ax

{if r>=ctx.rounds then break;}
            dec  cx
            jbe  @@2

{TWA4(s)[0] := Te0[t[0*4+0]] xor Te1[t[1*4+1]] xor Te2[t[2*4+2]] xor Te3[t[3*4+3]] xor TWA4(ctx.RK[r])[0];}
            mov  bl,byte ptr t[0*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr t[1*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr t[2*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr t[3*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+16]
    db $66; mov  word ptr s[0],ax

{TWA4(s)[1] := Te0[t[1*4+0]] xor Te1[t[2*4+1]] xor Te2[t[3*4+2]] xor Te3[t[0*4+3]] xor TWA4(ctx.RK[r])[1];}
            mov  bl,byte ptr t[1*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr t[2*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr t[3*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr t[0*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+20]
    db $66; mov  word ptr s[4],ax

{TWA4(s)[2] := Te0[t[2*4+0]] xor Te1[t[3*4+1]] xor Te2[t[0*4+2]] xor Te3[t[1*4+3]] xor TWA4(ctx.RK[r])[2];}
            mov  bl,byte ptr t[2*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr t[3*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr t[0*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr t[1*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+24]
    db $66; mov  word ptr s[8],ax

{TWA4(s)[3] := Te0[t[3*4+0]] xor Te1[t[0*4+1]] xor Te2[t[1*4+2]] xor Te3[t[2*4+3]] xor TWA4(ctx.RK[r])[3];}
            mov  bl,byte ptr t[3*4+0]
    db $66,$67,$8D,$34,$DA;
    db $66; mov  ax,word ptr TCe[si+3]

            mov  bl,byte ptr t[0*4+1]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+2]

            mov  bl,byte ptr t[1*4+2]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si+1]

            mov  bl,byte ptr t[2*4+3]
    db $66,$67,$8D,$34,$DA;
    db $66; xor  ax,word ptr TCe[si]

    db $66; xor  ax,es:[di+28]
            add  di,32
    db $66; mov  word ptr s[12],ax

            dec  cx
            jmp  @@1

@@2:        add  di,16      {di -> ctx.RK[ctx.rounds]}

            {Last round uses SBox}

            mov  bl, byte ptr t[0*4+0]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[0],al

            mov  bl, byte ptr t[1*4+1]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[1],al

            mov  bl, byte ptr t[2*4+2]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[2],al

            mov  bl, byte ptr t[3*4+3]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[3],al

            mov  bl, byte ptr t[1*4+0]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[4],al

            mov  bl, byte ptr t[2*4+1]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[5],al

            mov  bl, byte ptr t[3*4+2]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[6],al

            mov  bl, byte ptr t[0*4+3]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[7],al

            mov  bl, byte ptr t[2*4+0]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[8],al

            mov  bl, byte ptr t[3*4+1]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[9],al

            mov  bl, byte ptr t[0*4+2]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[10],al

            mov  bl, byte ptr t[1*4+3]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[11],al

            mov  bl, byte ptr t[3*4+0]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[12],al

            mov  bl, byte ptr t[0*4+1]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[13],al

            mov  bl, byte ptr t[1*4+2]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[14],al

            mov  bl, byte ptr t[2*4+3]
            sub  bh,bh
            shl  bx,3
            mov  al, byte ptr TCe[bx+7]
            mov  byte ptr s[15],al

{AES_XorBlock(s, ctx.RK[rnd], BO)}

   db $66;  mov  ax,word ptr [s]
   db $66;  mov  bx,word ptr [s+4]
   db $66;  mov  cx,word ptr [s+8]
   db $66;  mov  dx,word ptr [s+12]
   db $66;  xor  ax,es:[di]
   db $66;  xor  bx,es:[di+4]
   db $66;  xor  cx,es:[di+8]
   db $66;  xor  dx,es:[di+12]
            les  si,[BO]
   db $66;  mov  es:[si],ax
   db $66;  mov  es:[si+4],bx
   db $66;  mov  es:[si+8],cx
   db $66;  mov  es:[si+12],dx

   db $66;  popa

  end;
end;

