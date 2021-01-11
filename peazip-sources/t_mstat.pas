{-Test program for ministat unit,  we Nov. 2005}

program t_mstat;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

{$ifndef FPC}
  {$N+}
{$endif}


uses
  {$ifdef WINCRT}
     wincrt,
  {$endif}
  ministat;

const
  cx: array[1..5] of double = (16, 42, -8,  23,  5);
  cy: array[1..5] of double = (37, 98,  3, -21, 17);

{#Ex 4 values:}
const
  c4_mx=15.6;   c4_sx=18.84940317;
  c4_my=26.8;   c4_sy=45.05774073;
  c4_r=0.6462881832;
  c4_m=1.544891641;
  c4_b=2.699690400;


var
  Err: word;
  ctx: TStatXY;
  mx,my,sx,sy,r,m,b: double;


{---------------------------------------------------------------------------}
function eq(d1,d2: double): boolean;
begin
  eq := abs(d1-d2)<1E-9*abs(d2);
end;

{---------------------------------------------------------------------------}
procedure PrintResult(hdr: string);
begin
  writeln('================================');
  writeln(hdr);
  if Err<>0 then writeln('Err=',Err)
  else begin
    writeln('x:',mx:12:6, sx:15:8);
    writeln('y:',my:12:6, sy:15:8);
    writeln('r:', r:12:9, 'm:':6, m:12:9, 'b:':6, b:12:9);
  end;
end;


{---------------------------------------------------------------------------}
procedure test0;
var
  i: integer;
begin
  writeln('================================');
  writeln('Error Test');
  stat_init(ctx);
  stat_result(ctx,mx,my,sx,sy,r,m,b);
  if ctx.Error<>0 then writeln('1: OK, detected Error=', ctx.Error)
  else writeln('BUG: no error detected');
  stat_init(ctx);
  for i:=1 to 1 do stat_add(ctx, cx[i], cy[i]);
  if ctx.Error<>0 then writeln('2: unexpected Error=', ctx.Error);
  for i:=1 to 2 do stat_sub(ctx, cx[i], cy[i]);
  if ctx.Error<>0 then writeln('2: OK, detected Error=', ctx.Error)
  else writeln('BUG: no error detected');
  stat_init(ctx);
  for i:=1 to 5 do stat_add(ctx, cx[1], cy[i]);
  stat_result(ctx,mx,my,sx,sy,r,m,b);
  if ctx.Error<>0 then writeln('3: OK, detected Error=', ctx.Error)
  else writeln('BUG: no error detected');
  stat_init(ctx);
  for i:=1 to 5 do stat_add(ctx, cx[1], cy[1]);
  stat_result(ctx,mx,my,sx,sy,r,m,b);
  if ctx.Error<>0 then writeln('4: OK, detected Error=', ctx.Error)
  else writeln('BUG: no error detected');
end;

{---------------------------------------------------------------------------}
procedure test1;
  {-Kennedy example #4}
var
  i: integer;
begin
  stat_init(ctx);
  for i:=1 to 5 do stat_add(ctx, cx[i], cy[i]);
  stat_result(ctx,mx,my,sx,sy,r,m,b);
  PrintResult('Test 1');
end;


{---------------------------------------------------------------------------}
procedure test2;
  {-Test1 with add an remove junk}
var
  i: integer;
begin
  stat_init(ctx);
  {add junk}
  stat_add(ctx, 123, -456);
  for i:=1 to 5 do stat_add(ctx, cx[i], cy[i]);
  {remove junk}
  stat_sub(ctx, 123, -456);
  stat_result(ctx,mx,my,sx,sy,r,m,b);
  PrintResult('Test 2');
end;


{---------------------------------------------------------------------------}
procedure test3;
  {-Test1 with stat_full}
begin
  stat_full(@cx,@cy,5,mx,my,sx,sy,r,m,b, Err);
  PrintResult('Test 3');
end;


{---------------------------------------------------------------------------}
procedure test4;
  {-Test1 for x only}
begin
  stat1_full(@cx,5,mx,sx,Err);
  writeln('================================');
  writeln('Test 4');
  writeln('x:',mx:12:5, sx:15:8);
end;


{---------------------------------------------------------------------------}
procedure test5;
  {-Known answer (#Ex 4) test}
var
  ok: boolean;
begin
  stat_full(@cx,@cy,5,mx,my,sx,sy,r,m,b,Err);
  ok := eq(mx,c4_mx) and eq(my,c4_my) and eq(sx,c4_sx) and eq(sy,c4_sy) and eq(r,c4_r) and eq(m,c4_m) and eq(b,c4_b);
  writeln('================================');
  writeln('Test 5: ',ok);
end;

begin
  test0;
  Err := 0;
  test1;
  test2;
  test3;
  test4;
  test5;
end.
