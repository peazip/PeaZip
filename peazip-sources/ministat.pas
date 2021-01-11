unit MiniStat;


{Mini statistics unit: Accurate running values for 1 and 2 variables}


{$ifndef FPC}
  {$N+}
{$endif}


interface


(*************************************************************************

 DESCRIPTION   :  Mini statistics unit
                  Accurate running values for 1 and 2 variables
                  Procedures do nothing if ctx.Error <> 0

 REQUIREMENTS  :  TP5-7, D1-D7/D9-D10/D12, FPC, VP, WDOSX

 EXTERNAL DATA :  ---

 MEMORY USAGE  :  ---

 DISPLAY MODE  :  ---

 REFERENCES    :  [1] J.Kennedy, "More accurate statistics", PPC Journal V5N10P9, 1978
                  [2] W.Kahan, B.N.Parlett, "K”nnen Sie sie auf Ihren Rechner
                      verlassen?", Funkschau 1979, Heft 1/2, S.73/71


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     17.07.04  W.Ehrhardt  Initial version
 0.20     17.07.04  we          stat_full
 0.30     17.07.04  we          stat1 functions
 0.40     30.04.05  we          Error handling
 0.41     30.04.05  we          Err parameter for full versions
 0.42     30.05.05  we          More comments and references
 0.43     04.06.05  we          removed break statement, Err in stat1_full
 0.44     21.11.08  we          BTypes, Ptr2Inc
 0.45     20.02.12  we          Fix FPC2.6.0/64-bit bug
**************************************************************************)

(*
Usage example:
-------------
  stat1_init(stat);
  for n:=1 to NMAX do begin
    next_x = ...
    stat1_add(stat, next_x);
    stat1_result(stat,mx,sx);
    if stat.Error<>0 then {...}
    {process mean mx and sdev sx}
  end;
*)


(*-------------------------------------------------------------------------
 (C) Copyright 2004-2012 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

uses
  BTypes;

const
  MS_Err_SampleCnt  = $0001;
  MS_Err_SqrtArg    = $0002;
  MS_Err_Undefined  = $0003;


type
  TStatX  = record
              MXn, SXn: double;     {mean, 'variance'}
              Nn      : longint;    {sample counter  }
              Error   : word;
            end;

  TStatXY = record
              MXn, MYn: double;     {X and Y means   }
              SXn, SYn: double;     {'variances'     }
              CXYn    : double;     {'covariance'    }
              Nn      : longint;    {sample counter  }
              Error   : word;
            end;


procedure stat_init(var ctx: TStatXY);
  {-Initialize ctx}

procedure stat_add(var ctx: TStatXY; x,y: double);
  {-Accumulate x,y into ctx}

procedure stat_sub(var ctx: TStatXY; x,y: double);
  {-Remove x,y from ctx}

procedure stat_result(var ctx: TStatXY; var mx,my,sx,sy,r,m,b: double);
  {-Calculate means mx,my; sdevs sx,sy; correlation r; best fit y=m*x+b}

procedure stat_full(pcx,pcy: pointer; n: word; var mx,my,sx,sy,r,m,b: double; var Err: word);
  {-All in one statistics for n pairs from two double arrays}


procedure stat1_init(var ctx: TStatX);
  {-Initialize ctx}

procedure stat1_add(var ctx: TStatX; x: double);
  {-Accumulate x into ctx}

procedure stat1_sub(var ctx: TStatX; x: double);
  {-Remove x from ctx}

procedure stat1_result(var ctx: TStatX; var mx,sx: double);
  {-Calculate mean mx, sdev sx}

procedure stat1_full(pcx: pointer; n: word; var mx,sx: double; var Err: word);
  {-All in one statistics for n values from double array}


implementation


type
  pdouble = ^double;


{---------------------------------------------------------------------------}
procedure stat_init(var ctx: TStatXY);
  {-Initialize ctx}
begin
  fillchar(ctx, sizeof(ctx), 0);
end;


{---------------------------------------------------------------------------}
procedure stat_add(var ctx: TStatXY; x,y: double);
  {-Accumulate x,y into ctx}
var
  dx,dy,t: double;
begin
{ Add a x,y pair to context
  MX(n)  = Mx(n-1) + (x(n)-MX(n-1))/n
  MY(n)  = MY(n-1) + (y(n)-MY(n-1))/n
  SX(n)  = SX(n-1) + (x(n)-MX(n-1))^2*(n-1)/n
  SY(n)  = SY(n-1) + (y(n)-MY(n-1))^2*(n-1)/n
  CXY(n) = CXY(n-1) + (x(n)-MX(n-1))*(y(n)-MY(n-1))*(n-1)/n }
  with ctx do begin
    if Error<>0 then exit;
    if Nn<0 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    inc(Nn);
    dx := x - MXn;
    dy := y - MYn;
    {FPC2.6.0/64-bit seems to evaluate (Nn-1.0)/Nn in single precision!}
    t   := (Nn-1)/Nn;
    MXn := MXn + dx/Nn;
    MYn := MYn + dy/Nn;
    SXn := SXn + dx*dx*t;
    SYn := SYn + dy*dy*t;
    CXYn := CXYn + dx*dy*t;
  end;
end;



{---------------------------------------------------------------------------}
procedure stat_sub(var ctx: TStatXY; x,y: double);
  {-Remove x,y from ctx}
var
  dx,dy,t: double;
begin
{ Remove a x,y pair from ctx
  MX(n-1)  = (n*MX(n) - x(n))/(n-1)
  MY(n-1)  = (n*MY(n) - y(n))/(n-1)
  SX(n-1)  = SX(n-1)  - (x(n)-MX(n-1))^2*(n-1)/n
  SY(n-1)  = SY(n-1)  - (y(n)-MY(n-1))^2*(n-1)/n
  CXY(n-1) = CXY(n-1) - (x(n)-MX(n-1))*(y(n)-MY(n-1))*(n-1)/n }
  with ctx do begin
    if Error<>0 then exit;
    if Nn<=1 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    MXn := (Nn*MXn - x)/(Nn-1);
    MYn := (Nn*MYn - y)/(Nn-1);
    dx  := x - MXn;
    dy  := y - MYn;
    t   := (Nn-1.0)/Nn;
    SXn := SXn - dx*dx*t;
    SYn := SYn - dy*dy*t;
    CXYn := CXYn - dx*dy*t;
    dec(Nn);
  end;
end;



{---------------------------------------------------------------------------}
procedure stat_result(var ctx: TStatXY; var mx,my,sx,sy,r,m,b: double);
  {-Calculate means mx,my; sdevs sx,sy; correlation r; best fit y=m*x+b}
begin
  with ctx do begin
    if Error<>0 then exit;
    if Nn<=1 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    if (SXn<0.0) or (SYn<0.0) then begin
      Error := MS_Err_SqrtArg;
      exit;
    end;
    mx := MXn;
    my := MYn;
    sx := sqrt(SXn/(Nn-1));
    sy := sqrt(SYn/(Nn-1));
    if SXn<=0.0 then begin
      Error := MS_Err_Undefined;
      exit;
    end;
    if SYn<=0.0 then begin
      {special case: y=const}
      m := 1;
      r := 1;
      b := my;
      exit;
    end;
    {here SXn>0, SYn>0}
    r := CXYn/sqrt(SXn*SYn);
    m := CXYn/SXn;
    b := my - m*mx;
  end;
end;



{---------------------------------------------------------------------------}
procedure stat_full(pcx,pcy: pointer; n: word; var mx,my,sx,sy,r,m,b: double; var Err: word);
  {-All in one statistics for n pairs from two double arrays}
var
  ctx: TStatXY;
  i  : word;
begin
  stat_init(ctx);
  for i:=1 to n do begin
    stat_add(ctx, pdouble(pcx)^, pdouble(pcy)^);
    if ctx.Error<>0 then begin
      Err := ctx.Error;
      exit;
    end;
    inc(Ptr2Inc(pcx), sizeof(double));
    inc(Ptr2Inc(pcy), sizeof(double));
  end;
  stat_result(ctx, mx,my,sx,sy,r,m,b);
  Err := ctx.Error;
end;



{---------------------------------------------------------------------------}
procedure stat1_init(var ctx: TStatX);
  {-Initialize ctx}
begin
  fillchar(ctx, sizeof(ctx), 0);
end;


{---------------------------------------------------------------------------}
procedure stat1_add(var ctx: TStatX; x: double);
  {-Accumulate x into ctx}
var
  dx: double;
begin
{ MX(n)  = Mx(n-1) + (x(n)-MX(n-1))/n
  SX(n)  = SX(n-1) + (x(n)-MX(n-1))^2*(n-1)/n }
  with ctx do begin
    if Error<>0 then exit;
    if Nn<0 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    inc(Nn);
    dx  := x - MXn;
    MXn := MXn + dx/Nn;
    SXn := SXn + dx*dx*(Nn-1)/Nn;
  end;
end;


{---------------------------------------------------------------------------}
procedure stat1_sub(var ctx: TStatX; x: double);
  {-Remove x from ctx}
begin
{ MX(n-1)  = (n*MX(n) - x(n))/(n-1)
  SX(n-1)  = SX(n-1)  - (x(n)-MX(n-1))^2*(n-1)/n}
  with ctx do begin
    if Error<>0 then exit;
    if Nn<=1 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    MXn := (Nn*MXn - x)/(Nn-1);
    SXn := SXn - sqr(x-MXn)*(Nn-1)/Nn;
    dec(Nn);
  end;
end;


{---------------------------------------------------------------------------}
procedure stat1_result(var ctx: TStatX; var mx,sx: double);
  {-Calculate mean mx, sdev sx}
begin
  with ctx do begin
    if Error<>0 then exit;
    if Nn<=1 then begin
      Error := MS_Err_SampleCnt;
      exit;
    end;
    if SXn<0.0 then begin
      Error := MS_Err_SqrtArg;
      exit;
    end;
    mx := MXn;
    sx := sqrt(SXn/(Nn-1));
  end;
end;


{---------------------------------------------------------------------------}
procedure stat1_full(pcx: pointer; n: word; var mx,sx: double; var Err: word);
  {-All in one statistics for n values from double array}
var
  ctx: TStatX;
  i  : word;
begin
  stat1_init(ctx);
  for i:=1 to n do begin
    stat1_add(ctx, pdouble(pcx)^);
    if ctx.Error<>0 then begin
      Err := ctx.Error;
      exit;
    end;
    inc(Ptr2Inc(pcx), sizeof(double));
  end;
  stat1_result(ctx, mx, sx);
  Err := ctx.Error;
end;


end.
