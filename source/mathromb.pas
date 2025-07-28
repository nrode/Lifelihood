unit mathromb;

{$MODE Delphi}

{ ******************************************
  ****   Scientific Subroutine Library  ****
  ****         for Turbo Pascal         ****
  ****************************************** }

interface

type
  fint = function(var x: double): double;

procedure romb(var fx: fint; lower, upper, tol: double; var ans: double);

{methode integration romberg}
{en dessous il y a aussi des procedure pour calculer des gammas et des bessels}

{ ************************************************************************ }

implementation


{ ************************************************************************ }

procedure romb(var fx: fint; lower, upper, tol: double; var ans: double);
{ numerical integration by the Romberg method }
{ function fx, name cannot be passed by Turbo Pascal}
var
  nx: array[1..16] of integer;
  t: array[1..136] of double;
  done, error: boolean;
  pieces, nt, i, ii, n, nn, l, ntra, k, m, j: integer;
  delta_x, c, sum, fotom, x: double;
begin
  done := False;
  error := False;
  pieces := 1;
  nx[1] := 1;
  delta_x := (upper - lower) / pieces;
  c := (fx(lower) + fx(upper)) * 0.5;
  t[1] := delta_x * c;
  n := 1;
  nn := 2;
  sum := c;
  repeat
    n := n + 1;
    fotom := 4.0;
    nx[n] := nn;
    pieces := pieces * 2;
    l := pieces - 1;
    delta_x := (upper - lower) / pieces;
    { compute trapezoidal sum for 2^(n-1)+1 points }
    for ii := 1 to (l + 1) div 2 do
    begin
      i := ii * 2 - 1;
      x := lower + i * delta_x;
      sum := sum + fx(x);
    end;
    t[nn] := delta_x * sum;
    Write(pieces: 5, t[nn]);
    ntra := nx[n - 1];
    k := n - 1;
    { compute n-th row of T array }
    for m := 1 to k do
    begin
      j := nn + m;
      nt := nx[n - 1] + m - 1;
      t[j] := (fotom * t[j - 1] - t[nt]) / (fotom - 1.0);
      fotom := fotom * 4.0;
    end;
    writeln(j: 4, t[j]);
    if n > 4 then
    begin
      if t[nn + 1] <> 0.0 then
        if (abs(t[ntra + 1] - t[nn + 1]) <= abs(t[nn + 1] * tol)) or
          (abs(t[nn - 1] - t[j]) <= abs(t[j] * tol)) then
          done := True
        else if n > 15 then
        begin
          done := True;
          error := True;
        end;
    end;  { if n>4 }
    nn := j + 1
  until done;
  ans := t[j];
end;    { ROMBERG }

{ ************************************************************************ }

{ ************************************************************************ }

function gamma(x: real): real;
const
  pi = 3.1415926;
var
  i, j: integer;
  y, gam: real;
begin    { gamma function }
  if x >= 0.0 then
  begin
    y := x + 2.0;
    gam := sqrt(2 * pi / y) * exp(y * ln(y) + (1 - 1 / (30 * y * y)) / (12 * y) - y);
    gamma := gam / (x * (x + 1));
  end
  else    { x<0 }
  begin
    j := 0;
    y := x;
    repeat
      j := j + 1;
      y := y + 1.0
    until y > 0.0;
    gam := gamma(y);    { recursive call }
    for i := 0 to j - 1 do
      gam := gam / (x + 1);
    gamma := gam;
  end;  { x<0 }
end;    { gamma function }

{ ************************************************************************ }

function bessj(x, n: real): real;
  { cylindrical Bessel function of the first kind }
  { the gamma function is required }
const
  tol = 1.0E-4;
  pi = 3.1415926;
var
  i: integer;
  term, new_term, sum, x2: real;
begin  { bessj }
  x2 := x * x;
  if (x = 0.0) and (N = 1.0) then bessj := 0.0
  else if x > 15 then { asymptotic expansion }
    bessj := sqrt(2 / (pi * x)) * cos(x - pi / 4 - n * pi / 2)
  else
  begin
    if n = 0.0 then sum := 1.0
    else
      sum := exp(n * ln(x / 2)) / gamma(n + 1.0);
    new_term := sum;
    i := 0;
    repeat
      i := i + 1;
      term := new_term;
      new_term := -term * x2 * 0.25 / (i * (n + 1));
      sum := sum + new_term
    until abs(new_term) <= abs(sum * tol);
    bessj := sum;
  end;  { if}
end;  { bessj }

{**************************************************************************}

end.
