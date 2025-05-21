UNIT mathromb;

{ ******************************************
  ****   Scientific Subroutine Library  ****
  ****         for Turbo Pascal         ****
  ****************************************** }

INTERFACE

type
fint =  function(var x:double):double;

procedure romb(var fx:fint;lower,upper,tol: double; VAR ans: double);

  {methode integration romberg}
  {en dessous il y a aussi des procedure pour calculer des gammas et des bessels}

{ ************************************************************************ }

IMPLEMENTATION


{ ************************************************************************ }

PROCEDURE romb(var fx:fint;lower, upper, tol: double; VAR ans: double);
{ numerical integration by the Romberg method }
{ function fx, name cannot be passed by Turbo Pascal}
VAR
	nx			: ARRAY[1..16] of Integer;
	t			: ARRAY[1..136] of double;
	done,error		: Boolean;
	pieces,nt,i,ii,n,nn,
	l,ntra,k,m,j		: Integer ;
	delta_x,c,sum,fotom,x	: double ;
BEGIN
  done := false;
  error := false;
  pieces := 1;
  nx[1] := 1;
  delta_x := (upper-lower)/pieces;
  c := (fx(lower)+fx(upper))*0.5;
  t[1] := delta_x*c;
  n := 1;
  nn := 2;
  sum := c;
  REPEAT
    n := n+1;
    fotom := 4.0;
    nx[n] := nn;
    pieces := pieces*2;
    l := pieces-1;
    delta_x := (upper-lower)/pieces;
    { compute trapezoidal sum for 2^(n-1)+1 points }
    FOR ii:=1 to (l+1) div 2 DO
      BEGIN
	i := ii*2-1;
	x := lower+i*delta_x;
	sum := sum+fx(x)
      END;
    t[nn] := delta_x*sum;
    write(pieces:5,t[nn]);
    ntra := nx[n-1];
    k := n-1;
    { compute n-th row of T array }
    FOR m:=1 to k DO
      BEGIN
	j := nn+m;
	nt := nx[n-1]+m-1;
	t[j] := (fotom*t[j-1]-t[nt])/(fotom-1.0);
	fotom := fotom*4.0
      END;
    writeln(j:4,t[j]);
    IF n>4 THEN
      BEGIN
	IF t[nn+1]<>0.0 THEN
	  IF (abs(t[ntra+1]-t[nn+1])<=abs(t[nn+1]*tol))
	    OR (abs(t[nn-1]-t[j])<=abs(t[j]*tol)) THEN
	      done := true
	  ELSE IF n>15 THEN
	    BEGIN
	      done := true;
	      error := true
	    END
	END;	{ if n>4 }
    nn := j+1
  UNTIL done;
  ans := t[j]
END;		{ ROMBERG }

{ ************************************************************************ }

 { ************************************************************************ }

FUNCTION gamma(x: Real): Real;
CONST	pi	= 3.1415926;

VAR	i,j	: Integer;
	y,gam	: Real;

BEGIN		{ gamma function }
  IF x>=0.0 THEN
    BEGIN
      y := x+2.0;
      gam := sqrt(2*pi/y)*exp(y*ln(y)+(1-1/(30*y*y))/(12*y)-y);
      gamma := gam/(x*(x+1))
    END
  ELSE		{ x<0 }
    BEGIN
      j := 0;
      y := x;
      REPEAT
	j := j+1;
	y := y+1.0
      UNTIL y>0.0;
      gam := gamma(y);		{ recursive call }
      FOR i:=0 to j-1 DO
	gam := gam/(x+1);
      gamma := gam
    END	{ x<0 }
END;		{ gamma function }

{ ************************************************************************ }

FUNCTION bessj(x,n: Real): Real;
{ cylindrical Bessel function of the first kind }
{ the gamma function is required }

CONST	tol	= 1.0E-4;
	pi	= 3.1415926;

VAR	i		: Integer;
	term,new_term,
	sum,x2		: Real;

BEGIN	{ bessj }
  x2 := x*x;
  IF (x=0.0) AND (N=1.0) THEN bessj := 0.0
  ELSE IF x>15 THEN { asymptotic expansion }
    bessj := sqrt(2/(pi*x))*cos(x-pi/4-n*pi/2)
  ELSE
    BEGIN
      IF n=0.0 THEN sum := 1.0
      ELSE sum := exp(n*ln(x/2))/gamma(n+1.0);
      new_term := sum;
      i := 0;
  REPEAT
    i := i+1;
    term := new_term;
    new_term := -term*x2*0.25/(i*(n+1));
    sum := sum+new_term
  UNTIL abs(new_term)<=abs(sum*tol);
  bessj := sum
 END	{ if}
END;	{ bessj }
{**************************************************************************}

END.
