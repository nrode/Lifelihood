unit Alea;

{$MODE Delphi}

interface

procedure mars;
procedure marsini(var iu, ju, ku, lu: integer);

var
  uni: double;
  seed1, seed2, seed3, seed4: integer;

implementation

type
  matt = array[1..97] of double;

var
  u: matt;
  c, cm, cd, su, tu: double;
  ip, jp, ii, jj, mu: integer;
{*********************************************************************}
procedure mars;
{Marsaglia et al, Statistics and Probability letters 9, 35-39}
{->uni}
begin
  uni := u[ip] - u[jp];
  if (uni < 0.0) then uni := uni + 1.0;
  u[ip] := uni;
  ip := ip - 1;
  if (ip = 0) then ip := 97;
  jp := jp - 1;
  if (jp = 0) then jp := 97;
  c := c - cd;
  if (c < 0.0) then c := c + cm;
  uni := uni - c;
  if (uni < 0.0) then uni := uni + 1.0;
end;
{*********************************************}
procedure marsini(var iu, ju, ku, lu: integer);
begin
{randomize;
iu:=random(168);ju:=random(168);ku:=random(168);lu:=random(168);}
  for ii := 1 to 97 do
  begin
    su := 0.0;
    tu := 0.5;
    for jj := 1 to 24 do
    begin
      mu := ((((iu * ju) mod 179) * ku) mod 179);
      iu := ju;
      ju := ku;
      ku := mu;
      lu := (53 * lu + 1) mod 169;
      if (((lu * mu) mod 64) >= 32) then su := su + tu;
      tu := 0.5 * tu;
    end;
    u[ii] := su;
  end;
  c := 362436.0 / 16777216.0;
  cd := 7654321.0 / 16777216.0;
  cm := 16777213.0 / 16777216.0;
  ip := 97;
  jp := 33;
end;

end.
