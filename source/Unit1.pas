unit Unit1;

{$MODE Delphi}

interface

uses
  Unit2, Alea, SysUtils, Classes, fspec;

var
  list: TStringList;
  lect: string;

procedure readata;
procedure read_custom(var FD: function_D);
procedure read_custom_continuous_var(flag: boolean);
procedure Init_met_D(var md: Metropolis_D);


implementation

//{$R *.DFM}    {$I+}
{----init met ------------------------------------------------------------------------------------------------}
procedure Init_met_D(var md: Metropolis_D);
begin
  with md do
  begin
    ntr := StrToInt(list[18]);
    nst := StrToInt(list[19]);
    temp0 := StrToFloat(list[20]);
    tempf := StrToFloat(list[21]);
    climbrate := StrToFloat(list[22]);
    precision := StrToFloat(list[23]);
    maxrep := 40;
    tT := 4;
    t0 := 2;
    bgeup := 1 / 0.98;
    //a voir si bdgeup doit etre + gd que l'inverse du bgdown pour accelerer convergence
    bgedown := 0.99;
  end;
end;

{----readata-------------------------------------------------------------------------------------------------}
procedure readata;
var
  i, j, k, numhv, numevent: integer;
  currentgroup: integer;
  lal: TStringList;
  check: string;
  vc: vecti;
begin
  reset(f1);
  readln(f1);    //line 'data struct'
  lal := TStringList.Create;
  lal.Delimiter := ' ';
  readln(f1, check);     //line with matclutch
  lal.DelimitedText := check;
  if lal[1] = 'true' then matclutch := 1
  else
    matclutch := 0;
  readln(f1, check);     //line with cov names
  lal.DelimitedText := check;
  if lal[0] = 'none' then nbcov := 0
  else
  begin
    nbcov := lal.Count;
    setlength(vc, nbcov);
    setlength(covar, 2 * lal.Count + 1);
    //Covar i a l'indice i !!  2 x car version fact et cont de chacune
    for j := 1 to lal.Count do covar[j].Name := lal[j - 1];
    // nom des covar fact
    for j := lal.Count + 1 to 2 * lal.Count do covar[j].Name := lal[j - 1 - lal.Count];
    //nom des covar continues
    readln(f1, check);       //line with cov levels
    lal.DelimitedText := check;
    for j := 0 to lal.Count - 1 do
    begin
      covar[j + 1].lev := StrToInt(lal[j]);     //lev des fact
      covar[j + 1].typ := 0;                     //typ fact
    end;
    for j := lal.Count to 2 * lal.Count - 1 do
    begin
      covar[j + 1].lev := StrToInt(lal[j - lal.Count]);   //lev des cont
      covar[j + 1].typ := 1;                               //typ cont
      setlength(covar[j + 1].valcont, covar[j + 1].lev);     // les valcont initialisés
    end;
  end;

  nb_group := 1;                      //set nb of groups
  if nbcov > 0 then for j := 1 to nbcov do nb_group := nb_group * covar[j].lev;
  setlength(group, nb_group);     //premier groupe a l'indice 0!!
  for j := 0 to nb_group - 1 do group[j].nb_ind := 0;

  readln(f1);  //line 'modele'
  readln(f1, check);      //line wei wei wei
  lal.DelimitedText := check;
  for j := 0 to nb_group - 1 do group[j].mort.Name := lal[0];
  for j := 0 to nb_group - 1 do group[j].mat.Name := lal[1];
  for j := 0 to nb_group - 1 do group[j].ponte.Name := lal[2];
  for j := 0 to nb_group - 1 do group[j].use := 0;
  for j := 0 to nb_group - 1 do setlength(group[j].mort.vp, nbparmort);
  for j := 0 to nb_group - 1 do setlength(group[j].mat.vp, nbparmat);
  for j := 0 to nb_group - 1 do setlength(group[j].matsanspon.vp, nbparmat);
  for j := 0 to nb_group - 1 do setlength(group[j].ponte.vp, nbparponte);


  for j := 0 to nbparposs - 1 do
  begin
    readln(f1, check);
    lal.DelimitedText := check;
    modele[j].nbterms := lal.Count - 1;
    setlength(modele[j].term, lal.Count - 1);
    //setlength(modele[j].sizeterm,lal.Count);
    setlength(modele[j].nameterm, lal.Count - 1);
    setlength(modele[j].termcov0, lal.Count - 1);
    setlength(modele[j].termcov1, lal.Count - 1);
    setlength(modele[j].firstvi, lal.Count - 1);
    for i := 0 to nb_group - 1 do setlength(group[i].param[j].po, lal.Count - 1);
    for i := 0 to nb_group - 1 do setlength(group[i].param[j].valpo, lal.Count - 1);
    for i := 0 to nb_group - 1 do group[i].param[j].nbterms := lal.Count - 1;
    for i := 0 to lal.Count - 2 do modele[j].term[i] := StrToInt(lal[i + 1]);
  end;

  readln(f1);  //line 'data'
  while not EOF(f1) do
  begin
    readln(f1, check);
    lal.DelimitedText := check;
    if nbcov = 0 then currentgroup := 0
    else
    begin
      for i := 0 to nbcov - 1 do vc[i] := StrToInt(lal[i]);
      currentgroup := getgroup(vc);
    end;
    with group[currentgroup] do
    begin
      nb_ind := nb_ind + 1;
      use := 1;
      setlength(gi, nb_ind);
      i := nb_ind - 1;
      setlength(gi[i].cov, nbcov);
      numhv := 0;
      numevent := 0;
      for j := 0 to nbcov - 1 do gi[i].cov[j] := StrToInt(lal[j]);
      for j := 0 to lal.Count - 1 do if lal[j] = 'mor' then numhv := numhv + 1;
      setlength(gi[i].LH, numhv);
      gi[i].nb_hv := numhv;
      numhv := 1;
      for j := 0 to lal.Count - 1 do
      begin
        if lal[j] = 'sex' then
        begin
          numevent := numevent + 1;
          setlength(
            gi[i].LH[numhv - 1].events, numevent);
          gi[
            i].LH[numhv - 1].events[numevent - 1].Name := 'sex';
          gi[
            i].LH[numhv - 1].events[numevent - 1].t1 := strtofloat(lal[j + 1]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].t2 := strtofloat(lal[j + 2]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].tp := StrToInt(lal[j + 3]);
        end;

        if lal[j] = 'mat' then
        begin
          numevent := numevent + 1;
          setlength(
            gi[i].LH[numhv - 1].events, numevent);
          gi[
            i].LH[numhv - 1].events[numevent - 1].Name := 'mat';
          gi[
            i].LH[numhv - 1].events[numevent - 1].t1 := strtofloat(lal[j + 1]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].t2 := strtofloat(lal[j + 2]);
          if matclutch = 1 then
            gi[i].LH[numhv - 1].events[numevent - 1].tp := StrToInt(lal[j + 3]);
        end;
        if lal[j] = 'pon' then
        begin
          numevent := numevent + 1;
          setlength(
            gi[i].LH[numhv - 1].events, numevent);
          gi[
            i].LH[numhv - 1].events[numevent - 1].Name := 'pon';
          gi[
            i].LH[numhv - 1].events[numevent - 1].t1 := strtofloat(lal[j + 1]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].t2 := strtofloat(lal[j + 2]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].tp := StrToInt(lal[j + 3]);
        end;
        if lal[j] = 'mor' then
        begin
          numevent := numevent + 1;
          gi[
            i].LH[numhv - 1].nb_event := numevent;
          setlength(
            gi[i].LH[numhv - 1].events, numevent);
          gi[
            i].LH[numhv - 1].events[numevent - 1].Name := 'mor';
          gi[
            i].LH[numhv - 1].events[numevent - 1].t1 := strtofloat(lal[j + 1]);
          gi[
            i].LH[numhv - 1].events[numevent - 1].t2 := strtofloat(lal[j + 2]);
          numevent := 0;
          numhv := numhv + 1;
        end;

      end;  //of treating events
    end; //of treating that individual
  end;

  // Generer les non-pontes
  Gnop;

  // Gerer les juveniles non sexés
  for i := 0 to nb_group - 1 do for j := 0 to group[i].nb_ind - 1 do
      if group[i].gi[j].lh[0].events[0].tp = 2 then roughgarden(i, j);

  // Calcul nb ponte et des dates debut/fin chaque evenement
  for i := 0 to nb_group - 1 do for j := 0 to group[i].nb_ind - 1 do
      for k := 0 to group[i].gi[j].nb_hv - 1 do
      begin
        calculnbponte(group[i].gi[j].lh[k]);
        calendrier(group[i].gi[j].lh[k]);
      end;

  // Calcul de l'intervale moyen entre 1ere et 2eme ponte   si matclutch est vrai
  //calcinterp1p2moy;

  closefile(f1);
  lal.Free;
end;

{----read_custom------------------------------------------------------------------------------------------------}
procedure read_custom(var FD: function_D);
var
  lal: TStringList;
  check: string;
  i: integer;
begin
  setlength(suaux.vp, 11);
  //  1    morta            !!! faire distingo entre weibull et ln
  //  11   'to(ps)int';    tradeoff ponte -> surv intercept (evenement ponte augmente le hazard de survie de cette quantité)
  //  12    'to(ps)am'     tradeoff ponte -> surv amortissement du precedent avec le temps
  //  13    'to(ps)tp'     tradeoff ponte -> surv , augmentation du tradeoff en fonction linéaire de la taille ponte en question
  //  14     'sen(pu)t'    senescence de hazard ponte, effet lineaire du temps post-maturité
  // 15     'sen(pu)t2'    senescence de hazard ponte, effet quadratique du temps post-maturité
  // 16     sen(pn)t       senescence de taille ponte, effet linéaire du temps post-maturité
  // 17     sen(pn)t2      senescence de taille ponte, effet quadratique du temps post-maturité
  // 18     to(pupn)       tradeoff intervale ponte i-1 qui change taille ponte i

  reset(fc);
  lal := TStringList.Create;
  lal.Delimiter := ' ';

  for i := 0 to nbparposs - 1 do
    with FD.paramdescript[i] do
    begin
      readln(fc, check);
      lal.DelimitedText := check;
      Name := lal[0];
      minBound := StrToFloat(lal[1]);
      maxBound := StrToFloat(lal[2]);
    end;

  // if fitness_repar='TRUE' then
  // begin
  //   with FD.paramdescript[19] do        //n_offspring est le 11 param ds custom.txt
  //   begin
  //     readln(fc, check);
  //     lal.DelimitedText := check;
  //     name := lal[0];
  //     minBound := StrToFloat(lal[1]);
  //     maxBound := StrToFloat(lal[2]);
  //   end;
  // end;
  lal.Free;
  closefile(fc);
end;

{----read_custom_continuous_var------------------------------------------------------------------------------------------------}
procedure read_custom_continuous_var(flag: boolean);
var
  lal: TStringList;
  check: string;
  i, j: integer;
begin
  if flag then
  begin
    reset(file_continuous_var);
    lal := TStringList.Create;
    lal.Delimiter := ' ';

    for i := 1 to nbcov do
    begin
      readln(file_continuous_var, check);
      lal.DelimitedText := check;

      for j := 0 to covar[nbcov + i].lev - 1 do
      begin
        covar[nbcov + i].valcont[j] := StrToFloat(lal[j]);
      end;
    end;
    lal.Free;
    closefile(file_continuous_var);
  end
  else
  begin
    for i := 1 to nbcov do
      for j := 0 to covar[nbcov + i].lev - 1 do covar[nbcov + i].valcont[j] := j;
  end;

end;

end.
