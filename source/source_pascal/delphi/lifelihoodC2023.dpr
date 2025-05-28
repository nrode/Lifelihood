program lifelihoodC2023;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Unit2 in 'Unit2.pas',
  Alea in 'Alea.pas',
  fmath in 'fmath.pas',
  fspec in 'fspec.pas',
  mathromb in 'mathromb.pas',
  Unit1 in 'Unit1.pas';

begin

{--BLOC PRINCIPAL-------------------------------------------------------------------------------------------------}

TRY
  //lecture des parametres entr�e
  assignfile(input,'');
  reset(input);
  read(input,lect);
  list := TStringList.Create;
  list.Delimiter :=' ';
  list.DelimitedText:=lect;                //ca permet de lire la ligne d'input console et d'en faire une liste de string nomm�e ici list

  //infile
  nomf1:=list[0];
  writeln(nomf1);
  assignfile(f1,nomf1);     // f1 declar� ds unit2
  readata;
  fitness_repar:=StrToInt(list[7]);
  r:=StrToFloat(list[8]);

  //customfile
  nomfc:=list[1];
  assignfile(fc,nomfc);     // fc declar� ds unit2
  readcustom(ll_d);         // = decrireparam code precedent
  //outfile
  Try
  nominfile:=TStringList.Create;
  nominfile.Delimiter :='.';
  nominfile.DelimitedText:=nomf1;
  nomoutfile:=nominfile[0]+'.out';
  assignfile(outfile,nomoutfile);
  rewrite(outfile);
  closefile(outfile);
  Finally
  nominfile.free;
  End;

  //randon generator
  seed1:=StrToInt(list[9]);        savedseed[1]:=seed1;
  seed2:=StrToInt(list[10]);        savedseed[2]:=seed2;
  seed3:=StrToInt(list[11]);        savedseed[3]:=seed3;
  seed4:=StrToInt(list[12]);        savedseed[4]:=seed4;
  marsini(seed1,seed2,seed3,seed4);

  //LL maximization                    //a maodifier ce bloc pour faire des fit gxgroup + remettre procedure ecriture correspondante...
  Init_met_D(The_Met_D);
  interpretation(fitness_repar);      //anciennement readgrid
  Init_f_D( LL_D);
  automatic_met(LL_D, The_Met_D);

  //MCMC
  if StrToInt(list[3])>0 then begin
                              nbsample:=StrToInt(list[3]);
                              intervalsamples:=StrToInt(list[4]);          // interval between each sample
                              promenade(LL_D, The_Met_D);   // a verifier
                              end;
  //standard errors
  if list[5]='true' then calcSE(LL_D);

  //ecriture outfile
   printout_FD(LL_D,nomf1,StrToInt(list[3]));           // OK passer en argument de printout le nb de point de mcmc
   if list[6]='true' then GetAndWriteprobevents(LL_D);
   writeparamdescript(LL_D);

  list.Free;
  writeln('Lifelihoodization terminated');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
