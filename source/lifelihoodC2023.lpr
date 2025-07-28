program lifelihoodC2023;

{$MODE Delphi}

{$ifdef windows}
{$APPTYPE CONSOLE}
{$endif}

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
  //lecture des parametres entrée
  assignfile(input,'');
  reset(input);
  read(input,lect);
  list := TStringList.Create;
  list.Delimiter :=' ';
  list.DelimitedText:=lect;                //ca permet de lire la ligne d'input console et d'en faire une liste de string nommée ici list

  //infile
  nomf1:=list[0];
  assignfile(f1,nomf1);     // f1 declaré ds unit2
  readata;
  fitness_repar:=StrToInt(list[7]);
  r:=StrToFloat(list[8]);

  //customfile
  nomfc:=list[1];
  assignfile(fc,nomfc);     // fc declaré ds unit2
  readcustom(ll_d);         // = decrireparam code precedent
  //outfile
  
  //customfilevalcont
  //If there is no valcont file then valcont are set automatically as ordinals, else the values are read in the file
  if list[16]<>'' then
    valcontflag:=False
  else
    valcontflag:=True;
  writeln('Continuous variable(s) detected: ', valcontflag);
  
  Try
  nomoutfile := ChangeFileExt(nomf1, '.out');
  assignfile(outfile,nomoutfile);
  rewrite(outfile);
  closefile(outfile);
  Finally
  End;

  //randon generator
  seed1:=StrToInt(list[9]);        savedseed[1]:=seed1;
  seed2:=StrToInt(list[10]);        savedseed[2]:=seed2;
  seed3:=StrToInt(list[11]);        savedseed[3]:=seed3;
  seed4:=StrToInt(list[12]);        savedseed[4]:=seed4;
  marsini(seed1,seed2,seed3,seed4);

  // ratiomax for clutch size when senescence present
  ratiomax := StrToFloat(list[13]);
  
  // critical time for the increase in juvenile survival mortality compared to later survival
  tc := StrToFloat(list[14]);
  writeln('tc: ', tc);
  
  // maximum censoring time
  tinf := StrToFloat(list[15]);
  writeln('tinf: ', tinf);

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
  if list[5]='TRUE' then calcSE(LL_D);

  //ecriture outfile
   printout_FD(LL_D,nomf1,StrToInt(list[3]),list[5]);           // OK passer en argument de printout le nb de point de mcmc
   if list[6]='TRUE' then GetAndWriteprobevents(LL_D);
   writeparamdescript(LL_D);

  list.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
