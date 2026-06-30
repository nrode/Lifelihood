program lifelihood;

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

const
  ExpectedArgumentCount = 23;

var
  diagnostic_stage: string;

procedure SetStage(const Stage: string);
begin
  diagnostic_stage := Stage;
end;

procedure PrintFatalError(E: Exception);
begin
  Writeln('Lifelihood Pascal error');
  Writeln('  stage: ', diagnostic_stage);
  if E.ClassName = 'Exception' then
    Writeln('  reason: ', E.Message)
  else
    Writeln('  exception: ', E.ClassName, ': ', E.Message);
  if nomf1 <> '' then Writeln('  input_data: ', nomf1);
  if nomfc <> '' then Writeln('  parameter_bounds: ', nomfc);
  if path_continuous_var <> '' then Writeln('  continuous_values: ', path_continuous_var);
end;

procedure PrintUnhandledFatalError(Obj: TObject; Addr: CodePointer;
  FrameCount: Longint; Frames: PCodePointer);
var
  i: Longint;
begin
  Writeln('Lifelihood Pascal error');
  Writeln('  stage: ', diagnostic_stage);
  if Obj is Exception then
    Writeln('  exception: ', Obj.ClassName, ': ', Exception(Obj).Message)
  else if Obj is TObject then
    Writeln('  exception: ', Obj.ClassName)
  else
    Writeln('  exception: invalid exception object');
  if nomf1 <> '' then Writeln('  input_data: ', nomf1);
  if nomfc <> '' then Writeln('  parameter_bounds: ', nomfc);
  if path_continuous_var <> '' then Writeln('  continuous_values: ', path_continuous_var);
  Writeln('  address: $', HexStr(Addr));
  Writeln('  backtrace:');
  Writeln('    ', BackTraceStrFunc(Addr));
  if FrameCount > 0 then
    for i := 0 to FrameCount - 1 do
      Writeln('    ', BackTraceStrFunc(Frames[i]));
end;

begin

  {--BLOC PRINCIPAL-------------------------------------------------------------------------------------------------}

  ExceptProc := @PrintUnhandledFatalError;

  try
    //lecture des parametres entrée
    SetStage('reading command-line arguments from stdin');
    assignfile(input, '');
    reset(input);
    Read(input, lect);
    list := TStringList.Create;
    list.Delimiter := ' ';
    list.DelimitedText := lect;
    if list.Count <> ExpectedArgumentCount then
      raise Exception.Create(
        'Expected ' + IntToStr(ExpectedArgumentCount) +
        ' space-delimited arguments, got ' + IntToStr(list.Count));
    //ca permet de lire la ligne d'input console et d'en faire une liste de string nommée ici list

    //infile
    SetStage('reading input data file');
    nomf1 := list[0];
    assignfile(f1, nomf1);     // f1 declaré ds unit2
    readata;
    SetStage('reading fitness arguments');
    fitness_repar := list[6];
    r := StrToFloat(list[7]);

    //customfile
    SetStage('reading parameter bounds file');
    nomfc := list[1];
    assignfile(fc, nomfc);     // fc declaré ds unit2
    read_custom(ll_d);         // = decrireparam code precedent
    //outfile

    //customfilevalcont
    //If there is no path_continuous_var file then covar.valcont are set automatically as ordinals, else the values are read in the file
    SetStage('reading continuous covariate values');
    if list[16] = 'NULL' then
    begin
      continuous_var_flag := False;
    end
    else
    begin
      continuous_var_flag := True;
      path_continuous_var := list[16];
      assignfile(file_continuous_var, path_continuous_var);
    end;
    read_custom_continuous_var(continuous_var_flag);

    try
      SetStage('creating output file');
      nomoutfile := ChangeFileExt(nomf1, '.out');
      assignfile(outfile, nomoutfile);
      rewrite(outfile);
      closefile(outfile);
    finally
    end;

    //randon generator
    SetStage('initializing random generator');
    seed1 := StrToInt(list[8]);
    savedseed[1] := seed1;
    seed2 := StrToInt(list[9]);
    savedseed[2] := seed2;
    seed3 := StrToInt(list[10]);
    savedseed[3] := seed3;
    seed4 := StrToInt(list[11]);
    savedseed[4] := seed4;
    mersenne(seed1, seed2, seed3, seed4);

    // ratiomax for clutch size when senescence present
    SetStage('reading numeric fitting controls');
    ratiomax := StrToFloat(list[12]);

    // critical time for the increase in juvenile survival mortality compared to later survival
    tc := StrToFloat(list[13]);
    //writeln('tc: ', tc);

    // maximum censoring time
    tinf := StrToFloat(list[14]);
    //writeln('tinf: ', tinf);

    // sub-interval used to integrate the left and right censoring dates of each event
    sub_interval := StrToFloat(list[15]);
    //writeln('sub_interval: ', sub_interval);

    //LL maximization                    //a maodifier ce bloc pour faire des fit gxgroup + remettre procedure ecriture correspondante...
    SetStage('initializing metropolis controls');
    Init_met_D(The_Met_D);
    SetStage('interpreting model configuration');
    interpretation();      //anciennement readgrid
    SetStage('initializing likelihood function');
    Init_f_D(LL_D);
    SetStage('running likelihood optimization');
    automatic_met(LL_D, The_Met_D);

    //MCMC
    SetStage('running MCMC');
    if StrToInt(list[2]) > 0 then
    begin
      nbsample := StrToInt(list[2]);
      intervalsamples := StrToInt(list[3]);
      // interval between each sample
      promenade(LL_D, The_Met_D);   // a verifier
    end;
    //standard errors
    SetStage('computing standard errors');
    if list[4] = 'TRUE' then calcSE(LL_D);

    //ecriture outfile
    SetStage('writing output files');
    printout_FD(LL_D, nomf1, StrToInt(list[2]), list[4]);
    // OK passer en argument de printout le nb de point de mcmc
    if list[5] = 'TRUE' then GetAndWriteprobevents(LL_D);
    writeparamdescript(LL_D);

    list.Free;
  except
    on E: Exception do
    begin
      PrintFatalError(E);
      Halt(1);
    end;
  end;
end.
