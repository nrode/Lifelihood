(* version daphnie ou ds les données la premiere ponte et la maturité sont confondues;
ce qui fait qu'on ajoute une valeur de taille de ponte (tp) à la maturité et ca vaut zero pour ceux qui ont pas pondu/maturé
on a aussi 'eteint' tout ce qui concerne sex-ratio
attention au denombrement des events par life-history: le fait de confondre maturite et 1ere ponte change les calculs du nbevents et nbponte

mais on a fait des modifs a integrer ds toutes les version lifeprob ulterieures
-fonction erfra etait fausse, maintenant deux fonctions: erfrapos et erfra
-bug ds metropolis (mars a appler avant le deuxieme uni
- ajout des closefile un peu partout
- espace enlevés pour bon alignement ds format de sortie en batch
- ajout parametres ds les trade-off survie repro : dn et da  (respectivement effet taille de ponte et prametre d'amortissement du hazard de survie)
  ajoute fonction Sda
  fonction censored_mort modifée pour appeler Sda plutot que Sd
- nb parposs augmenté, donc f modifié
- survfunctype.vp [1..11] plutot que [1..5] car le nb param pour ponte augmente grave
- ajout nom des param au debut ligne modele pour chacun ds fichier d'entrée
- nvele proc calculnbponte qu'on fait au debut avec calendrierbis pour obtenir une bonne fois pour toutes le nb de ponte de chaque hv
- ajoute le numero de l'evenement ds l'hv pour mieux s'y retrouver

LISTE BUGS COURANTs
 -  attention aux valeurs des variables continues initialisées ds le code, verifier les niv de facteur correspondant
 - attention date de censure      !!!!!!!!!!!!!!!!!!!!
 - attention date critique initiale      !!!!!!!!!!!!!!!!!!!!
 - Les indices commencent à 0 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 A FAIRE
 tx ponte pour lgnormal pour reparfitness           //FAIT
 reflechir harmoniser fitness quand matclutch=true ou false      //FAIT

 ATTENTION POUR DEBUGGAGE fx (fonction sous l'integrale pour W repar) ne prend pas en compte mat et survie male

*)


unit Unit2;

{$MODE Delphi}

interface
{$O+}
uses Alea, SysUtils,math, fspec,fmath;

const
  theta = 2.5066282746310; { racine de 2Pi }
  racine2 = 1.414213562;  //racine de 2
  p_erfra = 0.47047;       { constants for Erf x rational approx Abram&Stegun p299, 7.1.25 }
  a1_erfra = 0.3480242;
  a2_erfra = -0.0958798;
  a3_erfra = 0.7478556;
  tinf = 1000;               //attention date de censure      !!!!!!!!!!!!!!!!!!!!               // a ajouter option reglable par utilisateur!!!!
  tc = 20;                    //attention date critique initiale      !!!!!!!!!!!!!!!!!!!!        // a ajouter option reglable par utilisateur!!!!
  intint=0.3;                 //ds generateur histoires de vies
  minus=0.00000000000000000000000000000000001;
  nbparposs=19;
  nbparmort= 5 ; {  mortuf  morta  Rmortum mortp propmal}
  nbparmat = 3 ; {  matuf mata Rmatum }
  nbparponte = 11; {pontu  ponta  (W)pontn  to(ps)int  to(ps)am  to(ps)tp  sen(pu)t  sen(pu)t2  sen(pn)t  sen(pn)t2  to(pnpu) }
  ratiomax = 10;  //facteur multiplicatif max de la taille de ponte avec les modeles qui incluent une senescene de repro cf fonction CalculRatioEspPoissonTronque

type
  var_info_type = record
                  minBound, maxBound, value, Bestvalue, step,SE: double;
                  sample:Array of double; //echantilloner distrib du param pour estimer supp lim
                  name: string;
                  typ, loc: integer;
                  end;
  param_type = record
                  minBound, maxBound, value: double;
                  name: string;
                  end;
  param_D = array[0..nbparposs-1] of param_type   ;
  function_D = record
               CurrResult, BestResult, MeanResult, VarResult: double;
               var_info: array of var_info_type;
               sample:Array of double; //echantilloner LL
               number_of_variables: integer;
               paramdescript:param_D;
               end;
  Metropolis_D = record
                 temp0, tempf, bgeup, bgedown, tT, t0, climbrate, pacc,precision: double;
                 ntr, nst,maxrep: integer;
                 end;
  event_type = record
               name: string[3];
               t1, t2, debut, fin: double;
               tp: integer;
               //num: integer;              //numero de l'event ds la lh : pas utile jusqu'ici
               end;
  life_history = record
                 events: Array of event_type;
                 nb_event: integer;
                 nbponte:integer;
                 end;
  covar_info=record
             typ:integer; //0:fact   1:continuous
             lev: integer;    //level  0...n  = factor, -1 continuous
             name:string;
             valcont:array of double;     //valeurs si utilisée en var continues
             end;
  ind_info = record
             nb_hv,nb_cov: integer;
             //sex : integer;      {0:fem  1:male  }
             lh: array of life_history;
             cov: array of double; //covariable values
             end;
  survfunctype = record
                 name: string[3];
                 vp: array of double;
                 end;
  vparam = Array [0 .. nbparposs-1] of integer;
  modeleparam=record
              nbterms:integer;
              term,firstvi:array of integer;
              termcov0,termcov1:array of integer;
              nameterm:array of string;
              end;
  modeleparaminst=record
              nbterms:integer;
              po:array of integer;
              valpo: array of double;
              end;
  modeletype=array[0..nbparposs-1] of modeleparam;
  modeletypeinst=array[0..nbparposs-1] of modeleparaminst;
  groupe_info = record
                gi: Array of ind_info;
                //ll:double;
                use, nb_ind: integer;
                //name: string;
                matsanspon,mat, mort, ponte: survfunctype;            //quand matclutch = true 'mat' refere au temps de mat + 1ere ponet, ce n'est pas a proprement parlé 'mat', d'ou cette survfunctype matsanspon pour le calcul propre de la fitness ds ce cas
                param: modeletypeinst; { numero des vi correspondante pour chaque param}
                end;
  mat=array of array of double;
  vect=array of double;
  vecti=array of integer;
  filetype= record
          nom:string;
          content:text;
          end;
var
  modele:modeletype;
  nbcov,nbsample,intervalsamples:integer;
  covar: array of covar_info;
  group: array of groupe_info;
  LL_D: function_D;
  The_Met_D: Metropolis_D;
  nb_group, nb_current_group, max_vars: integer;
  //f1:filetype;
  f1,fc,outfile: text;
  nom, nomf1,nomfc,nomoutfile, nommodfile: string;
  fitness_repar,matclutch:byte;
  r,xratiomax:double;
  H:mat;
  intervalp1p2:double; //intervale de temps entre 1ere et 2eme ponte pour tous les individus, utile quand matclutch=true pour avoir une idee moyenne de l'intervale entre maturite reelle et premiere ponte
  savedseed:array[1..4] of integer;
  suaux: survfunctype  ;


function max(x, y: double): double;
procedure printout_FD(var FD: function_D;nomf:string;nbrun:integer;exportinvhessian:String);
procedure automatic_met(var function_des: function_D; var Metrop_des: Metropolis_D);
procedure Init_f_D(var FD: function_D);
procedure interpretation(var fitrep:byte);
procedure ghv( var a,b:integer)      ;
procedure gnop     ;
Procedure calendrier(var hv: life_history);
procedure calculnbponte(var hv:life_history);
procedure calcinterp1p2moy;
procedure roughgarden(a,b:integer)      ;
procedure calcSE(var fd: function_D);
function getcov(group,covnum:integer):integer;
function getgroup(v:vecti):integer;
procedure decrireparam(var FD: function_D);
procedure promenade (var Fun_Des: function_D; var Met_Des: Metropolis_D);
procedure GetAndWriteprobevents(var FD: function_D);
procedure writeparamdescript(var FD: function_D);

implementation

{----------------------------------------------------------------------------------------------------}
function getgroup(v:vecti):integer;      //obtiens le num du groupe a partir des valeurs des cov individuelles
begin
Case nbcov of
0: getgroup:=0;
1: getgroup:=v[0];        //numero des groupes commence à 0!!
2: getgroup:=v[0]*(covar[2].lev)+v[1];
3: getgroup:=v[0]*(covar[2].lev*covar[3].lev)+v[1]*(covar[3].lev)+v[2];
end;
end;

{-------------attention procedure changée pour avoir l'ordre des interactions comme ds R-----------------
function getcov(group,covnum:integer):integer;                // renvoie la valeur du facteur correpondant a la covariable covnum pour le groupe numero 'group'
var a,help:integer;
begin
if covnum<(nbcov+1) then help:=covnum else help:=covnum-nbcov;    //si cov continue, donne le bon numero
Case nbcov of
0: getcov:=0;
1: getcov:=group;
2: case help of
        2: getcov:=group div (covar[2].lev)               ;
        1: getcov:=group mod (covar[2].lev)   ;
   end;
3: Begin
   a:= group div (covar[2].lev*covar[3].lev)   ;
   case help of
        3: getcov:=a             ;
        2: getcov:=(group-a*(covar[2].lev*covar[3].lev)) div covar[3].lev     ;
        1: getcov:=(group-a*(covar[2].lev*covar[3].lev)) mod covar[3].lev     ;
   end;
   End;
end;
end;                 }



{--------------------------------version originale--------------------------------------------------------------------}
function getcov(group,covnum:integer):integer;                // renvoie la valeur du facteur correpondant a la covariable covnum pour le groupe numero 'group'
var a,help:integer;
begin
if covnum<(nbcov+1) then help:=covnum else help:=covnum-nbcov;    //si cov continue, donne le bon numero
Case nbcov of
0: getcov:=0;
1: getcov:=group;
2: case help of
        1: getcov:=group div (covar[2].lev)               ;
        2: getcov:=group mod (covar[2].lev)   ;
   end;
3: Begin
   a:= group div (covar[2].lev*covar[3].lev)   ;
   case help of
        1: getcov:=a             ;
        2: getcov:=(group-a*(covar[2].lev*covar[3].lev)) div covar[3].lev     ;
        3: getcov:=(group-a*(covar[2].lev*covar[3].lev)) mod covar[3].lev     ;
   end;
   End;
end;
end;

{ ************************************************************************ }
 function link(x:double; var pt:param_type):double;
begin
if x>200 then link:=pt.maxbound-minus else
if x<-200 then link:=pt.minbound+minus else
           link:=pt.minBound+(pt.maxBound-pt.minBound)/(1+Exp(-x)) ;
end;

{ ************************************************************************ }
function delink(val:double; var pt:param_type):double;
begin
if val<=pt.minbound then begin
                        //messageform.Memo2.Lines[6]:=floattostr(val);
                        delink:=-200;
                        end
else if val>=pt.maxbound then begin
                        //messageform.Memo2.Lines[7]:=floattostr(val);
                        delink:=200;
                        end
else delink:=Ln((val-pt.minBound+0.0000000000000000)/(pt.maxBound-val+0.0000000000000000)) ;
end;

(****************************************************************)
function check(var hv:life_history):boolean;
var i,test : integer;
begin
test:=1;
 for i := 1 to hv.nb_event - 1 do if (hv.events[i].t1<hv.events[i-1].t1) and (hv.events[i].t2<tinf) and (hv.events[i-1].t2<tinf) then test:=test*0;
 If test=0 then check:=false else check:=true;
end;

{ *****generateur histoire de vie*****A REVOIR!!!************************************* }
procedure ghv(var a,b:integer)      ;
var hvvirt:life_history;
    indvirtuel:ind_info;
    interv,diviseur,pos:array of integer;
    i,j,ntot,compteur,reste:integer;

begin
with group[a].gi[b].lh[0] do
  begin
  setlength(interv,nb_event);
  for i := 0 to nb_event - 1 do if(events[0].t2=tinf) or (events[i].t2=tinf) then interv[i]:= 1 else interv[i]:= round((events[i].t2-events[i].t1)/intint) ;  {les hv ou mat est censurée: pas de decoupage (ni mat ni mort) et les morts censuree non decoupées aussi }
  setlength(diviseur,nb_event);
  diviseur[nb_event - 1]:=1;
  for i := 0 to nb_event - 2 do  diviseur[nb_event - 2-i]:=  diviseur[nb_event - 1-i]*interv[nb_event - 1-i];
  ntot:=diviseur[0]*interv[0];  (*nb total d'histoire de vie a parcourir*)
  compteur:=1;
  for i:= 0 to ntot - 1 do
             BEGIN
             setlength(pos,nb_event)  ;
             reste:=i;
             for j := 0 to nb_event - 2 do begin
                                           pos[j]:=reste div diviseur[j];
                                           reste:= reste mod diviseur[j]
                                           end;
             pos[nb_event-1]:=reste mod diviseur[nb_event - 2];
             setlength(hvvirt.events,nb_event);
             hvvirt.nb_event:=nb_event;
             for j := 0 to nb_event - 1 do
                              begin
                              hvvirt.events[j].t1:=events[j].t1+pos[j]* ((events[j].t2-events[j].t1)/interv[j])      ;
                              hvvirt.events[j].t2:=events[j].t1+(pos[j]+1)* ((events[j].t2-events[j].t1)/interv[j])  ;
                              hvvirt.events[j].name:=events[j].name;
                              if events[j].name='pon' then hvvirt.events[j].tp:= events[j].tp;
                              end;
             if check(hvvirt) then begin
                                   setlength(indvirtuel.lh,compteur);
                                   indvirtuel.lh[compteur-1]:=hvvirt;
                                   compteur:=compteur+1;
                                   end;
             END;

indvirtuel.nb_hv:=compteur-1;
//indvirtuel.sex:=group[a].gi[b].sex;
group[a].gi[b]:=indvirtuel;
end;
end;

(*****************************************************************)
procedure gnop;            {*creation des evenement non-ponte*}
var i,j,k,numevent:integer;
begin
 for i:= 0 to nb_group - 1 do
 for j := 0 to group[i].nb_ind - 1 do
 for k := 0 to group[i].gi[j].nb_hv - 1 do
   if (group[i].gi[j].lh[k].events[1].t2<tinf) and (group[i].gi[j].lh[k].events[0].tp=0) then begin              //c'est une femelle qui a maturé
                                                  numevent:= group[i].gi[j].lh[k].nb_event+1;
                                                  group[i].gi[j].lh[k].nb_event:=numevent;
                                                  setlength(group[i].gi[j].lh[k].events,numevent)  ;            {on ajoute evenement non ponte si il y a eu maturite}
                                                  group[i].gi[j].lh[k].events[numevent-1].name:='nop';
                                                  group[i].gi[j].lh[k].events[numevent-1].t1:= group[i].gi[j].lh[k].events[numevent-2].t1      ;        {    nop.t1 = mort.t1  }
                                                  group[i].gi[j].lh[k].events[numevent-1].t2:=tinf;     {on censure artificiellement l'evenement non ponte par definition}   //NB: si nop.t1 est negatif (quand mort et derniere ponte ds le meme intervale) alors c'est pas grave car les fonction surv sont protegée, ca donne une proba de 1 a l'evenement nop qui reflete l'absence d'info dans ce cas (on ne sait rien avec certitude: par exemple la mort peut intervenir au tout debut de l'intervale, ce qui correspondrait a un t1 de nonponte de zero)
                                                  end;
end;

{ *****generateur histoire de vie des sexe 2: vie femelle et la vie male************************* }
procedure roughgarden(a, b:integer)      ;
var indvirtuel:ind_info; i:integer;
begin
with group[a].gi[b] do
  begin
  setlength(lh,2);
  nb_hv:=2;
  setlength(lh[1].events,lh[0].nb_event);
  lh[1].nb_event:=lh[0].nb_event;
  lh[0].events[0].tp:=0;
  for i:=0 to lh[1].nb_event-1 do lh[1].events[i]:=lh[0].events[i];
  lh[1].events[0].tp:=1;
  end;
end;

{ ************************************************************************* }
function erfrapos(x: double): double;
{ Erf x rational approx Abram&Stegun p299, 7.1.25, for x>0 }
var
  t: double;
Begin
  if x>3.5 then erfrapos:= 1-minus else
  begin
  t := 1 / (1 + p_erfra * x);
  erfrapos := 1 - (a1_erfra * t + a2_erfra * t * t + a3_erfra * t * t * t) * Exp
    (-(x * x));
  end;
End;

{ ************************************************************************* }
function erfra(x: double): double;
{ use the fact that erf(x)=-erf(-x) }
Begin
 if x>0 then erfra:=erfrapos(x) else erfra:=-erfrapos(-x)
End;

{ ************************************************************************ }
function Incomplete_gamma_mathematica(a, b: double): double;                           //Verifié c'est bien Gamma[a,b] mathematica
begin
  Incomplete_gamma_mathematica := JGamma(a, b) ;
end;

{ ************************************************************************ }
function gamma_regularized_mathematica(a, b: double): double;                          //Verifié c'est bien GammaRegularized[a,b] mathematica
begin
  gamma_regularized_mathematica := JGamma(a, b);
end;

{ ************************************************************************* }
function surv( x: double; var su: survfunctype; sex:integer): double;
var   ans,eps,paramaux: double;
Begin
if x>=tinf then surv:=0 else
begin
  eps:=0.00000000000000000000001;
  With su do
  begin

    if name = 'exp' then            //ici hazard constant = 1/vp0  ; esperance du tps de survie est vp0
    BEGIN
      if vp[0] <= 0 then vp[0] := eps;
      if x <= 0 then  surv := 1
      else  if (sex=1) and (vp[2]>0) then surv := Expo(-(x / (vp[0]*vp[2]))) else surv := Expo(-(x / vp[0])) ;
    END;

      if name = 'wei' then                    //hazard est weibull, d'esperance vp0 ici. En version mathematica Weibull[vp1,paramaux] et esperance de cette loi vaut paramaux Gamma[1+1/vp1]
    BEGIN
      if vp[0] <= 0 then  vp[0] := eps;
      if vp[1] <= 0 then  vp[1] := eps;
      paramaux:= vp[0]/Gamma(1+1/vp[1]);          //donc ici vp[0] est l'esperance du temps d'attente de l'evenement
      if x <= 0 then      surv := 1
      else  if (sex=1) and (vp[2]>0) then surv := Expo(-(Power((x / (paramaux*vp[2])), vp[1]))) else surv := Expo(-(Power((x / paramaux), vp[1]))) ;
    END;

    if name = 'lgn' then
    BEGIN
      if vp[0] <= 0 then  vp[0] := eps;           //donc ici vp[0] est aussi l'esperance du temps d'attente de l'evenement
      if vp[1] <= 0 then  vp[1] := eps;
      if x <= 0 then   surv := 1
      else  begin
            if (sex=1) and (vp[2]>0) then  paramaux:= Ln(vp[0]*vp[2])-0.5*vp[1]*vp[1]       // esperance LgNormal(mu,sig) = Exp[mu+sig2/2]     //esperance x vp[2] si c'est un male
                                     else  paramaux:= Ln(vp[0])-0.5*vp[1]*vp[1]   ;    // esperance LgNormal(mu,sig) = Exp[mu+sig2/2]
            surv := 0.5*(1 + erfra((paramaux - ln(x) ) / (vp[1]* racine2)) ) ;          //surv = 1-CDF[LogNormale[mu,sig],x]  ou mu == paramaux  (on le calcule en reparametrant avec l'esperance vp0
            end;
    END;

    if name = 'gam' then
    BEGIN
      if vp[0] <= 0 then  vp[0] := eps;           //donc ici vp[0] est aussi l'esperance du temps d'attente de l'evenement
      if vp[1] <= 0 then  vp[1] := eps;
      if x <= 0 then   surv := 1
      else  if (sex=1) and (vp[2]>0) then  paramaux:= (vp[0]*vp[2])/ vp[1]        // esperance gamma(a,b) = ab          b = 1/paramaux
                                     else  paramaux:= vp[0] / (vp[1]);
      surv :=  gamma_regularized_mathematica(paramaux,x/vp[1]) ;    //surv = 1-CDF[Gamma[a,b],x] == 1-GammaRegularized[a,0,x/b] ==  GammaRegularized[a,x/b]   //CHECKé PENDANT 2 JOURS!!!!
    END;

  end; { du with }
 end; {du else}
end; { de surv }

{ ************************************************************************* }
function survp(var x: double; var  su: survfunctype; sex:integer): double;
begin
if su.vp[3]>0
   then begin if (x < tc) then survp := 1 else survp := su.vp[3] * surv(x, su, sex) end
   else survp := surv(x, su, sex);
end;

{ ************************************************************************* }
function censored_mat( t1, t2: double;var su: survfunctype; sex:integer): double;
begin
  censored_mat := surv(t1, su, sex) - surv(t2, su, sex);
end;

{ ************************************************************************* }
function CalculRatioEspPoissonTronque( var ev: event_type; var pon: survfunctype; var hv:life_history ):double;
var sum,tfrommat:double   ;
begin
if ev.name='pon' then  tfrommat:=  ev.fin-hv.events[1].fin else {si c'est 'mat'} tfrommat:=0  ;        // temps depuis la maturite qui est l'evenement [1]     vrai qqsoit matclutch si ev.name=pon sinon, si matcluth=true et ev.name='mat' compte senescence a partir de la mat=1ere ponte// Attention ca veut dire que senescence de taille de ponte ne veut pas dire exactement meme chose en matclutch true et false (false: senescence accumule depuis mat; true: senescence accumule depuis 1ere ponte(qui est aussi mat))
sum:= tfrommat*(pon.vp[8]+pon.vp[9]*tfrommat) + pon.vp[10]*(ev.fin-ev.debut) ;  // intercept + senpent t + senpentt t2 + to(pupn) duréeDernierintervaledeponte
if sum>20 then CalculRatioEspPoissonTronque:=ratiomax
          else if sum<-20 then CalculRatioEspPoissonTronque:=minus
                          else CalculRatioEspPoissonTronque :=  ratiomax / (1+Exp(xratiomax-sum))     ;   //ajusté pour valoir 1 quand sum=0: donc pas d'effet si vp8,vp9,vp10 sont nuls  //attention esp d'une loi de poisson tronquée ne peut pas etre <1     d'ou la condition sur espPoissontronque*ratio dans la procedure probevent
end;

{ ************************************************************************* }
function Survtotpon(tfromlastpon,tfrommat:double; var su: survfunctype; var lh:life_history;sex:integer ): double;         //fonction qui corrige S(t) en ajoutant un increment /decrement de hazard pour senescence du taux de ponte. attention le temps en argument est le temps depuis maturité!!
var i:integer;
    vp0,vp6,vp7: double;

BEgin                                                          //ATTENTION A FAIRE PROTECTION SUR VP[0]
if (su.vp[6]<>0) or (su.vp[7]<>0)
then
  begin
  suaux.name:=su.name;
  suaux.vp[1]:=su.vp[1];

  vp0:=delink(su.vp[0],LL_D.paramdescript[8]);
  vp6:=delink(su.vp[6],LL_D.paramdescript[14]);
  vp7:=delink(su.vp[7],LL_D.paramdescript[15]);
 // tfrommat:=t-lh.events[1].fin;    // temps depuis la maturite qui est l'evenement [1]     vrai qqsoit matclutch
  suaux.vp[0]:= link(vp0 + tfrommat*(vp6+tfrommat*vp7)  , LL_D.paramdescript[8])  ;         //change la moyenne de la fonction su localement en y mettant un effet du tfrommat
  Survtotpon :=   surv(tfromlastpon,suaux,sex);
    //sumhaz:=  tfrommat * tfrommat*(senput * 0.5 + senputt*tfrommat *0.3333333);          //l'integrale de l'ecart de hazard polynomial
    //if sumhaz>=0 then Survtotpon:=  Exp(-sumhaz)* surv(t,su,sex)                // Stot = St Exp(-somme increment hazard)
    //             else Survtotpon:=1-(1-surv(t,su,sex))* Exp(sumhaz);           // Stot = bidouille pour que Stot reste <1 mais plus grde que St si sumhaz<0
  end
else Survtotpon :=   surv(tfromlastpon,su,sex);

ENd;

{ ************************************************************************* }
function Sda(t,d,da,dn:double; var lh:life_history;sex:integer): double;         //fonction qui corrige S(t) en ajoutant le hazard cumulé du fait d'un trade off survie repro : la bonne fonction de survie devient le produit S(t) Sda(t,d,lh). Mais ici le l'increment de hazard a chaque ponte (d+dn*taille ponte) est amorti exponentiellement au taux da.
var i,aux:integer;
    sumhaz,ttodeath: double;
BEgin
if matclutch=1 then aux:=0 else aux:=1;
if lh.nbponte=0 then Sda:=1  //pas de ponte     // les males n'ont pas de ponte de toutes les facons
                else begin
                   sumhaz:=0;
                   for i:=1 to lh.nbponte do
                        begin
                        ttodeath:= (t-lh.events[i+aux].fin);  //aux = 1 :commence a la premiere  ponte dont l'indice est event[2]    //aux = 0 : commence a la maturite qui est une ponte et dont l'indice est event[1]
                        if ttodeath<0 then ttodeath:=0;     //WARNING dire qu'il y a un pb ds les données    //mort est le dernier evenement comme ca devrait, sauf erreur ds les données
                        if (da*ttodeath >11)
                        then sumhaz:=sumhaz+(d+dn*lh.events[i+aux].tp)/da  //le terme Exp est nul  l'integrale vaut d/da
                        else if (da*ttodeath < 0.0001)
                             then sumhaz:=sumhaz+(d+dn*lh.events[i+aux].tp) *ttodeath //l'amortissement est presque nul, l'intgrale vaut d * t
                             else sumhaz:=sumhaz+(d+dn*lh.events[i+aux].tp)*(1-Exp(-da*ttodeath))/da;   //l'integrale vaut d(1-exp(-da t))/da
                        end;
                   Sda:= Exp(-sumhaz)          //S(t) = Exp(-somme hazard cumulé)
                   end;
ENd;

{ ************************************************************************* }
function censored_mort( t1, t2: double; var su: survfunctype; sex:integer; var hv:life_history;d,da,dn:double): double;
begin
if (su.vp[3]>0)  then                     //i.e. if mortp est utilisé
  BEGIN
  if (t2 < tc) then
               censored_mort := 1 - su.vp[3] * surv(tc, su, sex) * Sda(tc,d,da,dn,hv,sex)
                              { tc est le temps critique de survie juvenile }
               else if t2 < tinf then if t1 < tc then censored_mort := 1 - survp(t2, su, sex) * Sda(t2,d,da,dn,hv,sex)
                                                 else censored_mort := survp(t1, su, sex)*Sda(t1,d,da,dn,hv,sex) - survp(t2, su, sex)*Sda(t2,d,da,dn,hv,sex)
                                 else if t1 < tc then censored_mort := 1
                                                 else censored_mort := survp(t1, su, sex)*Sda(t1,d,da,dn,hv,sex);
  END
else
  BEGIN
  if t2 >= tinf then  censored_mort := surv(t1, su, sex) * Sda(t1,d,da,dn,hv,sex)
                else  censored_mort := surv(t1, su, sex)*Sda(t1,d,da,dn,hv,sex) - surv(t2, su, sex)*Sda(t2,d,da,dn,hv,sex);
  END;

end;

{ ************************************************************************* }
function ppoissontrunc( esp: double; var  k: integer): double;
var
  P,mu: double;
  I: integer;
Begin
//  on veut retrouver l'esperance de la loi de poisson (mu) qui sert a generer la tronquée à partir de l'esp de la tronquée (esp)
// on sait que esp = mu/(1-Exp(-mu))
// on fait l'approx mu = esp-Exp(-(esp-1)) qui n'est pas triviale mais qui marche bien
//ATTENTION esp est forcement >= 1

mu :=   esp -Expo(-(esp-1))  ;

  If (mu <= 0) or (k < 0) then
    ppoissontrunc := 0.000000000000000001
  else
  if mu>20*k then  ppoissontrunc:=0.00000004
  else
  begin
    P := mu;
    for I := 2 to k do
      P := P * mu / I;
    ppoissontrunc := P / (Expo(mu) - 1);
  end;
End;

(****************************************************************)
function fx(var t,r:double; var legroupe:groupe_info):double;        //ATTENTION SENESCENCE DE REPRO ratiotailleponte pas inclu ds le calcul
   //var matur,rate,survie : double;
begin                                                                                 //NB ma == matsanspon si matclutch = true et ma == mat sinon
   //matur:=1-surv(t,ma);
   //survie:=surv(t,mo)*Expo(-r*t);
   //rate:= pon.vp[2] * Power(pon.vp[1],-pon.vp[2])*Power(t,pon.vp[2]-1);
   //fx:=matur*survie*rate ;
   with legroupe do
   begin
   if ponte.vp[0]<=0 then fx:=0
                   else if matclutch=0 then fx:= (*0.5*)( (1-surv(t,mat,0))*survp(t,mort,0) (*+ (1-surv(t,mat,1))*survp(t,mort,1)*) )    *Expo(-r*t) * (1/ponte.vp[0] )     //vp[0] est l'esperance du temps de ponte qqsoit la loi (exp, wei, lgn, gam), donc 1 sur cette esperance est le taux instatané de ponte  //le reste (survie, maturite) moyenne male/fem
                                           else fx:= (*0.5*)( (1-surv(t,mat(*sanspon*),0))*survp(t,mort,0) (*+ (1-surv(t,mat,1))*survp(t,mort,1)*) )    *Expo(-r*t) * (1/ponte.vp[0] )    ;


   // fitness:=romb(fitnessinst,0,?,0.001) probleme de passer arguments variés de fitnessinst ds romb
   end;
end;

{ ************************************************************************ }
function romb(var r:double; var legroupe:groupe_info):double;
{ numerical integration by the Romberg method }
{ function fx, name cannot be passed by Turbo Pascal}
VAR
	nx			: ARRAY[1..16] of Integer;
	t			: ARRAY[1..136] of double;
	done,error		: Boolean;
	pieces,nt,i,ii,n,nn,
	l,ntra,k,m,j		: Integer ;
	delta_x,c,sum,fotom,x, lower,upper,tol	: double ;


BEGIN
With legroupe do begin
  lower:=0;
  upper:=min(5*mort.vp[0],tinf);  //On se limite ici a un tmps 10 fois la moyenne, pour pas se casser les pieds avec les differentes distribs, assez large                            ///min(mort.vp[0]*Power(Ln(70),1/mort.vp[1]),tinf)      ;//VERIF EN LOGNORMALE!!!!  //la borne sup pour integrer fitness inst est prise telle que >99% individus sont morts a ce temps la. C'est vp0 Log(100)^(1/vp1)
  tol:=0.005;
  done := false;
  error := false;
  pieces := 1;
  nx[1] := 1;
  delta_x := (upper-lower)/pieces;
  c := (fx(lower,r,legroupe)+fx(upper,r,legroupe))*0.5;
  t[1] := delta_x*c;
  n := 1;
  nn := 2;
  sum := c;
  REPEAT
    inc(n); //n := n+1;
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
	sum := sum+fx(x,r,legroupe)  ;
       // nbcall:=nbcall+1
      END;
    t[nn] := delta_x*sum;
//    form1.memo1.lines[n]:=floattostrf(pieces,fffixed,3,3)+'  '+floattostrf(t[nn],fffixed,9,9);
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
//    form1.memo2.lines[n]:=floattostrf(j,fffixed,9,9)+'  '+floattostrf(t[j],fffixed,9,9);

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
  result := t[j]
end;  //du with
END;		{ ROMBERG }

{ ************************************************************************ }
Procedure calendrier(var hv: life_history);
var
  nbponte, I: integer;                         {sex, mat,(pon)xn,mor,nop}
begin                                          { 0 ,  1 ,  ...,  nbevent-2, nbevent-1}

  for I := 0 to hv.nb_event -1 do       //tous les evenements
    begin
    //hv.events[I].num:=I;  //calcul du numero de l'event: pas utile jusqu'ici
    hv.events[I].fin := (hv.events[I].t1 + hv.events[I].t2) / 2;
    if hv.events[I].name='mat' then hv.events[I].debut:=0;
    if hv.events[I].name='pon' then hv.events[I].debut:=hv.events[I-1].fin;
    if hv.events[I].name='mort' then hv.events[I].debut:=0;
    if hv.events[I].name='nop' then hv.events[I].debut:=hv.events[I - 2].fin;    { le derniere ponte est l'evenement I-2, au pire la maturite}
    end;
 //nb: les debut sont utiles pour les pontes maturite et nop
 //fins sont foireux pour tous les evenements censurés ou t2 = tinf, mais prete pas a consequences
 //
end;

{ ************************************************************************ }
procedure calculnbponte(var hv:life_history);
begin
if matclutch=1 then      //mode daphnie : la maturite est  une ponte
    if hv.events[1].tp=0 then hv.nbponte:=0 else hv.nbponte:=1+hv.nb_event-4;   //attention modifié pour daphnie ou mat=1ere ponte  et compte ds nbponte
if matclutch=0 then     //mode artemie : la maturite n'est pas une ponte
    hv.nbponte := round(max(hv.nb_event-4,0));
end;

{ ************************************************************************ }
procedure calcinterp1p2moy;
var i,j,nbind2pontes:integer;
    moy:double;
begin
nbind2pontes:=0;
moy:=0;

for i:= 0 to nb_group - 1 do  for j := 0 to group[i].nb_ind - 1 do
    if group[i].gi[j].lh[0].nbponte>1
    then begin
         nbind2pontes:=nbind2pontes+1;
         moy:=moy+ (group[i].gi[j].lh[0].events[2].fin- group[i].gi[j].lh[0].events[2].debut)
         end;
if nbind2pontes>0 then intervalp1p2:=moy/nbind2pontes else intervalp1p2:=0;

end;

{ ************************************************************************ }
function probevent(var ev: event_type; var mo, ma, pon: survfunctype;var integrale:double; sex:integer; var hv:life_history): double;
var probmale,survmale,survfem,esptailleponte:double;
begin
              if ev.name = 'sex' then if mo.vp[4] =0  then probevent:=1 else
                                      begin
                                      probmale:=mo.vp[4];
                                      probevent:=sex* probmale + (1-sex)*(1-probmale)+minus;
                                      end;
              if ev.name = 'mor' then probevent := censored_mort(ev.t1, ev.t2, mo, sex,hv,pon.vp[3],pon.vp[4],pon.vp[5]) + minus;
              if ev.name = 'mat' then
                                 if matclutch=0 then probevent := censored_mat(ev.t1, ev.t2, ma, sex) + minus
                                                    else if ev.tp>0 then begin
                                                                         if fitness_repar=1 then esptailleponte:= pon.vp[2]*CalculRatioEspPoissonTronque(ev, pon, hv)/integrale else esptailleponte:= pon.vp[2]*CalculRatioEspPoissonTronque(ev, pon, hv);
                                                                         if esptailleponte<=1 then  esptailleponte:=1+minus;  // attention du fait du ratio cette esperance pourrait se retrouver <1 et ce n'est pas possible mathematiquement
                                                                         probevent:=censored_mat(ev.t1, ev.t2, ma, sex)*ppoissontrunc(esptailleponte, ev.tp) +minus     ;
                                                                         end
                                                                    else probevent := censored_mat(ev.t1, ev.t2, ma, sex) + minus;

              if ev.name = 'nop' then probevent := survtotpon(ev.t1-ev.debut,ev.t1-hv.events[1].fin,pon,hv,sex) + minus;
              if ev.name = 'pon' then
                       begin
                       if fitness_repar=1 then esptailleponte:= pon.vp[2]*CalculRatioEspPoissonTronque(ev, pon, hv)/integrale else esptailleponte:= pon.vp[2]*CalculRatioEspPoissonTronque(ev, pon, hv);
                       if esptailleponte<1 then  esptailleponte:=1+minus;  // attention du fait du ratio cette esperance pourrait se retrouver <1 et ce n'est pas possible mathematiquement
                       probevent := (survtotpon(ev.t1-ev.debut, ev.t1-hv.events[1].fin , pon,hv,sex) - survtotpon(ev.t2-ev.debut, ev.t1-hv.events[1].fin ,pon,hv,sex) )  * ppoissontrunc(esptailleponte, ev.tp) + minus;       //(Stot(t1) - Stot(t2))prob(taille ponte/integrale) attention pon.vp[2] n'est pas a proprement parler la moyenne de la loi de poisson tronquée qui vaut en fait vp3/(1-Exp(-vp3)): il faut reecrire la function poissontrunc pour que son parametre corresponde a son esperance directement
                       end;                                                                                         //attention tfrommat calculé depuis ev.t1 car l'effet de snescence est pris en compte au debut de l'intervale

//append(outfile);
//writeln(outfile, ev.name,' ',floattostrf(ev.t1, fffixed, 20,13),' ',floattostrf(ev.t2, fffixed, 20,13),' ', floattostrf(ln(result), fffixed, 20,13));
//closefile(outfile);

end;

{ ************************************************************************ }
function getvar( var su: survfunctype): double;
var aux1,aux2:double;
begin
With su do begin
if name='exp' then getvar:=vp[0]*vp[0];   //var d'une exp est moy^2
if name='wei' then begin                                                                                        //NOTATIONS MATHEMATICA!!
                      aux1:=Gamma(1+1/vp[1]);
                      aux2:=Gamma(1+2/vp[1]);
                      getvar:=vp[0]*vp[0] * (aux2-aux1*aux1)/(aux1*aux1)  ;          //Weibull[vp1,vp0/Gamma(1+1/vp1)]   ca donne une esperance de vp0 et la variance est compliquée mais donnée par getvar
                      end;
if name='lgn' then getvar:=vp[0]*vp[0] * (Expo(vp[1]*vp[1])-1);             //LogNormale[0.5*(2Log[vp0]-vp1²) , vp1 ]  ca donne une esperane de vpO et la variance egale à   vp0²(Exp(vp1²)-1)
if name='gam' then getvar:=vp[0]*vp[1] ;      // Gamma[vp0/vp1,vp1]  ca donne une moyenne qui est vp0 et une variance vp0 vp1
end;
end;

{ ************************************************************************ }
procedure CalcMatsanspon(var legroupe:groupe_info);
var varmat, varpon,varmatsanspon,diffmoy:double;
begin
legroupe.matsanspon.name:='gam';
diffmoy:=legroupe.mat.vp[0]-legroupe.ponte.vp[0] ;
legroupe.matsanspon.vp[0]:=max(0.1,diffmoy)     ; //difference des esperance  mat = matsanspon + pon
legroupe.matsanspon.vp[2]:=legroupe.mat.vp[2];

varmat:=getvar(legroupe.mat);
varpon:=getvar(legroupe.ponte);
if varmat>varpon then varmatsanspon:=varmat-varpon else varmatsanspon:=0.001 ;

legroupe.matsanspon.vp[1]:=(legroupe.matsanspon.vp[0] * legroupe.matsanspon.vp[0])/  varmatsanspon;

end;

{ ************************************************************************ }
function f(var FD: function_D): double;
var
  ans1, ans2, ans3,integrale: double;
  I, j, k,l, m, n: integer;
begin
  ans1 := 0;
  with FD do
  begin { boucle sur les groupes }
    for j := 0 to nb_group-1 do
      if group[j].use = 1 then                  // A FAIRE USE=1 si existe au moins 1 individu ds groupe
      BEGIN
        //group[j].ll:=0;
        for k := 0 to 4 do
            begin
            if (group[j].param[k].po[0]=-1) then
                                          group[j].mort.vp[k] :=0
                                           else begin
                                                group[j].mort.vp[k]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].mort.vp[k] :=group[j].mort.vp[k] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].mort.vp[k]:=link(group[j].mort.vp[k],fd.paramdescript[k]);
                                                end;
            end;
        for k := 5 to 7 do
            begin
            if group[j].param[k].po[0]=-1 then group[j].mat.vp[k-5] :=0
                                           else begin
                                                group[j].mat.vp[k-5]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].mat.vp[k-5] :=group[j].mat.vp[k-5] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].mat.vp[k-5]:=link(group[j].mat.vp[k-5],fd.paramdescript[k]);
                                                end;
            end;
        for k := 8 to 18 do
            begin
            if group[j].param[k].po[0]=-1 then group[j].ponte.vp[k-8] :=0
                                           else begin
                                                group[j].ponte.vp[k-8]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].ponte.vp[k-8] :=group[j].ponte.vp[k-8] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].ponte.vp[k-8]:=link(group[j].ponte.vp[k-8],fd.paramdescript[k]);
                                                end;
            end;


        if fitness_repar=1 then begin
                              if group[j].ponte.vp[0]=0 then integrale:= minus else if matclutch=0 then integrale:= romb(r,group[j])
                                                                                                      else begin
                                                                                                           CalcMatsanspon(group[j]);
                                                                                                           integrale:= romb(r,group[j])
                                                                                                           end    ;
                              if integrale<=minus then integrale:=minus;
                              end;

        for I := 0 to group[j].nb_ind - 1 do                    { boucle sur les individus ds groupe j }
        begin
          ans2 := 0;
          for m := 0 to group[j].gi[I].nb_hv - 1 do              { boucle sur le nb histoire de vie individu i }
          begin
            ans3 := 1;
            for n := 0 to group[j].gi[I].lh[m].nb_event - 1 do        { boucle sur les evenements de l'histoire de vie m }
              ans3 := ans3 * probevent(group[j].gi[I].lh[m].events[n],group[j].mort, group[j].mat, group[j].ponte,integrale,group[j].gi[I].lh[m].events[0].tp,group[j].gi[I].lh[m]);   //group[j].gi[I].lh[m].events[0].tp = sex
                      { produit proba des evenement de hist de vie m }
            ans2 := ans2 + ans3;
          end;
          if ans2 > 0 then ans1 := ans1 + ln(ans2)
                      else ans1 := ans1 + 10*ln(minus);
           // group[j].ll:=ln(ans2);
        end;
      END;
  end; { fin boucle groupe }
  f := ans1;
end;

{ *************************************************************************}
function max(x, y: double): double;
begin
  if x > y then
    max := x
  else
    max := y
end;

{ *************************************************************************}
function min(x, y: double): double;
begin
  if x < y then
    min := x
  else
    min := y
end;

{ *************************************************************************** }
procedure Metropolise(var fun_des: function_D; var Met_des: Metropolis_D);

var
  df, newfresult, sm, ssq, nATT, t, savedvalue: double;
  beta, temp: double; { cooling rate }
  acc: boolean;
  m, I, j: integer;
begin
  { ****************************************** }
  with Met_des do { set up cooling rates: beta, temp }
  begin
    if (temp0 = 0) and (tempf = 0) then
      beta := 0
    else { beware of setting temp0 or temp f to zero }
    if (temp0 > 0) and (tempf > 0) then
      beta := (temp0 - tempf) / (ntr * temp0 * tempf);
    temp := temp0;
    pacc := 0;
  end;
  { ******************************************** }
  with fun_des do { set up function variables }
  begin
    CurrResult := f(fun_des);
    if CurrResult > BestResult then
    begin
      BestResult := CurrResult;
      for j := 1 to number_of_variables do
        with var_info[j] do
          Bestvalue := value;
    end;
  end;
  { ***************************************** }
  { initialise local variables }

  sm := 0;
  ssq := 0;
  nATT := 0;
  t := 0;

  repeat { a cooling process over ntr loops }
    t := t + 1;

    temp := max(0, temp / (1 + beta * temp));
    { le beta est calcule de telle sorte qu'en ntr fois on passe de temp0 a tempf exactement }
    { temp est la variable de temperature entre ces deux etats temp0 et tempf }
    for m := 1 to Met_des.nst do
    begin { nst trials at a given temperature }

      with fun_des do
      begin { perturb each variable }
        for I := 1 to number_of_variables do
        begin
          if abs(var_info[I].step) > 0
          { change the variable by its step, and put this in the new Function D }
          then
            with var_info[I] do
            begin
              nATT := nATT + 1;
              mars;
              savedvalue := value;
              if (uni <> 0) and (uni <> 1) then
              begin
                value := value + step * (uni - 0.5);
                { should we ACCept this ATTempted change ? }
                if (value < minBound) or (value > maxBound) then
                  acc := false
                else
                begin
                  newfresult := f(fun_des);
                  df := (newfresult - CurrResult);
                  if temp > 0 then
                  begin
                    df := df / temp;
                    if df > 0
                    { a tricky procedure to save time and avoid underflow }
                    then
                      acc := true
                    else if df < -10 then
                      acc := false
                       else begin
                       mars;
                       if (uni>0) then acc := (ln(uni) < df) else acc:=false { this is the important line}
                       end
                  end
                  else
                    acc := (df > 0);
                  { when temp=0 or negative, only accept df if it is an improvement }
                end; { of working out whether to accept the change }
              end
              else
                acc := false;
            end;

          if acc { the change was accepted }
          then
          begin
            CurrResult := newfresult;
            with var_info[I] do
              step := step * Met_des.bgeup;
            if newfresult > BestResult then
            begin
              BestResult := newfresult;
              for j := 1 to number_of_variables do
                with var_info[j] do
                  Bestvalue := value;
            end;
            with Met_des do
              pacc := pacc + 1
          end
          else { the change was rejected }
            with var_info[I] do
            begin
              value := savedvalue;
              step := step * Met_des.bgedown;
            end;
        end;

        sm := sm + CurrResult;
        ssq := ssq + sqr(CurrResult);
      end; { of running through "number_of_variables" perturbations }

    end; { of nst trials at a given temperatures }
  until t >= Met_des.ntr;
  { end of Metropolising ( the cooling process over ntr loops) }

  with fun_des do { update our knowledge of the function }
    with Met_des do { given the metropolis run we have just done }
    begin
      MeanResult := sm / (ntr * nst);
      VarResult := (ssq - sqr(MeanResult) * ntr * nst) / (ntr * nst - 1);
    end;

  with Met_des do { update our stats on the Metropolysis of this function }
    if nATT > 0 then
      pacc := pacc / nATT
    else
      pacc := 0;

end; { of Metropolise }

{ *************************************************************************** }
procedure automatic_met(var function_des: function_D; var Metrop_des: Metropolis_D);
var
  specific_Heat, tempt, LastResult: double;                               //A FAIRE PATTERN DE REFROIDISSEMENT AVEC UN STOP DE DECOMPRESSION A  T=1
  I, k, t, batchsize, rep: integer;

begin
  t := 0;
  repeat { recherche globale }
    t := t + 1;
    { searching }
    Metropolise(function_des, Metrop_des);
    { updating }
    with Metrop_des do
      with function_des do
      begin
        if tempf > 0 then
          specific_Heat := function_des.VarResult / sqr(tempf)
        else
          specific_Heat := 0;
        if specific_Heat > 0 then
        begin
          tempt := tempf - tT * climbrate / specific_Heat;
          if tempt / tempf > 0.9 then
            climbrate := climbrate * 3;
          if tempt / tempf < 0.1 then
            climbrate := climbrate / 3;
          tempf := tempf - tT * climbrate / specific_Heat;
        end;
      end;

  until (Metrop_des.tempf <= 0) or (Metrop_des.climbrate <= 0) or (t = Metrop_des.maxrep);

  { stop cooling start at best point found so far }
  with function_des do
    for k := 1 to number_of_variables do
      with var_info[k] do
      begin
        value := Bestvalue;
        step := step / 2;
      end;

function_des.bestresult :=f(function_des)    ;

  with Metrop_des do
    with function_des do
    begin
      { set Metropolis params to temp:=0 }
      temp0 := 0;
      tempf := 0;
      { start local search: }
      rep := 0;
      repeat
        for batchsize := 1 to 10 do
        begin
          rep := rep + 1;
          { local search from the last best point }
          for k := 1 to number_of_variables do
            with var_info[k] do
              value := Bestvalue;
          LastResult := BestResult;
          Metropolise(function_des, Metrop_des);
        end;
      until (BestResult - LastResult < Metrop_des.PRECISION) or (rep > Metrop_des.maxrep);

    end;
end;

{ *************************************************************************** }
procedure Hessian(var fd: function_D);
var auxH1,auxH2: mat;
    auxD2: array of array[1..2] of double;
    max:double;
    i,j:integer;
begin
setlength(H,fd.number_of_variables) ;
setlength(auxD2,fd.number_of_variables) ;
setlength(auxH1,fd.number_of_variables) ;
setlength(auxH2,fd.number_of_variables) ;
for i := 0 to fd.number_of_variables - 1 do setlength(H[i],fd.number_of_variables);   //faire la matrice n x n
for i := 0 to fd.number_of_variables - 1 do setlength(auxH1[i],fd.number_of_variables);
for i := 0 to fd.number_of_variables - 1 do setlength(auxH2[i],fd.number_of_variables);
for i := 1 to fd.number_of_variables do fd.var_info[i].step:=(fd.var_info[i].maxBound - fd.var_info[i].minBound)/1000;

{remplir la diagonale}
for i:= 1 to fd.number_of_variables do fd.var_info[i].value:=fd.var_info[i].Bestvalue ;
max:=f(fd);
for i:= 1 to fd.number_of_variables do          {calcul des f(a+delta,b)}
           begin
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  + fd.var_info[i].step;
           auxD2[i-1,1]:=f(fd);
           fd.var_info[i].value:=fd.var_info[i].Bestvalue;
           end;
for i:= 1 to fd.number_of_variables do          {calcul des f(a-delta,b)}
           begin
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  - fd.var_info[i].step;
           auxD2[i-1,2]:=f(fd);
           fd.var_info[i].value:=fd.var_info[i].Bestvalue;
           end;
for i:= 1 to fd.number_of_variables  do       {calcul des d2f/da2 =  (f(a+d,b)+f(a-d,b)-2f(a,b))/d^2}
           begin
           H[i-1,i-1]:=(auxD2[i-1,1]+auxD2[i-1,2]-2*max)/Power(fd.var_info[i].step,2);
           end;
for i:= 2 to fd.number_of_variables do          {calcul des f(a+deltaa,b-deltab), elements sous diagonal}
for j:= 1 to i-1 do
           begin
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  + fd.var_info[i].step;
           fd.var_info[j].value:=fd.var_info[j].Bestvalue  - fd.var_info[j].step;
           auxH1[i-1,j-1]:=f(fd);
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  ;
           fd.var_info[j].value:=fd.var_info[j].Bestvalue  ;
           end;
for i:= 2 to fd.number_of_variables do          {calcul des f(a-deltaa,b+deltab), elements sous diagonal}
for j:= 1 to i-1 do
           begin
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  - fd.var_info[i].step;
           fd.var_info[j].value:=fd.var_info[j].Bestvalue  + fd.var_info[j].step;
           auxH2[i-1,j-1]:=f(fd);
           fd.var_info[i].value:=fd.var_info[i].Bestvalue  ;
           fd.var_info[j].value:=fd.var_info[j].Bestvalue  ;
           end;
for i:= 1 to fd.number_of_variables-1 do          {calcul des d2f(a,b)/dadb, element sous diagonal}
for j:= 0 to i-1 do H[i,j]:=(auxD2[i,1]+auxD2[i,2]+auxD2[j,1]+auxD2[j,2]-2*max-auxH1[i,j]-auxH2[i,j])/(2*fd.var_info[i+1].step*fd.var_info[j+1].step);
for i:= 1 to fd.number_of_variables-1 do          {matrice est symetrique}
for j:= 0 to i-1 do H[j,i]:=H[i,j];

end;

{*************************************************************************}
procedure FSwap(var X, Y : double);
  var      Temp : double;
begin
    Temp := X;
    X := Y;
    Y := Temp;
end;

{*************************************************************************}
procedure adaptGaussJordan(Ub1, Ub2 : Integer; var flag:integer; var Det : double);
var
  Pvt        : double;       { Pivot }
  Ik, Jk,Lb     : Integer;     { Pivot's row and column }
  I, J, K    : Integer;     { Loop variables }
  T          : double;       { Temporary variable }
  PRow, PCol : vecti; //array[0..dim-1] of integer;  { Stores pivot's row and column }
  MCol       : vect; //array[0..dim-1] of double;     { Stores a column of matrix A }
  startTimeInversion,EndTimeInversion, elapsedTimeInversion: TDateTime;
  Hour, Minute, Sec, MSec: Word    ;

(*  procedure Terminate(ErrCode : Integer);
  { Set error code and deallocate arrays }
  begin
    DelIntVector(PRow, Ub1);
    DelIntVector(PCol, Ub1);
    DelVector(MCol, Ub1);
    SetErrCode(ErrCode);
  end;

*)         //procedure DecodeTime(Date: TDateTime; var Hour, Min, Sec, MSec: Word);;


begin
  startTimeInversion:=Time;
  SetLength(PRow,Ub1+1);SetLength(PCol,Ub1+1);SetLength(MCol,Ub1+1);
  Lb:=0;        //lower bound of H in both directions
  flag:=1;
  Det := 1.0;

  K := Lb;
  while (K <= Ub1) do
    begin
      { Search for largest pivot in submatrix A[K..Ub1, K..Ub1] }
      //Application.ProcessMessages;
      Pvt := H[K,K];
      Ik := K;
      Jk := K;
      for I := K to Ub1 do
        for J := K to Ub1 do
          if Abs(H[I,J]) > Abs(Pvt) then
            begin
              Pvt := H[I,J];
              Ik := I;
              Jk := J;
            end;

      { Store pivot's position }
      PRow[K] := Ik;
      PCol[K] := Jk;

      { Update determinant }
      Det := Det * Pvt;
      if Ik <> K then Det := - Det;
      if Jk <> K then Det := - Det;
      {timing explose}
      EndTimeInversion :=Time;
      elapsedTimeInversion:=EndTimeInversion -  startTimeInversion;
      DecodeTime(elapsedTimeInversion,Hour, Minute, Sec, MSec);
      if Minute>=3 then
          begin
          flag:=0;
          Exit
          end;

      { Too weak pivot ==> quasi-singular matrix }
      if Abs(Pvt) < 0.00000000001 then
        begin
          flag:=0;
          Exit
        end;

      { Exchange current row (K) with pivot row (Ik) }
      if Ik <> K then
        for J := Lb to Ub2 do
          FSwap(H[Ik,J], H[K,J]);

      { Exchange current column (K) with pivot column (Jk) }
      if Jk <> K then
        for I := Lb to Ub1 do
          FSwap(H[I,Jk], H[I,K]);

      { Store column K of matrix A into MCol
        and set this column to zero }
      for I := Lb to Ub1 do
        if I <> K then
          begin
            MCol[I] := H[I,K];
            H[I,K] := 0.0;
          end
        else
          begin
            MCol[I] := 0.0;
            H[I,K] := 1.0;
          end;

      { Transform pivot row }
      T := 1.0 / Pvt;
      for J := Lb to Ub2 do
        H[K,J] := T * H[K,J];

      { Transform other rows }
      for I := Lb to Ub1 do
        if I <> K then
          begin
            T := MCol[I];
            for J := Lb to Ub2 do
              H[I,J] := H[I,J] - T * H[K,J];
          end;

      Inc(K);

    end;

  { Exchange lines of inverse matrix }
  for I := Ub1 downto Lb do
    begin
      Ik := PCol[I];
      if Ik <> I then
        for J := Lb to Ub2 do
          FSwap(H[I,J], H[Ik,J]);
    end;

  { Exchange columns of inverse matrix }
  for J := Ub1 downto Lb do
    begin
      Jk := PRow[J];
      if Jk <> J then
        for I := Lb to Ub1 do
          FSwap(H[I,J],H[I,Jk]);
    end;


 // Terminate(MatOk);
end;

{ *************************************************************************** }
procedure calcSE(var fd: function_D);
var
detA:double;
i,flag:integer;

begin
  TRY
  Hessian(fd);
  adaptGaussJordan(fd.number_of_variables-1,fd.number_of_variables-1,flag, detA);
  if flag=1 then
            begin
            for i := 1 to fd.number_of_variables do if H[i-1,i-1]<0 then fd.var_info[i].SE:=Sqrt(-H[i-1,i-1]) else fd.var_info[i].SE:=-1;
            end;
  if flag=0 then
            begin
            for i := 1 to fd.number_of_variables do fd.var_info[i].SE:=-1;
            end;
  EXCEPT
  for i := 1 to fd.number_of_variables do fd.var_info[i].SE:=-1;
  END;


end;

{ ************************************************************************** }
procedure Init_f_D(var FD: function_D);
var
   j, k: integer;
begin
  with FD do
  begin
    number_of_variables := max_vars;
    BestResult := -1E10;
  end;

with fd.var_info[0] do
      begin
      name:='dum'    ;
      minbound:=0               ;
      maxbound:=0 ;
      value:=0;
      step:= 0;
      end;
for j:=1 to fd.number_of_variables do
with fd.var_info[j] do
      begin
       minbound:= -20;
       maxbound:= 20;
       step:= 1;
      end;

xRatiomax := Ln(ratiomax-1); //utile pour fonction CalculRatioEspPoissonTronque, pour ne pas avoir a le calculer tout le temps

end;

{ ************************************************************************** }
procedure decrireparam(var FD: function_D);
begin
setlength(suaux.vp,11);
with FD.paramdescript[0] do
      begin
        name := 'expt_death'; // 'E(tmort)f'
        minBound := 1;
        maxBound := 200;
     //   value := 15;
      end;

      with FD.paramdescript[1] do                 { faire distingo entre weibull et ln }
      begin
        name := 'survival_shape'; // 'morta'
        minBound := 0.001;
        maxBound := 30;
     //   value := 1;
      end;


      with FD.paramdescript[2] do
      begin
        name := 'ratio_expt_death'; // 'RE(tmort)m'
        minBound := 0.1;
        maxBound := 4;
      //  value := 1;
      end;

      with FD.paramdescript[3] do
      begin
        name := 'prob_death'; // 'mortp'
        minBound := 0.0001;
        maxBound := 1;
      //  value := (minBound + maxBound) / 2;
      end;

      with FD.paramdescript[4] do
      begin
        name := 'sex_ratio'; // 'propmal'
        minBound := 0.00001;
        maxBound := 0.99999;
     //   value := 0.5;
      end;

      with FD.paramdescript[5] do
      begin
        name := 'expt_maturity'; // 'E(tmat)f'
        minBound := 1;
        maxBound := 100;
      //  value := 30;
      end;

      with FD.paramdescript[6] do
      begin
        name := 'maturity_shape'; // 'mata'
        minBound := 0.0001;
        maxBound := 12;
     //   value := 1;
      end;

      with FD.paramdescript[7] do
      begin
        name := 'ratio_expt_maturity'; // 'RE(tmat)m'
        minBound := 0.1;
        maxBound := 10;
      //  value := 1;
      end;

      with FD.paramdescript[8] do
      begin
        name := 'expt_reproduction'; // 'E(tpon)'
        minBound := 0.1;
        maxBound := 200;
     //   value := 40;
      end;

      with FD.paramdescript[9] do
      begin
        name := 'reproduction_shape'; // 'ponta'
        minBound := 0.001;
        maxBound := 12;
    //    value := 1;
      end;

if fitness_repar=1
  then  with FD.paramdescript[10] do
        begin
        name := 'fitness'; // 'W'
        minBound := 0.0001;
        maxBound := 1000;
     //   value := 20;
        end
  else  with FD.paramdescript[10] do
        begin
        name := 'pontn';
        minBound := 1;
        maxBound := 50;    //attention a cette limite
     //   value := 20;
        end;

  with FD.paramdescript[11] do
      begin
        name := 'increase_death_hazard'; // 'to(ps)int'            //tradeoff ponte -> surv intercept (evenement ponte augmente le hazard de survie de cette quantité)
        minBound := 0.00001;
        maxBound := 10;
     //   value := 0.5;
      end;

  with FD.paramdescript[12] do         //tradeoff ponte -> surv amortissement du precedent avec le temps
      begin
        name := 'tof_reduction_date'; // 'to(ps)am'
        minBound := 0.0000001;
        maxBound := 10;
     //   value := 0.5;
      end;

  with FD.paramdescript[13] do          //tradeoff ponte -> surv , augmentation du tradeoff en fonction linéaire de la taille ponte en question
      begin
        name := 'increase_tof_n_offspring'; // 'to(ps)tp'
        minBound := 0.0000001;
        maxBound := 10;
     //   value := 0.5;
      end;

  with FD.paramdescript[14] do
      begin
        name := 'lin_decrease_hazard';  // 'sen(pu)t'           //senescence de hazard ponte, effet lineaire du temps post-maturité
        minBound := -20;
        maxBound := 20;
     //   value := 0.5;
      end;

  with FD.paramdescript[15] do          //senescence de hazard ponte, effet quadratique du temps post-maturité
      begin
        name := 'quad_senescence'; // 'sen(pu)t2'
        minBound := -20;
        maxBound := 20;
     //   value := 0.5;
      end;

  with FD.paramdescript[16] do        //senescence de taille ponte, effet linéaire du temps post-maturité
      begin
        name := 'quad_decrease_hazard'; // 'sen(pn)t'
        minBound := -10;
        maxBound := 10;
     //   value := 0.5;
      end;

  with FD.paramdescript[17] do       //senescence de taille ponte, effet quadratique du temps post-maturité
      begin
        name := 'quad_change_n_offspring'; // 'sen(pn)t2'
        minBound := -10;
        maxBound := 10;
     //   value := 0.5;
      end;

 with FD.paramdescript[18] do         //tradeoff intervale ponte i-1 qui change taille ponte i
      begin
        name := 'tof_n_offspring'; // 'to(pupn)'
        minBound := -10;
        maxBound := 10;
     //   value := 0.5;
      end;

end;

{----------------------------------------------------------------------------------------------------}
procedure interpretmodel(var fitrep:byte);
var i,j,k,l,count:integer;
   intinit:array[0..nbparposs-1] of double;
begin

intinit[0]:=  -0.37     ;
intinit[1]:= -2.23    ;
intinit[2]:=  0     ;
intinit[3]:= 0       ;
intinit[4]:=   0     ;
intinit[5]:=    -1.96    ;
intinit[6]:= -3.45     ;
intinit[7]:=  0       ;
intinit[8]:=      -3.036    ;
intinit[9]:= -3.35     ;
if fitrep=1 then intinit[10]:= -4.0  else intinit[10]:=-2.6      ;
intinit[11]:= 0     ;
intinit[12]:= 0     ;
intinit[13]:= 0     ;
intinit[14]:= 0     ;
intinit[15]:= 0    ;
intinit[16]:= 0     ;
intinit[17]:= 0     ;
intinit[18]:= 0     ;

// covar[3].valcont[0]:=0;
// covar[3].valcont[1]:=2.5;

// covar[4].valcont[0]:=0;
// covar[4].valcont[1]:=1;
{covar[4].valcont[0]:=0;
covar[4].valcont[3]:=9;
covar[4].valcont[4]:=12;
covar[4].valcont[5]:=23;

covar[6].valcont[0]:=15;
covar[6].valcont[1]:=20;
covar[6].valcont[2]:=22.5;
covar[6].valcont[3]:=25;
covar[6].valcont[4]:=27.5;
covar[6].valcont[5]:=30;
covar[6].valcont[6]:=32.5;
covar[6].valcont[7]:=35; }

for i := 1 to nbcov do
   for j := 0 to covar[i].lev-1
     do covar[nbcov+i].valcont[j]:=j;         //remplissage des valcont

//decrireparam(ll_d);
count:=0     ;
for i := 0 to nbparposs-1 do
   BEgin
   for j := 0 to modele[i].nbterms - 1 do
       begin
       if modele[i].term[j]=-1 then                    //determine the termcov
                               begin
                               modele[i].termcov0[j]:=-1;
                               modele[i].termcov1[j]:=-1 ;
                               end  ;
       if modele[i].term[j]=0 then                    //determine the termcov
                               begin
                               modele[i].termcov0[j]:=0;
                               modele[i].termcov1[j]:=-1 ;
                               modele[i].firstvi[j]:=count+1 ;
                               count:=count+1;
                               Setlength(ll_d.var_info,count+1);
                               LL_D.var_info[count].name:='int_'+LL_D.paramdescript[i].name;
                               LL_D.var_info[count].value:= intinit[i];
                               end  ;
       if (modele[i].term[j]>0) and (modele[i].term[j]<10) and (covar[modele[i].term[j]].typ=0) then                    //determine the termcov
                               begin
                               modele[i].termcov0[j]:=modele[i].term[j];
                               modele[i].termcov1[j]:=-1 ;
                               modele[i].firstvi[j]:=count+1 ;
                               Setlength(ll_d.var_info,count+covar[modele[i].termcov0[j]].lev-1+1);
                               for k:=1 to (covar[modele[i].termcov0[j]].lev-1) do
                                         begin
                                         LL_D.var_info[count+k].name:='eff_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov0[j]].name+inttostr(k);
                                         LL_D.var_info[count+k].value:= 0;
                                         end;
                               count:=count+covar[modele[i].termcov0[j]].lev-1;
                               end  ;
       //continue
       if (modele[i].term[j]>0) and (modele[i].term[j]<10) and (covar[modele[i].term[j]].typ=1) then                    //determine the termcov
                               begin
                               modele[i].termcov0[j]:=modele[i].term[j];
                               modele[i].termcov1[j]:=-1 ;
                               modele[i].firstvi[j]:=count+1 ;
                               count:=count+1;
                               Setlength(ll_d.var_info,count+1);
                               LL_D.var_info[count].name:='slo_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov0[j]].name;
                               LL_D.var_info[count].value:= 0;
                               end  ;



       if (modele[i].term[j]>10) then                    //determine the termcov
            BEGIN
            modele[i].termcov0[j]:=modele[i].term[j] mod 10;                                    //ligne changée termcov0 initialement
            modele[i].termcov1[j]:=modele[i].term[j] div 10 ;                                   //ligne changée termcov1 initialement
            modele[i].firstvi[j]:=count+1 ;
            if (covar[modele[i].termcov0[j]].typ+covar[modele[i].termcov1[j]].typ=0) then
                               //facteur x facteur
                               begin
                               Setlength(ll_d.var_info,count+(covar[modele[i].termcov0[j]].lev-1)*(covar[modele[i].termcov1[j]].lev-1)+1);
                               for k:=1 to (covar[modele[i].termcov0[j]].lev-1) do for l:=1 to (covar[modele[i].termcov1[j]].lev-1) do
                                        begin
                                        LL_D.var_info[count+k+(l-1)*(covar[modele[i].termcov0[j]].lev-1)].name:='eff_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov0[j]].name+inttostr(k)+':'+covar[modele[i].termcov1[j]].name+inttostr(l);
                                        LL_D.var_info[count+k+(l-1)*(covar[modele[i].termcov0[j]].lev-1)].value:=0;
                                        end;
                               count:=count+(covar[modele[i].termcov0[j]].lev-1)*(covar[modele[i].termcov1[j]].lev-1);
                               end  ;
            if (covar[modele[i].termcov0[j]].typ=1) and (covar[modele[i].termcov1[j]].typ=0) then
                               //continu x facteur
                               begin
                               Setlength(ll_d.var_info,count+(covar[modele[i].termcov1[j]].lev-1)+1);
                               for k:=1 to (covar[modele[i].termcov1[j]].lev-1) do
                                        begin
                                        LL_D.var_info[count+k].name:='slo_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov1[j]].name+inttostr(k)+':'+covar[modele[i].termcov0[j]].name+'_cont';
                                        LL_D.var_info[count+k].value:=0;
                                        end;
                               count:=count+covar[modele[i].termcov1[j]].lev-1;
                               end  ;
            if (covar[modele[i].termcov1[j]].typ=1) and (covar[modele[i].termcov0[j]].typ=0) then
                               //facteur x continu
                               begin
                               Setlength(ll_d.var_info,count+(covar[modele[i].termcov0[j]].lev-1)+1);
                               for k:=1 to (covar[modele[i].termcov0[j]].lev-1) do
                                        begin
                                        LL_D.var_info[count+k].name:='slo_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov0[j]].name+inttostr(k)+':'+covar[modele[i].termcov1[j]].name+'_cont';
                                        LL_D.var_info[count+k].value:=0;
                                        end;
                               count:=count+covar[modele[i].termcov0[j]].lev-1;
                               end  ;
            if (covar[modele[i].termcov1[j]].typ=1) and (covar[modele[i].termcov0[j]].typ=1) then
                               //continu x continu
                               begin
                               count:=count+1;
                               Setlength(ll_d.var_info,count+1);
                               LL_D.var_info[count+k].name:='slo_'+LL_D.paramdescript[i].name+'_'+covar[modele[i].termcov0[j]].name+'_cont'+':'+covar[modele[i].termcov1[j]].name+'_cont';
                               LL_D.var_info[count+1].value:=0;
                               end  ;
           END;
       end;
   ENd;
max_vars:=  count;
end;

{----------------------------------------------------------------------------------------------------}
procedure interpretmodelpargroupe;
var i,j,k:integer;
begin
for i := 0 to nb_group - 1 do with group[i] do
 for j := 0 to nbparposs - 1 do
   for k := 0 to group[i].param[j].nbterms - 1 do
        begin
        group[i].param[j].valpo[k] :=1;                  //valpo valent 1 si covar discrete, sinon, depend de valcont
        if modele[j].term[k]=-1 then                    //determine the termcov
                               group[i].param[j].po[k]:= -1 ;
        if modele[j].term[k]=0 then                    //determine the termcov
                               group[i].param[j].po[k]:= modele[j].firstvi[k] ;

        if (modele[j].term[k]>0) and (modele[j].term[k]<10) then                    //determine the termcov
                                  if covar[modele[j].termcov0[k]].typ=0
                                  then begin
                                       if getcov(i,modele[j].termcov0[k])=0    //le premier niveau indicé 0 est fitté deja ds l'intercept, il ne faut pas l'ajouter
                                       then group[i].param[j].po[k]:=-1
                                       else  group[i].param[j].po[k]:= modele[j].firstvi[k]  + getcov(i,modele[j].termcov0[k])-1  ;
                                       end
                                  else begin
                                       group[i].param[j].po[k]:= modele[j].firstvi[k];
                                       group[i].param[j].valpo[k]:=covar[modele[j].termcov0[k]].valcont[getcov(i,modele[j].termcov0[k])];
                                       end;
        if (modele[j].term[k]>10) then                    //determine the termcov
                                  begin
                                    //facteur x facteur
                                    if (covar[modele[j].termcov0[k]].typ+covar[modele[j].termcov1[k]].typ=0)
                                    then if getcov(i,modele[j].termcov0[k])*getcov(i,modele[j].termcov1[k])=0
                                         then group[i].param[j].po[k]:=-1
                                         else group[i].param[j].po[k]:=modele[j].firstvi[k]+getcov(i,modele[j].termcov0[k])-1+(getcov(i,modele[j].termcov1[k])-1)*(covar[modele[j].termcov0[k]].lev-1 );  // firstvi+x-1+(y-1)(nx-1)
                                         //else group[i].param[j].po[k]:=modele[j].firstvi[k]+getcov(i,modele[j].termcov1[k])-1+(getcov(i,modele[j].termcov0[k])-1)*(covar[modele[j].termcov1[k]].lev-1 );  // firstvi+x-1+(y-1)(nx-1)
                                    //continu x facteur
                                    if (covar[modele[j].termcov0[k]].typ=1) and (covar[modele[j].termcov1[k]].typ=0)
                                    then if getcov(i,modele[j].termcov1[k])=0
                                         then group[i].param[j].po[k]:=-1
                                         else begin
                                              group[i].param[j].po[k]:=modele[j].firstvi[k]+getcov(i,modele[j].termcov1[k]-1);  //le firstvi est le count+1, il faut donc firstvi+getcov-1   ; le getcov renvoie les valeurs 0 à level-1. Le cas zero est exclu d'entrée, les po commence a firstvi + getcov-1
                                              group[i].param[j].valpo[k]:=covar[modele[j].termcov0[k]].valcont[getcov(i,modele[j].termcov0[k])];
                                              end;
                                    //facteur x continu
                                    if (covar[modele[j].termcov1[k]].typ=1) and (covar[modele[j].termcov0[k]].typ=0)
                                    then if getcov(i,modele[j].termcov0[k])=0
                                         then group[i].param[j].po[k]:=-1
                                         else begin
                                              group[i].param[j].po[k]:=modele[j].firstvi[k]+getcov(i,modele[j].termcov0[k]-1);  //le firstvi est le count+1, il faut donc firstvi+getcov-1   ; le getcov renvoie les valeurs 0 à level-1. Le cas zero est exclu d'entrée, les po commence a firstvi + getcov-1
                                              group[i].param[j].valpo[k]:=covar[modele[j].termcov1[k]].valcont[getcov(i,modele[j].termcov1[k])];
                                              end;
                                    //continu x continu
                                    if (covar[modele[j].termcov1[k]].typ=1) and (covar[modele[j].termcov0[k]].typ=1)
                                    then begin
                                         group[i].param[j].po[k]:=modele[j].firstvi[k];
                                         group[i].param[j].valpo[k]:=covar[modele[j].termcov0[k]].valcont[getcov(i,modele[j].termcov0[k])]*covar[modele[j].termcov1[k]].valcont[getcov(i,modele[j].termcov1[k])];
                                         end;
                                  end;
        end;
end;

{ ************************************************************************** }
procedure interpretation(var fitrep:byte);
var
  I, j: integer;

begin
  LL_D.number_of_variables := max_vars;
  LL_D.BestResult := -1E10;
  interpretmodel(fitrep);
  interpretmodelpargroupe;
end;

{ ************************************************************************ }
procedure printout_FD(var FD: function_D;nomf:string;nbrun:integer;exportinvhessian:String);
var
  i,j: integer;
begin
  append(outfile);
  writeln(outfile, '---------------------------');
  writeln(outfile);
  writeln(outfile, 'datafile= ', nomf);
  writeln(outfile, 'seed1= ',inttostr(savedseed[1]),' seed2= ',inttostr(savedseed[2]),' seed3= ',inttostr(savedseed[3]),' seed4= ',inttostr(savedseed[4]));
  writeln(outfile, '#parameters= ', inttostr(fd.number_of_variables));
  writeln(outfile, 'Likelihood_max= ', fd.BestResult:10:6);
  with FD do { Write out the results }
    for i := 1 to number_of_variables do
        begin
        write(outfile, var_info[i].name,' ',var_info[i].value:10:8,' ',var_info[i].se:10:8);
        writeln(outfile);
        end;
  if exportinvhessian='TRUE' then
     with FD do
     begin
     writeln(outfile);
     writeln(outfile,'inverse of Hessian Matrix');
      For i :=1 to FD.number_of_variables do
      begin
      for j := 1 to FD.number_of_variables do write(outfile, H[i-1,j-1]:10:8,' ');
      writeln(outfile);
      end;
     end;

  if nbrun>0 then
  BEGIN
  writeln(outfile);
  writeln(outfile,'MCMCsamples');
  with FD do { Write out the samples }
    begin
    write(outfile, 'LL',' ') ;
    for j:=0 to nbsample-1 do write(outfile,fd.sample[j]:10:8,' ');
    writeln(outfile);
    for i := 1 to number_of_variables do
        begin
        write(outfile, var_info[i].name,' ') ;
        for j:=0 to nbsample-1 do write(outfile,var_info[i].sample[j]:10:8,' ');
        writeln(outfile);
        end;
    end;
  END;
  closefile(outfile);
end;

{ *************************************************************************** }
procedure promenade (var Fun_Des: function_D; var Met_Des: Metropolis_D);
var  t, k: integer;
    specific_Heat,tempt, LastResult: double;
     acc: boolean;
    m, i, j: integer;
    df, newfresult, sm, ssq, nATT, savedvalue: double;
    beta, temp: double;{cooling rate}

begin

{start walking at best point found so far }
with fun_des do
   for k := 1 to number_of_variables do
   with var_info[k] do
   begin
   value := bestvalue;
   step:=(maxbound-minbound)/1000          ;
   setlength(sample,nbsample);
   end;
{-----------------------metrop--at T=1-------------}

with Met_Des do { set up cooling rates: beta, temp }
begin
beta :=0;
temp := 1;
pacc := 0;

ntr:=nbsample;
nst:=intervalsamples;
end;

with Fun_Des do {set up function variables}
begin
CurrResult := f(fun_des);
bestresult:= currResult;
SetLength(sample,nbsample);   //donne la longueur du tableau de sampling
end;
{*****************************************}
{ initialise local variables }

sm := 0;
ssq := 0;
nATT := 0;
t := 0;

repeat { a cooling process over ntr loops }
t:=t+1;
//temp:=1;

for m := 1 to Met_Des.nst do
begin { nst trials at a given temperature }

with fun_des do
begin  { perturb each variable }
 for i := 1 to number_of_variables do
 begin
    //if abs(var_info[i].step) > 0 { change the variable by its step, and put this in the new Function D }
    //then
    with var_info[i] do
      begin
      nATT := nATT + 1;
      mars;
      savedvalue := value;
      if (uni<>0) and (uni<>1)
      then begin
      value := value + step * (uni - 0.5);
      { should we ACCept this ATTempted change ? }
         if (value < minBound) or (value > maxBound)
         then ACC := false
         else
           begin
           newFresult:= f(fun_des);
           df:= (newFresult - CurrResult);
           if temp > 0
           then
             begin
             df:= df / temp;
             if df > 0  {a tricky procedure to save time and avoid underflow}
             then acc:= true
             else if df < -10
                  then acc := false
                  else begin
                       mars;
                       if (uni>0) then acc := (ln(uni) < df) else acc:=false { this is the important line}
                       end
             end
           else acc := (df > 0); { when temp=0 or negative, only accept df if it is an improvement }
           end; {of working out whether to accept the change}
           end else acc:=false;
      end;

    if acc {the change was accepted}
    then
      begin
      CurrResult := newFresult;
      with var_info[i] do
      step := step * Met_des.bgeup;
      (*if newFresult > bestresult
      then
         begin
         bestresult := newFresult;
         for j := 1 to number_of_variables do
         with var_info[j] do
         bestvalue := value;
         end; *)
      with Met_Des do
      pacc := pacc + 1
      end
    else {the change was rejected}
      with var_info[i] do
         begin
         value := savedvalue;
         step := step * Met_Des.bgedown;
         end;
      end;

//sm := sm + CurrResult;
//ssq := ssq + sqr(CurrResult);
end; {of running through "number_of_variables" perturbations}

end; {of nst trials at a given temperatures}
  fun_des.sample[t-1]:=fun_des.CurrResult;
  for j := 1 to fun_des.number_of_variables do
  with fun_des.var_info[j] do sample[t-1]:= value;      //sampling at each of the ntr loops
  
until t >= Met_Des.ntr; {end of Metropolising ( the cooling process over ntr loops)}


with Met_Des do { update our stats on the Metropolysis of this function }
if nATT > 0 then
pacc := pacc / nATT
else
pacc := 0;


end; { of promenade }

(*******************************************************************************************)
procedure GetAndWriteprobevents(var FD: function_D);
var i,j,k,l,m,n:integer;
    integrale,valeur:double;
Begin
with FD do
   for k := 1 to number_of_variables do
   with var_info[k] do
   begin
   value := bestvalue;
   step:=(maxbound-minbound)/1000          ;
   setlength(sample,nbsample);
   end;

append(outfile);
writeln(outfile);
write(outfile, 'LL_per_events');
writeln(outfile);


  with FD do
  begin { boucle sur les groupes }
    for j := 0 to nb_group-1 do
      if group[j].use = 1 then                  // A FAIRE USE=1 si existe au moins 1 individu ds groupe
      BEGIN
        for k := 0 to 4 do
            begin
            if (group[j].param[k].po[0]=-1) then
                                          group[j].mort.vp[k] :=0
                                           else begin
                                                group[j].mort.vp[k]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].mort.vp[k] :=group[j].mort.vp[k] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].mort.vp[k]:=link(group[j].mort.vp[k],fd.paramdescript[k]);
                                                end;
            end;
        for k := 5 to 7 do
            begin
            if group[j].param[k].po[0]=-1 then group[j].mat.vp[k-5] :=0
                                           else begin
                                                group[j].mat.vp[k-5]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].mat.vp[k-5] :=group[j].mat.vp[k-5] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].mat.vp[k-5]:=link(group[j].mat.vp[k-5],fd.paramdescript[k]);
                                                end;
            end;
        for k := 8 to 18 do
            begin
            if group[j].param[k].po[0]=-1 then group[j].ponte.vp[k-8] :=0
                                           else begin
                                                group[j].ponte.vp[k-8]:=0;
                                                for l:=0 to group[j].param[k].nbterms-1 do
                                                           if group[j].param[k].po[l]>-1 then group[j].ponte.vp[k-8] :=group[j].ponte.vp[k-8] + var_info[group[j].param[k].po[l]].value*group[j].param[k].valpo[l];
                                                group[j].ponte.vp[k-8]:=link(group[j].ponte.vp[k-8],fd.paramdescript[k]);
                                                end;
            end;
        if fitness_repar=1 then begin
                              if group[j].ponte.vp[0]=0 then integrale:= minus else if matclutch=0 then integrale:= romb(r,group[j])
                                                                                                      else begin
                                                                                                           CalcMatsanspon(group[j]);
                                                                                                           integrale:= romb(r,group[j])
                                                                                                           end    ;
                              if integrale<=minus then integrale:=minus;
                              end;

        for I := 0 to group[j].nb_ind - 1 do                    { boucle sur les individus ds groupe j }
          for m := 0 to group[j].gi[I].nb_hv - 1 do              { boucle sur le nb histoire de vie individu i }
            for n := 0 to group[j].gi[I].lh[m].nb_event - 1 do        { boucle sur les evenements de l'histoire de vie m }
               begin
               valeur:=probevent(group[j].gi[I].lh[m].events[n],group[j].mort, group[j].mat, group[j].ponte,integrale,group[j].gi[I].lh[m].events[0].tp,group[j].gi[I].lh[m]);   //group[j].gi[I].lh[m].events[0].tp = sex
               write(outfile,group[j].gi[I].lh[m].events[n].name,' ',ln(valeur):10:8,' ');
               end;

      END;
  end; { fin boucle groupe }

closefile(outfile);

End;    //fin getandwrite

(*******************************************************************************************)
procedure writeparamdescript(var FD: function_D);
var i,j,k,l,m,n:integer;
    integrale,valeur:double;
Begin
append(outfile);
writeln(outfile);
writeln(outfile);
writeln(outfile, 'Parameter_Range_Table');
with FD do
   for k := 0 to nbparposs-1 do
   with FD.paramdescript[k] do  writeln(outfile,name,' ',minbound:5:8,' ',maxbound:5:8);
writeln(outfile,'ratiomax ',ratiomax);
writeln(outfile,'tinf_(right_censoring) ',tinf);
writeln(outfile,'tc_(juvenile_period_length) ',tc);
closefile(outfile);

End;    //fin writeparamdescript
end. { of program }
