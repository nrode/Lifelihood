## Compilation process

Documentation of the process of compiling the lifelihood project on both macOS and Windows.

<br>

### 2024 April 8

- 👎 Tried to use the [wine](https://www.winehq.org/) software for running `.exe` files directly on macOS, but does not seem to be solution to the problem. We rather need a compiler from source code directly.

- ✅ Followed tutorial from [this page](https://wiki.lazarus.freepascal.org/Installing_Lazarus_on_macOS) that explains how to install Lazarus on macOS (using [this page](https://sourceforge.net/projects/lazarus/)). Lazarus is a free cross-platform IDE which provides a Delphi-like development experience for Pascal. Main steps:
```
- Download and install Xcode (optional depending on your needs - see below for details).

- Install the global command line tools for Xcode. Definitely required.

- Download and install the Free Pascal Compiler (FPC) binaries package and the FPC source package from the Lazarus IDE file area (Important do this before you install the Lazarus IDE)

- Download and install the Lazarus IDE from the Lazarus IDE file area or, perhaps more useful, download and compile Lazarus source code, eg Lazarus Fixes 3.0

- Configure LLDB - the Apple supplied (and signed) debugger from within the Lazarus IDE.
```

- TODO next session: try to compile some code with Lazarus and see if it works.

<br>

### 2024 April 15

- ✅ Successfully compiled a simple print hello world program using Lazarus. It was compiled with `fpc sanbox.pas` and executed with `./sandbox`. The output was `Hello, World!`. The code is as follows:
```pascal
program HelloWorld;
uses crt;

begin
   writeln('Hello, World!');
   readkey;
end. 
```

- ✅ Created a simple project in Lazarus and compiled a simple program that prints "Hello, World!" to the console. The code is as follows:
```pascal
program HelloWorld;
uses crt;

begin
   writeln('Hello, World!');
   readkey;
end. 
```
Once compiled, I ran the program with `./project1` and it worked as expected.

- ✅ Create a project in Lazarus and tried to compile the whole `lifelihood` project. The project is not compiling because of the following error:
```
Unit1.pas(189,46) Error: Illegal assignment to for loop variable "j"
```
The error is in the following code:
```pascal
// Gerer les juveniles non sexés
for i:= 0 to nb_group - 1 do  for j := 0 to group[i].nb_ind - 1 do
 if group[i].gi[j].lh[0].events[0].tp=2 then roughgarden(i,j)  ;   
```
The `roughgarden` function is defined as follows:
```pascal
procedure roughgarden(var a,b:integer)      ;
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
```
Not sure why this error is happening since the `j` variable is not being assigned to in the `roughgarden` function.

So I change `procedure roughgarden(var a,b:integer)` to `procedure roughgarden(a,b:integer)` in order to ensure that a and b are not passed by reference. This seems to solve the problem.

- ✅ Tried again to compile the project after the change and it seems to be working. The project is compiling without any errors.

- Tried to run the project with input data and see if it works.


<br>

### 2024 April 23

- Project seems to not be compiled correctly because executable files don't do much, so I re start from a clean base. 
   - put 7 source files in a directory
   - create a new project (Application) in Lazarus
   - add the 7 source files to the project
   - convert the Delphi project to a Lazarus project (see [this page](https://forum.lazarus.freepascal.org/index.php?topic=36933.0)). It seems that the project is now a Lazarus project (`.dpr` file disappeared and `.lpi` and `.lpr` file appeared (as well as others)).

- Try to compile the project. Multiple errors are raised:
   - `Error: .section __DATA, __datacoal_nt, coalesced`, but apparently those are just warnings that Lazarus flags as errors (see [this page](https://forum.lazarus.freepascal.org/index.php?topic=65422.0) and [this page](https://wiki.lazarus.freepascal.org/Mac_Installation_FAQ)).
   - `Error: linker: Undefined symbols for architecture x86_64:`
   - `Error: linker: "WSRegisterBevel", referenced from:`
   - `Error: ld: symbol(s) not found for architecture x86_64`
   - and finally: `Error: Error while linking`

- So I post a question on the Lazarus forum to see if someone can help me with this issue at [this adress](https://forum.lazarus.freepascal.org/index.php?topic=67059.msg515282#msg515282)
   - someone redirected me to [this page](https://forum.lazarus.freepascal.org/index.php/topic,64812.0.html) that is a discussion about the exact same issue (at least in terms of error messages).
   - the problem seems to be that Xcode command line tools above 15 on Sonoma breaks the compilation process.
   - one easy solution seemed to just downgrade Xcode to a version before 15 since the bug seems to be related to older version.

- I removed Xcode from my mac, went to Apple developer website to download the latest Xcode version before 15 (14.3.1). I installed it but unfortunately MacOS Sonoma (my version) requires a more recent Xcode version. So I reinstalled the last Xcode version (15.3) and I will try to find another solution.

- Since the problem seems related to the LLBD debugger, I tried to use GDB, but `brew install gdb` tells me that GDB does not work on M1 macs (which is also my case).


<br>

### 2024 April 25

- Apparently the problem is not related to [the post](https://forum.lazarus.freepascal.org/index.php/topic,64812.30.html) I found.

- The post I posted on the Lazarus forum led to a [discussion](https://forum.lazarus.freepascal.org/index.php/topic,67059.0.html) with one of the administrators of the forum.

- After today's meeting with Nicolas and Thomas, we decided to change a bit the strategy by:
   - Compile for PC with Embarcadaro
   - Convert to Lazarus project and compile for PC (NB: try to execute step by step with "StepInto", "StepOver","BreakPoints")
   - Compile the Lazarus project with Mac (using both Lazarus and Embarcadaro)


<br>

### 2024 April 30

- ✅ **Compiling and Execute on Windows with Embarcadero**. Using Embarcadero, I created a console project, added the source files to it and compile it again. The project compiled without any errors. Then I ran the executable file with some data and it worked as expected.

- ✅ **Compiling and Execute on Windows with Lazarus**. I first converted the Delphi project into a Lazarus project using the built-in tool in Lazarus. However, when I try to compile it leads to the same error I was facing in [April 15](#april-15): 
   ```
   Unit1.pas(189,46) Error: Illegal assignment to for loop variable "j"
   ```
   - I tried to change the `roughgarden` function to `procedure roughgarden(a,b:integer)` instead of `procedure roughgarden(var a,b:integer)` and it seems to solve the problem (parameters passed by value instead of by reference).
   - Then I got new errors:
   ```
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomImageListResolution
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterMenuItem
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterMenu
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterMainMenu
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterPopupMenu
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterDragImageListResolution
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterLazAccessibleObject
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterControl
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterWinControl
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterGraphicControl
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomControl
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterScrollingWinControl
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterScrollBox
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomFrame
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomForm
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterHintWindow
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomScrollBar
   lifelihoodC2023.lpr(88,1) Error: Undefined symbol: WSRegisterCustomGroupBox
   ```
   I believe that this error is the Windows equivalent of the error I was facing on macOS. As originally suggested by the administrator of the Lazarus forum, I basically add the `Interfaces` unit to the `uses` clause of the main (.lpr) file, and it seems to solve the problem. The project compiles without any errors.

- ❌ **Compiling and Execute on macOS with Lazarus**. Dit not work.
   - I converted the Delphi project into a Lazarus project and tried to compile it on macOS. The project compiles without a single error `Error: -macosx_version_min has been renamed to -macos_version_min`, but it does not break the compilation process. However, when I try to run the executable file, it does not work. The R console (see root/R/lifehood_mac.R) returns the following error:
   ```R
   > system(
   +     command,
   +     input = arg_string,
   +     intern = FALSE,
   +     wait = TRUE,
   + )
   EAccessViolation: Access violation
   ```


<br>

### 2024 May 6

- The execution error comes from the `Metropolise` procedure (`Unit2.pas`) right after the `writeln('here');` line:

```pascal
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
    writeln('here');
    CurrResult := f(fun_des);
    if CurrResult > BestResult then
    begin
      BestResult := CurrResult;
      for j := 1 to number_of_variables do
        with var_info[j] do
          Bestvalue := value;
    end;
  end;
   { ******************************************** }

   code continues...
```

I'm not sure to understand what `f(fun_des)` does.

I also added some print statement in `Unit1.pas`, and error should be somewhere near them:

```pascal
For i:=0 to nbparposs-1 do
with FD.paramdescript[i] do
        begin
        readln(fc,check );
        lal.DelimitedText:=check;
        name := lal[0];
        minBound :=StrToFloat(lal[1]);
        maxBound :=StrToFloat(lal[2]);
        writeln('This actually prints');
        end;

if fitness_repar=1 then
begin
  writeln('This does not print');
  with FD.paramdescript[10] do        //pontn est le 11 param ds custom.txt
  begin
    writeln('Inside with block');
    readln(fc, check);
    lal.DelimitedText := check;
    name := lal[0];
    minBound := StrToFloat(lal[1]);
    maxBound := StrToFloat(lal[2]);
    writeln('After reading');
  end;
end;
```

### 2024 May 7: finally working!

According to the [same discussion](https://forum.lazarus.freepascal.org/index.php/topic,67059.0.html):

*"In unit2 of the converted project there is a list of units in the uses clause.*

*In that uses clause the units forms and grids are listed. If I understood correctly then these needs to be removed."*

So I removed the `Forms` and `Grids` units from the `uses` clause of the `Unit2.pas` file.

Then:
*"In that same unit2 you there is an implementation of the function f that has a line that reads:*

```pascal
Application.ProcessMessages;
```

*That can be removed (or commented) as well."*

I also replaced:

```pascal
{$APPTYPE CONSOLE}
```

by 

```pascal
{$ifdef windows}
{$APPTYPE CONSOLE}
{$endif}
```

I'm not sure why it was necessary, but I believe that the conversion to Lazarus was not perfect because this element is for Windows only.

Finally, I was able to compile the project on macOS and run it successfully. The project is now working on both Windows and macOS.

<br>

Most info can be found on the discussion on the Lazarus forum mentioned above: https://forum.lazarus.freepascal.org/index.php/topic,67059.0.html