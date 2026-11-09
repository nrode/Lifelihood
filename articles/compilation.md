# Compilation

In order to compile the underlying Pascal code, you need to install:

- [Lazarus](https://www.lazarus-ide.org/index.php?page=downloads): code
  editor for Pascal
- [FPC](https://www.freepascal.org/download.html) (Free Pascal
  Compiler): the compiler
- [Docker](https://www.docker.com/): facilitate the compilation process
- (ideally) [just](https://github.com/casey/just): facilitate the
  compilation process
- (optional) [Pascal VS Code / Positron
  extension](https://github.com/alefragnani/vscode-language-pascal):
  facilitates development

## Setup R environment

Then, you need to be **inside** the package itself when developing. For
this:

- Clone the project:

``` bash
git clone https://github.com/nrode/Lifelihood.git
```

- Open the project in RStudio or any other editor

- Install development dependencies: install all necessary dependencies
  for development, including **Lifelihood** itself, by running:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"devtools"``, ``"testthat"``, ``"roxygen2"``, ``"pkgdown"``)``)`\
`remotes``::``install_local``(``dependencies ``=`` ``TRUE``)`

When developing an R package, you should **not** use
[`library(lifelihood)`](https://nrode.github.io/Lifelihood/), but
rather:

\
`devtools``::`[`load_all`](https://devtools.r-lib.org/reference/load_all.html)`(``)`

This lets you use the latest local version without reinstalling the
package every time you make a change.

To check that everything worked as expected, run:

\
`devtools``::`[`test`](https://devtools.r-lib.org/reference/test.html)`(``)`

This runs all unit tests in the package. If everything is okay, it
should print several messages and end with something like this:

``` r
══ Results ═══════════════════════════════════════════════
Duration: 4.8 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 94 ]
```

## Compilation

- On macOS

Run `just macos`. This runs the `macos` recipes inside `justfile`, and
will automatically update:

- `inst/bin/lifelihood-macos-x86_64`

- `inst/bin/lifelihood-macos-aarch64`

- On Linux

Run `just linux`. This runs the `linux` recipes inside `justfile`, and
will automatically update:

- `inst/bin/lifelihood-linux-x86_64`
- `inst/bin/lifelihood-linux-aarch64`

The Linux builds use Docker with `linux/amd64` and `linux/arm64` images.
If this is the first time you’re compiling, this might take some time as
it will build Docker images first.

To build every supported macOS and Linux target, run `just`.

> Unless you are on a Linux machine, you need to have Docker installed
> and the Docker daemon running.

- On Windows

The `just windows` command is a work in progress. You can still compile
manually using Lazarus.

## Open project in Lazarus

Then open Lazarus and select “Open project.” Choose the `lifelihood.lpi`
file in `Lifelihood/source/`.

Finally, set the output directory for the compiled files. Go to
`Project > Project Options > Compiler Options > Paths`. In “Unit output
directory (-FU),” set the path to `Lifelihood/inst/bin/new/`. For
example, the full path might be
`/Users/josephbarbier/Desktop/Lifelihood/inst/bin/new/`.

The compiled files will be written to this directory. Then, take the
executable from the `new/` directory (`lifelihood.exe` on Windows or
`lifelihood` on macOS), remove the `C2023` suffix if present, and move
it to `Lifelihood/inst/bin/`, replacing the previous executable.

You should now be able to compile the project with `Run > Compile`.
