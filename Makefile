LIB = /Users/josephbarbier/.lazarus/lib
OUTDIR = inst/bin
OUTFILE = $(OUTDIR)/lifelihood

FLAGS = \
  -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl \
  -k-framework -kCocoa -l -vabq \
  -Fi$(OUTDIR) \
  -Fu$(LIB)/units/x86_64-darwin/cocoa \
  -Fu$(LIB)/LCLBase/units/x86_64-darwin \
  -Fu$(LIB)/freetypelaz/lib/x86_64-darwin \
  -Fu$(LIB)/LazUtils/lib/x86_64-darwin \
  -Fu$(LIB)/units/x86_64-darwin \
  -Fu./source/ \
  -FE$(OUTDIR) \
  -o$(OUTFILE) \
  -dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL

all: build

build:
	@echo "Compiling project..."
	fpc $(FLAGS) source/lifelihood.lpr

