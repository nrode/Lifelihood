# Makefile for compiling the Lifelihood Pascal project

FPC = /usr/local/bin/fpc
SRC = source/lifelihood.lpr
OUTDIR = inst/bin
OUTFILE = $(OUTDIR)/lifelihood

FLAGS = \
  -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl \
  -k-framework -kCocoa -l -vabq \
  -Fi$(OUTDIR) \
  -Fu/Users/josephbarbier/.lazarus/lib/units/x86_64-darwin/cocoa \
  -Fu/Users/josephbarbier/.lazarus/lib/LCLBase/units/x86_64-darwin \
  -Fu/Users/josephbarbier/.lazarus/lib/freetypelaz/lib/x86_64-darwin \
  -Fu/Users/josephbarbier/.lazarus/lib/LazUtils/lib/x86_64-darwin \
  -Fu/Users/josephbarbier/.lazarus/lib/units/x86_64-darwin \
  -Fu./source/ \
  -FE$(OUTDIR) \
  -o$(OUTFILE) \
  -dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL

all: build

build:
	@echo "Compiling project..."
	$(FPC) $(FLAGS) $(SRC)

clean:
	@echo "Cleaning up..."
	rm -f $(OUTFILE) *.o *.ppu *.a *.rsj *.rst *.s *.or
