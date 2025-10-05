.PHONY: all build

FPC ?= fpc
PROJECT_DIR ?= $(CURDIR)
LAZARUS_DIR ?= $(HOME)/.lazarus

BUILD_DIR := $(PROJECT_DIR)/inst/bin
SRC_DIR := $(PROJECT_DIR)/source

build:
	@mkdir -p $(BUILD_DIR)
	$(FPC) -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -k-framework -kCocoa -l -vabq \
	-Fi$(BUILD_DIR) \
	-Fu$(LAZARUS_DIR)/lib/units/x86_64-darwin/cocoa \
	-Fu$(LAZARUS_DIR)/lib/LCLBase/units/x86_64-darwin \
	-Fu$(LAZARUS_DIR)/lib/freetypelaz/lib/x86_64-darwin \
	-Fu$(LAZARUS_DIR)/lib/LazUtils/lib/x86_64-darwin \
	-Fu$(LAZARUS_DIR)/lib/units/x86_64-darwin \
	-Fu$(SRC_DIR) \
	-FE$(BUILD_DIR) \
	-o$(BUILD_DIR)/lifelihood \
	-dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL \
	$(SRC_DIR)/lifelihood.lpr
