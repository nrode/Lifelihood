#
# Cross-Platform Makefile for Free Pascal / Lazarus Project
#

# --- Project Configuration ---

# The name of your main project source file (without extension)
PROJECT_LPR = lifelihood

# The source directory
SRCDIR = source

# The output directory for compiled files
OUTDIR = inst/bin

# --- Platform Detection ---

# Default to Linux if OS is not Windows_NT
ifeq ($(OS),Windows_NT)
    detected_os := Windows
    # For Windows, we need to define 'uname -m' behavior or assume it
    # We will assume x86_64, but this can be overridden
    detected_cpu ?= x86_64
else
    detected_os := $(shell uname -s)
    detected_cpu := $(shell uname -m)
endif

# --- OS-Specific Configuration ---

# Default Lazarus directory paths. User can override these by setting them as environment variables.
# Example: export LAZ_DIR=/path/to/my/lazarus
ifeq ($(detected_os),Darwin)  # macOS
    OS_TARGET = darwin
    CPU_TARGET = $(detected_cpu)
    LCL_WIDGETSET = cocoa
    FRAMEWORK_FLAGS = -k-framework -kCocoa
    LAZ_DIR ?= $(HOME)/.lazarus
    EXE_EXT =

else ifeq ($(detected_os),Linux)
    OS_TARGET = linux
    CPU_TARGET = $(detected_cpu)
    LCL_WIDGETSET ?= gtk3 # Can be overridden with gtk2, qt5, etc.
    FRAMEWORK_FLAGS =
    # Check common Lazarus installation paths
    ifeq ($(wildcard /usr/lib/lazarus),)
        LAZ_DIR ?= $(HOME)/.lazarus
    else
        LAZ_DIR ?= /usr/lib/lazarus
    endif
    EXE_EXT =

else ifeq ($(detected_os),Windows)
    OS_TARGET = win64 # Assuming 64-bit target, can be changed to win32
    CPU_TARGET = $(detected_cpu)
    LCL_WIDGETSET = win32
    FRAMEWORK_FLAGS = -WC -WR # Console application flags for Windows
    LAZ_DIR ?= C:/lazarus
    EXE_EXT = .exe

endif

# --- Paths and Output ---

# Construct the full output file path
OUTFILE = $(OUTDIR)/$(PROJECT_LPR)$(EXE_EXT)

# Standard Lazarus library path
LIB = $(LAZ_DIR)/lcl

# --- Compiler Flags ---

# Base flags for the Free Pascal Compiler (FPC)
FLAGS = \
  -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl \
  -l -vabq \
  -FE$(OUTDIR) \
  -Fi$(OUTDIR) \
  -o$(OUTFILE)

# Platform-specific unit paths and flags
FLAGS += \
  -Fu$(LIB)/units/$(CPU_TARGET)-$(OS_TARGET) \
  -Fu$(LIB)/units/$(CPU_TARGET)-$(OS_TARGET)/$(LCL_WIDGETSET) \
  -Fu$(LAZ_DIR)/components/lazutils/lib/$(CPU_TARGET)-$(OS_TARGET) \
  -Fu$(LAZ_DIR)/packager/units/$(CPU_TARGET)-$(OS_TARGET) \
  -Fu$(SRCDIR) \
  $(FRAMEWORK_FLAGS)

# LCL-specific defines
FLAGS += \
  -dLCL -dLCL$(LCL_WIDGETSET) -dRELEASE

# --- Build Targets ---

# Default target
all: build

# The main build rule
build:
	@echo "--- Compiling for $(detected_os) ($(CPU_TARGET)) ---"
	@mkdir -p $(OUTDIR)
	fpc $(FLAGS) $(SRCDIR)/$(PROJECT_LPR).lpr

# A rule to display detected variables for debugging
debug:
	@echo "OS Detected    : $(detected_os)"
	@echo "CPU Detected   : $(detected_cpu)"
	@echo "LCL Widgetset  : $(LCL_WIDGETSET)"
	@echo "Lazarus Dir    : $(LAZ_DIR)"
	@echo "Output File    : $(OUTFILE)"
	@echo "Compiler Flags : $(FLAGS)"

.PHONY: all build clean debug