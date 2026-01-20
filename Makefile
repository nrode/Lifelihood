.PHONY: all macos linux windows check-image check-image-linux check-image-windows \
        rust-macos rust-linux rust-windows rust-all rust-test rust-clean

FPC ?= fpc
PROJECT_DIR ?= $(CURDIR)
LAZARUS_DIR ?= $(HOME)/.lazarus

BUILD_DIR := $(PROJECT_DIR)/inst/bin
SRC_DIR := $(PROJECT_DIR)/source
RUST_DIR := $(SRC_DIR)/rust

LINUX_IMAGE := pascal-builder-linux

.DEFAULT_GOAL := macos

# ---- Rust Build Targets ----

rust-all: rust-macos rust-linux rust-windows

rust-macos:
	@echo "Building Rust binary for macOS..."
	@mkdir -p $(BUILD_DIR)
	cd $(RUST_DIR) && cargo build --release
	cp $(RUST_DIR)/target/release/lifelihood $(BUILD_DIR)/lifelihood-macos-rust

rust-linux:
	@echo "Building Rust binary for Linux..."
	@mkdir -p $(BUILD_DIR)
	cd $(RUST_DIR) && cross build --release --target x86_64-unknown-linux-gnu
	cp $(RUST_DIR)/target/x86_64-unknown-linux-gnu/release/lifelihood $(BUILD_DIR)/lifelihood-linux-rust

rust-windows:
	@echo "Building Rust binary for Windows..."
	@mkdir -p $(BUILD_DIR)
	cd $(RUST_DIR) && cross build --release --target x86_64-pc-windows-gnu
	cp $(RUST_DIR)/target/x86_64-pc-windows-gnu/release/lifelihood.exe $(BUILD_DIR)/lifelihood-windows-rust.exe

rust-test:
	@echo "Running Rust tests..."
	cd $(RUST_DIR) && cargo test

rust-clean:
	@echo "Cleaning Rust build artifacts..."
	cd $(RUST_DIR) && cargo clean

# ---- Pascal Build Targets ----

macos:
	@echo "Building for macOS..."
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
		-o$(BUILD_DIR)/lifelihood-macos \
		-dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL \
		$(SRC_DIR)/lifelihood.lpr

# ---- Linux Build ----
linux: check-image-linux
	@echo "Building for Linux (native arch)..."
	docker run --rm -v $(shell pwd):/src -w /src $(LINUX_IMAGE) \
		fpc source/lifelihood.lpr -oinst/bin/lifelihood-linux

check-image-linux:
	@if [ -z "$$(docker images -q $(LINUX_IMAGE))" ]; then \
		echo "Building Docker image for Linux ($(LINUX_IMAGE))..."; \
		docker build -t $(LINUX_IMAGE) .; \
	else \
		echo "Docker image '$(LINUX_IMAGE)' already exists."; \
	fi


# ---- Windows Build ----
windows:
	fpc -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -l -vabq -Fiinst/bin -FuC:\Users\josephbarbier\.lazarus/lib/units/i386-win32/win32 -FuC:\Users\josephbarbier\.lazarus/lib/LCLBase/units/i386-win32 -FuC:\Users\josephbarbier\.lazarus/lib/freetypelaz/lib/i386-win32 -FuC:\Users\josephbarbier\.lazarus/lib/LazUtils/lib/i386-win32 -FuC:\Users\josephbarbier\.lazarus/lib/units/i386-win32 -Fusource -FEinst/bin -oinst/bin/lifelihood-windows.exe -dLCL -dLCLwin32 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL source/lifelihood.lpr