.PHONY: all macos linux windows all test clean

PROJECT_DIR ?= $(CURDIR)
BUILD_DIR := $(PROJECT_DIR)/inst/bin
SRC_DIR := $(PROJECT_DIR)/source
RUST_DIR := src

all: macos linux windows

macos:
	@echo "Building Rust binary for macOS..."
	cd $(RUST_DIR) && cargo build --release
	cp target/aarch64-apple-darwin/release/lifelihood $(BUILD_DIR)/lifelihood-macos

linux:
	@echo "Building Rust binary for Linux..."
	@mkdir -p $(BUILD_DIR)
	cd $(RUST_DIR) && cross build --release --target x86_64-unknown-linux-gnu
	cp target/x86_64-unknown-linux-gnu/release/lifelihood $(BUILD_DIR)/lifelihood-linux

windows:
	@echo "Building Rust binary for Windows..."
	@mkdir -p $(BUILD_DIR)
	cd $(RUST_DIR) && cross build --release --target x86_64-pc-windows-gnu
	cp target/x86_64-pc-windows-gnu/release/lifelihood.exe $(BUILD_DIR)/lifelihood-windows.exe

test:
	@echo "Running Rust tests..."
	cd $(RUST_DIR) && cargo test

clean:
	@echo "Cleaning Rust build artifacts..."
	cd $(RUST_DIR) && cargo clean
