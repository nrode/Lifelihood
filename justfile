# Use bash on macOS/Linux

set shell := ["bash", "-c"]

# Use cmd.exe on Windows so `just windows` works without WSL/Git-Bash/MSYS

set windows-shell := ["cmd.exe", "/c"]

fpc := env_var_or_default("FPC", "fpc")
project_dir := invocation_directory()
lazarus_dir := home_dir() / ".lazarus"
build_dir := project_dir / "inst/bin"
src_dir := project_dir / "source"
unit_dir := env_var_or_default("LIFELIHOOD_UNIT_DIR", "/tmp/lifelihood-fpc-units")
macos_fpc_options := "-MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -v0 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL"
linux_fpc_options := "-v0"
linux_x86_64_platform := env_var_or_default("LIFELIHOOD_LINUX_X86_64_PLATFORM", "linux/amd64")
linux_aarch64_platform := env_var_or_default("LIFELIHOOD_LINUX_AARCH64_PLATFORM", "linux/arm64")
linux_x86_64_image := env_var_or_default("LIFELIHOOD_LINUX_X86_64_IMAGE", "pascal-builder-linux-x86_64")
linux_aarch64_image := env_var_or_default("LIFELIHOOD_LINUX_AARCH64_IMAGE", "pascal-builder-linux-aarch64")
project_dir_win := env_var_or_default("CD", invocation_directory())
build_dir_win := project_dir_win / "inst" / "bin"
src_dir_win := project_dir_win / "source"
lazarus_dir_win := env_var_or_default("USERPROFILE", "") / ".lazarus"

default: cross

cross: macos linux

# ---- macOS Build ----
macos: macos-x86_64 macos-aarch64

macos-x86_64:
    @echo "Building for macOS (x86_64)..."
    @mkdir -p {{ build_dir }}
    @mkdir -p {{ unit_dir }}/macos-x86_64
    {{ fpc }} {{ macos_fpc_options }} \
        -Px86_64 \
        -Fi{{ src_dir }} \
        -Fu{{ src_dir }} \
        -FU{{ unit_dir }}/macos-x86_64 \
        -FE{{ build_dir }} \
        -o{{ build_dir }}/lifelihood-macos-x86_64 \
        {{ src_dir }}/lifelihood.lpr

macos-aarch64:
    @echo "Building for macOS (aarch64)..."
    @mkdir -p {{ build_dir }}
    @mkdir -p {{ unit_dir }}/macos-aarch64
    {{ fpc }} {{ macos_fpc_options }} \
        -Paarch64 \
        -Fi{{ src_dir }} \
        -Fu{{ src_dir }} \
        -FU{{ unit_dir }}/macos-aarch64 \
        -FE{{ build_dir }} \
        -o{{ build_dir }}/lifelihood-macos-aarch64 \
        {{ src_dir }}/lifelihood.lpr

# ---- Linux Build ----
linux: linux-x86_64 linux-aarch64

linux-x86_64: build-image-linux-x86_64
    @echo "Building for Linux (x86_64, {{ linux_x86_64_platform }})..."
    @mkdir -p {{ build_dir }}
    docker run --rm --platform {{ linux_x86_64_platform }} -v "{{ project_dir }}:/src" -w /src {{ linux_x86_64_image }} \
        bash -lc 'mkdir -p /tmp/lifelihood-fpc-units/linux-x86_64 && fpc {{ linux_fpc_options }} -Px86_64 -Fisource -Fusource -FU/tmp/lifelihood-fpc-units/linux-x86_64 -FEinst/bin -oinst/bin/lifelihood-linux-x86_64 source/lifelihood.lpr'

linux-aarch64: build-image-linux-aarch64
    @echo "Building for Linux (aarch64, {{ linux_aarch64_platform }})..."
    @mkdir -p {{ build_dir }}
    docker run --rm --platform {{ linux_aarch64_platform }} -v "{{ project_dir }}:/src" -w /src {{ linux_aarch64_image }} \
        bash -lc 'mkdir -p /tmp/lifelihood-fpc-units/linux-aarch64 && fpc {{ linux_fpc_options }} -Paarch64 -Fisource -Fusource -FU/tmp/lifelihood-fpc-units/linux-aarch64 -FEinst/bin -oinst/bin/lifelihood-linux-aarch64 source/lifelihood.lpr'

build-image-linux-x86_64:
    @echo "Building Docker image for Linux ({{ linux_x86_64_image }}, {{ linux_x86_64_platform }})..."
    docker build --platform {{ linux_x86_64_platform }} -t {{ linux_x86_64_image }} .

build-image-linux-aarch64:
    @echo "Building Docker image for Linux ({{ linux_aarch64_image }}, {{ linux_aarch64_platform }})..."
    docker build --platform {{ linux_aarch64_platform }} -t {{ linux_aarch64_image }} .

# ---- Windows Build ----
windows:
    mkdir inst\bin 2>NUL || exit /b 0
    mkdir lifelihood-fpc-units 2>NUL || exit /b 0
    mkdir lifelihood-fpc-units\windows 2>NUL || exit /b 0
    {{ fpc }} -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -l -vabq -Fisource -Fusource -FUlifelihood-fpc-units\windows -FEinst\bin -oinst\bin\lifelihood-windows.exe -dLCL -dLCLwin32 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL source\lifelihood.lpr

clean:
    find . -maxdepth 1 -type d -regex '.*/lifelihood_[0-9]\{2,4\}_[0-9]\{2,4\}_[0-9]\{2,4\}_[0-9]\{2,4\}' -exec rm -rf {} +

    rm -rf doc/
    rm -rf docs/
    rm -rf figure/
