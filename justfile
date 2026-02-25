# Use bash on macOS/Linux

set shell := ["bash", "-c"]

# Use cmd.exe on Windows so `just windows` works without WSL/Git-Bash/MSYS

set windows-shell := ["cmd.exe", "/c"]

fpc := env_var_or_default("FPC", "fpc")
project_dir := invocation_directory()
lazarus_dir := home_dir() / ".lazarus"
build_dir := project_dir / "inst/bin"
src_dir := project_dir / "source"
linux_platform := env_var_or_default("LIFELIHOOD_LINUX_PLATFORM", "linux/amd64")
linux_cpu := env_var_or_default("LIFELIHOOD_LINUX_CPU", "x86_64")
linux_image := env_var_or_default("LIFELIHOOD_LINUX_IMAGE", "pascal-builder-linux-amd64")

# ---- macOS Build ----
macos:
    @echo "Building for macOS..."
    @mkdir -p {{ build_dir }}
    {{ fpc }} -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -k-framework -kCocoa -l -vabq \
        -Fi{{ build_dir }} \
        -Fu{{ lazarus_dir }}/lib/units/x86_64-darwin/cocoa \
        -Fu{{ lazarus_dir }}/lib/LCLBase/units/x86_64-darwin \
        -Fu{{ lazarus_dir }}/lib/freetypelaz/lib/x86_64-darwin \
        -Fu{{ lazarus_dir }}/lib/LazUtils/lib/x86_64-darwin \
        -Fu{{ lazarus_dir }}/lib/units/x86_64-darwin \
        -Fu{{ src_dir }} \
        -FE{{ build_dir }} \
        -o{{ build_dir }}/lifelihood-macos \
        -dLCL -dLCLcocoa -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL \
        {{ src_dir }}/lifelihood.lpr

# ---- Linux Build ----
linux: build-image-linux
    @echo "Building for Linux ({{ linux_cpu }}, {{ linux_platform }})..."
    docker run --rm --platform {{ linux_platform }} -v $(pwd):/src -w /src {{ linux_image }} \
        fpc source/lifelihood.lpr -P{{ linux_cpu }} -oinst/bin/lifelihood-linux

build-image-linux:
    @echo "Building Docker image for Linux ({{ linux_image }}, {{ linux_platform }})..."
    docker build --platform {{ linux_platform }} -t {{ linux_image }} .

# ---- Windows Build ----
windows:
    {{ fpc }} -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -l -vabq -Fi{{ build_dir }} -Fu{{ lazarus_dir }}\lib\units\i386-win32\win32 -Fu{{ lazarus_dir }}\lib\LCLBase\units\i386-win32 -Fu{{ lazarus_dir }}\lib\freetypelaz\lib\i386-win32 -Fu{{ lazarus_dir }}\lib\LazUtils\lib\i386-win32 -Fu{{ lazarus_dir }}\lib\units\i386-win32 -Fu{{ src_dir }} -FE{{ build_dir }} -o{{ build_dir }}\lifelihood-windows.exe -dLCL -dLCLwin32 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL {{ src_dir }}\lifelihood.lpr
