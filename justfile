set shell := ["bash", "-c"]

fpc := env_var_or_default("FPC", "fpc")
project_dir := invocation_directory()
lazarus_dir := home_dir() / ".lazarus"
build_dir := project_dir / "inst/bin"
src_dir := project_dir / "source"
linux_image := "pascal-builder-linux"

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
linux: check-image-linux
    @echo "Building for Linux (native arch)..."
    docker run --rm -v $(pwd):/src -w /src {{ linux_image }} \
        fpc source/lifelihood.lpr -oinst/bin/lifelihood-linux

check-image-linux:
    @if [ -z "$(docker images -q {{ linux_image }})" ]; then \
        echo "Building Docker image for Linux ({{ linux_image }})..."; \
        docker build -t {{ linux_image }} .; \
    else \
        echo "Docker image '{{ linux_image }}' already exists."; \
    fi

# ---- Windows Build ----
windows:
    @mkdir -p inst/bin
    {{ fpc }} -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -l -vabq \
        -Fi{{ build_dir }} \
        -Fu{{ lazarus_dir }}/lib/units/i386-win32/win32 \
        -Fu{{ lazarus_dir }}/lib/LCLBase/units/i386-win32 \
        -Fu{{ lazarus_dir }}/lib/freetypelaz/lib/i386-win32 \
        -Fu{{ lazarus_dir }}/lib/LazUtils/lib/i386-win32 \
        -Fu{{ lazarus_dir }}/lib/units/i386-win32 \
        -Fu{{ src_dir }} \
        -FE{{ build_dir }} \
        -o{{ build_dir }}/lifelihood-windows.exe \
        -dLCL -dLCLwin32 -dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL \
        {{ src_dir }}/lifelihood.lpr
