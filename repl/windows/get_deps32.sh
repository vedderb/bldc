#!/bin/bash
# Downloads 32-bit Windows dependencies for lbm32.exe
# Run from the repl/windows directory.
# Requirements: curl or wget, tar, zstd

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

SDL2_VER=2.32.6
SDL2_IMAGE_VER=2.8.8

SDL2_URL="https://github.com/libsdl-org/SDL/releases/download/release-${SDL2_VER}/SDL2-devel-${SDL2_VER}-mingw.tar.gz"
SDL2_IMAGE_URL="https://github.com/libsdl-org/SDL_image/releases/download/release-${SDL2_IMAGE_VER}/SDL2_image-devel-${SDL2_IMAGE_VER}-mingw.tar.gz"

MSYS2_BASE="https://repo.msys2.org/mingw/mingw32"

download() {
    local url="$1" dest="$2"
    echo "  -> $url"
    if command -v curl &>/dev/null; then
        curl -L --fail -o "$dest" "$url"
    else
        wget -q -O "$dest" "$url"
    fi
}

msys2_find_pkg() {
    local base="$1"
    local result
    result=$(curl -s "$MSYS2_BASE/" | grep -o "\"${base}-[^\"]*\.pkg\.tar\.zst\"" | tr -d '"' | sort -V | tail -1)
    if [ -z "$result" ]; then
        echo "ERROR: could not find package '$base' in MSYS2 repo" >&2
        exit 1
    fi
    echo "$result"
}

extract_msys2_pkg() {
    local pkg="$1" destdir="$2"
    local extractdir="$TMPDIR/${pkg}.extracted"
    mkdir -p "$extractdir"
    tar -I zstd -xf "$TMPDIR/$pkg" -C "$extractdir"
    mkdir -p "$destdir"
    cp -r "$extractdir/mingw32/"* "$destdir/"
}

download_msys2_pkg() {
    local base="$1" destdir="$2"
    local pkg
    pkg=$(msys2_find_pkg "$base")
    echo "  found: $pkg"
    download "$MSYS2_BASE/$pkg" "$TMPDIR/$pkg"
    extract_msys2_pkg "$pkg" "$destdir"
}

echo "=== Downloading SDL2 ${SDL2_VER} ==="
download "$SDL2_URL" "$TMPDIR/sdl2.tar.gz"
tar -xzf "$TMPDIR/sdl2.tar.gz" -C "$TMPDIR"
mkdir -p "$SCRIPT_DIR/SDL2"
cp -r "$TMPDIR/SDL2-${SDL2_VER}/i686-w64-mingw32/"* "$SCRIPT_DIR/SDL2/"

echo "=== Downloading SDL2_image ${SDL2_IMAGE_VER} ==="
download "$SDL2_IMAGE_URL" "$TMPDIR/sdl2_image.tar.gz"
tar -xzf "$TMPDIR/sdl2_image.tar.gz" -C "$TMPDIR"
mkdir -p "$SCRIPT_DIR/SDL2_image"
cp -r "$TMPDIR/SDL2_image-${SDL2_IMAGE_VER}/i686-w64-mingw32/"* "$SCRIPT_DIR/SDL2_image/"

echo "=== Downloading zlib (MSYS2 mingw32) ==="
download_msys2_pkg "mingw-w64-i686-zlib" "$SCRIPT_DIR/zlib"

echo "=== Downloading libpng (MSYS2 mingw32) ==="
download_msys2_pkg "mingw-w64-i686-libpng" "$SCRIPT_DIR/libpng"

echo "=== Downloading termcap (MSYS2 mingw32, readline dependency) ==="
download_msys2_pkg "mingw-w64-i686-termcap" "$SCRIPT_DIR/readline"

echo "=== Downloading readline (MSYS2 mingw32) ==="
download_msys2_pkg "mingw-w64-i686-readline" "$SCRIPT_DIR/readline"

echo ""
echo "=== 32-bit dependency DLLs ==="
echo "SDL2:"
ls "$SCRIPT_DIR/SDL2/bin/"*.dll
echo "SDL2_image:"
ls "$SCRIPT_DIR/SDL2_image/bin/"*.dll
echo "zlib:"
ls "$SCRIPT_DIR/zlib/bin/"*.dll
echo "libpng:"
ls "$SCRIPT_DIR/libpng/bin/"*.dll
echo "readline:"
ls "$SCRIPT_DIR/readline/bin/"*.dll
echo ""
echo "Done. You can now run: make -f WinMakefile"
