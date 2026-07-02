#!/bin/bash
# Downloads 64-bit Windows dependencies for lbm64.exe
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

MSYS2_BASE="https://repo.msys2.org/mingw/mingw64"

download() {
    local url="$1" dest="$2"
    echo "  -> $url"
    if command -v curl &>/dev/null; then
        curl -L --fail -o "$dest" "$url"
    else
        wget -q -O "$dest" "$url"
    fi
}

# Find the current package filename in the MSYS2 repo by scraping the directory listing
msys2_find_pkg() {
    local base="$1"  # e.g. mingw-w64-x86_64-termcap
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
    cp -r "$extractdir/mingw64/"* "$destdir/"
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
mkdir -p "$SCRIPT_DIR/SDL2_64"
cp -r "$TMPDIR/SDL2-${SDL2_VER}/x86_64-w64-mingw32/"* "$SCRIPT_DIR/SDL2_64/"

echo "=== Downloading SDL2_image ${SDL2_IMAGE_VER} ==="
download "$SDL2_IMAGE_URL" "$TMPDIR/sdl2_image.tar.gz"
tar -xzf "$TMPDIR/sdl2_image.tar.gz" -C "$TMPDIR"
mkdir -p "$SCRIPT_DIR/SDL2_image_64"
cp -r "$TMPDIR/SDL2_image-${SDL2_IMAGE_VER}/x86_64-w64-mingw32/"* "$SCRIPT_DIR/SDL2_image_64/"

echo "=== Downloading zlib (MSYS2 mingw64) ==="
download_msys2_pkg "mingw-w64-x86_64-zlib" "$SCRIPT_DIR/zlib_64"

echo "=== Downloading libpng (MSYS2 mingw64) ==="
download_msys2_pkg "mingw-w64-x86_64-libpng" "$SCRIPT_DIR/libpng_64"

echo "=== Downloading termcap (MSYS2 mingw64, readline dependency) ==="
download_msys2_pkg "mingw-w64-x86_64-termcap" "$SCRIPT_DIR/readline_64"

echo "=== Downloading readline (MSYS2 mingw64) ==="
download_msys2_pkg "mingw-w64-x86_64-readline" "$SCRIPT_DIR/readline_64"

echo ""
echo "=== 64-bit dependency DLLs ==="
echo "SDL2:"
ls "$SCRIPT_DIR/SDL2_64/bin/"*.dll
echo "SDL2_image:"
ls "$SCRIPT_DIR/SDL2_image_64/bin/"*.dll
echo "zlib:"
ls "$SCRIPT_DIR/zlib_64/bin/"*.dll
echo "libpng:"
ls "$SCRIPT_DIR/libpng_64/bin/"*.dll
echo "readline:"
ls "$SCRIPT_DIR/readline_64/bin/"*.dll
echo ""
echo "Done. You can now run: make -f WinMakefile lbm64"
