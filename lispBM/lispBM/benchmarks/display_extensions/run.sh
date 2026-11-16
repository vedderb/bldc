#!/bin/bash
# Builds and runs the C-level display_extensions drawing-routine
# benchmark (bench.c). Writes a CSV named with the current date/time
# and the LBM version into results/, so multiple runs on the same day
# don't clobber each other.
#
# Usage:
#   ./run.sh          benchmark only
#   ./run.sh --ppm    also dump one .ppm snapshot per shape into ppm/,
#                     for visually sanity-checking that the benchmark
#                     draws what it claims to (see bench.c)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Building bench..."
make -C "$SCRIPT_DIR" >/dev/null

cd "$SCRIPT_DIR"
./bench "$@"

if [ "$1" == "--ppm" ]; then
    feh ./ppm/*.ppm
fi
