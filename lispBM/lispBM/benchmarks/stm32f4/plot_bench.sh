#!/bin/bash

set -e
cd "$(dirname "$0")"

VENV_DIR=".venv"
REQUIRED_PACKAGES=(pandas matplotlib numpy)

create_venv() {
    local py="$1"
    echo "Creating virtualenv in $VENV_DIR using $py..."
    rm -rf "$VENV_DIR"
    "$py" -m venv "$VENV_DIR"
}

if [ ! -x "$VENV_DIR/bin/pip" ]; then
    # The default python3 on this system may be a custom build without
    # ensurepip, which produces a venv with no pip. Prefer it, but fall
    # back to other interpreters if it doesn't give us a working pip.
    for py in python3 /usr/bin/python3 python3.10 python3.11 python3.12; do
        command -v "$py" >/dev/null 2>&1 || continue
        create_venv "$py" || true
        if [ -x "$VENV_DIR/bin/pip" ]; then
            break
        fi
    done
fi

if [ ! -x "$VENV_DIR/bin/pip" ]; then
    echo "error: could not create a virtualenv with a working pip." >&2
    echo "Install a python3 with a working 'venv'/'ensurepip' module (e.g. 'sudo apt install python3-venv') and re-run." >&2
    exit 1
fi

# shellcheck source=/dev/null
source "$VENV_DIR/bin/activate"

if ! python -c "import pandas, matplotlib, numpy" >/dev/null 2>&1; then
    echo "Installing required packages: ${REQUIRED_PACKAGES[*]}..."
    pip install --quiet --upgrade pip
    pip install --quiet "${REQUIRED_PACKAGES[@]}"
fi

python plot_bench.py

deactivate
