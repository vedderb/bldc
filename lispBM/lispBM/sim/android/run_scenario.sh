#!/bin/bash
# Build the runner with a given scenario, install it on the connected
# emulator/device, and launch it.
#
# Always builds against API 30 with Java 11 (Qt 5.15 requirement).
# The same APK runs on any Android version >= 6.0.
# Use android.sh <version> to start the right emulator before running this.
#
# Usage: run_scenario.sh [scenario-dir] [arch]
#   scenario-dir  Path to the scenario directory (default: ../scenarios/hello_widgets)
#   arch          x86_64 | arm64 | armv7            (default: x86_64)
#
# Environment:
#   QT_ROOT      Root of aqtinstall Qt tree  (default: ~/Qt)
#   QT_VERSION   Qt version to use           (default: 5.15.2)

set -e

# Qt 5.15 Gradle (~5.6) requires Java 11; breaks on Java 17+.
if [[ "$(uname)" == "Darwin" ]]; then
  JAVA11=$(/usr/libexec/java_home -v 11 2>/dev/null || true)
else
  JAVA11=/usr/lib/jvm/java-11-openjdk-amd64
fi
if [ -d "$JAVA11" ]; then
  export JAVA_HOME="$JAVA11"
  export PATH="$JAVA_HOME/bin:$PATH"
else
  echo "warning: JDK 11 not found — install with: sudo apt install openjdk-11-jdk"
fi

# Kill any Gradle daemon started with a different JVM.
pkill -f GradleDaemon 2>/dev/null || true
export GRADLE_OPTS="-Dorg.gradle.daemon=false"

SIM_DIR="$(cd "$(dirname "$0")" && pwd)"
RUNNER_DIR="$SIM_DIR/runner"

SCENARIO_DIR="${1:-$SIM_DIR/scenarios/hello_widgets}"
if [[ "$(uname)" == "Darwin" ]]; then
  ARCH="${2:-arm64}"
else
  ARCH="${2:-x86_64}"
fi

SCENARIO_DIR="$(cd "$SCENARIO_DIR" && pwd)"

QT_ROOT="${QT_ROOT:-$HOME/Qt}"
QT_VERSION="${QT_VERSION:-5.15.2}"

# ── Arch → Android ABI ────────────────────────────────────────────────────────
case "$ARCH" in
  x86_64) ANDROID_ABI="x86_64";      EXTRA_DEFINES="" ;;
  arm64)  ANDROID_ABI="arm64-v8a";   EXTRA_DEFINES="" ;;
  armv7)  ANDROID_ABI="armeabi-v7a"; EXTRA_DEFINES="DEFINES+=LBM32" ;;
  *)      echo "Unknown arch '$ARCH' (supported: x86_64 arm64 armv7)"; exit 1 ;;
esac

QMAKE="$QT_ROOT/$QT_VERSION/android/bin/qmake"
if [ ! -x "$QMAKE" ]; then
  echo "error: qmake not found at $QMAKE"
  echo "Install: aqt install-qt linux android $QT_VERSION android -O $QT_ROOT"
  exit 1
fi

SCENARIO_QRC="$SCENARIO_DIR/scenario.qrc"
[ -f "$SCENARIO_QRC" ] || { echo "error: no scenario.qrc in $SCENARIO_DIR"; exit 1; }

BUILD_DIR="$SIM_DIR/build-${ARCH}"

echo "=== Scenario : $(basename "$SCENARIO_DIR") ==="
echo "=== Arch     : $ARCH ($ANDROID_ABI) ==="
echo "=== Build dir: $BUILD_DIR ==="
echo ""

# ── Build ────────────────────────────────────────────────────────────────────
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

"$QMAKE" \
  "SCENARIO_QRC=$SCENARIO_QRC" \
  "ANDROID_ABIS=$ANDROID_ABI" \
  ${EXTRA_DEFINES:+"$EXTRA_DEFINES"} \
  "$RUNNER_DIR/runner.pro"

make -j"$(nproc)"
make apk

# ── Find APK ──────────────────────────────────────────────────────────────────
APK=$(find . -name "*debug*.apk" | head -1)
[ -n "$APK" ] || { echo "error: no APK found under $BUILD_DIR"; exit 1; }
echo "APK: $APK"

# ── Package name ──────────────────────────────────────────────────────────────
if command -v aapt &>/dev/null; then
  PKG=$(aapt dump badging "$APK" | grep "^package:" | sed "s/.*name='//;s/'.*//")
else
  PKG="org.qtproject.example.lbm_sim_runner"
  echo "note: aapt not found; assuming package name: $PKG"
fi

# ── Install & launch ──────────────────────────────────────────────────────────
echo ""
echo "=== Installing ==="
adb install -r "$APK"

echo ""
echo "=== Launching ==="
adb shell am start -n "$PKG/org.qtproject.qt5.android.bindings.QtActivity"

echo ""
echo "App is running.  Follow output with:"
echo "  adb logcat -s Qt,lbm"
