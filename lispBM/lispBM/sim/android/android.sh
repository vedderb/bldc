#!/bin/bash
# Start the Android emulator for LispBM simulation.
#
# Usage: android.sh [android-version]
#   android-version  11 | 12 | 13 | 14 | 15  (default: 11)
#
# On Linux  uses x86_64 images (KVM-accelerated).
# On macOS  uses arm64 images  (native Apple Silicon).
#
# The required system image and AVD are created automatically if absent.
# Build and deploy a scenario with: ./scripts/run_scenario.sh

set -e

ANDROID_VER="${1:-11}"

SDK_ROOT="${ANDROID_SDK_ROOT:-$HOME/android/sdk}"
SDKMAN="$SDK_ROOT/cmdline-tools/latest/bin/sdkmanager"

# ── Android version → API level ───────────────────────────────────────────────
case "$ANDROID_VER" in
  11) API_LEVEL=30 ;;
  12) API_LEVEL=31 ;;
  13) API_LEVEL=33 ;;
  14) API_LEVEL=34 ;;
  15) API_LEVEL=35 ;;
  *)  echo "Unknown Android version '$ANDROID_VER' (supported: 11–15)"; exit 1 ;;
esac

# ── Select arch based on host ─────────────────────────────────────────────────
if [[ "$(uname)" == "Darwin" ]]; then
  ARCH="arm64-v8a"
else
  ARCH="x86_64"
fi

# ── AVD name ──────────────────────────────────────────────────────────────────
if [ "$ANDROID_VER" = "11" ]; then
  AVD="lbm_${ARCH%%[-_]*}"   # lbm_arm64 or lbm_x86_64
else
  AVD="lbm_${ARCH%%[-_]*}_android${ANDROID_VER}"
fi

SYSIMG="system-images;android-${API_LEVEL};google_apis;${ARCH}"

# ── Auto-create AVD if needed ─────────────────────────────────────────────────
if ! avdmanager list avd 2>/dev/null | grep -q "Name: $AVD"; then
  echo "AVD '$AVD' not found — installing system image and creating AVD..."
  if [ -x "$SDKMAN" ]; then
    "$SDKMAN" --sdk_root="$SDK_ROOT" "$SYSIMG"
  else
    echo "warning: sdkmanager not found — assuming system image is installed."
  fi
  avdmanager create avd -n "$AVD" -k "$SYSIMG" -d pixel_5 --force
  echo "AVD '$AVD' created."
fi

echo "Starting emulator: $AVD (Android $ANDROID_VER, API $API_LEVEL)"
emulator -avd "$AVD" -no-snapshot-save &

echo "Waiting for device..."
adb wait-for-device

echo "Waiting for boot to complete..."
while [ "$(adb shell getprop sys.boot_completed 2>/dev/null | tr -d '\r')" != "1" ]; do
  sleep 2
done

echo "Emulator ready ($AVD)."
echo "Tip: adb logcat -s Qt,lbm"
