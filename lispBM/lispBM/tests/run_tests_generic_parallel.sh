#!/bin/bash

# Parallel Generic test runner for LispBM test binaries
# This script runs tests in parallel across multiple CPU cores
#
# Usage: run_tests_generic_parallel.sh <variant> [logfile] [num_jobs]
#   variant: One of: 32bit, 64bit, 32bit_time, 64bit_time, gc, revgc, cov, cov_64
#   logfile: Optional custom log file path
#   num_jobs: Optional number of parallel jobs (default: auto-detect)

VARIANT="$1"
LOGFILE="$2"
SELECTED_NUM_JOBS="$3"

if [ -z "$VARIANT" ]; then
  echo "Usage: $0 <variant> [logfile] [num_jobs]"
  echo "  variant: 32bit, 64bit, 32bit_time, 64bit_time, gc, revgc"
  echo "  logfile: Optional custom log file path"
  echo "  num_jobs: Optional number of parallel jobs (default: auto-detect)"
  exit 1
fi

# Detect number of CPU cores if not specified
if [ -z "$SELECTED_NUM_JOBS" ]; then
  SELECTED_NUM_JOBS=$(nproc 2>/dev/null || echo 4)
fi

# Set a default timeout value
timeout="50"

NUM_JOBS=$SELECTED_NUM_JOBS

# Configure based on variant
case "$VARIANT" in
  32bit)
    BINARY="test_lisp_code_cps"
    USE_TIMEOUT=true
    EXCLUDE_TEST="test_is_64bit.lisp"
    DESCRIPTION="32BIT TESTS"
    COVERAGE_JSON="tests_cov_32.json"
    COVERAGE_ENABLED=true
    ;;
  64bit)
    BINARY="test_lisp_code_cps_64"
    USE_TIMEOUT=true
    EXCLUDE_TEST="test_is_32bit.lisp"
    DESCRIPTION="64BIT TESTS"
    COVERAGE_JSON="tests_cov_64.json"
    COVERAGE_ENABLED=true
    ;;
  32bit_time)
    BINARY="test_lisp_code_cps_time"
    USE_TIMEOUT=false
    EXCLUDE_TEST="test_is_64bit.lisp"
    DESCRIPTION="32BIT TIME SCHEDULER TESTS"
    COVERAGE_ENABLED=false
    ;;
  64bit_time)
    BINARY="test_lisp_code_cps_64_time"
    USE_TIMEOUT=false
    EXCLUDE_TEST="test_is_32bit.lisp"
    DESCRIPTION="64BIT TIME SCHEDULER TESTS"
    COVERAGE_ENABLED=false
    ;;
  gc)
    BINARY="test_lisp_code_cps_gc"
    USE_TIMEOUT=true
    # Set a huge timeout for always_gc tests
    # Parallelising these seems to increase the time they take per test.
    timeout="200"
    EXCLUDE_TEST="test_is_64bit.lisp"
    DESCRIPTION="ALWAYS GC TESTS"
    COVERAGE_ENABLED=false
    ;;
  revgc)
    BINARY="test_lisp_code_cps_revgc"
    USE_TIMEOUT=true
    EXCLUDE_TEST="test_is_64bit.lisp"
    DESCRIPTION="POINTER REVERSAL GC TESTS"
    COVERAGE_ENABLED=false
    ;;
  *)
    echo "Unknown variant: $VARIANT"
    echo "Valid variants: 32bit, 64bit, 32bit_time, 64bit_time, gc, revgc, cov, cov_64"
    exit 1
    ;;
esac

echo "Running $VARIANT tests with $NUM_JOBS parallel jobs"
echo ""
echo "BUILDING"

# Don't clean coverage data here - it should be done once at the start
# This allows multiple test variants to accumulate coverage data
# Note: If running standalone, do 'make clean' and clean *.gcda *.gcno in tests/ and tests/c_unit/ first

rm -f "$BINARY"
make "$BINARY"

# Setup logfile
date=$(date +"%Y-%m-%d_%H-%M")

if [ -z "$LOGFILE" ]; then
  case "$VARIANT" in
    32bit) LOGFILE="log_${date}.log" ;;
    64bit) LOGFILE="log_64_${date}.log" ;;
    32bit_time) LOGFILE="log_sch_time${date}.log" ;;
    64bit_time) LOGFILE="log_64_time_${date}.log" ;;
    gc) LOGFILE="log_gc_${date}.log" ;;
    revgc) LOGFILE="log_revgc_${date}.log" ;;
  esac
fi

echo "PERFORMING $DESCRIPTION: $date"

# Build expected failures list
expected_fails=()

# Common expected failures for small heap sizes
if [ "$USE_TIMEOUT" = true ]; then
  expected_fails+=("$BINARY -t $timeout -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -s -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -s -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -i -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -i -s -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -i -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -i -s -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -t $timeout -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -t $timeout -i -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -t $timeout -s -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -t $timeout -i -s -h 512 tests/test_match_stress_2.lisp")
else
  expected_fails+=("$BINARY -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -s -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -s -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -i -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -i -s -h 1024 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -i -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -i -s -h 512 tests/test_take_iota_0.lisp")
  expected_fails+=("$BINARY -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -i -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -s -h 512 tests/test_match_stress_2.lisp")
  expected_fails+=("$BINARY -i -s -h 512 tests/test_match_stress_2.lisp")
fi

# Build test configuration
if [ "$USE_TIMEOUT" = true ]; then
  test_config=(
    "-t $timeout -h 32768"
    "-t $timeout -i -h 32768"
    "-t $timeout -s -h 32768"
    "-t $timeout -i -s -h 32768"
    "-t $timeout -h 16384"
    "-t $timeout -i -h 16384"
    "-t $timeout -s -h 16384"
    "-t $timeout -i -s -h 16384"
    "-t $timeout -h 8192"
    "-t $timeout -i -h 8192"
    "-t $timeout -s -h 8192"
    "-t $timeout -i -s -h 8192"
    "-t $timeout -h 4096"
    "-t $timeout -i -h 4096"
    "-t $timeout -s -h 4096"
    "-t $timeout -i -s -h 4096"
    "-t $timeout -h 2048"
    "-t $timeout -i -h 2048"
    "-t $timeout -s -h 2048"
    "-t $timeout -i -s -h 2048"
    "-t $timeout -h 1024"
    "-t $timeout -i -h 1024"
    "-t $timeout -s -h 1024"
    "-t $timeout -i -s -h 1024"
    "-t $timeout -h 512"
    "-t $timeout -i -h 512"
    "-t $timeout -s -h 512"
    "-t $timeout -i -s -h 512"
  )
else
  test_config=(
    "-h 32768"
    "-i -h 32768"
    "-s -h 32768"
    "-i -s -h 32768"
    "-h 16384"
    "-i -h 16384"
    "-s -h 16384"
    "-i -s -h 16384"
    "-h 8192"
    "-i -h 8192"
    "-s -h 8192"
    "-i -s -h 8192"
    "-h 4096"
    "-i -h 4096"
    "-s -h 4096"
    "-i -s -h 4096"
    "-h 2048"
    "-i -h 2048"
    "-s -h 2048"
    "-i -s -h 2048"
    "-h 1024"
    "-i -h 1024"
    "-s -h 1024"
    "-i -s -h 1024"
    "-h 512"
    "-i -h 512"
    "-s -h 512"
    "-i -s -h 512"
  )
fi

# Add architecture-specific expected failures
for conf in "${test_config[@]}" ; do
  expected_fails+=("$BINARY $conf tests/$EXCLUDE_TEST")
done

# Create temporary directory for parallel job outputs
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Function to run tests for a single configuration
run_config_tests() {
  local config_idx=$1
  local arg=$2
  local prg=$3

  # Create private output files for this configuration
  local result_file="$TEMP_DIR/results_${config_idx}.txt"
  local fails_file="$TEMP_DIR/fails_${config_idx}.txt"
  local log_file="$TEMP_DIR/log_${config_idx}.txt"

  local success_count=0
  local fail_count=0

  echo "Configuration: $arg" > "$result_file"

  for lisp in tests/*.lisp; do
    tmp_file=$(mktemp)
    ./$prg $arg $lisp > $tmp_file 2>&1
    result=$?

    if [ $result -eq 1 ]; then
      success_count=$((success_count+1))
    else
      # Record the failing test
      echo "$prg $arg $lisp" >> "$fails_file"
      fail_count=$((fail_count+1))

      # Log the failure
      echo "$lisp FAILED" >> "$log_file"
      cat $tmp_file >> "$log_file"
    fi
    rm $tmp_file
  done

  # Write summary for this configuration
  echo "SUCCESS:$success_count" >> "$result_file"
  echo "FAIL:$fail_count" >> "$result_file"
}

export -f run_config_tests
export timeout
export TEMP_DIR

# Run all configurations in parallel
config_idx=0
pids=()
for arg in "${test_config[@]}"; do
    # Launch background job
    run_config_tests $config_idx "$arg" "$BINARY" &
    pids+=($!)
    config_idx=$((config_idx+1))

    # Limit number of parallel jobs
    while [ $(jobs -r | wc -l) -ge $NUM_JOBS ]; do
        sleep 0.1
    done
done

# Wait for all background jobs to complete
echo "Waiting for all test configurations to complete..."
for pid in "${pids[@]}"; do
    wait $pid
done
echo "All parallel tests completed. Combining results..."

# Combine all results
success_count=0
fail_count=0
failing_tests=()

# Collect results from all configurations
for result_file in "$TEMP_DIR"/results_*.txt; do
  if [ -f "$result_file" ]; then
    success=$(grep "^SUCCESS:" "$result_file" | cut -d: -f2)
    fail=$(grep "^FAIL:" "$result_file" | cut -d: -f2)
    success_count=$((success_count + success))
    fail_count=$((fail_count + fail))
  fi
done

# Collect all failing tests
for fails_file in "$TEMP_DIR"/fails_*.txt; do
  if [ -f "$fails_file" ]; then
    while IFS= read -r line; do
      failing_tests+=("$line")
    done < "$fails_file"
  fi
done

# Combine all log files into the main logfile
for log_file in "$TEMP_DIR"/log_*.txt; do
    if [ -f "$log_file" ]; then
    cat "$log_file" >> "$LOGFILE"
  fi
done

# Check which failures are expected
expected_count=0

for (( i = 0; i < ${#failing_tests[@]}; i++ )); do
  expected=false
  for (( j = 0; j < ${#expected_fails[@]}; j++ )); do
    if [[ "${failing_tests[$i]}" == "${expected_fails[$j]}" ]]; then
      expected=true
    fi
  done
  if $expected ; then
    expected_count=$((expected_count+1))
    echo "(OK - expected to fail) ${failing_tests[$i]}"
  else
    echo "(FAILURE) ${failing_tests[$i]}"
  fi
done

echo "Tests passed: $success_count"
echo "Tests failed: $fail_count"
echo "Expected fails: $expected_count"
echo "Actual fails: $((fail_count - expected_count))"

# Generate coverage report if enabled (do this BEFORE checking for failures)
if [ "$COVERAGE_ENABLED" = true ] && [ -n "$COVERAGE_JSON" ]; then
  echo "Generating coverage report..."
  gcovr --filter ../src --gcov-ignore-parse-errors=negative_hits.warn --merge-mode-functions merge-use-line-max --json "$COVERAGE_JSON"
  if [ $? -eq 0 ]; then
    echo "Coverage report generated: $COVERAGE_JSON"
  else
    echo "WARNING: Coverage report generation failed"
  fi
fi

# Exit with error if there were unexpected failures
if [ $((fail_count - expected_count)) -gt 0 ]; then
  exit 1
fi
