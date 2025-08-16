#!/bin/bash

echo "BUILDING C UNIT TESTS"

cd c_unit

# Clean previous builds
rm -rf coverage
mkdir coverage
rm -f *.gcno
rm -f *.gcda
rm -f *.exe

# Build all C unit tests
make all

date=$(date +"%Y-%m-%d_%H-%M")
logfile="log_c_unit_${date}.log"

if [ -n "$1" ]; then
   logfile=$1
fi

echo "PERFORMING C UNIT TESTS: " $date

# List of tests that are expected to fail
expected_fails=("test_failure.exe")

success_count=0
fail_count=0
failing_tests=()

# Run all .exe files in the c_unit directory
for exe in *.exe; do
    if [ -f "$exe" ]; then
        echo "Running: $exe"
        tmp_file=$(mktemp)
        ./$exe > $tmp_file 2>&1
        result=$?
        
        # Check if the test printed "SUCCESS"
        if [ $result -eq 0 ] && grep -q "SUCCESS" $tmp_file; then
            echo "$exe: PASSED"
            success_count=$((success_count+1))
        else
            echo "$exe: FAILED"
            failing_tests+=("$exe")
            fail_count=$((fail_count+1))
            
            # Log the failure details
            echo "=== FAILURE: $exe ===" >> $logfile
            cat $tmp_file >> $logfile
            echo "" >> $logfile
        fi
        rm $tmp_file
    fi
done

# Check which failures were expected
expected_count=0

for (( i = 0; i < ${#failing_tests[@]}; i++ ))
do
  expected=false
  for (( j = 0; j < ${#expected_fails[@]}; j++))
  do
      if [[ "${failing_tests[$i]}" == "${expected_fails[$j]}" ]] ;
      then
          expected=true
      fi
  done
  if $expected ; then
      expected_count=$((expected_count+1))
      echo "(OK - expected to fail)" ${failing_tests[$i]}
  else
      echo "(FAILURE)" ${failing_tests[$i]}
  fi
done

echo ""
echo "C Unit Tests Summary:"
echo "Tests passed: $success_count"
echo "Tests failed: $fail_count"
echo "Expected fails: $expected_count"
echo "Actual fails: $((fail_count - expected_count))"

# Generate coverage report if gcovr is available
if command -v gcovr &> /dev/null; then
    echo "Generating coverage report..."
    gcovr --filter ../../src --gcov-ignore-parse-errors=negative_hits.warn --json c_unit_tests_cov.json
fi

cd ..

if [ $((fail_count - expected_count)) -gt 0 ]; then
    exit 1
else
    echo "All C unit tests passed!"
    exit 0
fi
