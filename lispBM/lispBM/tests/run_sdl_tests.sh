#!/bin/bash

success_count=0
fail_count=0
expected_fail_count=0
unexpected_fail_count=0
failing_tests=()

# Tests that are expected to fail
expected_fails=("sdl_tests/test_failure.lisp")

timeout_val=10

cd ../repl
make clean
make cov
cd ../tests

date=$(date +"%Y-%m-%d_%H-%M")
logfile="log_sdl_tests_${date}.log"

for fn in sdl_tests/*.lisp
do
    ok=false
    fail_timeout=false;
    timeout $timeout_val ../repl/repl_cov -M 11 --terminate -s $fn | grep 'SUCCESS' &> /dev/null
    res=$?
    if [ $res == 124 ]; then
        fail_timeout=true;
    else
        if [ $res == 0 ]; then
            ok=true
        fi
    fi

    # Check if this test is expected to fail
    is_expected_fail=false
    for expected_fail in "${expected_fails[@]}"; do
        if [ "$fn" = "$expected_fail" ]; then
            is_expected_fail=true
            break
        fi
    done

    if $ok; then
        if $is_expected_fail; then
            echo "UNEXPECTED PASS: $fn (expected to fail)"
            echo "UNEXPECTED PASS: $fn (expected to fail)" >> $logfile
            unexpected_fail_count=$((unexpected_fail_count+1))
        else
            success_count=$((success_count+1))
            echo "Test OK: $fn"
        fi
    else
        if $is_expected_fail; then
            expected_fail_count=$((expected_fail_count+1))
            if [ $fail_timeout == true ]; then
                echo "Expected timeout: $fn"
                echo "Expected timeout: $fn" >> $logfile
            else
                echo "Expected fail: $fn"
                echo "Expected fail: $fn" >> $logfile
            fi
        else
            unexpected_fail_count=$((unexpected_fail_count+1))
            failing_tests+=("$fn")
            if [ $fail_timeout == true ]; then
                echo "Timeout: $fn"
                echo "Timeout: $fn" >> $logfile
            else
                echo "FAIL: $fn"
                echo "FAIL: $fn" >> $logfile
            fi
        fi
    fi
done

echo Tests passed: $success_count
echo Expected failures: $expected_fail_count
echo Unexpected failures: $unexpected_fail_count

if [ $unexpected_fail_count -eq 0 ]; then
    echo "All tests completed as expected!"
else
    echo "There were $unexpected_fail_count unexpected test failures."
fi


## Go to repl directory and collect the coverage data
cd ../repl
rm -f sdl_tests_cov.json
gcovr --filter ../src --gcov-ignore-parse-errors=negative_hits.warn --json sdl_tests_cov.json
cd ../tests

