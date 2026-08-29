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
make FEATURES="sdl coverage"
cd ../tests

rm -rf sdl_tests/png_out
mkdir -p sdl_tests/png_out

date=$(date +"%Y-%m-%d_%H-%M")
logfile="log_sdl_tests_${date}.log"

for fn in sdl_tests/*.lisp
do
    ok=false
    fail_timeout=false;
    timeout $timeout_val ../repl/repl -M $((1024*1024)) --terminate -s $fn | grep 'SUCCESS' &> /dev/null
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


## compare rendered PNGs to the GOLD.
#
#  If there is a difference between the current render and the GOLD then
#  one has to inspect the visual difference and check if it is better, a
#  worthwhile tradeoff or a bug.

echo ""
echo "Comparing rendered images against gold references:"

if command -v compare >/dev/null 2>&1; then
    mkdir -p sdl_tests/png_diff
    gold_match=0
    gold_diff=0
    gold_missing=0
    diff_list=()

    for f in sdl_tests/png_out/*.png; do
        name=$(basename "$f")
        gold_file="sdl_tests/gold/$name"
        if [ ! -f "$gold_file" ]; then
            echo "NO GOLD REFERENCE: $name"
            gold_missing=$((gold_missing+1))
            continue
        fi

        ae=$(compare -metric AE "$gold_file" "$f" "sdl_tests/png_diff/$name" 2>&1 >/dev/null)
        if [ "$ae" == "0" ]; then
            gold_match=$((gold_match+1))
            rm -f "sdl_tests/png_diff/$name"
        else
            echo "DIFFERS FROM GOLD: $name (differing pixels: $ae)"
            gold_diff=$((gold_diff+1))
            diff_list+=("$name")
        fi
    done

    echo ""
    echo "Gold comparison: $gold_match match, $gold_diff differ, $gold_missing without a gold reference"
    if [ $gold_diff -gt 0 ]; then
        echo "Differing images (gold: sdl_tests/gold/<name>, new: sdl_tests/png_out/<name>, highlighted diff: sdl_tests/png_diff/<name>):"
        for name in "${diff_list[@]}"; do
            echo "  $name"
        done
    fi
else
    echo "ImageMagick 'compare' not found; skipping gold comparison."
fi

## Go to repl directory and collect the coverage data
cd ../repl
rm -f sdl_tests_cov.json
gcovr --filter ../src --gcov-ignore-parse-errors=negative_hits.warn --json sdl_tests_cov.json
cd ../tests

