#!/bin/bash

release=$1

if [ -z "$1" ]; then
    echo "No arg"
    exit 1
fi

START_TIME=$(date +%s)


print_elapsed () {
    ELAPSED=$(($(date +%s) - START_TIME))
    printf "elapsed: %s\n\n" "$(date -d@$ELAPSED -u +%H\ hours\ %M\ min\ %S\ sec)"
}


reportdir=./test_reports/version_$release

echo Logging $release tests in $reportdir

mkdir -p $reportdir
mkdir -p $reportdir/scan-build
mkdir -p $reportdir/cppcheck

release_readme="readme_${release}.md"

echo "# LispBM Release ${release} Test logs" > $reportdir/$release_readme
echo "" >> $reportdir/$release_readme

cd repl

cppcheck32log="../${reportdir}/cppcheck/cppcheck_32bit_${release}.txt"
cppcheck64log="../${reportdir}/cppcheck/cppcheck_64bit_${release}.txt"


echo "Running CPPCHECK"
./run_cppcheck.sh $cppcheck32log $cppcheck64log &> /dev/null

print_elapsed

cd ..

cd tests

ls -al tests/ > ../$reportdir/tests_list.txt

############################################################
# 32bit tests
unit_tests_log_file="32bit_unit_tests_log_${release}.txt"
failing_unit_tests_log_file="failing_32bit_unit_tests_log_${release}.txt"

echo "Running 32bit tests"

./run_tests.sh ../$reportdir/$failing_unit_tests_log_file >> ../$reportdir/$unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
# 32bit time based scheduler tests
unit_tests_time_log_file="32bit_time_unit_tests_log_${release}.txt"
failing_unit_tests_time_log_file="failing_32bit_time_unit_tests_log_${release}.txt"

echo "Running 32bit tests with time scheduler"

./run_tests_time.sh ../$reportdir/$failing_unit_tests_time_log_file >> ../$reportdir/$unit_tests_time_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_time_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
# 64bit tests
unit_tests_64_log_file="64bit_unit_tests_log_${release}.txt"
failing_unit_tests_64_log_file="failing_64bit_unit_tests_log_${release}.txt"

echo "Running 64bit tests"

./run_tests64.sh ../$reportdir/$failing_unit_tests_64_log_file >> ../$reportdir/$unit_tests_64_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_log_file >> ../$reportdir/$release_readme 

print_elapsed

############################################################
# 64bit time based scheduler tests
unit_tests_64_time_log_file="64bit_time_unit_tests_log_${release}.txt"
failing_unit_tests_64_time_log_file="failing_64bit_time_unit_tests_log_${release}.txt"

echo "Running 64bit tests with time scheduler"

./run_tests64_time.sh ../$reportdir/$failing_unit_tests_64_time_log_file >> ../$reportdir/$unit_tests_64_time_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_time_log_file >> ../$reportdir/$release_readme 

print_elapsed

############################################################
# Always GC tests
gc_unit_tests_log_file="gc_unit_tests_log_${release}.txt"
failing_gc_unit_tests_log_file="failing_gc_unit_tests_log_${release}.txt"

echo "Running always gc tests"

./run_tests_gc.sh ../$reportdir/$failing_gc_unit_tests_log_file >> ../$reportdir/$gc_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## ALWAYS GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$gc_unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
#Pointer reversal gc tests
revgc_unit_tests_log_file="revgc_unit_tests_log_${release}.txt"
failing_revgc_unit_tests_log_file="failing_revgc_unit_tests_log_${release}.txt"

echo "Running ptr-rev gc tests"

./run_tests_gc_rev.sh ../$reportdir/$failing_revgc_unit_tests_log_file >> ../$reportdir/$revgc_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## POINTER REVERSAL GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$revgc_unit_tests_log_file >> ../$reportdir/$release_readme 

print_elapsed

# ############################################################
# # Run the 32bit tests for a coverage report.
# echo "Collecting coverage data for tests"

# ./run_tests_cov.sh &> /dev/null

# print_elapsed

############################################################
#

echo "Running REPL based tests"

failing_repl_tests_log_file="failing_repl_tests_log_${release}.txt"
repl_tests_log_file="repl_tests_log_${release}.txt"
./run_repl_tests.sh ../$reportdir/$failing_repl_tests_log_file >> ../$reportdir/$repl_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## REPL TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$repl_tests_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
#

echo "Running Image tests"

failing_image_tests_log_file="failing_image_tests_log_${release}.txt"
image_tests_log_file="image_tests_log_${release}.txt"
./run_image_tests.sh ../$reportdir/$failing_image_tests_log_file >> ../$reportdir/$image_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## IMAGE TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$image_tests_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
#

echo "Running SDL tests"

failing_sdl_tests_log_file="failing_sdl_tests_log_${release}.txt"
sdl_tests_log_file="sdl_tests_log_${release}.txt"
./run_sdl_tests.sh ../$reportdir/$failing_sdl_tests_log_file >> ../$reportdir/$sdl_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## SDL TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$sdl_tests_log_file >> ../$reportdir/$release_readme

print_elapsed

############################################################
#

echo "Running c_unit tests"

failing_c_unit_tests_log_file="failing_c_unit_tests_log_${release}.txt"
c_unit_tests_log_file="c_unit_tests_log_${release}.txt"
./run_c_unit.sh ../$reportdir/$failing_c_unit_tests_log_file >> ../$reportdir/$c_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## C UNIT TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$c_unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed


############################################################
# Collect coverage

echo "Gathering coverage data and assembling report"

./collect_coverage.sh

cd ..
cp -r tests/coverage $reportdir/coverage

print_elapsed

############################################################

echo "Running scan-build"

echo "" >> ./$reportdir/$release_readme
make clean &> /dev/null
if command -v scan-build-19
then
    echo "## scan-build version 19" >> ./$reportdir/$release_readme
    scan-build-18 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt 2> /dev/null
else
    if command -v scan-build-18
    then
        echo "## scan-build version 18" >> ./$reportdir/$release_readme
        scan-build-18 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt 2> /dev/null
    else
        if command -v scan-build-14
        then
            echo "## scan-build version 14" >> ./$reportdir/$release_readme
            scan-build-14 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt 2> /dev/null
        else
            if command -v scan-build-10
            then
                echo "## scan-build version 10" >> ./$reportdir/$release_readme
                scan-build-10 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt 2> /dev/null
            else
                echo "scan-build could not be found"
                exit 1
            fi
        fi
    fi
fi
tail -n 1 $reportdir/scan_build_$release.txt >> ./$reportdir/$release_readme

print_elapsed

############################################################

echo "Running Infer"

make clean &> /dev/null
infer run -- make &> /dev/null

cp ./infer-out/report.txt $reportdir/infer_${release}.txt

echo "" >> ./$reportdir/$release_readme
echo "## INFER ISSUES" >> ./$reportdir/$release_readme
tail -n 3 $reportdir/infer_${release}.txt >> ./$reportdir/$release_readme

print_elapsed

echo "DONE!"
