#!/bin/bash

release=$1

if [ -z "$1" ]; then
    echo "No arg"
    exit 1
fi


reportdir=./test_reports/version_$release

echo $release
echo $reportdir

mkdir -p $reportdir
mkdir -p $reportdir/scan-build
mkdir -p $reportdir/cppcheck

release_readme="readme_${release}.md"

echo "# LispBM Release ${release} Test logs" > $reportdir/$release_readme
echo "" >> $reportdir/$release_readme

cd repl

cppcheck32log="../${reportdir}/cppcheck/cppcheck_32bit_${release}.txt"
cppcheck64log="../${reportdir}/cppcheck/cppcheck_64bit_${release}.txt"

./run_cppcheck.sh $cppcheck32log $cppcheck64log

cd ..

cd tests

ls -al tests/ > ../$reportdir/tests_list.txt

############################################################
# 32bit tests
unit_tests_log_file="unit_tests_log_${release}.txt"
failing_unit_tests_log_file="failing_unit_tests_log_${release}.txt"

./run_tests.sh ../$reportdir/$failing_unit_tests_log_file >> ../$reportdir/$unit_tests_log_file
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_log_file >> ../$reportdir/$release_readme

############################################################
# 32bit time based scheduler tests
unit_tests_time_log_file="unit_tests_log_${release}.txt"
failing_unit_tests_time_log_file="failing_unit_tests_log_${release}.txt"

./run_tests.sh ../$reportdir/$failing_unit_tests_time_log_file >> ../$reportdir/$unit_tests_time_log_file
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_time_log_file >> ../$reportdir/$release_readme

############################################################
# 64bit tests
unit_tests_64_log_file="unit_tests_log_64_${release}.txt"
failing_unit_tests_64_log_file="failing_unit_tests_log_64_${release}.txt"

./run_tests64.sh ../$reportdir/$failing_unit_tests_64_log_file >> ../$reportdir/$unit_tests_64_log_file
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_log_file >> ../$reportdir/$release_readme 

############################################################
# 64bit time based scheduler tests
unit_tests_64_time_log_file="unit_tests_log_64_${release}.txt"
failing_unit_tests_64_time_log_file="failing_unit_tests_log_64_${release}.txt"

./run_tests64.sh ../$reportdir/$failing_unit_tests_64_time_log_file >> ../$reportdir/$unit_tests_64_time_log_file
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_time_log_file >> ../$reportdir/$release_readme 

############################################################
# Always GC tests
gc_unit_tests_log_file="gc_unit_tests_log_${release}.txt"
failing_gc_unit_tests_log_file="failing_gc_unit_tests_log_${release}.txt"

./run_tests_gc.sh ../$reportdir/$failing_gc_unit_tests_log_file >> ../$reportdir/$gc_unit_tests_log_file
echo "" >> ../$reportdir/$release_readme
echo "## ALWAYS GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$gc_unit_tests_log_file >> ../$reportdir/$release_readme

############################################################
#Pointer reversal gc tests
revgc_unit_tests_log_file="revgc_unit_tests_log_${release}.txt"
failing_revgc_unit_tests_log_file="failing_revgc_unit_tests_log_${release}.txt"

./run_tests_gc_rev.sh ../$reportdir/$failing_revgc_unit_tests_log_file >> ../$reportdir/$revgc_unit_tests_log_file
echo "" >> ../$reportdir/$release_readme
echo "## POINTER REVERSAL GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$revgc_unit_tests_log_file >> ../$reportdir/$release_readme 

############################################################
# Run the 32bit tests for a coverage report.
./run_tests_cov.sh


cd ..
cp -r tests/coverage $reportdir/coverage


echo "" >> ./$reportdir/$release_readme
make clean
if command -v scan-build-19
then
    echo "## scan-build version 19" >> ./$reportdir/$release_readme
    scan-build-18 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt
else
    if command -v scan-build-18
    then
        echo "## scan-build version 18" >> ./$reportdir/$release_readme
        scan-build-18 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt
    else
        if command -v scan-build-14
        then
            echo "## scan-build version 14" >> ./$reportdir/$release_readme
            scan-build-14 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt
        else
            if command -v scan-build-10
            then
                echo "## scan-build version 10" >> ./$reportdir/$release_readme
                scan-build-10 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt
            else
                echo "scan-build could not be found"
                exit 1
            fi
        fi
    fi
fi
tail -n 1 $reportdir/scan_build_$release.txt >> ./$reportdir/$release_readme


make clean
infer run -- make

cp ./infer-out/report.txt $reportdir/infer_${release}.txt

echo "" >> ./$reportdir/$release_readme
echo "## INFER ISSUES" >> ./$reportdir/$release_readme
tail -n 3 $reportdir/infer_${release}.txt >> ./$reportdir/$release_readme
