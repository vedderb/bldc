#!/bin/bash

release=$1

if [ -z "$1" ]; then
    echo "No arg"
    exit 1
fi

START_TIME=$(date +%s)


print_elapsed () {
    ELAPSED=$(($(date +%s) - START_TIME))
    local elapsed_msg
    elapsed_msg=$(printf "elapsed: %s\n\n" "$(date -d@$ELAPSED -u +%H\ hours\ %M\ min\ %S\ sec)")
    echo "$elapsed_msg"
    if [ -n "$1" ]; then
        echo "$elapsed_msg" >> "$1"
    fi
}

directories=("tests"
             "tests/c_unit"
             "repl")

for d in "${directories[@]}"; do
    echo "Running clean in: "$d""
    (cd $d && make clean)
done

reportdir=./test_reports/version_$release

echo Logging $release tests in $reportdir

mkdir -p $reportdir
mkdir -p $reportdir/scan-build
mkdir -p $reportdir/cppcheck

release_readme="readme_${release}.md"

echo "# LispBM Release ${release} Test logs" > $reportdir/$release_readme
echo "" >> $reportdir/$release_readme
echo "## Build Machine Information" >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme
echo "- **Date**: $(date '+%Y-%m-%d %H:%M:%S %Z')" >> $reportdir/$release_readme
echo "- **Hostname**: $(hostname)" >> $reportdir/$release_readme
echo "- **OS**: $(uname -s) $(uname -r)" >> $reportdir/$release_readme
echo "- **Architecture**: $(uname -m)" >> $reportdir/$release_readme
echo "- **CPU**: $(grep 'model name' /proc/cpuinfo | head -1 | cut -d':' -f2 | xargs)" >> $reportdir/$release_readme
echo "- **CPU Cores**: $(nproc)" >> $reportdir/$release_readme
echo "- **Memory**: $(free -h | grep Mem | awk '{print $2}')" >> $reportdir/$release_readme
echo "- **GCC Version**: $(gcc --version | head -1)" >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme

echo "" >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme
echo "## Tools versions" >> $reportdir/$release_readme
gcovr --version >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme
cppcheck --version >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme
infer --version >> $reportdir/$release_readme
echo "" >> $reportdir/$release_readme

cd repl

cppcheck32log="../${reportdir}/cppcheck/cppcheck_32bit_${release}.txt"
cppcheck64log="../${reportdir}/cppcheck/cppcheck_64bit_${release}.txt"

echo "Running CPPCHECK"
./run_cppcheck.sh $cppcheck32log $cppcheck64log &> /dev/null

echo "## CPPCHECK" >> ../$reportdir/$release_readme
echo "" >> ../$reportdir/$release_readme
echo "See the cppcheck directory for results." >> ../$reportdir/$release_readme
print_elapsed ../$reportdir/$release_readme

cd ..

cd tests

ls -al tests/ > ../$reportdir/tests_list.txt

############################################################
# 32bit tests
unit_tests_log_file="32bit_unit_tests_log_${release}.txt"
failing_unit_tests_log_file="failing_32bit_unit_tests_log_${release}.txt"

echo "Running 32bit tests"

#./run_tests.sh ../$reportdir/$failing_unit_tests_log_file >> ../$reportdir/$unit_tests_log_file 2> /dev/null
./run_tests_generic_parallel.sh 32bit ../$reportdir/$failing_unit_tests_log_file >> ../$reportdir/$unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
# 32bit time based scheduler tests
unit_tests_time_log_file="32bit_time_unit_tests_log_${release}.txt"
failing_unit_tests_time_log_file="failing_32bit_time_unit_tests_log_${release}.txt"

echo "Running 32bit tests with time scheduler"

#./run_tests_time.sh ../$reportdir/$failing_unit_tests_time_log_file >> ../$reportdir/$unit_tests_time_log_file 2> /dev/null
./run_tests_generic_parallel.sh 32bit_time ../$reportdir/$failing_unit_tests_time_log_file >> ../$reportdir/$unit_tests_time_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_time_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
# 64bit tests
unit_tests_64_log_file="64bit_unit_tests_log_${release}.txt"
failing_unit_tests_64_log_file="failing_64bit_unit_tests_log_${release}.txt"

echo "Running 64bit tests"

#./run_tests64.sh ../$reportdir/$failing_unit_tests_64_log_file >> ../$reportdir/$unit_tests_64_log_file 2> /dev/null
./run_tests_generic_parallel.sh 64bit ../$reportdir/$failing_unit_tests_64_log_file >> ../$reportdir/$unit_tests_64_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
# 64bit time based scheduler tests
unit_tests_64_time_log_file="64bit_time_unit_tests_log_${release}.txt"
failing_unit_tests_64_time_log_file="failing_64bit_time_unit_tests_log_${release}.txt"

echo "Running 64bit tests with time scheduler"

#./run_tests64_time.sh ../$reportdir/$failing_unit_tests_64_time_log_file >> ../$reportdir/$unit_tests_64_time_log_file 2> /dev/null
./run_tests_generic_parallel.sh 64bit_time ../$reportdir/$failing_unit_tests_64_time_log_file >> ../$reportdir/$unit_tests_64_time_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$unit_tests_64_time_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
# Always GC tests
gc_unit_tests_log_file="gc_unit_tests_log_${release}.txt"
failing_gc_unit_tests_log_file="failing_gc_unit_tests_log_${release}.txt"

echo "Running always gc tests"

#./run_tests_gc.sh ../$reportdir/$failing_gc_unit_tests_log_file >> ../$reportdir/$gc_unit_tests_log_file 2> /dev/null
./run_tests_generic_parallel.sh gc ../$reportdir/$failing_gc_unit_tests_log_file >> ../$reportdir/$gc_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## ALWAYS GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$gc_unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
#Pointer reversal gc tests
revgc_unit_tests_log_file="revgc_unit_tests_log_${release}.txt"
failing_revgc_unit_tests_log_file="failing_revgc_unit_tests_log_${release}.txt"

echo "Running ptr-rev gc tests"

#./run_tests_gc_rev.sh ../$reportdir/$failing_revgc_unit_tests_log_file >> ../$reportdir/$revgc_unit_tests_log_file 2> /dev/null
./run_tests_generic_parallel.sh revgc ../$reportdir/$failing_revgc_unit_tests_log_file >> ../$reportdir/$revgc_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## POINTER REVERSAL GC UNIT TESTS RESULTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$revgc_unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
#

echo "Running REPL based tests"

failing_repl_tests_log_file="failing_repl_tests_log_${release}.txt"
repl_tests_log_file="repl_tests_log_${release}.txt"
./run_repl_tests.sh ../$reportdir/$failing_repl_tests_log_file >> ../$reportdir/$repl_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## REPL TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$repl_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
#

echo "Running Image tests"

failing_image_tests_log_file="failing_image_tests_log_${release}.txt"
image_tests_log_file="image_tests_log_${release}.txt"
./run_image_tests.sh ../$reportdir/$failing_image_tests_log_file >> ../$reportdir/$image_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## IMAGE TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$image_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
#

echo "Running SDL tests"

failing_sdl_tests_log_file="failing_sdl_tests_log_${release}.txt"
sdl_tests_log_file="sdl_tests_log_${release}.txt"
./run_sdl_tests.sh ../$reportdir/$failing_sdl_tests_log_file >> ../$reportdir/$sdl_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## SDL TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$sdl_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme

############################################################
#

echo "Running c_unit tests"

failing_c_unit_tests_log_file="failing_c_unit_tests_log_${release}.txt"
c_unit_tests_log_file="c_unit_tests_log_${release}.txt"
./run_c_unit.sh ../$reportdir/$failing_c_unit_tests_log_file >> ../$reportdir/$c_unit_tests_log_file 2> /dev/null
echo "" >> ../$reportdir/$release_readme
echo "## C UNIT TESTS" >> ../$reportdir/$release_readme
tail -n 4 ../$reportdir/$c_unit_tests_log_file >> ../$reportdir/$release_readme

print_elapsed ../$reportdir/$release_readme


############################################################
# Collect coverage

echo "Gathering coverage data and assembling report"

echo "" >> ../$reportdir/$release_readme
echo "## Coverage collection" >> ../$reportdir/$release_readme

./collect_coverage.sh  >> ../$reportdir/collect_coverage_log_${release}.txt

cd ..
cp -r tests/coverage $reportdir/coverage

print_elapsed ./$reportdir/$release_readme

############################################################

echo "Running scan-build"

echo "" >> ./$reportdir/$release_readme
make clean &> /dev/null
if command -v scan-build-19
then
    echo "## scan-build version 19" >> ./$reportdir/$release_readme
    scan-build-19 -o ./$reportdir/scan-build make -j4 >> ./$reportdir/scan_build_$release.txt 2> /dev/null
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

print_elapsed ./$reportdir/$release_readme

############################################################

echo "Running Infer"

make clean &> /dev/null
infer run -- make &> /dev/null

cp ./infer-out/report.txt $reportdir/infer_${release}.txt

echo "" >> ./$reportdir/$release_readme
echo "## INFER ISSUES" >> ./$reportdir/$release_readme
tail -n 3 $reportdir/infer_${release}.txt >> ./$reportdir/$release_readme

print_elapsed ./$reportdir/$release_readme

echo "DONE!"
