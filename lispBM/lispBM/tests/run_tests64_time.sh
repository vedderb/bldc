#!/bin/bash

echo "BUILDING"

rm -f test_lisp_code_cps_64_time
make test_lisp_code_cps_64_time


date=$(date +"%Y-%m-%d_%H-%M")
logfile="log_64_time_${date}.log"

if [ -n "$1" ]; then
   logfile=$1
fi

echo "PERFORMING 64BIT TESTS: " $date


expected_fails=("test_lisp_code_cps_64_time -h 1024 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -s -h 1024 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -h 512 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -s -h 512 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -i -h 1024 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -i -s -h 1024 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -i -h 512 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -i -s -h 512 tests/test_take_iota_0.lisp"
                "test_lisp_code_cps_64_time -h 512 tests/test_match_stress_2.lisp"
		"test_lisp_code_cps_64_time -i -h 512 tests/test_match_stress_2.lisp"
		"test_lisp_code_cps_64_time -s -h 512 tests/test_match_stress_2.lisp"
		"test_lisp_code_cps_64_time -i -s -h 512 tests/test_match_stress_2.lisp"
              )

success_count=0
fail_count=0
failing_tests=()
result=0

test_config=("-h 32768"
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
              "-i -s -h 512")

for conf in "${test_config[@]}" ; do
    expected_fails+=("test_lisp_code_cps_64_time $conf tests/test_is_32bit.lisp")
done


for prg in "test_lisp_code_cps_64_time" ; do
    for arg in "${test_config[@]}"; do
        echo "Configuration: " $arg
        for lisp in tests/*.lisp; do
            tmp_file=$(mktemp)
            ./$prg $arg $lisp > $tmp_file
            result=$?
            if [ $result -eq 1 ]
            then
                success_count=$((success_count+1))
            else
                failing_tests+=("$prg $arg $lisp")
                fail_count=$((fail_count+1))

                echo $lisp FAILED
                cat $tmp_file >> $logfile
            fi
            rm $tmp_file
        done
    done
done

# echo -e $failing_tests

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


echo Tests passed: $success_count
echo Tests failed: $fail_count
echo Expected fails: $expected_count
echo Actual fails: $((fail_count - expected_count))

if [ $((fail_count - expected_count)) -gt 0 ]
then
    exit 1
fi
