#!/bin/bash

echo "BUILDING"

make clean
make allrev

echo "PERFORMING TESTS:"

expected_fails=("test_lisp_code_cps -h 1024 test_take_iota_0.lisp"
                "test_lisp_code_cps -s -h 1024 test_take_iota_0.lisp"
                "test_lisp_code_cps -h 512 test_take_iota_0.lisp"
                "test_lisp_code_cps -s -h 512 test_take_iota_0.lisp"
                "test_lisp_code_cps -i -h 1024 test_take_iota_0.lisp"
                "test_lisp_code_cps -i -s -h 1024 test_take_iota_0.lisp"
                "test_lisp_code_cps -i -h 512 test_take_iota_0.lisp"
                "test_lisp_code_cps -i -s -h 512 test_take_iota_0.lisp"
               )


success_count=0
fail_count=0
failing_tests=()
result=0

for exe in *.exe; do

    if [ "$exe" = "test_gensym.exe" ]; then
        continue
    fi

    ./$exe

    result=$?

    echo "------------------------------------------------------------"
    if [ $result -eq 1 ]
    then
        success_count=$((success_count+1))
        echo $exe SUCCESS
    else

        fail_count=$((fail_count+1))
        echo $exe FAILED
    fi
    echo "------------------------------------------------------------"
done

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

#"test_lisp_code_cps_nc"
for prg in "test_lisp_code_cps" ; do
    for arg in "${test_config[@]}"; do
        for lisp in *.lisp; do

            ./$prg $arg $lisp

            result=$?

            echo "------------------------------------------------------------"
            #echo $arg
            if [ $result -eq 1 ]
            then
                success_count=$((success_count+1))
                echo $lisp SUCCESS
            else

                #!/bin/bash
                # foo=('foo bar' 'foo baz' 'bar baz')
                # bar=$(printf ",%s" "${foo[@]}")
                # bar=${bar:1}

                # echo $bar
                str=$(printf "%s " "$prg $arg $lisp")
                #echo $str

                failing_tests+=("$prg $arg $lisp")
                fail_count=$((fail_count+1))
                #echo $failing_tests

                echo $lisp FAILED
            fi
            echo "------------------------------------------------------------"
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
