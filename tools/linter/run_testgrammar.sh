#!/bin/bash

failing_tests=()
fail_count=0;

for i in ../../tests/tests/*.lisp; do
    ./Testgrammar $i
    if [ $? -eq 1 ]
    then
       echo Test $i failed
       fail_count=$((fail_count+1))
       failing_tests+=("$i")
    fi
done

for i in ../../tests/repl_tests/*.lisp; do
    ./Testgrammar $i
    if [ $? -eq 1 ]
    then
       echo Test $i failed
       fail_count=$((fail_count+1))
       failing_tests+=("$i")
    fi
done

for i in ../../tests/sdl_tests/*.lisp; do
    ./Testgrammar $i
    if [ $? -eq 1 ]
    then
       echo Test $i failed
       fail_count=$((fail_count+1))
       failing_tests+=("$i")
    fi
done

for i in ../../tests/image_tests/*.lisp; do
    ./Testgrammar $i
    if [ $? -eq 1 ]
    then
       echo Test $i failed
       fail_count=$((fail_count+1))
       failing_tests+=("$i")
    fi
done


for (( i = 0; i < ${#failing_tests[@]}; i++ ))
do
    echo "(FAILURE)" ${failing_tests[$i]}
done

echo Failed tests: $fail_count
