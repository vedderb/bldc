#!/bin/bash

# Test 10 is semantically equivalent if the environment would have a binding of a.

# Array of test expressions
test_expressions=(
    '`(,@(list 1 2 3))'
    '`(a ,@(list 1 2) b)'
    '`(,@())'
    '`(1 ,@(list 2 3) 4)'
    '``(a ,,(+ 1 2))'
    '`(quote ,@(list 1 2))'
    '`(,@(cdr (quote (0 1 2 3))))'
    '`(list ,@(list 1 2 3))'
    "\`,'a"
    "\`\`,a"
    "\`,\`a"
    "\`(1 . 2)"
    "\`',(car ())"
    "\`(,1 ,2 . ,3)"
    "\`(,@nil ,1)"
    "\`(1 2 ,@() ,@() 3)"
    "\`\`(,,@(list 0 1 2))"
    "\`\`(,,@(cdr '(0 1 2 3)) ,4)"
    "\`\`(1 2 ,,@() ,,@())"
    "\`\`(a ,,(+ 1 2) ,(+ 3 4))"
    "\`5"
    "\`,5"
    "(let ((x 5)) \`(let ((x ,(+ x 10))) \`(list ,,x ,x)))"
    "\`,@0"
    "\`(1 2 ,@'(3 . 4))"
    "\`(1 2 ,@(list 3 4 5) ,@(list 6 7 8) 9 10)"
    "\`(1 2 ,@'() ,@'() 3)"
    "\`\`(1 2 ,,@'() ,,@'())"
    "\`(list 1 \`(,@(list 1 2 3) \`(,@(list 4 5 6))))"
    
)

# Array of expressions known to have expected differences
# These won't count as test failures
expected_differences=(
    '`(,@())'  # Guile errors, LispBM returns nil
)

total_tests=0
matching_tests=0
differing_tests=0
expected_diffs=0

echo "Running quasiquote comparison tests between Guile and LispBM"
echo "============================================================"
echo

is_expected_difference() {
    local expr="$1"
    for expected in "${expected_differences[@]}"; do
        if [ "$expr" = "$expected" ]; then
            return 0
        fi
    done
    return 1
}

test_expression() {
    local expr="$1"
    total_tests=$((total_tests + 1))
    
    echo "Test $total_tests: $expr"

    #emacs lisp test
    # echo -n "  Emacs:  "
    # if emacs_result=$(emacs --batch --eval "(prin1 $expr)" 2>/dev/null); then
    #     echo "$emacs_result"
    # else
    #     emacs_result="ERROR"
    #     echo "ERROR or unsupported"
    # fi

    # Guile test
    echo -n "  Guile:  "
    if guile_result=$(guile -c "(display $expr)" 2>/dev/null); then
        echo "$guile_result"
    else
        guile_result="ERROR"
        echo "ERROR or unsupported"
    fi
    
    # LispBM test
    echo -n "  LispBM: "
    if lbm_result=$(cd /home/joels/Current/lispbm && ./repl/repl -e "$expr" --terminate 2>/dev/null | grep -v "Image\|version\|creating\|Lisp REPL\|Type\|Goodbye" | tail -1 | sed 's/^> //'); then
        echo "$lbm_result"
    else
        lbm_result="ERROR"
        echo "ERROR or unsupported"
    fi
    
    # Compare results
    if [ "$guile_result" = "$lbm_result" ]; then
        echo "  Result: MATCH"
        matching_tests=$((matching_tests + 1))
    elif [ "$guile_result" = "ERROR" ] || [ "$lbm_result" = "ERROR" ]; then
        if is_expected_difference "$expr"; then
            echo "  Result: EXPECTED DIFFER (Known difference)"
            expected_diffs=$((expected_diffs + 1))
        else
            echo "  Result: DIFFER (Error in one system)"
            differing_tests=$((differing_tests + 1))
        fi
    else
        echo "  Result: DIFFER - Testing semantic equivalence..."
        
        # Test if both expressions evaluate to the same result
        echo -n "  Guile eval:  "
        if guile_eval=$(guile -c "(display (eval '$guile_result (interaction-environment)))" 2>/dev/null); then
            echo "$guile_eval"
        else
            guile_eval="ERROR"
            echo "ERROR"
        fi
        
        echo -n "  LispBM eval: "
        if lbm_eval=$(cd /home/joels/Current/lispbm && ./repl/repl -e "(eval '$lbm_result)" --terminate 2>/dev/null | grep -v "Image\|version\|creating\|Lisp REPL\|Type\|Goodbye" | tail -1 | sed 's/^> //'); then
            echo "$lbm_eval"
        else
            lbm_eval="ERROR"
            echo "ERROR"
        fi
        
        # Compare evaluated results
        if [ "$guile_eval" = "$lbm_eval" ] && [ "$guile_eval" != "ERROR" ]; then
            echo "  Result: SEMANTIC MATCH"
            matching_tests=$((matching_tests + 1))
        else
            if is_expected_difference "$expr"; then
                echo "  Result: EXPECTED SEMANTIC DIFFER (Known difference)"
                expected_diffs=$((expected_diffs + 1))
            else
                echo "  Result: SEMANTIC DIFFER"
                differing_tests=$((differing_tests + 1))
            fi
        fi
    fi
    echo
}

# Run all tests
for expr in "${test_expressions[@]}"; do
    test_expression "$expr"
done

# Summary
echo "============================================================"
echo "Summary:"
echo "  Total tests: $total_tests"
echo "  Matching: $matching_tests"
echo "  Expected differences: $expected_diffs"
echo "  Unexpected differences: $differing_tests"

if [ $differing_tests -eq 0 ]; then
    echo "  Result: ALL TESTS PASSED"
    if [ $expected_diffs -gt 0 ]; then
        echo "  Expected diffs: $expected_diffs"
    fi
    exit 0
else
    echo "  Result: $differing_tests TESTS FAILED"
    exit 1
fi

