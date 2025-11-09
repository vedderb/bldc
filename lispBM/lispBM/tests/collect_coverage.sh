
mkdir -p coverage
rm -f coverage/*

cov_files=("tests_cov_32.json"
           "tests_cov_64.json"
           "../repl/repl_tests_cov.json"
           "../repl/image_tests_cov.json"
           "./c_unit/c_unit_tests_cov.json"
           "../repl/sdl_tests_cov.json")

echo "Checking if cov files exist:"
for c in "${cov_files[@]}" ; do
    if [ -e $c ]; then
        echo "  File $c exists"
    else
        echo "  WARNING! File $c does not exist"
    fi
done

echo ""
echo "Collect cov file meta data:"
for c in "${cov_files[@]}" ; do
    if [ -e $c ]; then
        echo " "$c""
        echo "    Modified: $(stat -c '%y' "$c")"
        echo "    Changed:  $(stat -c '%z' "$c")"
    fi
done

tc_args=()
for c in "${cov_files[@]}"; do
    tc_args+=("--json-add-tracefile" "$c")
done

echo ""
echo "Running gcovr"
gcovr --filter ../src --merge-mode-functions merge-use-line-max "${tc_args[@]}" --html-details coverage/coverage.html
