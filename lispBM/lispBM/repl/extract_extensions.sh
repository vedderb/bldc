#!/bin/bash
# extract_extensions.sh

# Get the name parameter (default to "name" if not provided)
name_param="${1:-name}"

# Extract functions that return lbm_value and take (lbm_value *args, lbm_uint argn)
awk -v name="$name_param" '
BEGIN {
    print "#include \"lispbm.h\""
    print ""
    print "// Function stubs:"
    print ""
}

/^[[:space:]]*(static[[:space:]]+)?lbm_value[[:space:]]+[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*\([[:space:]]*lbm_value[[:space:]]*\*[[:space:]]*args[[:space:]]*,[[:space:]]*lbm_uint[[:space:]]+argn[[:space:]]*\)/ {
    # Found function declaration, capture it
    func_line = $0
    
    # Extract function name
    match(func_line, /(static[[:space:]]+)?lbm_value[[:space:]]+([a-zA-Z_][a-zA-Z0-9_]*)[[:space:]]*\(/, name_match)
    func_name = name_match[2]
    
    # Generate lisp function name: remove "ext_" prefix and convert _ to -
    lisp_name = func_name
    if (match(lisp_name, /^ext_/)) {
        lisp_name = substr(lisp_name, 5)  # Remove "ext_" prefix
    }
    gsub(/_/, "-", lisp_name)  # Convert underscores to dashes
    
    # Store for later printing
    functions[++func_count] = func_name
    lisp_names[func_count] = lisp_name
    
    print "static lbm_value " func_name "(lbm_value *args, lbm_uint argn) {"
    print "    // TODO: Implement " func_name
    print "    return lbm_enc_sym(SYM_EERROR);"
    print "}"
    print ""
}

END {
    print "// Extension registration function:"
    print ""
    print "void load_" name "_extensions(void) {"
    for (i = 1; i <= func_count; i++) {
        print "    lbm_add_extension(\"" lisp_names[i] "\", " functions[i] ");"
    }
    print "}"
}'