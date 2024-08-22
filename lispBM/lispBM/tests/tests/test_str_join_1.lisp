(check (eq
    (str-join '("ab" "cde" "f"))
    (str-join '("ab" "cde" "f") "")
    "abcdef"
))