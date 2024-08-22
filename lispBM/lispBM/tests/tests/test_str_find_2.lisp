(check (=
    (str-find "ab ab" "ab" 100)
    (str-find "ab ab" "ab" -100 'left)
    (str-find "ab" "abab")
    (str-find "ab" "abab" 'left)
    (str-find "ab ab" '())
    -1
))