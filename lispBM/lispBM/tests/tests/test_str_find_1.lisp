(check (=
    (str-find "ab ab" "ab" 0 1)
    (str-find "ab ab" "ab" -1 'left)
    (str-find "ab ab" "ab" -4)
    (str-find [1 1 1 2 3] [2 3 0] 3)
    3
))