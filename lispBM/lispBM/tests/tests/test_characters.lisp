(check (and
    (eq \#a 97b)
    (eq "hi" [\#h \#i \#\0])
    (eq [7 \#\b 9] [7 8 9]) ; scary D:
    (eq (trap (read "\\#\\I")) '(exit-error read_error))
    (eq
        (list \#\0 \#\a \#\b \#\t \#\n \#\v \#\f \#\r \#\e \#\s \#\" \#\\ \#\d)
        (list 0b 7b 8b 9b 10b 11b 12b 13b 27b 32b 34b 92b 127b)
    )
))
