
(check (and (= (buflen "apa") 4)
            (= (buflen "bepa") 5)
            (= (buflen "kurt1") 6)
            ; spell-checker: disable-next-line
            (eq "hellö\n" [104 101 108 108 195 182 10 0])
            (eq "\0\a\b\t\n\v\f\r\e\s\"\\\d" [0 7 8 9 10 11 12 13 27 32 34 92 127 0])
            (eq (trap (read "\"invalid escape: \\I\"")) '(exit-error read_error))))
