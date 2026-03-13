
(define entry-str-from-n
  (ref-entry "str-from-n"
             (list
              (para (list "`str-from-n` converts a number to its string representation."
                          "The form is `(str-from-n number)` or `(str-from-n number format)`"
                          "where `format` is an optional printf-style format string."
                          "Floats default to `\"%g\"` format, integers default to `\"%d\"`."
                          "The result is at most 100 characters long."
                          ))
              (code '((str-from-n 42)
                      (str-from-n 3.14)
                      (str-from-n 255 "%x")
                      (str-from-n 3.14159 "%.2f")
                      ))
              end)))

(define entry-str-join
  (ref-entry "str-join"
             (list
              (para (list "`str-join` concatenates a list of strings into a single string."
                          "The form is `(str-join strings)` or `(str-join strings delimiter)`"
                          "where `delimiter` is an optional string inserted between each element."
                          "The default delimiter is the empty string."
                          ))
              (code '((str-join (list "Hello" " " "World"))
                      (str-join (list "a" "b" "c") "-")
                      (str-join (list "one" "two" "three") ", ")
                      ))
              end)))

(define entry-str-to-i
  (ref-entry "str-to-i"
             (list
              (para (list "`str-to-i` parses a string as an integer."
                          "The form is `(str-to-i string)` or `(str-to-i string base)`."
                          "The optional `base` argument specifies the number base."
                          "Base 0 auto-detects the base from the string prefix"
                          "(`0x` for hex, `0` for octal, otherwise decimal)."
                          "Returns an `i32`."
                          ))
              (code '((str-to-i "42")
                      (str-to-i "-100")
                      (str-to-i "ff" 16)
                      (str-to-i "0xff" 0)
                      (str-to-i "101" 2)
                      ))
              end)))

(define entry-str-to-f
  (ref-entry "str-to-f"
             (list
              (para (list "`str-to-f` parses a string as a floating-point number."
                          "The form is `(str-to-f string)`."
                          "Returns an `f32`."
                          ))
              (code '((str-to-f "3.14")
                      (str-to-f "-2.718")
                      (str-to-f "1e10")
                      ))
              end)))

(define entry-str-len
  (ref-entry "str-len"
             (list
              (para (list "`str-len` returns the length of a string in characters,"
                          "not counting the null terminator."
                          "The form is `(str-len string)`."
                          ))
              (code '((str-len "Hello")
                      (str-len "")
                      (str-len "Hello, World!")
                      ))
              end)))

(define entry-str-part
  (ref-entry "str-part"
             (list
              (para (list "`str-part` extracts a substring from a string."
                          "The form is `(str-part string start)` or `(str-part string start length)`."
                          "Returns a new string starting at character index `start`,"
                          "optionally limited to `length` characters."
                          "Returns an error if `start` is beyond the end of the string."
                          ))
              (code '((str-part "Hello, World!" 7)
                      (str-part "Hello, World!" 0 5)
                      (str-part "Hello, World!" 7 5)
                      ))
              end)))

(define entry-str-split
  (ref-entry "str-split"
             (list
              (para (list "`str-split` splits a string into a list of substrings."
                          "The form is `(str-split string delimiter)` where `delimiter`"
                          "is either a string or an integer."
                          ))
              (para (list "When `delimiter` is a string, the input is split at any character"
                          "found in the delimiter string."
                          ))
              (para (list "When `delimiter` is an integer, the input is split into"
                          "chunks of that many characters."
                          ))
              (code '((str-split "hello world" " ")
                      (str-split "a,b;c,d" ",;")
                      (str-split "abcdef" 2)
                      ))
              end)))

(define entry-str-replace
  (ref-entry "str-replace"
             (list
              (para (list "`str-replace` replaces all occurrences of a pattern in a string."
                          "The form is `(str-replace string pattern replacement)` or"
                          "`(str-replace string pattern)` to remove all occurrences."
                          "Returns a new string with all occurrences of `pattern` replaced."
                          ))
              (code '((str-replace "hello world" "world" "LispBM")
                      (str-replace "aabbcc" "bb" "XX")
                      (str-replace "hello world" "o")
                      ))
              end)))

(define entry-str-to-lower
  (ref-entry "str-to-lower"
             (list
              (para (list "`str-to-lower` converts a string to lowercase."
                          "The form is `(str-to-lower string)`."
                          "Returns a new string with all characters converted to lowercase."
                          ))
              (code '((str-to-lower "Hello, World!")
                      (str-to-lower "LISPBM")
                      ))
              end)))

(define entry-str-to-upper
  (ref-entry "str-to-upper"
             (list
              (para (list "`str-to-upper` converts a string to uppercase."
                          "The form is `(str-to-upper string)`."
                          "Returns a new string with all characters converted to uppercase."
                          ))
              (code '((str-to-upper "Hello, World!")
                      (str-to-upper "lispbm")
                      ))
              end)))

(define entry-str-cmp
  (ref-entry "str-cmp"
             (list
              (para (list "`str-cmp` compares two strings lexicographically."
                          "The form is `(str-cmp str1 str2)` or `(str-cmp str1 str2 n)`"
                          "where `n` limits the comparison to the first `n` characters."
                          "Returns a negative integer if `str1` is less than `str2`,"
                          "zero if they are equal, and a positive integer if `str1` is greater."
                          ))
              (code '((str-cmp "abc" "abc")
                      (str-cmp "abc" "abd")
                      (str-cmp "abd" "abc")
                      (str-cmp "abcdef" "abcxyz" 3)
                      ))
              end)))

(define entry-str-replicate
  (ref-entry "str-replicate"
             (list
              (para (list "`str-replicate` creates a string by repeating a character."
                          "The form is `(str-replicate n char)` where `n` is the number"
                          "of characters and `char` is the character as a byte value."
                          "Returns a new string of length `n`."
                          ))
              (code '((str-replicate 5 \#a)
                      (str-replicate 3 \#-)
                      (str-replicate 8 \#x)
                      ))
              end)))

(define entry-str-find
  (ref-entry "str-find"
             (list
              (para (list "`str-find` searches for a substring within a string."
                          "The form is `(str-find string substr)` with optional additional arguments."
                          "Returns the index of the first match, or -1 if not found."
                          "The `substr` argument can also be a list of strings,"
                          "in which case the search finds any of them."
                          ))
              (para (list "Optional arguments (may be given in any order after `substr`):"
                          ))
              (bullet (list "An integer sets the start position for the search."
                            "A second integer sets which occurrence to find (0 = first)."
                            "`'left` searches from right to left."
                            "`'nocase` performs a case-insensitive search."
                            ))
              (code '((str-find "hello world" "world")
                      (str-find "hello world" "xyz")
                      (str-find "abcabc" "bc" 0 1)
                      (str-find "Hello World" "world" 'nocase)
                      (str-find "abcabc" "bc" 'left)
                      ))
              end)))

(define entry-to-str
  (ref-entry "to-str"
             (list
              (para (list "`to-str` converts one or more LispBM values to a string."
                          "The form is `(to-str val1 val2 ...)`."
                          "Multiple values are separated by a single space."
                          "The result is at most 300 characters long."
                          ))
              (code '((to-str 42)
                      (to-str 3.14)
                      (to-str 'hello)
                      (to-str 1 2 3)
                      (to-str "hello" "world")
                      ))
              end)))

(define entry-to-str-delim
  (ref-entry "to-str-delim"
             (list
              (para (list "`to-str-delim` converts one or more LispBM values to a string"
                          "using a custom delimiter between values."
                          "The form is `(to-str-delim delimiter val1 val2 ...)`."
                          "The first argument is the delimiter string."
                          "The result is at most 300 characters long."
                          ))
              (code '((to-str-delim ", " 1 2 3)
                      (to-str-delim " | " 'a 'b 'c)
                      (to-str-delim "" "Hello" " " "World")
                      ))
              end)))

(define chapter-conversion
  (section 2 "Conversion"
           (list entry-str-from-n
                 entry-str-to-i
                 entry-str-to-f
                 entry-to-str
                 entry-to-str-delim
                 )))

(define chapter-operations
  (section 2 "String Operations"
           (list entry-str-len
                 entry-str-part
                 entry-str-join
                 entry-str-split
                 entry-str-replace
                 entry-str-replicate
                 )))

(define chapter-search
  (section 2 "Searching and Comparing"
           (list entry-str-cmp
                 entry-str-find
                 )))

(define chapter-case
  (section 2 "Case Conversion"
           (list entry-str-to-lower
                 entry-str-to-upper
                 )))

(define manual
  (list
   (section 1 "LispBM String Extensions Reference Manual"
            (list
             (para (list "The string extensions provide functions for manipulating strings,"
                         "converting between strings and other types, and searching strings."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             (para (list "In LispBM, strings are byte arrays terminated by a null byte."
                         "String indices are zero-based and measured in bytes."
                         ))
             chapter-conversion
             chapter-operations
             chapter-search
             chapter-case
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "stringref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "String extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
