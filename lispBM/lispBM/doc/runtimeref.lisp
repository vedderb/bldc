

(define evaluation-quota
  (ref-entry "set-eval-quota"
             (list
              (para (list "`set-eval-quota` sets the number of evaluation steps that is"
                          "given to each context when given turn to execute by the round-robin"
                          "scheduler."
                          ))
              (code '((set-eval-quota 30)
                      ))
              end)))


(define chapter-scheduling
  (section 2 "Scheduling"
           (list evaluation-quota)))


(define num-free
  (ref-entry "mem-num-free"
             (list
              (para (list "`mem-num-free` returns the number of free words in the LBM memory."
                          "This is the memory where arrays and strings are stored."
                          ))
              (code '((mem-num-free)
                      ))
              end)))

(define longest-free
  (ref-entry "mem-longest-free"
             (list
              (para (list "`mem-longest-free` returns the length in words of the longest"
                          "consecutive sequence of free words in the LBM memory."
                          ))
              (code '((mem-num-free)
                      ))
              end)))

(define memory-size
  (ref-entry "mem-size"
             (list
              (para (list "`mem-size` returns the size of the LBM memory."
                          ))
              (code '((mem-size)
                      ))
              end)))

(define heap-state
  (ref-entry "lbm-heap-state"
             (list
              (para (list "`lbm-heap-state` can be used to query information about heap usage."
                          ))
              (code '((lbm-heap-state 'get-heap-size)
                      (lbm-heap-state 'get-heap-bytes)
                      (lbm-heap-state 'get-num-alloc-cells)
                      (lbm-heap-state 'get-num-alloc-arrays)
                      (lbm-heap-state 'get-gc-num)
                      (lbm-heap-state 'get-gc-num-marked)
                      (lbm-heap-state 'get-gc-num-recovered-cells)
                      (lbm-heap-state 'get-gc-num-recovered-arrays)
                      (lbm-heap-state 'get-gc-num-least-free)
                      (lbm-heap-state 'get-gc-num-last-free)
                      ))
              end)))


(define chapter-memory
  (section 2 "Memory"
           (list num-free
                 longest-free
                 memory-size
                 heap-state)))

(define gc-stack
  (ref-entry "set-gc-stack-size"
             (list
              (para (list "With `set-gc-stack-size` you can change the size of the stack used for heap traversal"
                          "by the garbage collector."
                          ))
              (code '((set-gc-stack-size 100)
                      ))
              end)))

(define chapter-gc
  (section 2 "GC"
           (list gc-stack)))



(define environment-get
  (ref-entry "env-get"
             (list
              (para (list "`env-get` can be used to reify, turn into value, parts of the global environment."
                          "The global environment is stored as a hashtable and an index into this hashtable"
                          "is used to extract the bindings stored under that hash."
                          ))
              (code '((env-get 0)
                      (env-get 1)
                      (env-get 2)
                      (env-get 3)
                      (env-get 4)
                      (env-get 5)
                      (env-get 6)
                      (env-get 7)
                      (env-get 8)
                      (env-get 9)
                      (env-get 10)
                      (env-get 11)
                      (env-get 12)
                      (env-get 13)
                      (env-get 14)
                      (env-get 15)
                      (env-get 16)
                      (env-get 17)
                      (env-get 18)
                      (env-get 19)
                      (env-get 20)
                      (env-get 21)
                      (env-get 22)
                      (env-get 23)
                      (env-get 24)
                      (env-get 25)
                      (env-get 26)
                      (env-get 27)
                      (env-get 28)
                      (env-get 29)
                      (env-get 30)
                      (env-get 31)
                      ))
              end)))

(define environment-set
  (ref-entry "env-set"
             (list
              (para (list "`env-set` destructively sets an entry in the global environment hashtable."
                          ))
              (program '(((if (eq (env-get 1) nil)
                              (env-set 1 (list '(a . 75))))
                          (env-get 1))
                         ))
              (para (list "Note that in the example code above there is no guarantee that the symbol"
                          "`a` actually hashes to index 1 in the environment table."
                          "So `a` is most likely impossible to look up from this environment."
                          "The use case for `env-set` and `env-get` are rather that they are"
                          "together. "
                          "Use `env-get` to extract index `i` from the table, then modify it in some way"
                          "and end by using `env-set` to the same index `i`."
                          ))
              end)))


(define local-environment-get
  (ref-entry "local-env-get"
             (list
              (para (list "`local-env-get` can be used to reify, turn into value, the local environment."
                          ))
              (code '((local-env-get)
                      ))
              (program '(((let (( a 50))
                            (local-env-get)))
                         ))
              end)))

(define chapter-environments
  (section 2 "Environments"
           (list environment-get
                 environment-set
                 local-environment-get
                 )))


(define symbol-table-size
  (ref-entry "symtab-size"
             (list
              (para (list "`symtab-size` returns the size of the symbol table in bytes."
                          ))
              (code '((symtab-size)
                      ))
              end)))

(define symbol-table-size-flash
  (ref-entry "symtab-size-flash"
             (list
              (para (list "`symtab-size-flash` returns the size in bytes of the portion of the symbol table"
                          "that is stored in flash."
                          ))
              (code '((symtab-size-flash)
                      ))
              end)))


(define symbol-table-size-names
  (ref-entry "symtab-size-names"
             (list
              (para (list "`symtab-size-names` returns the size in bytes of the string names stored in"
                          "the symbol table."
                          ))
              (code '((symtab-size-names)
                      ))
              end)))

(define symbol-table-size-names-flash
  (ref-entry "symtab-size-names-flash"
             (list
              (para (list "`symtab-size-names` returns the size in bytes of the string names stored in"
                          "the symbol table in flash."
                          ))
              (code '((symtab-size-names-flash)
                      ))
              end)))


(define chapter-symboltable
  (section 2 "Symbol table"
           (list symbol-table-size
                 symbol-table-size-flash
                 symbol-table-size-names
                 symbol-table-size-names-flash
                 )))


(define version
  (ref-entry "lbm-version"
             (list
              (para (list "`lbm-version` returns the version of the lbm runtime system."
                          ))
              (code '((lbm-version)
                      ))
              end)))

(define arch
  (ref-entry "is-64bit"
             (list
              (para (list "`is-64bit` returns true if a 64bit version of lbm is running."
                          ))
              (code '((is-64bit)
                      ))
              end)))


(define word
  (ref-entry "word-size"
             (list
              (para (list "`word-size` returns 4 on 32bit LBM  and 8 on 64bits."
                          ))
              (code '((word-size)
                      ))
              end)))

(define chapter-versioning
  (section 2 "Version"
           (list version
                 arch
                 word
           )))


(define manual
  (list
   (section 1 "LispBM Runtime Extensions Reference Manual"
            (list
             (para (list "The runtime extensions, if present, can be either compiled"
                         "in a minimal or a full mode."
                         "In the minimal mode only `set-eval-quota` is present."
                         "Minimal mode is the default when compiling LBM. To get the"
                         "full mode the `-DFULL_RTS_LIB` flag must be used when compiling."
                         ))
             chapter-environments
             chapter-gc
             chapter-memory
             chapter-scheduling
             chapter-symboltable
             chapter-versioning
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "runtimeref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Runtime reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
