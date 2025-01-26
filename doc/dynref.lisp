
(define df-abs
  (ref-entry "abs"
             (list
              (para (list "Compute the absolute value"
                          ))
              (code '((abs -1)
                      (abs -1.0)
                      (abs 1)
                      (abs 1.0)
                      (abs (sin -3.14))
                      ))
              end
              )
             )
  )
              
             

(define df-apply
  (ref-entry "apply"
             (list
              (para (list "Apply a function taking n arguments to a list of n elements"
                          ))
              (code '((apply + (list 1 2 3))
                      ))
              end
              )
             )
  )
  
(define df-filter
  (ref-entry "filter"
             (list
              (para (list "Filter a list from unwanted elements as decided by a predicate"
                          ))
              (code '((define even (lambda (x) (= (mod x 2) 0)))
                      (filter even (list 1 2 3 4 5 6 7 8 9))
                      ))
              end
              )
             ))

(define df-foldl
  (ref-entry "foldl"
             (list
              (para (list "`foldl` walks through a list, left to right, while accumulating"
                          "a result from applying a function to the element at hand and the result"
                          "of its previous step. foldl takes an initial value used as the that is combined"
                          "with the leftmost value of the list in the first iteration."
                          ))
              
              (code '((foldl + 0 (list 1 2 3 4 5 6 7 8 9 10))
                      ))
              
              (para (list "`foldl` has an advantage over `foldr` in that it is implemented in a"
                          "tail-recursive and constant-storage way."
                          ))
              
              (para (list "Funnily enough, `foldl` using function `cons` and initial value `nil`"
                          "converts a list to a snoc-list."
                          ))
              
              (code '((foldl cons nil (list 1 2 3 4 5 6 7 8 9 10))
                      ))

              (para (list "Now we are going off on a tangent, but `car` and `cdr` switches roles"
                          "with each other when operating on a snoc-list."
                          ))
              (code '((define my-snoc-list (foldl cons nil (list 1 2 3 4 5 6 7 8 9 10)))
                      (cdr my-snoc-list)
                      (car my-snoc-list)
                      ))
              end 
              )
             ))

             (define df-foldr
  (ref-entry "foldr"
             (list
              (para (list "`foldr` walks through a list, right to left, while combining value and"
                          "prev result step by step."
                          "An initial value is provided and here used in the first, rightmost, operation."
                          ))

              (para (list "`foldr` has a disadvantage compared to `foldl` as I don't think it is"
                          "possible to give `foldr` a constant-space and tail-recursive implementation."
                          "One can make `foldr` tail-recursive by writing it in continuation passing style (CPS)."
                          "The CPS version of foldr will run without building upon the return stack, but instead"
                          "it will construct  and expand upon a continuation object each iteration."
                          "This continuation object will grow at the same rate as the call-stack otherwise would"
                          "but it would grow in heap usage."
                          ))
                          
              (code '((foldr + 0 (list 1 2 3 4 5 6 7 8 9 10))
                      ))

              (para (list "Much less amusingly compared to `foldl`, `foldr` of `cons` with initial  value  `nil`"
                          "is the identity function on proper lists."
                          ))
              
              (code '((foldr cons nil (list 1 2 3 4 5 6 7 8 9 10))
                      ))
              end
              )
             ))

(define df-iota
  (ref-entry "iota"
             (list
              (para (list "`iota` takes one number as argument and generates a list of values up to"
                          "(not including) the given number."
                          ))
              (code '((iota 4)
                      ))
              end
              )
             ))

(define df-second
  (ref-entry "second"
             (list
              (para (list "`second` extracts the second element from a list."
                          ))
              (code '((second (list 1 2 3 4 ))
                      (second (list 1))
                      ))
              end
              )
             ))
  
(define df-str-cmp-asc
  (ref-entry "str-cmp-asc"
             (list
              (para (list "compare strings according to alphabetical order ascending."
                          ))
              (code '((read-eval "(str-cmp-asc \"apa\" \"bepa\")")
                      (read-eval "(str-cmp-asc \"bepa\" \"apa\")")
                      ))
              end
              )
             ))

(define df-str-cmp-dsc
  (ref-entry "str-cmp-dsc"
             (list
              (para (list "compare strings according to alphabetical order descending."
                          ))

              (code '((read-eval "(str-cmp-dsc \"apa\" \"bepa\")")
                      (read-eval "(str-cmp-dsc \"bepa\" \"apa\")")
                      ))
              end
              )
             ))

(define df-str-merge
  (ref-entry "str-merge"
             (list
              (para (list "`str-merge` is an alternative name for the `str-join` operations."
                          "It is kept around for backwards compatibility."
                          ))
              (code '((read-eval "(str-merge \"Kurt\" \" \" \"Russel\")")
                       ))
              end
              )
             ))

(define df-third
  (ref-entry "third"
             (list
              (para (list "`third` extracts the third element from a list."
                          ))
              (code '((third (list 1 2 3 4))
                      (third (list 1))
                      ))

              end
              )
             ))

(define dynamic-functions
  (section 2 "functions"
           (list 'hline
                 df-abs
                 df-apply
                 df-filter
                 df-foldl
                 df-foldr
                 df-iota
                 df-second
                 df-str-cmp-asc
                 df-str-cmp-dsc
                 df-str-merge
                 df-third
                 )
           )
  )

(define dm-defun
  (ref-entry "defun"
             (list
              (para (list "`defun` is a macro that provides a shorthand form for defining a named function."
                          "`(defun name args body)` is expanded into `(define name (lambda args body))`."
                          ))
              (code '((defun f (x) (+ x 1))
                      (f 1)
                      ))

              end
              )
             ))

(define dm-defunret
  (ref-entry "defunret"
             (list
              (para (list "`defunret` is like `defun` but you are allowed to `return` at any point"
                          "in the function body."
                          ))
              (code '((defunret g (x) (progn
                                        1
                                        2
                                        (return 55)
                                        3
                                        4
                                        x
                                        ))
                      (g 10)
                        ))

              end
              )
             ))


(define dm-defmacro
  (ref-entry "defmacro"
             (list
              (para (list "`defmacro` is a macro that provides a shorthand form for defining macros."
                          "`(defmacro name args body)` expands into `(define name (macro args body))`."
                          ))
              (code '((read-eval "(defmacro my-macro (a) `(list 'apa ,a))")
                      (my-macro 10)
                      ))
                                

              end
              )
             ))


(define dynamic-macros
  (section 2 "macros"
           (list 'hline
                 dm-defun
                 dm-defunret
                 dm-defmacro
                 )
           )
  )

(define dlm-loopfor
  (ref-entry "loopfor"
             (list
              (para (list "`loopfor` has the form `(loopfor it start cond update body)`"
                          "and implements a for loop as familiar from for example C."
                     ))
              (para (list "`it` is the iterator, `start` is what it is initialized to,"
                          "`cond` is the condition that has the be true for the loop to continue"
                          "running, `update` is how to update the iterator after each iteration"
                          "and body is the code to execute each iteration. The iterator can be"
                          "accessed from within body."
                          ))
              (program '(((define r 0)
                          (loopfor i 0 (< i 5) (+ i 1) (setq r (+ r i)))
                          r)
                       ))
              end
              )
             ))

(define dlm-loopwhile
  (ref-entry "loopwhile"
             (list
              (para (list "`loopwhile` has the form `(loopwhile cond body)`"
                          "and implements a while loop as familiar from for example C."
                          ))
              
              (para (list "`cond` is the condition that has the be true"
                          "for the loop to continue running and `body` is the code"
                          "to execute each iteration."
                          ))
              (program '(((define a 0)
                          (loopwhile (< a 10)
                                     (setq a (+ a 1))
                                     )
                          a)
                         ))
              end
              )
             ))

(define dlm-looprange
  (ref-entry "looprange"
             (list
              (para (list "`looprange` has the form `(looprange it start end body)`"
                          "and implements a loop over a range similar to python's `for i in range(n)`."
                          ))
              (para (list "Iterate `it` from `start` to `end` and evaluate `body` for"
                          "each iteration. The iterator it can be accessed from within body."
                          ))
              (program '(((define b 0)
                          (looprange i 0 11
                                     (setq b (+ b i))
                                     )
                          b)
                         ))
              (program '(((define my-ls nil)
                          (looprange i 0 5
                                     (setq my-ls (cons i my-ls)))
                          my-ls)
                         ))
              end
              )
             ))

(define dlm-loopforeach
  (ref-entry "loopforeach"
             (list
              (para (list "`loopforeach` has the form `(loopforeach it lst body)`"
                          "and implements a loop over the elements of a list"
                          "similar to python's `for e in ...`."
                          ))
              (para (list "Iterate over every element in the list `lst` and evaluate"
                          "`body` for each iteration. The iterator `it` can be accessed"
                          "from within body."
                          ))
              (program '(((define m 0)
                          (loopforeach e (list 2 4 6 8)
                                       (setq m (+ m e)))
                          m)
                         ))
              end
              )
             ))

(define dlm-loopwhile-thd
  (ref-entry "loopwhile-thd"
             (list
              (para (list "`loopwhile-thd` is like `loopwhile` but the thread runs in its own thread asynchronously."
                          "The form of a `loopwhile-thd` is `(loopwhile-thd stack cond body)`."
                          ))
              (para (list "A  While-loop that starts in a new thread. The argument `stack` is the stack-size"
                          "of the thread, `cond` is the condition that has the be true for the loop to"
                          "continue running and `body` is the code to execute each iteration. The"
                          "difference from the regular loopwhile is that the evaluator will continue"
                          "running the code after this one before this one finishes, as this loop is"
                          "evaluated in a new thread."
                          ))
              (para (list "The following examples assumes that your LispBM integration has a way to `print`."
                          ))
              (para (list "Example that forever prints \"Hello World\" every two seconds:"
                          "\n\n```\n"
                          "(loopwhile-thd 100 t { \n"
                          "   (print \"Hello World\") \n"
                          "   (sleep 2) \n"
                          "})\n"
                          "```\n\n"
                          "The above is equivalent to the following code"
                          "\n\n"
                          "```\n"
                          "(spawn 100 (fn () (loopwhile t {\n"
                          "                 (print \"Hello World\")\n"
                          "                 (sleep 2)\n"
                          "})))\n"
                          "```\n\n"
                          "It is possible to give the thread a name and/or"
                          "a stack size. That gives the following combinations of possibilities:"
                          "\n\n"
                          "No name and default stack size\n"
                          "\n\n"
                          "```\n"
                          "(loopwhile-thd () t {\n"
                          "        (print \"Hello World1\")\n"
                          "        (sleep 2)\n"
                          "})\n"
                          "```\n\n"
                          "No name and stack size 100\n"
                          "\n\n"
                          "```\n"
                          "(loopwhile-thd 100 t {\n"
                          "        (print \"Hello World2\")\n"
                          "        (sleep 2)\n"
                          "})\n"
                          "```\n\n"
                          "Name ThdTest and default stack size\n"
                          "\n\n"
                          "```\n"
                          "(loopwhile-thd \"ThdTest\" t {\n"
                          "        (print \"Hello World3\"\n)"
                          "        (sleep 2)\n"
                          "})\n"
                          "```\n"
                          "Name ThdTest2 and stack size 100\n"
                          "\n\n"
                          "```\n"
                          "(loopwhile-thd (\"ThdTest2\" 100) t {\n"
                          "        (print \"Hello World4\")\n"
                          "        (sleep 2)\n"
                          "})\n"
                          "```\n"
                          ))
              end
              )
             ))
         
(define dynamic-loop-macros
  (section 2 "loop macros"
           (list 'hline
                 dlm-loopfor
                 dlm-loopwhile
                 dlm-looprange
                 dlm-loopforeach
                 dlm-loopwhile-thd
                 )
           )
  )

(define da-array-to-list
  (ref-entry "array-to-list"
             (list
              (para (list "Convert an array to a list"
                          ))
              (code '((array-to-list (list-to-array (list 1 2 3)))
                      (array-to-list [| 1 2 3 4|])
                      ))
              end
              )
             )
  )

(define da-list-to-array
  (ref-entry "list-to-array"
             (list
              (para (list "Convert a list to an array"
                          ))
              (code '((list-to-array (list 1 2 3))
                      (list-to-array '(nil nil nil))
                      ))
              end
              )
             )
  )

(define da-is-array
    (ref-entry "array?"
	       (list
		(para (list "Array predicate is true for arrays."
			    ))
		(code '((array? [| 1 2 3 |])
			(array? 1)
			(array? 'apa)
			(array? (list 1 2 3))
			))
		end
		)
	       )
  )

(define dynamic-arrays
    (section 2 "Array functions"
	     (list 'hline
		   da-list-to-array
		   da-array-to-list
		   da-is-array
		   )
	     )
  )

(define dynamic-defstruct
  (section 2 "defstruct and its operations"
           (list 'hline
                 (para (list "`defstruct` defines a datastructure with named fields similar to a"
                             "`struct` in C."
                             " `defstruct` takes two arguments, a struct name and a list of"
                             "fields `(defstruct name list-of-fields)`."
                             ))
                 (para (list "Structs are implemented as arrays of lisp values and offer constant time"
                             "lookup of each of its fields. The struct itself does not occupy heap cells, but"
                             "the values stored in the fields may."
                             ))
                 (para (list "As structs are allocated from array memory (lbm_memory), there is a potential"
                             "for causing memory fragmentation."
                             ))
                 (para (list "The example below creates a structure type called my-struct with three fields"
                             "called `a`, `b` and `c`."
                             ))
                 (code '((defstruct my-struct (a b c))
                         ))
                 (para (list "Now instances of `my-struct` can be creted using `make-my-struct`."
                             ))
                 (code '((define s1 (make-my-struct))
                         ))
                 (para (list "when a struct is defined using `defstruct` a number of functions for"
                             "manipulation of that kind of struct is automatically generated."
                             ))
                 (bullet '("make-name : function for creation of struct with name `name`."
                           "name? : predicate that is true for instances of the struct named `name`."
                           "name-x : setter/getter for struct `name` and field `x`."
                           ))
                 (para (list "This will be more clear by showing with `my-struct` as example."
                             ))
                 (code '((my-struct? s1)
                         (read-eval "(my-struct? \"hej\")")
                         (my-struct? 10)
                         ))
                 (para (list "`my-struct?` is a predicate that is true for instances of `my.struct`."
                             ))
                 (code '((my-struct-a s1 10)
                         (my-struct-b s1 20)
                         (my-struct-c s1 30)
                         ))
                 (para (list "`my-struct-x` with 2 arguments is a setter."
                             "with just the struct instance as argument it is a getter."
                             ))
                 (code '((my-struct-a s1)
                         (my-struct-b s1)
                         (my-struct-c s1)
                         ))
                 (para (list "Instances of a struct can also be allocated in a compactible memory region (defrag mem)."
                             ))
                 (code '((define dm (dm-create 1000))
                         (define s2 (make-my-struct dm))
                         ))
                 (para (list "For more information about defragmentable memory see the LispBM [reference manual](https://github.com/svenssonjoel/lispBM/blob/master/doc/lbmref.md)."
                             ))
                 )
           )
  )

(define manual
  (list
   (section 1 "LispBM library of dynamically loadable functionality"
            (list
             (para (list "With LispBM comes a small library of dynamically loadable functions and macros."
                         "These functions occupy flash memory (if on an embedded platform) but only use"
                         "heap if they are actually used by the application."
                         "Providing dynamically loadable operations is a way to save space while offering"
                         "more sweet functionality."))
             (para (list "LispBM can be built with varying amount of included dynamic loadable functionality."
                         "It is up to the LispBM integrator to decide what is included."
                         "As an integrator you can also decide to roll your entirely own set of dynamically loadable"
                         "operations."))
             (para (list "The inclusion of dynamically loadable functionality from this library is"
                         "determined when LispBM is compiled using the following flags:" ))
             (bullet '("LBM_USE_DYN_FUNS : Add a library of functions to the dynamic loader."
                       "LBM_USE_DYN_MACROS : Add a library of macros to the dynamic loader."
                       "LBM_USE_DYN_DEFSTRUCT : Add the defstruct mechanism, requires LBM_USE_DYN_FUNS and LBM_USE_DYN_MACROS."
                       "LBM_USE_DYN_LOOPS : Add loop macros, requires LBM_USE_DYN_MACROS."
		       "LBM_USE_DYN_ARRAYS : Add functions on arrays. Requires LBM_USE_DYN_MACROS and LBM_USE_DYN_LOOPS."
                       ))
             (para (list "The flags should be given to the compiler as -Dx for example -DLBM_USE_DYN_FUNS."
                         ))
             dynamic-functions
             dynamic-macros
             dynamic-loop-macros
	     dynamic-arrays
             dynamic-defstruct
             
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "dynref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Dynlib reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
