
(def ch-symbols
     (section 2 "About Symbols"
              ( list
                (para (list "Symbols are very important and central to LispBM and also perhaps"
                            "a bit different from identifiers/names used in languages such as C."
                            "A short introduction to symbols could be a good place to start."
                            ))
                (para (list "One way to think about a symbol is as a name. Used as a name, a"
                            "symbol can identify a value or function in the environment. A"
                            "symbol can also be used as data in and of itself, more on"
                            "this later."
                            ))
                'newline
                'hline
                (bold "NOTE") 'newline

                (para (list "Symbols are expressed as strings in your program such as `a`, `let`,"
                            "`define`, `+` or `orange`. The \"reader\", the part of LBM that parses"
                            "code, translates each symbol into a 28bit value. The string `orange`"
                            "for example is only of interest if you print a symbol and then the"
                            "runtime system will look up what string corresponds to the 28bit"
                            "identifier you want to print. So the runtime system is never wasting"
                            "time comparing strings to see if a symbol is this or that symbol, it's"
                            "all integer comparisons."
                            ))
                'hline

                (para (list "You associate values with symbols using, <a href=\"#define\">define</a>,"
                            "<a href=\"#let\">let</a> and you can change the value bound to a \"variable\""
                            "using <a href=\"#set\">set</a>, <a href=\"#setvar\">setq</a> or <a href=\"#setvar\">setvar</a>."
                            ))

                (para (list "Not all symbols are treated the same in LBM. Some symbols are treated as"
                            "special because of their very fundamental nature. Among these special symbols"
                            "you find `define`, `let` and `lambda` for example. These are things that you"
                            "should not be able to redefine and trying to redefine them leads to an error."
                            "Symbols that start with `ext-` are special and reserved for use together"
                            "with extensions that are loaded and bound at runtime."
                            ))
                (para (list "Examples of symbols used as data are `nil` and `t`. `nil` represents"
                            "\"nothing\", the empty list or other similar things and `t`"
                            "represents true.  But any symbol can be used as data by quoting it"
                            "`'`, see <a href=\"#quotes-and-quasiquotation\"> Quotes and Quasiquotation </a>."
                            ))

                (section 3 "Valid Symbol Names"
                         (list
                          (para (list "A symbol is a string of characters following the rules:"
                                      "1. The first character is a one of 'a' - 'z' or 'A' - 'Z' or '+-*/=<>#!'."
                                      "2. The rest of the characters are in 'a' - 'z' or 'A' - 'Z' or '0' - '9' or '+-*/=<>!?_'."
                                      "3. At most 256 characters long."
                                      ))
                          (para (list "Note that lower-case and upper-case alphabetical letters are considered identical"
                                      "so the symbol `apa` is the same symbol as `APA`."
                                      ))

                          (para (list "examples of valid symbols:"
                                      "```"
                                      "apa"
                                      "apa?"
                                      "!apa"
                                      "kurt_russel_is_great"
                                      "```"
                                      ))
                          end))

                end)))

(define overflow-behaviour
  (section 3 "Overflow behaviour"
           (list
            (para (list "Operations on fixed bitwidth numerical types can lead to overflow."
                        "The ranges representable in 32bit LBMs integer types are the following:"
                        ))
            (bullet '("`type-char`  : 0 - 255"
                      "`type-i`     : -134217728 - 1342177272"
                      "`type-u`     : 0 - 268435455"
                      "`type-i32`   : -2147483648 - 2147483647"
                      "`type-u32`   : 0- 4294967295"
                      "`type-i64`   : -9223372036854775808 - 9223372036854775807"
                      "`type-u64`   : 0 - 18446744073709551615"
                      ))
            (code '((+ 255b 1b)
                    (- 0b 1b)
                    ( + 134217727 1)
                    (- -134217728 1)
                    (+ 268435455u 1u)
                    (- 0u 1u)
                    (+ 2147483647i32 1i32)
                    (- 2147483648i32 1i32)
                    (+ 4294967295u32 1)
                    (- 0u32 1)
                    (+ 9223372036854775807i64 1i64)
                    (- -9223372036854775808i64 1i64)
                    (+ 18446744073709551615u64 1u64)
                    (- 0u64 1u64)
                    ))
            end))
            )

(define numerical-cost
  (section 3 "Cost of numerical operations"
           (list
            (para (list "All Values in LBM are encoded in one way or another. The encoded value"
                        "holds additional information about type and garbage collection mark"
                        "bit.  Operations that operate on an LBM value needs to unpack this"
                        "encoded format and extract the actual numerical information from the"
                        "representation. This has a cost and operations on numbers are in"
                        "general a bit slower than what one gets in, for example C."
                        ))
            (para (list "The chart below shows the time it takes to perform 10 million"
                        "additions on the x86 architecture (a i7-6820HQ) in 32 and 64 Bit mode."
                        ))
            (image "Perfomance of 10 million additions at various types on X86"
                   "./images/lbm_arith_pc.png"
                   )
            (para (list "In 64Bit mode the x86 version of LBM shows negligible differences in"
                        "cost of additions at different types."
                        ))
            
            (para (list "For addition performance on embedded systems, we use the the EDU VESC"
                        "motorcontroller as the STM32F4 candidate and the VESC EXPRESS for a"
                        "RISCV data point."
                        ))
            (image "Performance of 100000 additions at various types on ESP32C3 and STM32F4"
                   "./images/lbm_arith_embedded.png"
                   )
            (para (list "In general, on 32Bit platforms, the cost of operations on numerical"
                        "types that are 32Bit or less are about equal in cost."
                        "The costs"
                        "presented here was created by timing a large number of 2 argument"
                        "additions. Do not see these measurements as the \"truth carved in stone\","
                        "LBM performance keeps changing over time as we make improvements, but"
                        "use them as a rough guiding principle."
                        ))
            end))
  )


(define ch-numbers
  (section 2 "Numbers and numerical types"
           (list
            (para (list "LBM supports signed and unsigned integer types as well as float and double."
                        "The numerical types in LBM are"
                        ))
            (bullet (list "byte   - unsigned 8bit value."
                          "i      - signed 28bit value  (56bits on 64bit platforms)."
                          "u      - unsigned 28bit value (56bits on 64bit platforms)."
                          "i32    - signed 32bit value."
                          "u32    - unsigned 32bit value."
                          "i64    - signed 64bit value."
                          "u64    - unsigned 64bit value."
                          "f32    - (float) a 32bit floating point value."
                          "f64    - (double) a 64bit floating point value."
                          ))
            (para (list "The byte and the char value have identical representation and type,"
                        "thus char is an unsigned 8 bit type in LBM."
                        ))
            (para (list "An integer literal is interpreted to be of type `i`, a 28/56bit signed"
                        "integer value.  A literal with decimal point is interpreted to be a"
                        "type `f32` or float value."
                        ))
            (para (list "To specify literals of the other types, the value must be postfixed with"
                        "a qualifier string.  The qualifiers available in LBM are: `b`, `i`,"
                        "`u`, `i32`, `u32`, `i64`, `u64`, `f32` and `f63`.  The `i` and `f32`"
                        "qualifiers are never strictly needed but can be added if one so"
                        "wishes."
                        ))
            (para (list "So for example:"
                        ))
            (bullet '("`1b`     - Specifies a byte typed value of 1"
                      "`1.0f64` - Specifies a 64bit float with value 1.0."
                      ))
            (para (list "**Note** that it is an absolute requirement to include a decimal when"
                        "writing a floating point literal in LBM."
                        ))
            (para (list "We are trying to make type conversions feel familiar to people who know a bit of "
                        "C programming. On a 32bit platform LBM"
                        "numerical types are ordered according to: `byte < i < u < i32 < u32 <"
                        "i64 < u64 < float < double`.  Operations such as `(+ a b)`, figures"
                        "out the largest type according to the ordering above and converts all"
                        "the values to this largest type."
                        ))
            (para (list "Example:"
                        ))
            (bullet '("`(+ 1u 3i32)` - Promotes the 1u value type i32 and performs the addition, resulting in 4i32."
                      "`(+ 1  3.14)` - Here the value 1 is of type `i` which is smaller than `f32`, the result 4.14f32."
                      ))
            (para (list "A potential source of confusion is that `f32` is a larger type than"
                        "`i64` and `u64`. this means that if you, for example, add 1.0 to an"
                        "`i64` value you will get an `f32` back. If you instead wanted the"
                        "float to be converted into a double before the addition, this has to"
                        "be done manually."
                        ))
            (para (list "Example:"
                        ))
            (bullet '(" `(+ (to-double 1.0) 5i64)`    - Manually convert a value to double."
                      ))
            (para (list "The `type-of` operation can be used to query a value for its type. On the"
                        "numerical types the `type-of` operation answers as follows:"
                        ))
            (bullet '("`(type-of 1b)`     -> `type-char`"
                      "`(type-of 1)`      -> `type-i`"
                      "`(type-of 1u)`     -> `type-u`"
                      "`(type-of 1i32)`   -> `type-i32`"
                      "`(type-of 1u32)`   -> `type-u32`"
                      "`(type-of 1i64)`   -> `type-i64`"
                      "`(type-of 1u64)`   -> `type-u64`"
                      "`(type-of 1.0)`    -> `type-float`"
                      "`(type-of 1.0f64)` -> `type-double`"
                      ))
            end
            overflow-behaviour
            numerical-cost
            ))
  )


(defun semantics ()
  (section 3 "The meaning (semantics) that LispBM imposes on S-Expressions"
           (list
            (para (list "The S-expressions discussed in the previous section are merely tree structures."
                        "The Lisp evaluator provides a computational interpretation for these trees."
                        "However, not all trees are valid Lisp programs."
                        "This section focuses on those trees that do make sense as Lisp programs and"
                        "their meaning to the Lisp evaluator."
                        ))
            (para (list "**Values and expressions**"
                        ))
            (para (list "The LispBM evaluator transforms expressions"
                        "into values. For instance, the expression  `(+ 1 2)` is evaluated to the value `3`."
                        ))
            (code '((+ 1 2)
                    ))
            (para (list "In LispBM the distinction between expressions and values is often blurred."
                        "For example, it is possible to write a function that returns a result that"
                        "can itself be interpreted as code"
                        ))
            (code '((read-eval "(defun mk-code (x) `(+ ,x 1))")
                    (mk-code 10)
                    ))
            (para (list "The result of evaluating `(mk-code 10)` is the list containing a `+`, `10` and `1`."
                        "This list is the value that `(mk-code 10)` evaluates to."
                        "Now, the result of `(mk-code 10)`, since it is valid lisp, can be evaluated."
                        ))
            (code '((eval (mk-code 10))
                    ))
            (para (list "In most cases this is quite natural and our functions will result in, Strings, lists and numbers"
                        "that are easily and naturally understood as values."
                        ))
            (para (list "Still, it is worthwhile to remember that values can be expressions and expressions can be values."
                        ))
            (para (list "**Errors**"
                        ))
            (para (list "Some times evaluation is impossible. This could be because the program is malformed, a type mismatch or"
                        "a division by zero (among many other possibilities)."
                        "Errors terminate the evaluation of the expression. To recover from an error"
                        "the programmer needs to explicitly `trap` it."
                        ))
            (code '((trap (/ 1 0 ))
                    ))
            (para (list "**Environments**"
                        ))
            (para (list "LispBM expressions are evaluated in relation to a global and a local environment."
                        "An environment is a key-value store where the key is a lisp symbol and the value"
                        "is any lisp value." 
                        ))
        
            (para (list "The rest of this section will now explain the meaning of LBM programs by informally"
                        "showing **expressions**, what **values** they evaluate into and how they change and depend on the environments"
                        ))

            (para (list "**Atoms**"
                        ))
            
            (para (list "Some atoms, such as Numbers, Strings and byte arrays cannot be further evaluated."
                        ))
            (code '((read-eval "10")
                    (read-eval "\"hello world\"")
                    (read-eval "[1 2 3 4]")
                    )
                  )
            
            (para (list "Symbols evaluate by a lookup in the environment."
                        "First, the local environment is searched for a binding of the symbol."
                        "If unable to find a binding in the local environment, the global environment is searched."
                        "If unable to find a binding in the global environment as well, the runtime system attempts to dynamically load"
                        "a binding using a system provided callback function."
                        "If all of the above fails to provide a value a `variable_not_bound` error is produced."
                        ))
            (para (list "**Composite forms**"
                        ))
            (para (list "A composite form, such as `(e1 ... eN)` is evaluated in different ways depending"
                        "on what `e1` is."
                        "There are three major categories that `e1` can fall into. Either `e1` is something that"
                        "represents a function and `(e1 ... eN)` is a function application."
                        "Or `e1` is a so-called *special-form* that form the core of the LBM language."
                        "Or lastly, `e1` is anything else and the composite form is malformed and will ultimately result in an error."
                        ))
            (para (list "The composite form `(e1 ... eN)` is evaluated by first checking if `e1` is a special form or not."
                         "if `e1` is a special form the composite form is passed to a special-form evaluator."
                         "if `e1` is not a special form,  the composite form is evaluated as a function application."
                         "These two major branches of composite form evaluation are described below."
                         ))
            (para (list "**Special form evaluation**"
                        ))
            (para (list "Below are a selection of basic special-forms in lispBM together with their evaluation process"
                        ))
            (bullet (list "**quote**: `(quote a)` evaluates to a for any a"
                          "**define**: `(define s e)`, `e` is evaluated into `v` and the global environment is augmented with the pair `(s . v)`"
                          "**lambda**: `(lambda params body)` is evaluated into '(closure params body env)`. `env` is the local environment there the lambda expression is evaluated."
                          "**if**: `(if e1 e2 e3)` is evaluated by evaluating `e1` into `v1` if `v1` is nil, `e3` is evaluated otherwise `e2` is evaluated."
			  "**progn**: `(progn e1 e2 ... eN)` is evaluated by evaluating `e1` then `e2` and so on until `eN`. The value `v` that `eN` evaluats into is the value `(progn e1 e2 ... eN)` evaluates to."
			  "**and**: `(and e1 e2 ... eN)` evaluates the `eI` expressions from left to right as long as they result in a non-nil value."
			  "**or**: `(or e1 e2 ... eN)` evaluates the `eI` expressions from left to right until there is a non-nil result."
			  ))
	    (para (list "`and`, `or`, `progn` and `if` evaluates expressions in sequence."
			"`if` evaluates first the condition expression and then"
			"either the true or false branch. `progn` evaluates all of the expressions in sequence."
			"In the case of `and`, `or`, `progn` and `if`, the constituent expressions are all evaluated in the same local environment."
			"Any extensions to the local environment performed by an expresison in the sequence is only visible within that expression itself."
			))
	    (bullet (list "**let**: `(let ((s1 e1) (s2 e2) ... (sN eN) e)` eI are evaluated in order into `vI`. The local environment is extended with `(sI . vI)`. `sI` is visible in `eJ` for `J >= I`. `e` is then evaluated in the extended local environment."
			  "**setq**: `(setq s e)' is evaluated by first evaluating `e` into `v`. The environments are then scanned for a bining of `s`. local environment is searched first followed by global. If a binding of `s` is found it is modified into `(s . v)`."
			  ))
	    (para (list "If no binding of `s` is found when evaluating `(setq s e)` a `variable_not_bound` error is triggered."
			))
            ;; (bullet (list "callcc"
            ;;               "atomic"
            ;;               "macro"
            ;;               "closure"
            ;;               "cond"
            ;;               "trap"
            ;;               ))
            (para (list "**Function application evaluation**"
                        ))
            (para (list "The evaluation strategies explained here are applied to composite expressions"
                        "of the `(e1 ... eN)` form."
                        ))
            (para (list "**The quote and the quasiquote**"
                        ))
            (para (list "The LBM parser (Reader) expands usages of the character sequences:"
                        "`'`, `` ` ``, `,` and `,@`."
                        "The `'` as in `'a` is expanded into `(quote a)` for any a."
                        "The remaining `` ` ``, `,` and `,@` are expanded into applications of `quote`, `append` and `cons`"
                        "using the algorithms described by Bawden in [quasiquotation in lisp](https://brics.dk/NS/99/1/BRICS-NS-99-1.pdf#page=6)."
                        ))
            ))
  )

(defun concurrency-and-semantics ()
  (section 3 "Concurrency and Semantics"
           (list
            (para (list "TODO: Finish section."
                        ))
            )
           ))


(define ch-syntax-semantics
  (section 2 "Syntax and semantics"
           (list
            (para (list "Opinions on Lisp syntax varies widely depending on a persons programming experience and preferences."
                        "If you look around, or ask around you could find any of the following, and probably more views on lisp syntax:"
                        ))
            (bullet (list "**Concise and expressive** Lisp syntax is quite minimalist, you can do a lot with very little syntax to learn about."
                          "**Uniform and elegant** Data and code are represented in the same way. This property is called Homoiconicity."
                          "**Too many parenthesis** A common complaint is that it can be easy to get lost in all the parantheses. While it may be easy to write lisp, it can be very hard to read someone elses code."
                          ))
            (para (list "Lisp programs are written using S-expressions, a notation introduced by"
                        "[McCarthy](http://www-formal.stanford.edu/jmc/recursive.pdf). An"
                        "S-expression describes a tree in an unambiguous way."
                        "An example of an S-expression is `(+ 1 2)` and the tree it represents is shown below:"
                        ))
            (s-exp-graph "add_one_two" '(+ 1 2)
                         )
            (para (list "Another example `(+ (* a a) (* b b))` which as a lisp program means $a^2 + b^2$:"
                        ))
            (s-exp-graph "sum_of_squares" '(+ (* a a) (* b b))
                         )
            (para (list "In Lisp, which stands for \"LISt Processor\", a list is a right leaning tree ending in the symbol \"nil\"."
                        "By convention these right leaning expressions are easy to write and requires only a few parentheses. The"
                        "example below shows how the list created by lisp program `(list 1 2 3 4)` is represented as a tree:"
                        ))
            (s-exp-graph "list_1234" (list 1 2 3 4)
                         )
            (para (list "A left leaning structure requires full parenthesization and can be expressed in lisp as"
                        "`(cons (cons (cons (cons nil 4) 3) 2) 1)`."
                        ))
            (s-exp-graph "snoc_1234" (cons (cons (cons (cons nil 4) 3) 2) 1)
                         )
            (para (list "The conventions strongly favor the right leaning case."
                        ))
            (para (list "There are no two different trees that correspond to a given S-expression and thus parsing of S-expressions is unambiguous."
                        "The unambiguous nature of S-expressions is useful in areas other than lisp programming as well."
                        "[KiCad](https://dev-docs.kicad.org/en/file-formats/sexpr-intro/) uses S-expressions to represent tree data in some of its"
                        "file formats. Apperantly [WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format)"
                        "uses S-expressions as well to describe WebAssembly modules"
                        ))
            (newline)
            (para (list "S-expressions are built from two things, **Atoms** and **Pairs** of S-expressions."
                        "So an S-expression is either:"
                        ))
            (newline)
            (bullet (list " An **Atom** *a* "
                          " A **Pair** *a*,*b* of S-expressions `(a . b)` "
                          ))
            (para (list "In LispBM the set of atoms consist of:"
                        ))
            (bullet (list "Numbers: Such as `1`, `2`, `3.14`, `65b`, `2u32`"
                          "Strings: Such as \"hello world\", \"door\" ..."
                          "Byte Arrays: Such as [1 2 3 4 5]"
                          "Symbols: Such as `a`, `lambda`, `define`, `kurt-russel` ..."
                          ))
            (para (list "In LispBM a pair of S-expressions is created by an application of `cons` as `(cons a b)`"
                        "which creates the pair `(a . b)`. Convention is that `(e0 e1 ... eN)` = `(e0 . ( e1 . ... ( eN . nil)))`."
                        ))
            (para (list "A structure such as `(e0 e1 ... eN)` is called a list."
                        ))
            (semantics)
            (concurrency-and-semantics)
            
                      )
           )
  )
           

(define ch-fun-imp
  (section 2 "Functional and Imperative programming"
           (list
            (para (list "To differentiate from Imperative and Functional, think of imperative programs "
                        "as sequences of operations that update a state and "
                        "functional programs as transformations of values through application "
                        "of compositions of functions."
                        "Functional programming languages often let functions be values, which means that functions "
                        "can be stored in lists, returned from other functions and so on"
                        ))
            (para (list "LispBM is a multiparadigm programming language."
                        "Most languages are a mix of functional and imperative and"
                        "differ in what style it makes most convenient."
                        "At one end of this spectrum we find C which makes imperative easy and functional hard, "
                        "and in the other end Haskell with the opposite favouritism."
                        "In LispBM we try to not unfairly favour any particular style over the other."
                        ))
            (para (list "Picking a functional or an imperative style does have consequences though."
                        "Functional LispBM programs have properties such as persistance of data, that"
                        "can be broken using the imperative part of the language."
                        ))
            (para (list "With the imperative features of the language it is also in some "
                        "places possible to peek under the hood of the runtime system."
                        "you can detect when and how environments are shared or copied for example."
                        "Please avoid exploiting the power of destructive updates for evil purposes."
                        ))
            (para (list "The list below shows imperative operations from the core of LispBM."
                        "In the extension libraries there are many more of the kind."
                        ))
            (bullet '("**set**      - Destructively update a binding. Similar to C's ="
                      "**setq**     - Destructively update a binding. Similar to C's ="
                      "**setix**    - Destructive update of element in list."
                      "**setcar**   - Destructive update of car field in cons cell."
                      "**sercdr**   - Destructive update of cdr field in cons cell." 
                      "**setassoc** - Destructive update of field in association list"
                      "**bufset**   - The bufset family of functions destructively updates ByteArrays."
                      "**bufclear** - Destructive clear of ByteArray."
                      "**progn**    - Sequence operations."
                      "**define**   - In LispBM, variables can be defined more than once. A second define of a variable is a destructive update."
                      ))
            )
           ))

;; Arithmetic section

(define arith-add
  (ref-entry "+"
           (list
            (para (list "Adds up an aribtrary number of values. The form of a `+` expression is `(+ expr1 ... exprN)`."
                        ))

            (code '((+ 1 2)
                    (+ 1 2 3 4)
                    (+ 1 1u)
                    (read-eval "(+ 2i 3.14)")
                    ))
            end)))

(define arith-sub
  (ref-entry "-"
           (list
            (para (list "Subtract an arbitrary number of values from a value. The form of a `-` expression is `(- expr1 ... exprN)`."
                        ))
            (code '((- 5 3)
                    (- 10 5 5)
                    (- 10 2u)
                    (read-eval "(- 10 3.14)")
                    ))
            end)))

(define arith-mul
  (ref-entry "*"
           (list
            (para (list "Multiplying an arbitrary number of values. The form of a `*` expression is `(* expr1 ... exprN)`."
                        ))

            (code '((* 2 2)
                    (* 2 3 4 5)
                    (* 10 2u)
                    (read-eval "(* 4 3.14)")
                             ))
            end)))

(define arith-div
  (ref-entry "/"
             (list
              (para (list "Division. The form of a `/` expression is `(/ expr1 ... exprN)`."
                          "The resulting type is the same as the inputs (after their types have been promoted of course)."
                          ))
              (code '((/ 128 2)
                      (read-eval "(/ 6.28 2)")
                      (/ 256 2 2 2 2 2 2 2)
                      (/ 5 2)))
              end)))

(define arith-mod
  (ref-entry "mod"
             (list
              (para (list "Modulo operation. The form of a `mod` expression is `(mod expr1 exp2)`."
                          "The modulo operation is not generalised to n arguments."
                          ))
              (code '((mod 5 3)
                      (mod 1024 100)
                      (mod -7 5)))
              end)))
              
(define arith-int-div
  (ref-entry "//"
             (list
              (para (list "Integer division operation. Like normal division except if the result is a floating point value"
                          "it is cast to an integer, which floors the result. The form of a `//` expression is"
                          "`(// expr1 ... exprN)`."
                          "Can be used as a elegant complement to `mod`, with `//` returning the quotient and `mod`"
                          "returning the remainder of a division."
                          ))
              (code '((// 5.0 2)
                      {
                        (var total-seconds 62.5)
                        (var minutes (// total-seconds 60))
                        (var seconds (mod total-seconds 60))
                        (str-join (list (str-from-n minutes) "m " (str-from-n seconds) "s") "")
                      })
              end))))

(define arithmetic
  (section 2 "Arithmetic"
           (list 'hline
                 arith-add
                 arith-sub
                 arith-mul
                 arith-div
                 arith-mod
                 arith-int-div)
           ))

;; Comaprisons section

(define  comp-eq
  (ref-entry "eq"
            (list
             (para (list "Compare values for equality. The `eq` operation implements structural"
                         "equiality. The form of an 'eq` expression is `(eq expr1 ... exprN)`."
                         "\n"
                         "Structural equality means that the values must have the identical in"
                         "memory representations to be considered equal."
                         ))
             (code '((eq (+ 1 2) 3)
                     (eq 1 1 1 1)
                     (eq 1 1 2 1)
                     (eq (+ 3 4) (+ 2 5) (+ 1 6))
                     (eq (list 1 2 3 4) (list 1 2 3 4))
                     (eq (list 1 2 4 5) (list 1 2 3 4))
                     ))
             end)))

(define comp-not-eq
  (ref-entry "not-eq"
             (list
              (para (list "`not-eq` implements the negation of eq. In other words, `(not-eq a b c)` evaluates to the same result as `(not (eq a b c))`."
                          ))
              (code '((not-eq (+ 1 2) 3)
                      (not-eq 1 1 1 1)
                      (not-eq 1 1 2 1)
                      (not-eq (+ 3 4) (+ 2 5) (+ 1 6))
                      (not-eq (list 1 2 3 4) (list 1 2 3 4))
                      (not-eq (list 1 2 4 5) (list 1 2 3 4))
                      ))
              end)))

(define comp-=
  (ref-entry "="
             (list
              (para (list "The `=` operation can only be used on numerical arguments. If you know you are comparing numbers, it will be more efficient to use `=`."
                          "\n"
                          "An important difference between `eq` and `=` is"
                          "that `=` compare the numerical values of the arguments. A 3 is a 3"
                          "independent of them being different types. `eq` on the other"
                          "hand compares the representations of the arguments exactly and they must"
                          "match in structure, type and value to be considered equal."
                          ))
              (code '((= 1 1)
                      (= 1 2)
                      (= (+ 2 3) (+ 1 4))
                      (= (+ 1 2) (+ 2 3))
                      ))
              end)))

(define comp-!=
  (ref-entry "!="
             (list
              (para (list "The `!=` operation implements the negation of `=`. So, `(!= a b)` evaluates to the same result as `(not (= a b))`."
                          ))
              (code '((!= 1 1)
                      (!= 1 2)
                      (!= (+ 2 3) (+ 1 4))
                      (!= (+ 1 2) (+ 2 3))
                      ))
              end)))

(define comp->
  (ref-entry ">"
             (list
              (para (list "Greater than comparison. A greater than comparison has the form `(> expr1 ... exprN)` and evaluates to `t` if expr1 is greater than all of expr2 ... exprN."
                          ))

              (code '((> 5 2)
                      (> 2 5)
                      (> 3.14 1)
                      (> 1 3.14)
                      ))
              end)))

(define comp-<
  (ref-entry "<"
             (list
              (para (list "Less than comparison. A less than comparison has the form `(> expr1 ... exprN)` and evaluates to `t` if expr1 is less than all of expr2 ... exprN."
                          ))

              (code '((< 5 2)
                      (< 5 2)
                      (read-eval "(< 3.14 1)")
                      (read-eval "(< 1 3.14)")
                      ))
              end)))

(define comp->=
  (ref-entry ">="
             (list
              (para (list "Greater than or equal comparison. A greater than comparison has the form `(>= expr1 ... exprN)` and evaluates to `t` if expr1 is greater than or equal to all of expr2 ... exprN."
                          ))

              (code '((>= 1 1)
                      (>= 5 2)
                      (>= 2 5)
                      (read-eval "(>= 3.14 1)")
                      (read-eval "(>= 1 3.14)")
                      ))
              end)))

(define comp-<=
  (ref-entry "<="
             (list
              (para (list "Less than or equal comparison. A less than or equal comparison has the form `(<= expr1 ... exprN)` and evaluates to `t` if expr1 is less than or equal to all of expr2 ... exprN."
                          ))

              (code '((<= 1 1)
                      (<= 5 2)
                      (<= 2 5)
                      (read-eval "(<= 3.14 1)")
                      (read-eval "(<= 1 3.14)")
                      ))
              end)))

(define comparisons
  (section 2 "Comparisons"
           (list 'hline
                 comp-eq
                 comp-not-eq
                 comp-=
                 comp->
                 comp-<
                 comp->=
                 comp-<=
                 )
           ))

;; Boolean operators

(define bool-and
  (ref-entry "and"
             (list
              (para (list "Boolean `and` operation between n arguments. The form of an `and`"
                          "expression is `(and expr1 ... exprN)`.  This operation treats all"
                          "non-nil values as true. Boolean `and` is \"shirt-circuiting\" and only"
                          "evaluates until a false is encountered."
                          ))
              (code '((and t t)
                      (and t t (+ 1 2))
                      (and t (< 5 3))
                      ))
              end)))

(define bool-or
  (ref-entry "or"
             (list
              (para (list "Boolean `or` operation between n arguments. The form of an `or`"
                          "expression is `(or expr1 ... exprN)`.  This operation treats all"
                          "non-nil values as true. Boolean `or` is \"short-circuiting\" and only"
                          "evaluates until a true is encountered."
                          ))

              (code '((or nil nil)
                      (or nil t)
                      (or t nil)
                      (or t t)
                      (or nil (+ 1 2))
                      ))
              end)))

(define bool-not
  (ref-entry "not"
             (list
              (para (list "Boolean `not` takes one argument. The form of a `not` expression is"
                          "`(not expr)`. All non-nil values are considered true."
                          ))

              (code '((not t)
                      (not nil)
                      (not 42)
                      ))
              end)))



(define boolean
  (section 2 "Boolean operators"
           (list 'hline
                 bool-and
                 bool-or
                 bool-not
                 )
           ))

;; Predicates

(define is-a-list
  (ref-entry "list?"
             (list
              (para (list "the `list?` predicate is true for all lists, empty (nil) or not."
                          ))
              (code '((list? nil)
                      (list? '())
                      (list? (list 1 2 3))
                      (list? '(1 2 3))
                      (list? 2)
                      (list? 'kurt-russel)
                      ))
              end)))

(define is-a-number
  (ref-entry "number?"
             (list
              (para (list "the `number?` predicate is true for all numbers."
                          ))
              (code '((number? nil)
                      (number? 1)
                      (number? 2u)
                      (number? 3.14f32)
                      (number? 'michael-shanks)
                      (number? 'james-spader)
                      ))
              end)))


(define predicates
  (section 2 "Predicates"
           (list 'hline
                 is-a-list
                 is-a-number
                 )
           ))

;; Bitwise operations
(define bit-shl
  (ref-entry "shl"
             (list
              (para (list "The shift left operation takes two arguments. The first argument is a value to shift and the"
                          "second argument is the number of bit positions to shift the value."
                          ))

              (code '((shl 1 2)
                      (shl 1u32 2)
                      (shl 1u64 2)
                      ))

              end)))

(define bit-shr
  (ref-entry "shr"
             (list
              (para (list "The shift right operation takes two arguments. The first argument is a"
                          "value to shift and the second argument in the number of bit positions"
                          "to shift the value."
                          ))
              (code '((shr 4 2)
                      (shr 4u32 2)
                      (shr 4u64 2)))

              end)))

(define bit-and
  (ref-entry "bitwise-and"
             (list
              (para (list "Performs the bitwise and operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-and 1048831u32 0xFFFF)
                      ))

              end)))

(define bit-or
  (ref-entry "bitwise-or"
             (list
              (para (list "Performs the bitwise or operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-or 1048816 0xF)
                      ))
              end)))

(define bit-xor
  (ref-entry "bitwise-xor"
             (list
              (para (list "Performs the bitwise exclusive or operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-xor 1048816 0xFF)
                      ))
              end)))

(define bit-not
  (ref-entry "bitwise-not"
             (list
              (para (list "Performs the bitwise negation operations on a value. The result is of same type as"
                          "the argument."
                          ))
              (code '((bitwise-not 4096u32)
                      ))
              end)))

(define bitwise
  (section 2 "Bit level operations"
           (list 'hline
                 bit-shl
                 bit-shr
                 bit-and
                 bit-or
                 bit-xor
                 bit-not
            )
           ))

;; Nil and t, true and false

(define value-nil
  (ref-entry "nil"
             (list
              (para (list "Represents the empty list. The nil value is also considered to be false by conditionals."
                          "`nil` is a symbol but it cannot be redefined and will always evaluate to itself."
                          ))
              (code '((cons 1 nil)
                      (if nil 3 100)
                      nil
                      ))

              end)))

(define value-t
  (ref-entry "t"
             (list
              (para (list "All non nil values are considered true in conditionals. `t` should be used in cases where an"
                          "explicit true makes sense. `t` is a symbol but it cannot be redefined and will always evaluate to itself."
                          ))
              (code '((cons 1 t)
                      (if t 3 100)
                      t
                      ))
              end)))

(define value-false
  (ref-entry "false"
             (list
              (para (list "`false` is an alias for `nil`."
                          ))
              (code '((read-eval "(cons 1 false)")
                      (read-eval "(if false 3 100)")
                      (read-eval "false")
                      ))
              end)))

(define value-true
  (ref-entry "true"
             (list
              (para (list "`true` is an alias for `t`."
                          ))
              (code '((read-eval "(cons 1 true)")
                      (read-eval "(if true 3 100)")
                      (read-eval "true")
                      ))
              end)))


(define nil-and-t
  (section 2 "nil and t, true and false"
           (list 'hline
                 value-nil
                 value-t
                 value-false
                 value-true
                 )
           ))

;; Quotes and quasiqoutation

(define op-quote
  (ref-entry "quote"
             (list
              (para (list "Usages of the `'` quote symbol in input code is replaced with the"
                          "symbol quote by the reader.  Evaluating a quoted expression, (quote"
                          "a), results in a unevaluated."
                          ))
              (code '((read-eval "'(+ 1 2)")
                      (read-eval "(eval '(+ 1 2))")
                      (read-eval "'kurt")
                      (quote (+ 1 2))
                      (eval (quote (+ 1 2)))
                      (quote kurt)
                      ))
              end)))

(define op-quasi
  (ref-entry "`"
             (list
              (para (list "The backwards tick `` ` `` is called the quasiquote. It is similar to"
                          "the `'` but allows splicing in results of computations using the <a"
                          "href=\"#,\">,</a> and the <a href=\"#commaat\">,@</a> operators."
                          ))
              (para (list "The result of `'(+ 1 2)` and `` `(+ 1 2)`` are similar in effect. Both"
                          "result in the result value of `(+ 1 2)`, that is a list containing +,"
                          "1 and 2.  When `` `(+ 1 2)`` is read into the heap it is expanded into"
                          "the expression `(append (quote (+)) (append (quote (1)) (append (quote"
                          "(2)) (quote nil))))` which evaluates to the list `(+ 1 2)`."
                          ))
              (code '((read-eval "`(+ 1 2)")
                      (read-eval "`(+ 1 ,(+ 1 1))")
                      (append (quote (+ 1)) (list (+ 1 1)))
                      ))
              end)))

(define op-comma
  (ref-entry ","
             (list
              (para (list "The comma is used to splice the result of a computation into a quasiquotation."
                          ))
              (para (list "The expression `` `(+ 1 ,(+ 1 1))`` is expanded by the reader into"
                          "`(append (quote (+)) (append (quote (1)) (append (list (+ 1 1)) (quote nil))))`."
                          "Evaluating the expression above results in the list `(+ 1 2)`."
                          ))
              (code '((read-eval "`(+ 1 ,(+ 1 1))")
                      ))
              end)))

(define op-commaat
  (ref-entry ",@"
             (list
              (para (list "The comma-at operation is used to splice in the result of a computation (that"
                          "returns a list) into a list when quasiquoting."
                          ))
              (code '((read-eval "`(1 2 3 ,@(range 4 10))")
                      ))
              end)))

(define quotes
  (section 2 "Quotes and Quasiquotation"
           (list (para
                  (list "Code and data share the same representation, it is only a matter of"
                        "how you look at it. The tools for changing view, or interpretation,"
                        "are the quotation and quasiquotation operations."
                        ))
                 'hline
                 op-quote
                 op-quasi
                 op-comma
                 op-commaat
                 )))

;; Built-in operations

(define built-in-eval
  (ref-entry "eval"
             (list
              (para (list "Evaluate data as an expression. The data must represent a valid expression."
                          "The form of an `eval` expression is `(eval expr)`."
                          "An optional environment can be passed in as the first argument:"
                          "`(eval env-expr expr)`."
                          ))

              (code '((eval (list + 1 2))
                      (eval '(+ 1 2))
                      (eval '( (a . 100) ) '(+ a 1))
                      (read-eval "(eval `(+ 1 ,@(range 2 5)))")
                      ))
              end)))

(define built-in-eval-program
  (ref-entry "eval-program"
             (list
              (para (list "Evaluate a list of data where each element represents an expression."
                          "The form of an `eval-program` expression is `(eval-program program-expr)`."
                          "A `program-expr` is a list of expressions where each element in the list"
                          "can be evaluated by `eval`."
                          ))
              (para (list "An optional environment can be passed in as the first arguement:"
                          "`(eval-program env-expr program-expr)`."
                          ))
              (code '((eval-program (list (list + 1 2) (list + 3 4)))
                      (eval-program '( (+ 1 2) (+ 3 4)))
                      (eval-program (list (list define 'a 10) (list + 'a 1)))
                      (read-eval "(eval-program '( (define a 10) (+ a 1)))")
                      ))
              end)))

(define built-in-type-of
  (ref-entry "type-of"
             (list
              (para (list "The `type-of` function returns a symbol that indicates what type the"
                          "argument is. The form of a `type-of` expression is `(type-of expr)`."
                          ))
              (code '((type-of 1)
                      (type-of 1u)
                      (type-of 1i32)
                      (type-of 1u32)
                      (type-of 1i64)
                      (type-of 1u64)
                      (type-of 3.14)
                      (type-of 3.14f64)
                      (read-eval "(type-of 'apa)")
                      (type-of (list 1 2 3))
                      ))
              end)))

(define built-in-sym2str
  (ref-entry "sym2str"
             (list
              (para (list "The `sym2str` function converts a symbol to its string representation."
                          "The resulting string is a copy of the original so you cannot destroy"
                          "built in symbols using this function."
                          ))
              (code '((sym2str (quote lambda))
                      (read-eval "(sym2str 'lambda)")
                      ))
              end)))

(define built-in-str2sym
  (ref-entry "str2sym"
             (list
              (para (list "The `str2sym` function converts a string to a symbol."
                          ))
              (code '((str2sym "hello")
                      ))
              end)))

(define built-in-sym2u
  (ref-entry "sym2u"
             (list
              (para (list "The `sym2u` function returns the numerical value used by the runtime system for a symbol."
                          ))
              (code '((sym2u (quote lambda))
                      (read-eval "(sym2u 'lambda)")
                      ))
              end)))

(define built-in-u2sym
  (ref-entry "u2sym"
             (list
              (para (list "The `u2sym` function returns the symbol associated with the"
                          "numerical value provided. This symbol may be undefined in which case you"
                          "get as result a unnamed symbol."
                          ))
              (code '((u2sym 259u)
                      (u2sym 66334u)
                      ))
              end)))

(define built-in-gc
  (ref-entry "gc"
             (list
              (para (list "The `gc` function runs the garbage collector so that it can reclaim"
                          "values from the heap and LBM memory that are nolonger needed."
                          ))
              (para (list "**Note** that one should not need to run this function. GC is run"
                          "automatically when needed."
                          ))
              (code '((gc)
                      ))
              end)))

(define built-in-rest-args
  (ref-entry "rest-args"
             (list
              (para (list "`rest-args` are related to user defined functions. As such `rest-args` is"
                          "also given a brief explanation in the section about the  <a href=\"#lambda\">lambda</a>."
                          ))
              
              (para (list "`rest-args` is a mechanism for handling optional arguments in functions."
                          "Say you want to define a function with 2 arguments and an optional 3rd argument."
                          "You can do this by creating a 3 argument function and check if argument 3 is valid or not in the body of the function"
                          ))
              (code '((defun my-fun (x y opt) (if opt (+ x y opt)
                                                (+ x y)))
                      (my-fun 1 2 nil)
                      (my-fun 1 2 100)
                      ))
              (para (list "This approach works well if your function has 1,2 or some other small number"
                          "of optional arguments. However, functions with many optional arguments will look"
                          "messy at the application site, `(my-fun 1 2 nil nil nil nil 32 nil kurt-russel)` for examples"
                          ))
              (para (list "Functions you create, using lambda or defun, do actually take an arbitrary number of"
                          "arguments. In other words, it is no error to pass in 5 arguments to a 2 argument defun or lambda function."
                          "The extra arguments will by default just be ignored."
                          ))
              (code '((defun my-fun (x y) (+ x y))
                      (my-fun 1 2)
                      (my-fun 1 2 100 200 300 400 500)
                      ))
              (para (list "all of those extra arguments, `100 200 300 400 500` passed into my-fun are"
                          "ignored. But if we want to, we can access these extra arguments through the"
                          "`rest-args` operation."
                          ))
              (code '((defun my-fun (x y) (apply + (cons x (cons y (rest-args)))))
                      (my-fun 1 2 100)
                      (my-fun 1 2 100 1000 10000)
                      ))
              (para (list "`rest-args` gives a clean looking interface to functions taking arbitrary optional arguments."
                          "Functions that make use of `rest-args` must, however, be written specifically to do so and"
                          "are themself responsible for the figuring out the positional semantics of extra arguments."
                          ))
              (para (list "One was to explicitly carry the semantics of an optional argument into the function body"
                          "is to add optional arguments as key-value pairs where the key states the meaning."
                          "Then `rest-args` becomes essentially an association list that you query using `assoc`."
                          "For example:"
                          ))
              (code '((defun my-fun (x) (assoc (rest-args) x))
                      (my-fun 'kurt-russel '(apa . 10) '(bepa . 20) '(kurt-russel . is-great))
                      (my-fun 'apa '(apa . 10) '(bepa . 20) '(kurt-russel . is-great))
                      (my-fun 'bepa '(apa . 10) '(bepa . 20) '(kurt-russel . is-great))
                      ))
              (para (list "The `rest-args` operation also, itself, takes an optional numerical argument that"
                          "acts as an index into the list of rest arguments."
                          ))
              (code '((defun my-fun (i) (rest-args i))
                      (my-fun 0 1 2 3)
                      (my-fun 1 1 2 3)
                      (my-fun 2 1 2 3)
                      ))
                          
              )))

(define built-ins
  (section 2 "Built-in operations"
           (list 'hline
                 built-in-rest-args
                 built-in-eval
                 built-in-eval-program
                 built-in-type-of
                 built-in-sym2str
                 built-in-str2sym
                 built-in-sym2u
                 built-in-u2sym
                 built-in-gc
                 )))

;; Special forms

(define special-form-if
  (ref-entry "if"
             (list
              (para (list "Conditionals are written as `(if cond-expr then-expr else-exp)`.  If"
                          "the cond-expr evaluates to <a href=\"#nil\"> nil </a> the else-expr will"
                          "be evaluated.  for any other value of cond-expr the then-expr will be"
                          "evaluated."
                          ))
              (code '((if t 1 2)
                      (if nil 1 2)
                      ))
              end)))

(define special-form-cond
  (ref-entry "cond"
             (list
              (para (list "`cond` is a generalization of `if` to discern between n different cases"
                          "based on boolean expressions. The form of a `cond` expression is:"
                          "`(cond ( cond-expr1 expr1) (cond-expr2 expr2) ... (cond-exprN exprN))`."
                          "The conditions are checked from first to last and for the first `cond-exprN`"
                          "that evaluates to true, the corresponding `exprN` is evaluated."
                          ))
              (para (list "If no `cond-exprN` evaluates to true, the result of the entire conditional"
                          "is `nil`."
                          ))
              (program  '(((define a 0)
                           (cond ( (< a 0) 'abrakadabra)
                                 ( (> a 0) 'llama)
                                 ( (= a 0) 'hello-world))
                           )
                          ((define a 5)
                           (cond ( (= a 1) 'doughnut)
                                 ( (= a 7) 'apple-strudel)
                                 ( (= a 10) 'baklava)))
                          ))

              end)))

(define special-form-lambda
  (ref-entry "lambda"
             (list
              (para (list "You create an anonymous function with lambda. The function can be given a name by binding the lambda expression using <a href=\"#define\">define</a>"
                          "or <a href=\"#let\">let</a>. A lambda expression has the form `(lambda param-list body-expr)`."
                          ))
              (code '((lambda (x) (+ x 1))
                      ((lambda (x) (+ x 1)) 1)
                      ))
              (para (list "You can give more arguments to a function created using lambda. The extra arguments can be accessed"
                          "in the lambda body by calling the `rest-args` function which gives back auxiliary arguments"
                          "as a list."
                          ))
              (code '(((lambda (x) (cons x (rest-args))) 1 2 3 4 5 6)
                      ((lambda (x) (cons x (rest-args))) 1)
                      ))
              (para (list "`rest-args` takes an optional numerical argument that is used to index into the list containing the rest of the arguments."
                          ))
              (code '(((lambda (x) (rest-args 0)) 1 2 3 4 5)
                      ((lambda (x) (rest-args 1)) 1 2 3 4 5)
                      ((lambda (x) (rest-args 2)) 1 2 3 4 5)
                      ((lambda (x) (rest-args 3)) 1 2 3 4 5)
                      ))
              end)))

(define special-form-closure
  (ref-entry "closure"
             (list
              (para (list "A <a href=\"#lambda\"> lambda </a> expression evaluates into a closure"
                          "which is very similar to a <a href=\"#lambda\">lambda</a> but extended"
                          "with a captured environment for any names unbound in the param-list"
                          "appearing in the body-expr.  The form of a closure is `(closure"
                          "param-list body-exp environment)`."
                          ))
              (code '((lambda (x) (+ x 1))
                      (let ((a 1)) (lambda (x) (+ a x)))
                      (let ((a 1) (b 2)) (lambda (x) (+ a b x)))
                      ))
              end)))

(define special-form-let
  (ref-entry "let"
             (list
              (para (list "Local environments are created using let. The let binding in lispbm"
                          "allows for mutually recursive bindings. The form of a let is `(let"
                          "list-of-bindings body-expr)` and evaluating this expression means that"
                          "body-expr is evaluted in an environment extended with the"
                          "list-of-bindings."
                          ))

              (code '((let ((a 1) (b 2)) (+ a b))
                      (let ((f (lambda (x) (if (= x 0) 0 (g (- x 1)))))
                            (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
                        (f 11))
                      ))
              (para (list "You can deconstruct composite values while let binding."
                          ))
              (code '(( let ( ( (a b) (list 1 2) ) ) (+ a b))
                      ( let ( ( (a . as) (list 1 2 3 4 5 6)) ) (cons a (reverse as) ))
                      ))
              
              
              end)))

(define special-form-loop
  (ref-entry "loop"
             (list
              (para (list "loop allows to repeatedly evaluate an expression for as long as a condition"
                          "holds. The form of a loop is `(loop list-of-local-bindings condition-exp body-exp)`."
                          ))
              (para (list "The  `list-of-local-bindings` are very similar to how `let` works, just that here"
                          "the `body-exp` is repeated."
                          ))

              (program '(((define sum 0)
                          (loop ( (a 0) )  (<= a 10) { (setq sum (+ sum a)) (setq a (+ a 1)) })
                          sum)
                         ))
              end)))

(define special-form-define
  (ref-entry "define"
             (list
              (para (list "You can give names to values in a global scope by using define."
                          "The form of define is `(define name expr)`. The expr is evaluated and it is the"
                          "result of the evaluated expr that is stored in the environment."
                          "In lispbm you can redefine already defined values."
                          ))
              (code '((define apa 10)
                      ))
              end)))

(define special-form-undefine
  (ref-entry "undefine"
             (list
              (para (list "A definition in the global can be removed using undefine.  The form of"
                          "an undefine expression is `(undefine name-expr)` where name-expr"
                          "should evaluate to a symbol (for example `'apa`)."
                          ))
              {
              (define apa 10)
              (code '((undefine 'apa)
                      ))
              }
              (para (list "It is also possible to undefine several bindings at the same time by"
                          "providing a list of names."
                          ))
              {
              (define apa 10)
              (define bepa 20)
              (define cepa 30)
              (code '((undefine '(apa bepa cepa))
                      ))
              }
              end)))

(define special-form-set
  (ref-entry "set"
             (list
              (para (list "The `set` form is used to change the value of some variable in an environment."
                          "You can use `set` to change the value of a global definition or a local definition."
                          "An application of the `set` form looks like `(set var-expr val-expr)` where"
                          "`var-expr` should evaluate to a symbol. The `val-expr` is evaluated before"
                          "rebinding the variable. `set` returns the value that `val-expr` evaluates to."
                          ))
              (program '(((define a 10)
                          (set 'a 20)
                          a)
                         ))
              (para (list "`set` works in local environments too such as in the body of a `let`"
                          "or in a `progn`-local variable created using `var`."
                          ))
              (program '(((progn (var a 10) (set 'a 20) a))
                          ))

              end)))

(define special-form-setq
  (ref-entry "setq"
             (list
              (para (list "The `setq` special-form is similar to `set` and to `setvar` but expects the first argument"
                          "to be a symbol. The first argument to `setq` is NOT evaluated."
                          ))
              (program '(((define a 10)
                          (setq a 20)
                          a)
                         ))
              (para (list "Just like `set` and `setvar`, `setq` can be used on variables that"
                          "are bound locally such as in the body of a `let` or a `progn`-local variable"
                          "created using `var`."
                          ))
              (program '(((progn (var a 10) (setq a 20) a))
                         ))
              end)))

(define special-form-setvar
  (ref-entry "setvar"
             (list
              (para (list "`setvar` is the exact same thing as `set`"
                          ))
              end)))

(define special-form-progn
  (ref-entry "progn"
             (list
              (para (list "The progn special form allows you to sequence a number of expressions."
                          "The form of a progn expression is `(progn expr1 ... exprN)`."
                          ))
              (para (list "The evaluation result of a progn sequence is the value that the last `exprN`"
                          "evaluated to. This is useful for sequencing of side-effecting operations."
                          ))
              (code '((progn 1 2 3)
                      (progn (define a 10) (define b 20) (+ a b))
                      ))
              end)))

(define special-form-brack
  (ref-entry "{"
             (list
              (para (list "The curlybrace `{` syntax is a short-form (syntactic sugar) for `(progn`."
                          "The parser replaces occurrences of `{` with `(progn`. The `{` should be"
                          "closed with an `}`."
                          ))
              (para (list "These two programs are thus equivalent:"
                          ))
              (para (list "```clj\n"
                          "(progn\n"
                          "  (define a 10)\n"
                          "  (define b 20)\n"
                          "  (+ a b))\n"
                          "```\n"
                          ))
              (para (list "And"
                          ))
              (para (list "```clj\n"
                          "{\n"
                          "  (define a 10)\n"
                          "  (define b 20)\n"
                          "  (+ a b)\n"
                          "}\n"
                          "```\n"
                          ))
              end)))

(define special-form-close-brack
  (ref-entry "}"
             (list
              (para (list "The closing curlybrace `}` should be used to close an opening `{` but purely"
                          "for esthetical reasons. The `}` is treated identically to a regular closing parenthesis `)`."
                          ))
              (para (list
                          "The opening `{` and closing `}` curlybraces are used as a short-form for `progn`-blocks"
                          "of sequences expressions."
                          ))
              end)))


(define special-form-var
  (ref-entry "var"
             (list
              (para (list "The var special form allows local bindings in a progn expression. A"
                          "var expression is of the form (var symbol expr) and the symbol `symbol`"
                          "is bound to the value that `expr` evaluates to withing the rest of the progn expression."
                          ))

              (code '((progn (var a 10) (var b 20) (+ a b))
                      (progn (var a 10) (var b (+ a 10)) (+ a b))
                      ))
              (para (list "You can deconstruct composite value while var binding."
                          ))
              (code '((progn (var (a b) (list 1 2)) (+ a b))
                      (progn (var (a . as) (list 1 2 3 4 5 6)) (cons a (reverse as)))
                      ))
              end)))


(define special-form-read
  (ref-entry "read"
             (list
              (para (list "Parses a string resulting in either an expression or the <a href=\"#read_error\">read_error</a> in case"
                          "the string can not be parsed into an expression. The form of a read expression is"
                          "`(read string)`."
                          ))
              (code '((read-eval "(read \"1\")")
                      (read-eval "(read \"(+ 1 2)\")")
                      (read-eval "(read \"(lambda (x) (+ x 1))\")")
                      ))
              end)))


(define special-form-read-program
  (ref-entry "read-program"
             (list
              (para (list "Parses a string containing multiple sequenced expressions. The resulting list of"
                          "expressions can be evaluated as a program using <a href=\"#eval-program\">eval-program</a>."
                          "The form of a read-program expression is `(read-program string)`."
                          ))
              (code '((read-eval "(read-program \"(define apa 1) (+ 2 apa)\")")
                      ))
              end)))

(define special-form-read-eval-program
  (ref-entry "read-eval-program"
             (list
              (para (list "Parses and evaluates a program incrementally. `read-eval-program` reads a top-level expression"
                          "then evaluates it before reading the next."
                          ))
              (code '((read-eval "(read-eval-program \"(define a 10) (+ a 10)\")")
                      ))
              (para (list "`read-eval-program` supports the `@const-start` and `@const-end` annotations which move all"
                          "global definitions created in the program to constant memory (flash)."
                          ))
              (code '((read-eval "(read-eval-program \"@const-start (define a 10) (+ a 10) @const-end\")")
                      ))
              end)))

(define special-form-trap
  (ref-entry "trap"
             (list
              (para (list "`trap` lets you catch an error rather than have the evaluation context terminate."
                          "The form of a trap expression is `(trap expr)`."
                          "If expr crashes with an error `e` then `(trap expr)` evaluates"
                          "to `(exit-error e)`. If expr successfully runs and returns `r`,"
                          "then `(trap expr)` evaluates to (exit-ok r)."
                          ))
              (code '((trap (/ 1 0))
                      (trap (+ 1 2))
                      ))
              (para (list "`trap` catches any error except for fatal errors."
                          "A fatal error will still lead to the context being terminated."
                          ))
              end)))

(define special-forms
  (section 2 "Special forms"
           (list
            (para (list "Special forms looks a lot like functions but they are allowed to"
                        "break the norms when it comes to evaluation order of arguments."
                        "a special form may choose to evaluate or not, freely, from its"
                        "list of arguments."
                        ))
            (list 'hline
                  special-form-if
                  special-form-cond
                  special-form-lambda
                  special-form-closure
                  special-form-let
                  special-form-loop
                  special-form-define
                  special-form-undefine
                  special-form-set
                  special-form-setq
                  special-form-setvar
                  special-form-progn
                  special-form-brack
                  special-form-close-brack
                  special-form-var
                  special-form-read
                  special-form-read-program
                  special-form-read-eval-program
                  special-form-trap
                  )
            )))

;; Lists and cons cells

(define lists-car
  (ref-entry "car"
             (list
              (para (list "Use `car` to access the `car` field of a cons cell. A `car` expression has the form `(car expr)`."
                          ))
              (para (list "Taking the `car` of a number of symbol type is in general a <a href=\"#type_error\">type_error</a>."
                          ))

              (code '((car (cons 1 2))
                      (car (list 9 8 7))
                      ))
              end)))

(define lists-first
  (ref-entry "first"
             (list
              (para (list "`first` is an alternative  name for the `car` operation."
                          "Use `first` to access the first element of a list or pair. A `first` expression  has the form `(first expr)`."
                          ))
              (code '((first (cons 1 2))
                      (first (list 9 8 7))
                      ))
              end)))


(define lists-cdr
  (ref-entry "cdr"
             (list
              (para (list "Use `cdr` to access the `cdr` field of a cons cell. A `cdr` expression has the form `(cdr expr)`."
                          ))
              (code '((cdr (cons 1 2))
                      (cdr (list 9 8 7))
                      ))
              end)))

(define lists-rest
  (ref-entry "rest"
             (list
              (para (list "`rest` is an alternative name for the `cdr` operation."
                          "Use `rest` to access all elements except the first one"
                          "of a list, or to access the second element in a pair. A `rest` expression has the form `(rest expr)`."
                          ))
              (code '((rest (cons 1 2))
                      (rest (list 9 8 7))
                      ))
              end)))

(define lists-cons
  (ref-entry "cons"
             (list
              (para (list "The `cons` operation allocates a cons cell from the heap and populates the"
                          "`car` and the `cdr` fields of this cell with its two arguments."
                          "The form of a `cons` expression is `(cons expr1 expr2)`."
                          "To build well formed lists the innermost cons cell should have"
                          "nil in the cdr field."
                          ))
              (code '((cons 1 (cons 2 (cons 3 nil)))
                      (cons 1 2)
                      (cons + 1)
                      (cons (cons 1 2) (cons 3 4))
                      ))
              end)))

(define lists-dot
  (ref-entry "."
             (list
              (para (list "The dot, `.`, operation creates a pair. The form of a dot expression"
                          "is `(expr1 . expr2)`. By default the evaluator will attempt to evaluate the"
                          "result of `(expr1 . expr2)` unless it is prefixed with `'`."
                          ))
              (code '('(1 . 2)
                      '((1 . 2) . 3)
                      ))
              end)))

(define lists-list
  (ref-entry "list"
             (list
              (para (list "The `list` function is used to create proper lists. The function"
                          "takes n arguments and is of the form `(list expr1 ... exprN)`."
                          ))
              (code '((list 1 2 3 4)
                      ))
              end)))

(define lists-length
  (ref-entry "length"
             (list
              (para (list "Computes the length of a list. The `length` function takes"
                          "one argument and is of the form `(length expr)`."
                          ))
              (code '((length (list 1 2 3 4))
                      ))
              end)))

(define lists-range
  (ref-entry "range"
             (list
              (para (list "The `range` function computes a list with integer values from a"
                          "range specified by its endpoints. The form of a range expression"
                          "is `(range start-expr end-expr)`. The end point in the range is excluded."
                          ))
              (code '((range 4 8)
                      (range 0 10)
                      (range -4 4)
                      ))
              end)))

(define lists-append
  (ref-entry "append"
             (list
              (para (list "The `append` function combines two lists into a longer list."
                          "An `append` expression is of the form `(append expr1 expr2)`."
                          ))
              (code '((append (list 1 2 3 4) (list 5 6 7 8))
                      ))
              end)))

(define lists-ix
  (ref-entry "ix"
             (list
              (para (list "Index into a list using the `ix` function. The form of an `ix` expression"
                          "is `(ix list-expr index-expr)`. Indexing starts from 0 and if you index out of bounds the result is nil."
                          "A negative index accesses values starting from the end of the list."
                          ))
              (code '((ix (list 1 2 3 4) 1)
                      (ix (list 1 2 3 4) -1)
                      ))
              end)))


(define lists-setix
  (ref-entry "setix"
             (list
              (para (list "Destructively update an element in a list. The form of a `setix` expression"
                          "is `(setix list-expr index-extr value-expr)`. Indexing starts from 0 and"
                          "if you index out of bounds the result is nil."
                          "A negative value -n will update the nth value from the end of the list."
                          ))
              (code '((setix (list 1 2 3 4 5) 2 77)
                      (setix (list 1 2 3 4 5) -2 66)
                      ))
              end)))

(define lists-setcar
  (ref-entry "setcar"
             (list
              (para (list "The `setcar` is a destructive update of the car field of a cons-cell."
                          ))
              (program '(((define apa '(1 . 2))
                          (setcar apa 42)
                          apa
                          )
                         ((define apa (list 1 2 3 4))
                          (setcar apa 42)
                          apa
                          )
                         ))
              end)))

(define lists-setcdr
  (ref-entry "setcdr"
             (list
              (para (list "The `setcdr` is a destructive update of the cdr field of a cons-cell."
                          ))
              (program '(((define apa '(1 . 2))
                          (setcdr apa 42)
                          apa
                          )
                         ((define apa (list 1 2 3 4))
                          (setcdr apa (list 99 100))
                          apa
                          )
                         ))
              end)))

(define lists-take
  (ref-entry "take"
             (list
              (para (list "`take` creates a list containing the `n` first elements of another list."
                          "The form of a `take` expression is `(take list-exp n-exp)`."
                          ))
              (program '(((define apa (list 1 2 3 4 5 6 7 8 9 10))
                          (take apa 5)
                          )
                         ))
              end)))

(define lists-drop
  (ref-entry "drop"
             (list
              (para (list "`drop` creates a list from another list by dropping the `n` first elements of that list."
                          "The form of a `drop` expression is `(drop list-exp n-exp)`."
                          ))
              (program '(((define apa (list 1 2 3 4 5 6 7 8 9 10))
                          (drop apa 5)
                          )
                         ))
              end)))

(define lists-reverse
  (ref-entry "reverse"
             (list
              (para (list "`reverse` creates a list containing the same elements as an existing list but in reverse order."
                          "The form of a `reverse` expression is `(reverse list-exp)`."
                          ))
              (program '(((define apa (list 1 2 3 4 5 6 7 8 9 10))
                          (reverse apa)
                          )
                         ))
              end)))

(define lists-rotate
  (ref-entry "rotate"
             (list
              (para (list "`rotate` creates a list containing the same elements as an existing list but rotated some number of step along a direction."
                          "The form of a `rotate` expression is `(rotate list-exp dist-expr)`."
                          "The sign of the value dist-expr evaluates to, decides direction of rotation."
                          ))
              (code '((define apa (list 1 2 3 4 5 6 7 8 9 10))
                      (rotate apa 1)
                      (rotate apa -1)
                      (rotate apa 3)
                      (rotate apa -3)
                      ))
              (para (list "Rotating a list in the negative direction is slightly faster than rotating in the positive direction."
                          "The chart below shows the time 1 Million 3 step rotations take in each direction at varying"
                          "list lengths."
                          "The data is collected on x86."
                          ))
              (image "Performance of list rotate" "images/rotate_pos_neg.png")
              end)))


(define lists-merge
  (ref-entry "merge"
             (list
              (para (list "`merge` merges two lists that are ordered according to a comparator into"
                          "a single ordered list. The form of a `merge` expression is `(merge comparator-exp list-exp1 list-exp2)`."
                          ))
              (program '(((define a (list 2 4 6 8 10 12))
                          (define b (list 1 3 5))
                          (merge < a b)
                          )
                         ))
              end)))

(define lists-sort
  (ref-entry "sort"
             (list
              (para (list "`sort` orders a list of values according to a comparator. The sorting"
                          "algorithm used is an in-place merge-sort. A copy of the input list is created"
                          "at the beginning of the sort to provide a functional interface from the user's"
                          "point of view. The form of a sort expression is `(sort comparator-exp list-exp)`"
                          ))
              (program '(((define a (list 1 9 2 5 1 8 3))
                          (sort < a)
                          )
                         ))
              end)))



(define lists
  (section 2 "Lists and cons cells"
           (list
            (para (list "Lists are built using cons cells. A cons cell is represented by the lbm_cons_t struct in the"
                        "implementation and consists of two fields named the `car` and the `cdr`."
                        "There is no special meaning associated with the `car` and the `cdr` each can hold"
                        "a lbm_value. See <a href=\"#cons\">cons</a> and <a href=\"#list\">list</a> for two ways to create structures of"
                        "cons cells on the heap."
                        ))
            (image "cons cell" "images/cons_cell.png")
            (para (list "A cons cell can be used to store a pair of values. You create a pair by"
                        "sticking a value in both the car and cdr field of a cons cell using either `'(1 . 2)` or"
                        "`(cons 1 2)`."
                        ))
            (image "pair" "images/pair.png")
            (para (list "A list is a number of cons cells linked together where the car fields hold values"
                        "and the cdr fields hold pointers (the last cdr field is nil). The list below"
                        "can be created either as `'(1 2 3)` or as `(list 1 2 3)`."
                        ))
            (image "list" "images/list.png")
            lists-car
            lists-first
            lists-cdr
            lists-rest
            lists-cons
            lists-dot
            lists-list
            lists-length
            lists-range
            lists-append
            lists-ix
            lists-setix
            lists-setcar
            lists-setcdr
            lists-take
            lists-drop
            lists-reverse
            lists-rotate
            lists-merge
            lists-sort
            )))

;; Association lists

(define assoc-acons
  (ref-entry "acons"
             (list
              (para (list "The `acons` form is similar to `cons`, it attaches one more element"
                          "onto an alist. The element that is added consists of a key and a value"
                          "so `acons` takes one more argument than `cons`. The form of an"
                          "`acons` expression is `(acons key-expr val-expr alist-expr)`."
                          "The `alist-expr` should evaluate to an alist but there are no checks"
                          "to ensure this."
                          ))
              (para (list "Example that adds the key `4` and associated value `lemur` to"
                          "an existing alist."
                          ))
              (code '((acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark)))
                      ))
              end)))
(define assoc-assoc
  (ref-entry "assoc"
             (list
              (para (list "The `assoc` function looks up the first value in an alist matching a given a key. "
                          "The form of an `assoc` expression is `(assoc alist-expr key-expr)`"
                          ))
              (code '((assoc (list '(1 . horse) '(2 . donkey) '(3 . shark)) 2)
                      ))
              end)))

(define assoc-cossa
  (ref-entry "cossa"
             (list
              (para (list "The `cossa` function looks up the first key in an alist that matches a given value."
                          "The form of an `cossa` expression is `(cossa alist-expr value-expr)`"
                          ))
              (code '((cossa (list '(1 . horse) '(2 . donkey) '(3 . shark)) 'donkey)
                      ))
              end)))

(define assoc-setassoc
  (ref-entry "setassoc"
             (list
              (para (list "The `setassoc` function destructively updates a key-value mapping in an"
                          "alist. The form of a `setassoc` expression is `(setassoc alist-expr key-expr value-expr)`."
                          ))
              (program '(((define apa (list '(1 . horse) '(2 . donkey) '(3 . shark)))
                          (setassoc apa 2 'llama)
                          )
                         ))
              end)))





(define assoc-lists
  (section 2 "association lists (alists)"
           (list
            (para (list "Association lists (alists) are, just like regular lists, built out"
                        "of cons-cells. The difference is that an alist is a list of pairs"
                        "where the first element in each par can be thought of as a key and"
                        "the second element can be thought of as the value. So alists implement"
                        "a key-value lookup structure."
                        ))
            (para (list "`(list '(1 . horse) '(2 . donkey) '(3 . shark))` is an example"
                        "of an alist with integer keys and symbol values."
                        ))
            assoc-acons
            assoc-assoc
            assoc-cossa
            assoc-setassoc
            )))

;; Arrays Byte buffers

(define arrays-bufcreate
  (ref-entry "bufcreate"
             (list
              (para (list "Create an array of bytes. The form of a `bufcreate` expression is `(bufcreate size-expr)`"
                          ))
              (code '((define data (bufcreate 10))
		      (define empty-array (bufcreate 0))
                      ))
              end)))

(define arrays-buflen
  (ref-entry "buflen"
             (list
              (para (list "Returns the size of a buffer in number of bytes. The form"
                          "of an `buflen` expression is `(buflen buf-expr)` where"
                          "buf-expr has to evaluate into a buffer."
                          ))
              (code '((buflen data)
                      ))
              end)))

(define arrays-bufget
  (ref-entry "bufget-[X]"
             (list
              (para (list "Read a value from a buffer. The contents of a buffer can be read"
                          "as a sized integer or unsigned value using as many bytes from the buffer"
                          "as the X portion of the function name implies."
                          "The form of a bufget expression is `(bufget-[X] buf-expr ix-expr)` where"
                          "`ix-expr` evaluates to a number indicating the byte position to start"           
                          "reading from."
                          ))
              (code '((define data [0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])
                      (bufget-i8 data 0)
                      (bufget-i16 data 0)
                      (bufget-i32 data 0)
                      (bufget-u8 data 0)
                      (bufget-u16 data 0)
                      (bufget-u32 data 0)
                      ))
              end)))

(define arrays-bufset
  (ref-entry "bufset-[X]"
             (list
              (para (list "The `bufset` functions performs a destructive updates to a buffer."
                          "The form of a `bufset` expression is `(bufset-[X] buf-expr ix-expr val-expr)`"
                          "where `ix-expr` evaluates to a number indicating where in the buffer to"
                          "start writing and `val-expr` is the value to write."
                          ))
              (code '((define data [0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])
                      (bufset-i8 data 0 10)
                      data
                      (bufset-i16 data 0 20)
                      data
                      (bufset-i32 data 0 -1)
                      data
                      (bufset-u8 data 0 10)
                      data
                      (bufset-u16 data 0 20)
                      data
                      (bufset-u32 data 0 0xFFFFFFFFu32)
                      data
                      ))
              end)))

(define arrays-bufclear
  (ref-entry "bufclear"
             (list
              (para (list "To clear a byte array the function bufclear can be used `(bufclear arr optByte optStart optLen)`"
                          "Where arr is the byte array to clear, optByte is the optional argument"
                          "of what to clear with (default 0), optStart is the optional argument"
                          "of which position to start clearing (default 0) and optLen is the"
                          "optional argument of how many bytes to clear after start (default the"
                          "entire array). Example:"
                          ))
              (code '((define data [0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF])
                      (bufclear data)
                      data
                      (bufclear data 0xff)
                      data
                      (bufclear data 1 5)
                      data
                      (bufclear data 1 5 8)
                      data
                      (bufclear data 0xAA 1 5)
                      data
                      ))
              end)))

(define arrays-literal
  (ref-entry "Byte-array literal syntax"
             (list
              (para (list "Byte-array (buffer) literals can be created using the `[` and `]` syntax to enclose"
                          "values to initialize the array with. The `[` and `]` syntax is complete"
                          "resolved in the parser and thus cannot contain arbitrary lisp terms."
                          "the values listed between the `[` and the `]` must be literals!"
                          ))
              (para (list "The form of the `[` and `]` syntax is `[ val1 ... valN ]`."
                          ))
              (code '([1 2 3 4 5 6 7 8 9 10]
                      ))
              end)))

(define arrays
  (section 2 "Arrays (byte buffers)"
           (list 'hline
                 arrays-bufcreate
                 arrays-buflen
                 arrays-bufget
                 arrays-bufset
                 arrays-bufclear
                 arrays-literal
                 )))

;; Defragmentable memory

(define defrag-introduction
    (list 
     (para (list "LBM has two types of memory, the HEAP and the LBM_MEMORY. Lists and pairs are all stored on the heap."
		 "Arrays and large values (such as 64bit numbers are stored on LBM_MEMORY."
		 "The HEAP has a nice property that all allocations on it are the same size and therefore the HEAP is imune"
		 "the problems caused by fragmentation."
		 "On LBM_MEMORY arbitrarily sized arrays can be allocated and fragmentation can cause an allocation to fail even"
		 "though there is enough free bytes."
		 ))
     (para (list "One way to resolve the fragmentation problem is to use a compacting garbage collector."
		 "We have opted to not use a compacting garbage collector on the LBM_MEMORY as it is quite complicated."
		 "It is extra complicated given how this memory is a shared resource between C extensions and the lisp runtime system."
		 ))
     (para (list "Our solution is to allow the programmer to create a memory block inside of the LBM_MEMORY in which we will run a defragmentation"
		 "routine when needed. The defragmentable memory can only be used to allocate non-zero sized byte arrays on the lisp side."
		 "The idea is that the programmer calculates the maximum size of simultaneously used arrays (+ the overhead of 3 words per allocation)"
		 "needed for a small critical set of arrays used in the program and allocates a defragmentable memory of that size."
		 ))
     (para (list "The LBM (non-compacting) gabage collector frees arrays from a defragmentable memory area automatically."
		 "An allocation in the defragmentable memory area that fails triggers garbage collection followed by compaction (if needed)."
		 ))
     )
  )

(define defrag-dm-create
    (ref-entry "dm-create"
	       (list
		(para (list "`dm-create` creates a region of defragmentable memory for bytearrays within LBM memory."
			    "The form of a `dm-create` expression is `(dm-create size-expr)`." 
			    ))
		(code '((define dm (dm-create 1000))))
		)))

(define defrag-dm-alloc
    (ref-entry "dm-alloc"
	       (list
		(para (list "`dm-alloc` is used to allocate a byte-array from a region of defragmentable memory."
			    "The form of a `dm-alloc` expression is `(dm-alloc DM-expr size-expr)`."
			    "where `DM-expr` evaluates to the defragmentable region to allocate from and `size-expr` is the number of bytes to allocate."
			    "Each allocation uses up 12 extre bytes of header that you do not include in `size-expr`."
		      ))
		(code '((define arr10 (dm-alloc dm 10))
			(define arr100 (dm-alloc dm 100))))
	       )))

(define defrag-mem
    (section 2 "Defragmentable memory"
	     (list defrag-introduction
		   'hline
		   defrag-dm-create
		   defrag-dm-alloc
		   )))


;; Pattern matching

(define pm-match
  (ref-entry "match"
             (list
              (para (list "Pattern-matching is expressed using match. The form of a match expression is"
                          "`(match expr (pat1 expr1) ... (patN exprN))`. Pattern-matching compares"
                          "the shape of an expression to each of the `pat1` ... `patN`"
                          "and evaluates the expression `exprM` of the pattern that matches."
                          "In a pattern you can use a number of match-binders or wildcards: `_`, `?`, `?i`,`?u`,`?float`."
                          ))
              (code '((match 'orange (green 1) (orange 2) (blue 3))
                      ))
              end)))

(define pm-no_match
  (ref-entry "no_match"
             (list
              (para (list "The `no_match` symbol is returned from pattern matching if no case matches the expression."
                          ))
              (bullet '("Add a catch-all case to your pattern-matching. `_`.")
                      )
              end)))

(define pm-_
  (ref-entry "_"
             (list
              (para (list "The underscore pattern matches anything."
                          ))
              (code '((match 'fish (horse 'its-a-horse) (pig 'its-a-pig) (_ 'i-dont-know))
                      ))
              end)))

(define pm-?
  (ref-entry "?"
             (list
              (para (list "The `?` pattern matches anything and binds that anything to variable."
                          "Using the `?` pattern is done as `(? var)` and the part of the expression"
                          "that matches is bound to `var`."
                          ))
              (code '((match '(orange 17) ((green (? n)) (+ n 1)) ((orange (? n)) (+ n 2)) ((blue (? n)) (+ n 3)))
                      ))
              end)))

(define pm-guards
  (ref-entry "Match with guards"
             (list
              (para (list "Patterns used in a match expressions can be augmented with a boolean"
                          "guard to further discern between cases. A pattern with a guard is of the"
                          "form `(pattern-expr guard-expr expr)`. A pattern with a guard, matches only"
                          "if the pattern structurally matches and if the guard-expr evaluates to true"
                          "in the match environment."
                          ))
              (code '((define x 1)
                      (match x
                             ( (? y) (< y 0) 'less-than-zero)
                             ( (? y) (> y 0) 'greater-than-zero)
                             ( (? y) (= y 0) 'equal-to-zero))
                      ))
              end)))

(define pattern-matching
  (section 2 "Pattern-matching"
           (list 'hline
                 pm-match
                 pm-no_match
                 pm-_
                 pm-?
                 pm-guards
                 )))

;; Concurrency

(define conc-spawn
  (ref-entry "spawn"
             (list
              (para (list "Use `spawn` to launch a concurrent process. Spawn takes a closure and"
                          "arguments to pass to that closure as its arguments. The form of a"
                          "spawn expression is `(spawn opt-name opt-stack-size closure arg1"
                          "... argN)`."
                          ))
              (para (list "Each process has a runtime-stack which is used for the evaluation of"
                          "expressions within that process. The stack size needed by a process"
                          "depends on"
                          " 1. How deeply nested expressions evaluated by the process are."
                          " 2. Number of recursive calls (Only if a function is NOT tail-recursive)."
                          " 3. The Number of arguments that functions called by the process take."
                          ))
              (para (list "Having a stack that is too small will result in a `out_of_stack` error."
                          ))
              (para (list "The default stack size is 256 words (1K Bytes) and should be more than"
                          "enough for reasonable programs. Many processes will work perfectly"
                          "fine with a lot less stack. You can find a good size by trial and error."
                          ))
              end)))

(define conc-spawn-trap
  (ref-entry "spawn-trap"
             (list
              (para (list "Use `spawn-trap` to spawn a child process and enable trapping of exit"
                          "conditions for that child. The form of a `spawn-trap` expression is"
                          "`(spawn-trap opt-name opt-stack-size closure arg1 .. argN)`.  If the"
                          "child process is terminated because of an error, a message is sent to"
                          "the parent process of the form `(exit-error tid err-val)`. If the"
                          "child process terminates successfully a message of the form `(exit-ok"
                          "tid value)` is sent to the parent."
                          ))
              (program '(((defun thd () (+ 1 2))
                          (spawn-trap thd)
                          (recv ((exit-error (? tid) (? e)) 'crash)
                                ((exit-ok    (? tid) (? v)) 'ok))
                          )
                         ((defun thd () (+ 1 kurt-russel))
                          (spawn-trap thd)
                          (recv ((exit-error (? tid) (? e)) 'crash)
                                ((exit-ok    (? tid) (? v)) 'ok))
                          )
                         ))
              end)))

(define conc-self
  (ref-entry "self"
             (list
              (para (list "Use `self` to obtain the thread-id of the thread in which `self` is evaluated."
                          "The form of a `self` expression is `(self)`. The thread id is of an integer type."
                          ))
              (code '((self)
                      ))
              end)))

(define conc-wait
  (ref-entry "wait"
             (list
              (para (list "Use `wait` to wait for a spawned process to finish."
                          "The argument to `wait` should be a process id."
                          "The `wait` blocks until the process with the given process id finishes."
                          "When the process with with the given id finishes, the wait function returns True."
                          ))
              (para (list "Be careful to only wait for processes that actually exist and do"
                          "finish. Otherwise you will wait forever."
                          ))
              end)))

(define conc-yield
  (ref-entry "yield"
             (list
              (para (list "To put a process to sleep, call `yield`. The argument to `yield`"
                          "is number indicating at least how many microseconds the process should sleep."
                          ))
              (code '((yield 10)
                      ))
              end)))
(define conc-sleep
  (ref-entry "sleep"
             (list
              (para (list "'sleep' puts a thread to sleep and differs from 'yield' only in the argument."
                          "'sleep' takes a floating point number indicating how long in seconds the thread"
                          "should sleep at least."
                          ))
              (code '((sleep 1.0)
                      ))
              end)))

(define conc-atomic
  (ref-entry "atomic"
             (list
              (para (list "`atomic` can be used to execute a LispBM one or more expression without allowing"
                          "the runtime system to switch process during that time. `atomic` is similar to"
                          "progn with the addition of being uninterruptable."
                          ))
              (code '((atomic (+ 1 2) (+ 3 4) (+ 4 5))
                      ))
              end)))

(define conc-exit-ok
  (ref-entry "exit-ok"
             (list
              (para (list "The `exit-ok` function terminates the thread in a \"successful\" way and"
                          "returnes a result specified by the programmer. The form of an"
                          "`exit-ok` expression is `(exit-ok value)`.  If the process that calls"
                          "`exit-ok` was created using `spawn-trap` a message of the form"
                          "`(exit-ok tid value)` is be sent to the parent of this process."
                          ))
              end)))

(define conc-exit-error
  (ref-entry "exit-error"
             (list
              (para (list "The `exit-error` function terminates the thread with an error"
                          "specified by the programmer.  The form of an `exit-error` expression"
                          "is `(exit-error err_val)`. If the process that calls `exit-error` was"
                          "created using `spawn-trap` a message of the form `(exit-error tid"
                          "err_val)` is sent to the parent of this process."
                          ))
              end)))

(define conc-kill
  (ref-entry "kill"
             (list
              (para (list "The `kill` function allows you to force terminate"
                          "another thread. It has the signature `(kill thread-id-expr val-expr)`,"
                          "where `thread-id-expr` is the thread that you want to terminate,"
                          "and `val-expr` is the final result the thread dies with."
                          ))
              (program '(((defun f () (f))
                          (define id (spawn f))
                          (kill id nil)
                          )
                         ))
              (para (list "The `val-expr` can be observed if the thread exit status is captured using `spawn-trap`"
                          ))
              (program '(((defun f () (f))
                          (define id (spawn-trap f))
                          (kill id 'kurt-russel)
                          (recv (( ? x) x))
                          )
                         ))
              (para (list "The `val-expr` could be used to communicate to a thread monitor that the"
                          "thread it monitors has been intentionally but externally killed."
                          ))
              
              end)))



(define concurrency
  (section 2 "Concurrency"
           (list
            (para (list "The concurrency support in LispBM is provided by the set of functions,"
                        "`spawn`, `wait`, `yeild` and `atomic` described below.  Concurrency in"
                        "LispBM is scheduled by a round-robin scheduler that splits the runtime"
                        "system evaluator fairly (with caveats, below) between all running processes."
                        ))
            (para (list "When a process is scheduled to run, made active, it is given a quota of"
                        "evaluator \"steps\" to use up. The process then runs until that quota is"
                        "exhausted or the process itself has signaled it wants to sleep by"
                        "yielding or blocking (for example by waiting for a message using the"
                        "message passing system)."
                        ))
            (para (list "A process can also request to not be \"pre-empted\" while executing a"
                        "certain expression by invoking `atomic`. One should take care to make"
                        "blocks of atomic code as small as possible as it disrupts the fairness"
                        "of the scheduler. While executing inside of an atomic block the process"
                        "has sole ownership of the shared global environment and can perform"
                        "atomic read-modify-write sequences to global data."
                        ))
            'hline
            conc-spawn
            conc-spawn-trap
            conc-self
            conc-wait
            conc-yield
            conc-sleep
            conc-atomic
            conc-exit-ok
            conc-exit-error
            conc-kill
            )
           ))

(define mp-send
  (ref-entry "send"
             (list
              (para (list "Messages can be sent to a process by using `send`. The form"
                          "of a `send` expression is `(send pid msg)`. The message, msg,"
                          "can be any LispBM value."
                          ))
              end)))

(define mp-recv
  (ref-entry "recv"
             (list
              (para (list "To receive a message use the `recv` command. A process"
                          "will block on a `recv` until there is a matching message in"
                          "the mailbox."
                          "The `recv` syntax is very similar to [match](#match)."
                          ))
              (program '(((send (self) 28)
                          (recv ((? n) (+ n 1)))
                          )
                         ))
              end)))

(define mp-recv-to
  (ref-entry "recv-to"
             (list
              (para (list "Like [recv](#recv), `recv-to` is used to receive"
                          "messages but `recv-to` takes an extra timeout argument."
                          "It then receives a message containing the symbol"
                          "`timeout` after the timeout period ends."
                          ))
              (para (list "The form of an `recv-to` expression is"
                          "```clj"
                          "(recv-to timeout-secs"
                          "                (pattern1 exp1)"
                          "                ..."
                          "                (patternN expN))"
                          "```"
                          ))
              (program '(((send (self) 28)
                          (recv-to 0.1
                                   ((? n) (+ n 1))
                                   (timeout 'no-message))

                          )
                         ))
              (program '(((send (self) 'not-foo)
                          (recv-to 0.1
                                   (foo 'got-foo)
                                   (timeout 'no-message))

                          )
                         ))
              end)))

(define mp-set-mailbox-size
  (ref-entry "set-mailbox-size"
             (list
              (para (list "Change the size of the mailbox in the current process."
                          "Standard mailbox size is 10 elements."
                          ))
              (code '((set-mailbox-size 100)
                      (set-mailbox-size 5000000)
                      ))
              end)))

(define message-passing
  (section 2 "Message-passing"
           (list
            mp-send
            mp-recv
            mp-recv-to
            mp-set-mailbox-size
            )
           ))

;; Flat values

(define fv-flatten
  (ref-entry "flatten"
             (list
              (para (list "The `flatten` function takes a value as single argument and returns the flat representation if"
                          "successful. A flatten expression has the form `(flatten expr)`. Note that `expr` is evaluated"
                          "before the flattening. A flat value can be turned back into a normal lisp value applying `unflatten`"
                          ))
              (code '((define a (flatten (+ 1 2 3)))
                      (unflatten a)
                      (define a (flatten '(+ 1 2 3)))
                      (unflatten a)
                      ))
              (para (list "A flat value is a byte-array containing an encoding of the value."
                          ))
              end)))

(define fv-unflatten
  (ref-entry "unflatten"
             (list
              (para (list "`unflatten` converts a flat value back into a lisp value. Te form of an"
                          "`unflatten` expression is `(unflatten flat-value)`"
                          ))
              (code '((define a (flatten (+ 1 2 3)))
                      (unflatten a)
                      (define a (flatten '(+ 1 2 3)))
                      (unflatten a)
                      ))
              end)))

(define flat-values
  (section 2 "Flat values"
           (list
            (para (list "Lisp values can be \"flattened\" into an array representation. The flat"
                        "representation of a value contains all information needed so that the"
                        "value can be recreated, \"unflattened\", in another instance of the"
                        "runtime system (for example running on another microcontroller)."
                        ))
            (para (list "Not all values can be flattened, custom types for example cannot."
                        ))
            (para (list "Flat values are designed for recursive encoding and decoding"
                        "each sub-value contains all information about its size either implicitly"
                        "or explicitly (as is the case with arrays)."
                        ))
            (para (list "multibyte values are stored in network byte order (big endian)."
                        ))
            (para (list "**Cons** A cons cell is encoded into a byte 0x1 followed by the encoding of the"
                        "car and then the cdr field of that cons cell."
                        ))
            (table '("cons " "car value" "cdr value")
                   '(("0x1" "M bytes" "N bytes")))
            (para (list "**Symbol as value** A symbol value can be flattened. Note that symbol values"
                        "only make sense locally. A flattened symbol value will only make sense in the same"
                        "runtime system instance that flattened it."
                        ))
            (table '("symbol-value" "value")
                   '(("0x2" "4 bytes on 32bit, 8 bytes on 64bit")))
            (para (list "**Symbol as string** A symbol can be flattened as a string and thus make sense across"
                        "runtime system instances."
                        ))
            (table '("symbol-string" "string")
                   '(("0x3" "zero terminated C style string")))
            (para (list "**Byte Arrays** Byte arrays can be flattened and the length is stored explicitly."
                        ))
            (table '("byte array" "size in bytes" "data")
                   '(("0xD" "4 bytes" "size bytes")))

            (para (list "The rest of the atomic types are flattened according to the following:"
                        ))
            (table '("type" "flat-id" "value")
                   '(("byte" "0x4" "1 Byte")
                     ("i28"  "0x5" "4 Bytes")
                     ("u28"  "0x6" "4 Bytes")
                     ("i32"  "0x7" "4 Bytes")
                     ("u32"  "0x8" "4 Bytes")
                     ("float" "0x9" "4 Bytes")
                     ("i64"  "0xA"  "8 Bytes")
                     ("u64"  "0xB"  "8 Bytes")
                     ("double" "0xC" "8 Bytes")
                     ("i56"  "0xE" "8 Bytes")
                     ("u56"  "0xF" " 8 Bytes")))
            (para (list "Note that some of the types are only present of 32Bit runtime systems and"
                        "some only on 64 bit."
                        "i28 is present on 32 bit and i56 on 64 bit. likewise for u28 and u56."
                        ))
            (para (list "When LispBM unflattens a i56 or u56 on a 32bit system it creates a i64 or u64"
                        "in its place."
                        ))
            (para (list "Symbols as values, are not possible to transfer between runtime systems in general"
                        "and is even more pointless between a 32 and 64 bit runtime system."
                        ))
            fv-flatten
            fv-unflatten
            )
           ))

;; MACRO!

(define m-macro
  (ref-entry "macro"
             (list
              (para (list "The form of a `macro` expression is: `(macro args body)`"
                          ))
              (code '((read-eval "(define defun (macro (name args body)\n                    `(define ,name (lambda ,args ,body))))")
                      (defun inc (x) (+ x 1))
                      (inc 1)
                      ))
              end)))

(define macros
  (section 2 "Macros"
           (list
            (para (list "lispBM macros are created using the `macro` keyword. A macro"
                        "is quite similar to [lambda](#lambda) in lispBM except that"
                        "arguments are passed in unevaluated. Together with the code-splicing"
                        "capabilities given by [quasiquotation](#quasiquotation), this"
                        "provides a powerful code-generation tool."
                        ))
            (para (list "A macro application is run through the interpreter two times. Once to"
                        "evaluate the body of the macro on the unevaluated arguments. The result of"
                        "this first application should be a program. The resulting program then goes"
                        "through the interpreter again to compute final values."
                        ))
            (para (list "Given this repeated evaluation, macros are not a performance boost in"
                        "lispbm.  Macros are really a feature that should be used to invent new"
                        "programming abstractions in cases where it is ok to pay a little for"
                        "the overhead for benefits in expressivity."
                        ))
            m-macro
            )
           ))

(define cc
  (section 2 "Call with current continutation"
           (list
            (para (list "\"Call with current continuation\" is called `call-cc` in LBM."
                        "Call with current continuation saves the \"current continuation\", which encodes what"
                        "the evaluator will do next, into an object in the language. This encoded"
                        "continuation object behaves as a function taking one argument."
                        ))
            (para (list "The `call-cc` should be given a function, `f`, as the single argument. This"
                        "function, `f`, should also take a single argument, the continuation."
                        "At any point in the body of `f` the continuation can be applied to"
                        "a value, in essense replacing the entire `call-cc` with that value. All side-effecting operations"
                        "operations up until the application of the continuation will take effect."
                        ))
            (para (list "From within a `call-cc` application it is possible to bind the continuation to a global variable"
                        "which will allow some pretty arbitrary control flow."
                        ))
            (para (list "The example below creates a macro for a `progn` facility that"
                        "allows returning at an arbitrary point.\n"
                        "```clj\n"
                        "(define do (macro (body)\n"
                        "                  `(call-cc (lambda (return) (progn ,@body)))))\n"
                        "```\n"
                        "The example using `do` below makes use of `print` which is not a"
                        "built-in feature of lispBM. There are just to many different ways a programmer may"
                        "want to implement `print` on an microcontroller. Use the lispBM extensions"
                        "framework to implement your own version of `print`\n"
                        "```clj\n"
                        "(do ((print 10)\n"
                        "     (return 't)\n"
                        "     (print 20)))\n"
                        "```\n"
                        "In the example above only \"10\" will be printed."
                        "Below is an example that conditionally returns.\n"
                        "```clj\n"
                        "(define f (lambda (x)\n"
                        "            (do ((print \"hello world\")\n"
                        "                 (if (= x 1)\n"
                        "                     (return 't)\n"
                        "                     nil)\n"
                        "                 (print \"Gizmo!\")))))\n"
                        "```\n"
                        ))
            )
           ))

;; Error handling

(define error-handling
  (section 2 "Error handling"
           (list
            (para (list "If an error occurs while evaluating a program, the process that runs"
                        "that program is killed.  The result of the killed process is set to an"
                        "error symbol indicating what went wrong."
                        ))
            (para (list "If the process was created using `spawn` (or equivalently, started by a"
                        "issuing a command in the repl), the process dies and an error message"
                        "is presented over the registered printing callback (dependent on how LispBM"
                        "is integrated into your system). The `ctx_done_callback` is also called"
                        "and performs other integration dependent tasks related to the shutting down"
                        "of a process."
                        ))
            (para (list "If the process was created using `spawn-trap`, in addition to the"
                        "above, a message is sent to the parent process (the process that"
                        "executed the spawn-trap) containing information about the process that"
                        "struck an error. See <a href=\"#spawn-trap\">spawn-trap</a>."
                        "The parent process can now choose to restart the process that crashed"
                        "or to take some other action."
                        ))
            (para (list "Another way to catch errors is to use `trap` which works similar to `spawn-trap`"
                        "but it does not spawn a thread."
                        "`trap` takes one argument which is an expressions. The expression is evaluated and if"
                        "it fails `(trap expr)` returns an object representing the error."
                        "For more information on `trap`, see  <a href=\"#trap\">trap</a>."
                        ))
            (ref-entry "read_error"
                       (list
                        (para (list "The `read_error` symbol is returned if the reader cannot parse the input code."
                                    "Read errors are most likely caused by syntactically incorrect input programs."
                                    ))
                        (bullet '("Check that all opening parenthesis are properly closed."))
                        end))
            (ref-entry "type_error"
                       (list
                        (para (list "The `type_error` symbol is returned by built-in functions or extensions"
                                    "if the values passed in are of incompatible types."
                                    ))
                        end))
            (ref-entry "eval_error"
                       (list
                        (para (list "The `eval_error` symbol is returned if evaluation could"
                                    "not proceed to evaluate the expression. This could be because the"
                                    "expression is malformed."
                                    ))
                        (para (list "Evaluation error happens on programs that may be syntactically correct"
                                    "(LispBM has a very low bar for what is considered syntactically correct),"
                                    "but semantically nonsensical."
                                    ))
                        (bullet '("Check the program for mistakes."
                                  "Are your parenthesis enclosing the correct subterms?"
                                  "Check that you haven't written, for example, (1 + 2) where it should be (+ 1 2)."
                                  ))
                        end))
            (ref-entry "out_of_memory"
                       (list
                        (para (list "The `out_of_memory` symbol is returned if the heap is full and running"
                                    "the garbage collector was not able to free any memory up."
                                    ))
                        (para (list "The program you have written requires more memory."
                                    ))
                        (bullet '("Increase the heap size."
                                  "Rewrite the application to use less memory."
                                  ))
                        end))
            (ref-entry "fatal_error"
                       (list
                        (para (list "The `fatal_error` symbol is returned in cases where the"
                                    "LispBM runtime system cannot proceed. Something is corrupt and it is"
                                    "not safe to continue."
                                    ))
                        (bullet '("If this happens please send the program and the full error message to blog.joel.svensson@gmail.com. It will be much appreciated."
                                  ))
                        end))
            (ref-entry "out_of_stack"
                       (list
                        (para (list "The `out_of_stack` symbol is returned if the evaluator"
                                    "runs out of continuation stack (this is its runtime-stack). You are"
                                    "most likely writing a non-tail-recursive function that is exhausting all"
                                    "the resources."
                                    ))
                        (bullet '("Check your program for recursive functions that are not tail-recursive Rewrite these in tail-recursive form."
                                  "If you spawned this process in a small stack. For example (spawn 10 prg), try to spawn it with a larger stack."
                                  ))
                        end))
            (ref-entry "division_by_zero"
                       (list
                        (para (list "The `division_by_zero` symbol is returned when dividing by zero."
                                    ))
                        (bullet '("Check your math."
                                  "Add 0-checks into your code at a strategic position."
                                  ))
                        end))
            (ref-entry "variable_not_bound"
                       (list
                        (para (list "The `variable_not_bound` symbol is returned when evaluating a variable (symbol) that is neighter bound nor special (built-in function)."
                                    ))
                        end))
            )
           ))

;; Flash memory

;; (define const-symbol-strings
;;   (ref-entry "@const-symbol-strings"
;;              (list
;;               (para (list "`@const-symbol-strings` functionality have been combined with `@const-start` and `@const-end`."
;;                           "Now symbols created while in a const block, end up in flash storage."
;;                           ))
;;               (para (list "~~if `@const-symbol-strings` directive is placed in a file, symbols will be created"
;;                           "in flash memory instead of the arrays memory.~~"
;;                           ))
;;               end)))

(define const-start
  (ref-entry "@const-start"
             (list
              (para (list "`@const-start` opens a block of code where each global definition is"
                          "moved to constant memory (flash) automatically. This can be used only together with the"
                          "incremental reader (such as `read-eval-program`)."
                          ))
              (para (list "A `@const-start` opened block should be closed with a `@const-end`. Constant blocks"
                          "cannot be nested."
                          ))
              (verb '("```clj\n"
                      "@const-start\n"
                      "(defun f (x) (+ x 1))\n"
                      "@const-end\n"
                      "\n"
                      "(+ (f 1) 2)\n"
                      "```"
                      ))
              end)))

(define const-end
  (ref-entry "@const-end"
             (list
              (para (list "`@const-end` closes an block opened by `@const-start`."
                          ))
              end)))

(define flash-move
  (ref-entry "move-to-flash"
             (list
              (para (list "A value can be moved to flash storage to save space on the normal"
                          "evaluation heap or lbm memory.  A `move-to-flash` expression is of the"
                          "form `(move-to-flash sym opt-sym1 ... opt-symN)`.  The symbols `sym`,"
                          "`opt-sym1 ... opt-symN` should be globally bound to the values you"
                          "want moved to flash. After the value has been moved, the environment"
                          "binding is updated to point into flash memory. **CAUTION** This"
                          "function should be used carefully. Ideally a value should be moved to"
                          "flash immediately after it is created so there is no chance that other"
                          "references to original value exists."
                          ))
              (program '(((define a [1 2 3 4 5 6])
                          (move-to-flash a)
                          a
                          )
                         ((define ls '(1 2 3 4 5))
                          (move-to-flash ls)
                          ls
                          )
                         (( defun f (x) (+ x 1))
                          (move-to-flash f)
                          (f 10)
                          )
                         ))
              end)))

(define flash
  (section 2 "Flash memory"
           (list
            (para (list "Flash memory can be used to store data and functions that are constant."
                        "Things can be moved to flash explicitly using the `move-to-flash` function"
                        "or as part of the reading procedure. To move things automatically to flash during"
                        "reading, there are `@`directives."
                        ))
            'hline
            ;const-symbol-strings
            const-start
            const-end
            flash-move
            )
           ))

;; Type convertion function

(define type-conv
  (section 2 "Type convertions"
           (list
            (ref-entry "to-byte"
                       (list
                        (para (list "Convert any numerical value to a byte."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-byte 1234)
                                (read-eval "(to-byte 3.14)")
                                (to-byte 'apa)
                                ))
                        end))
            (ref-entry "to-i"
                       (list
                        (para (list "Convert a value of any numerical type to an integer."
                                    "The resulting integer is a 28bit value on 32bit platforms and 56 bits on 64 bit platforms."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-i 25b)
                                (read-eval "(to-i 3.14)")
                                (to-i 'apa)))
                        end))
            (ref-entry "to-u"
                       (list
                        (para (list "Convert a value of any numerical type to an unsigned integer."
                                    "The resulting integer is a 28bit value on 32bit platforms and 56 bits on 64 bit platforms."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-u 25b)
                                (read-eval "(to-u 3.14)")
                                (to-u 'apa)
                                ))
                        end))
            (ref-entry "to-i32"
                       (list
                        (para (list "Convert any numerical value to a 32bit int."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-i32 25b)
                                (read-eval "(to-i32 3.14)")
                                (to-i32 'apa)
                                ))
                        end))
            (ref-entry "to-u32"
                       (list
                        (para (list "Convert any numerical value to a 32bit unsigned int."
                                    ))
                        (code '((to-u32 25b)
                                (read-eval "(to-u32 3.14)")
                                (to-u32 'apa)
                                ))
                        end))
            (ref-entry "to-float"
                       (list
                        (para (list "Convert any numerical value to a single precision floating point value."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-float 25b)
                                (read-eval "(to-float 3.14)")
                                (to-float 'apa)
                                ))
                        end))
            (ref-entry "to-i64"
                       (list
                        (para (list "Convert any numerical value to a 64bit int."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-i64 25b)
                                (read-eval "(to-i64 3.14)")
                                (to-i64 'apa)
                                ))
                        end))
            (ref-entry "to-u64"
                       (list
                        (para (list "Convert any numerical value to a 64bit unsigned int."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-u64 25b)
                                (read-eval "(to-u64 3.14)")
                                (to-u64 'apa)
                                ))
                        end))
            (ref-entry "to-double"
                       (list
                        (para (list "Convert any numerical value to a double precision floating point value."
                                    "If the input is not a number the output of this function will be 0."
                                    ))
                        (code '((to-double 25b)
                                (read-eval "(to-double 3.14)")
                                (to-double 'apa)
                                ))
                        end))
            )))

;; Manual

(define manual
  (list
   (section 1 "LispBM Reference Manual"
            (list ch-symbols
                  ch-numbers
                  ch-syntax-semantics
                  ch-fun-imp
                  (section 1 "Reference"
                           (list arithmetic
                                 comparisons
                                 boolean
                                 predicates
                                 bitwise
                                 nil-and-t
                                 quotes
                                 built-ins
                                 special-forms
                                 lists
                                 assoc-lists
                                 arrays
				 defrag-mem
                                 pattern-matching
                                 concurrency
                                 message-passing
                                 flat-values
                                 macros
                                 cc
                                 error-handling
                                 flash
                                 type-conv
                                 info
                                 ))
                  )
            )
   )
  )



(defun render-manual ()
  (let ((h (fopen "lbmref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "Reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )

