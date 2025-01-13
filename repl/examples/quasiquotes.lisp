

;; testcases from: https://git.savannah.nongnu.org/cgit/alisp.git/tree/test.pl
;; tests involving ,. are removed or expressed as ,@
;; '@ makes no sense in LBM as @ is not an allowed symbol, at-signs are valid only to the reader.
;; Some LBM expansions differ from the alisp expansions but to me they seem to be semantically
;; equivalent.
;; ex: (append (quote (1 2)) (list) (list)) != (quote (1 2))
;;     Not syntactically equivalent, but if both sides are evaluated the result is the same.
;;     TODO: have the qq-expander remove list with no args when created.
;;
;; TODO: Make LBM treat ,@x where x is not a list as an error.

(define qq-tests
  ( list
    (cons "`,'a"  "A")
    (cons "``,a"  "`,A")
    (cons "`,`a"  "A")
    (cons "`(1 . 2)"  "(1 . 2)")
    (cons "``(a ,,(+ 1 2))"  "`(A ,3)")
    (cons "``(a ,,(+ 1 2) ,(+ 3 4))"  "`(A ,3 ,(+ 3 4))")
    (cons "``(a ,(+ ,1 2))"  "`(A ,(+ 1 2))")
    (cons "`(,1 . ,2)"  "(1 . 2)")
    (cons "`(,1 ,2 . ,3)" "(1 2 . 3)")
    (cons "`(0 ,1 2 ,3)" "(0 1 2 3)")
    (cons "`',(car ())" "'NIL")
    (cons "`',(car ())" "(QUOTE NIL)")
    (cons "`(if ``(progn ,,1))" "(IF ``(PROGN ,,1))")
    ;;(cons "'@" "@") ;; Makes no sense in LBM where @ is not allowed to exist past the reader
    (cons "`(,@())" "NIL")
    (cons "`(,@() ,@())" "NIL")
    (cons "`(1 2 ,@() ,@() 3)" "(1 2 3)")
    (cons "`(,@nil 1)" "(1)")
    (cons "`(,@nil ,1)" "(1)")
    (cons "``(1 2 ,,@() ,,@())" "`(1 2)")  ;; ** FAILS because of expansion technicality.
    (cons "``(1 2 ,@() ,@())" "`(1 2)")
    (cons "``(,@())" "`(,@NIL)")
    (cons "`(\n,@(\n))" "NIL")
    ;;(cons "`(,@5)" "5")         ;; ,@5 should be error
    ;;(cons "``(,,@0)" "`,0")     ;; ,@0 should be error   **FAILS
    ;;(cons "`(1 ,@5)" "(1 . 5)") ;; ,@5 should be error
    (cons "`(,@'(3 4))" "(3 4)")
    (cons "`(1 2 ,@'(3 . 4))" "(1 2 3 . 4)")
    (cons "'(`(\n,@(2)))" "(`(,@(2)))")
    (cons "'(0 . `(\n,@(2)))" "(0 . `(,@(2)))")
    (cons "``(,,@(list 0 1 2))" "(list 0 1 2)")
    (cons "`(,@``(3 4))" "`(3 4)")
    (cons "`(,1 ,@(cdr '(1 2 3 4)))" "(1 2 3 4)")
    (cons "`(,1 ,@(cdr '(1 2 3 4)) ,5)" "(1 2 3 4 5)")
    (cons "`(,@(cdr '(0 1 2 3 4)) 5)" "(1 2 3 4 5)")
    (cons "`(,@(cdr '(0 1 2 3)) ,4)" "(1 2 3 4)")
    (cons "`(,@(cdr '(0 1 2 3)) ,4)" "(1 2 3 4)")
    (cons "``(,,@(cdr '(0 1 2 3)) ,4)" "`(,@(list 1 2 3) ,4)")
    ))

(defun read-eval (str)
  (eval (read str)))

(defun performtest (p) {
       (var a (car p))
       (var b (cdr p))
       (var ra (read-eval a))
       (var rb (read b))
       (print "check: " a " == " b)
       (if (eq ra rb) (print "OK: " ra " == " rb "\n")
         (print "FAIL: " ra " != " rb "\n"))
       })

(loopforeach elt qq-tests (performtest elt))
