(hide-trapped-error)

;; Waste some heap and lbm_memory
(define apa (range 0 178))
(define bepa (flatten apa))

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define time-read-eval
    (macro (str)
           `(progn
              (var file-handle (fopen ,str "r"))
              (var test-program (load-file file-handle))
              (fclose file-handle)
              (var t0 (systime))
              (var res (read-eval-program test-program))
              (print (- (systime) t0))
              res)))

;; Test program_1000.lisp (1267 bytes)
(define r1 (number? (time-read-eval "repl_tests/test_data/program_1000.lisp")))

(debug_test r1 1)

(define r2 (number? (time-read-eval "repl_tests/test_data/program_1000.lisp")))

(debug_test r2 2)

(if (and r1 r2)
    (print "SUCCESS")
    (print "FAILURE"))
