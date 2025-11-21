(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

(define time-read-eval
    (macro (str)
           `(progn
              (var t0 (systime))
              (var res (read-eval-program ,str))
              (print (- (systime) t0))
              res)))

;; Test program_1001.lisp (1270 bytes)
(define file-handle (fopen "repl_tests/test_data/program_1001.lisp" "r"))
(define test-program (load-file file-handle))
(fclose file-handle)
(define r1 (number? (time-read-eval test-program)))

(debug_test r1 1)

(if r1
    (print "SUCCESS")
    (print "FAILURE"))