
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


;; Test program_1002.lisp that causes strange reader behavior
(define file1002 (fopen "repl_tests/test_data/program_1002.lisp" "r"))
(define prog1002 (load-file file1002))
(fclose file1002)

(print "Loaded program_1002.lisp, size: " (length prog1002))
(print "First 50 bytes: " (bufget-u8 prog1002 0) " " (bufget-u8 prog1002 1) " " (bufget-u8 prog1002 2) " ...")

(define test-result (trap (time-read-eval prog1002)))
(print "Test result: " test-result)

(define r1 (eq 'exit-error (car test-result)))
(debug_test r1 1)

(if r1
    (print "SUCCESS - Got expected error")
    (print "FAILURE - Did not get expected error"))