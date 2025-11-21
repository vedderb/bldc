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

;; Test program_odd_1.lisp that causes strange reader behavior
(define file_odd_1 (fopen "repl_tests/test_data/program_odd_1.lisp" "r"))
(define prog_odd_1 (load-file file_odd_1))
(fclose file_odd_1)

(print "Loaded program_odd_1.lisp, size: " (length prog_odd_1))
(print "First 50 bytes: " (bufget-u8 prog_odd_1 0) " " (bufget-u8 prog_odd_1 1) " " (bufget-u8 prog_odd_1 2) " ...")

(define test-result (trap (time-read-eval prog_odd_1)))
(print "Test result: " test-result)

(define r1 (eq 'exit-error (car test-result)))
(debug_test r1 1)

(if (and r1)
    (print "SUCCESS - Got expected error")
    (print "FAILURE - Did not get expected error"))
