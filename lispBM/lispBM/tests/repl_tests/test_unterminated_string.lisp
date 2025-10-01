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

;; Test unterminated string from file (should trigger tokenizer error)
(define file-handle (fopen "repl_tests/test_data/unterminated_string.lisp" "r"))
(define unterminated-program (load-file file-handle))
(fclose file-handle)
(define r1 (eq 'exit-error (car (trap (time-read-eval unterminated-program)))))

(debug_test r1 1)

(if r1
    (print "SUCCESS")
    (print "FAILURE"))