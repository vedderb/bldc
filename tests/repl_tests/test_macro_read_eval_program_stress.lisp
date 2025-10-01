(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

;; Load a program file once
(define file-handle (fopen "repl_tests/test_data/program_1000.lisp" "r"))
(define test-program (load-file file-handle))
(fclose file-handle)

(print "Loaded program, size: " (length test-program) " bytes")

;; Expected result - run it once to establish baseline
(define expected-result 120);; (read-eval-program test-program))
(print "Expected result: " expected-result)

(define time-read-eval
    (macro (str)
           `(progn
              (var t0 (systime))
              (var res (read-eval-program ,str))
              (print (- (systime) t0))
              res)))


;; Stress test: run read-eval-program repeatedly in a loop
(define stress-iterations 1000)
(define failures 0)

(define stress-test 
  (lambda (iteration)
    (if (< iteration stress-iterations)
        {
          (define result (time-read-eval test-program))
          (if (= result expected-result)
              {
                (if (= (mod iteration 10) 0) 
                    (print "Iteration " iteration " - OK"))
              }
              {
                (print "FAILURE at iteration " iteration ": got " result " expected " expected-result)
                (setq failures (+ failures 1))
              })
          (stress-test (+ iteration 1))
        }
        (print "Stress test complete. Iterations: " iteration " Failures: " failures))))

;; Run the stress test
(stress-test 0)

;; Final verification
(define final-result (read-eval-program test-program))
(define r1 (= final-result expected-result))

(debug_test r1 1)

(if (and r1 (= failures 0))
    (print "SUCCESS")
    (print "FAILURE"))
