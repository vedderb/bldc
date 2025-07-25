

;; test that filling the heap with an assoc list is a recoverable error.

(define f (lambda (acc)
            (f (acons 1 2 acc))))

(check (eq '(exit-error out_of_memory) (trap (f nil))))
