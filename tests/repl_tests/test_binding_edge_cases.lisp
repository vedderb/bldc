

;; Note that some of these corner cases are cought by
;; create_binding_location and some by fill_binding_location.
;; If all binding locations that are filled have been created
;; using create_binding_location then some checks are redundant.

(define r1 (trap (let ((1 1) (2 2))
                   (+ 1 2))))


(define r2 (trap { (+ 1 2) (var 1 1) (+ 1 1) }))

(define r3 (trap (let (((a . b) 1))
                   (+ a 1))))

(define r4 (trap (let ((1 (list 1 2)))
                       (+ 1 2))))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error type_error))
         (eq r4 '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
                    
