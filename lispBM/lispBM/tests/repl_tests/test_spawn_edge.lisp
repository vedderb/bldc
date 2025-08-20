(defun f () (+ 1 2))

(define e1 (trap (spawn 'apa f)))
(define e2 (trap (spawn 'apa 100 f)))
(define e3 (trap (spawn "hello" 'apa f)))
(define e4 (trap (spawn-trap 'apa f)))
(define e5 (trap (spawn-trap 'apa 100 f)))
(define e6 (trap (spawn-trap "hello" 'apa f)))

(define a (spawn "hello" 100 f))
(define b (spawn-trap "hello" 100 f))

(define err '(exit-error type_error))

(wait a)
(wait b)

(if (and (eq e1 err)
         (eq e2 err)
         (eq e3 err)
         (eq e4 err)
         (eq e5 err)
         (eq e6 err))
    (print "SUCCESS")
    (print "FAILURE")
    )
