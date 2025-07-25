;; The paradoxical combinator Y (Lisp in small pieces p63)

;; Fixed point theorem in lisp
;;
;; (let ((W (lambda (w)
;;           (lambda (f)
;;             (f ((w w) f)) ) )))
;;   (W W))

(define fix
  (let  (( d (lambda (w)
               (lambda (f)
                 (f (lambda (x) (((w w) f) x))) ) ))) ;; nabla-converted
    (d d) ) )



;; fix of meta-fact (fix meta-fact) is the factorial function.
(define meta-fact (lambda (f)
                    (lambda (n)
                      (if (= n 1) 1
                        (* n (f (- n 1))) ) ) ))

