

@const-start

(define r0 (and t t t t nil t))
(define r1 (and t t t t t))
(define r2 (and nil))
(define r3 (and 1 2 . t))     ;; should be true since incorrect syntax at end of list is ignored.

;; this is really (and 1 quote apa) so what it evaluates too depends on what
;; quote and apa evaluates to.
(define r4 (eq '(exit-error variable_not_bound) (trap (and 1 . 'apa)))) ;; apa is unbound error 

;; (and 1 address_into_lbm_mem . float_tag)  : shows importance of expensive lbm_is_cons check
(define r5 (and 1 . 3.14f32))
@const-end


(define my-and
    (lambda (x y)
      (if x
          (if y 't
              nil)
          nil)))

(if (my-and (my-and (my-and (not r0) r1)
                    (my-and (not r2) r3))
            (my-and r4 r5))
    (print "SUCCESS")
    (print "FAILURE"))
