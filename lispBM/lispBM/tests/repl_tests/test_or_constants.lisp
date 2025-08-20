

@const-start

(define r0 (or nil nil nil t nil))
(define r1 (or nil nil nil nil))
(define r2 (or nil))
(define r3 (or nil nil . t))     ;; should be true since incorrect syntax at end of list is ignored.

;; this is really (or nil quote apa) so what it evaluates too depends on what
;; quote and apa evaluates to.
(define r4 (or nil . 'apa)) ;; quote evaluates to quote, which is true

;; (or nil address_into_lbm_mem . float_tag)  : shows importance of expensive lbm_is_cons check
(define r5 (or nil . 3.14f32))
@const-end


(define my-and
    (lambda (x y)
      (if x
          (if y 't
              nil)
          nil)))

(if (my-and (my-and (my-and r0 (not r1))
                    (my-and (not r2) (not r3)))
            (my-and r4 (not r5)))
    (print "SUCCESS")
    (print "FAILURE"))
