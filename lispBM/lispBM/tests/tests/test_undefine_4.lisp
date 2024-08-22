(define a 10)
(define b 1)

;; setvar nolonger works on undefined variables.
;; so setvar cannot be used in the loop.
(defun f (n)
  (if (= n 0) 0
    (progn
      (define b 10)
      (undefine 'b)
      (f (- n 1)))))

(f 10000)

(define b 75)

(check (and (= a 10)
            (= b 75)))
