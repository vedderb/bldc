
(define a 0)

(loop ( (v1 '(1 2 3 4 5 6 7 8 9 10))
        (v2 '(true nil nil nil nil nil true nil nil true )) )
      (and v1 v2)
      {
      (if (car v2)
          (setq a (+ a (car v1)))
        ())
      (setq v1 (cdr v1))
      (setq v2 (cdr v2))
      })


(check (= a 18))

