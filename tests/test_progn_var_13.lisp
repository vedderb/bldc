

(define i1
  (let ( (apa 5) )
    (progn
      (progn
        (setq apa 20)
        )
      (progn
        (var apa 10)
        (setq apa 30)
        )
      (progn
        apa
        )
      )
    )
  )

(check (= i1 20))
