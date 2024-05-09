
(define  get-vars (lambda (body)
  (match (type-of body)
         ( type-symbol (set-insert nil body))
         ( type-list   (set-union (get-vars (car body)) (get-vars (cdr body))))
         ( _ nil)
         )))

(define  pick-out (lambda (vars env) 
  (match env
         ( nil nil )
         ( ((? b) . (? bs)) (member vars (car b)) (cons b (pick-out vars bs)))
         ( ( _ . (?  bs))   (pick-out vars bs))
         )))

(define clean-cl-env (lambda (clo)
  (setix clo  3 (pick-out (get-vars (ix clo 2))
                          (ix clo 3)))))




     



       
