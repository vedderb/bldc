;; Dynamic scoping vs lexical scoping
;; page 22 23 in lisp in small pieces.

(define my-map (lambda (f ls) ; mapcar in "Lisp" (CL I assume)
                 (match ls
                       (nil nil)
                       (((? x) . (? xs)) (cons (f x) (my-map f xs))))))


(let ((ls '(a b c)))
  (my-map (lambda (x) (ix ls x)) '(2 1 0)))

;; Result is (c b a) while on a dynamic scope lisp it would be (0 0 0)
;; in `(lambda (x) (ix ls x))` ls is free and would take on the value
;; of whatever ls is in scope.
;;
;; in the map ls takes on values (2 1 0) (1 0) and (0) and we
;; index at pos 2 in (2 1 0) => 0
;; index at pos 1 in (1 0) => 0
;; index at pos 0 in (0) => 0
;; result => (0 0 0)
