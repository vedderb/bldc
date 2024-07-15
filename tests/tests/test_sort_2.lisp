
(define is-sorted
  (lambda (acc cmp ls)
    (if (or (eq ls nil)
            (eq (cdr ls) nil))
        acc
      (is-sorted (and acc (cmp (car ls) (car (cdr ls)))) cmp (cdr ls)))))

(define cmp (lambda (x y) (< x y)))

(define a '(2 1))
(define len-a (length a))

(define b (sort cmp a))

(check (and (is-sorted 't cmp b) (= (length b) len-a)))
