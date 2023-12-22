
(define is-sorted
  (lambda (acc cmp ls)
    (if (or (eq ls nil)
            (eq (cdr ls) nil))
        acc
      (is-sorted (and acc (cmp (car ls) (car (cdr ls)))) cmp (cdr ls)))))

(define cmp (lambda (x y) (< (car x) (car y))))

(define a '( ( 2 . "russel" ) ( 1 . "kurt" ) (4 . "great") (3 . "is")))
(define len-a (length a))

(define b (sort cmp a))

(check (and (is-sorted 't cmp b)
            (= (length b) len-a)
            (eq (cdr (ix b 0)) "kurt")
            (eq (cdr (ix b 1)) "russel")
            (eq (cdr (ix b 2)) "is")
            (eq (cdr (ix b 3)) "great")))
