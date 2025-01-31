(define list2array (lambda (ls)
  (let ((n (length ls)) (arr (mkarray n)) (i 0)) {
    (loopforeach e ls { (setix arr i e) (setq i (+ i 1)) }) arr })))

(define array2list (lambda (arr)
  (let ((n (length arr)) (ls nil)) {
    (loopfor i (- n 1) (>= i 0) (- i 1) { (setq ls (cons (ix arr i) ls)) }) ls })))
             

(defun array? (a) (eq (type-of a) type-lisparray))
