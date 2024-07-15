
(def v (vector 1.0 2.0 3.0))

(def v2 (vmult 2.0 v))

(defun zipwith (f x y)
  (match (cons x y)
         ((_ . nil) nil)
         ((nil . _) nil)
         (((? a). (? b)) (cons (f (car a) (car b)) (zipwith f (cdr a) (cdr b))))))

(defun fold (f i x)
  (match x
         (nil i)
         (((? x) . (? xs)) (f x (fold f i xs)))))

(def diff (zipwith (fn (x y) (- x y))  (vector-to-list v2) '(2.0 4.0 6.0)))
(def sum (fold (fn (x y) (+ x y)) 0.0 diff))

(check (< sum 0.001))
