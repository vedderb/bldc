
(defun f (x)
  (match x
         ( (? x) (eq (type-of x) type-i) 'an-integer)
         ( (? x) 'something-else)))

(check (and (eq (f 23) 'an-integer)
            (eq (f 0.3) 'something-else)))
