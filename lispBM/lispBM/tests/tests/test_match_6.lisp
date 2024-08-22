
(defun f (x)
  (match (type-of x)
	 (type-i64 (+ 1 x))
	 (type-u64 (+ 2 x))
	 (type-double (+ 3 x))))


(check (and (= (f 1i64) 2i64)
            (= (f 2u64) 4u64)
            (> (f 3.14f64) 6.13f64)
            (< (f 3.14f64) 6.15f64)))

