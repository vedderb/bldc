
(defun f (x)
  (match x
	 ((?i64 a) (+ 1 a))
	 ((?u64 a) (+ 2 a))
	 ((?double a) (+ 3 a))))


(and (= (f 1i64) 2i64)
     (= (f 2u64) 4u64)
     (> (f 3.14f64) 6.13f64)
     (< (f 3.14f64) 6.15f64))

