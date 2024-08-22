

(defun larger ( x )
  (> x 10))

(defun smaller ( x )
  (< x 10))

(defun f (x)
  (match x
         ( ((? x) . ((? y) . _))  (smaller x) (list y 'smaller))
         ( ((? x) . ((? y) . _))  (larger x) (list y 'larger))
         ( _ 'whatever)))

(check (and (eq (f '(0 1 2 3)) '(1 smaller))
            (eq (f '(11 9 8 7)) '(9 larger))))
