
(defun f (x)
  (match x
         ( ((? x) . ((? y) . _))  (< x 10) (list y 'smaller))
         ( ((? x) . ((? y) . _))  (> x 10) (list y 'larger))
         ( _ 'whatever)))

(check (and (eq (f '(0 1 2 3)) '(1 smaller))
            (eq (f '(11 9 8 7)) '(9 larger))))
