
(defun larger ( x ) {
       (gc)
       (> x 10)
       })

(defun smaller ( x ) {
       (gc)
       (< x 10)
       })

(defun f (x)
  (atomic
   (var z "apa")
   (match x
          ( ((? x) . ((? y) . _))  (smaller z) (list y 'smaller))
          ( ((? x) . ((? y) . _))  (larger z) (list y 'larger))
          ( _ 'whatever))))

(check (and (eq (f '(0 1 2 3)) 'whatever)
            (eq (f '(11 9 8 7)) 'whatever)))
