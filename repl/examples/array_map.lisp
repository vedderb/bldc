

(defun amap (f arr)
  (let ((new-arr (mkarray (length arr))))
    {
    (loop ( (i 0) )
          ( < i (length arr))
          {
          (print i)
          (setix new-arr i (f (ix arr i)))
          (setq i (+ i 1))
          }
          )
    new-arr
    }
    ))


(def a (list-to-array (range 10)))

(def b (amap (lambda (x) (+ x 1)) a))

(print (str-merge "Done: " (to-str b))) 


