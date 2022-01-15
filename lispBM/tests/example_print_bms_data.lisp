(define rate 2) ; Update rate in hz

(define map (lambda (f xs)
    (if (= xs nil)
	nil
  (cons (f (car xs)) (map f (cdr xs))))))

(let ((f (lambda ()
   (progn
     (print (list "V Tot  : " (get-bms-val "v_tot")))
     (print (list "V Cells: " (map (lambda (c)(get-bms-val "v_cell" c)) (range 0 (- (get-bms-val "cell_num") 1)))))
     (print " ")
     (yield (/ 1000000.0 rate))
     (f)
)))) (f))

