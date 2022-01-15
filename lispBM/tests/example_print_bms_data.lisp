(define rate 2) ; Update rate in hz

(define map (lambda (f xs)
    (if (= xs nil)
	nil
  (cons (f (car xs)) (map f (cdr xs))))))
  
(define iota (lambda (n)
  (let ((iacc (lambda (acc i)
          (if (< i 0)
          acc
            (iacc (cons i acc) (- i 1))))))
  (iacc nil n))))

(define range (lambda (start end)
   (map (lambda (x) (+ x start)) (iota (- end start))
)))

(define foldl (lambda (f i xs)
  (if (= xs nil)
    i
	(foldl f (f i (car xs)) (cdr xs)))))

(let ((f (lambda ()
   (progn
     (define v-cells (map (lambda (c)(get-bms-val "v_cell" c)) (range 0 (- (get-bms-val "cell_num") 1))))
     (print (list "V Tot  : " (get-bms-val "v_tot")))
     (print (list "V Cells: " v-cells))
     (print (list "foldl  : " (foldl + 0 v-cells)))
     (print (list "ADCs   : " (map (lambda (c)(get-adc c)) (iota 1))))
     (print " ")
     (yield (/ 1000000.0 rate))
     (f)
)))) (f))

