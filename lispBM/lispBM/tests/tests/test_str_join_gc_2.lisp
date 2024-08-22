(defun repeatq (expr n) 
    (loop ((i 0)) (< i n) {
        (eval expr)
        (setq i (+ i 1))
    })
)

(def expr '(str-join '("ab" "cde" "f") ""))

(gc)

(def mem-left (mem-num-free))

(repeatq expr 1000)

(gc)

(check (<= mem-left (mem-num-free)))
