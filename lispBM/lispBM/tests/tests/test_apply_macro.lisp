(defmacro macro-quote (arg1 arg2) `(list ',arg1 ',arg2))
(defmacro macro-eval (arg1 arg2) `(list ,arg1 ,arg2))
(defmacro macro-no-args () ''symbol)

(check (and
    (eq (apply macro-quote '(a b)) '(a b))
    {
        (var local 'c)
        (eq (apply macro-eval '('a local)) '(a c))
    }
    (eq (apply macro-no-args nil) 'symbol)
    (eq (apply (macro (arg1 arg2) `(list ,arg1 ,arg2)) (iota 2)) '(0 1))
))

