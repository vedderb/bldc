{
(var f 10)
(setq f (lambda (x) (if (= x 0) 0 (f (- x 1)))))
(check (= (f 10) 0))
}
