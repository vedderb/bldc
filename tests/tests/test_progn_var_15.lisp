{
(var f (lambda (x) (if (= x 0) 0 (f (- x 1)))))
(var a (f 10))
(check (= a 0))
}
