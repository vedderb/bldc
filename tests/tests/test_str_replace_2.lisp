(define r1 (eq (str-replace "Hello World!" "World" "LispBM") "Hello LispBM!"))

(define r2 (eq (str-replace "Hello World!" " World") "Hello!"))

(check (and r1 r2))
