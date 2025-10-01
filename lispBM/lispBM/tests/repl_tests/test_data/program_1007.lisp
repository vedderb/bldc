;; Valid Lisp program exactly 1000 bytes long
;; Tests buffer boundary issues at even 1000-byte alignment

(define factorial (lambda (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(define fibonacci (lambda (n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define sum-list (lambda (lst)
  (if (eq lst nil)
      0
      (+ (car lst) (sum-list (cdr lst))))))

(define reverse-list (lambda (lst)
  (define reverse-helper (lambda (lst acc)
    (if (eq lst nil)
        acc
        (reverse-helper (cdr lst) (cons (car lst) acc)))))
  (reverse-helper lst nil)))

(define map-function (lambda (f lst)
  (if (eq lst nil)
      nil
      (cons (f (car lst)) (map-function f (cdr lst))))))

(define filter-function (lambda (pred lst)
  (if (eq lst nil)
      nil
      (if (pred (car lst))
          (cons (car lst) (filter-function pred (cdr lst)))
          (filter-function pred (cdr lst))))))

(define test-data '(1 2 3 4 5 6 7 8 9 10))

(define result1 (factorial 5))
(define result2 (fibonacci 8))
(define result3 (sum-list test-data))
(define result4 (reverse-list test-data))
(define result5 (map-function (lambda (x) (* x x)) test-data))
(define result6 (filter-function (lambda (x) (= (mod x 2) 0)) test-data))

result1;a
b
c
d
e
f
g
