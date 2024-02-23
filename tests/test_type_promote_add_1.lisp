
;; Promotion of byte type

(defun apply (f x) (eval (cons f x)))

(define a1 (eq (type-of (+ 1b 1b)) type-char))
(define a2 (eq (type-of (+ 1b 1)) type-i))
(define a3 (eq (type-of (+ 1b 1u)) type-u))
(define a4 (eq (type-of (+ 1b 1i32)) type-i32))
(define a5 (eq (type-of (+ 1b 1u32)) type-u32))
(define a6 (eq (type-of (+ 1b 1i64)) type-i64))
(define a7 (eq (type-of (+ 1b 1u64)) type-u64))
(define a8 (eq (type-of (+ 1b 1.0f32)) type-float))
(define a9 (eq (type-of (+ 1b 1.0f64)) type-Double))

(check (apply and (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
