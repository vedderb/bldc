
;; Promotion of u64 type

(defun apply (f x) (eval (cons f x)))

(define a1 (eq (type-of (+ 1u64 1b)) type-u64))
(define a2 (eq (type-of (+ 1u64 1)) type-u64))
(define a3 (eq (type-of (+ 1u64 1u)) type-u64))
(define a4 (eq (type-of (+ 1u64 1i32)) type-u64))
(define a5 (eq (type-of (+ 1u64 1u32)) type-u64))
(define a6 (eq (type-of (+ 1u64 1i64)) type-u64))
(define a7 (eq (type-of (+ 1u64 1u64)) type-u64))
(define a8 (eq (type-of (+ 1u64 1.0f32)) type-float))
(define a9 (eq (type-of (+ 1u64 1.0f64)) type-Double))

(check (apply and (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
