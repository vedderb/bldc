
;; Promotion of f64 type

(defun apply (f x) (eval (cons f x)))

(define a1 (eq (type-of (* 1.0f64 1b)) type-double))
(define a2 (eq (type-of (* 1.0f64 1)) type-double))
(define a3 (eq (type-of (* 1.0f64 1u)) type-double))
(define a4 (eq (type-of (* 1.0f64 1i32)) type-double))
(define a5 (eq (type-of (* 1.0f64 1u32)) type-double))
(define a6 (eq (type-of (* 1.0f64 1i64)) type-double))
(define a7 (eq (type-of (* 1.0f64 1u64)) type-double))
(define a8 (eq (type-of (* 1.0f64 1.0f32)) type-double))
(define a9 (eq (type-of (* 1.0f64 1.0f64)) type-Double))

(check (apply and (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
