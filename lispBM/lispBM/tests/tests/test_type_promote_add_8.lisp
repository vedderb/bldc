
;; Promotion of f32 type

(defun apply (f x) (eval (cons f x)))

(define a1 (eq (type-of (+ 1.0f32 1b)) type-float))
(define a2 (eq (type-of (+ 1.0f32 1)) type-float))
(define a3 (eq (type-of (+ 1.0f32 1u)) type-float))
(define a4 (eq (type-of (+ 1.0f32 1i32)) type-float))
(define a5 (eq (type-of (+ 1.0f32 1u32)) type-float))
(define a6 (eq (type-of (+ 1.0f32 1i64)) type-float))
(define a7 (eq (type-of (+ 1.0f32 1u64)) type-float))
(define a8 (eq (type-of (+ 1.0f32 1.0f32)) type-float))
(define a9 (eq (type-of (+ 1.0f32 1.0f64)) type-Double))

(check (apply and (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
