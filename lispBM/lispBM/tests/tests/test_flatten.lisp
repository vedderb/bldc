
(define r1 (eq '(exit-error type_error) (trap (flatten))))

(define r2 (eq (unflatten (flatten 1b)) 1b))
(define r3 (eq (unflatten (flatten 1)) 1))
(define r4 (eq (unflatten (flatten 1u)) 1u))
(define r5 (eq (unflatten (flatten 1i32)) 1i32))
(define r6 (eq (unflatten (flatten 1u32)) 1u32))
(define r7 (eq (unflatten (flatten 1i64)) 1i64))
(define r8 (eq (unflatten (flatten 1u64)) 1u64))
(define r9 (eq (unflatten (flatten 1.0f32)) 1.0f32))
(define r10 (eq (unflatten (flatten 1.0f64)) 1.0f64))

(define arr (list-to-array (list 1 2 3)))

(define r11 (eq (unflatten (flatten arr)) arr))

(define barr [1 2 3 4])

(define r12 (eq (unflatten (flatten barr)) barr))

(define ls (list 1 arr barr))

(define r13 (eq (unflatten (flatten ls)) ls))

;; custom types are not flattenable at the moment.
(define r14 (eq '(exit-error eval_error) (trap (flatten (list-to-vector (list 1 2 3))))))

(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13))

  
