
(define is-u (lambda (x) (eq (type-of x) type-u)))

(define r1 (is-u (lbm-heap-state 'get-heap-size)))
(define r2 (is-u (lbm-heap-state 'get-heap-bytes)))
(define r3 (is-u (lbm-heap-state 'get-num-alloc-cells)))
(define r4 (is-u (lbm-heap-state 'get-gc-num)))
(define r5 (is-u (lbm-heap-state 'get-gc-num-marked)))
(define r6 (is-u (lbm-heap-state 'get-gc-num-recovered-cells)))
(define r7 (is-u (lbm-heap-state 'get-gc-num-recovered-arrays)))
(define r8 (is-u (lbm-heap-state 'get-gc-num-least-free)))
(define r9 (is-u (lbm-heap-state 'get-gc-num-last-free)))

(check (and r1 r2 r3 r4 r5 r6 r7 r8 r9))
