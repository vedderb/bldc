;; Circular variant of test_gc_stack_adversarial.lisp: same car-leaning
;; adversarial structure (see paper/paper.tex, make-adversarial /
;; Figure adversarial in the ifl26 project), but the deepest cell along
;; the leftward spine (the one whose car would normally be the nil
;; base case) is tied back with setcar to point at the root, making
;; the whole structure circular.
;;
;; This checks two things at once:
;;
;;  1. The mark phase's mark-bit check correctly absorbs the back-edge
;;     with no extra GC-stack depth: the root is marked before its car
;;     is descended into, so by the time traversal reaches the deepest
;;     cell and follows its car back to the root, the root is already
;;     marked and marking stops there (heap.c, lbm_gc_mark_phase).
;;     Verified locally at small n that (gc) alone on a circular
;;     version of this structure completes without incident.
;;
;;  2. Consequently, circularity does not change the failure mode of
;;     the known car-leaning weakness: at n = 400 (matching the
;;     non-circular test and the paper's own worked example) this
;;     still overflows the fixed-size GC mark stack and hits
;;     lbm_critical_error() exactly as the non-circular version does -
;;     same clean, documented, unrecoverable exit (REPL_EXIT_CRITICAL_ERROR,
;;     22, "GC stack overflow!" / "CRITICAL ERROR"), not a hang and not
;;     some worse failure mode (e.g. the mark phase looping forever on
;;     the cycle instead of hitting the stack limit).
;;
;; NOTE: this file intentionally never compares or otherwise walks
;; into the circular structure with `eq`/`struct_eq` (fundamental.c).
;; That function only short-circuits on exact pointer identity at the
;; top of a comparison and otherwise recurses into car/cdr
;; unconditionally, so calling it on two values that share this cycle
;; from different starting points causes unbounded C-stack recursion
;; and segfaults the process outright (uncontrolled crash, not the
;; documented/handled GC-stack error this test is about). That is a
;; separate, more severe issue than what this test exercises.
;;
;; Expected to fail (registered in run_repl_tests.sh /
;; run_persist_tests.sh's expected_fails), same as
;; test_gc_stack_adversarial.lisp.

(defun make-adversarial (acc ls)
  (match ls
         (nil  (cons acc nil))
         (((? a) . (? b)) (make-adversarial (cons acc (cons a nil)) b))))

(defun nth-car (n c)
  (if (= n 0) c (nth-car (- n 1) (car c))))

(define n 400)
(define a (make-adversarial nil (range n)))
(define deepest (nth-car n a))
(setcar deepest a)

(gc)

(print "SUCCESS")
