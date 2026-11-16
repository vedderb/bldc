;; Stresses the known weakness of the default stack-based mark phase
;; garbage collector: a "car-leaning" adversarial heap structure whose
;; leftward spine is deeper than the fixed-size GC mark stack
;; (GC_STACK_SIZE in repl/repl.c). See paper/paper.tex in the ifl26
;; project (make-adversarial / Figure adversarial) for the full
;; description.
;;
;; make-adversarial turns a plain list into a snoc list with a pointer
;; to a heap cell in every car field along the leftward spine, so
;; marking it requires as deep a GC stack as the spine is long. With
;; 400 elements the leftward spine is deeper than the mark stack, so
;; running (gc) here provokes lbm_critical_error() (heap.c), which is
;; documented as unrecoverable: the eval thread terminates and the
;; repl process exits with REPL_EXIT_CRITICAL_ERROR (22), printing
;; "GC stack overflow!" / "CRITICAL ERROR" instead of ever reaching the
;; SUCCESS print below.
;;
;; This test is therefore expected to fail (registered in the
;; expected_fails list of run_repl_tests.sh / run_persist_tests.sh). If
;; it ever starts printing SUCCESS instead, that means the mark-stack
;; overflow no longer crashes the runtime (e.g. a fallback to the
;; Deutsch-Schorr-Waite pointer-reversal marking algorithm was wired
;; in) and this test, along with the paper text describing the
;; weakness, should be revisited.

(defun make-adversarial (acc ls)
  (match ls
         (nil  (cons acc nil))
         (((? a) . (? b)) (make-adversarial (cons acc (cons a nil)) b))))

(define a (make-adversarial nil (range 400)))
(gc)

(print "SUCCESS")
