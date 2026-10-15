;; Regression test for the const-heap write-once check, and for the
;; cascading failure it triggers.
;;
;; run_persist_tests.sh runs each script twice against the same image.lbm
;; without an intervening (image-save), so the const-heap bump pointer
;; resets to address 0 on the second run. (systime) makes the content of
;; a @const-start block genuinely non-deterministic across the two runs,
;; so run 2 must observe a differing word at an address run 1 already
;; committed. That surfaces as ENC_SYM_FATAL_ERROR (flash write error).
;;
;; Plain `trap` cannot catch a fatal error at all - error_ctx_base
;; explicitly excludes ENC_SYM_FATAL_ERROR from trap's unwind path
;; (eval_cps.c), so a fatal error always kills the context it occurs in,
;; trap or no trap. `spawn-trap` is different: it notifies the *parent*
;; on any child death, fatal included, via a message rather than by
;; unwinding trap's stack. So every non-deterministic const-heap write
;; below runs in its own spawned child, and this script catches each
;; failure by receiving that message.
;;
;; The important finding this test locks in: the aborted define for `l`
;; still advances the const-heap bump pointer by whatever got reserved
;; before the mismatch fired. It does not roll back. So every subsequent
;; const-heap write in *this* boot - even a fully deterministic one, `m`
;; below - is now comparing against the wrong stretch of leftover flash
;; content and fails too, via the exact same fatal-error path. One
;; non-deterministic write desyncs the const heap for the rest of the
;; run; it is not merely "lose one binding."

(define worker-l (lambda ()
  (read-eval-program "@const-start (define l (list 1 2 (systime))) @const-end")))

(define cid-l (spawn-trap worker-l))
(define r-l (recv ((? x) x)))

;; The aborted define never reaches the global-binding step: l is left
;; completely unbound, not stale. Its old cells from a prior run are
;; still physically present in flash, just orphaned - nothing references
;; them.
(define l-access (trap l))

;; Deterministic content, but starts at whatever address l's aborted
;; write left the bump pointer at, not the address m used last run - so
;; it collides with leftover l content and fails too. Needs spawn-trap
;; for the same reason l did: a fatal error here would otherwise kill
;; this whole script.
(define worker-m (lambda ()
  (read-eval-program "@const-start (define m (list 1 2 3)) @const-end")))

(define cid-m (spawn-trap worker-m))
(define r-m (recv ((? x) x)))

(define m-access (trap m))

(define run1 (eq (car r-l) 'exit-ok))

(define ok
  (if run1
      (and (eq l-access `(exit-ok ,l))
           (eq r-m `(exit-ok ,cid-m (1 2 3)))
           (eq m-access '(exit-ok (1 2 3))))
      (and (eq r-l `(exit-error ,cid-l fatal_error))
           (eq l-access '(exit-error variable_not_bound))
           (eq r-m `(exit-error ,cid-m fatal_error))
           (eq m-access '(exit-error variable_not_bound)))))

(if ok
    (print "SUCCESS")
    (print "FAILURE"))
