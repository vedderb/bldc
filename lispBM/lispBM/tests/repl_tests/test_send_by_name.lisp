(define fun (lambda () (recv ((? x) x))))

(define wa (spawn-trap fun))
(define ra (send wa 'hello-cid))
(define oa (recv ((? x) x)))

(define wb (spawn-trap "worker-b" fun))
(define rb (send "worker-b" 'hello-name))
(define ob (recv ((? x) x)))

;; no context named "no-such-worker" exists, so this falls through to
;; checking the currently running context (named "main-s" when run via -s)
;; by name.
(define rc (send "no-such-worker" 'nope))
(define rd (send 999999 'nope))

;; [1 2 3 4] is a byte array with no null terminator, so it cannot be a
;; genuine thread name (a name is always exactly-terminated). Accepting it
;; as a name would let find_receiver_and_send's strncmp match it against
;; any real name sharing that byte prefix, e.g. [97 112 97] ("apa", no
;; terminator) would match a thread named "apa1". So this is rejected as
;; a type error rather than silently searched for.
(define re (trap (send [1 2 3 4] 'nope)))

(if (and (eq ra t)
         (eq oa `(exit-ok ,wa hello-cid))
         (eq rb t)
         (eq ob `(exit-ok ,wb hello-name))
         (eq rc nil)
         (eq rd nil)
         (eq re '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE")
    )
