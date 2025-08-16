
(define r (sdl-init))

(define w 500)
(define h 500)

(define win (sdl-create-window "LISP-GFX" w h))
(define rend (sdl-create-soft-renderer win))

(define r1 (custom-destruct rend))
(define r2 (custom-destruct win))

(define r3 (eq '(exit-error eval_error) (trap (custom-destruct 1))))
(define r4 (eq '(exit-error eval_error) (trap (custom-destruct))))

(if (and r r1 r2 r3 r4) (print "SUCCESS")
    (print "FAILURE"))
