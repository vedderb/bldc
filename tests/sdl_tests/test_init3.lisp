
(define r (sdl-init))

(define w 500)
(define h 500)

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(define win (sdl-create-window "LISP-GFX" w h))
(define rend (sdl-create-soft-renderer win))

(define r1 (sdl-clear rend))
(define r2 (sdl-present))

(define r3 (print win))
(define r4 (print rend))

(define r5 (eq (type-of rend) 'type-custom))
(define r6 (eq (type-of win) 'type-custom))


(if (and r r1 r2 r3 r4) (print "SUCCESS")
    (print "FAILURE"))

