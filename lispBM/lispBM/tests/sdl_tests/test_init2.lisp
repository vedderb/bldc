
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

(if (and r r1 r2) (print "SUCCESS")
    (print "FAILURE"))

