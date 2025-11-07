
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


(let ((win (sdl-create-window "LISP-GFX" w h)))
      (sdl-create-soft-renderer win))


(gc)

;; if we haven't crashed, print success
(print "SUCCESS")
