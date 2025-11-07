
(sdl-init)

(define win (sdl-create-window "Display library" 400 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Connect the renderer to the display library
(define r (sdl-set-active-renderer rend))


;; These are ok
(define img400x200 (img-buffer 'indexed2 400 200))
(define img200x100 (img-buffer 'indexed4 200 100))

;; Typeerror
(define img-error (trap (img-buffer 'apa 100 100)))

(define r1 (img-buffer? img400x200))
(define r2 (img-buffer? img200x100))
(define r3 (img-buffer? img-error))

(if (and r1 r2 (not r3))
    (print "SUCCESS")
    (print "FAILURE"))
