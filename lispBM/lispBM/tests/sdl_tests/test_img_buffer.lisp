
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
(define img10x10 (img-buffer 'rgb888 10 10))

;; Typeerror
(define img-error (trap (img-buffer 'apa 100 100)))

(if (and img400x200 img200x100 img10x10 (eq img-error '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
