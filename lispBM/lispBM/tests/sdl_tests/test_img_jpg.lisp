(sdl-init)

(define win (sdl-create-window "Display library" 550 750))
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
(sdl-set-active-renderer rend)

(define cat-pic-file (fopen "./sdl_tests/lispbm.jpeg" "r"))
(define cat-pic-jpg (load-file cat-pic-file))

(define r (disp-render-jpg cat-pic-jpg 0 0))

(if r
    (print "SUCCESS")
    (print "FAILURE"))
