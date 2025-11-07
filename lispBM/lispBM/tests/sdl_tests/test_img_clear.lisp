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
(sdl-set-active-renderer rend)

(define img400x200 (img-buffer 'indexed2 400 200))

;; Test img-clear function
;; Set some pixels first
(img-setpix img400x200 10 10 1)
(img-setpix img400x200 20 20 1)
(img-setpix img400x200 30 30 1)

;; Test clearing with valid color index
(define r1 (img-clear img400x200 0))

;; Test clearing with different color
(define r2 (img-clear img400x200 1))

;; Test with invalid image buffer
(define r3 (trap (img-clear "not-an-image" 0)))
(define r4 (trap (img-clear nil 0)))

;; Test with invalid color (depends on color format)
(define r5 (img-clear img400x200 5)) ;; indexed2 only supports 0-3

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 (eq r3 '(exit-error type_error)) (eq r4 '(exit-error type_error)) r5)
    (print "SUCCESS")
    (print "FAILURE"))