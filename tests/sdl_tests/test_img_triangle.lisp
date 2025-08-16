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

;; Test img-triangle function
;; Draw normal triangle
(define r1 (img-triangle img400x200 100 50 150 150 50 150 1))

;; Draw triangle at different position
(define r2 (img-triangle img400x200 200 30 250 100 200 100 1))

;; Draw triangle with some points outside bounds
(define r3 (img-triangle img400x200 -10 50 30 10 30 90 1))
(define r4 (img-triangle img400x200 370 170 420 170 395 220 1))

;; Draw degenerate triangle (all points on same line)
(define r5 (img-triangle img400x200 300 100 310 100 320 100 1))

;; Draw triangle with same points (degenerate)
(define r6 (img-triangle img400x200 50 50 50 50 50 50 1))

;; Test with invalid image buffer
(define r7 (trap (img-triangle "not-an-image" 0 0 10 0 5 10 1)))
(define r8 (trap (img-triangle nil 0 0 10 0 5 10 1)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6
         (eq r7 '(exit-error type_error))
         (eq r8 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
