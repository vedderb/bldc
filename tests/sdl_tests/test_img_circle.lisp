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

;; Test img-circle function
;; Draw circle at center
(define r1 (img-circle img400x200 200 100 30 1))

;; Draw smaller circle
(define r2 (img-circle img400x200 100 80 20 1))

;; Draw circle partially outside bounds
(define r3 (img-circle img400x200 10 10 15 1))
(define r4 (img-circle img400x200 390 190 15 1))

;; Test with zero radius
(define r5 (img-circle img400x200 50 50 0 1))

;; Test with negative coordinates (should still work)
(define r6 (img-circle img400x200 -10 50 20 1))

;; Test with invalid image buffer 
(define r7 (trap (img-circle "not-an-image" 100 100 20 1)))
(define r8 (trap (img-circle nil 100 100 20 1)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6
         (eq r7 '(exit-error type_error))
         (eq r8 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
