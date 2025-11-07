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

;; Test img-line function
;; Draw horizontal line
(define r1 (img-line img400x200 10 10 50 10 1))

;; Draw vertical line
(define r2 (img-line img400x200 20 20 20 60 1))

;; Draw diagonal line
(define r3 (img-line img400x200 100 50 150 100 1))

;; Test line outside image bounds (should still work but clip)
(define r4 (img-line img400x200 -10 -10 50 50 1))
(define r5 (img-line img400x200 350 150 450 250 1))

;; Test with invalid image buffer
(define r6 (trap (img-line "not-an-image" 0 0 10 10 1)))
(define r7 (trap (img-line nil 0 0 10 10 1)))

;; Test with invalid color for indexed2 (should work with any value)
(define r8 (img-line img400x200 200 100 250 100 5))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5
         (eq r6 '(exit-error type_error))
         (eq r7 '(exit-error type_error))
         r8)
    (print "SUCCESS")
    (print "FAILURE"))
