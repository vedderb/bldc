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

;; Test img-rectangle function
;; Draw normal rectangle
(define r1 (img-rectangle img400x200 50 50 100 80 1))

;; Draw rectangle at edge
(define r2 (img-rectangle img400x200 0 0 50 50 1))
(define r3 (img-rectangle img400x200 350 150 50 50 1))

;; Draw rectangle partially outside bounds
(define r4 (img-rectangle img400x200 -10 -10 30 30 1))
(define r5 (img-rectangle img400x200 380 180 50 50 1))

;; Test with zero width/height
(define r6 (img-rectangle img400x200 200 100 0 20 1))
(define r7 (img-rectangle img400x200 200 100 20 0 1))

;; Test with negative dimensions (should handle gracefully)
(define r8 (img-rectangle img400x200 100 100 -20 20 1))

;; Test with invalid image buffer
(define r9 (trap (img-rectangle "not-an-image" 0 0 10 10 1)))
(define r10 (trap (img-rectangle nil 0 0 10 10 1)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8
         (eq r9 '(exit-error type_error))
         (eq r10 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
