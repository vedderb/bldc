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

;; Test img-arc function
;; Draw quarter arc (0 to 90 degrees)
(define r1 (img-arc img400x200 100 100 40 0 90 1))

;; Draw half arc (0 to 180 degrees)
(define r2 (img-arc img400x200 250 100 30 0 180 1))

;; Draw full circle arc (0 to 360 degrees)
(define r3 (img-arc img400x200 350 50 20 0 360 1))

;; Draw arc with negative angles
(define r4 (img-arc img400x200 100 150 25 -45 45 1))

;; Draw arc with angles > 360
(define r5 (img-arc img400x200 200 150 20 0 450 1))

;; Draw arc partially outside bounds
(define r6 (img-arc img400x200 10 10 15 0 90 1))
(define r7 (img-arc img400x200 390 190 15 180 270 1))

;; Test with zero radius
(define r8 (img-arc img400x200 50 50 0 0 90 1))

;; Test with invalid image buffer
(define r9 (trap (img-arc "not-an-image" 100 100 20 0 90 1)))
(define r10 (trap (img-arc nil 100 100 20 0 90 1)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8
         (eq r9 '(exit-error type_error))
         (eq r10 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
