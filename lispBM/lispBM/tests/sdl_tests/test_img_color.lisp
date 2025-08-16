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

;; Test img-color function
;; Create regular color
(define color1 (img-color 'regular 0xFF0000))  ;; Red
(define color2 (img-color 'regular 0x00FF00))  ;; Green
(define color3 (img-color 'regular 0x0000FF))  ;; Blue

;; Create gradient colors
(define color4 (img-color 'gradient_x 0xFF0000 0x0000FF 100 0))
(define color5 (img-color 'gradient_y 0x00FF00 0xFF00FF 200 50))

;; Test with invalid color type
(define color6 (trap (img-color 'invalid-type 0xFF0000)))

;; Test with minimal arguments for gradient - should work with defaults
(define color7 (img-color 'gradient_x 0xFF0000))

;; Test with too many arguments for regular color
(define color8 (img-color 'regular 0xFF0000 0x00FF00 100 50))  ;; Should work, extra args ignored

;; Test with invalid arguments
(define color9 (trap (img-color "not-a-symbol" 0xFF0000)))

;; Verify results exist (colors are opaque data structures)
(define r1 (not (eq color1 nil)))
(define r2 (not (eq color2 nil)))
(define r3 (not (eq color3 nil)))
(define r4 (not (eq color4 nil)))
(define r5 (not (eq color5 nil)))
(define r6 (eq color6 '(exit-error type_error)))
(define r7 (not (eq color7 nil)))
(define r8 (not (eq color8 nil)))
(define r9 (eq color9 '(exit-error type_error)))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9)
    (print "SUCCESS")
    (print "FAILURE"))