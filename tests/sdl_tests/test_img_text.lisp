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

;; Load a font
(define font-file (fopen "./sdl_tests/font_16_26.bin" "r"))
(define font (load-file font-file))

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

(define img400x200 (img-buffer 'indexed2 400 200))

;; Test img-text function
;; Draw simple text
(define r1 (img-text img400x200 50 50 1 0 font "Hello"))

;; Draw text at different position
(define r2 (img-text img400x200  50 80 1 0 font "World"))

;; Draw empty string
(define r3 (img-text img400x200 50 110 1 0 font ""))

;; Draw text with special characters
(define r4 (img-text img400x200 50 140 1 0 font "Test 123!@#"))

;; Draw text partially outside bounds
(define r5 (img-text img400x200  -5 10 1  0 font "Edge"))
(define r6 (img-text img400x200  350 180 1  0 font "Corner"))

;; Test with invalid image buffer - function doesn't validate, returns t
(define r7 (trap (img-text "not-an-image" 0 0 1 0  font "text")))
(define r8 (trap (img-text nil 0 0 1 0  font "text")))

;; Test with invalid text argument - function doesn't validate, returns t
(define r9 (trap (img-text img400x200  0 0 1 0 font 123)))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6
         (eq r7 '(exit-error type_error))
         (eq r8 '(exit-error type_error))
         (eq r9 '(exit-error type_error))
         )
    (print "SUCCESS")
    (print "FAILURE"))
