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
(define font-file (fopen "./sdl_tests/Ubuntu-Regular.ttf" "r"))
(define font-data (load-file font-file))
(define font (ttf-prepare font-data 32 'indexed4 " HeloWrdTts123!@#EgfCrnx"))

(define aa-red '(0 4456448 10027008 16711680))

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

(define img400x200 (img-buffer 'rgb888 400 200))

;; Test img-text function
;; Draw simple text
(define r1 (ttf-text img400x200 50 50 aa-red font "Hello"))

;; Draw text at different position
(define r2 (ttf-text img400x200  50 80 aa-red font "World"))

;; Draw empty string
(define r3 (ttf-text img400x200 50 110 aa-red font ""))

;; Draw text with special characters
(define r4 (ttf-text img400x200 50 140 aa-red  font "Test 123!@#"))

;; Draw text partially outside bounds
(define r5 (ttf-text img400x200  -5 10 aa-red font "Edge"))
(define r6 (ttf-text img400x200  350 180 aa-red font "Corner"))

;; Test with invalid image buffer - function doesn't validate, returns t
(define r7 (trap (ttf-text "not-an-image" 0 0 aa-red  font "text")))
(define r8 (trap (ttf-text nil 0 0 aa-red  font "text")))

;; Test with invalid text argument - function doesn't validate, returns t
(define r9 (trap (ttf-text img400x200  0 0 aa-red font 123)))

;; Test with newline
(define r10 (ttf-text img400x200 50 50 aa-red font "Hel\nlo"))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6 r10
         (eq r7 '(exit-error type_error))
         (eq r8 '(exit-error type_error))
         (eq r9 '(exit-error type_error))
         )
    (print "SUCCESS")
    (print "FAILURE"))
