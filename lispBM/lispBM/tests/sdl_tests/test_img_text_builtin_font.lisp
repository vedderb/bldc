(sdl-init)

(define win (sdl-create-window "Display library - built-in font test" 400 220))
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

(define img400x220 (img-buffer 'indexed2 400 220))

;; img-text without an explicit font argument uses the built-in retro5x7 font.

;; Draw simple text
(define r1 (img-text img400x220 50 20 1 0 "Hello"))

;; Draw text at a different position
(define r2 (img-text img400x220 50 40 1 0 "World"))

;; Draw empty string
(define r3 (img-text img400x220 50 60 1 0 ""))

;; Draw text with special characters
(define r4 (img-text img400x220 50 80 1 0 "Test 123!@#"))

;; Draw text partially outside bounds
(define r5 (img-text img400x220 -5 100 1 0 "Edge"))
(define r6 (img-text img400x220 350 190 1 0 "Corner"))

;; Orientation attributes with the built-in font
(define r7 (img-text img400x220 20 200 1 0 "UP" 'up))
(define r8 (img-text img400x220 380 20 1 0 "DOWN" 'down))

;; Magnify attribute with the built-in font
(define r9 (img-text img400x220 150 120 1 0 "Big" '(magnify 3)))

(define r10 (trap (img-text "not-an-image" 0 0 1 0 "text")))
(define r11 (trap (img-text nil 0 0 1 0 "text")))
(define r12 (trap (img-text img400x220 0 0 1 0 123)))

;; Display the result
(disp-render img400x220 0 0 '(0x000000 0xFFFFFF))
(save-img img400x220 "sdl_tests/png_out/test_img_text_builtin_font.png" '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9
         (eq r10 '(exit-error type_error))
         (eq r11 '(exit-error type_error))
         (eq r12 '(exit-error type_error))
         )
    (print "SUCCESS")
    (print "FAILURE"))
