(sdl-init)

(define win (sdl-create-window "Display library - built-in fonts test" 300 150))
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

(define img300x150 (img-buffer 'indexed2 300 150))

;; Font 0: Tom Thumb 4x6
(define r1 (img-text img300x150 10 10 1 0 0 "Font 0 Hello"))

;; Font 1: retro 5x7
(define r2 (img-text img300x150 10 40 1 0 1 "Font 1 Hello"))

;; Font 2: IBM VGA 8x8
(define r3 (img-text img300x150 10 70 1 0 2 "Font 2 Hello"))

;; Font 3: luizbills 4x6
(define r4 (img-text img300x150 10 100 1 0 3 "Font 3 Hello"))

;; No explicit font-id uses TINYGFX_DEFAULT_FONT_ID
(define r5 (img-text img300x150 10 130 1 0 "Default font"))

;; Unknown font-id should fail
(define r6 (trap (img-text img300x150 10 10 1 0 99 "bad")))

;; Display the result
(disp-render img300x150 0 0 '(0x000000 0xFFFFFF))
(save-img img300x150 "sdl_tests/png_out/test_img_text_fonts.png" '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5
         (eq r6 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
