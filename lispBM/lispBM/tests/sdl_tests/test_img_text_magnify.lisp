(sdl-init)

(define win (sdl-create-window "Display library - text magnification test" 400 300))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(define font-file (f-open "./sdl_tests/font_16_26.bin" "r"))
(define font (load-file font-file))

(sdl-set-active-renderer rend)

(define img (img-buffer 'indexed2 400 300))

;; font is 16x26px per char; with magnification:
;; mag=1: height=26px   (y=5..31)
;; mag=2: height=52px   (y=35..87)
;; mag=3: height=78px   (y=95..173)
;; mag=4: height=104px  (y=180..284)

(define r1 (img-text img 5  5   1 0 font "Hi" '(magnify 1)))
(define r2 (img-text img 5  35  1 0 font "Hi" '(magnify 2)))
(define r3 (img-text img 5  95  1 0 font "Hi" '(magnify 3)))
(define r4 (img-text img 5  180 1 0 font "Hi" '(magnify 4)))

(disp-render img 0 0 '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4)
    (print "SUCCESS")
    (print "FAILURE"))
