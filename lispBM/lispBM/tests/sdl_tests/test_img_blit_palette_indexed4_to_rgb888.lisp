(sdl-init)

(define win (sdl-create-window "Blit palette - indexed4 -> rgb888" 200 200))
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

;; indexed4 source: four quadrants, indices 0 1 2 3.
(define src (img-buffer 'indexed4 60 60))
(img-clear src 0)                                   ; top-left    -> 0
(img-rectangle src 30 0  30 30 1 '(filled))          ; top-right   -> 1
(img-rectangle src 0  30 30 30 2 '(filled))          ; bottom-left -> 2
(img-rectangle src 30 30 30 30 3 '(filled))          ; bottom-right-> 3

(define dst (img-buffer 'rgb888 60 60))
(img-clear dst 0)

(define no-palette-result (trap (img-blit dst src 0 0 -1)))

(define blit-ok (img-blit dst src 0 0 -1
                           '(palette (0x000000 0xFF0000 0x00FF00 0x0000FF))))

(define tl-px (img-getpix dst 15 15))  ; src 0 -> black
(define tr-px (img-getpix dst 45 15))  ; src 1 -> red
(define bl-px (img-getpix dst 15 45))  ; src 2 -> green
(define br-px (img-getpix dst 45 45))  ; src 3 -> blue

(define bad-len-short (trap (img-blit dst src 0 0 -1 '(palette (0x000000 0xFF0000)))))
(define bad-len-long (trap (img-blit dst src 0 0 -1
                                      '(palette (0x000000 0xFF0000 0x00FF00 0x0000FF 0xFFFFFF)))))

(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed4_to_rgb888.png")

(if (and (eq no-palette-result '(exit-error eval_error))
         blit-ok
         (= tl-px 0x000000) (= tr-px 0xFF0000) (= bl-px 0x00FF00) (= br-px 0x0000FF)
         (eq bad-len-short '(exit-error eval_error))
         (eq bad-len-long '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
