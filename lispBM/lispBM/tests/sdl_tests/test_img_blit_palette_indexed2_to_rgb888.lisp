(sdl-init)

(define win (sdl-create-window "Blit palette - indexed2 -> rgb888" 200 200))
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

;; indexed2 source: background 0, filled circle index 1.
(define src (img-buffer 'indexed2 60 60))
(img-clear src 0)
(img-circle src 30 30 20 1 '(filled))

(define dst (img-buffer 'rgb888 60 60))
(img-clear dst 0)

;; rgb destination from an indexed source also requires a palette, this time
;; mapping src indices directly to real rgb888 colors.
(define no-palette-result (trap (img-blit dst src 0 0 -1)))

(define blit-ok (img-blit dst src 0 0 -1 '(palette (0x101018 0xE04030))))

(define bg-px (img-getpix dst 2 2))     ; outside the circle -> src index 0
(define fg-px (img-getpix dst 30 30))   ; circle center -> src index 1

(define bad-len-short (trap (img-blit dst src 0 0 -1 '(palette (0x101018)))))
(define bad-len-long (trap (img-blit dst src 0 0 -1 '(palette (0x101018 0xE04030 0x30C060)))))

(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed2_to_rgb888.png")

(if (and (eq no-palette-result '(exit-error eval_error))
         blit-ok
         (= bg-px 0x101018)
         (= fg-px 0xE04030)
         (eq bad-len-short '(exit-error eval_error))
         (eq bad-len-long '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
