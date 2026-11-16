(sdl-init)

(define win (sdl-create-window "Blit palette - indexed16 -> rgb888" 200 200))
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

;; indexed16 source: 16 single pixels, one per index value, spaced 4px apart.
(define src (img-buffer 'indexed16 64 4))
(img-clear src 0)
(img-setpix src 0  0 0)  (img-setpix src 4  0 1)  (img-setpix src 8  0 2)  (img-setpix src 12 0 3)
(img-setpix src 16 0 4)  (img-setpix src 20 0 5)  (img-setpix src 24 0 6)  (img-setpix src 28 0 7)
(img-setpix src 32 0 8)  (img-setpix src 36 0 9)  (img-setpix src 40 0 10) (img-setpix src 44 0 11)
(img-setpix src 48 0 12) (img-setpix src 52 0 13) (img-setpix src 56 0 14) (img-setpix src 60 0 15)

(define dst (img-buffer 'rgb888 64 4))
(img-clear dst 0)

(define no-palette-result (trap (img-blit dst src 0 0 -1)))

;; A 16-entry grayscale ramp: src index i -> gray level i*0x101010.
(define rgb-palette
  (list 0x000000 0x101010 0x202020 0x303030
        0x404040 0x505050 0x606060 0x707070
        0x808080 0x909090 0xA0A0A0 0xB0B0B0
        0xC0C0C0 0xD0D0D0 0xE0E0E0 0xF0F0F0))

(define blit-ok (img-blit dst src 0 0 -1 (list 'palette rgb-palette)))

(define ok
  (and (= (img-getpix dst 0  0) 0x000000) (= (img-getpix dst 4  0) 0x101010)
       (= (img-getpix dst 32 0) 0x808080) (= (img-getpix dst 60 0) 0xF0F0F0)))

;; palette length must equal src depth (16 for indexed16).
(define bad-len-short (trap (img-blit dst src 0 0 -1 (list 'palette (take rgb-palette 15)))))

(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed16_to_rgb888.png")

(if (and (eq no-palette-result '(exit-error eval_error))
         blit-ok ok
         (eq bad-len-short '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
