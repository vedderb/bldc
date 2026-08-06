(sdl-init)

(define win (sdl-create-window "Blit palette - indexed16 -> indexed4" 200 200))
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

(define dst (img-buffer 'indexed4 64 4))
(img-clear dst 0)

(define no-palette-result (trap (img-blit dst src 0 0 -1)))

;; Quantize 16 levels down to 4: src index i -> dst index (i / 4).
(define blit-ok (img-blit dst src 0 0 -1
                           '(palette (0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3))))

(define ok
  (and (= (img-getpix dst 0  0) 0) (= (img-getpix dst 4  0) 0)
       (= (img-getpix dst 8  0) 0) (= (img-getpix dst 12 0) 0)
       (= (img-getpix dst 16 0) 1) (= (img-getpix dst 20 0) 1)
       (= (img-getpix dst 24 0) 1) (= (img-getpix dst 28 0) 1)
       (= (img-getpix dst 32 0) 2) (= (img-getpix dst 36 0) 2)
       (= (img-getpix dst 40 0) 2) (= (img-getpix dst 44 0) 2)
       (= (img-getpix dst 48 0) 3) (= (img-getpix dst 52 0) 3)
       (= (img-getpix dst 56 0) 3) (= (img-getpix dst 60 0) 3)))

;; palette length must equal src depth (16 for indexed16).
(define bad-len-short (trap (img-blit dst src 0 0 -1 '(palette (0 0 0 0 1 1 1 1 2 2 2 2 3 3 3)))))
(define bad-len-long (trap (img-blit dst src 0 0 -1 '(palette (0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 3)))))

(disp-render dst 0 0 '(0x000000 0xFFFFFF 0x3080E0 0xE04030))
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed16_to_indexed4.png" '(0x000000 0xFFFFFF 0x3080E0 0xE04030))

(if (and (eq no-palette-result '(exit-error eval_error))
         blit-ok ok
         (eq bad-len-short '(exit-error eval_error))
         (eq bad-len-long '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
