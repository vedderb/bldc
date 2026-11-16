(sdl-init)

(define win (sdl-create-window "Blit palette - indexed4 -> indexed2" 200 200))
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

(define dst (img-buffer 'indexed2 60 60))
(img-clear dst 0)

(define no-palette-result (trap (img-blit dst src 0 0 -1)))

;; Quantize four gray levels down to two: {0,1} -> 0, {2,3} -> 1.
(define blit-ok (img-blit dst src 0 0 -1 '(palette (0 0 1 1))))

(define tl-px (img-getpix dst 15 15))  ; src 0 -> 0
(define tr-px (img-getpix dst 45 15))  ; src 1 -> 0
(define bl-px (img-getpix dst 15 45))  ; src 2 -> 1
(define br-px (img-getpix dst 45 45))  ; src 3 -> 1

;; palette length must equal src depth (4 for indexed4).
(define bad-len-short (trap (img-blit dst src 0 0 -1 '(palette (0 0 1)))))
(define bad-len-long (trap (img-blit dst src 0 0 -1 '(palette (0 0 1 1 1)))))

(disp-render dst 0 0 '(0x000000 0xFFFFFF))
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed4_to_indexed2.png" '(0x000000 0xFFFFFF))

(if (and (eq no-palette-result '(exit-error eval_error))
         blit-ok
         (= tl-px 0) (= tr-px 0) (= bl-px 1) (= br-px 1)
         (eq bad-len-short '(exit-error eval_error))
         (eq bad-len-long '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
