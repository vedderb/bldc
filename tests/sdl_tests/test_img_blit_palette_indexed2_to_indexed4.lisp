(sdl-init)

(define win (sdl-create-window "Blit palette - indexed2 -> indexed4" 200 200))
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

(define dst (img-buffer 'indexed4 60 60))
(img-clear dst 0)

;; dst (indexed4) has at least as many colors as src (indexed2), so no
;; palette is required: src index i identity-maps to dst index i.
;; See test_img_blit_indexed_identity.lisp for dedicated coverage of that.
(define identity-ok (img-blit dst src 0 0 -1))
(define bg-identity-px (img-getpix dst 2 2))    ; src index 0 -> 0
(define fg-identity-px (img-getpix dst 30 30))  ; src index 1 -> 1

(img-clear dst 0)

;; An explicit palette remaps src {0,1} -> dst {2,3} instead.
(define blit-ok (img-blit dst src 0 0 -1 '(palette (2 3))))

(define bg-px (img-getpix dst 2 2))     ; outside the circle -> src index 0 -> 2
(define fg-px (img-getpix dst 30 30))   ; circle center -> src index 1 -> 3

;; palette length must equal src depth (2 for indexed2), not more or less.
(define bad-len-short (trap (img-blit dst src 0 0 -1 '(palette (2)))))
(define bad-len-long (trap (img-blit dst src 0 0 -1 '(palette (2 3 1)))))

(disp-render dst 0 0 '(0x000000 0xFFFFFF 0x3080E0 0xE04030))
(save-img dst "sdl_tests/png_out/test_img_blit_palette_indexed2_to_indexed4.png" '(0x000000 0xFFFFFF 0x3080E0 0xE04030))

(if (and identity-ok
         (= bg-identity-px 0)
         (= fg-identity-px 1)
         blit-ok
         (= bg-px 2)
         (= fg-px 3)
         (eq bad-len-short '(exit-error eval_error))
         (eq bad-len-long '(exit-error eval_error)))
    (print "SUCCESS")
    (print "FAILURE"))
