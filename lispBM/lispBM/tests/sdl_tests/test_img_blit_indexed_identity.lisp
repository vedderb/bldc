(sdl-init)

(define win (sdl-create-window "Blit identity - indexed to indexed, no palette" 200 200))
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

;; When blitting indexed -> indexed with no palette attribute, a
;; destination with at least as many colors as the source should accept
;; the blit and map source index i to dest index i unchanged.

;; indexed2 source: background 0, filled circle index 1.
(define src2 (img-buffer 'indexed2 60 60))
(img-clear src2 0)
(img-circle src2 30 30 20 1 '(filled))

;; indexed4 source: four quadrants, indices 0 1 2 3.
(define src4 (img-buffer 'indexed4 60 60))
(img-clear src4 0)
(img-rectangle src4 30 0  30 30 1 '(filled))
(img-rectangle src4 0  30 30 30 2 '(filled))
(img-rectangle src4 30 30 30 30 3 '(filled))

;; indexed2 -> indexed4 (4 >= 2): identity map, no palette needed.
(define dst4 (img-buffer 'indexed4 60 60))
(img-clear dst4 0)
(define ok-2-to-4 (img-blit dst4 src2 0 0 -1))
(define bg-2-to-4 (img-getpix dst4 2 2))    ; src 0 -> 0
(define fg-2-to-4 (img-getpix dst4 30 30))  ; src 1 -> 1

;; indexed2 -> indexed16 (16 >= 2): identity map, no palette needed.
(define dst16a (img-buffer 'indexed16 60 60))
(img-clear dst16a 0)
(define ok-2-to-16 (img-blit dst16a src2 0 0 -1))
(define bg-2-to-16 (img-getpix dst16a 2 2))    ; src 0 -> 0
(define fg-2-to-16 (img-getpix dst16a 30 30))  ; src 1 -> 1

;; indexed4 -> indexed16 (16 >= 4): identity map, no palette needed.
(define dst16b (img-buffer 'indexed16 60 60))
(img-clear dst16b 0)
(define ok-4-to-16 (img-blit dst16b src4 0 0 -1))
(define tl-4-to-16 (img-getpix dst16b 15 15))  ; src 0 -> 0
(define tr-4-to-16 (img-getpix dst16b 45 15))  ; src 1 -> 1
(define bl-4-to-16 (img-getpix dst16b 15 45))  ; src 2 -> 2
(define br-4-to-16 (img-getpix dst16b 45 45))  ; src 3 -> 3

;; indexed4 -> indexed2 (2 < 4): dest has fewer colors, still requires a
;; palette -- no palette should still error.
(define dst2 (img-buffer 'indexed2 60 60))
(img-clear dst2 0)
(define bad-4-to-2 (trap (img-blit dst2 src4 0 0 -1)))

;; indexed16 -> indexed4 (4 < 16): dest has fewer colors, still requires a
;; palette -- no palette should still error.
(define src16 (img-buffer 'indexed16 60 60))
(img-clear src16 5)
(define bad-16-to-4 (trap (img-blit dst4 src16 0 0 -1)))

;; Supplying an explicit palette still overrides identity mapping even
;; when dest has enough colors for it to apply.
(img-clear dst4 0)
(define override-ok (img-blit dst4 src2 0 0 -1 '(palette (2 3))))
(define bg-override (img-getpix dst4 2 2))    ; src 0 -> 2
(define fg-override (img-getpix dst4 30 30))  ; src 1 -> 3

(disp-render dst16b 0 0 '(0x000000 0xFFFFFF 0x3080E0 0xE04030))
(save-img dst16b "sdl_tests/png_out/test_img_blit_indexed_identity.png" '(0x000000 0xFFFFFF 0x3080E0 0xE04030))

(if (and ok-2-to-4 (= bg-2-to-4 0) (= fg-2-to-4 1)
         ok-2-to-16 (= bg-2-to-16 0) (= fg-2-to-16 1)
         ok-4-to-16
         (= tl-4-to-16 0) (= tr-4-to-16 1) (= bl-4-to-16 2) (= br-4-to-16 3)
         (eq bad-4-to-2 '(exit-error eval_error))
         (eq bad-16-to-4 '(exit-error eval_error))
         override-ok (= bg-override 2) (= fg-override 3))
    (print "SUCCESS")
    (print "FAILURE"))
