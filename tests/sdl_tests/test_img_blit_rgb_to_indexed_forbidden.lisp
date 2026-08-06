(sdl-init)

(define win (sdl-create-window "Blit rgb -> indexed forbidden" 200 200))
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

;; blitting a rgb source into an indexed destination is never supported, with
;; or without a palette attribute -- there is no sensible way to turn an
;; arbitrary rgb888 color into a dest index automatically.
(define src332 (img-buffer 'rgb332 20 20))
(define src565 (img-buffer 'rgb565 20 20))
(define src888 (img-buffer 'rgb888 20 20))
(img-clear src332 0xFF0000)
(img-clear src565 0xFF0000)
(img-clear src888 0xFF0000)

(define dst2  (img-buffer 'indexed2 20 20))
(define dst4  (img-buffer 'indexed4 20 20))
(define dst16 (img-buffer 'indexed16 20 20))
(img-clear dst2 0)
(img-clear dst4 0)
(img-clear dst16 0)

(define r1 (trap (img-blit dst2  src332 0 0 -1)))
(define r2 (trap (img-blit dst4  src565 0 0 -1)))
(define r3 (trap (img-blit dst16 src888 0 0 -1)))

;; Still forbidden even with a palette attribute given.
(define r4 (trap (img-blit dst4 src888 0 0 -1 '(palette (0 1 2 3)))))

;; rgb -> rgb (any subformat pair) remains unaffected and still works.
(define dst_rgb (img-buffer 'rgb565 20 20))
(define r5 (img-blit dst_rgb src888 0 0 -1))

(disp-render dst4 0 0 '(0x000000 0xFFFFFF))
(save-img dst4 "sdl_tests/png_out/test_img_blit_rgb_to_indexed_forbidden.png" '(0x000000 0xFFFFFF))

(if (and (eq r1 '(exit-error eval_error))
         (eq r2 '(exit-error eval_error))
         (eq r3 '(exit-error eval_error))
         (eq r4 '(exit-error eval_error))
         r5)
    (print "SUCCESS")
    (print "FAILURE"))
