(sdl-init)

(define win (sdl-create-window "Display library - blit llama test" 1120 550))
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

;; lama2.bin is a pre-made indexed2 (256x256) image_buffer_t, the same asset
;; used as "llama-bin" throughout doc/displayref.lisp. Loading it is just a
;; raw deserialize, no decode step -- unlike a JPEG source, it's already a
;; real (recognizable) indexed image, so it's a much better source for
;; exercising same-format indexed blits than a threshold-converted photo.
(define llama-file (f-open "./sdl_tests/lama2.bin" "r"))
(define llama (load-file llama-file))

;; Plain (no rotate/scale/tile/compose) same-format blits go through
;; bulk_blast_bits. x-offsets alternate between multiples of 8 (byte-aligned,
;; memcpy per row for indexed2's 8-pixels-per-byte packing) and
;; non-multiples of 8 (misaligned, word_blast_bits per row).
(define dst (img-buffer 'indexed2 1120 550))
(img-clear dst)

(define r1 (img-blit dst llama 0   0   -1))  ; x=0,   aligned
(define r2 (img-blit dst llama 283 0   -1))  ; x=283, misaligned (283 mod 8 = 3)
(define r3 (img-blit dst llama 560 0   -1))  ; x=560, aligned
(define r4 (img-blit dst llama 843 0   -1))  ; x=843, misaligned (843 mod 8 = 3)
(define r5 (img-blit dst llama 0   280 -1))  ; x=0,   aligned
(define r6 (img-blit dst llama 283 280 -1))  ; x=283, misaligned
(define r7 (img-blit dst llama 560 280 -1))  ; x=560, aligned
(define r8 (img-blit dst llama 843 280 -1))  ; x=843, misaligned

;; Display and save the result with a 2-color palette.
(disp-render dst 0 0 '(0x000000 0xFFFFFF))
(save-img dst "sdl_tests/png_out/test_img_blit_llama.png" '(0x000000 0xFFFFFF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8)
    (print "SUCCESS")
    (print "FAILURE"))
