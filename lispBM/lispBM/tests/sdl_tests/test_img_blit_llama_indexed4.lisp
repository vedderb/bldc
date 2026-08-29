(sdl-init)

(define win (sdl-create-window "Display library - blit llama indexed4 test" 1120 550))
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

;; lama2.bin is a pre-made indexed2 (256x256) image_buffer_t (see
;; test_img_blit_llama.lisp). Load it, then convert it to indexed4 by
;; blitting it once through a palette remap {0,1} -> {0,1}. This first
;; blit is indexed2 -> indexed4, a format *change*, so it does NOT go
;; through bulk_blast_bits -- it needs the per-pixel palette-compose path,
;; since indexed formats carry no color information to convert generically.
(define llama-file (f-open "./sdl_tests/lama2.bin" "r"))
(define llama2 (load-file llama-file))

(define llama4 (img-buffer 'indexed4 256 256))
(img-clear llama4 0)
(define r0 (img-blit llama4 llama2 0 0 -1 '(palette (0 1))))

;; From here on, source and dest are both indexed4, so plain blits go
;; through bulk_blast_bits. indexed4 packs 4 pixels per byte, so a row is
;; byte-aligned exactly when x mod 4 == 0. Reusing the same x/y layout as
;; test_img_blit_llama.lisp's indexed2 lineup happens to give the same
;; aligned/misaligned split here too (0 and 560 are multiples of 4, 283 and
;; 843 are not).
(define dst (img-buffer 'indexed4 1120 550))
(img-clear dst)

(define r1 (img-blit dst llama4 0   0   -1))  ; x=0,   aligned
(define r2 (img-blit dst llama4 283 0   -1))  ; x=283, misaligned (283 mod 4 = 3)
(define r3 (img-blit dst llama4 560 0   -1))  ; x=560, aligned
(define r4 (img-blit dst llama4 843 0   -1))  ; x=843, misaligned (843 mod 4 = 3)
(define r5 (img-blit dst llama4 0   280 -1))  ; x=0,   aligned
(define r6 (img-blit dst llama4 283 280 -1))  ; x=283, misaligned
(define r7 (img-blit dst llama4 560 280 -1))  ; x=560, aligned
(define r8 (img-blit dst llama4 843 280 -1))  ; x=843, misaligned

;; Display and save the result with a 2-color palette (only indices 0/1 used).
(disp-render dst 0 0 '(0x000000 0xFFFFFF 0x000000 0x000000))
(save-img dst "sdl_tests/png_out/test_img_blit_llama_indexed4.png" '(0x000000 0xFFFFFF 0x000000 0x000000))

(if (and r0 r1 r2 r3 r4 r5 r6 r7 r8)
    (print "SUCCESS")
    (print "FAILURE"))
