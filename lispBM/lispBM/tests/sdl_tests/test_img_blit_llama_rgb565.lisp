(sdl-init)

(define win (sdl-create-window "Display library - blit llama rgb565 test" 580 290))
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
;; test_img_blit_llama.lisp). Load it, then convert it to rgb565 by
;; blitting it once through a palette remap {0,1} -> real rgb888 colors
;; (putpixel converts down to rgb565 automatically), scaled to 128x128 --
;; rgb565 is 2 bytes/pixel, so an 8-copy non-overlapping lineup at full
;; 256x256 would need a canvas bigger than the sdl_tests suite's default
;; -M memory budget. This first blit is indexed2 -> rgb565, a format
;; *change*, so it does NOT go through bulk_blast_bits -- it needs the
;; per-pixel palette-compose path (the scale doesn't change that).
(define llama-file (f-open "./sdl_tests/lama2.bin" "r"))
(define llama2 (load-file llama-file))

(define llama565 (img-buffer 'rgb565 128 128))
(img-clear llama565 0)
(define r0 (img-blit llama565 llama2 0 0 -1 '(palette (0x000000 0xFFFFFF)) '(scale 0.5)))

;; From here on, source and dest are both rgb565, so plain blits go through
;; bulk_blast_bits. Like rgb332, rgb565 is a whole number of bytes per
;; pixel (2 bytes), so a row is *always* byte-aligned no matter what x is --
;; every one of these 8 blits takes the memcpy fast path, none of them ever
;; reach word_blast_bits. Unlike the packed-format lineups, x doesn't need
;; to be chosen for alignment here, any non-overlapping grid will do.
(define dst (img-buffer 'rgb565 580 290))
(img-clear dst)

(define r1 (img-blit dst llama565 0   0   -1))
(define r2 (img-blit dst llama565 145 0   -1))
(define r3 (img-blit dst llama565 290 0   -1))
(define r4 (img-blit dst llama565 435 0   -1))
(define r5 (img-blit dst llama565 0   145 -1))
(define r6 (img-blit dst llama565 145 145 -1))
(define r7 (img-blit dst llama565 290 145 -1))
(define r8 (img-blit dst llama565 435 145 -1))

;; Display and save the result -- no palette needed, rgb565 pixels are
;; already real colors.
(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_llama_rgb565.png")

(if (and r0 r1 r2 r3 r4 r5 r6 r7 r8)
    (print "SUCCESS")
    (print "FAILURE"))
