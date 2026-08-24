(sdl-init)

(define win (sdl-create-window "Display library - blit llama rgb332 test" 1120 550))
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
;; test_img_blit_llama.lisp). Load it, then convert it to rgb332 by
;; blitting it once through a palette remap {0,1} -> real rgb888 colors
;; (putpixel converts down to rgb332 automatically). This first blit is
;; indexed2 -> rgb332, a format *change*, so it does NOT go through
;; bulk_blast_bits -- it needs the per-pixel palette-compose path.
(define llama-file (f-open "./sdl_tests/lama2.bin" "r"))
(define llama2 (load-file llama-file))

(define llama332 (img-buffer 'rgb332 256 256))
(img-clear llama332 0)
(define r0 (img-blit llama332 llama2 0 0 -1 '(palette (0x000000 0xFFFFFF))))

;; From here on, source and dest are both rgb332, so plain blits go through
;; bulk_blast_bits. Unlike the packed indexed formats, rgb332 is one whole
;; byte per pixel, so a row is *always* byte-aligned no matter what x is --
;; every one of these 8 blits takes the memcpy fast path, none of them ever
;; reach word_blast_bits. Reusing the same x/y layout as the other llama
;; lineup tests just for a consistent look, alignment isn't a variable here.
(define dst (img-buffer 'rgb332 1120 550))
(img-clear dst)

(define r1 (img-blit dst llama332 0   0   -1))
(define r2 (img-blit dst llama332 283 0   -1))
(define r3 (img-blit dst llama332 560 0   -1))
(define r4 (img-blit dst llama332 843 0   -1))
(define r5 (img-blit dst llama332 0   280 -1))
(define r6 (img-blit dst llama332 283 280 -1))
(define r7 (img-blit dst llama332 560 280 -1))
(define r8 (img-blit dst llama332 843 280 -1))

;; Display and save the result -- no palette needed, rgb332 pixels are
;; already real colors.
(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_llama_rgb332.png")

(if (and r0 r1 r2 r3 r4 r5 r6 r7 r8)
    (print "SUCCESS")
    (print "FAILURE"))
