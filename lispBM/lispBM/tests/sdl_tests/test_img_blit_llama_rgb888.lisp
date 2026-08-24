(sdl-init)

(define win (sdl-create-window "Display library - blit llama rgb888 test" 290 290))
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
;; test_img_blit_llama.lisp). Load it, then convert it to rgb888 by
;; blitting it once through a palette remap {0,1} -> real rgb888 colors,
;; scaled to 128x128 -- rgb888 is 3 bytes/pixel, the most memory-hungry
;; format, so even a 4-copy non-overlapping lineup at full 256x256 would
;; come close to the sdl_tests suite's default -M memory budget. This
;; first blit is indexed2 -> rgb888, a format *change*, so it does NOT go
;; through bulk_blast_bits -- it needs the per-pixel palette-compose path
;; (the scale doesn't change that).
(define llama-file (f-open "./sdl_tests/lama2.bin" "r"))
(define llama2 (load-file llama-file))

(define llama888 (img-buffer 'rgb888 128 128))
(img-clear llama888 0)
(define r0 (img-blit llama888 llama2 0 0 -1 '(palette (0x000000 0xFFFFFF)) '(scale 0.5)))

;; From here on, source and dest are both rgb888, so plain blits go through
;; bulk_blast_bits. Like rgb332/rgb565, rgb888 is a whole number of bytes
;; per pixel (3 bytes), so a row is *always* byte-aligned no matter what x
;; is -- all 4 of these blits take the memcpy fast path, none of them ever
;; reach word_blast_bits. Just 4 llamas this time to keep memory use down.
(define dst (img-buffer 'rgb888 290 290))
(img-clear dst)

(define r1 (img-blit dst llama888 0   0   -1))
(define r2 (img-blit dst llama888 162 0   -1))
(define r3 (img-blit dst llama888 0   162 -1))
(define r4 (img-blit dst llama888 162 162 -1))

;; Display and save the result -- no palette needed, rgb888 pixels are
;; already real colors.
(disp-render dst 0 0)
(save-img dst "sdl_tests/png_out/test_img_blit_llama_rgb888.png")

(if (and r0 r1 r2 r3 r4)
    (print "SUCCESS")
    (print "FAILURE"))
