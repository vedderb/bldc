(sdl-init)

(define win (sdl-create-window "Display library - blit tile test" 400 300))
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

;; Create various source patterns for tiling
(define tile_small (img-buffer 'indexed4 8 8))
(define tile_rect (img-buffer 'indexed4 12 6))
(define tile_pattern (img-buffer 'indexed4 16 16))
(define dst_img (img-buffer 'indexed4 400 300))

;; Create a small checkerboard pattern
(img-rectangle tile_small 0 0 4 4 1)
(img-rectangle tile_small 4 4 4 4 1)
(img-rectangle tile_small 0 4 4 4 2)
(img-rectangle tile_small 4 0 4 4 2)

;; Create a simple stripe pattern
(img-rectangle tile_rect 0 0 4 6 1)
(img-rectangle tile_rect 4 0 4 6 2)
(img-rectangle tile_rect 8 0 4 6 3)

;; Create a more complex pattern
(img-circle tile_pattern 8 8 6 1)
(img-rectangle tile_pattern 4 4 8 8 2)
(img-triangle tile_pattern 0 0 8 0 4 8 3)

;; Test basic tiling - should fill entire destination
(define r1 (img-blit dst_img tile_small 0 0 -1 '(tile)))

;; Clear and test tiling with rectangular pattern
(img-clear dst_img 0)
(define r2 (img-blit dst_img tile_rect 0 0 -1 '(tile)))

;; Clear and test tiling with complex pattern
(img-clear dst_img 0)
(define r3 (img-blit dst_img tile_pattern 0 0 -1 '(tile)))

;; Test tiling with clipping - should tile only within clipped area
(img-clear dst_img 0)
(define r4 (img-blit dst_img tile_small 50 50 -1 '(tile) '(clip 50 50 100 80)))

;; Test tiling with offset starting position
(img-clear dst_img 0)
(define r5 (img-blit dst_img tile_rect 20 30 -1 '(tile)))

;; Test tiling with transparency
(img-clear dst_img 0)
(img-rectangle dst_img 0 0 400 300 3) ; Fill background with color 3
(define r6 (img-blit dst_img tile_small 0 0 0 '(tile))) ; Tile with color 0 transparent

;; Test tiling combined with scaling (if supported)
(img-clear dst_img 0)
(define r7 (trap (img-blit dst_img tile_small 0 0 -1 '(tile) '(scale 2.0))))

;; Test tiling with very small source
(define tiny_tile (img-buffer 'indexed4 2 2))
(img-setpix tiny_tile 0 0 1)
(img-setpix tiny_tile 1 1 2)
(img-clear dst_img 0)
(define r8 (img-blit dst_img tiny_tile 0 0 -1 '(tile)))

;; Test tiling with source larger than destination area
(define large_tile (img-buffer 'indexed4 50 50))
(img-circle large_tile 25 25 20 1)
(img-rectangle large_tile 10 10 30 30 2)
(img-clear dst_img 0)
(define r9 (img-blit dst_img large_tile 0 0 -1 '(tile) '(clip 0 0 60 45)))

;; Test tiling edge cases
(define r10 (trap (img-blit dst_img tile_small 0 0 -1 '(tile invalid))))  ; invalid additional property
(define r11 (trap (img-blit "invalid" tile_small 0 0 -1 '(tile))))        ; invalid destination

;; Test single pixel tile
(define pixel_tile (img-buffer 'indexed4 1 1))
(img-setpix pixel_tile 0 0 2)
(img-clear dst_img 0)
(define r12 (img-blit dst_img pixel_tile 100 100 -1 '(tile) '(clip 100 100 50 50)))

;; Display final result (last test with pixel tiling in small area)
(disp-render dst_img 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

;; Test buffer properties
(define dims (img-dims dst_img))
(define is_buffer (img-buffer? dst_img))

(if (and r1 r2 r3 r4 r5 r6 r8 r9 r12 is_buffer (eq dims '(400 300)))
    (print "SUCCESS")
    (print "FAILURE"))
