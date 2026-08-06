(sdl-init)

(define win (sdl-create-window "Display library - diagonal line thickness test" 500 400))
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

(define img500x400 (img-buffer 'indexed4 500 400))

;; Non-axis-aligned lines, thickness 0 through 6, at a few different slopes
;; to exercise the rasterizer across shallow, 45 degree and steep angles,
;; and both increasing and decreasing y.

;; Shallow slope, going down-right (dx=45, dy=12)
(define r1  (img-line img500x400  20 30  65 42 1 '(thickness 0)))
(define r2  (img-line img500x400  80 30 125 42 1 '(thickness 1)))
(define r3  (img-line img500x400 140 30 185 42 1 '(thickness 2)))
(define r4  (img-line img500x400 200 30 245 42 1 '(thickness 3)))
(define r5  (img-line img500x400 260 30 305 42 1 '(thickness 4)))
(define r6  (img-line img500x400 320 30 365 42 1 '(thickness 5)))
(define r7  (img-line img500x400 380 30 425 42 1 '(thickness 6)))

;; 45 degree slope, going down-right (dx=35, dy=35)
(define r8  (img-line img500x400  20 100  55 135 2 '(thickness 0)))
(define r9  (img-line img500x400  80 100 115 135 2 '(thickness 1)))
(define r10 (img-line img500x400 140 100 175 135 2 '(thickness 2)))
(define r11 (img-line img500x400 200 100 235 135 2 '(thickness 3)))
(define r12 (img-line img500x400 260 100 295 135 2 '(thickness 4)))
(define r13 (img-line img500x400 320 100 355 135 2 '(thickness 5)))
(define r14 (img-line img500x400 380 100 415 135 2 '(thickness 6)))

;; Steep slope, going down-right (dx=12, dy=45)
(define r15 (img-line img500x400  20 180  32 225 3 '(thickness 0)))
(define r16 (img-line img500x400  80 180  92 225 3 '(thickness 1)))
(define r17 (img-line img500x400 140 180 152 225 3 '(thickness 2)))
(define r18 (img-line img500x400 200 180 212 225 3 '(thickness 3)))
(define r19 (img-line img500x400 260 180 272 225 3 '(thickness 4)))
(define r20 (img-line img500x400 320 180 332 225 3 '(thickness 5)))
(define r21 (img-line img500x400 380 180 392 225 3 '(thickness 6)))

;; 45 degree slope, going up-right (dx=35, dy=-35)
(define r22 (img-line img500x400  20 330  55 295 2 '(thickness 0)))
(define r23 (img-line img500x400  80 330 115 295 2 '(thickness 1)))
(define r24 (img-line img500x400 140 330 175 295 2 '(thickness 2)))
(define r25 (img-line img500x400 200 330 235 295 2 '(thickness 3)))
(define r26 (img-line img500x400 260 330 295 295 2 '(thickness 4)))
(define r27 (img-line img500x400 320 330 355 295 2 '(thickness 5)))
(define r28 (img-line img500x400 380 330 415 295 2 '(thickness 6)))

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))
(save-img img500x400 "sdl_tests/png_out/test_img_line_diagonal_thickness.png" '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14
         r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))
