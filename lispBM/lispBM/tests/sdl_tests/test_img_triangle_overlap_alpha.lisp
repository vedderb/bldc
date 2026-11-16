(sdl-init)

(define win (sdl-create-window "Display library - overlapping alpha triangles" 320 320))
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

(define img (img-buffer 'rgb888 320 320))
(define bg 0x101018)
(img-clear img bg)

;; Alpha now lives entirely in img-blit, not in the shapes themselves: each
;; triangle is drawn *opaque* into its own scratch buffer, sized to its
;; bounding box, then composited onto img with img-blit's 'alpha attribute.
;; marker is the scratch buffer's background fill -- passed as img-blit's
;; transparent_color so only the triangle's own pixels participate.
(define marker 0x00FF01)

(defun min2 (a b) (if (< a b) a b))
(defun max2 (a b) (if (> a b) a b))
(defun min3 (a b c) (min2 a (min2 b c)))
(defun max3 (a b c) (max2 a (max2 b c)))

(defun triangle-layer (dest x0 y0 x1 y1 x2 y2 color alpha)
  (let ((minx (min3 x0 x1 x2))
        (maxx (max3 x0 x1 x2))
        (miny (min3 y0 y1 y2))
        (maxy (max3 y0 y1 y2))
        (margin 2))
    (let ((ox (- minx margin))
          (oy (- miny margin))
          (w (+ (- maxx minx) (* 2 margin)))
          (h (+ (- maxy miny) (* 2 margin))))
      (let ((buf (img-buffer 'rgb888 w h)))
        (progn
          (img-clear buf marker)
          (img-triangle buf (- x0 ox) (- y0 oy) (- x1 ox) (- y1 oy) (- x2 ox) (- y2 oy) color '(filled))
          (img-blit dest buf ox oy marker (list 'alpha alpha)))))))

;; Three overlapping translucent right triangles, each the same shape shifted
;; diagonally by 40px from the previous one, drawn red -> green -> blue. This
;; exercises fill_triangle's opaque rasterization together with blit's alpha
;; compositing, with genuine 1x, 2x and 3x overlap.
(define alpha-v 180)
(define red-rgb   0xE04030)
(define green-rgb 0x30C060)
(define blue-rgb  0x3080E0)

(define t1 (triangle-layer img 50  50  220 50  50  220 red-rgb   alpha-v))
(define t2 (triangle-layer img 90  90  260 90  90  260 green-rgb alpha-v))
(define t3 (triangle-layer img 130 130 300 130 130 300 blue-rgb  alpha-v))

;; Reference alpha blend, matching alpha_blend_rgb888's div255(src*a + dst*(255-a) + 127).
(defun blend1 (src dst alpha) (/ (+ (* src alpha) (+ (* dst (- 255 alpha)) 127)) 255))
(defun blend-px (src dst alpha)
  (let ((sr (shr (bitwise-and src 0xFF0000) 16))
        (sg (bitwise-and (shr src 8) 0xFF))
        (sb (bitwise-and src 0xFF))
        (dr (shr (bitwise-and dst 0xFF0000) 16))
        (dg (bitwise-and (shr dst 8) 0xFF))
        (db (bitwise-and dst 0xFF)))
    (+ (shl (blend1 sr dr alpha) 16) (+ (shl (blend1 sg dg alpha) 8) (blend1 sb db alpha)))))

;; Sample points chosen by simulating fill_triangle's exact rasterization
;; offline, so each is confidently inside only the intended set of triangles
;; (well clear of any edge, immune to +/-1px rounding).
(define px-t1-only     (img-getpix img 60  60))
(define px-t1-t2       (img-getpix img 120 120))
(define px-t2-only     (img-getpix img 200 120))
(define px-t1-t2-t3    (img-getpix img 133 132))
(define px-background  (img-getpix img 20  270))

(define expected-t1-only  (blend-px red-rgb bg alpha-v))
(define expected-t2-only  (blend-px green-rgb bg alpha-v))
(define expected-t1-t2    (blend-px green-rgb expected-t1-only alpha-v))
(define expected-t1-t2-t3 (blend-px blue-rgb expected-t1-t2 alpha-v))

(define checks-ok (and (= px-t1-only    expected-t1-only)
                        (= px-t2-only    expected-t2-only)
                        (= px-t1-t2      expected-t1-t2)
                        (= px-t1-t2-t3   expected-t1-t2-t3)
                        (= px-background bg)))

(define dims (img-dims img))
(define is_buffer (img-buffer? img))

(disp-render img 0 0)
(save-img img "sdl_tests/png_out/test_img_triangle_overlap_alpha.png")

(if (and t1 t2 t3
         is_buffer (eq dims '(320 320))
         checks-ok)
    (print "SUCCESS")
    (print "FAILURE"))
