(sdl-init)

(define win (sdl-create-window "Display library - overlapping alpha circles" 300 300))
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

(define img (img-buffer 'rgb888 300 300))
(img-clear img 0x101018)

;; Alpha now lives entirely in img-blit, not in the shapes themselves: each
;; circle is drawn *opaque* into its own small scratch buffer, sized to its
;; bounding box, then composited onto img with img-blit's 'alpha attribute.
;; marker is the scratch buffer's background fill -- passed as img-blit's
;; transparent_color so only the ring/circle pixels themselves participate.
(define marker 0x00FF01)

;; mode is 'filled, a thickness number, or nil for a thin (outline) circle.
(defun circle-layer (dest cx cy r color alpha mode)
  (let ((margin 2))
    (let ((size (+ (* 2 r) (* 2 margin)))
          (lx (+ r margin))
          (ly (+ r margin)))
      (let ((buf (img-buffer 'rgb888 size size)))
        (progn
          (img-clear buf marker)
          (if (eq mode 'filled)
              (img-circle buf lx ly r color '(filled))
              (if mode
                  (img-circle buf lx ly r color (list 'thickness mode))
                  (img-circle buf lx ly r color)))
          (img-blit dest buf (- cx lx) (- cy ly) marker (list 'alpha alpha)))))))

;; Four overlapping thick rings plus one translucent ring through the middle.
;; This exercises blit's alpha-compositing with genuine overlap, and a plain
;; thin outline circle (below) exercises it specifically at its 4 pole pixels.
(define c1 (circle-layer img 110 110 70 0xE04030 160 24))
(define c2 (circle-layer img 170 110 70 0x30C060 160 24))
(define c3 (circle-layer img 110 170 70 0x3080E0 160 24))
(define c4 (circle-layer img 170 170 70 0xE0C030 160 24))
(define c5 (circle-layer img 140 140 30 0xFFFFFF 120 30))

;; A thin (outline, no thickness attribute) alpha circle. Its 4 pole pixels
;; ((cx,cy+-r) and (cx+-r,cy)) must be blended exactly once, not twice --
;; regression test for a bug where the octant mirror's x0==0 step collapsed
;; onto the same 4 pixels via two different putpixel calls each, which is
;; invisible for opaque draws but visibly wrong (over-blended) with alpha.
(define c6 (circle-layer img 250 250 20 0xFFFFFF 128 nil))

(define pole-top   (img-getpix img 250 270))
(define pole-bot   (img-getpix img 250 230))
(define pole-right (img-getpix img 270 250))
(define pole-left  (img-getpix img 230 250))

;; expected: single blend of white(255) at alpha=128 over the 0x101018
;; background, matching alpha_blend_rgb888's div255(src*a + dst*(255-a) + 127)
(defun blend1 (src dst alpha) (/ (+ (* src alpha) (+ (* dst (- 255 alpha)) 127)) 255))
(define expected-r (blend1 0xFF 0x10 128))
(define expected-g (blend1 0xFF 0x10 128))
(define expected-b (blend1 0xFF 0x18 128))
(define expected-pole (+ (shl expected-r 16) (+ (shl expected-g 8) expected-b)))

(define poles-ok (and (= pole-top expected-pole)
                       (= pole-bot expected-pole)
                       (= pole-right expected-pole)
                       (= pole-left expected-pole)))

;; overlap sanity: where red and green rings cross should differ from
;; either ring's own (single-blend) color -- proof genuine compositing
;; happened rather than a flat overwrite.
(define overlap-px (img-getpix img 140 110))
(define red-only-px (img-getpix img 85 110))
(define overlap-differs (and (not (= overlap-px red-only-px))
                              (not (= overlap-px 0x101018))))

(define dims (img-dims img))
(define is_buffer (img-buffer? img))

(disp-render img 0 0)
(save-img img "sdl_tests/png_out/test_img_circle_overlap_alpha.png")

(if (and c1 c2 c3 c4 c5 c6
         is_buffer (eq dims '(300 300))
         poles-ok
         overlap-differs)
    (print "SUCCESS")
    (print "FAILURE"))
