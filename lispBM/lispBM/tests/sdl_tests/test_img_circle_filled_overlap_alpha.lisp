(sdl-init)

(define win (sdl-create-window "Display library - overlapping filled alpha circles" 300 300))
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
;; circle is drawn *opaque* into its own scratch buffer, sized to its
;; bounding box, then composited onto img with img-blit's 'alpha attribute.
;; marker is the scratch buffer's background fill -- passed as img-blit's
;; transparent_color so only the circle's own pixels participate.
(define marker 0x00FF01)

(defun circle-layer (dest cx cy r color alpha)
  (let ((margin 2))
    (let ((size (+ (* 2 r) (* 2 margin)))
          (lx (+ r margin))
          (ly (+ r margin)))
      (let ((buf (img-buffer 'rgb888 size size)))
        (progn
          (img-clear buf marker)
          (img-circle buf lx ly r color '(filled))
          (img-blit dest buf (- cx lx) (- cy ly) marker (list 'alpha alpha)))))))

;; Four overlapping filled alpha circles, laid out so the center point sits
;; inside all four -- exercises fill_circle's shared Bresenham machinery
;; together with genuine layered alpha compositing via blit (1-circle,
;; 2-circle, and 4-circle overlap regions).
(define c1 (circle-layer img 120 120 80 0xE04030 140))
(define c2 (circle-layer img 180 120 80 0x30C060 140))
(define c3 (circle-layer img 120 180 80 0x3080E0 140))
(define c4 (circle-layer img 180 180 80 0xE0C030 140))

;; sample points chosen by distance to each circle's center:
;;   (70,70)    is within r=80 of circle 1 only            -> single blend
;;   (150,90)   is within r=80 of circles 1 and 2 only      -> 2-way overlap
;;   (150,150)  is within r=80 of all four circles          -> 4-way overlap
;;   (10,10)    is outside all four circles                 -> background
(define bg-px       (img-getpix img 10 10))
(define single-px    (img-getpix img 70 70))
(define overlap2-px  (img-getpix img 150 90))
(define overlap4-px  (img-getpix img 150 150))

;; each additional layer of alpha compositing should produce a genuinely
;; different color -- not a no-op overwrite, and not saturating early.
(define layering-ok (and (not (= single-px bg-px))
                          (not (= overlap2-px bg-px))
                          (not (= overlap2-px single-px))
                          (not (= overlap4-px bg-px))
                          (not (= overlap4-px overlap2-px))))

(define dims (img-dims img))
(define is_buffer (img-buffer? img))

(disp-render img 0 0)
(save-img img "sdl_tests/png_out/test_img_circle_filled_overlap_alpha.png")

(if (and c1 c2 c3 c4
         is_buffer (eq dims '(300 300))
         layering-ok)
    (print "SUCCESS")
    (print "FAILURE"))
