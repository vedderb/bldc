(sdl-init)

(define win (sdl-create-window "Display library - retro font fallback test" 500 400))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

(sdl-set-active-renderer rend)

(define img (img-buffer 'rgb888 500 400))

;; Basic fallback text (mag 1, char height = 5px, spaced 10px apart)
(define r1 (img-text img 10 5  0xFFFFFF 0x000000 "Hello"))
(define r2 (img-text img 10 15 0xFFFFFF 0x000000 ""))
(define r3 (img-text img 10 25 0xFFFFFF 0x000000 "Test 123!"))
(define r4 (img-text img 10 35 0xFFFFFF 0x000000 (str-from-n 3.14159 "%.5f")))
(define r5 (img-text img 10 45 0xFFFFFF 0x000000 (str-from-n 42)))

;; Spacing (mag 1, height 5px)
(define r8 (img-text img 10 58 0xFFFFFF 0x000000 "Spaced" '(spacing 4)))

;; Magnification
;; mag=4: height = 20px  (y=70..90)
(define r6 (img-text img 10 70 0xFFFFFF 0x000000 "Mag4" '(magnify 4)))
;; mag=8: height = 40px  (y=100..140)
(define r7 (img-text img 10 100 0xFFFFFF 0x000000 "Mag8" '(magnify 8)))

;; Narrow glyphs (mag=4, height=20px, right half of canvas x=200, spaced 25px)
;; , and .
(define r19 (img-text img 200 70  0xFFFFFF 0x000000 "A,B.C" '(magnify 4)))
;; : and ;
(define r20 (img-text img 200 95  0xFFFFFF 0x000000 "1:2;3" '(magnify 4)))
;; ' and !
(define r21 (img-text img 200 120 0xFFFFFF 0x000000 "It's!" '(magnify 4)))
;; -
(define r22 (img-text img 200 145 0xFFFFFF 0x000000 "A-B"   '(magnify 4)))

;; Alignment (mag 1, height 5px, spaced 10px, x=250 is the anchor)
(define r9  (img-text img 250 150 0xFFFFFF 0x000000 "Left"   '(align left)))
(define r10 (img-text img 250 162 0xFFFFFF 0x000000 "Center" '(align center)))
(define r11 (img-text img 250 174 0xFFFFFF 0x000000 "Right"  '(align right)))

;; Rotation (mag=4, height=20px)
;; Rot0 and Rot180 are horizontal: place them side by side in top half
;; Rot0   (y=190..210) left column
(define r12 (img-text img 50  190 0xFFFFFF 0x000000 "Rot0"   '(magnify 4) '(rotate 0)))
;; Rot180 (y=190..210) right column
(define r14 (img-text img 270 190 0xFFFFFF 0x000000 "Rot180" '(magnify 4) '(rotate 180)))

;; Rot90 and Rot270 are vertical: text width (~80px) becomes vertical extent
;; center_y = y + 10; place with enough room above and below
;; Rot90  (center at y=310, extends ~y=270..350) left column
(define r13 (img-text img 50  300 0xFFFFFF 0x000000 "Rot90"  '(magnify 4) '(rotate 90)))
;; Rot270 (center at y=310, extends ~y=265..355) right column
(define r15 (img-text img 270 300 0xFFFFFF 0x000000 "Rot270" '(magnify 4) '(rotate 270)))

;; Combined: short string, mag=5 (height=25px), placed in open area top-right
(define r16 (img-text img 400 190 0xFFFFFF 0x000000 "Hi!"
                      '(magnify 5) '(spacing 2) '(align left) '(rotate 0)))

;; Edge: text partially off screen, bottom strip below rotation section
(define r17 (img-text img -5  375 0xFFFFFF 0x000000 "OffLeft"))
(define r18 (img-text img 480 375 0xFFFFFF 0x000000 "OffRight"))

(disp-render img 0 0 '())

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18
         r19 r20 r21 r22)
    (print "SUCCESS")
    (print "FAILURE"))
