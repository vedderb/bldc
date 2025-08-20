(sdl-init)

(define win (sdl-create-window "Display library - blit properties test" 500 400))
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

;; Create source images with recognizable patterns
(define src_small (img-buffer 'indexed4 30 30))
(define src_rect (img-buffer 'indexed4 40 20))
(define dst_img (img-buffer 'indexed4 500 400))

;; Draw patterns in source images
(img-rectangle src_small 5 5 20 20 1)
(img-circle src_small 15 15 8 2)
(img-triangle src_small 10 20 20 25 15 25 3)

(img-rectangle src_rect 5 2 30 16 1)
(img-line src_rect 0 10 40 10 2)
(img-circle src_rect 20 10 6 3)

;; Test basic blit (no properties)
(define r1 (img-blit dst_img src_small 10 10 -1))

;; Test rotate property - rotate around center of source
(define r2 (img-blit dst_img src_small 80 50 -1 '(rotate 15 15 45)))   ; 45 degrees
(define r3 (img-blit dst_img src_small 150 50 -1 '(rotate 15 15 90)))  ; 90 degrees
(define r4 (img-blit dst_img src_small 220 50 -1 '(rotate 15 15 180))) ; 180 degrees
(define r5 (img-blit dst_img src_small 290 50 -1 '(rotate 15 15 270))) ; 270 degrees

;; Test rotate property with different rotation centers
(define r6 (img-blit dst_img src_small 360 50 -1 '(rotate 0 0 45)))    ; rotate around top-left
(define r7 (img-blit dst_img src_small 430 50 -1 '(rotate 30 30 45)))  ; rotate around bottom-right

;; Test scale property
(define r8 (img-blit dst_img src_small 10 120 -1 '(scale 0.5)))        ; scale down
(define r9 (img-blit dst_img src_small 80 120 -1 '(scale 1.0)))        ; normal scale
(define r10 (img-blit dst_img src_small 150 120 -1 '(scale 1.5)))      ; scale up
(define r11 (img-blit dst_img src_small 250 120 -1 '(scale 2.0)))      ; scale up more

;; Test very small and large scales
(define r12 (img-blit dst_img src_small 350 120 -1 '(scale 0.25)))     ; very small
(define r13 (img-blit dst_img src_small 400 120 -1 '(scale 3.0)))      ; very large

;; Test tile property - should repeat the source to fill destination
(define r14 (img-blit dst_img src_small 10 200 -1 '(tile)))

;; Test clip property - clip output in destination coordinates
(define r15 (img-blit dst_img src_rect 200 200 -1 '(clip 210 205 25 15))) ; clip to small area
(define r16 (img-blit dst_img src_rect 280 200 -1 '(clip 285 200 30 25))) ; clip partially

;; Test combining properties
(define r17 (img-blit dst_img src_small 10 280 -1 '(rotate 15 15 45) '(scale 1.5)))    ; rotate + scale
(define r18 (img-blit dst_img src_small 120 280 -1 '(scale 0.8) '(clip 125 285 20 20))) ; scale + clip

;; Test transparency with properties
(define r19 (img-blit dst_img src_small 200 280 0 '(rotate 15 15 30)))  ; rotate with transparency
(define r20 (img-blit dst_img src_small 280 280 2 '(scale 1.2)))        ; scale with transparency

;; Test properties with partial off-screen blits
(define r21 (img-blit dst_img src_small 480 50 -1 '(scale 1.5)))        ; scaled, partially off-screen
(define r22 (img-blit dst_img src_small 470 370 -1 '(rotate 15 15 45))) ; rotated, partially off-screen

;; Test edge cases and invalid properties
(define r23 (trap (img-blit dst_img src_small 350 280 -1 '(invalid-property))))
(define r24 (trap (img-blit dst_img src_small 380 280 -1 '(scale))))      ; scale without value
(define r25 (trap (img-blit dst_img src_small 410 280 -1 '(rotate 15))))  ; rotate with insufficient args

;; Test with scale 0 or negative values
;; NOTE: bug found scale 0 leads to fp-exception and crash.
(define r26 (trap (img-blit dst_img src_small 50 350 -1 '(scale 0))))     ; scale 0
(define r27 (trap (img-blit dst_img src_small 100 350 -1 '(scale -1))))   ; negative scale

;; Test clipping with invalid coordinates
(define r28 (trap (img-blit dst_img src_small 150 350 -1 '(clip -10 -10 20 20)))) ; negative clip coords
(define r29 (trap (img-blit dst_img src_small 200 350 -1 '(clip 0 0 -5 -5))))    ; negative clip size

;; Test complex multi-property combinations
(define r30 (img-blit dst_img src_small 300 320 -1 '(rotate 15 15 30) '(scale 0.7) '(clip 305 325 25 25)))

;; Test buffer properties
(define dims (img-dims dst_img))
(define is_buffer (img-buffer? dst_img))

;; Display the result with 4-color palette
(disp-render dst_img 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r30
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))
