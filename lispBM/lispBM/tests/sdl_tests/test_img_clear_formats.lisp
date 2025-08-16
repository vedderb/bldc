(sdl-init)

(define win (sdl-create-window "Display library - img-clear formats test" 400 300))
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

;; Test img-clear on indexed16 format (4 bits per pixel, 16 colors)
(define img_indexed16 (img-buffer 'indexed16 200 100))

;; Draw some content first
(define r1 (img-rectangle img_indexed16 10 10 50 30 5))
(define r2 (img-circle img_indexed16 100 50 20 10))

;; Test clearing with different indexed16 colors
(define r3 (img-clear img_indexed16 0))    ; clear to color 0 (black)
(define r4 (img-clear img_indexed16 7))    ; clear to color 7 
(define r5 (img-clear img_indexed16 15))   ; clear to color 15 (max for indexed16)

;; Draw something and clear again
(define r6 (img-triangle img_indexed16 50 20 80 60 20 60 12))
(define r7 (img-clear img_indexed16 3))    ; clear to color 3

;; Test img-clear on rgb332 format (8-bit color: 3R-3G-2B bits)
(define img_rgb332 (img-buffer 'rgb332 200 100))

;; rgb332 encoding: RRRGGGBB
(define red332 0xE0)       ; 111 000 00 = pure red
(define green332 0x1C)     ; 000 111 00 = pure green  
(define blue332 0x03)      ; 000 000 11 = pure blue
(define yellow332 0xFC)    ; 111 111 00 = red + green

;; Draw some content first
(define r8 (img-rectangle img_rgb332 20 20 40 25 red332))
(define r9 (img-circle img_rgb332 120 60 15 green332))

;; Test clearing with different rgb332 colors
(define r10 (img-clear img_rgb332 0x00))      ; clear to black
(define r11 (img-clear img_rgb332 red332))    ; clear to red
(define r12 (img-clear img_rgb332 blue332))   ; clear to blue
(define r13 (img-clear img_rgb332 yellow332)) ; clear to yellow

;; Draw something and clear again
(define r14 (img-line img_rgb332 10 10 150 80 0x92))  ; gray line
(define r15 (img-clear img_rgb332 green332))  ; clear to green

;; Test img-clear on rgb565 format (16-bit color: 5R-6G-5B bits)
(define img_rgb565 (img-buffer 'rgb565 200 100))

;; rgb565 colors (16-bit values)
(define red565 0xF800)     ; 11111 000000 00000 = pure red
(define green565 0x07E0)   ; 00000 111111 00000 = pure green
(define blue565 0x001F)    ; 00000 000000 11111 = pure blue
(define white565 0xFFFF)   ; 11111 111111 11111 = white
(define gray565 0x8410)    ; medium gray

;; Draw some content first
(define r16 (img-rectangle img_rgb565 30 15 60 40 red565))
(define r17 (img-arc img_rgb565 130 50 25 0 180 blue565))

;; Test clearing with different rgb565 colors
(define r18 (img-clear img_rgb565 0x0000))    ; clear to black
(define r19 (img-clear img_rgb565 red565))    ; clear to red
(define r20 (img-clear img_rgb565 green565))  ; clear to green
(define r21 (img-clear img_rgb565 white565))  ; clear to white

;; Draw something and clear again
(define r22 (img-triangle img_rgb565 80 20 120 70 40 70 gray565))
(define r23 (img-clear img_rgb565 blue565))   ; clear to blue

;; Test img-clear on rgb888 format (24-bit color, stored as 32-bit)
(define img_rgb888 (img-buffer 'rgb888 200 100))

;; rgb888 colors (24-bit values)
(define red888 0xFF0000)     ; pure red
(define green888 0x00FF00)   ; pure green
(define blue888 0x0000FF)    ; pure blue
(define white888 0xFFFFFF)   ; white
(define black888 0x000000)   ; black
(define gray888 0x808080)    ; gray
(define orange888 0xFF8000)  ; orange

;; Draw some content first
(define r24 (img-rectangle img_rgb888 25 25 50 35 red888))
(define r25 (img-circle img_rgb888 140 60 30 green888))

;; Test clearing with different rgb888 colors
(define r26 (img-clear img_rgb888 black888))  ; clear to black
(define r27 (img-clear img_rgb888 red888))    ; clear to red
(define r28 (img-clear img_rgb888 green888))  ; clear to green
(define r29 (img-clear img_rgb888 blue888))   ; clear to blue
(define r30 (img-clear img_rgb888 white888))  ; clear to white

;; Draw something and clear again
(define r31 (img-line img_rgb888 0 0 199 99 gray888))
(define r32 (img-clear img_rgb888 orange888)) ; clear to orange

;; Test edge cases - clearing empty buffers
(define img_empty (img-buffer 'indexed16 50 50))
(define r33 (img-clear img_empty 8))  ; clear empty buffer

;; Test clearing very small buffers
(define img_tiny (img-buffer 'rgb332 10 10))
(define r34 (img-clear img_tiny 0x1F))  ; clear tiny buffer

;; Test clearing with invalid colors (should handle gracefully)
(define r35 (trap (img-clear img_indexed16 16)))   ; color > max for indexed16 (15)
(define r36 (trap (img-clear img_indexed16 -1)))   ; negative color
(define r37 (trap (img-clear "invalid" 5)))        ; invalid buffer

;; Test creating buffers with zero dimensions (should be errors)
(define r38 (trap (img-buffer 'rgb565 0 50)))   ; zero width should error
(define r39 (trap (img-buffer 'rgb888 50 0)))   ; zero height should error
(define r40 (trap (img-buffer 'indexed16 0 0))) ; zero width and height should error

;; Test buffer properties after clearing
(define dims16 (img-dims img_indexed16))
(define dims332 (img-dims img_rgb332))
(define dims565 (img-dims img_rgb565))
(define dims888 (img-dims img_rgb888))

;; Display one of the cleared buffers
(disp-render img_indexed16 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 0xFFFF00 0xFF00FF 0x00FFFF 0x808080 
                                 0x400000 0x004000 0x000040 0x404000 0x400040 0x004040 0x404040 0xC0C0C0))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32 r33 r34
         (eq dims16 '(200 100)) (eq dims332 '(200 100)) (eq dims565 '(200 100)) (eq dims888 '(200 100))
         ;; Verify error conditions return errors
         (eq (car r38) 'exit-error) (eq (car r39) 'exit-error) (eq (car r40) 'exit-error))
    (print "SUCCESS")
    (print "FAILURE"))