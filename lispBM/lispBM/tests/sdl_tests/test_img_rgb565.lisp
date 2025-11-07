(sdl-init)

(define win (sdl-create-window "Display library - rgb565 test" 400 200))
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

;; Test rgb565 format (16-bit color: 5 bits red, 6 bits green, 5 bits blue)
(define img400x200 (img-buffer 'rgb565 400 200))

;; Test basic drawing operations with rgb565 colors
;; rgb565 encoding: RRRRRGGGGGGBBBBB
(define red565 0xF800)     ; 11111 000000 00000 = pure red
(define green565 0x07E0)   ; 00000 111111 00000 = pure green  
(define blue565 0x001F)    ; 00000 000000 11111 = pure blue
(define yellow565 0xFFE0)  ; 11111 111111 00000 = red + green
(define magenta565 0xF81F) ; 11111 000000 11111 = red + blue
(define cyan565 0x07FF)    ; 00000 111111 11111 = green + blue
(define white565 0xFFFF)   ; 11111 111111 11111 = white
(define gray565 0x8410)    ; 10000 100000 10000 = gray

(define r1 (img-rectangle img400x200 10 10 50 30 red565))
(define r2 (img-circle img400x200 100 50 20 green565))
(define r3 (img-triangle img400x200 150 20 180 60 120 60 blue565))
(define r4 (img-line img400x200 200 10 350 40 yellow565))
(define r5 (img-arc img400x200 300 100 30 0 180 magenta565))

;; Test with various rgb565 color combinations
(define r6 (img-rectangle img400x200 50 100 40 30 cyan565))
(define r7 (img-circle img400x200 150 130 15 white565))
(define r8 (img-triangle img400x200 200 100 230 140 170 140 gray565))

;; Test with different intensity levels and precise color control
(define dark_red 0x8000)   ; 10000 000000 00000 = dark red
(define med_green 0x0400)  ; 00000 100000 00000 = medium green
(define light_blue 0x0010) ; 00000 000000 10000 = light blue
(define orange565 0xFC00)  ; 11111 100000 00000 = orange
(define purple565 0x8010)  ; 10000 000000 10000 = purple

(define r9 (img-rectangle img400x200 300 120 30 20 dark_red))
(define r10 (img-line img400x200 10 170 100 170 med_green))
(define r11 (img-circle img400x200 350 170 15 light_blue))
(define r12 (img-arc img400x200 50 150 25 90 270 orange565))
(define r13 (img-triangle img400x200 120 150 140 180 100 180 purple565))

;; Test buffer properties
(define dims (img-dims img400x200))
(define is_buffer (img-buffer? img400x200))

;; Display the result - no color mapping needed for rgb565
(disp-render img400x200 0 0)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 is_buffer
         (eq dims '(400 200)))
    (print "SUCCESS")
    (print "FAILURE"))