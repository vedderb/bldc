(sdl-init)

(define win (sdl-create-window "Display library - rgb332 test" 400 200))
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

;; Test rgb332 format (8-bit color: 3 bits red, 3 bits green, 2 bits blue)
(define img400x200 (img-buffer 'rgb332 400 200))

;; Test basic drawing operations with rgb332 colors
;; rgb332 encoding: RRRGGGBB
(define red332 0xE0)       ; 111 000 00 = pure red
(define green332 0x1C)     ; 000 111 00 = pure green  
(define blue332 0x03)      ; 000 000 11 = pure blue
(define yellow332 0xFC)    ; 111 111 00 = red + green
(define magenta332 0xE3)   ; 111 000 11 = red + blue
(define cyan332 0x1F)      ; 000 111 11 = green + blue
(define white332 0xFF)     ; 111 111 11 = white
(define gray332 0x92)      ; 100 100 10 = gray

(define r1 (img-rectangle img400x200 10 10 50 30 red332))
(define r2 (img-circle img400x200 100 50 20 green332))
(define r3 (img-triangle img400x200 150 20 180 60 120 60 blue332))
(define r4 (img-line img400x200 200 10 350 40 yellow332))
(define r5 (img-arc img400x200 300 100 30 0 180 magenta332))

;; Test with various rgb332 color combinations
(define r6 (img-rectangle img400x200 50 100 40 30 cyan332))
(define r7 (img-circle img400x200 150 130 15 white332))
(define r8 (img-triangle img400x200 200 100 230 140 170 140 gray332))

;; Test with different intensity levels
(define dark_red 0x80)     ; 100 000 00 = dark red
(define med_green 0x10)    ; 000 100 00 = medium green
(define light_blue 0x02)   ; 000 000 10 = light blue

(define r9 (img-rectangle img400x200 300 120 50 30 dark_red))
(define r10 (img-line img400x200 10 170 100 170 med_green))
(define r11 (img-circle img400x200 350 170 15 light_blue))

;; Test buffer properties
(define dims (img-dims img400x200))
(define is_buffer (img-buffer? img400x200))

;; Display the result - no color mapping needed for rgb332
(disp-render img400x200 0 0)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 is_buffer
         (eq dims '(400 200)))
    (print "SUCCESS")
    (print "FAILURE"))
