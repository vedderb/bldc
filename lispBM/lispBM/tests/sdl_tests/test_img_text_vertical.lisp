(sdl-init)

(define win (sdl-create-window "Display library - vertical text test" 500 400))
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

;; Load a font
(define font-file (fopen "./sdl_tests/font_16_26.bin" "r"))
(define font (load-file font-file))

(define img500x400 (img-buffer 'indexed4 500 400))

;; Test normal horizontal text for comparison
(define r1 (img-text img500x400 50 50 1 0 font "Horizontal"))

;; Test vertical text with 'up direction (prints upwards)
(define r2 (img-text img500x400 150 350 2 0 font "Upward Text" 'up))
(define r3 (img-text img500x400 200 350 3 0 font "UP" 'up))
(define r4 (img-text img500x400 250 350 1 0 font "Vertical Up" 'up))

;; Test vertical text with 'down direction (prints downwards) 
(define r5 (img-text img500x400 350 50 2 0 font "Downward Text" 'down))
(define r6 (img-text img500x400 400 50 3 0 font "DOWN" 'down))
(define r7 (img-text img500x400 450 50 1 0 font "Vertical Down" 'down))

;; Test shorter text strings with vertical directions
(define r8 (img-text img500x400 100 300 1 0 font "A" 'up))       ; single character up
(define r9 (img-text img500x400 120 300 2 0 font "Hi" 'up))      ; short text up
(define r10 (img-text img500x400 300 100 3 0 font "X" 'down))    ; single character down
(define r11 (img-text img500x400 320 100 1 0 font "OK" 'down))   ; short text down

;; Test empty string with vertical directions
(define r12 (img-text img500x400 140 200 2 0 font "" 'up))       ; empty string up
(define r13 (img-text img500x400 160 200 3 0 font "" 'down))     ; empty string down

;; Test vertical text at edge positions
(define r14 (img-text img500x400 50 390 1 0 font "Bottom Up" 'up))     ; near bottom, going up
(define r15 (img-text img500x400 470 10 2 0 font "Top Down" 'down))    ; near top, going down
(define r16 (img-text img500x400 10 200 3 0 font "Left Up" 'up))       ; left edge, going up
(define r17 (img-text img500x400 490 200 1 0 font "Right Down" 'down)) ; right edge, going down

;; Test vertical text that may go off-screen
(define r18 (img-text img500x400 180 20 2 0 font "May Go Off Top" 'up))     ; might go off top
(define r19 (img-text img500x400 380 380 3 0 font "May Go Off Bottom" 'down)) ; might go off bottom

;; Test longer text strings with vertical directions
(define r20 (img-text img500x400 220 320 1 0 font "Long Upward String" 'up))
(define r21 (img-text img500x400 420 80 2 0 font "Long Downward String" 'down))

;; Test numbers and special characters with vertical text
(define r22 (img-text img500x400 160 280 3 0 font "123" 'up))
(define r23 (img-text img500x400 360 120 1 0 font "!@#" 'down))

;; Test invalid direction arguments (should handle gracefully)
(define r24 (trap (img-text img500x400 50 100 2 0 font "Invalid Dir" 'left)))   ; invalid direction
(define r25 (trap (img-text img500x400 50 120 2 0 font "Invalid Dir" 'right)))  ; invalid direction
(define r26 (trap (img-text img500x400 50 140 2 0 font "Invalid Dir" 123)))     ; invalid direction type

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result with 4-color palette
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))