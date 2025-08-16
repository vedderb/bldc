(sdl-init)

(define win (sdl-create-window "Display library - line properties test" 400 300))
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

(define img400x300 (img-buffer 'indexed4 400 300))

;; Test basic lines (no properties)
(define r1 (img-line img400x300 10 10 100 10 1))      ; horizontal line
(define r2 (img-line img400x300 10 20 10 60 1))       ; vertical line
(define r3 (img-line img400x300 120 10 170 60 1))     ; diagonal line

;; Test lines with thickness property
(define r4 (img-line img400x300 10 80 100 80 2 '(thickness 1)))   ; thickness 1
(define r5 (img-line img400x300 10 100 100 100 2 '(thickness 3))) ; thickness 3
(define r6 (img-line img400x300 10 130 100 130 2 '(thickness 5))) ; thickness 5
(define r7 (img-line img400x300 10 170 100 170 2 '(thickness 8))) ; thickness 8

;; Test lines with dotted property (dotted dash_length gap_length)
(define r8 (img-line img400x300 120 80 220 80 3 '(dotted 2 2)))    ; small dots
(define r9 (img-line img400x300 120 100 220 100 3 '(dotted 4 4)))  ; medium dots
(define r10 (img-line img400x300 120 120 220 120 3 '(dotted 8 3))) ; long dash, short gap
(define r11 (img-line img400x300 120 140 220 140 3 '(dotted 3 8))) ; short dash, long gap
(define r12 (img-line img400x300 120 160 220 160 3 '(dotted 6 6))) ; even dash/gap

;; Test combination of thickness and dotted
(define r13 (img-line img400x300 240 80 340 80 1 '(thickness 3) '(dotted 5 5)))
(define r14 (img-line img400x300 240 100 340 100 2 '(thickness 2) '(dotted 4 2)))

;; Test dotted lines at various angles
(define r15 (img-line img400x300 250 120 350 140 3 '(dotted 4 4)))  ; slight angle
(define r16 (img-line img400x300 250 150 350 180 3 '(dotted 6 3)))  ; steeper angle
(define r17 (img-line img400x300 280 190 280 250 3 '(dotted 5 5)))  ; vertical dotted

;; Test edge cases
(define r18 (img-line img400x300 10 200 100 200 1 '(thickness 0)))  ; thickness 0
(define r19 (img-line img400x300 10 220 100 220 2 '(dotted 0 5)))   ; dash length 0
(define r20 (img-line img400x300 10 240 100 240 2 '(dotted 5 0)))   ; gap length 0

;; Test very thick lines
(define r21 (img-line img400x300 120 200 220 200 1 '(thickness 10)))
(define r22 (img-line img400x300 120 230 220 230 2 '(thickness 15)))

;; Test lines with properties going off-screen
(define r23 (img-line img400x300 350 50 450 100 1 '(thickness 6)))
(define r24 (img-line img400x300 250 250 350 350 3 '(dotted 8 4)))

;; Test with invalid property values
(define r25 (trap (img-line img400x300 300 200 350 200 1 '(thickness -1))))
(define r26 (trap (img-line img400x300 300 220 350 220 1 '(dotted -1 5))))

;; Test buffer properties
(define dims (img-dims img400x300))
(define is_buffer (img-buffer? img400x300))

;; Display the result with 4-color palette
(disp-render img400x300 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 
         r18 r19 r20 r21 r22 r23 r24 is_buffer (eq dims '(400 300)))
    (print "SUCCESS")
    (print "FAILURE"))
