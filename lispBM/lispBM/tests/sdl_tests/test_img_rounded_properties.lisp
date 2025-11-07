(sdl-init)

(define win (sdl-create-window "Display library - rounded properties test" 500 400))
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

(define img500x400 (img-buffer 'indexed4 500 400))

;; Test rounded rectangles with different corner radius values
(define r1 (img-rectangle img500x400 20 20 80 60 1 '(rounded 5)))    ; small radius
(define r2 (img-rectangle img500x400 120 20 80 60 2 '(rounded 10)))   ; medium radius
(define r3 (img-rectangle img500x400 220 20 80 60 3 '(rounded 20)))   ; large radius
(define r4 (img-rectangle img500x400 320 20 80 60 1 '(rounded 30)))   ; very large radius

;; Test filled rounded rectangles
(define r5 (img-rectangle img500x400 20 100 80 60 2 '(rounded 8) '(filled)))   ; filled + rounded
(define r6 (img-rectangle img500x400 120 100 80 60 3 '(rounded 15) '(filled))) ; filled + larger radius
(define r7 (img-rectangle img500x400 220 100 80 60 1 '(rounded 25) '(filled))) ; filled + very large radius

;; Test rounded rectangles with different aspect ratios
(define r8 (img-rectangle img500x400 320 100 120 40 2 '(rounded 12)))  ; wide rectangle
(define r9 (img-rectangle img500x400 380 20 40 120 3 '(rounded 12)))   ; tall rectangle

;; Test edge cases for rounded rectangles
(define r10 (img-rectangle img500x400 20 180 60 60 1 '(rounded 0)))    ; radius 0 (normal rectangle)
(define r11 (img-rectangle img500x400 100 180 60 60 2 '(rounded 30)))  ; radius = half width/height
(define r12 (img-rectangle img500x400 180 180 60 60 3 '(rounded 50)))  ; radius > half dimensions

;; Test very small rounded rectangles
(define r13 (img-rectangle img500x400 260 180 20 20 1 '(rounded 5)))   ; small rect, small radius
(define r14 (img-rectangle img500x400 300 180 20 20 2 '(rounded 10)))  ; small rect, large radius
(define r15 (img-rectangle img500x400 340 180 15 15 3 '(rounded 7)))   ; very small rect

;; Test rounded arcs (as mentioned in documentation)
(define r16 (img-arc img500x400 80 280 40 0 90 1 '(rounded)))          ; rounded arc endpoints
(define r17 (img-arc img500x400 180 280 40 45 135 2 '(rounded)))       ; rounded arc different angle
(define r18 (img-arc img500x400 280 280 40 90 270 3 '(rounded)))       ; rounded large arc

;; Test rounded arcs with thickness
(define r19 (img-arc img500x400 80 340 35 0 180 1 '(thickness 8) '(rounded)))  ; thick rounded arc
(define r20 (img-arc img500x400 180 340 35 45 225 2 '(thickness 5) '(rounded))) ; medium thick rounded
(define r21 (img-arc img500x400 280 340 35 90 180 3 '(thickness 3) '(rounded))) ; thin rounded arc

;; Test rounded rectangles going off-screen
(define r22 (img-rectangle img500x400 460 120 60 80 1 '(rounded 15)))  ; partially off-screen
(define r23 (img-rectangle img500x400 480 340 40 80 2 '(rounded 10)))  ; mostly off-screen

;; Test combining multiple properties with rounded
(define r24 (img-rectangle img500x400 380 180 100 60 3 '(rounded 12) '(filled))) ; rounded + filled
(define r25 (img-arc img500x400 380 280 30 0 270 1 '(thickness 6) '(rounded)))  ; thick + rounded

;; Test invalid rounded values
(define r26 (trap (img-rectangle img500x400 400 20 60 40 1 '(rounded -5))))     ; negative radius
(define r27 (trap (img-rectangle img500x400 400 60 60 40 1 '(rounded))))        ; rounded without value

;; Test rounded with very large values
(define r28 (img-rectangle img500x400 20 260 80 50 2 '(rounded 100)))           ; huge radius
(define r29 (img-rectangle img500x400 120 260 80 50 3 '(rounded 1000)))         ; extremely large radius

;; Test minimum size rectangles with rounding
(define r30 (img-rectangle img500x400 220 260 5 5 1 '(rounded 2)))              ; tiny rect with rounding
(define r31 (img-rectangle img500x400 240 260 10 5 2 '(rounded 3)))             ; small rect, different proportions

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result with 4-color palette
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r28 r29 r30 r31
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))