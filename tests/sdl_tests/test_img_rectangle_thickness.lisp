(sdl-init)

(define win (sdl-create-window "Display library - rectangle thickness test" 600 500))
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

(define img600x500 (img-buffer 'indexed4 600 500))

;; Test rectangles with different thickness values
(define r1 (img-rectangle img600x500 20 20 80 60 1 '(thickness 1)))    ; thin border
(define r2 (img-rectangle img600x500 120 20 80 60 2 '(thickness 2)))   ; normal border
(define r3 (img-rectangle img600x500 220 20 80 60 3 '(thickness 3)))   ; medium border
(define r4 (img-rectangle img600x500 320 20 80 60 1 '(thickness 5)))   ; thick border
(define r5 (img-rectangle img600x500 420 20 80 60 2 '(thickness 8)))   ; very thick border

;; Test rectangles with thickness larger than dimensions
(define r6 (img-rectangle img600x500 20 100 40 30 3 '(thickness 20)))  ; thickness > width/height
(define r7 (img-rectangle img600x500 80 100 30 40 1 '(thickness 15)))  ; thickness > width/height
(define r8 (img-rectangle img600x500 130 100 50 25 2 '(thickness 30))) ; thickness >> height

;; Test small rectangles with thickness
(define r9 (img-rectangle img600x500 200 100 20 20 3 '(thickness 2)))  ; small square, thin border
(define r10 (img-rectangle img600x500 240 100 15 15 1 '(thickness 3))) ; small square, thick border
(define r11 (img-rectangle img600x500 270 100 10 25 2 '(thickness 1))) ; thin tall rectangle
(define r12 (img-rectangle img600x500 290 100 25 10 3 '(thickness 1))) ; thin wide rectangle

;; Test rectangles with thickness and different aspect ratios
(define r13 (img-rectangle img600x500 20 180 120 40 1 '(thickness 4))) ; wide rectangle, thick border
(define r14 (img-rectangle img600x500 160 180 40 120 2 '(thickness 6))) ; tall rectangle, thick border
(define r15 (img-rectangle img600x500 220 180 80 80 3 '(thickness 10))) ; square, very thick border

;; Test thickness with zero values (edge cases)
(define r16 (img-rectangle img600x500 320 180 60 40 1 '(thickness 0))) ; zero thickness (normal rectangle)
(define r17 (img-rectangle img600x500 400 180 60 40 2 '(thickness 1))) ; minimum thickness

;; Test rectangles with thickness going off-screen
(define r18 (img-rectangle img600x500 550 180 70 60 3 '(thickness 5))) ; partially off-screen
(define r19 (img-rectangle img600x500 580 220 40 50 1 '(thickness 3))) ; mostly off-screen

;; Test rectangles with thickness and filled (should fill entire area)
(define r20 (img-rectangle img600x500 20 280 80 60 2 '(thickness 3) '(filled))) ; filled overrides thickness
(define r21 (img-rectangle img600x500 120 280 80 60 3 '(thickness 5) '(filled))) ; filled overrides thickness
(define r22 (img-rectangle img600x500 220 280 80 60 1 '(thickness 10) '(filled))) ; filled overrides thickness

;; Test rectangles with thickness and rounded corners
(define r23 (img-rectangle img600x500 320 280 80 60 2 '(thickness 2) '(rounded 10))) ; thick + rounded
(define r24 (img-rectangle img600x500 420 280 80 60 3 '(thickness 4) '(rounded 15))) ; thick + rounded
(define r25 (img-rectangle img600x500 520 280 60 60 1 '(thickness 6) '(rounded 20))) ; thick + rounded

;; Test very large thickness values
(define r26 (img-rectangle img600x500 20 380 100 80 2 '(thickness 50))) ; extremely thick
(define r27 (img-rectangle img600x500 140 380 80 80 3 '(thickness 100))) ; massive thickness
(define r28 (img-rectangle img600x500 240 380 60 60 1 '(thickness 200))) ; thickness >> dimensions

;; Test minimum size rectangles with thickness
(define r29 (img-rectangle img600x500 320 380 5 5 2 '(thickness 1)))   ; tiny rect, normal thickness
(define r30 (img-rectangle img600x500 340 380 3 3 3 '(thickness 2)))   ; tiny rect, thick thickness
(define r31 (img-rectangle img600x500 360 380 8 12 1 '(thickness 1)))  ; small rect, normal thickness

;; Test thickness with different colors
(define r32 (img-rectangle img600x500 380 380 40 40 1 '(thickness 3))) ; color 1
(define r33 (img-rectangle img600x500 430 380 40 40 2 '(thickness 3))) ; color 2
(define r34 (img-rectangle img600x500 480 380 40 40 3 '(thickness 3))) ; color 3

;; Test invalid thickness values
(define r35 (trap (img-rectangle img600x500 500 100 40 30 1 '(thickness -5))))  ; negative thickness
(define r36 (trap (img-rectangle img600x500 500 140 40 30 1 '(thickness))))     ; thickness without value

;; Test degenerate rectangles with thickness
(define r37 (img-rectangle img600x500 520 180 0 30 2 '(thickness 2)))  ; zero width
(define r38 (img-rectangle img600x500 540 180 30 0 3 '(thickness 2)))  ; zero height
(define r39 (img-rectangle img600x500 570 180 0 0 1 '(thickness 2)))   ; zero width and height

;; Test buffer properties
(define dims (img-dims img600x500))
(define is_buffer (img-buffer? img600x500))

;; Display the result with 4-color palette
(disp-render img600x500 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32 r33 r34 r37 r38 r39
         is_buffer (eq dims '(600 500)))
    (print "SUCCESS")
    (print "FAILURE"))