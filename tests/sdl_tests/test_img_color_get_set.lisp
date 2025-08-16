(sdl-init)

(define win (sdl-create-window "Display library - color get/set test" 400 300))
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

;; Test img-color-get with regular colors
(define color1 (img-color 'regular 0xFF0000))  ; red color
(define color2 (img-color 'regular 0x00FF00))  ; green color
(define color3 (img-color 'regular 0x0000FF))  ; blue color

;; Test getting color-0 property from regular colors
(define r1 (img-color-get color1 'color-0))  ; should return 0xFF0000
(define r2 (img-color-get color2 'color-0))  ; should return 0x00FF00
(define r3 (img-color-get color3 'color-0))  ; should return 0x0000FF

;; Test img-color-get with gradient colors
(define grad_x (img-color 'gradient_x 0xFF0000 0x00FF00 100 10 'repeat))
(define grad_y (img-color 'gradient_y 0x0000FF 0xFFFF00 80 5 'mirrored))

;; Test getting properties from gradient colors
(define r4 (img-color-get grad_x 'color-0))    ; should return 0xFF0000
(define r5 (img-color-get grad_x 'color-1))    ; should return 0x00FF00
(define r6 (img-color-get grad_x 'width))      ; should return 100
(define r7 (img-color-get grad_x 'offset))     ; should return 10

(define r8 (img-color-get grad_y 'color-0))    ; should return 0x0000FF
(define r9 (img-color-get grad_y 'color-1))    ; should return 0xFFFF00
(define r10 (img-color-get grad_y 'width))     ; should return 80
(define r11 (img-color-get grad_y 'offset))    ; should return 5

;; Test img-color-set with regular colors
(define color4 (img-color 'regular 0x808080))  ; gray color

;; Modify the color using img-color-set
(define r12 (img-color-set color4 'color-0 0xFF8000))  ; change to orange

;; Verify the change took effect
(define r13 (img-color-get color4 'color-0))  ; should now return 0xFF8000

;; Test img-color-set with gradient colors
(define grad_x2 (img-color 'gradient_x 0x800000 0x008000 50 0 'repeat))

;; Modify gradient properties
(define r14 (img-color-set grad_x2 'color-0 0xFF0080))   ; change first color
(define r15 (img-color-set grad_x2 'color-1 0x0080FF))   ; change second color
(define r16 (img-color-set grad_x2 'width 75))           ; change width
(define r17 (img-color-set grad_x2 'offset 15))          ; change offset

;; Verify the changes took effect
(define r18 (img-color-get grad_x2 'color-0))  ; should return 0xFF0080
(define r19 (img-color-get grad_x2 'color-1))  ; should return 0x0080FF
(define r20 (img-color-get grad_x2 'width))    ; should return 75
(define r21 (img-color-get grad_x2 'offset))   ; should return 15

;; Test img-color-set with repeat-type property
(define grad_y2 (img-color 'gradient_y 0x400040 0x004040 60 20 'repeat))

;; Change repeat type to mirrored
(define r22 (img-color-set grad_y2 'repeat-type 'mirrored))

;; Test error conditions for img-color-get
(define r23 (trap (img-color-get "invalid" 'color-0)))     ; invalid color object
(define r24 (trap (img-color-get color1 'invalid-prop)))   ; invalid property
(define r25 (trap (img-color-get color1 'color-1)))        ; color-1 on regular color (should fail)
(define r26 (trap (img-color-get color1 'width)))          ; width on regular color (should fail)

;; Test error conditions for img-color-set
(define r27 (trap (img-color-set "invalid" 'color-0 0xFF0000)))  ; invalid color object
(define r28 (trap (img-color-set color1 'invalid-prop 0xFF0000))) ; invalid property
(define r29 (trap (img-color-set color1 'color-1 0xFF0000)))      ; color-1 on regular color (should fail)
(define r30 (trap (img-color-set color1 'width 100)))             ; width on regular color (should fail)

;; Test type validation for img-color-set values
(define r31 (trap (img-color-set grad_x 'color-0 "invalid")))    ; invalid color value
(define r32 (trap (img-color-set grad_x 'width "invalid")))      ; invalid width value
(define r33 (trap (img-color-set grad_x 'repeat-type 'invalid))) ; invalid repeat-type

;; Test using modified colors in drawing operations
;;(img-rectangle img400x300 50 50 60 40 color4)    ; should use modified orange color
;(img-rectangle img400x300 150 50 60 40 grad_x2)  ; should use modified gradient
;(img-rectangle img400x300 250 50 60 40 grad_y2)  ; should use modified gradient

;; Test original colors haven't changed
(define r34 (img-color-get color1 'color-0))  ; should still be 0xFF0000
(define r35 (img-color-get grad_x 'color-0))  ; should still be 0xFF0000
(define r36 (img-color-get grad_x 'width))    ; should still be 100

;; Draw with original colors to verify they work
;(img-rectangle img400x300 50 150 60 40 color1)   ; original red
;(img-rectangle img400x300 150 150 60 40 grad_x)  ; original gradient
;(img-rectangle img400x300 250 150 60 40 grad_y)  ; original gradient

;; Test buffer properties
(define dims (img-dims img400x300))
(define is_buffer (img-buffer? img400x300))

;; Display the result
(disp-render img400x300 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(list )


(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r34 r35 r36
         is_buffer (eq dims '(400 300))
         ;; Verify color values
         (eq r1 0xFF0000u32) (eq r2 0x00FF00u32) (eq r3 0x0000FFu32)
         (eq r4 0xFF0000u32) (eq r5 0x00FF00u32) (eq r6 100i32) (eq r7 10i32)
         (eq r8 0x0000FFu32) (eq r9 0xFFFF00u32) (eq r10 80i32) (eq r11 5i32)
         (eq r13 0xFF8000u32)  ; modified color
         (eq r18 0xFF0080u32) (eq r19 0x0080FFu32) (eq r20 75i32) (eq r21 15i32)  ; modified gradient
         ;; Verify error conditions
         (eq (car r23) 'exit-error) (eq (car r24) 'exit-error) (eq (car r25) 'exit-error) (eq (car r26) 'exit-error)
         (eq (car r27) 'exit-error) (eq (car r28) 'exit-error) (eq (car r29) 'exit-error) (eq (car r30) 'exit-error)
         (eq (car r31) 'exit-error) (eq (car r32) 'exit-error) (eq (car r33) 'exit-error))
    (print "SUCCESS")
    (progn
      (print "FAILURE - Debug info:")
      (print (list "color values:" r1 r2 r3))
      (print (list "gradient props:" r4 r5 r6 r7))
      (print (list "modified color:" r13))
      (print (list "modified gradient:" r18 r19 r20 r21))
      (print "FAILURE")))
