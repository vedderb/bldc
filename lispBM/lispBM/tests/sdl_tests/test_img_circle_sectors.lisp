(sdl-init)

(define win (sdl-create-window "Display library - circle sectors test" 500 400))
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

;; Test basic circle sectors (outline only)
(define r1 (img-circle-sector img500x400 60 60 40 0 90 1))     ; quarter sector (pie slice)
(define r2 (img-circle-sector img500x400 160 60 40 45 135 1))  ; half sector
(define r3 (img-circle-sector img500x400 260 60 40 90 270 1))  ; three-quarter sector
(define r4 (img-circle-sector img500x400 360 60 40 0 360 1))   ; full circle sector

;; Test filled circle sectors
(define r5 (img-circle-sector img500x400 60 160 40 0 90 2 '(filled)))     ; filled quarter pie
(define r6 (img-circle-sector img500x400 160 160 40 45 135 2 '(filled)))  ; filled half pie
(define r7 (img-circle-sector img500x400 260 160 40 90 270 2 '(filled)))  ; filled three-quarter pie
(define r8 (img-circle-sector img500x400 360 160 40 0 180 2 '(filled)))   ; filled semicircle pie

;; Test sectors with thickness property
(define r9 (img-circle-sector img500x400 60 260 35 30 120 3 '(thickness 3)))   ; thick outline
(define r10 (img-circle-sector img500x400 160 260 35 45 135 3 '(thickness 5))) ; thicker outline
(define r11 (img-circle-sector img500x400 260 260 35 90 180 3 '(thickness 2))) ; thin outline

;; Test various angle ranges for pie slices
(define r12 (img-circle-sector img500x400 360 260 30 0 30 1))    ; small 30-degree pie slice
(define r13 (img-circle-sector img500x400 430 260 30 315 45 1))  ; slice crossing 0 degrees
(define r14 (img-circle-sector img500x400 60 340 25 270 360 2))  ; 90-degree slice

;; Test different radii
(define r15 (img-circle-sector img500x400 150 340 15 0 90 1))    ; small radius pie
(define r16 (img-circle-sector img500x400 220 340 25 0 120 2))   ; medium radius pie
(define r17 (img-circle-sector img500x400 320 340 35 0 60 3))    ; large radius pie

;; Test edge cases
(define r18 (img-circle-sector img500x400 420 340 20 0 0 1))     ; zero-degree sector
(define r19 (img-circle-sector img500x400 460 340 20 90 90 2))   ; same start/end angle
(define r20 (img-circle-sector img500x400 400 380 25 180 180 3)) ; same start/end at 180

;; Test sectors that go off-screen
(define r21 (img-circle-sector img500x400 480 380 30 0 90 1))    ; partially off-screen
(define r22 (img-circle-sector img500x400 520 420 25 0 180 2))   ; mostly off-screen

;; Test combining filled and thickness properties
(define r23 (img-circle-sector img500x400 160 340 30 45 135 1 '(filled) '(thickness 4)))

;; Test invalid parameters
(define r24 (trap (img-circle-sector img500x400 100 100 -5 0 90 1)))         ; negative radius
(define r25 (trap (img-circle-sector img500x400 100 100 25 450 500 1)))      ; angles > 360
(define r26 (trap (img-circle-sector "invalid" 100 100 25 0 90 1)))          ; invalid buffer

;; Test with reverse angle order (end < start) - should wrap around
(define r27 (img-circle-sector img500x400 360 340 25 270 45 3))              ; wraps around 0

;; Test very small and very large sectors
(define r28 (img-circle-sector img500x400 450 160 20 0 1 1))     ; 1-degree sector (very thin)
(define r29 (img-circle-sector img500x400 450 200 20 0 359 2))   ; almost full circle sector

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result with 4-color palette
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r27 r28 r29
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))