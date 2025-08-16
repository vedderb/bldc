(sdl-init)

(define win (sdl-create-window "Display library - circle segments test" 500 400))
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

;; Test basic circle segments (outline only)
(define r1 (img-circle-segment img500x400 60 60 40 0 90 1))     ; quarter segment
(define r2 (img-circle-segment img500x400 160 60 40 45 135 1))  ; half segment  
(define r3 (img-circle-segment img500x400 260 60 40 90 270 1))  ; three-quarter segment
(define r4 (img-circle-segment img500x400 360 60 40 0 360 1))   ; full circle segment

;; Test filled circle segments
(define r5 (img-circle-segment img500x400 60 160 40 0 90 2 '(filled)))     ; filled quarter
(define r6 (img-circle-segment img500x400 160 160 40 45 135 2 '(filled)))  ; filled half
(define r7 (img-circle-segment img500x400 260 160 40 90 270 2 '(filled)))  ; filled three-quarter
(define r8 (img-circle-segment img500x400 360 160 40 0 180 2 '(filled)))   ; filled semicircle

;; Test various angle ranges
(define r9 (img-circle-segment img500x400 60 260 30 30 60 3))    ; small 30-degree segment
(define r10 (img-circle-segment img500x400 130 260 30 0 30 3))   ; 30-degree from 0
(define r11 (img-circle-segment img500x400 200 260 30 315 45 3)) ; segment crossing 0 degrees
(define r12 (img-circle-segment img500x400 270 260 30 270 360 3)) ; 90-degree segment

;; Test different radii
(define r13 (img-circle-segment img500x400 60 340 10 0 180 1))   ; small radius
(define r14 (img-circle-segment img500x400 120 340 20 0 180 2))  ; medium radius
(define r15 (img-circle-segment img500x400 200 340 35 0 180 3))  ; large radius

;; Test edge cases
(define r16 (img-circle-segment img500x400 350 340 25 0 0 1))    ; zero-degree segment
(define r17 (img-circle-segment img500x400 400 340 25 90 90 2))  ; same start/end angle
(define r18 (img-circle-segment img500x400 450 340 25 180 180 3)) ; same start/end at 180

;; Test segments that go off-screen
(define r19 (img-circle-segment img500x400 480 380 30 0 90 1))   ; partially off-screen
(define r20 (img-circle-segment img500x400 520 420 25 0 180 2))  ; mostly off-screen

;; Test filled segments with various angles
(define r21 (img-circle-segment img500x400 400 260 25 45 90 1 '(filled)))    ; small filled segment
(define r22 (img-circle-segment img500x400 450 260 25 200 300 2 '(filled)))  ; different angle range

;; Test invalid parameters
(define r23 (trap (img-circle-segment img500x400 100 100 -5 0 90 1)))         ; negative radius
(define r24 (trap (img-circle-segment img500x400 100 100 25 450 500 1)))      ; angles > 360
(define r25 (trap (img-circle-segment "invalid" 100 100 25 0 90 1)))          ; invalid buffer

;; Test with reverse angle order (end < start)
(define r26 (img-circle-segment img500x400 350 180 30 270 45 3))              ; wraps around 0

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result with 4-color palette
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r26
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))