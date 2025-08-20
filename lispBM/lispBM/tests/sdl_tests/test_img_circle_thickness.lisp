(sdl-init)

(define win (sdl-create-window "Display library - circle thickness test" 500 400))
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

;; Test circles with different thickness values
(define r1 (img-circle img500x400 60 60 40 1 '(thickness 1)))    ; thin circle
(define r2 (img-circle img500x400 160 60 40 2 '(thickness 3)))   ; medium thickness
(define r3 (img-circle img500x400 260 60 40 3 '(thickness 5)))   ; thick circle
(define r4 (img-circle img500x400 360 60 40 1 '(thickness 8)))   ; very thick circle

;; Test circles with various radii and thickness combinations
(define r5 (img-circle img500x400 60 160 20 2 '(thickness 2)))   ; small circle, thin
(define r6 (img-circle img500x400 160 160 30 3 '(thickness 4)))  ; medium circle, medium thickness
(define r7 (img-circle img500x400 260 160 50 1 '(thickness 6)))  ; large circle, thick
(define r8 (img-circle img500x400 360 160 25 2 '(thickness 10))) ; thick border, smaller radius

;; Test circles with thickness larger than radius
(define r9 (img-circle img500x400 60 260 15 3 '(thickness 20)))  ; thickness > radius
(define r10 (img-circle img500x400 160 260 10 1 '(thickness 15))) ; thickness >> radius
(define r11 (img-circle img500x400 260 260 8 2 '(thickness 10)))  ; very thick vs small radius

;; Test edge cases for thickness
(define r12 (img-circle img500x400 360 260 30 3 '(thickness 0)))  ; zero thickness (should be normal circle)
(define r13 (img-circle img500x400 60 340 25 1 '(thickness 1)))   ; minimum thickness
(define r14 (img-circle img500x400 160 340 35 2 '(thickness 2)))  ; small thickness

;; Test circles with thickness going off-screen
(define r15 (img-circle img500x400 480 340 30 3 '(thickness 5)))  ; partially off-screen
(define r16 (img-circle img500x400 520 380 25 1 '(thickness 8)))  ; mostly off-screen

;; Test different colors with thickness
(define r17 (img-circle img500x400 260 340 20 1 '(thickness 3)))  ; color 1
(define r18 (img-circle img500x400 320 340 20 2 '(thickness 3)))  ; color 2  
(define r19 (img-circle img500x400 380 340 20 3 '(thickness 3)))  ; color 3

;; Test very large thickness values
(define r20 (img-circle img500x400 420 160 40 2 '(thickness 50))) ; extremely thick
(define r21 (img-circle img500x400 420 260 35 1 '(thickness 100))) ; massive thickness

;; Test thickness with very small circles
(define r22 (img-circle img500x400 300 320 5 3 '(thickness 2)))   ; small circle, normal thickness
(define r23 (img-circle img500x400 330 320 3 1 '(thickness 1)))   ; tiny circle, thin thickness
(define r24 (img-circle img500x400 360 320 7 2 '(thickness 4)))   ; small circle, thick border

;; Test invalid thickness values
(define r25 (trap (img-circle img500x400 100 100 25 1 '(thickness -5))))  ; negative thickness
(define r26 (trap (img-circle img500x400 100 100 25 1 '(thickness))))     ; thickness without value

;; Test buffer properties
(define dims (img-dims img500x400))
(define is_buffer (img-buffer? img500x400))

;; Display the result with 4-color palette
(disp-render img500x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24
         is_buffer (eq dims '(500 400)))
    (print "SUCCESS")
    (print "FAILURE"))