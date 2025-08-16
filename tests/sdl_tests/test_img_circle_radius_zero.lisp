(sdl-init)

(define win (sdl-create-window "Display library - circle radius zero test" 400 300))
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

;; Test filled circles with radius 0 (should behave like single pixel)
(define r1 (img-circle img400x300 50 50 0 1 '(filled)))     ; filled circle radius 0
(define r2 (img-circle img400x300 100 50 0 2 '(filled)))    ; filled circle radius 0, different color
(define r3 (img-circle img400x300 150 50 0 3 '(filled)))    ; filled circle radius 0, different color
(define r4 (img-circle img400x300 200 50 0 1 '(filled)))    ; filled circle radius 0, color 1 again

;; Test regular circles with radius 0 (outline only)
(define r5 (img-circle img400x300 50 100 0 1))              ; outline circle radius 0
(define r6 (img-circle img400x300 100 100 0 2))             ; outline circle radius 0, different color
(define r7 (img-circle img400x300 150 100 0 3))             ; outline circle radius 0, different color

;; Test circles with radius 0 and thickness (edge case)
(define r8 (img-circle img400x300 50 150 0 1 '(thickness 1)))   ; radius 0 with thickness 1
(define r9 (img-circle img400x300 100 150 0 2 '(thickness 3)))  ; radius 0 with thickness 3
(define r10 (img-circle img400x300 150 150 0 3 '(thickness 5))) ; radius 0 with thickness 5
(define r11 (img-circle img400x300 200 150 0 1 '(thickness 10))) ; radius 0 with large thickness

;; Test filled circles with radius 0 and thickness combined
(define r12 (img-circle img400x300 50 200 0 2 '(filled) '(thickness 2)))   ; filled + thickness, radius 0
(define r13 (img-circle img400x300 100 200 0 3 '(filled) '(thickness 1)))  ; filled + thickness, radius 0
(define r14 (img-circle img400x300 150 200 0 1 '(filled) '(thickness 4)))  ; filled + thickness, radius 0

;; Test circle sectors with radius 0 and filled
(define r15 (img-circle-sector img400x300 250 50 0 0 90 1 '(filled)))      ; filled sector, radius 0
(define r16 (img-circle-sector img400x300 300 50 0 45 135 2 '(filled)))    ; filled sector, radius 0
(define r17 (img-circle-sector img400x300 350 50 0 0 180 3 '(filled)))     ; filled sector, radius 0

;; Test circle segments with radius 0 and filled
(define r18 (img-circle-segment img400x300 250 100 0 0 90 1 '(filled)))    ; filled segment, radius 0
(define r19 (img-circle-segment img400x300 300 100 0 45 135 2 '(filled)))  ; filled segment, radius 0
(define r20 (img-circle-segment img400x300 350 100 0 0 180 3 '(filled)))   ; filled segment, radius 0

;; Test multiple radius 0 circles at same position (should overwrite)
(define r21 (img-circle img400x300 80 250 0 1 '(filled)))   ; first circle
(define r22 (img-circle img400x300 80 250 0 2 '(filled)))   ; second circle, same position, different color
(define r23 (img-circle img400x300 80 250 0 3 '(filled)))   ; third circle, same position, different color

;; Test radius 0 circles at edge positions
(define r24 (img-circle img400x300 0 0 0 1 '(filled)))      ; top-left corner
(define r25 (img-circle img400x300 399 0 0 2 '(filled)))    ; top-right corner
(define r26 (img-circle img400x300 0 299 0 3 '(filled)))    ; bottom-left corner
(define r27 (img-circle img400x300 399 299 0 1 '(filled)))  ; bottom-right corner

;; Test radius 0 circles off-screen (should be safe)
(define r28 (img-circle img400x300 -1 150 0 2 '(filled)))   ; off-screen left
(define r29 (img-circle img400x300 400 150 0 3 '(filled)))  ; off-screen right
(define r30 (img-circle img400x300 200 -1 0 1 '(filled)))   ; off-screen top
(define r31 (img-circle img400x300 200 300 0 2 '(filled)))  ; off-screen bottom

;; Verify we can read back the pixels we set
;; getpix is not added as an extension
;; (define p1 (img-getpix img400x300 50 50))   ; should be color 1
;; (define p2 (img-getpix img400x300 100 50))  ; should be color 2  
;; (define p3 (img-getpix img400x300 150 50))  ; should be color 3
;; (define p4 (img-getpix img400x300 80 250))  ; should be color 3 (last one written)

;; Test buffer properties
(define dims (img-dims img400x300))
(define is_buffer (img-buffer? img400x300))

;; Display the result
(disp-render img400x300 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31
         is_buffer (eq dims '(400 300)))
         ;;(eq p1 1) (eq p2 2) (eq p3 3) (eq p4 3))
    (print "SUCCESS")
    (progn
      (print "FAILURE - Debug info:")
      (print (list "p1:" p1 "p2:" p2 "p3:" p3 "p4:" p4))
      (print "FAILURE")))
