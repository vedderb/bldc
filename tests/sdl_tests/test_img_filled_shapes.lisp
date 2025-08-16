(sdl-init)

(define win (sdl-create-window "Display library - filled shapes test" 400 300))
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

;; Test filled rectangles
(define r1 (img-rectangle img400x300 10 10 60 40 1))           ; outline only
(define r2 (img-rectangle img400x300 80 10 60 40 2 '(filled))) ; filled

;; Test filled circles
(define r3 (img-circle img400x300 40 80 25 1))           ; outline only
(define r4 (img-circle img400x300 120 80 25 2 '(filled))) ; filled

;; Test filled triangles
(define r5 (img-triangle img400x300 200 20 240 60 160 60 1))           ; outline only
(define r6 (img-triangle img400x300 280 20 320 60 240 60 2 '(filled))) ; filled

;; Test filled arcs
(define r7 (img-arc img400x300 40 150 30 0 180 1))           ; outline only
(define r8 (img-arc img400x300 120 150 30 0 180 2 '(filled))) ; filled

;; Test filled circle segments  
(define r9 (img-circle-segment img400x300 220 150 30 45 135 1))           ; outline only
(define r10 (img-circle-segment img400x300 300 150 30 45 135 2 '(filled))) ; filled

;; Test various filled shapes with different parameters
(define r11 (img-rectangle img400x300 10 200 50 50 3 '(filled)))
(define r12 (img-circle img400x300 100 225 20 3 '(filled)))
(define r13 (img-triangle img400x300 150 200 180 240 120 240 3 '(filled)))
(define r14 (img-arc img400x300 250 225 25 90 270 3 '(filled)))

;; Test edge cases with filled property
(define r15 (img-rectangle img400x300 300 200 50 50 1 '(filled)))  ; at edge
(define r16 (img-circle img400x300 380 280 15 2 '(filled)))        ; partially off-screen

;; Test with invalid filled property format (should still work)
;; TODO: Look at this behaviour
;;(define r17 (trap (img-rectangle img400x300 350 50 30 30 1 '(invalid-property))))
;;(define r18 (trap (img-circle img400x300 350 100 15 1 '(filled invalid))))

;; Test buffer properties
(define dims (img-dims img400x300))
(define is_buffer (img-buffer? img400x300))

;; Display the result with 4-color palette
(disp-render img400x300 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

;; (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 
;;          is_buffer (eq dims '(400 300))
;;          ;; These should work even with invalid properties
;;          (eq r17 '(exit-error eval_error)) 
;;          (eq r18 '(exit-error eval_error)))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 
         is_buffer (eq dims '(400 300)))
    (print "SUCCESS")
    (print "FAILURE"))
