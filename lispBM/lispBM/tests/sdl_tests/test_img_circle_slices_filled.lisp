(sdl-init)

(define win (sdl-create-window "Display library - filled circle slices test" 600 500))
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

;; Test filled circle sectors (pie slices) - comprehensive coverage
(define r1 (img-circle-sector img600x500 80 80 50 0 90 1 '(filled)))      ; filled quarter pie
(define r2 (img-circle-sector img600x500 200 80 50 0 180 2 '(filled)))     ; filled half pie  
(define r3 (img-circle-sector img600x500 320 80 50 0 270 3 '(filled)))     ; filled three-quarter pie
(define r4 (img-circle-sector img600x500 440 80 50 45 225 1 '(filled)))    ; filled half pie, offset
(define r5 (img-circle-sector img600x500 80 200 40 90 180 2 '(filled)))    ; filled quarter, different position
(define r6 (img-circle-sector img600x500 200 200 40 180 270 3 '(filled)))  ; filled quarter, bottom-left
(define r7 (img-circle-sector img600x500 320 200 40 270 360 1 '(filled)))  ; filled quarter, top-left
(define r8 (img-circle-sector img600x500 440 200 40 315 45 2 '(filled)))   ; filled quarter crossing 0

;; Test filled circle segments (chords) - comprehensive coverage  
(define r9 (img-circle-segment img600x500 80 320 50 0 90 1 '(filled)))     ; filled quarter chord
(define r10 (img-circle-segment img600x500 200 320 50 0 180 2 '(filled)))  ; filled half chord
(define r11 (img-circle-segment img600x500 320 320 50 0 270 3 '(filled)))  ; filled three-quarter chord
(define r12 (img-circle-segment img600x500 440 320 50 45 225 1 '(filled))) ; filled half chord, offset
(define r13 (img-circle-segment img600x500 80 420 40 90 180 2 '(filled)))  ; filled quarter chord, different position
(define r14 (img-circle-segment img600x500 200 420 40 180 270 3 '(filled))) ; filled quarter chord, bottom-left
(define r15 (img-circle-segment img600x500 320 420 40 270 360 1 '(filled))) ; filled quarter chord, top-left
(define r16 (img-circle-segment img600x500 440 420 40 315 45 2 '(filled)))  ; filled quarter chord crossing 0

;; Test small filled sectors and segments
(define r17 (img-circle-sector img600x500 520 80 20 0 45 3 '(filled)))     ; small filled sector
(define r18 (img-circle-segment img600x500 520 200 20 0 45 1 '(filled)))   ; small filled segment
(define r19 (img-circle-sector img600x500 520 320 15 0 60 2 '(filled)))    ; tiny filled sector
(define r20 (img-circle-segment img600x500 520 420 15 0 60 3 '(filled)))   ; tiny filled segment

;; Test large filled sectors and segments  
(define r21 (img-circle-sector img600x500 560 160 35 0 300 1 '(filled)))   ; large filled sector
(define r22 (img-circle-segment img600x500 560 280 35 30 330 2 '(filled))) ; large filled segment

;; Test filled with very small angles
(define r23 (img-circle-sector img600x500 150 350 30 0 10 3 '(filled)))    ; very thin filled sector
(define r24 (img-circle-segment img600x500 250 350 30 0 10 1 '(filled)))   ; very thin filled segment
(define r25 (img-circle-sector img600x500 350 350 30 0 5 2 '(filled)))     ; extremely thin filled sector
(define r26 (img-circle-segment img600x500 450 350 30 0 5 3 '(filled)))    ; extremely thin filled segment

;; Test filled with crossing zero degrees
(define r27 (img-circle-sector img600x500 150 450 25 350 20 1 '(filled)))  ; filled sector crossing 0
(define r28 (img-circle-segment img600x500 250 450 25 340 30 2 '(filled))) ; filled segment crossing 0
(define r29 (img-circle-sector img600x500 350 450 25 315 45 3 '(filled)))  ; filled sector crossing 0, larger
(define r30 (img-circle-segment img600x500 450 450 25 300 60 1 '(filled))) ; filled segment crossing 0, larger

;; Test filled slices going off-screen
(define r31 (img-circle-sector img600x500 580 450 30 0 90 2 '(filled)))    ; partially off-screen filled
(define r32 (img-circle-segment img600x500 600 480 25 0 180 3 '(filled)))  ; mostly off-screen filled

;; Test buffer properties
(define dims (img-dims img600x500))
(define is_buffer (img-buffer? img600x500))

;; Display the result with 4-color palette
(disp-render img600x500 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32
         is_buffer (eq dims '(600 500)))
    (print "SUCCESS")
    (print "FAILURE"))