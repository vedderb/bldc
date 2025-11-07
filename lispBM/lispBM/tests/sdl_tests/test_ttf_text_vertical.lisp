(sdl-init)

(define win (sdl-create-window "Display library - TTF vertical text test" 400 200))
(define rend (sdl-create-soft-renderer win))

(defun event-loop (w)
  (let ((event (sdl-poll-event)))
    (if (eq event 'sdl-quit-event)
        (custom-destruct w)
        (progn  
          (yield 5000)
          (event-loop w)))))

(spawn 100 event-loop win)

;; Load and prepare a TTF font - only prepare characters we actually use
(define font-file (fopen "./sdl_tests/Ubuntu-Regular.ttf" "r"))
(define font-data (load-file font-file))
;; Exact characters used in test strings:
;; Space, letters: HorizontalTFUpwerdVicDnAXOKBotmLeftRighMyGfSngC!@#$%^&*()aslruge
;; Numbers: 123
(define font (ttf-prepare font-data 32 'indexed4 " UpwardsDonHiztl"))

;; Define color palette for anti-aliased text
(define aa-colors '(0 4456448 10027008 16711680))  ; black to red gradient

;; Connect the renderer to the display library
(sdl-set-active-renderer rend)

(define img400x200 (img-buffer 'rgb888 400 200))

;; Test normal horizontal TTF text for comparison
(define r1 (ttf-text img400x200 50 50 aa-colors font "Horizontal"))

;; Test vertical TTF text with 'up direction (prints upwards)
(define r2 (ttf-text img400x200 150 150 aa-colors font "Upwards" 'up))
(define r3 (ttf-text img400x200 200 150 aa-colors font "Up" 'up))
(define r4 (ttf-text img400x200 250 150 aa-colors font "Upwards Up" 'up))

;; Test vertical TTF text with 'down direction (prints downwards) 
(define r5 (ttf-text img400x200 250 50 aa-colors font "Downwards" 'down))
(define r6 (ttf-text img400x200 200 50 aa-colors font "Down" 'down))
(define r7 (ttf-text img400x200 250 50 aa-colors font "Downwards Down" 'down))

;; Test shorter text strings with vertical directions
(define r8 (ttf-text img400x200 100 100 aa-colors font "D" 'up))       ; single character up
(define r9 (ttf-text img400x200 120 100 aa-colors font "ow" 'up))      ; short text up
(define r10 (ttf-text img400x200 300 100 aa-colors font "n" 'down))    ; single character down
(define r11 (ttf-text img400x200 320 100 aa-colors font "ri" 'down))   ; short text down

;; Test empty string with vertical directions
(define r12 (ttf-text img400x200 140 100 aa-colors font "" 'up))       ; empty string up
(define r13 (ttf-text img400x200 160 100 aa-colors font "" 'down))     ; empty string down

;; Test vertical TTF text at edge positions
(define r14 (ttf-text img400x200 50 190 aa-colors font "Upwards" 'up))     ; near bottom, going up
(define r15 (ttf-text img400x200 270 10 aa-colors font "Downwards" 'down))    ; near top, going down
(define r16 (ttf-text img400x200 10 100 aa-colors font "Upwards" 'up))       ; left edge, going up
(define r17 (ttf-text img400x200 290 100 aa-colors font "Downwards" 'down)) ; right edge, going down

;; Test vertical TTF text that may go off-screen
(define r18 (ttf-text img400x200 180 20 aa-colors font "Upwards Upwards" 'up))     ; might go off top
(define r19 (ttf-text img400x200 180 180 aa-colors font "Downwards Downwards" 'down)) ; might go off bottom

;; Test longer text strings with vertical directions
(define r20 (ttf-text img400x200 220 20 aa-colors font "Upwards Upwards Upwards" 'up))
(define r21 (ttf-text img400x200 120 80 aa-colors font "Downwards Downwards Downwards" 'down))


;; Test invalid direction arguments (should handle gracefully)
(define r30 (trap (ttf-text img400x200 50 100 aa-colors font "Horizontal" 'left)))   ; invalid direction
(define r31 (trap (ttf-text img400x200 50 120 aa-colors font "Horizontal" 'right)))  ; invalid direction
(define r32 (trap (ttf-text img400x200 50 140 aa-colors font "Horizontal" 123)))     ; invalid direction type

;; Test with different color palettes for vertical text
(define aa-blue '(0 1118481 2236962 3355443))   ; black to blue gradient
(define aa-green '(0 2228224 4456448 6684672))  ; black to green gradient

(define r33 (ttf-text img400x200 100 100 aa-blue font "Upwards" 'up))
(define r34 (ttf-text img400x200 200 50 aa-green font "Downwards" 'down))

;; Test buffer properties
(define dims (img-dims img400x200))
(define is_buffer (img-buffer? img400x200))

;; Display the result
(disp-render img400x200 0 0 aa-colors)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r33 r34
         is_buffer (eq dims '(400 200)))
    (print "SUCCESS")
    (print "FAILURE"))
