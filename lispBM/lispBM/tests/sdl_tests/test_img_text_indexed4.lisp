(sdl-init)

(define win (sdl-create-window "Text on indexed4 Test" 600 400))
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

;; Load a font
(define font-file (fopen "./sdl_tests/font_16_26_aa.bin" "r"))
(define font (load-file font-file))

;; Create indexed4 image buffer (4 colors: 0, 1, 2, 3)
(define img600x400 (img-buffer 'indexed4 600 400))

;; Clear with background color 0
(img-clear img600x400 0)

;; Test 1-4: Basic text with different foreground colors, background 0
(define test1 (img-text img600x400 50 50 1 0 font "FG1 BG0"))
(define test2 (img-text img600x400 50 80 2 0 font "FG2 BG0"))  
(define test3 (img-text img600x400 50 110 3 0 font "FG3 BG0"))
(define test4 (img-text img600x400 50 140 0 0 font "FG0 BG0"))  ; Invisible on same background

;; Test 5-8: Different background colors with foreground 1
(define test5 (img-text img600x400 300 50 1 0 font "FG1 BG0"))
(define test6 (img-text img600x400 300 80 1 1 font "FG1 BG1"))   ; Same fg/bg
(define test7 (img-text img600x400 300 110 1 2 font "FG1 BG2"))
(define test8 (img-text img600x400 300 140 1 3 font "FG1 BG3"))

;; Test 9-12: All foreground/background combinations with high contrast
(define test9 (img-text img600x400 50 200 3 0 font "FG3 BG0"))   ; Blue on black
(define test10 (img-text img600x400 50 230 2 0 font "FG2 BG0"))  ; Green on black
(define test11 (img-text img600x400 50 260 1 0 font "FG1 BG0"))  ; Red on black
(define test12 (img-text img600x400 50 290 0 3 font "FG0 BG3"))  ; Black on blue

;; Test 13-16: Different background colors with foreground 2
(define test13 (img-text img600x400 300 200 2 0 font "FG2 BG0"))
(define test14 (img-text img600x400 300 230 2 1 font "FG2 BG1"))
(define test15 (img-text img600x400 300 260 2 2 font "FG2 BG2"))  ; Same fg/bg
(define test16 (img-text img600x400 300 290 2 3 font "FG2 BG3"))

;; Test 17-20: Text positioning at boundaries
(define test17 (img-text img600x400 0 0 1 0 font "Top-Left"))
(define test18 (img-text img600x400 520 0 2 0 font "Top-Right"))
(define test19 (img-text img600x400 0 370 3 0 font "Bottom-Left"))
(define test20 (img-text img600x400 480 370 1 0 font "Bottom-Right"))

;; Test 21-24: Text with background colors showing
(define test21 (img-text img600x400 150 50 1 2 font "Red on Green"))
(define test22 (img-text img600x400 150 80 2 3 font "Green on Blue"))
(define test23 (img-text img600x400 150 110 3 1 font "Blue on Red"))
(define test24 (img-text img600x400 150 140 0 1 font "Black on Red"))

;; Test 25-28: Vertical text with different fg/bg combinations
(define test25 (img-text img600x400 450 350 1 0 font "UP" 'up))
(define test26 (img-text img600x400 470 350 2 3 font "UP-BG" 'up))
(define test27 (img-text img600x400 490 50 3 0 font "DOWN" 'down))
(define test28 (img-text img600x400 510 50 1 2 font "DOWN-BG" 'down))

;; Test 29-32: Single characters with all color combinations
(define test29 (img-text img600x400 400 200 0 1 font "A"))
(define test30 (img-text img600x400 415 200 1 2 font "B"))
(define test31 (img-text img600x400 430 200 2 3 font "C"))
(define test32 (img-text img600x400 445 200 3 0 font "D"))

;; Test 33-36: Empty strings (should succeed but render nothing)
(define test33 (img-text img600x400 100 320 1 0 font ""))
(define test34 (img-text img600x400 110 320 2 1 font ""))
(define test35 (img-text img600x400 120 320 3 2 font ""))
(define test36 (img-text img600x400 130 320 0 3 font ""))

;; Test 37-40: Special characters with background colors
(define test37 (img-text img600x400 50 350 1 2 font "!@#$%"))
(define test38 (img-text img600x400 200 350 2 3 font "()[]{}"))
(define test39 (img-text img600x400 350 350 3 1 font "+-=*"))
(define test40 (img-text img600x400 450 350 0 2 font ".,;:"))

;; Test 41-44: Overlapping text with different background colors
(define test41 (img-text img600x400 150 170 1 0 font "BACK"))
(define test42 (img-text img600x400 155 175 2 3 font "OVER"))  ; With background
(define test43 (img-text img600x400 160 180 3 0 font "LAP"))   ; No background
(define test44 (img-text img600x400 165 185 1 2 font "TEST"))  ; With background

;; Test 45-48: Edge cases with invalid colors (should clamp/wrap)
(define test45 (img-text img600x400 400 100 4 0 font "FG4"))    ; FG > 3
(define test46 (img-text img600x400 400 120 1 4 font "BG4"))    ; BG > 3
(define test47 (img-text img600x400 400 140 255 0 font "FG255")) ; Large FG
(define test48 (img-text img600x400 400 160 1 255 font "BG255")) ; Large BG

;; Test 49-52: Negative color values
(define test49 (img-text img600x400 500 100 -1 0 font "FG-1"))
(define test50 (img-text img600x400 500 120 1 -1 font "BG-1"))
(define test51 (img-text img600x400 500 140 -2 -3 font "NEG"))
(define test52 (img-text img600x400 500 160 2 0 font "Normal"))

;; Test AA font with color lists (antialiased font supports color gradients)
;; Test 53-56: Two-color lists (background + foreground)
(define test53 (img-text img600x400 50 320 '(0 1) font "2-Color BG0->FG1"))
(define test54 (img-text img600x400 250 320 '(2 3) font "2-Color BG2->FG3"))
(define test55 (img-text img600x400 450 320 '(1 0) font "2-Color BG1->FG0"))
(define test56 (img-text img600x400 50 340 '(3 2) font "2-Color BG3->FG2"))

;; Test 57-60: Three-color lists (background + 2 intermediate colors)
(define test57 (img-text img600x400 250 340 '(0 1 2) font "3-Color 0->1->2"))
(define test58 (img-text img600x400 450 340 '(1 2 3) font "3-Color 1->2->3"))
(define test59 (img-text img600x400 50 360 '(3 2 1) font "3-Color 3->2->1"))
(define test60 (img-text img600x400 250 360 '(2 0 1) font "3-Color 2->0->1"))

;; Test 61-64: Four-color lists (background + 3 gradient colors)
(define test61 (img-text img600x400 450 360 '(0 1 2 3) font "4-Color 0->1->2->3"))
(define test62 (img-text img600x400 50 380 '(3 2 1 0) font "4-Color 3->2->1->0"))
(define test63 (img-text img600x400 300 380 '(1 3 0 2) font "4-Color 1->3->0->2"))
(define test64 (img-text img600x400 520 380 '(2 0 3 1) font "4-Color 2->0->3->1"))

;; Test error conditions  
;; Test 65: Invalid image buffer
(define test65 (trap (img-text "not-an-image" 50 50 1 0 font "Error")))

;; Test 66: Nil image buffer
(define test66 (trap (img-text nil 50 50 1 0 font "Error")))

;; Test 67: Invalid font
(define test67 (trap (img-text img600x400 50 50 1 0 "invalid" "Error")))

;; Test 68: Invalid text (non-string)
(define test68 (trap (img-text img600x400 50 50 1 0 font 12345)))

;; Test 69-72: Invalid color lists (out of range values)
(define test69 (img-text img600x400 100 50 '(0 4) font "Invalid 4"))  ; 4 > 3
(define test70 (img-text img600x400 100 70 '(-1 1) font "Invalid -1")) ; -1 < 0
(define test71 (img-text img600x400 100 90 '(0 1 5) font "Invalid 5")) ; 5 > 3
(define test72 (img-text img600x400 100 110 '(255 0 1 2) font "Invalid 255")) ; 255 > 3

;; Display the result with indexed4 color palette
(disp-render img600x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF))

;; (list test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
;;          test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
;;          test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
;;          test31 test32 test33 test34 test35 test36 test37 test38 test39 test40
;;          test41 test42 test43 test44 test45 test46 test47 test48 test49 test50
;;          test51 test52 test53 test54 test55 test56 test57 test58 test59 test60
;;          test61 test62 test63 test64
;;          (eq test65 '(exit-error type_error))
;;          (eq test66 '(exit-error type_error))
;;          (eq test67 '(exit-error type_error))
;;          (eq test68 '(exit-error type_error))
;;          (eq test69 '(exit-error type_error))
;;          (eq test70 '(exit-error type_error))
;;          (eq test71 '(exit-error type_error))
;;          (eq test72 '(exit-error type_error)))

;; Check test results
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         test31 test32 test33 test34 test35 test36 test37 test38 test39 test40
         test41 test42 test43 test44 test45 test46 test47 test48 test49 test50
         test51 test52 test53 test54 test55 test56 test57 test58 test59 test60
         test61 test62 test63 test64
         (eq test65 '(exit-error type_error))
         (eq test66 '(exit-error type_error))
         (eq test67 '(exit-error type_error))
         (eq test68 '(exit-error type_error))
         test69
         test70
         test71
         test72)
    (print "SUCCESS")
    (print "FAILURE"))
