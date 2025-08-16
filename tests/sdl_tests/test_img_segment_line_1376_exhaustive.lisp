(sdl-init)

(define win (sdl-create-window "Line 1376 Exhaustive Test" 800 600))
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

(define img800x600 (img-buffer 'indexed16 800 600))

;; Clear the image buffer with black background
(img-clear img800x600 0)

;; Exhaustive tests targeting both branches of line 1376: if (angle0 < angle1)
;; We need in_both_caps && slice_overlaps0 && slice_overlaps1 to reach line 1376
;; Then test both: angle0 < angle1 (THEN) and angle0 >= angle1 (ELSE)

;; PART 1: THEN branch (angle0 < angle1) - Normal angle progression
;; These should reach lines 1380, 1393, etc. depending on angle0 < or >= 180

;; Systematic radius testing: 20, 30, 40, 50, 60, 70, 80
;; Systematic thickness testing: 3, 5, 7, 10, 12, 15, 18, 20
;; Systematic angle ranges to maximize slice intersection probability

;; Test 1-8: Radius 20 with various angle ranges and thicknesses
(define test1 (img-circle-segment img800x600 50 50 20 30 150 1 '(thickness 8)))
(define test2 (img-circle-segment img800x600 100 50 20 45 135 2 '(thickness 10)))
(define test3 (img-circle-segment img800x600 150 50 20 60 120 3 '(thickness 12)))
(define test4 (img-circle-segment img800x600 200 50 20 20 160 4 '(thickness 15)))
(define test5 (img-circle-segment img800x600 250 50 20 10 170 5 '(thickness 18)))
(define test6 (img-circle-segment img800x600 300 50 20 0 180 6 '(thickness 20)))
(define test7 (img-circle-segment img800x600 350 50 20 15 165 7 '(thickness 12)))
(define test8 (img-circle-segment img800x600 400 50 20 25 155 8 '(thickness 10)))

;; Test 9-16: Radius 30 with various combinations
(define test9 (img-circle-segment img800x600 450 50 30 40 140 9 '(thickness 7)))
(define test10 (img-circle-segment img800x600 500 50 30 35 145 10 '(thickness 9)))
(define test11 (img-circle-segment img800x600 550 50 30 50 130 11 '(thickness 11)))
(define test12 (img-circle-segment img800x600 600 50 30 30 150 12 '(thickness 13)))
(define test13 (img-circle-segment img800x600 650 50 30 20 160 13 '(thickness 16)))
(define test14 (img-circle-segment img800x600 700 50 30 10 170 14 '(thickness 18)))
(define test15 (img-circle-segment img800x600 750 50 30 5 175 15 '(thickness 20)))
(define test16 (img-circle-segment img800x600 50 100 30 15 165 1 '(thickness 14)))

;; Test 17-24: Radius 40 - medium size for good intersection geometry
(define test17 (img-circle-segment img800x600 100 100 40 25 155 2 '(thickness 8)))
(define test18 (img-circle-segment img800x600 150 100 40 35 145 3 '(thickness 10)))
(define test19 (img-circle-segment img800x600 200 100 40 45 135 4 '(thickness 12)))
(define test20 (img-circle-segment img800x600 250 100 40 30 150 5 '(thickness 15)))
(define test21 (img-circle-segment img800x600 300 100 40 20 160 6 '(thickness 18)))
(define test22 (img-circle-segment img800x600 350 100 40 10 170 7 '(thickness 20)))
(define test23 (img-circle-segment img800x600 400 100 40 0 180 8 '(thickness 16)))
(define test24 (img-circle-segment img800x600 450 100 40 40 140 9 '(thickness 12)))

;; Test 25-32: Radius 50 - classic size
(define test25 (img-circle-segment img800x600 500 100 50 50 130 10 '(thickness 6)))
(define test26 (img-circle-segment img800x600 550 100 50 60 120 11 '(thickness 8)))
(define test27 (img-circle-segment img800x600 600 100 50 30 150 12 '(thickness 10)))
(define test28 (img-circle-segment img800x600 650 100 50 20 160 13 '(thickness 12)))
(define test29 (img-circle-segment img800x600 700 100 50 15 165 14 '(thickness 15)))
(define test30 (img-circle-segment img800x600 750 100 50 10 170 15 '(thickness 18)))
(define test31 (img-circle-segment img800x600 50 150 50 5 175 1 '(thickness 20)))
(define test32 (img-circle-segment img800x600 100 150 50 25 155 2 '(thickness 14)))

;; Test 33-40: Radius 60 - larger radius
(define test33 (img-circle-segment img800x600 150 150 60 45 135 3 '(thickness 8)))
(define test34 (img-circle-segment img800x600 200 150 60 35 145 4 '(thickness 10)))
(define test35 (img-circle-segment img800x600 250 150 60 25 155 5 '(thickness 12)))
(define test36 (img-circle-segment img800x600 300 150 60 40 140 6 '(thickness 15)))
(define test37 (img-circle-segment img800x600 350 150 60 30 150 7 '(thickness 18)))
(define test38 (img-circle-segment img800x600 400 150 60 20 160 8 '(thickness 20)))
(define test39 (img-circle-segment img800x600 450 150 60 15 165 9 '(thickness 16)))
(define test40 (img-circle-segment img800x600 500 150 60 10 170 10 '(thickness 12)))

;; Test 41-48: Critical radii 61, 62, 63 that might trigger edge cases
(define test41 (img-circle-segment img800x600 550 150 61 30 150 11 '(thickness 8)))
(define test42 (img-circle-segment img800x600 600 150 61 45 135 12 '(thickness 10)))
(define test43 (img-circle-segment img800x600 650 150 61 20 160 13 '(thickness 12)))
(define test44 (img-circle-segment img800x600 700 150 62 35 145 14 '(thickness 15)))
(define test45 (img-circle-segment img800x600 750 150 62 25 155 15 '(thickness 18)))
(define test46 (img-circle-segment img800x600 50 200 62 40 140 1 '(thickness 20)))
(define test47 (img-circle-segment img800x600 100 200 63 50 130 2 '(thickness 16)))
(define test48 (img-circle-segment img800x600 150 200 63 30 150 3 '(thickness 12)))

;; Test 49-56: Larger radii 70, 80 for different geometric conditions
(define test49 (img-circle-segment img800x600 200 200 70 40 140 4 '(thickness 8)))
(define test50 (img-circle-segment img800x600 250 200 70 30 150 5 '(thickness 10)))
(define test51 (img-circle-segment img800x600 300 200 70 25 155 6 '(thickness 12)))
(define test52 (img-circle-segment img800x600 350 200 80 45 135 7 '(thickness 15)))
(define test53 (img-circle-segment img800x600 400 200 80 35 145 8 '(thickness 18)))
(define test54 (img-circle-segment img800x600 450 200 80 30 150 9 '(thickness 20)))
(define test55 (img-circle-segment img800x600 500 200 80 20 160 10 '(thickness 16)))
(define test56 (img-circle-segment img800x600 550 200 80 40 140 11 '(thickness 12)))

;; PART 2: ELSE branch (angle0 >= angle1) - Wrap-around angles
;; These test the split slice condition where angles wrap around 0°/360°

;; Test 57-64: Small radii with wrap-around angles
(define test57 (img-circle-segment img800x600 600 200 20 300 60 12 '(thickness 8)))   ; 300°-60°
(define test58 (img-circle-segment img800x600 650 200 20 315 45 13 '(thickness 10)))  ; 315°-45°
(define test59 (img-circle-segment img800x600 700 200 20 270 90 14 '(thickness 12)))  ; 270°-90°
(define test60 (img-circle-segment img800x600 750 200 20 330 30 15 '(thickness 15)))  ; 330°-30°
(define test61 (img-circle-segment img800x600 50 250 30 285 75 1 '(thickness 18)))   ; 285°-75°
(define test62 (img-circle-segment img800x600 100 250 30 320 40 2 '(thickness 20)))  ; 320°-40°
(define test63 (img-circle-segment img800x600 150 250 30 350 10 3 '(thickness 16)))  ; 350°-10°
(define test64 (img-circle-segment img800x600 200 250 30 340 20 4 '(thickness 12)))  ; 340°-20°

;; Test 65-72: Medium radii with wrap-around
(define test65 (img-circle-segment img800x600 250 250 40 270 90 5 '(thickness 8)))   ; 270°-90°
(define test66 (img-circle-segment img800x600 300 250 40 300 60 6 '(thickness 10)))  ; 300°-60°
(define test67 (img-circle-segment img800x600 350 250 40 315 45 7 '(thickness 12)))  ; 315°-45°
(define test68 (img-circle-segment img800x600 400 250 40 330 30 8 '(thickness 15)))  ; 330°-30°
(define test69 (img-circle-segment img800x600 450 250 50 285 75 9 '(thickness 18)))  ; 285°-75°
(define test70 (img-circle-segment img800x600 500 250 50 320 40 10 '(thickness 20))) ; 320°-40°
(define test71 (img-circle-segment img800x600 550 250 50 340 20 11 '(thickness 16))) ; 340°-20°
(define test72 (img-circle-segment img800x600 600 250 50 350 10 12 '(thickness 12))) ; 350°-10°

;; Test 73-80: Larger radii with wrap-around
(define test73 (img-circle-segment img800x600 650 250 60 270 90 13 '(thickness 8)))  ; 270°-90°
(define test74 (img-circle-segment img800x600 700 250 60 300 60 14 '(thickness 10))) ; 300°-60°
(define test75 (img-circle-segment img800x600 750 250 60 315 45 15 '(thickness 12))) ; 315°-45°
(define test76 (img-circle-segment img800x600 50 300 70 330 30 1 '(thickness 15)))  ; 330°-30°
(define test77 (img-circle-segment img800x600 100 300 70 285 75 2 '(thickness 18))) ; 285°-75°
(define test78 (img-circle-segment img800x600 150 300 70 320 40 3 '(thickness 20))) ; 320°-40°
(define test79 (img-circle-segment img800x600 200 300 80 340 20 4 '(thickness 16))) ; 340°-20°
(define test80 (img-circle-segment img800x600 250 300 80 350 10 5 '(thickness 12))) ; 350°-10°

;; Test 81-88: Critical radii with wrap-around
(define test81 (img-circle-segment img800x600 300 300 61 270 90 6 '(thickness 8)))  ; 270°-90°
(define test82 (img-circle-segment img800x600 350 300 61 300 60 7 '(thickness 10))) ; 300°-60°
(define test83 (img-circle-segment img800x600 400 300 61 315 45 8 '(thickness 12))) ; 315°-45°
(define test84 (img-circle-segment img800x600 450 300 62 330 30 9 '(thickness 15))) ; 330°-30°
(define test85 (img-circle-segment img800x600 500 300 62 285 75 10 '(thickness 18))) ; 285°-75°
(define test86 (img-circle-segment img800x600 550 300 62 320 40 11 '(thickness 20))) ; 320°-40°
(define test87 (img-circle-segment img800x600 600 300 63 340 20 12 '(thickness 16))) ; 340°-20°
(define test88 (img-circle-segment img800x600 650 300 63 350 10 13 '(thickness 12))) ; 350°-10°

;; Test 89-96: Edge cases - very small and very large wrap-around angles
(define test89 (img-circle-segment img800x600 700 300 40 359 1 14 '(thickness 8)))   ; 359°-1° (tiny wrap)
(define test90 (img-circle-segment img800x600 750 300 40 358 2 15 '(thickness 10)))  ; 358°-2°
(define test91 (img-circle-segment img800x600 50 350 50 180 179 1 '(thickness 12)))  ; 180°-179° (tiny wrap)
(define test92 (img-circle-segment img800x600 100 350 50 181 179 2 '(thickness 15))) ; 181°-179°
(define test93 (img-circle-segment img800x600 150 350 60 200 100 3 '(thickness 18))) ; 200°-100° (large wrap)
(define test94 (img-circle-segment img800x600 200 350 60 220 80 4 '(thickness 20)))  ; 220°-80°
(define test95 (img-circle-segment img800x600 250 350 70 240 60 5 '(thickness 16)))  ; 240°-60°
(define test96 (img-circle-segment img800x600 300 350 70 260 40 6 '(thickness 12)))  ; 260°-40°

;; Test 97-100: Maximum thickness tests to force slice intersections
(define test97 (img-circle-segment img800x600 350 350 50 30 150 7 '(thickness 25)))  ; Very thick, normal angles
(define test98 (img-circle-segment img800x600 400 350 50 270 90 8 '(thickness 25)))  ; Very thick, wrap-around
(define test99 (img-circle-segment img800x600 450 350 60 45 135 9 '(thickness 30)))  ; Extremely thick, normal
(define test100 (img-circle-segment img800x600 500 350 60 315 45 10 '(thickness 30))) ; Extremely thick, wrap

;; Test error conditions
(define test101 (trap (img-circle-segment "invalid" 100 100 50 30 150 1 '(thickness 8))))
(define test102 (trap (img-circle-segment img800x600 100 100 50 30 150 "invalid" '(thickness 8))))

;; Display the result with a 16-color palette
(disp-render img800x600 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

;; Check test results - all drawing operations should succeed
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         test31 test32 test33 test34 test35 test36 test37 test38 test39 test40
         test41 test42 test43 test44 test45 test46 test47 test48 test49 test50
         test51 test52 test53 test54 test55 test56 test57 test58 test59 test60
         test61 test62 test63 test64 test65 test66 test67 test68 test69 test70
         test71 test72 test73 test74 test75 test76 test77 test78 test79 test80
         test81 test82 test83 test84 test85 test86 test87 test88 test89 test90
         test91 test92 test93 test94 test95 test96 test97 test98 test99 test100
         (eq test101 '(exit-error type_error))
         (eq test102 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))