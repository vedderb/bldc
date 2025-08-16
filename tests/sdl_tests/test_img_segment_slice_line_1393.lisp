(sdl-init)

(define win (sdl-create-window "Circle Segment Line 1393 Test" 600 400))
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

(define img600x400 (img-buffer 'indexed16 600 400))

;; Clear the image buffer with black background
(img-clear img600x400 0)

;; Tests targeting line 1393 in handle_arc_slice
;; Conditions needed:
;; - segment=true, filled=false (img-circle-segment without 'filled)
;; - angle0 < angle1 
;; - angle0 >= 180 degrees (>= M_PI) - KEY DIFFERENCE from line 1380
;; - in_both_caps && slice_overlaps0 && slice_overlaps1
;; - Thick enough to create slice intersections

;; Test 1: 225-315 degrees, thickness 6, radius 50
;; Start angle > 180 degrees, centered around 270 degrees
(define test1 (img-circle-segment img600x400 100 100 50 225 315 1 '(thickness 6)))

;; Test 2: 210-330 degrees, thickness 8, radius 60
;; Wider angle range, thicker line, start > 180
(define test2 (img-circle-segment img600x400 250 100 60 210 330 2 '(thickness 8)))

;; Test 3: 240-300 degrees, thickness 10, radius 45
;; Centered around 270 degrees, very thick
(define test3 (img-circle-segment img600x400 400 100 45 240 300 3 '(thickness 10)))

;; Test 4: 195-345 degrees, thickness 5, radius 55
;; Very wide angle, start just over 180 degrees
(define test4 (img-circle-segment img600x400 500 100 55 195 345 4 '(thickness 5)))

;; Test 5: 225-270 degrees, thickness 12, radius 40
;; Quarter circle in bottom quadrants, thick line
(define test5 (img-circle-segment img600x400 100 200 40 225 270 5 '(thickness 12)))

;; Test 6: 210-300 degrees, thickness 7, radius 65
;; Larger radius with moderate thickness
(define test6 (img-circle-segment img600x400 250 200 65 210 300 6 '(thickness 7)))

;; Test 7: 255-285 degrees, thickness 15, radius 35
;; Narrow angle around 270 degrees, very thick
(define test7 (img-circle-segment img600x400 400 200 35 255 285 7 '(thickness 15)))

;; Test 8: 190-350 degrees, thickness 4, radius 70
;; Almost semicircle, thinner line, large radius, start just > 180
(define test8 (img-circle-segment img600x400 500 200 70 190 350 8 '(thickness 4)))

;; Test edge case: start angle exactly 180 degrees
;; Test 9: 180-270 degrees, thickness 8, radius 50
(define test9 (img-circle-segment img600x400 100 300 50 180 270 9 '(thickness 8)))

;; Test 10: 180-300 degrees, thickness 6, radius 55
(define test10 (img-circle-segment img600x400 250 300 55 180 300 10 '(thickness 6)))

;; Test 11: 180-360 degrees, thickness 5, radius 45
;; Half circle starting at 180 degrees
(define test11 (img-circle-segment img600x400 400 300 45 180 360 11 '(thickness 5)))

;; Test specific radii that might trigger edge cases (61, 62, 63)
;; Test 12: Radius 61, 225-315 degrees, thickness 8
(define test12 (img-circle-segment img600x400 500 300 61 225 315 12 '(thickness 8)))

;; Test 13: Radius 62, 210-330 degrees, thickness 6
(define test13 (img-circle-segment img600x400 50 50 62 210 330 13 '(thickness 6)))

;; Test 14: Radius 63, 240-300 degrees, thickness 10
(define test14 (img-circle-segment img600x400 150 50 63 240 300 14 '(thickness 10)))

;; Test angles just over 180 degrees
;; Test 15: 181-271 degrees, thickness 5, radius 50
(define test15 (img-circle-segment img600x400 250 50 50 181 271 15 '(thickness 5)))

;; Test 16: 185-275 degrees, thickness 7, radius 45
(define test16 (img-circle-segment img600x400 350 50 45 185 275 1 '(thickness 7)))

;; Test 17: 190-280 degrees, thickness 9, radius 55
(define test17 (img-circle-segment img600x400 450 50 55 190 280 2 '(thickness 9)))

;; Test angles that should maximize slice intersection probability in 3rd/4th quadrants
;; Test 18: 200-340 degrees, thickness 6, radius 48
(define test18 (img-circle-segment img600x400 550 50 48 200 340 3 '(thickness 6)))

;; Test 19: 220-320 degrees, thickness 8, radius 52
(define test19 (img-circle-segment img600x400 50 150 52 220 320 4 '(thickness 8)))

;; Test 20: 205-335 degrees, thickness 7, radius 58
(define test20 (img-circle-segment img600x400 150 150 58 205 335 5 '(thickness 7)))

;; Test with rounded ends (which might affect cap line calculations)
;; Test 21: 225-315 degrees, thickness 8, radius 50, rounded
(define test21 (img-circle-segment img600x400 250 150 50 225 315 6 '(rounded) '(thickness 8)))

;; Test 22: 210-330 degrees, thickness 10, radius 45, rounded
(define test22 (img-circle-segment img600x400 350 150 45 210 330 7 '(rounded) '(thickness 10)))

;; Test crossing 360/0 degree boundary
;; Test 23: 270-30 degrees (wraps around), thickness 6, radius 50
(define test23 (img-circle-segment img600x400 450 150 50 270 30 8 '(thickness 6)))

;; Test 24: 315-45 degrees (wraps around), thickness 8, radius 45
(define test24 (img-circle-segment img600x400 550 150 45 315 45 9 '(thickness 8)))

;; Test 25: 300-60 degrees (wraps around), thickness 7, radius 55
(define test25 (img-circle-segment img600x400 50 250 55 300 60 10 '(thickness 7)))

;; Test large angles starting > 180 degrees
;; Test 26: 180-359 degrees, thickness 5, radius 50
;; Almost full circle starting at 180
(define test26 (img-circle-segment img600x400 150 250 50 180 359 11 '(thickness 5)))

;; Test 27: 200-359 degrees, thickness 6, radius 45
(define test27 (img-circle-segment img600x400 250 250 45 200 359 12 '(thickness 6)))

;; Test very thick segments that should definitely intersect both caps
;; Test 28: 225-315 degrees, thickness 20, radius 50
(define test28 (img-circle-segment img600x400 350 250 50 225 315 13 '(thickness 20)))

;; Test 29: 210-330 degrees, thickness 18, radius 55
(define test29 (img-circle-segment img600x400 450 250 55 210 330 14 '(thickness 18)))

;; Test 30: 240-300 degrees, thickness 16, radius 45
(define test30 (img-circle-segment img600x400 550 250 45 240 300 15 '(thickness 16)))

;; Test specific angles that create good geometry for the condition
;; Test 31: 202.5-337.5 degrees (135 degree span), thickness 6, radius 50
(define test31 (img-circle-segment img600x400 50 350 50 202.5 337.5 1 '(thickness 6)))

;; Test 32: 247.5-292.5 degrees (45 degree span), thickness 14, radius 38
(define test32 (img-circle-segment img600x400 150 350 38 247.5 292.5 2 '(thickness 14)))

;; Test 33: 191.25-348.75 degrees, thickness 5, radius 60
(define test33 (img-circle-segment img600x400 250 350 60 191.25 348.75 3 '(thickness 5)))

;; Test error conditions (should not crash)
;; Test 34: Invalid image buffer
(define test34 (trap (img-circle-segment "invalid" 100 100 50 225 315 1 '(thickness 6))))

;; Test 35: Invalid color
(define test35 (trap (img-circle-segment img600x400 100 100 50 225 315 "invalid" '(thickness 6))))

;; Display the result with a 16-color palette
(disp-render img600x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

;; Check test results
;; Most drawing operations should succeed (return t)
;; Error conditions should return appropriate error types
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         test31 test32 test33
         (eq test34 '(exit-error type_error))
         (eq test35 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))