(sdl-init)

(define win (sdl-create-window "Rounded Arcs Edge Cases Test" 800 600))
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

;; Test rounded arcs with specific radii (61, 62, 63) and thicknesses (2, 8, 16)
;; These combinations might reveal edge cases in the rounded arc rendering

;; Section 1: Thickness 2 tests
;; Row 1: Radius 61, thickness 2
;; Test 1: 0.5 degrees (very small arc)
(define test1 (img-arc img800x600 80 80 61 0 0.5 1 '(rounded) '(thickness 2)))

;; Test 2: 179 degrees (just under semicircle)
(define test2 (img-arc img800x600 200 80 61 0 179 2 '(rounded) '(thickness 2)))

;; Test 3: 180 degrees (exact semicircle)
(define test3 (img-arc img800x600 320 80 61 0 180 3 '(rounded) '(thickness 2)))

;; Test 4: 181 degrees (just over semicircle)
(define test4 (img-arc img800x600 440 80 61 0 181 4 '(rounded) '(thickness 2)))

;; Test 5: 359 degrees (almost full circle)
(define test5 (img-arc img800x600 560 80 61 0 359 5 '(rounded) '(thickness 2)))

;; Test 6: 361 degrees (over full circle)
(define test6 (img-arc img800x600 680 80 61 0 361 6 '(rounded) '(thickness 2)))

;; Row 2: Radius 62, thickness 2
(define test7 (img-arc img800x600 80 200 62 0 0.5 7 '(rounded) '(thickness 2)))
(define test8 (img-arc img800x600 200 200 62 0 179 8 '(rounded) '(thickness 2)))
(define test9 (img-arc img800x600 320 200 62 0 180 9 '(rounded) '(thickness 2)))
(define test10 (img-arc img800x600 440 200 62 0 181 10 '(rounded) '(thickness 2)))
(define test11 (img-arc img800x600 560 200 62 0 359 11 '(rounded) '(thickness 2)))
(define test12 (img-arc img800x600 680 200 62 0 361 12 '(rounded) '(thickness 2)))

;; Row 3: Radius 63, thickness 2
(define test13 (img-arc img800x600 80 320 63 0 0.5 13 '(rounded) '(thickness 2)))
(define test14 (img-arc img800x600 200 320 63 0 179 14 '(rounded) '(thickness 2)))
(define test15 (img-arc img800x600 320 320 63 0 180 15 '(rounded) '(thickness 2)))
(define test16 (img-arc img800x600 440 320 63 0 181 1 '(rounded) '(thickness 2)))
(define test17 (img-arc img800x600 560 320 63 0 359 2 '(rounded) '(thickness 2)))
(define test18 (img-arc img800x600 680 320 63 0 361 3 '(rounded) '(thickness 2)))

;; Section 2: Thickness 8 tests (offset to right side)
;; Row 1: Radius 61, thickness 8
(define test19 (img-arc img800x600 80 440 61 0 0.5 4 '(rounded) '(thickness 8)))
(define test20 (img-arc img800x600 200 440 61 0 179 5 '(rounded) '(thickness 8)))
(define test21 (img-arc img800x600 320 440 61 0 180 6 '(rounded) '(thickness 8)))
(define test22 (img-arc img800x600 440 440 61 0 181 7 '(rounded) '(thickness 8)))
(define test23 (img-arc img800x600 560 440 61 0 359 8 '(rounded) '(thickness 8)))
(define test24 (img-arc img800x600 680 440 61 0 361 9 '(rounded) '(thickness 8)))

;; Row 2: Radius 62, thickness 8
(define test25 (img-arc img800x600 80 520 62 0 0.5 10 '(rounded) '(thickness 8)))
(define test26 (img-arc img800x600 200 520 62 0 179 11 '(rounded) '(thickness 8)))
(define test27 (img-arc img800x600 320 520 62 0 180 12 '(rounded) '(thickness 8)))
(define test28 (img-arc img800x600 440 520 62 0 181 13 '(rounded) '(thickness 8)))
(define test29 (img-arc img800x600 560 520 62 0 359 14 '(rounded) '(thickness 8)))
(define test30 (img-arc img800x600 680 520 62 0 361 15 '(rounded) '(thickness 8)))

;; Section 3: Thickness 16 tests (using smaller image to fit)
;; Create additional tests with thickness 16 - these will be larger
;; Test with radius 63 and various angles
(define test31 (img-arc img800x600 100 50 63 0 0.5 1 '(rounded) '(thickness 16)))
(define test32 (img-arc img800x600 250 50 63 0 179 2 '(rounded) '(thickness 16)))
(define test33 (img-arc img800x600 400 50 63 0 180 3 '(rounded) '(thickness 16)))
(define test34 (img-arc img800x600 550 50 63 0 181 4 '(rounded) '(thickness 16)))
(define test35 (img-arc img800x600 700 50 63 0 359 5 '(rounded) '(thickness 16)))

;; Test different starting angles with rounded arcs
;; Test 36: Radius 61, start at 45 degrees, 180 degree arc, thickness 2
(define test36 (img-arc img800x600 50 150 61 45 225 6 '(rounded) '(thickness 2)))

;; Test 37: Radius 62, start at 90 degrees, 179 degree arc, thickness 8
(define test37 (img-arc img800x600 150 150 62 90 269 7 '(rounded) '(thickness 8)))

;; Test 38: Radius 63, start at 270 degrees, 181 degree arc, thickness 16
(define test38 (img-arc img800x600 350 150 63 270 451 8 '(rounded) '(thickness 16)))

;; Test very small angles with different thicknesses
;; Test 39: Radius 61, 0.1 degree arc, thickness 2
(define test39 (img-arc img800x600 450 150 61 0 0.1 9 '(rounded) '(thickness 2)))

;; Test 40: Radius 62, 0.1 degree arc, thickness 8
(define test40 (img-arc img800x600 550 150 62 0 0.1 10 '(rounded) '(thickness 8)))

;; Test 41: Radius 63, 0.1 degree arc, thickness 16
(define test41 (img-arc img800x600 650 150 63 0 0.1 11 '(rounded) '(thickness 16)))

;; Test edge cases around 360 degrees with different thicknesses
;; Test 42: Radius 61, exactly 360 degrees, thickness 2
(define test42 (img-arc img800x600 750 150 61 0 360 12 '(rounded) '(thickness 2)))

;; Test 43: Radius 62, exactly 360 degrees, thickness 8
(define test43 (img-arc img800x600 50 270 62 0 360 13 '(rounded) '(thickness 8)))

;; Test 44: Radius 63, exactly 360 degrees, thickness 16
(define test44 (img-arc img800x600 200 270 63 0 360 14 '(rounded) '(thickness 16)))

;; Test negative angles with rounded arcs
;; Test 45: Radius 61, negative start angle, thickness 2
(define test45 (img-arc img800x600 350 270 61 -45 135 15 '(rounded) '(thickness 2)))

;; Test 46: Radius 62, negative start angle, thickness 8  
(define test46 (img-arc img800x600 500 270 62 -90 90 1 '(rounded) '(thickness 8)))

;; Test 47: Radius 63, negative start angle, thickness 16
(define test47 (img-arc img800x600 650 270 63 -180 0 2 '(rounded) '(thickness 16)))

;; Test error conditions
;; Test 48: Invalid image buffer
(define test48 (trap (img-arc "invalid" 100 100 61 0 180 1 '(rounded) '(thickness 2))))

;; Test 49: negitive thickness is ok
(define test49 (img-arc img800x600 100 100 61 0 180 1 '(rounded) '(thickness -2)))

;; Test 50: Invalid color
(define test50 (trap (img-arc img800x600 100 100 61 0 180 "invalid" '(rounded) '(thickness 2))))

;; Test 51: Rounded with no thickness is fine!
(define test51 (img-arc img800x600 100 100 61 0 180 1 '(rounded)))

;; Test 52: Zero thickness is fine (defaults back to 1)
(define test52 (img-arc img800x600 100 100 61 0 180 1 '(rounded) '(thickness 0)))

;; Display the result with a 16-color palette
(disp-render img800x600 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))


;; (list test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
;;          test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
;;          test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
;;          test31 test32 test33 test34 test35 test36 test37 test38 test39 test40
;;          test41 test42 test43 test44 test45 test46 test47
;;          (eq test48 '(exit-error type_error))
;;          test49
;;          (eq test50 '(exit-error type_error))
;;          test51
;;          test52)

;; Check test results
;; Most drawing operations should succeed (return t)
;; Error conditions should return appropriate error types
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         test31 test32 test33 test34 test35 test36 test37 test38 test39 test40
         test41 test42 test43 test44 test45 test46 test47
         (eq test48 '(exit-error type_error))
         test49
         (eq test50 '(exit-error type_error))
         test51
         test52)
    (print "SUCCESS")
    (print "FAILURE"))
