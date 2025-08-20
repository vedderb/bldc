(sdl-init)

(define win (sdl-create-window "Filled Arcs Edge Cases Test" 600 400))
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

;; Test filled arcs with specific radii: 61, 62, 63
;; These radii might reveal edge cases in the arc filling algorithm

;; Row 1: Radius 61 tests
;; Test 1: 0.5 degrees (very small arc)
(define test1 (img-arc img600x400 80 80 61 0 0.5 1 '(filled)))

;; Test 2: 179 degrees (just under semicircle)
(define test2 (img-arc img600x400 200 80 61 0 179 2 '(filled)))

;; Test 3: 180 degrees (exact semicircle)
(define test3 (img-arc img600x400 320 80 61 0 180 3 '(filled)))

;; Test 4: 181 degrees (just over semicircle)
(define test4 (img-arc img600x400 440 80 61 0 181 4 '(filled)))

;; Test 5: 359 degrees (almost full circle)
(define test5 (img-arc img600x400 520 80 61 0 359 5 '(filled)))

;; Row 2: Radius 62 tests
;; Test 6: 0.5 degrees
(define test6 (img-arc img600x400 80 200 62 0 0.5 6 '(filled)))

;; Test 7: 179 degrees
(define test7 (img-arc img600x400 200 200 62 0 179 7 '(filled)))

;; Test 8: 180 degrees
(define test8 (img-arc img600x400 320 200 62 0 180 8 '(filled)))

;; Test 9: 181 degrees
(define test9 (img-arc img600x400 440 200 62 0 181 9 '(filled)))

;; Test 10: 359 degrees
(define test10 (img-arc img600x400 520 200 62 0 359 10 '(filled)))

;; Row 3: Radius 63 tests
;; Test 11: 0.5 degrees
(define test11 (img-arc img600x400 80 320 63 0 0.5 11 '(filled)))

;; Test 12: 179 degrees
(define test12 (img-arc img600x400 200 320 63 0 179 12 '(filled)))

;; Test 13: 180 degrees
(define test13 (img-arc img600x400 320 320 63 0 180 13 '(filled)))

;; Test 14: 181 degrees
(define test14 (img-arc img600x400 440 320 63 0 181 14 '(filled)))

;; Test 15: 359 degrees
(define test15 (img-arc img600x400 520 320 63 0 359 15 '(filled)))

;; Test 16: 361 degrees (over full circle) with radius 61
(define test16 (img-arc img600x400 50 50 61 0 361 1 '(filled)))

;; Test 17: 361 degrees with radius 62
(define test17 (img-arc img600x400 150 50 62 0 361 2 '(filled)))

;; Test 18: 361 degrees with radius 63
(define test18 (img-arc img600x400 250 50 63 0 361 3 '(filled)))

;; Test different starting angles with critical radii
;; Test 19: Radius 61, start at 45 degrees, 180 degree arc
(define test19 (img-arc img600x400 350 50 61 45 225 4 '(filled)))

;; Test 20: Radius 62, start at 90 degrees, 179 degree arc
(define test20 (img-arc img600x400 450 50 62 90 269 5 '(filled)))

;; Test 21: Radius 63, start at 270 degrees, 181 degree arc
(define test21 (img-arc img600x400 550 50 63 270 451 6 '(filled)))

;; Test very small angles with different radii
;; Test 22: Radius 61, 0.1 degree arc
(define test22 (img-arc img600x400 50 150 61 0 0.1 7 '(filled)))

;; Test 23: Radius 62, 0.1 degree arc
(define test23 (img-arc img600x400 150 150 62 0 0.1 8 '(filled)))

;; Test 24: Radius 63, 0.1 degree arc
(define test24 (img-arc img600x400 250 150 63 0 0.1 9 '(filled)))

;; Test edge cases around 360 degrees
;; Test 25: Radius 61, exactly 360 degrees
(define test25 (img-arc img600x400 350 150 61 0 360 10 '(filled)))

;; Test 26: Radius 62, exactly 360 degrees
(define test26 (img-arc img600x400 450 150 62 0 360 11 '(filled)))

;; Test 27: Radius 63, exactly 360 degrees
(define test27 (img-arc img600x400 550 150 63 0 360 12 '(filled)))

;; Test negative angles
;; Test 28: Radius 61, negative start angle
(define test28 (img-arc img600x400 50 250 61 -45 135 13 '(filled)))

;; Test 29: Radius 62, negative start angle
(define test29 (img-arc img600x400 150 250 62 -90 90 14 '(filled)))

;; Test 30: Radius 63, negative start angle
(define test30 (img-arc img600x400 250 250 63 -180 0 15 '(filled)))

;; Test error conditions
;; Test 31: Invalid image buffer
(define test31 (trap (img-arc "invalid" 100 100 61 0 180 1 '(filled))))

;; Test 32: Invalid radius (negative)
(define test32 (img-arc img600x400 100 100 -61 0 180 1 '(filled)))

;; Test 33: Invalid color
(define test33 (trap (img-arc img600x400 100 100 61 0 180 "invalid" '(filled))))

;; Display the result with a 16-color palette
(disp-render img600x400 0 0 '(0x000000 0xFF0000 0x00FF00 0x0000FF 
                               0xFFFF00 0xFF00FF 0x00FFFF 0xFFFFFF
                               0x800000 0x008000 0x000080 0x808000
                               0x800080 0x008080 0x808080 0xC0C0C0))

;; (list test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
;;          test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
;;          test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
;;          test32
;;          (eq test31 '(exit-error type_error))
;;          (eq test33 '(exit-error type_error)))

;; Check test results
;; Most drawing operations should succeed (return t)
;; Error conditions should return appropriate error types
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10
         test11 test12 test13 test14 test15 test16 test17 test18 test19 test20
         test21 test22 test23 test24 test25 test26 test27 test28 test29 test30
         (eq test31 '(exit-error type_error))
         test32
         (eq test33 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
