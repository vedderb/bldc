(sdl-init)

(define win (sdl-create-window "Filled Arcs Test" 400 200))
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

(define img400x200 (img-buffer 'indexed16 400 200))

;; Clear the image buffer with black background
(img-clear img400x200 0)

;; Test 1: Filled arc using img-arc
(define test1 (img-arc img400x200 60 60 25 0 180 1 '(filled)))

;; Test 2: Another filled arc with different angles
(define test2 (img-arc img400x200 160 60 20 45 225 2 '(filled)))

;; Test 3: Filled arc covering more than 180 degrees
(define test3 (img-arc img400x200 260 60 22 30 270 3 '(filled)))

;; Test 4: Nearly full circle filled arc
(define test4 (img-arc img400x200 340 60 18 10 350 4 '(filled)))

;; Test 5: Filled circle (360 degree arc)
(define test5 (img-circle img400x200 60 140 25 5 '(filled)))

;; Test 6: Filled circle sector (pie slice)
(define test6 (img-circle-sector img400x200 160 140 20 0 90 6 '(filled)))

;; Test 7: Another filled sector with different angle
(define test7 (img-circle-sector img400x200 260 140 18 45 180 7 '(filled)))

;; Test 8: Filled circle segment (chord area)
(define test8 (img-circle-segment img400x200 340 140 16 0 120 8 '(filled)))

;; Test 9: Small filled arc
(define test9 (img-arc img400x200 30 30 10 0 90 9 '(filled)))

;; Test 10: Filled arc with negative start angle
(define test10 (img-arc img400x200 370 30 12 -45 45 10 '(filled)))

;; Test 11: Filled arc with angles > 360
(define test11 (img-arc img400x200 30 170 14 0 450 11 '(filled)))

;; Test 12: Very small filled arc
(define test12 (img-arc img400x200 370 170 8 60 120 12 '(filled)))

;; Test edge cases and error conditions
;; Test 13: Zero radius filled arc
(define test13 (img-arc img400x200 100 100 0 0 90 13 '(filled)))

;; Test 14: Invalid image buffer
(define test14 (trap (img-arc "invalid" 100 100 20 0 90 1 '(filled))))

;; Test 15: Invalid color
(define test15 (trap (img-arc img400x200 100 100 20 0 90 "invalid" '(filled))))

;; Display the result
(disp-render img400x200 0 0 '(0x000000 0x222222 0x444444 0x666666 0x888888 0xAAAAAA 0xCCCCCC 0xEEEEEE
                               0x111111 0x333333 0x555555 0x777777 0x999999 0xBBBBBB 0xDDDDDD 0xFFFFFF))

;; Check test results
;; Tests 1-13 should succeed (return t)
;; Tests 14-15 should return error conditions
(if (and test1 test2 test3 test4 test5 test6 test7 test8 test9 test10 
         test11 test12 test13
         (eq test14 '(exit-error type_error))
         (eq test15 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))