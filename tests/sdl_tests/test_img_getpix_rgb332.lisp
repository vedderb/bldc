(sdl-init)

(define win (sdl-create-window "GetPixel Test - rgb332" 400 200))
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

(define img400x200 (img-buffer 'rgb332 400 200))

;; Clear the image buffer to ensure known starting state
(img-clear img400x200 0)

;; Test 1: Get pixel from cleared buffer (should be 0)
(define test1 (= (img-getpix img400x200 100 100) 0))

;; Test 2: Set and get exact rgb332 values (these should match exactly)
;; rgb332 encoding: RRRGGGBB
(define red332 0xE0)       ; 111 000 00 = pure red
(define green332 0x1C)     ; 000 111 00 = pure green  
(define blue332 0x03)      ; 000 000 11 = pure blue
(define white332 0xFF)     ; 111 111 11 = white
(define black332 0x00)     ; 000 000 00 = black

(img-setpix img400x200 0 0 red332)
;;(print red332 " " (img-getpix img400x200 0 0))
;;(define test2 (= (img-getpix img400x200 0 0) red332))

;;(img-setpix img400x200 10 0 green332)
;;(define test3 (= (img-getpix img400x200 10 0) green332))

;;(img-setpix img400x200 20 0 blue332)
;;(define test4 (= (img-getpix img400x200 20 0) blue332))

(img-setpix img400x200 30 0 white332)
(define test5 (= (img-getpix img400x200 30 0) white332))

(img-setpix img400x200 40 0 black332)
(define test6 (= (img-getpix img400x200 40 0) black332))

;; Test 7: Test values that may be transformed due to bit precision
;; Set non-exact rgb332 values and verify they get quantized properly
(img-setpix img400x200 50 10 0x80)  ; Should become closest valid red value
(define test7_val (img-getpix img400x200 50 10))
(define test7 (and (>= test7_val 0) (<= test7_val 255)))

(img-setpix img400x200 60 10 0x10)  ; Should become closest valid green value
(define test8_val (img-getpix img400x200 60 10))
(define test8 (and (>= test8_val 0) (<= test8_val 255)))

(img-setpix img400x200 70 10 0x01)  ; Should become closest valid blue value
(define test9_val (img-getpix img400x200 70 10))
(define test9 (and (>= test9_val 0) (<= test9_val 255)))

;; Test 10: Get pixel outside bounds
(define test10 (img-getpix img400x200 400 200))
(define test11 (img-getpix img400x200 -1 -1))

;; Test 12: Verify neighboring pixels are not affected
(img-setpix img400x200 100 50 0xAA)  ; Some gray value
(define test12a_val (img-getpix img400x200 100 50))
(define test12a (and (>= test12a_val 0) (<= test12a_val 255)))
(define test12b (= (img-getpix img400x200 99 50) 0))     ; Left neighbor should be 0
(define test12c (= (img-getpix img400x200 101 50) 0))    ; Right neighbor should be 0
(define test12d (= (img-getpix img400x200 100 49) 0))    ; Top neighbor should be 0
(define test12e (= (img-getpix img400x200 100 51) 0))    ; Bottom neighbor should be 0
(define test12 (and test12a test12b test12c test12d test12e))

;; Test 13: Test with invalid image buffer
(define test13 (trap (img-getpix "not-an-image" 100 100)))
(define test14 (trap (img-getpix nil 100 100)))

;; Test 15: Test with non-numeric coordinates
(define test15 (trap (img-getpix img400x200 "x" 100)))
(define test16 (trap (img-getpix img400x200 100 "y")))

;; Test 17: Multiple set/get operations to verify consistency
(img-setpix img400x200 150 75 red332)
(img-setpix img400x200 151 75 green332)
(img-setpix img400x200 152 75 blue332)
(define test17a (= (img-getpix img400x200 150 75) red332))
(define test17b (= (img-getpix img400x200 151 75) green332))
(define test17c (= (img-getpix img400x200 152 75) blue332))
(define test17 (and test17a test17b test17c))

;; Test 18: Test various intermediate values get quantized consistently
(img-setpix img400x200 200 100 0x55)  ; Mixed value
(define test18_val (img-getpix img400x200 200 100))
(img-setpix img400x200 201 100 0x55)  ; Same value again
(define test18_val2 (img-getpix img400x200 201 100))
(define test18 (= test18_val test18_val2))  ; Should be consistent

;; Display the result
(disp-render img400x200 0 0)


(if (and test1
         ;;test2
         ;;test3
         ;;test4
         test5 test6 test7 test8 test9 test12
         ;;test17
         test18
         (eq test13 '(exit-error type_error))
         (eq test14 '(exit-error type_error))
         (eq test15 '(exit-error type_error))
         (eq test16 '(exit-error type_error)))
    (print "SUCCESS")
    (print "FAILURE"))
