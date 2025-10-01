;; Test cases targeting ENC_SYM_TERROR returns in string extensions
;; These tests specifically target uncovered type error paths

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " " x))))

;; Test str-from-n with non-number first argument
(define r1 (eq (trap (str-from-n 'not-a-number)) '(exit-error type_error)))

(debug_test r1 1)

;; Test str-from-n with non-array second argument when provided
(define r2 (eq (trap (str-from-n 65 'not-an-array)) '(exit-error type_error)))

(debug_test r2 2)

;; Test str-join with non-list first argument
(define r3 (eq (trap (str-join 'not-a-list)) '(exit-error type_error)))

(debug_test r3 3)

;; Test str-join with list containing non-string elements
(define r4 (eq (trap (str-join '("hello" 42 "world"))) '(exit-error type_error)))

(debug_test r4 4)

;; Test str-join with non-string separator (second arg)
(define r5 (eq (trap (str-join '("hello" "world") 42)) '(exit-error type_error)))

(debug_test r5 5)

;; Test str-to-i with non-string argument
(define r6 (eq (trap (str-to-i 42)) '(exit-error type_error)))

(debug_test r6 6)

;; Test str-to-i with non-number base argument
(define r7 (eq (trap (str-to-i "123" 'not-a-number)) '(exit-error type_error)))

(debug_test r7 7)

;; Test str-to-f with non-string argument
(define r8 (eq (trap (str-to-f 42)) '(exit-error type_error)))

(debug_test r8 8)

;; Test str-part with non-number second argument
(define r9 (eq (trap (str-part "hello" 'not-a-number)) '(exit-error type_error)))

(debug_test r9 9)

;; Test str-part with non-string first argument
(define r10 (eq (trap (str-part 42 1)) '(exit-error type_error)))

(debug_test r10 10)

;; Test str-part with non-number third argument
(define r11 (eq (trap (str-part "hello" 1 'not-a-number)) '(exit-error type_error)))

(debug_test r11 11)

;; Test str-split with wrong number of arguments
(define r12 (eq (trap (str-split "hello")) '(exit-error type_error)))

(debug_test r12 12)

;; Test str-split with non-string first argument
(define r13 (eq (trap (str-split 42 " ")) '(exit-error type_error)))

(debug_test r13 13)

;; Test str-split with non-string second argument (delimiter)
(define r14 (eq (ix (str-split "hello world" 42) 0) "hello world"))

(debug_test r14 14)

;; Test str-replace with non-string first argument
(define r15 (eq (trap (str-replace 42 "old" "new")) '(exit-error type_error)))

(debug_test r15 15)

;; Test str-replace with non-string second argument (pattern to replace)
(define r16 (eq (trap (str-replace "hello" 42 "new")) '(exit-error type_error)))

(debug_test r16 16)

;; Test str-replace with non-string third argument (replacement)
(define r17 (eq (trap (str-replace "hello" "old" 42)) '(exit-error type_error)))

(debug_test r17 17)

;; Test str-to-lower with non-string argument
(define r18 (eq (trap (str-to-lower 42)) '(exit-error type_error)))

(debug_test r18 18)

;; Test str-to-upper with non-string argument
(define r19 (eq (trap (str-to-upper 42)) '(exit-error type_error)))

(debug_test r19 19)

;; Test str-cmp with non-string first argument
(define r20 (eq (trap (str-cmp 42 "hello")) '(exit-error type_error)))

(debug_test r20 20)

;; Test str-cmp with non-string second argument
(define r21 (eq (trap (str-cmp "hello" 42)) '(exit-error type_error)))

(debug_test r21 21)

;; Test str-cmp with non-number third argument (length limit)
(define r22 (eq (trap (str-cmp "hello" "world" 'not-a-number)) '(exit-error type_error)))

(debug_test r22 22)

;; Test str-len with non-string argument
(define r23 (eq (trap (str-len 42)) '(exit-error type_error)))

(debug_test r23 23)

;; Test str-replicate with non-number first argument
(define r24 (eq (trap (str-replicate 'not-a-number 5)) '(exit-error type_error)))

(debug_test r24 24)

;; Test str-replicate with non-number second argument
(define r25 (eq (trap (str-replicate "hello" 'not-a-number)) '(exit-error type_error)))

(debug_test r25 25)

;; Test str-find with too few arguments (< 2)
(define r26 (eq (trap (str-find "hello")) '(exit-error eval_error)))

(debug_test r26 26)

;; Test str-find with too many arguments (> 6)
(define r27 (eq (trap (str-find "hello" "e" 0 5 t t t)) '(exit-error eval_error)))

(debug_test r27 27)

;; Test to-str-delim with no arguments
(define r28 (eq (trap (to-str-delim)) '(exit-error eval_error)))

(debug_test r28 28)

;; Test to-str-delim with non-string delimiter
(define r29 (eq (ix (trap (to-str-delim 42 "hello")) 0) 'exit-error))

(debug_test r29 29)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29)
    (print "SUCCESS")
    (print "FAILURE"))
