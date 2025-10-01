;; Test cases targeting ENC_SYM_EERROR and ENC_SYM_TERROR returns in array extensions
;; These tests specifically target uncovered error handling paths

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

;; Create a test buffer for operations
(define test-buf (bufcreate 32))

;; Test free with wrong argument count (no args)
(define r1 (eq (trap (free)) '(exit-error eval_error)))
(debug_test r1 1)

;; Test free with wrong argument count (too many args)  
(define r2 (eq (trap (free test-buf "extra")) '(exit-error eval_error)))
(debug_test r2 2)

;; Test free with wrong argument type (not an array)
(define r3 (eq (trap (free 42)) '(exit-error type_error)))
(debug_test r3 3)

;; Test free with read-only array (should succeed but return nil)

@const-start
(define read-only-str "readonly")
@const-end

(define r4 (eq (trap (free read-only-str)) '(exit-error type_error)))
(debug_test r4 4)

;; Test bufset operations with wrong argument counts
;; bufset functions need 3 or 4 args (buffer, index, value, [endianness])

;; Test bufset-i8 with wrong argument count (too few)
(define r5 (eq (ix (trap (bufset-i8 test-buf)) 0) 'exit-error))
(debug_test r5 5)

;; Test bufset-i8 with wrong argument count (too many)
(define r6 (eq (ix (trap (bufset-i8 test-buf 0 42 'little-endian "extra")) 0) 'exit-error))
(debug_test r6 6)

;; Test bufset-i8 with non-array first argument
(define r7 (eq (trap (bufset-i8 42 0 100)) '(exit-error type_error)))
(debug_test r7 7)

;; Test bufset-i8 with non-number index
(define r8 (eq (trap (bufset-i8 test-buf 'not-a-number 100)) '(exit-error type_error)))
(debug_test r8 8)

;; Test bufset-i8 with non-number value
(define r9 (eq (trap (bufset-i8 test-buf 0 'not-a-number)) '(exit-error type_error)))
(debug_test r9 9)

;; Test "a string is a buffer". This is fine.
(define r10 (eq (bufset-i16 "not-array" 0 100) t))
(debug_test r10 10)

;; Test bufset-i32 with wrong types  
(define r11 (eq (trap (bufset-i32 test-buf 'not-index 100)) '(exit-error type_error)))
(debug_test r11 11)

;; Test bufset-u8 with wrong types
(define r12 (eq (trap (bufset-u8 test-buf 0 'not-value)) '(exit-error type_error)))
(debug_test r12 12)

;; Test bufset-u16 with wrong argument count
(define r13 (eq (ix (trap (bufset-u16 test-buf)) 0) 'exit-error))
(debug_test r13 13)

;; Test bufset-u24 with wrong types
(define r14 (eq (trap (bufset-u24 42 0 100)) '(exit-error type_error)))
(debug_test r14 14)

;; Test bufset-u32 with wrong types
(define r15 (eq (trap (bufset-u32 test-buf 'not-number 100)) '(exit-error type_error)))
(debug_test r15 15)

;; Test bufset-f32 with wrong types
(define r16 (eq (trap (bufset-f32 test-buf 0 'not-float)) '(exit-error type_error)))
(debug_test r16 16)

;; Test bufget operations with wrong argument counts and types
;; bufget functions need 2 or 3 args (buffer, index, [endianness])

;; Test bufget-i8 with wrong argument count (no args)
(define r17 (eq (ix (trap (bufget-i8)) 0) 'exit-error))
(debug_test r17 17)

;; Test bufget-i8 with wrong argument count (too many)
(define r18 (eq (ix (trap (bufget-i8 test-buf 0 'little-endian "extra")) 0) 'exit-error))
(debug_test r18 18)

;; Test bufget-i8 with non-array first argument
(define r19 (eq (trap (bufget-i8 42 0)) '(exit-error type_error)))
(debug_test r19 19)

;; Test bufget-i8 with non-number index
(define r20 (eq (trap (bufget-i8 test-buf 'not-a-number)) '(exit-error type_error)))
(debug_test r20 20)

;; Test "string is buffer" - this is OK! 
(define r21 (eq (bufget-i16 "not-array" 0) 28271))
(debug_test r21 21)

;; Test bufget-i32 with wrong types
(define r22 (eq (trap (bufget-i32 test-buf 'not-index)) '(exit-error type_error)))
(debug_test r22 22)

;; Test bufget-u8 with wrong types
(define r23 (eq (trap (bufget-u8 42 0)) '(exit-error type_error)))
(debug_test r23 23)

;; Test bufget-u16 with wrong argument count
(define r24 (eq (ix (trap (bufget-u16)) 0) 'exit-error))
(debug_test r24 24)

;; Test bufget-u24 with wrong types
(define r25 (eq (trap (bufget-u24 test-buf 'not-number)) '(exit-error type_error)))
(debug_test r25 25)

;; Test "string is buffer" - This is ok!
(define r26 (eq (bufget-u32 "not-buffer" 0) 1852797997u32))
(debug_test r26 26)

;; Test bufget-f32 with wrong types
(define r27 (eq (trap (bufget-f32 test-buf 'not-index)) '(exit-error type_error)))
(debug_test r27 27)

;; Test buflen with wrong argument count (no args)
(define r28 (eq (ix (trap (buflen)) 0) 'exit-error))
(debug_test r28 28)

;; Test buflen with wrong argument type
(define r29 (eq (trap (buflen 42)) '(exit-error eval_error)))
(debug_test r29 29)

;; Test bufclear with wrong argument count (no args)
(define r30 (eq (ix (trap (bufclear)) 0) 'exit-error))
(debug_test r30 30)

;; Test bufclear with wrong argument count (too many args)
(define r31 (eq (ix (trap (bufclear test-buf 0 0 0 "extra")) 0) 'exit-error))
(debug_test r31 31)

;; Test bufclear with non-array first argument
(define r32 (eq (trap (bufclear 42)) '(exit-error type_error)))
(debug_test r32 32)

;; Test bufclear with non-number clear byte
(define r33 (eq (trap (bufclear test-buf 'not-number)) '(exit-error type_error)))
(debug_test r33 33)

;; Test bufclear with non-number start position
(define r34 (eq (trap (bufclear test-buf 0 'not-number)) '(exit-error type_error)))
(debug_test r34 34)

;; Test bufclear with start position beyond buffer size
(define r35 (eq (trap (bufclear test-buf 0 100)) '(exit-error type_error)))
(debug_test r35 35)

;; Test bufclear with non-number length
(define r36 (eq (trap (bufclear test-buf 0 0 'not-number)) '(exit-error type_error)))
(debug_test r36 36)

;; Test bufcpy with wrong argument count (not exactly 5)
(define r37 (eq (ix (trap (bufcpy test-buf)) 0) 'exit-error))
(debug_test r37 37)

;; Test bufset-bit with wrong argument count
(define r38 (eq (ix (trap (bufset-bit test-buf)) 0) 'exit-error))
(debug_test r38 38)

;; Test bufset-bit with non-array first argument
(define r39 (eq (trap (bufset-bit 42 0 1)) '(exit-error type_error)))
(debug_test r39 39)

;; Test bufset-bit with non-number position
(define r40 (eq (trap (bufset-bit test-buf 'not-number 1)) '(exit-error type_error)))
(debug_test r40 40)

;; Test bufset-bit with non-number bit value
(define r41 (eq (trap (bufset-bit test-buf 0 'not-number)) '(exit-error type_error)))
(debug_test r41 41)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 
         r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32 r33 r34 r35 r36 r37 r38 r39 r40 r41)
    (print "SUCCESS")
    (print "FAILURE"))
