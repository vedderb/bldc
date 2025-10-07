;; Test cases for array extensions endianness functionality
;; These tests target the little-endian/big-endian code paths

(hide-trapped-error)

(define debug_test (lambda (x i)
                     (if (not x) (print "TEST " i " FAILED: " x))))

;; Create a test buffer for operations
(define test-buf (bufcreate 32))

;; Test bufset operations with little-endian flag

;; Test bufset-i16 with little-endian
(define r1 (and (bufset-i16 test-buf 0 0x1234 'little-endian)
               (= (bufget-u8 test-buf 0) 0x34)
               (= (bufget-u8 test-buf 1) 0x12)))
(debug_test r1 1)

;; Test bufset-i16 with big-endian (default)
(define r2 (and (bufset-i16 test-buf 2 0x1234)
               (= (bufget-u8 test-buf 2) 0x12)
               (= (bufget-u8 test-buf 3) 0x34)))
(debug_test r2 2)

;; Test bufset-i32 with little-endian
(define r3 (and (bufset-i32 test-buf 4 0x12345678u32 'little-endian)
               (= (bufget-u8 test-buf 4) 0x78)
               (= (bufget-u8 test-buf 5) 0x56)
               (= (bufget-u8 test-buf 6) 0x34)
               (= (bufget-u8 test-buf 7) 0x12)))
(debug_test r3 3)

;; Test bufset-i32 with big-endian (default)
(define r4 (and (bufset-i32 test-buf 8 0x12345678u32)
               (= (bufget-u8 test-buf 8) 0x12)
               (= (bufget-u8 test-buf 9) 0x34)
               (= (bufget-u8 test-buf 10) 0x56)
               (= (bufget-u8 test-buf 11) 0x78)))
(debug_test r4 4)

;; Test bufset-u16 with little-endian
(define r5 (and (bufset-u16 test-buf 12 0xABCD 'little-endian)
               (= (bufget-u8 test-buf 12) 0xCD)
               (= (bufget-u8 test-buf 13) 0xAB)))
(debug_test r5 5)

;; Test bufset-u16 with big-endian (default)
(define r6 (and (bufset-u16 test-buf 14 0xABCD)
               (= (bufget-u8 test-buf 14) 0xAB)
               (= (bufget-u8 test-buf 15) 0xCD)))
(debug_test r6 6)

;; Test bufset-u24 with little-endian
(define r7 (and (bufset-u24 test-buf 16 0x123456 'little-endian)
               (= (bufget-u8 test-buf 16) 0x56)
               (= (bufget-u8 test-buf 17) 0x34)
               (= (bufget-u8 test-buf 18) 0x12)))
(debug_test r7 7)

;; Test bufset-u24 with big-endian (default)
(define r8 (and (bufset-u24 test-buf 19 0x123456)
               (= (bufget-u8 test-buf 19) 0x12)
               (= (bufget-u8 test-buf 20) 0x34)
               (= (bufget-u8 test-buf 21) 0x56)))
(debug_test r8 8)

;; Test bufset-u32 with little-endian
(define r9 (and (bufset-u32 test-buf 22 0x12345678u32 'little-endian)
               (= (bufget-u8 test-buf 22) 0x78)
               (= (bufget-u8 test-buf 23) 0x56)
               (= (bufget-u8 test-buf 24) 0x34)
               (= (bufget-u8 test-buf 25) 0x12)))
(debug_test r9 9)

;; Test bufset-u32 with big-endian (default)
(define r10 (and (bufset-u32 test-buf 26 0x12345678u32)
                (= (bufget-u8 test-buf 26) 0x12)
                (= (bufget-u8 test-buf 27) 0x34)
                (= (bufget-u8 test-buf 28) 0x56)
                (= (bufget-u8 test-buf 29) 0x78)))
(debug_test r10 10)

;; Test bufset-f32 with little-endian
(define test-buf2 (bufcreate 16))
(define r11 (and (bufset-f32 test-buf2 0 3.14159f32 'little-endian)
                (bufset-f32 test-buf2 4 3.14159f32)
                (= (bufget-u32 test-buf2 0 'little-endian)
                   (bufget-u32 test-buf2 4))))
(debug_test r11 11)

;; Test bufget operations with endianness

;; Set up test data for gets
(define get-buf (bufcreate 16))
(bufset-u8 get-buf 0 0x12)
(bufset-u8 get-buf 1 0x34)
(bufset-u8 get-buf 2 0x56) 
(bufset-u8 get-buf 3 0x78)
(bufset-u8 get-buf 4 0x9A)
(bufset-u8 get-buf 5 0xBC)
(bufset-u8 get-buf 6 0xDE)
(bufset-u8 get-buf 7 0xF0)

;; Test bufget-i16 with little-endian vs big-endian
(define r12 (and (= (bufget-i16 get-buf 0 'little-endian) 0x3412)
                (= (bufget-i16 get-buf 0) 0x1234)))
(debug_test r12 12)

;; Test bufget-i32 with little-endian vs big-endian  
(define r13 (and (= (bufget-i32 get-buf 0 'little-endian) 0x78563412)
                (= (bufget-i32 get-buf 0) 0x12345678)))
(debug_test r13 13)

;; Test bufget-u16 with little-endian vs big-endian
(define r14 (and (= (bufget-u16 get-buf 0 'little-endian) 0x3412)
                (= (bufget-u16 get-buf 0) 0x1234)))
(debug_test r14 14)

;; Test bufget-u24 with little-endian vs big-endian
(define r15 (and (= (bufget-u24 get-buf 0 'little-endian) 0x563412)
                (= (bufget-u24 get-buf 0) 0x123456)))
(debug_test r15 15)

;; Test bufget-u32 with little-endian vs big-endian
(define r16 (and (= (bufget-u32 get-buf 0 'little-endian) 0x78563412u32)
                (= (bufget-u32 get-buf 0) 0x12345678u32)))
(debug_test r16 16)

;; Test bufget-f32 with different endianness
(define f32-buf (bufcreate 8))
(bufset-f32 f32-buf 0 1.5f32 'little-endian)
(bufset-f32 f32-buf 4 1.5f32)
(define r17 (not (= (bufget-f32 f32-buf 0 'little-endian)
                   (bufget-f32 f32-buf 4 'little-endian))))
(debug_test r17 17)

;; Test that 'big-endian symbol also works (should be same as default)
(define r18 (= (bufget-u16 get-buf 0 'big-endian)
              (bufget-u16 get-buf 0)))
(debug_test r18 18)

;; Test invalid endianness symbol (should use default)
(define r19 (= (bufget-u16 get-buf 0 'invalid-endian)
              (bufget-u16 get-buf 0)))
(debug_test r19 19)

(if (and r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19)
    (print "SUCCESS")
    (print "FAILURE"))
