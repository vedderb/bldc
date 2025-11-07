

(define r1 (eq (shl 1b 1) 2b))
(define r2 (eq (shr 1b 1) 0b))

;; TODO: Hex notation for byte literals is not possible
;;       for totally obvious reasons...
(define r3 (eq (bitwise-and (to-byte 0x55) (to-byte 0xAA)) 0b))
(define r4 (eq (bitwise-or  (to-byte 0x55) (to-byte 0xAA)) 255b))

(define r5 (eq (bitwise-xor (to-byte 0xFF) (to-byte 0xAA)) (to-byte 0x55)))
(define r6 (eq (bitwise-not (to-byte 0x55)) (to-byte 0xAA)))


(if (and r1 r2 r3 r4 r5 r6) (print "SUCCESS")
    (print "FAILURE"))
