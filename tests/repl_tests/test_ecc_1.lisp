
(hide-trapped-error)

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))


; --- helpers ---

; corrupt a buffer by XOR-ing a byte at a given position
(defun corrupt (buf pos val)
  (progn
    (bufset-u8 buf pos (bitwise-xor (bufget-u8 buf pos) val))
    buf))


; --- rs-encoded-size ---

; valid calls
(define t1  (= (rs-encoded-size 100 4)  104))
(define t2  (= (rs-encoded-size 100 32) 132))
(define t3  (= (rs-encoded-size 223 32) 255))
(define t4  (= (rs-encoded-size 1   2)    3))

; total > 255 must fail
(define t5  (eq (trap (rs-encoded-size 224 32)) '(exit-error eval_error)))

; nroots odd must fail
(define t6  (eq (trap (rs-encoded-size 100 3))  '(exit-error eval_error)))

; nroots < 2 must fail
(define t7  (eq (trap (rs-encoded-size 100 0))  '(exit-error eval_error)))
(define t8  (eq (trap (rs-encoded-size 100 1))  '(exit-error eval_error)))

; wrong types
(define t9  (eq (trap (rs-encoded-size "x" 4)) '(exit-error type_error)))
(define t10 (eq (trap (rs-encoded-size 100 "x")) '(exit-error type_error)))


;;  The starting buffer

(define obuf [72 101 108 108 111 32 87 111 114 108])

(defun payload-eq (buf1 buf2 payload-size) {
       (var ok t)
       (loopfor i 0 (< i payload-size) (+ i 1)
                (if (!= (bufget-u8 buf1 i) (bufget-u8 buf2 i))
                    (setq ok nil)
                  ()))
       ok
       })

;; Encode - decode

(define t11
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufcpy buf 0 obuf 0 (length obuf))
      (rs-encode buf 4)
      (var n-fixed (rs-decode buf 4))
      (print n-fixed)
      (print buf)
      (and (payload-eq buf obuf (length obuf) (= n-fixed 0))))))

;; Encode - corrupt - decode

(define t12
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufcpy buf 0 obuf 0 (length obuf))
      (rs-encode buf 4)
      (corrupt buf 3 0xff)     ; trash payload byte 3

      (var n-fixed (rs-decode buf 4))
      (print n-fixed)
      (print buf)
      (and (payload-eq buf obuf (length obuf) (= n-fixed 1))))))


;; payload byte 3 should be restored to 108
(define t13
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufcpy buf 0 obuf 0 (length obuf))
      (rs-encode buf 4)
      (corrupt buf 3 0xff)
      (rs-decode buf 4)
      (= (bufget-u8 buf 3) 108))))

;; Corruption in the ECC byte area
(define t14
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufset-u8 buf 0 1)
      (bufset-u8 buf 1 2)
      (bufset-u8 buf 2 3)
      (bufset-u8 buf 3 4)
      (bufset-u8 buf 4 5)
      (bufset-u8 buf 5 6)
      (bufset-u8 buf 6 7)
      (bufset-u8 buf 7 8)
      (bufset-u8 buf 8 9)
      (bufset-u8 buf 9 10)
      (rs-encode buf 4)
      (corrupt buf 11 0xab)    ; trash an ECC byte (index 11 = payload[10+1])
      (= (rs-decode buf 4) 1))))

; --- correct two errors with nroots=4 ---

(define t15
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufset-u8 buf 0 10)
      (bufset-u8 buf 1 20)
      (bufset-u8 buf 2 30)
      (bufset-u8 buf 3 40)
      (bufset-u8 buf 4 50)
      (bufset-u8 buf 5 60)
      (bufset-u8 buf 6 70)
      (bufset-u8 buf 7 80)
      (bufset-u8 buf 8 90)
      (bufset-u8 buf 9 100)
      (rs-encode buf 4)
      (corrupt buf 0 0x55)     ; two errors
      (corrupt buf 9 0xaa)
      (= (rs-decode buf 4) 2))))

; --- three errors with nroots=4 exceeds capacity, should return eval-error ---

(define t16
  (let ((buf (bufcreate (rs-encoded-size 10 4))))
    (progn
      (bufset-u8 buf 0 10)
      (bufset-u8 buf 1 20)
      (bufset-u8 buf 2 30)
      (bufset-u8 buf 3 40)
      (bufset-u8 buf 4 50)
      (bufset-u8 buf 5 60)
      (bufset-u8 buf 6 70)
      (bufset-u8 buf 7 80)
      (bufset-u8 buf 8 90)
      (bufset-u8 buf 9 100)
      (rs-encode buf 4)
      (corrupt buf 0 0x11)
      (corrupt buf 4 0x22)
      (corrupt buf 9 0x33)
      (< (rs-decode buf 4) 0))))

; --- nroots=32: correct up to 16 errors, verify one ---

(define t17
  (let ((buf (bufcreate (rs-encoded-size 50 32))))
    (progn
      (loop ((i 0)) (< i 50) (progn (bufset-u8 buf i i) (setq i (+ i 1))))
      (rs-encode buf 32)
      (corrupt buf 0  0x01)
      (corrupt buf 5  0x02)
      (corrupt buf 10 0x04)
      (corrupt buf 15 0x08)
      (corrupt buf 20 0x10)
      (corrupt buf 25 0x20)
      (corrupt buf 30 0x40)
      (corrupt buf 35 0x80)
      (= (rs-decode buf 32) 8))))

; --- verify payload is intact after multi-error correction ---

(define t18
  (let ((buf (bufcreate (rs-encoded-size 10 8))))
    (progn
      (bufset-u8 buf 0 0xde)
      (bufset-u8 buf 1 0xad)
      (bufset-u8 buf 2 0xbe)
      (bufset-u8 buf 3 0xef)
      (bufset-u8 buf 4 0xca)
      (bufset-u8 buf 5 0xfe)
      (bufset-u8 buf 6 0xba)
      (bufset-u8 buf 7 0xbe)
      (bufset-u8 buf 8 0x01)
      (bufset-u8 buf 9 0x02)
      (rs-encode buf 8)
      (corrupt buf 2 0xff)
      (corrupt buf 7 0x55)
      (corrupt buf 9 0x11)
      (rs-decode buf 8)
      (and (= (bufget-u8 buf 0) 0xde)
           (= (bufget-u8 buf 1) 0xad)
           (= (bufget-u8 buf 2) 0xbe)
           (= (bufget-u8 buf 3) 0xef)
           (= (bufget-u8 buf 4) 0xca)
           (= (bufget-u8 buf 5) 0xfe)
           (= (bufget-u8 buf 6) 0xba)
           (= (bufget-u8 buf 7) 0xbe)
           (= (bufget-u8 buf 8) 0x01)
           (= (bufget-u8 buf 9) 0x02)))))


(check t1  true  "t1  rs-encoded-size 100 4")
(check t2  true  "t2  rs-encoded-size 100 32")
(check t3  true  "t3  rs-encoded-size 223 32")
(check t4  true  "t4  rs-encoded-size 1 2")
(check t5  true  "t5  total > 255 -> error")
(check t6  true  "t6  odd nroots -> error")
(check t7  true  "t7  nroots=0 -> error")
(check t8  true  "t8  nroots=1 -> error")
(check t9  true  "t9  bad type arg0")
(check t10 true  "t10 bad type arg1")
(check t11 true  "t11 encode+decode no errors")
(check t12 true  "t12 one payload error detected and corrected")
(check t13 true  "t13 payload byte restored after correction")
(check t14 true  "t14 one ECC byte error corrected")
(check t15 true  "t15 two errors corrected")
(check t16 true  "t16 three errors uncorrectable")
(check t17 true  "t17 eight errors corrected with nroots=32")
(check t18 true  "t18 payload intact after multi-error correction")

(if (and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
         t11 t12 t13 t14 t15 t16 t17 t18)
    (print "SUCCESS")
    (print "FAILURE"))
