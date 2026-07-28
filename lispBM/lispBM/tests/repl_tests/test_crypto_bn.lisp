
(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))


(define t1 (check (bytes-to-hex [0xde 0xad 0xbe 0xef]) "deadbeef" "t1 bytes-to-hex"))
(define t2 (check (bytes-to-hex [0x00]) "00" "t2 bytes-to-hex single zero"))
(define t3 (check (bytes-to-hex [0x0f 0xf0]) "0ff0" "t3 bytes-to-hex leading nibble zero"))


(define t4 (check (bytes-to-hex (hex-to-bytes "deadbeef")) "deadbeef" "t4 hex-to-bytes round-trip"))
(define t5 (check (bytes-to-hex (hex-to-bytes "00ff")) "00ff" "t5 hex-to-bytes with zero byte"))
;; odd number of hex chars (not counting null) -> error
(define t6 (check (eq (trap (hex-to-bytes "abc")) '(exit-error eval_error)) t "t6 hex-to-bytes odd nibbles"))


(define t7  (check (bn-to-u32 (bn-from-u32 0))          0u32  "t7  bn round-trip 0"))
(define t8  (check (bn-to-u32 (bn-from-u32 42))         42u32 "t8  bn round-trip 42"))
(define t9  (check (bn-to-u32 (bn-from-u32 0xffffffff)) 4294967295u32 "t9  bn round-trip max u32"))

(define t10 (check (eq (trap (bn-to-u32 (bn-add (bn-from-u32 0xffffffff) (bn-from-u32 1))))
                       '(exit-error eval_error))
                   t "t10 bn-to-u32 overflow -> error"))

(define t11 (check (bn-cmp (bn-from-u32 5)  (bn-from-u32 5))   0  "t11 bn-cmp equal"))
(define t12 (check (bn-cmp (bn-from-u32 10) (bn-from-u32 3))   1  "t12 bn-cmp greater"))
(define t13 (check (bn-cmp (bn-from-u32 3)  (bn-from-u32 10)) -1  "t13 bn-cmp less"))

(define t14 (check (bn-to-u32 (bn-add (bn-from-u32 3) (bn-from-u32 4))) 7u32 "t14 bn-add simple"))

(define t15 (check (bytes-to-hex (bn-to-bytes (bn-add (bn-from-u32 0xffffffff) (bn-from-u32 1))))
                   "0000000100000000"
                   "t15 bn-add carry across limb boundary"))

(define t16 (check (bn-to-u32 (bn-sub (bn-from-u32 10) (bn-from-u32 3))) 7u32 "t16 bn-sub simple"))
(define t17 (check (bn-to-u32 (bn-sub (bn-from-u32 5)  (bn-from-u32 5))) 0u32 "t17 bn-sub equal -> zero"))
;; b > a must return eval_error
(define t18 (check (eq (trap (bn-sub (bn-from-u32 3) (bn-from-u32 10))) '(exit-error eval_error))
                   t "t18 bn-sub b>a -> error"))

(define t19 (check (bn-to-u32 (bn-mul (bn-from-u32 6) (bn-from-u32 7))) 42u32 "t19 bn-mul simple"))

(define t20 (check (bytes-to-hex (bn-to-bytes (bn-mul (bn-from-u32 0xffffffff) (bn-from-u32 0xffffffff))))
                   "fffffffe00000001"
                   "t20 bn-mul large product"))

;; 10 / 3 = q:3 r:1
(define t21 (let ((qr (bn-divmod (bn-from-u32 10) (bn-from-u32 3))))
  (check (and (= (bn-to-u32 (car qr)) 3u32)
              (= (bn-to-u32 (cdr qr)) 1u32))
         t "t21 bn-divmod simple")))

;; a < b -> q:0 r:a
(define t22 (let ((qr (bn-divmod (bn-from-u32 3) (bn-from-u32 10))))
  (check (and (= (bn-to-u32 (car qr)) 0u32)
              (= (bn-to-u32 (cdr qr)) 3u32))
         t "t22 bn-divmod a < b")))

;; exact division: 12 / 4 = q:3 r:0
(define t23 (let ((qr (bn-divmod (bn-from-u32 12) (bn-from-u32 4))))
  (check (and (= (bn-to-u32 (car qr)) 3u32)
              (= (bn-to-u32 (cdr qr)) 0u32))
         t "t23 bn-divmod exact")))

;; 2^10 mod 1000 = 1024 mod 1000 = 24
(define t24 (check (bn-to-u32 (bn-modexp (bn-from-u32 2) (bn-from-u32 10) (bn-from-u32 1000)))
                   24u32 "t24 bn-modexp 2^10 mod 1000"))

;; Fermat's little theorem: 3^6 mod 7 = 1  (a^(p-1) ≡ 1 mod p)
(define t25 (check (bn-to-u32 (bn-modexp (bn-from-u32 3) (bn-from-u32 6) (bn-from-u32 7)))
                   1u32 "t25 bn-modexp Fermat 3^6 mod 7"))
;; Fermat's little theorem: 3^7 mod 7 = 3  (a^p ≡ a mod p)
(define t25b (check (bn-to-u32 (bn-modexp (bn-from-u32 3) (bn-from-u32 7) (bn-from-u32 7)))
                    3u32 "t25b bn-modexp Fermat 3^7 mod 7"))

;; Toy RSA: n=33, e=3, d=7   (p=3 q=11)
;; encrypt: 4^3 mod 33 = 64 mod 33 = 31
(define t26 (check (bn-to-u32 (bn-modexp (bn-from-u32 4) (bn-from-u32 3) (bn-from-u32 33)))
                   31u32 "t26 bn-modexp toy-RSA encrypt"))
;; decrypt: 31^7 mod 33 = 4
(define t27 (check (bn-to-u32 (bn-modexp (bn-from-u32 31) (bn-from-u32 7) (bn-from-u32 33)))
                   4u32 "t27 bn-modexp toy-RSA decrypt"))

;; base^0 mod n = 1
(define t28 (check (bn-to-u32 (bn-modexp (bn-from-u32 999) (bn-from-u32 0) (bn-from-u32 7)))
                   1u32 "t28 bn-modexp base^0 = 1"))


;; --- bn-from-bytes / bn-to-bytes ---

;; round-trip of big-endian byte sequence
(define t29 (check (bytes-to-hex (bn-to-bytes (bn-from-bytes [0xca 0xfe 0xba 0xbe])))
                   "cafebabe" "t29 bn-from/to-bytes round-trip"))

;; bn-from-bytes requires length divisible by 4
(define t30 (check (eq (trap (bn-from-bytes [0x01 0x02 0x03])) '(exit-error eval_error))
                   t "t30 bn-from-bytes odd size -> error"))


(if (and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
         t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
         t21 t22 t23 t24 t25 t25b t26 t27 t28 t29 t30)
    (print "SUCCESS")
    (print "FAILURE"))
