


(hide-trapped-error)


;; ------------------- t1

(define bytes [0x00 0x00 0x00 0x01])                                          
(define bn (bn-from-bytes bytes))                                             
(define bytes1 (bn-to-bytes bn))

(define t1 (eq bytes1 bytes))


;; -------------------- t2
(define a (bn-from-bytes [0x00 0x00 0x00 0x01])) 
(define b (bn-from-bytes [0xFF 0xFF 0xFF 0xFF]))                              
(define e (bn-to-bytes (bn-add b a)))                                         
(define t2 (eq e [0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x00]))

;; ---------------------- t3

(define a (bn-from-u32 10))
(define b (bn-from-u32 102))
(define c (bn-add a b))
(define d (bn-to-u32 c))
(define t3 (= 112 d))


;; ---------------------- t4

(define d (bn-cmp a b))
(define e (bn-cmp b a))
(define f (bn-cmp a a))
(define g (bn-cmp b b))

(define t4 (and (= d -1)
                (= e 1)
                (= f 0)
                (= g 0)))


;; ---------------------- t5
;; single-limb divisor: 100 / 7 = 14 remainder 2

(define a  (bn-from-u32 100))
(define b  (bn-from-u32 7))
(define qr (bn-divmod a b))
(define t5 (and (= (bn-to-u32 (car qr)) 14)
                (= (bn-to-u32 (cdr qr)) 2)))

;; ---------------------- t6
;; a < b: 3 / 10 = 0 remainder 3

(define a  (bn-from-u32 3))
(define b  (bn-from-u32 10))
(define qr (bn-divmod a b))
(define t6 (and (= (bn-to-u32 (car qr)) 0)
                (= (bn-to-u32 (cdr qr)) 3)))

;; ---------------------- t7
;; multi-limb: 0x0000000100000000 / 0x00000003 = 0x55555555 remainder 1

(define a  (bn-from-bytes [0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x00]))
(define b  (bn-from-u32 3))
(define qr (bn-divmod a b))
(define t7 (and (= (bn-to-u32 (car qr)) 0x55555555u32)
                (= (bn-to-u32 (cdr qr)) 1)))

;; -------------------- t8
;; multi-limb divisor: 0x0000000200000000 / 0x0000000100000001 = 1 remainder 0x00000000FFFFFFFF

(define a  (bn-from-bytes [0x00 0x00 0x00 0x02 0x00 0x00 0x00 0x00]))
(define b  (bn-from-bytes [0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x01]))
(define qr (bn-divmod a b))
(define t8 (and (= (bn-to-u32 (car qr)) 1)
                (eq (bn-to-bytes (cdr qr)) [0x00 0x00 0x00 0x00 0xFF 0xFF 0xFF 0xFF])))


;; -------------------- t9
;; 2^10 mod 1000 = 1024 mod 1000 = 24

(define t9 (= (bn-to-u32 (bn-modexp (bn-from-u32 2)
                                     (bn-from-u32 10)
                                     (bn-from-u32 1000)))
              24u32))

;; -------------------- t10
;; 3^3 mod 5 = 27 mod 5 = 2

(define t10 (= (bn-to-u32 (bn-modexp (bn-from-u32 3)
                                      (bn-from-u32 3)
                                      (bn-from-u32 5)))
               2u32))

;; -------------------- t11
;; multi-limb: 2^64 mod 1000000007 = 4294967296^2 mod 1000000007

(define a (bn-from-u32 2))
(define b (bn-from-bytes [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x40]))
(define c (bn-from-u32 1000000007u32)) ;; prime
(define d (bn-modexp a b c))
(define res 582344008u32) ;; from Wolfram alpha

(define t11 (= (bn-to-u32 d) res))


;; ------------- t12
;; multi-limb: 2^(64 * 64) mod 1000000007

(define a (bn-from-u32 2))
(define b (bn-from-bytes [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x40]))
(define c (bn-from-u32 1000000007u32))
(define d (bn-modexp a (bn-mul b b) c))
(define res 246797651u32) ;; from Wolfram alpha

(define t12 (= (bn-to-u32 d) res))

;; ------------- t13
;; multi-limb: 2^(64 * 64) mod 1000000009 

(define a (bn-from-u32 2))
(define b (bn-from-bytes [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x40]))
(define c (bn-from-u32 1000000009u32)) ;; also prime
(define d (bn-modexp a (bn-mul b b) c))
(define res 860434291u32) ;; from Wolfram alpha

(define t13 (= (bn-to-u32 d) res))


(define a (bn-from-u32 65537u32))                                          
(define b  (bn-from-u32 2u32))            
(define c    (bn-from-bytes [0x00 0x00 0x00 0x02 0x00 0x00 0x00 0x07]))
(define d    (bn-modexp a b c))                                          
(define t14  (eq (bn-to-bytes d) [0x00 0x00 0x00 0x01 0x00 0x02 0x00 0x01])) 

(if (and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) 
    (print "SUCCESS")
  (print "FAILURE"))
    
