;; 100 bytes becomes 25 words
(define dm (if (= (word-size) 8)
               (dm-create 200)
             (dm-create 100)))

(define a1 (dm-alloc dm 10))
(bufclear a1 0xFF)

(dm-alloc dm 10)

(define a2 (dm-alloc dm 10))
(bufclear a2 170)

(gc) ;; removes a 10 alloc

(define a3 (dm-alloc dm 10))
(bufclear a3 99)

(check (and (eq a2 [170 170 170 170 170 170 170 170 170 170])
	    (eq a1 [255 255 255 255 255 255 255 255 255 255])
            (eq a3 [99  99  99  99  99  99  99  99  99  99 ])))

