;; odd sizes tests
(if (= (word-size) 8)
    (define dm (dm-create 200))
  (define dm (dm-create 100)))

(define a1 (dm-alloc dm 11))
(bufclear a1 0xFF)

(dm-alloc dm 11)

(define a2 (dm-alloc dm 9))
(bufclear a2 170)

;; perform allocations until there must have been a GC + defrag

(dm-alloc dm 7)
(dm-alloc dm 9)
(dm-alloc dm 11)
(dm-alloc dm 13)
(dm-alloc dm 15)
(dm-alloc dm 17)
(dm-alloc dm 19)
(dm-alloc dm 21)
(dm-alloc dm 9)

(define a3 (dm-alloc dm 11))
(bufclear a3 99)

(check (and (eq a2 [170 170 170 170 170 170 170 170 170])
	    (eq a1 [255 255 255 255 255 255 255 255 255 255 255])
	    (eq a3 [99 99 99 99 99 99 99 99 99 99 99])))
