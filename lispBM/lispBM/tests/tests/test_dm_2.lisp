;; 100 bytes becomes 25 words

(if (= (word-size) 8)
    (define dm (dm-create 200))
  (define dm (dm-create 100))
  )

(define a1 (dm-alloc dm 10))
(bufclear a1 0xFF)

(dm-alloc dm 10)

(define a2 (dm-alloc dm 10))
(bufclear a2 170)

;; perform allocations until there must have been a GC + defrag

(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)
(dm-alloc dm 10)

(define a3 (dm-alloc dm 10))
(bufclear a3 99)

(check (and (eq a2 [170 170 170 170 170 170 170 170 170 170])
            (eq a1 [255 255 255 255 255 255 255 255 255 255])
            (eq a3 [99 99 99 99 99 99 99 99 99 99])))

