;; 100 bytes becomes 25 words

;; This is not how to write dm programs!
(let ((dm (if (= (word-size) 8) (dm-create 200)
            (dm-create 100))))
  {
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
  }
  )

(gc)

;; here no a1 a2 a3 should be accessible

(check ( = 1 1)) 
;; really just a check that nothing crashed. 
