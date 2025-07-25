

;; 1000 bytes becomes 250 words
(if (= (word-size) 8)
    (define dm (dm-create 1100))
  (define dm (dm-create 1000)))

;; each allocation of 100bytes occupies a total of 28words (3 words header + 25 words data)
;; 28 * 9 = 252
;; so at most 8 allocations of 100bytes can fit


(define a1 (dm-alloc dm 100)) ;; 28 words
(dm-alloc dm 10)              ;; 34

(define a2 (dm-alloc dm 100)) ;; 62
(dm-alloc dm 10)              ;; 68

(define a3 (dm-alloc dm 100)) ;; 96
(dm-alloc dm 10)              ;; 102

(define a4 (dm-alloc dm 100)) ;; 130
(dm-alloc dm 10)              ;; 136

(define a5 (dm-alloc dm 100)) ;; 164
(dm-alloc dm 10)              ;; 170

(define a6 (dm-alloc dm 100)) ;; 198
(dm-alloc dm 10)              ;; 204

(define a7 (dm-alloc dm 100)) ;; 232
(dm-alloc dm 10)              ;; 238

(define a8 (dm-alloc dm 100)) ;; Triggers defrag and frees up 42 words
                              ;; allocation should be ok!

(define merr '(exit-error out_of_memory))
(check (eq merr (trap (dm-alloc dm 100))))
