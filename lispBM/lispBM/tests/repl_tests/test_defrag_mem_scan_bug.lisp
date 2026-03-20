;; Test to trigger a potential bug in lbm_defrag_mem_alloc_internal.
;;
;; The bug: In the FREE_LEN state, when the scanner hits a live
;; allocation after counting a too-small free region, it does i++
;; before transitioning to INIT. This causes INIT to read the
;; data-pointer field as if it were the size field, computing
;; a wrong jump distance.
;;
;; To trigger this we need:
;;   1. Allocate A1, A2, A3 consecutively in defrag memory
;;   2. Let A2 be freed by GC (creating a hole)
;;   3. Request an allocation larger than the hole
;;   4. The scanner should pass through the hole, hit A3,
;;      and misread A3's layout due to the off-by-one.
;;
;; On 32-bit: word size = 4 bytes, header = 3 words
;;   alloc of 4 bytes = 4 words (3 header + 1 data)
;;   alloc of 8 bytes = 5 words (3 header + 2 data)
;;
;; On 64-bit: word size = 8 bytes, header = 3 words
;;   alloc of 8 bytes  = 4 words (3 header + 1 data)
;;   alloc of 16 bytes = 5 words (3 header + 2 data)

;; Use a pool large enough for 3 small allocations plus a larger one.
;; 32-bit: 3*4 + 8 = 20 words = 80 bytes, plus margin -> 200 bytes
;; 64-bit: 3*4 + 8 = 20 words = 160 bytes, plus margin -> 400 bytes
(define ws (word-size))
(define dm (if (= ws 8) (dm-create 400) (dm-create 200)))

;; Small allocation size: 1 word of data
(define small-bytes ws)
;; Larger allocation size: needs more words than the hole left by one small alloc
(define large-bytes (* ws 3))

;; A1: kept alive
(define a1 (dm-alloc dm small-bytes))
(bufclear a1 0xAA)

;; A2: will be freed - allocate in a let so it goes out of scope
(let ((a2 (dm-alloc dm small-bytes)))
  (bufclear a2 0xBB)
  )

;; A3: kept alive
(define a3 (dm-alloc dm small-bytes))
(bufclear a3 0xCC)

;; GC to free A2, creating a hole between A1 and A3
(gc)

;; Now request a larger allocation that does NOT fit in the hole.
;; The scanner should:
;;   - Skip past A1 (INIT state, reads size, jumps)
;;   - Enter the hole (FREE_LEN state, counts free words)
;;   - Hit A3 (hole too small) -> transition to INIT with i++
;;   - BUG: now reads A3's data-pointer as size, jumps wrong
(define a4 (dm-alloc dm large-bytes))

;; Verify everything is still consistent
(define ok t)

;; a1 should still have its data
(define i 0)
(loop ((i 0)) (< i small-bytes) {
  (if (not (= (bufget-u8 a1 i) 0xAA)) (setq ok nil))
  (setq i (+ i 1))
})

;; a3 should still have its data
(loop ((i 0)) (< i small-bytes) {
  (if (not (= (bufget-u8 a3 i) 0xCC)) (setq ok nil))
  (setq i (+ i 1))
})

;; a4 should be a valid allocation (not an error symbol)
(if (not (eq (type-of a4) 'type-array)) (setq ok nil))

(if ok (print "SUCCESS")
    (print "FAILURE"))
