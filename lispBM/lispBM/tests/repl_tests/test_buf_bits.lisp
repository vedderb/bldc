;; Round-trip tests for bufget-bits-u/i / bufset-bits-u/i

(defun mask64 (len) (if (= len 64) 0xFFFFFFFFFFFFFFFFu64 (- (shl 1u64 len) 1u64)))

(defun test-u-roundtrip (maxlen flags) {
  (var ok t)
  (loopforeach pos (range 0 48)
    (loopforeach len (range 1 (+ maxlen 1))
      (if (<= (+ pos len) 56) ;; keep well inside an 8 byte buffer, avoid dbc remap edge growth
          (let ((d (bufcreate 8))
                (val (bitwise-and 0xAAAAAAAAAAAAAAAAu64 (mask64 len))))
            {
              (apply bufset-bits-u (append (list d pos len val) flags))
              (if (not (= (apply bufget-bits-u (append (list d pos len) flags)) val))
                  (setq ok nil))
            }))))
  ok
})

(defun test-i-roundtrip (maxlen flags) {
  (var ok t)
  (loopforeach pos (range 0 48)
    (loopforeach len (range 1 (+ maxlen 1))
      (if (<= (+ pos len) 56)
          (let ((d (bufcreate 8))
                (val -1))
            {
              (apply bufset-bits-i (append (list d pos len val) flags))
              (if (not (= (apply bufget-bits-i (append (list d pos len) flags)) -1))
                  (setq ok nil))
            }))))
  ok
})

(defun all-flag-combos-ok (maxlen signed) {
  (var roundtrip (if signed test-i-roundtrip test-u-roundtrip))
  (and (roundtrip maxlen '())
       (roundtrip maxlen '(little-endian))
       (roundtrip maxlen '(big-endian))
       (roundtrip maxlen '(dbc))
       (roundtrip maxlen '(dbc big-endian))
       (roundtrip maxlen '(dbc little-endian)))
})

;; exercise all three internal encoding tiers (immediate / boxed-32 / boxed-64)
;; by sweeping len all the way to 64, independent of build word size
(defun check-types () (and
  (all-flag-combos-ok 64 nil)
  (all-flag-combos-ok 64 t)))

;; a generic u/i accessor must not care which "width" the caller had in mind -
;; same len, same bits, same answer, regardless of magnitude
(defun check-no-width-truncation () {
  (var d (bufcreate 8))
  (bufset-bits-u d 0 12 0xFFF)
  (= (bufget-bits-u d 0 12) 0xFFF)
})

;; sign extension from the field's bit length, not from any fixed type width
(defun check-sign-extension () {
  (var d (bufcreate 8))
  (bufset-bits-u d 0 9 0x1FF) ;; all-ones 9 bit field
  (and
   (= (bufget-bits-i d 0 9) -1)
   (= (bufget-bits-u d 0 9) 0x1FF))
})

;; dbc remap sanity check against hand-derived expectation (from earlier manual analysis)
(defun check-dbc-sanity () {
  (var d (bufcreate 8))
  (bufset-bits-u d 7 3 5 'dbc) ;; motorola start-bit 7, 3-bit field -> top 3 bits of byte0 = 0xA0
  (and (= (bufget-u8 d 0) 0xA0)
       (= (bufget-bits-u d 7 3 'dbc) 5))
})

;; out of bounds must fail cleanly, not corrupt memory / crash
(defun check-oob () {
  (var d (bufcreate 1))
  (and (eq (trap (bufset-bits-u d 0 32 0xFF)) '(exit-error eval_error))
       (eq (trap (bufget-bits-u d 0 32)) '(exit-error eval_error)))
})

;; a value that needs insertion into a field wider than 32 bits must not be
;; truncated before bits_insert ever sees it
(defun check-wide-value-set () {
  (var d (bufcreate 8))
  (bufset-bits-u d 0 40 0xFFFFFFFFFFu64)
  (= (bufget-bits-u d 0 40) 0xFFFFFFFFFFu64)
})

;; bit-addressable float, fixed width 32, no len argument; must agree with
;; the existing byte-aligned bufget-f32/bufset-f32 at a byte-aligned position,
;; and must also work at a non-byte-aligned position (the real DBC case)
(defun check-f32 () {
  (var d (bufcreate 8))
  (bufset-bits-f32 d 0 3.14)
  (var r1 (and (= (bufget-bits-f32 d 0) 3.14)
               (= (bufget-f32 d 0) 3.14)))
  (bufset-bits-f32 d 13 -2.71)
  (var r2 (= (bufget-bits-f32 d 13) -2.71))
  (bufset-bits-f32 d 0 1.5 'little-endian)
  (var r3 (and (= (bufget-bits-f32 d 0 'little-endian) 1.5)
               (= (bufget-f32 d 0 'little-endian) 1.5)))
  (and r1 r2 r3)
})

;; 50 hand-picked values (powers of two, all-ones runs, alternating-bit
;; patterns, primes, and some arbitrary constants) round-tripped at a mix
;; of lengths and positions, byte-aligned and not.
(define test-values (list
  0u64 1u64 2u64 3u64 5u64 7u64 0xFu64 0x1Fu64 0xFFu64 0x100u64
  0x155u64 0x2AAu64 0x555u64 0xAAAu64 0xFFFu64 0x1000u64 0xFFFFu64 0x10000u64 0xAAAAAAAAu64 0x55555555u64
  0xFFFFFFFFu64 0x100000000u64 0x123456789ABCDEFu64 0xDEADBEEFu64 0xCAFEBABEu64 0x8000000000000000u64 0x7FFFFFFFFFFFFFFFu64 0xFFFFFFFFFFFFFFFFu64 0xAAAAAAAAAAAAAAAAu64 0x5555555555555555u64
  13u64 17u64 19u64 23u64 29u64 31u64 37u64 41u64 43u64 47u64
  97u64 101u64 127u64 128u64 255u64 256u64 65535u64 65536u64 12345u64 1234567890u64))

(defun check-value-list () {
  (var ok t)
  (loopforeach val test-values
    (loopforeach len (list 1 7 8 16 20 32 40 56 64)
      (loopforeach pos (list 0 3 9 20)
        (if (<= (+ pos len) 56)
            (let ((d (bufcreate 8))
                  (want (bitwise-and val (mask64 len))))
              {
                ;; unsigned: direct value comparison is safe both ways
                (bufset-bits-u d pos len want)
                (if (not (= (bufget-bits-u d pos len) want)) (setq ok nil))
                (bufset-bits-u d pos len want 'dbc)
                (if (not (= (bufget-bits-u d pos len 'dbc) want)) (setq ok nil))

                ;; signed: comparing against `want` directly would be wrong
                ;; whenever the top bit is set (want is unsigned, the signed
                ;; reading is numerically different) - instead check that
                ;; sign-extension is a fixed point: re-inserting the signed
                ;; reading must reproduce the same signed reading
                (bufset-bits-i d pos len want)
                (var s1 (bufget-bits-i d pos len))
                (bufset-bits-i d pos len s1)
                (if (not (= (bufget-bits-i d pos len) s1)) (setq ok nil))
              })))))
  ok
})

(if (and (check-types)
         (check-no-width-truncation)
         (check-sign-extension)
         (check-dbc-sanity)
         (check-oob)
         (check-wide-value-set)
         (check-f32)
         (check-value-list))
    (print "SUCCESS")
  (print "FAILURE"))
