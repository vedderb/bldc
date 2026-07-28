
;; Test sha256 (byte array variant) against known SHA256 vectors.
;; Input arrays contain sequential bytes [0, 1, 2, ... n-1].
;; Expected values verified with python3: hashlib.sha256(bytes(range(n))).hexdigest()

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

;; Helper: build a byte buffer with sequential values 0..n-1
(defun make-seq-buf (n) 
  (let ((buf (bufcreate n)))
    {
    (loopfor i 0 (< i n) (+ i 1) 
      (bufset-u8 buf i (mod i 256)))
    buf
    }))

;; n=0: empty
(define t1  (check (bytes-to-hex (sha256 []))
                   "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                   1))

;; n=1: single zero byte
(define t2  (check (bytes-to-hex (sha256 [0]))
                   "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d"
                   2))

;; n=2: [1 2]
(define t3  (check (bytes-to-hex (sha256 [1 2]))
                   "a12871fee210fb8619291eaea194581cbd2531e4b23759d225f6806923f63222"
                   3))

;; n=54: just under the single-block boundary
(define t4  (check (bytes-to-hex (sha256 (make-seq-buf 54)))
                   "675f28acc0b90a72d1c3a570fe83ac565555db358cf01826dc8eefb2bf7ca0f3"
                   4))

;; n=55: last length that fits in one block
(define t5  (check (bytes-to-hex (sha256 (make-seq-buf 55)))
                   "463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59"
                   5))

;; n=56: first length requiring two blocks
(define t6  (check (bytes-to-hex (sha256 (make-seq-buf 56)))
                   "da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562"
                   6))

;; n=63
(define t7  (check (bytes-to-hex (sha256 (make-seq-buf 63)))
                   "29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488"
                   7))

;; n=64: exactly one full data block
(define t8  (check (bytes-to-hex (sha256 (make-seq-buf 64)))
                   "fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108"
                   8))

;; n=127
(define t9  (check (bytes-to-hex (sha256 (make-seq-buf 127)))
                   "92ca0fa6651ee2f97b884b7246a562fa71250fedefe5ebf270d31c546bfea976"
                   9))

;; n=128: exactly two full data blocks
(define t10 (check (bytes-to-hex (sha256 (make-seq-buf 128)))
                   "471fb943aa23c511f6f72f8d1652d9c880cfa392ad80503120547703e56a2be5"
                   10))

(if (and t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
    (print "SUCCESS")
    (print "FAILURE"))
