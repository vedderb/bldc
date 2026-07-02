
;; Test sha256-str against known SHA256 vectors.
;; Expected values verified with: printf -n "..." | sha256sum

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

;; empty string
(define t1 (check (bytes-to-hex (sha256-str ""))
                  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                  1))

;; 1 byte
(define t2 (check (bytes-to-hex (sha256-str "a"))
                  "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb"
                  2))

;; 2 bytes
(define t3 (check (bytes-to-hex (sha256-str "ab"))
                  "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"
                  3))

;; 3 bytes
(define t4 (check (bytes-to-hex (sha256-str "abc"))
                  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                  4))

;; 56 bytes - crosses single-block boundary (55 bytes is the max that fits
;; in one 512-bit block alongside the padding and length field)
(define t5 (check (bytes-to-hex (sha256-str "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
                  "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
                  5))

(if (and t1 t2 t3 t4 t5)
    (print "SUCCESS")
    (print "FAILURE"))
