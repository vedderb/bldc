;; Bignum example - RSA

;; Modulus (calculated from p * q)
(define n
  (bn-from-bytes
   (hex-to-bytes
    (str-join (list "c8f4f7f614ab65b9ab8f3acb0c1dbd5d51561a0aa0c71e9ca928dad1f6b951ce"
                    "5a58cab032f9751de9660acbdd0b43ce8dfed3a5cf43675aa430bccabcaf07e1"
                    "4f43f3ba48b1aee0838721fc35ff25e55b0cc388851ec5c9071997431a6e856f"
                    "83c90d1524ea4ef0c40631d6dbf3713e5c9a9a47eca77350d97bd8cb79888843"
                    "2f6ce7ac31e4c5f965f6356ed714420ec0227ce9217733861019e51732b0cea2"
                    "922b5c314d829c9a0296b26070ddca5c7806bf1e937897d71856d82502e2300a"
                    "815f46c40b1fe51d6471507f4954f5612fe18e72523ba08dc9a2ca9d6b4f2f54"
                    "fc6959f1605d7ccb82a07154a167b0f0fc75b4f8375eb7685e5911cc8d0f5ead")))))

;; Public exponent
;; n and e forms the public key.
(define e
  (bn-from-bytes (hex-to-bytes "00010001")))

;; Private exponent
;; n and d forms the private key.
(define d
  (bn-from-bytes
   (hex-to-bytes
    (str-join (list "1f38a005a574739b8321ff9a68497e902f00d78eeb27c483c66c4882f2781c53"
                    "9909ce83cd87a04083708d20db38d2d216918ec4660d2cd3924ad82a0f628a30"
                    "e125c78a1b9d2f74463b0a76aad4f74848fb14adf330b2bde6d27d03b9e98de5"
                    "a36deb106be9d355e8bea5a3a20c927d83d9a93a9f73eca095097ec72e1c9e3e"
                    "bb7194eebebfaec4b8c2b983af4742eb7fcdbd7380840f07a4d64ceb39cb91b0"
                    "f4e9734a26efae307ccf904c5ee62bb529a2185af5c8101f5df3cc471b87a8fe"
                    "0010bd82172ca2a538f8d46f020438153921690dea97f46e88b6eb5c15ba2cf2"
                    "7e248eeb9b993108023ebc16c551993ff4877ed0ef76490c5cd6bf362217d529")))))

;; m - message
;; e - public exponent
;; n - the modulus
(defun rsa-encrypt (m e n)
  (bn-modexp m e n))

;; c - ciphertext (encrypted data)
;; d - private exponent
;; n - the modulus
(defun rsa-decrypt (c d n)
  (bn-modexp c d n))


(define hash-val (sha256-str "hello world"))

(define ciphertext (rsa-encrypt hash-val e n))
(define deciphered (rsa-decrypt ciphertext d n))

(print hash-val)
(print deciphered)
(print (bn-cmp hash-val deciphered))
