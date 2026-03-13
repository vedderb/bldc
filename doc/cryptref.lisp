
(define entry-sha256
  (ref-entry "sha256"
             (list
              (para (list "`sha256` computes the SHA-256 hash of a byte array."
                          "The form of a `sha256` expression is `(sha256 buf)` where `buf`"
                          "is a byte array of any length."
                          "Returns a 32-byte array containing the hash."
                          "Specified in FIPS PUB 180-4."
                          ))
              (code '((sha256 [])
                      (sha256 [1 2 3])
                      (bytes-to-hex (sha256 []))
                      (bytes-to-hex (sha256 [1 2 3]))
                      ))
              end)))

(define entry-sha256-str
  (ref-entry "sha256-str"
             (list
              (para (list "`sha256-str` computes the SHA-256 hash of a string."
                          "The form of a `sha256-str` expression is `(sha256-str str)` where `str`"
                          "is a string."
                          "The null terminator is not included in the hash."
                          "Returns a 32-byte array containing the hash."
                          "Specified in FIPS PUB 180-4."
                          ))
              (code '((bytes-to-hex (sha256-str ""))
                      (bytes-to-hex (sha256-str "abc"))
                      (bytes-to-hex (sha256-str "hello world"))
                      ))
              end)))

(define entry-bytes-to-hex
  (ref-entry "bytes-to-hex"
             (list
              (para (list "`bytes-to-hex` converts a byte array to a lowercase hexadecimal string."
                          "The form of a `bytes-to-hex` expression is `(bytes-to-hex buf)`"
                          "where `buf` is a byte array."
                          "Each byte is represented as exactly two hex digits."
                          "Returns a string of length `2 * (buflen buf)`."
                          ))
              (code '((bytes-to-hex [0xde 0xad 0xbe 0xef])
                      (bytes-to-hex [0 1 2 3 255])
                      (bytes-to-hex (sha256-str "abc"))
                      ))
              end)))

(define entry-aes128-enc
  (ref-entry "aes128-enc"
             (list
              (para (list "`aes128-enc` encrypts a single 16-byte block using AES-128."
                          "The form of an `aes128-enc` expression is `(aes128-enc key data)`"
                          "where `key` is a 16-byte array and `data` is a 16-byte array."
                          "Returns a 16-byte array containing the encrypted block."
                          "This is a raw block cipher — the caller is responsible for"
                          "padding and any block chaining mode."
                          "Specified in FIPS 197."
                          ))
              (code '((bytes-to-hex
                       (aes128-enc [0x2b 0x7e 0x15 0x16 0x28 0xae 0xd2 0xa6
                                    0xab 0xf7 0x15 0x88 0x09 0xcf 0x4f 0x3c]
                                   [0x32 0x43 0xf6 0xa8 0x88 0x5a 0x30 0x8d
                                    0x31 0x31 0x98 0xa2 0xe0 0x37 0x07 0x34]))
                      ))
              end)))

(define entry-aes128-dec
  (ref-entry "aes128-dec"
             (list
              (para (list "`aes128-dec` decrypts a single 16-byte block using AES-128."
                          "The form of an `aes128-dec` expression is `(aes128-dec key data)`"
                          "where `key` is a 16-byte array and `data` is a 16-byte array."
                          "Returns a 16-byte array containing the decrypted block."
                          "This is a raw block cipher — the caller is responsible for"
                          "unpadding and any block chaining mode."
                          "Specified in FIPS 197."
                          ))
              (code '((bytes-to-hex
                       (aes128-dec [0x2b 0x7e 0x15 0x16 0x28 0xae 0xd2 0xa6
                                    0xab 0xf7 0x15 0x88 0x09 0xcf 0x4f 0x3c]
                                   [0x39 0x25 0x84 0x1d 0x02 0xdc 0x09 0xfb
                                    0xdc 0x11 0x85 0x97 0x19 0x6a 0x0b 0x32]))
                      ))
              end)))

(define entry-aes256-enc
  (ref-entry "aes256-enc"
             (list
              (para (list "`aes256-enc` encrypts a single 16-byte block using AES-256."
                          "The form of an `aes256-enc` expression is `(aes256-enc key data)`"
                          "where `key` is a 32-byte array and `data` is a 16-byte array."
                          "Returns a 16-byte array containing the encrypted block."
                          "This is a raw block cipher — the caller is responsible for"
                          "padding and any block chaining mode."
                          "Specified in FIPS 197."
                          ))
              (code '((bytes-to-hex
                       (aes256-enc [0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07
                                    0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f
                                    0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17
                                    0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f]
                                   [0x00 0x11 0x22 0x33 0x44 0x55 0x66 0x77
                                    0x88 0x99 0xaa 0xbb 0xcc 0xdd 0xee 0xff]))
                      ))
              end)))

(define entry-aes256-dec
  (ref-entry "aes256-dec"
             (list
              (para (list "`aes256-dec` decrypts a single 16-byte block using AES-256."
                          "The form of an `aes256-dec` expression is `(aes256-dec key data)`"
                          "where `key` is a 32-byte array and `data` is a 16-byte array."
                          "Returns a 16-byte array containing the decrypted block."
                          "This is a raw block cipher — the caller is responsible for"
                          "unpadding and any block chaining mode."
                          "Specified in FIPS 197."
                          ))
              (code '((bytes-to-hex
                       (aes256-dec [0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07
                                    0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f
                                    0x10 0x11 0x12 0x13 0x14 0x15 0x16 0x17
                                    0x18 0x19 0x1a 0x1b 0x1c 0x1d 0x1e 0x1f]
                                   [0x8e 0xa2 0xb7 0xca 0x51 0x67 0x45 0xbf
                                    0xea 0xfc 0x49 0x90 0x4b 0x49 0x60 0x89]))
                      ))
              end)))

(define entry-hex-to-bytes
  (ref-entry "hex-to-bytes"
             (list
              (para (list "`hex-to-bytes` converts a lowercase or uppercase hexadecimal string"
                          "to a byte array."
                          "The form of a `hex-to-bytes` expression is `(hex-to-bytes str)`"
                          "where `str` is a string containing an even number of hex digits."
                          ))
              (code '((hex-to-bytes "deadbeef")
                      (hex-to-bytes "DEADBEEF")
                      (hex-to-bytes "000102ff")
                      (hex-to-bytes (bytes-to-hex (sha256-str "abc")))
                      ))
              end)))

(define chapter-sha256
  (section 2 "SHA-256"
           (list entry-sha256
                 entry-sha256-str
                 )))

(define chapter-aes
  (section 2 "AES Block Cipher"
           (list entry-aes128-enc
                 entry-aes128-dec
                 entry-aes256-enc
                 entry-aes256-dec
                 )))

(define chapter-utilities
  (section 2 "Utility functions"
           (list entry-bytes-to-hex
                 entry-hex-to-bytes
                 )))


(define manual
  (list
   (section 1 "LispBM Crypto Extensions Reference Manual"
            (list
             (para (list "The crypto extensions provide cryptographic hash and block cipher"
                         "primitives. These are low-level building blocks — padding,"
                         "block chaining modes and higher-level constructions are left"
                         "to the caller."
                         "These extensions may or may not be present depending on the"
                         "platform and configuration of LispBM."
                         ))
             chapter-sha256
             chapter-aes
             chapter-utilities
             ))
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "cryptref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (gc)
    (var t0 (systime))
    (render r manual)
    (print "Crypto extensions reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
