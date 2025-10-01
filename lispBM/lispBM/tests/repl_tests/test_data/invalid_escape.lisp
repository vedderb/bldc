;; Program with invalid escape sequences
;; This should trigger TOKENIZER_STRING_ERROR when parsed

(define msg1 "invalid escape: \z")
(define msg2 "another invalid: \x")  
(define msg3 "incomplete escape at end: \")
(define char1 #\q)  ;; Invalid character escape