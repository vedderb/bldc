;; Program with unterminated string literal
;; This should trigger TOKENIZER_STRING_ERROR when parsed

(define msg "this string is never closed and should cause a tokenizer error