
(defun repeat_eval (c n)
  (if ( = n 0)
      ()
    (progn
      (c n)
      (bufcreate 10)
      (repeat_eval c (- n 1)))))



(defun code (x) (def apa (eval `(read (str-merge "bepa" (str-from-n x))))))

;; 4 + 1 (2 or 3) bytes per string, padded to 8 bytes
;; 3 * 4 bytes per symtable entry
;; tot = (3 * 4) + 8 = 20 bytes;
;; 20bytes * 415 = 8300bytes
;; I cannot quite remember what this test is all about.
;;  - It fills up lbm_memory with symbols.
;;  - > 415 results in a read error if 16k lbm mem is given to test_list_code_cps.
;;  Doesn't feel right.

; Create just enough symbols and symbols and arrays to trigger GC.
(repeat_eval code 415)

(check (eq apa 'bepa1))
