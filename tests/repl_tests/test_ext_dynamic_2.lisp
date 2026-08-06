

;; On the incremental reader (reg-dyn-ext2) can run before the reader processes
;; ext-d2.

(reg-dyn-ext2)

(if (and (eq (ext-d2 42) nil)
         (eq (ext-d2 10) 42)
         (eq (ext-d2 1000) 10))
    (print "SUCCESS")
    (print "FAILURE"))
