(defun str-cmp-asc (a b) (< (str-cmp a b) 0))
(defun str-cmp-dsc (a b) (> (str-cmp a b) 0))


(define r1 (str-cmp-asc "abc" "def"))
(define r2 (not (str-cmp-asc "def" "abc")))

(check (and r1 r2))
