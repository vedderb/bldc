(defun str-cmp-asc (a b) (< (str-cmp a b) 0))
(defun str-cmp-dsc (a b) (> (str-cmp a b) 0))

(define r1 (not (str-cmp-dsc "abc" "def")))
(define r2 (str-cmp-dsc "def" "abc"))

(check (and r1 r2))
