
(def arr (mkarray 3))

(setix arr 0 (list 1 2 3))
(setix arr 1 (list 4 5 6))
(setix arr 2 (list 7 8 9))

(gc)

(check (and (eq (ix arr 0) (list 1 2 3))
            (eq (ix arr 1) (list 4 5 6))
            (eq (ix arr 2) (list 7 8 9))))

