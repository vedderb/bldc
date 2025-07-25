

(define r1 (= (length (deg2rad 1 2 3)) 3))
(define r2 (= (length (rad2deg 1 2 3)) 3))


(check (and r1 r2))
