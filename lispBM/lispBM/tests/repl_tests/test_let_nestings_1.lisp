
;; Deeply nested in body position.
(define r1 (let ((a1 10))
             (let ((a2 20))
               (let ((a3 30))
                 (let ((a4 40))
                   (let ((a5 50))
                     (let ((a6 60))
                       (let ((a7 70))
                         (let ((a8 80))
                           (let ((a9 90))
                             (+ a1 a2 a3 a4 a5 a6 a7 a8 a9)))))))))))

(define b1 (= r1 450))

;; Deeply nested in bound value position

(define r2 (let ((a1
                  (let ((a2
                         (let ((a3
                                (let ((a4
                                       (let ((a5
                                              (let ((a6
                                                     (let ((a7
                                                            (let ((a8
                                                                   (let ((a9 101))
                                                                     a9)))
                                                              a8)))
                                                       a7)))
                                                a6)))
                                         a5)))
                                  a4)))
                           a3)))
                    a2)))
             a1))


(define b2 (= r2 101))

;; Deeply nested in body position with dependencies

(define r3 (let ((a1 10))
             (let ((a2 (+ a1 10)))
               (let ((a3 (+ a2 10)))
                 (let ((a4 (+ a3 10)))
                   (let ((a5 (+ a4 10)))
                     (let ((a6 (+ a5 10)))
                       (let ((a7 (+ a6 10)))
                         (let ((a8 (+ a7 10)))
                           (let ((a9 (+ a8 10)))
                             (+ a1 a2 a3 a4 a5 a6 a7 a8 a9)))))))))))

(define b3 (= r3 450))


(if (and b1 b2 b3)
    (print "SUCCESS")
  (print "FAILURE"))

