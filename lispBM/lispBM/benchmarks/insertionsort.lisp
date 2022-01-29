
(define insert (lambda (l ls)
                 (match ls
                        ( nil (cons l nil))
                        ( ((? a) . (? xs))
                          (if (< l a)
                              (cons l (cons a xs))
                              (cons a (insert l xs)))))))

(define sort (lambda (ls)
               (match ls
                      (nil nil)
                      ( ((? a) . (? xs))
                        (insert a (sort xs))))))

(sort '(4 2 0 9 1 56 2 4 7))
