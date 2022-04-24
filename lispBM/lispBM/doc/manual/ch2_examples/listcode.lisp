
(defun elem (ls e)
  (if (eq ls 'nil)
      'nil
    (if (eq (first ls) e)
        e
      (elem (rest ls) e))))

(defun elem-pm (ls e)
  (match ls
         ( nil nil )
         ( ((? x) . (? xs))
           (if (eq x e) e (elem-pm xs e)) )))

(defun elem-pm2 (ls e)
  (match ls
         ( nil nil )
         ( (,e . _) e )
         ( (_ . (? xs)) (elem-pm2 xs e) )))

(defun length-notail (ls)
  (if (eq ls nil)
      0
    ( + 1 (length-notail (rest ls)))))


(defun length-tail (ls)
  (let ((len-helper (lambda (acc ls)
                      (if (eq ls nil)
                          acc
                        (len-helper (+ 1 acc) (rest ls))))))
    (len-helper 0 ls)))


(defun iota (n)
  (let ((iacc (lambda (acc i)
                (if (< i 0) acc
                    (iacc (cons i acc) (- i 1))))))
    (iacc nil (- n 1))))


(defun reverse (xs)
  (let ((revacc (lambda (acc xs)
                  (if (eq nil xs) acc
                      (revacc (cons (first xs) acc) (rest xs))))))
    (revacc nil xs)))

(defun take-n (n xs)
  (if ( = 0 n)
      nil
      (cons (first xs) (take-n (- n 1) (rest xs) ))))

(defun take-t (n xs)
  (let ((takeacc (lambda (acc n xs)
                   (if (= n 0) acc
                       (takeacc (cons (first xs) acc) (- n 1) (rest xs))))))
    (takeacc nil n xs)))

(defun take-t2 (n xs)
  (let ((takeacc (lambda (acc n xs)
                   (if (= n 0) acc
                       (takeacc (cons (first xs) acc) (- n 1) (rest xs))))))
    (reverse (takeacc nil n xs))))


(defun drop-n (n xs)
  (if ( = 0 n)
      xs
      (drop-n (- n 1) (rest xs))))
