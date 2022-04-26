
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

(defun zip (xs ys)
  (if (or (eq xs nil) (eq ys nil)) nil
      (cons (cons (first xs) (first ys)) (zip (rest xs) (rest ys)))))
          
(defun zip-t (xs ys)
  (let ((zipacc (lambda (acc xs ys)
                  (if (or (eq xs nil) (eq ys nil)) acc
                      (zipacc (cons (cons (first xs) (first ys)) acc) (rest xs) (rest ys)
                              )))))
    (reverse (zipacc nil xs ys))))


(defun map (f xs)
  (if (eq xs nil) nil
      (cons (f (first xs)) (map f (rest xs)))))


(defun map-t (f xs)
  (let ((mapacc (lambda (acc f xs)
                  (if (eq xs nil) acc
                      (mapacc (cons (f (first xs)) acc) f (rest xs))))))
    (reverse (mapacc nil f xs))))


(defun foldl (f i xs)
  (if (eq xs nil) i
      (foldl f (f i (first xs)) (rest xs))))

(defun foldr (f i xs)
  (if (eq xs nil) i
      (f (first xs) (foldr f i (rest xs)))))


(defun replace-assoc (x y)
  (if (eq (first x) (first y))
      x
      y))


(defun reassoc (x xs) 
    (map (lambda (y) (replace-assoc x y)) xs))
