
(define reverse
  (lambda (xs)
    (let ((revacc (lambda (acc xs)
		    (if (= nil xs)
			acc
		      (revacc (cons (car xs) acc) (cdr xs))))))
      (revacc nil xs))))

(define iota (lambda (n)
         (let ((iacc (lambda (acc i)
                 (if (< i 0)
                 acc
                   (iacc (cons i acc) (- i 1))))))
         (iacc nil n))))

(define length (lambda (xs)
		 (let ((len (lambda (l xs)
			      (if (= xs nil)
				  l
				(len (+ l 1) (cdr xs))))))
		   (len 0 xs))))

(define take (lambda (n xs)
	       (let ((take-tail
		      (lambda (acc n xs)
			(if (num-eq n 0)
			    acc
			  (take-tail (cons (car xs) acc) (- n 1) (cdr xs))))))
		 (reverse (take-tail nil n xs)))))

(define drop (lambda (n xs)
	       (if (num-eq n 0)
		   xs
		 (if (= xs nil)
		     nil
		   (drop (- n 1) (cdr xs))))))

(define zip (lambda (xs ys)
	      (if ( = xs nil)
		  nil
		(if ( = ys nil)
		    nil
		  (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))))

(define map (lambda (f xs)
	      (if (= xs nil)
		  nil
		(cons (f (car xs)) (map f (cdr xs))))))

(define lookup (lambda (x xs)
		 (if (= xs nil)
		     nil
		   (if (= (car (car xs)) x)
		       (car (cdr (car xs)))
		     (lookup x (cdr xs))))))

(define foldr (lambda (f i xs)
		(if (= xs nil)
		    i
		  (f (car xs) (foldr f i (cdr xs))))))

(define foldl (lambda (f i xs)
		(if (= xs nil)
		    i
		  (foldl f (f i (car xs)) (cdr xs)))))
