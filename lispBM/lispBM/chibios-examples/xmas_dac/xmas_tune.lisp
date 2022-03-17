

(define pause '(1136 . 0))
(define h '(2033 . 4095))
(define c '(1915 . 4095))
(define d '(1706 . 4095)) 
(define e '(1520 . 4095))
(define f '(1432 . 4095))
(define fs '(1352 . 4095))
(define g '(1276 . 4095))
(define a '(1136 . 4095))
(define h1 '(1012 . 4095))
(define c1 '(956 . 4095))
    
(define quarter 500000)
(define eight   250000)



(define melody
    (list `(,quarter . ,g)  `(,eight . ,g) `(,eight . ,a) `(,quarter . ,g)
	  `(,quarter . ,e)  `(,eight . ,e) `(,eight . ,f) `(,quarter . ,e)
	  `(,quarter . ,d)  `(,eight . ,d) `(,eight . ,e) `(,eight   . ,d)
	  `(,eight   . ,h)
	  `(,eight   . ,c)  `(,eight . ,d) `(,eight . ,e) `(,eight   . ,f)
	  `(,quarter . ,g)
	  `(,quarter . ,g)  `(,eight . ,g) `(,eight . ,a) `(,quarter . ,g)
	  `(,quarter . ,d)  `(,eight . ,g) `(,eight . ,a) `(,quarter . ,h1)
	  `(,quarter . ,c1) `(,quarter . ,e) `(,quarter . ,fs)
	  `(,quarter . ,g)  `(,quarter . ,pause) `(,quarter . ,pause)
	  `(,eight   . ,c)  `(,eight . ,c)
	  `(,eight   . ,c)  `(,eight . ,d) `(,eight . ,e)   `(,eight . ,d)
	  `(,eight   . ,c)  `(,eight . ,d) `(,quarter . ,e) `(,quarter . ,c)
	  `(,eight   . ,d)  `(,eight . ,d) `(,eight . ,d)   `(,eight . ,e)
	  `(,eight . ,f) `(,eight . ,d)
	  `(,eight   . ,d)  `(,eight . ,e) `(,quarter . ,f) `(,quarter . ,d)
	  `(,eight   . ,e)  `(,eight . ,f) `(,quarter . ,g) `(,eight . ,f)
	  `(,eight . ,e)
	  `(,eight   . ,f)  `(,eight . ,g) `(,quarter . ,a) `(,eight . ,g)
	  `(,eight . ,f)
	  `(,eight   . ,g)  `(,eight . ,a) `(,quarter . ,h1) `(,eight . ,a)
	  `(,eight . ,g)
	  `(,quarter . ,c1)  `(,quarter . ,c1) `(,quarter . ,pause)
	  ))

  
(define send-note (lambda (n)
		    (note (car n) (cdr n))))

(define play (lambda (x)
	       (match x
		      ( nil (play melody))
		      ( (? ls) (progn
			        (send-note (cdr (car ls)))
			        (yield (car (car ls)))
			        (play (cdr ls)))))))

			       
		      
