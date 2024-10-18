
(defun f (a b c)
  c)

(defun g (x) (+ x 5))

(defun h (i j k) (+ i j k))

(defun i (i)
  i)

(define apa 100)

(define prgs (list "(+ 1 2) (+ 1 2) (f 3.14 4 99)"
		   "(g 5)"
		   "(+ 1 2) (+ 1 2) (h 1 2 3)"
		   "nil nil (i apa)"
		   ))

(define ress (list 99 10 6 100))

(define orig (list 12u32
		   "kurt russel"
		   1b
		   2b
		   "is great"
		   [1 2 3 4 5]
		   55i32
		   24
		   'apa
		   "spongebob"
		   ))

(defun flat-op (n) {
       (var a (flatten (take orig n)))
       (unflatten a)
       (flat-op (- n 1))
       })

(defun flat-op-10-to-1 (n)
       (if (= n 0) t
	   {
	   (flat-op n)
	   (flat-op-10-to-1 (- n 1))
	   }))

(defun flat-loop () {
       (flat-op-10-to-1 10)
       (flat-loop)
       }
       )

(define id (spawn flat-loop))


(defun t-prgs ()
  {
  (var r (eq (ix ress 0) (read-eval-program (ix prgs 0))))
  (setq r (and r (eq (ix ress 1) (read-eval-program (ix prgs 1)))))
  (setq r (and r (eq (ix ress 2) (read-eval-program (ix prgs 2)))))
  (setq r (and r (eq (ix ress 3) (read-eval-program (ix prgs 3)))))
  r
  })


(defun test (n)
  (if (= n 0) t 
      (if (t-prgs)
	  (test (- n 1))
	  nil)
      )
  )
	   

(check (test 100))
(kill id 0)
  
  
