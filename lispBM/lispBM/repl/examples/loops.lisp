;; const char *loop_extensions_dyn_load[4] = {
;;   "(define loopfor (macro (it start cnd update body) (me-loopfor it start cnd update body)))",
;;   "(define loopwhile (macro (cnd body) (me-loopwhile cnd body)))",
;;   "(define looprange (macro (it start end body) (me-looprange it start end body)))",
;;   "(define loopforeach (macro (it lst body) (me-loopforeach it lst body)))"
;; };


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for loop
(define loopfor2
  (macro (it start cnd update body)
         `(call-cc-unsafe (lambda (break) (loop ( (,it ,start ) )
                                                 ,cnd
                                                 { ,body
                                                 (setq ,it ,update) })))))
  

(loopfor2 i 0 (< i 100) (+ i 1) (if (= i 72) (break 'apa) (print i)))

;; a02 is where a gensym would come in handy

(define loopfor3
  (macro (it start cnd update body)
         `(call-cc-unsafe (lambda (break)
                            (let ((a02 (lambda (,it res)
                                         (if ,cnd (a02 ,update ,body) res))))
                              (a02 ,start nil))))
         ))

(print "loopfor3")

(define res (loopfor3 i 0 (< i 10) (+ i 1) (if (= i 6) (break 'apa) (print i))))
(print "loopfor3 res: " res)

(define res (loopfor3 i 0 (< i 10) (+ i 1) (if (= i 420) (break 'apa) (print i))))
(print "loopfor3 res: " res)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; while loop
(define loopwhile2
   (macro (cnd body)
          `(call-cc-unsafe (lambda (break) (loop ()
                                                 ,cnd
                                                 ,body)))))
(define n 0)

(print "loopwhile2")

(loopwhile2 (<= n 10)
            {
            (print "hej")
            (break)
            (setq n (+ n 1))
            })

(define loopwhile3
  (macro (cnd body)
         `(call-cc-unsafe (lambda (break) (let ((a02 (lambda (res)
                       (if ,cnd (a02 ,body) res))))
            (a02 nil))))
         ))

(print "loopwhile3")
(define n 0)
(loopwhile3 (<= n 10)
            {
            (print "hej")
            (break)
            (setq n (+ n 1))
            })
(define n 0)
(loopwhile3 (<= n 4)
            {
            (print n "hej")
            (setq n (+ n 1))
            })


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; looprange
(define looprange2
  (macro (it start end body)
         `(call-cc-unsafe (lambda (break) (loop ( (,it ,start) )
                                                 ( < ,it ,end)
                                                 { ,body                            
                                                 (setq ,it (+ ,it 1))
                                                 })))))

(print "looprange2")
(looprange2 i 0 10
            {
            (print i)
            (if (= i 4) (break))
            })


(define looprange3
  (macro (it start end body)
         `(call-cc-unsafe
           (lambda (break)
             (let ((a02 (lambda (,it res)
                          (if (< ,it ,end) (a02 (+ ,it 1) ,body) res))))
               (a02 ,start nil))))
         ))

(print "looprange3")
(looprange2 i 0 10
            {
            (print i)
            (if (= i 4) (break))
            })

(looprange2 i 0 10
            {
            (print i)
            })


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loopforeach
(define loopforeach2
  (macro (it lst body)
         `(call-cc-unsafe (lambda (break) (loop ( (a01 ,lst))
                                                a01
                                                {
                                                (var ,it (car a01))
                                                ,body
                                                (setq a01 (cdr a01))
                                                })))))

(print "loopforeach2")

(loopforeach2 e (list 1 2 3 4)
              {
              (print e)
              (if (= e 3) (break))
              }
              )

(define loopforeach3
  (macro (it lst body)
         `(call-cc-unsafe (lambda (break) (let ((a02 (lambda (,it rst res)
                        (if (eq ,it nil) res (a02 (car rst) (cdr rst) ,body)))))
            (a02 (car ,lst) (cdr ,lst) nil))))
         ))

(print "loopforeach3")

(loopforeach3 e (list 1 2 3 4)
              {
              (print e)
              (if (= e 3) (break))
              }
              )

(loopforeach3 e (list 1 2 3 4)
              {
              (print e)
              }
              )



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defunret
(define defunret
  (macro (name args body)
         `(define ,name
            (lambda ,args (call-cc-unsafe (lambda (return) ,body))))))
