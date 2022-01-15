(define f (lambda ()
   (progn
     (define encval (get-encoder)) ; So that lisp_stats shows the value
     
     (define bms-tot (get-bms-val "v_tot"))
     (define bms-c1 (get-bms-val "v_cell" 1))
     (define bms-upd (get-bms-val "msg_age"))
     (define bms-hum (get-bms-val "hum"))
     
     (set-servo (/ (- 360.0 encval) 360.0))
     (yield 20000)
     (f)
)))
     
(f)

