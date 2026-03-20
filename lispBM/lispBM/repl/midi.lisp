

(defun ljud (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 1 1 0.1 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-saw 1.0 0.0)
  ;;(patch-osc-tvp-set patch-num 1 'osc-saw 0.4 0.0)
  ;;(patch-mod-set patch-num 1 0 'mod-env 8.0)
  (patch-lfo-set patch-num 0 'osc-sine 10)
  (patch-mod-set patch-num 0 1 'mod-lfo1 5.0)
  ;;(patch-mod-set patch-num 1 1 'mod-lfo1 3.5)
  ;;(patch-filter-set patch-num 'simple-lpf 2800.0)
  })


(defun violin (patch-num)
  {
  (patch-clear patch-num)
  (patch-adsr-set patch-num 0.4 0.1 0.7 0.15)
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.6 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.4 0.0)
  (patch-mod-set patch-num 1 0 'mod-env 8.0)
  (patch-lfo-set patch-num 0 'osc-sine 5.5)
  (patch-mod-set patch-num 0 1 'mod-lfo1 3.0)
  (patch-mod-set patch-num 1 1 'mod-lfo1 3.5)
  (patch-filter-set patch-num 'simple-lpf 2800.0)
  })

(defun p1 (patch-num)
  {
  (patch-clear patch-num)
  (patch-osc-tvp-set patch-num 0 'osc-triangle 0.5 0.0)
  (patch-osc-pan-set patch-num 0 -0.9)
  (patch-osc-tvp-set patch-num 1 'osc-triangle 0.5 0.25)
  (patch-osc-pan-set patch-num 1 0.9)
  (patch-adsr-set patch-num 0.001 0.1 0.0 0.2)
  (patch-filter-set patch-num 'simple-hpf 500.0)
  (patch-mod-set patch-num 0 3 'mod-vel 0.5)
  })

(defun p2 (patch-num)
  {
  (patch-clear patch-num)
  (patch-osc-tvp-set patch-num 0 'osc-saw 0.4 0.0)
  (patch-osc-pan-set patch-num 0 -0.95)
  (patch-osc-tvp-set patch-num 1 'osc-saw 0.4 0.33)
  (patch-osc-pan-set patch-num 1 0.95)
  (patch-adsr-set patch-num 0.005 0.3 0.0 0.3)
  (patch-filter-set patch-num 'simple-hpf 500.0)       
  })

(defun tom (patch-num)
  {
  (patch-clear patch-num)
  (patch-osc-tvp-set patch-num 0 'osc-sine 0.8 0.0)
  (patch-osc-pan-set patch-num 0 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-triangle 0.3 0.0)
  (patch-osc-pan-set patch-num 1 0.0)
  (patch-mod-set patch-num 0 0 'mod-env 80.0)
  (patch-mod-set patch-num 1 0 'mod-env 60.0)
  (patch-adsr-set patch-num 0.001 0.15 0.0 0.05)
  (patch-filter-set patch-num 'simple-lpf 1200.0)
  })

(defun snare (patch-num)
  {
  (patch-clear patch-num)
  (patch-osc-tvp-set patch-num 0 'osc-noise 0.6 0.0)
  (patch-osc-pan-set patch-num 0 0.0)
  (patch-osc-tvp-set patch-num 1 'osc-triangle 0.4 0.0)
  (patch-osc-pan-set patch-num 1 0.0)
  (patch-mod-set patch-num 1 0 'mod-env 40.0)
  (patch-adsr-set patch-num 0.001 0.08 0.0 0.02)
  (patch-filter-set patch-num 'simple-hpf 800.0)
  })




(violin 0)
(p1 1)
(p2 2)
(tom 3)
(snare 4)

(defun beat ()
  {
  ;; Kick pattern (using low tom as kick)
  (note-on 3 36 127)   ; Kick
  (sleep 0.25)

  ;; Snare
  (note-on 4 50 110)
  (sleep 0.25)

  ;; Kick
  (note-on 3 36 120)
  (sleep 0.125)

  ;; Light kick
  (note-on 3 36 90)
  (sleep 0.125)
  
  ;; Snare
  (note-on 4 50 115)
  (sleep 0.25)
  })

;(looprange i 0 10 
;           (beat)
;           )


;; Play on p2
;; (note-on 2 72 127)
;; (sleep 1.5)
;; (note-off 2 72)

;; (note-on 2 64 127)
;; (sleep 1.5)
;; (note-off 2 64)

;; (note-on 2 71 127)
;; (sleep 1.5)
;; (note-off 2 71)



;; ;; Play on p1
;; (note-on 1 72 127)
;; (sleep 0.5)
;; (note-off 1 72)

;; (note-on 1 64 127)
;; (sleep 0.5)
;; (note-off 1 64)

;; (note-on 1 71 127)
;; (sleep 0.5)
;; (note-off 1 71)




;; ;; Play on "violin"
;; (note-on 0 64 90)  ;; E4
;; (sleep 0.5)
;; (note-off 0 64)

;; (note-on 0 67 95)  ;; G4
;; (sleep 0.5)
;; (note-off 0 67)

;; (note-on 0 71 100) ;; B4
;; (sleep 0.8)
;; (note-off 0 71)

(defun midi-thd ()
  (loopwhile t {
        (match (midi-read)
               (nil nil)
               ((note-on (? n) (? v)) {
                (print "note on: " n " " v )
                (note-on 0 n v)
                })
               ((note-off (? n) _ )      (note-off 0 n))
               )
        }))

(spawn midi-thd)

(define music
    '((76 100)
      (71 100)
      (72 100)
      (74 100)
      (72 100)
      (71 100)
      (70 100)))


(defun play (x)
  (if (eq x nil) ()
      (let (((note vel) (car x))) {
           (note-on 0 note vel)
           (sleep 0.2)
           (note-off 0 note)
           (play (cdr x))
           })))


;(midi-connect 20 0) ;; I know the keyboard is there.
