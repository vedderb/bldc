(hw-reg-en 1)
(hw-reg-adj 1900)

(loopwhile-thd 100 t {
        (def reg-v (hw-reg-v))
        (def reg-i (hw-reg-i))
        (def reg-t (hw-reg-t))
        (def reg5-v (hw-reg5-v))
        (def reg-pwr (* (hw-reg-v) (hw-reg-i)))

        (sleep 0.01)
})

(set-aux 1 0)

(def sample-now 0)
(def samples (range (* 90 3)))

(defun plot-v (graph sample) {
        (setix samples sample-now (list graph (secs-since t-start) sample))
        (setq sample-now (+ sample-now 1))
})

(def t-start (systime))

(looprange i 0 10 {
        (plot-v 0 (hw-reg-v))
        (plot-v 1 (hw-reg-i))
        (plot-v 2 (* (hw-reg-i) (hw-reg-v)))
        (sleep 0.002)
})

(set-aux 1 1)

(looprange i 0 80 {
        (plot-v 0 (hw-reg-v))
        (plot-v 1 (hw-reg-i))
        (plot-v 2 (* (hw-reg-i) (hw-reg-v)))
        (sleep 0.002)
})

(set-aux 1 0)

(plot-init "Time" "Value")
(plot-add-graph "V-Reg")
(plot-add-graph "I-Reg")
(plot-add-graph "Pwr-Reg")

(loopforeach i samples {
        (plot-set-graph (ix i 0))
        (plot-send-points (ix i 1) (ix i 2))
})
