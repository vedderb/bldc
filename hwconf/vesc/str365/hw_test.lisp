(hw-reg-en 1) ; Enable main regulator
(hw-reg-adj 2048) ; Adjust output voltage (TODO: better interface)

; High voltage switch on and off in thread
(loopwhile-thd 100 t {
        (hw-sw-hv 1)
        (sleep 1)
        (hw-sw-hv 0)
        (sleep 1)
})

(loopwhile t {
        (def reg-v (hw-reg-v)) ; Main regulator voltage
        (def reg-i (hw-reg-i)) ; Main regulator current
        (def reg-t (hw-reg-t)) ; Main regulator temperature
        (def reg5-v (hw-reg5-v)) ; 5V regulator voltage

        (sleep 0.01)
})
