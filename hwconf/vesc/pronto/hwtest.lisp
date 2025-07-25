
(set-aux 1 0)
(set-aux 1 1)

(loopwhile-thd 100 t {
    (def v12 (hw-reg-v))
    (def v5 (hw-reg5-v))
    (def i12 (hw-reg-i))
    (def t12 (hw-reg-i))
    (sleep 0.01)
})
