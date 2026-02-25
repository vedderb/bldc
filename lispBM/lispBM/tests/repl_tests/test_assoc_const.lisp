@const-start

(def data-load '(
        (v-in . 0.0)
))

@const-end

(setassoc data-load 'v-in 10.0)

(if (= (assoc data-load 'v-in) 0.0)
    (print "SUCCESS")
    (print "FAILURE"))
