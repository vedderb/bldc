(define  wait-for-button (lambda () {
                           (print "Press the button...")
                           (gpio-wait)
                           (print "Button pressed!")
                           (print "Count: " (button-count))
                           }))
