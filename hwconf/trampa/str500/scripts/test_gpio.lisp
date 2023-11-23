(gpio-configure 2 'pin-mode-out)
(gpio-configure 3 'pin-mode-out)
(gpio-configure 10 'pin-mode-out)
(gpio-configure 8 'pin-mode-out)

(gpio-write 2 0)
(gpio-write 3 0)
(gpio-write 10 0)
(gpio-write 8 0)
(can-cmd 93 "(set-aux 1 0)")

;(gpio-write 8 1)
;(can-cmd 93 "(set-aux 1 1)")

(loopwhile t {
        (gpio-write 2 1)
        (sleep 1.0)
        (gpio-write 2 0)
        (sleep 1.0)
        
        (gpio-write 3 1)
        (sleep 1.0)
        (gpio-write 3 0)
        (sleep 1.0)
        
        (gpio-write 10 1)
        (sleep 1.0)
        (gpio-write 10 0)
        (sleep 1.0)
        
        (gpio-write 2 1)
        (sleep 1.0)
        (gpio-write 3 1)
        (sleep 1.0)
        (gpio-write 10 1)
        (sleep 2.0)
        
        (gpio-write 2 0)
        (gpio-write 3 0)
        (gpio-write 10 0)
        (sleep 1)
})
