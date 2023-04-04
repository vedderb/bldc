

(check (eq (progn
             (call-cc (lambda (k) (k))) 'nil)))
