(define cmds-init '(
    (0xAE)      ; Display off
    (0xD5 0x80) ; Osc freq
    (0xA8 0x3F) ; Mux ratio
    (0xD3 0x00) ; Display offset
    (0x8D 0x14) ; Char reg
    (0x81 0xCF) ; Set contrast
    (0x20 0x00) ; Memory addr mode
    (0x21 0 127) ; Column addr
    (0x22 0 7) ; Page addr
    (0x40) ; Start line
    (0xA1) ; Seg remap op
    (0xC8) ; Com scan dir op
    (0xDA 0x12) ; Com pin conf
    (0xD9 0xF1) ; Precharge
    (0xDB 0x40) ; Vcom deselect
    (0xA4) ; Dis ent disp on
    (0xA6) ; Dis normal
    (0x2E) ; Deactivate scroll
    (0xAF) ; Disaply on
    ))

(check (and (= (car (car cmds-init)) 0xAE)
            (= (car (car (cdr cmds-init))) 0xD5)
            (= (car (car (cdr (cdr cmds-init)))) 0xA8)))
