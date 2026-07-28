(define width 1200)
(define height 360)

(def tab (wasm-create-tab "AA Comparison"))
(def canvas (wasm-add-canvas tab width height))

(define img (img-buffer 'indexed4 width height))

(import "https://raw.githubusercontent.com/google/fonts/main/ufl/ubuntu/Ubuntu-Regular.ttf" 'font-data)

(define chars
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

(define font-10 (ttf-prepare font-data 10 'indexed4 chars))
(define font-16 (ttf-prepare font-data 16 'indexed4 chars))
(define font-24 (ttf-prepare font-data 24 'indexed4 chars))
(define font-32 (ttf-prepare font-data 32 'indexed4 chars))

; 0 black, 1 dark grey, 2 light grey, 3 white
(define colors '(0x000000 0x555555 0xAAAAAA 0xFFFFFF))

(define aa-none '(3 3 3 3))
(define aa-low  '(1 3 3 3))
(define aa-mid  '(1 2 3 3))
(define aa-high '(0 2 3 3))
(define aa-full '(0 1 2 3))

(img-clear img 0)

; headings
(ttf-text img 20   35 aa-none font-16 "Size")
(ttf-text img 90   35 aa-none font-16 "No AA")
(ttf-text img 300  35 aa-none font-16 "Low AA")
(ttf-text img 510  35 aa-none font-16 "Mid AA")
(ttf-text img 720  35 aa-none font-16 "High AA")
(ttf-text img 930  35 aa-none font-16 "Full AA")

; row 10
(ttf-text img 20 80 aa-none font-16 "10")

(ttf-text img 90  80 aa-none font-10 "Hello, World!")
(ttf-text img 300 80 aa-low  font-10 "Hello, World!")
(ttf-text img 510 80 aa-mid  font-10 "Hello, World!")
(ttf-text img 720 80 aa-high font-10 "Hello, World!")
(ttf-text img 930 80 aa-full font-10 "Hello, World!")

; row 16
(ttf-text img 20 135 aa-none font-16 "16")

(ttf-text img 90  135 aa-none font-16 "Hello, World!")
(ttf-text img 300 135 aa-low  font-16 "Hello, World!")
(ttf-text img 510 135 aa-mid  font-16 "Hello, World!")
(ttf-text img 720 135 aa-high font-16 "Hello, World!")
(ttf-text img 930 135 aa-full font-16 "Hello, World!")

; row 24
(ttf-text img 20 205 aa-none font-16 "24")

(ttf-text img 90  205 aa-none font-24 "Hello, World!")
(ttf-text img 300 205 aa-low  font-24 "Hello, World!")
(ttf-text img 510 205 aa-mid  font-24 "Hello, World!")
(ttf-text img 720 205 aa-high font-24 "Hello, World!")
(ttf-text img 930 205 aa-full font-24 "Hello, World!")

; row 32
(ttf-text img 20 295 aa-none font-16 "32")

(ttf-text img 90  295 aa-none font-32 "Hello, World!")
(ttf-text img 300 295 aa-low  font-32 "Hello, World!")
(ttf-text img 510 295 aa-mid  font-32 "Hello, World!")
(ttf-text img 720 295 aa-high font-32 "Hello, World!")
(ttf-text img 930 295 aa-full font-32 "Hello, World!")

(disp-render img 0 0 colors)
