

(display-to-image)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-image render-target)
(disp-clear)

(define img-rgb888 (img-buffer 'rgb888 320 200))
(define disp img-rgb888)

(defun code-disp-str (xs) (code-disp (map (lambda (x) (list 'read-eval x)) xs)))
(defun code-png-str (img c xs) (code-png img c (map (lambda (x) (list 'read-eval x)) xs)))


;; VESC style import emulator
(define import (macro (file sym)
                      `(define ,(eval sym) (load-file (fopen ,file "r")))))


(define font-file (fopen "Ubuntu-Regular.ttf" "r"))
(define font (load-file font-file))


(define font-ttf-prepare
  (ref-entry "ttf-prepare"
             (list
              (para (list "`ttf-prepare` intializes font and prerenders glyphs."
                          "The result of `ttf-prepare` is a binary blob conaining"
                          "all the information needed to print text using the prepared glyphs"
                          "The form of a `ttf-prepare` expression is: `(ttf-prepare font-data scale img-format utf8-str)`."
                          ))
              (bullet '("font-data : A ttf font file loaded or imported"
                        "scale : Floating point value specifying text size scaling."
                        "img-format : Prerendering format. Formats are described in the [displayref](./displayref.md)."
                        "utf8-str : A string containing the UTF8 characters to prerender."))
              (para (list "Note that only characters mentioned in the `utf-string` will be usable."
                          ))
              (code '((define b (ttf-prepare font 32 'indexed4 "helo wrd!"))
                      ))
              (para (list "Note try to not put duplicate characters in the utf8-str."
                          "Duplicate characters use extra memory temporarily which could be a problem if you are"
                          "already low on mem."
                          ))
              end)
             )
  )
                    
(define font-ttf-text
  (ref-entry "ttf-text"
             (list
              (para (list "`ttf-text` draws text on an image from the [display library](./displayref.md)."
                          "The form of an `ttf-text` expression is `(ttf-text img pos-x pos-y colors font-blob utf8-str opt-dir opt-linespacing)`."
                          ))
              (bullet '("img : A display library image."
                        "pos-x : Horizontal placement of text within the image."
                        "pos-y : Vertical placement of text within the image."
                        "colors : List of colors to use for antialiasing."
                        "font-blob : The font object created by `ttf-prepare`."
                        "utf8-str : The text to draw."
                        "opt-dir : Optional argument specifying direction up or down using symbols `up` and `down`."
                        "opt-linespacing : A binary number specifying a scaling of the linespacing."
                        ))

              (code '((define aa-red '(0x000000 0x440000 0x990000 0xFF0000))))
                        
              (code-png-str 'disp '()
                            '("(ttf-text disp 70 120 aa-red b \"hello world!\")"
                              "(ttf-text disp 20 40 aa-red b \"hello\" 'down)"
                              "(ttf-text disp 50 90 aa-red b \"hello\" 'up)"
                              "(ttf-text disp 70 90 aa-red b \"hello\\nworld!\")"
                              "(ttf-text disp 70 90 aa-red b \"hello\\nworld!\" 2.0 )"
                              ))
              end)
             )
  )

(define font-ttf-line-height
  (ref-entry "ttf-line-height"
             (list
              (para (list "Obtain line-height from a prepared font object."
                          ))
              (code '((ttf-line-height b)
                      ))
              end)
             )
  )

(define font-ttf-ascender
  (ref-entry "ttf-ascender"
             (list
              (para (list "Obtain the ascender metrics from a prepared font object."
                          ))
              (code '((ttf-ascender b)
                      ))
              end)
             )
  )

(define font-ttf-descender
  (ref-entry "ttf-descender"
             (list
              (para (list "Obtain the descender metrics from a prepared font object."
                          ))
              (code '((ttf-descender b)
                      ))
              end)
             )
  )

(define font-ttf-line-gap
  (ref-entry "ttf-line-gap"
             (list
              (para (list "Obtain the line-gap metrics from a prepared font object."
                          ))
              (code '((ttf-line-gap b)
                      ))
              end)
             )
  )

(define font-ttf-glyph-dims
  (ref-entry "ttf-glyph-dims"
             (list
              (para (list "Obtain the dimensions of a glyph from a prepared font object."
                          ))
              (code '((ttf-glyph-dims b "o")
                      ))
              end)
             )
  )

(define font-ttf-text-dims
  (ref-entry "ttf-text-dims"
             (list
              (para (list "Obtain the dimensions of a string of text rendered using a prepared font object."
                          ))
              (code '((ttf-text-dims b "hello")
                      ))
              end)
             )
  )
                    

(define font-example
  (ref-entry "Example: Using a font"
             (list
              (code '((img-clear disp)))
              (program-disp '(((import "Roboto-Regular.ttf" 'roboto)
                               (define ft (ttf-prepare roboto 32 'indexed4 "helo wrd"))
                               (define aa-green '(0x000000 0x004400 0x009900 0x00FF00))
                               (ttf-text disp 40 40 aa-green ft "hello world")
                               (disp-render disp 0 0)
                               ))
                            )
              end)
             )
  )


(define manual
  (list
   (section 1 "LispBM TTF Font Library"
            (list
             (para (list "The LispBM TTF font extensions are based on the [libschrift](https://github.com/tomolt/libschrift) library"
                         ))
	     
	     end ))
   (section 1 "Reference"
            (list
             font-ttf-prepare
             font-ttf-text
             font-ttf-line-height
             font-ttf-ascender
             font-ttf-descender
             font-ttf-line-gap
             font-ttf-glyph-dims
             font-ttf-text-dims
             )
            )
   (section 1 "Examples"
            (list
             font-example
             )
            )
   info
   )
  )

(defun render-manual ()
  (let ((h (fopen "ttfref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "TTF reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
