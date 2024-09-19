


(display-to-image)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-image render-target)
(disp-clear)

(define create_image1
  (ref-entry "img-buffer"
             (list
              (para (list "Allocate an image buffer from lbm memory or from a compactible region."
                          "The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`."
                          ))
              (code '((define my-img (img-buffer 'indexed2 320 200))
                      ))
              (program '(((define my-dm (dm-create 10000))
                          (define my-img (img-buffer my-dm 'indexed2 320 200))
                          )
                         ))
              end)))

(define arcs
    (ref-entry "arcs"
	       (list
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-arc my-img 100 100 50 160 100 1)
			    (img-arc my-img 100 100 50 160 100 1 '(dotted 15 15))
			    (img-arc my-img 100 100 50 160 100 1 '(filled))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10))
			    (img-arc my-img 100 100 50 160 100 1 '(rounded))
			    ))
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10) '(rounded))
			  ))
		end)))


(define circles
  (ref-entry "circles"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle my-img 100 100 80 1)
                          (img-circle my-img 100 100 80 1 '(thickness 5))
                          (img-circle my-img 100 100 80 1 '(dotted 14 14))
                          (img-circle my-img 100 100 80 1 '(filled))
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle my-img 100 100 80 1 '(dotted 14 14) '(resolution 6))
                          ))
              end)))

(define circle-sectors
  (ref-entry "circle sectors"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-sector my-img 220 40 40 90 200 1)
                          (img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3))
                          ))
              end)))

(define circle-segments
  (ref-entry "circle segments"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-segment my-img 100 100 80 0 100 1)
                          (img-circle-segment my-img 100 100 80 0 100 1 '(filled))
                          ))
              end)))


(define lines
    (ref-entry "lines"
	       (list
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-line my-img 0 0 320 200 1)
                            (img-line my-img 0 200 320 0 1 '(dotted 4 20))
			    ))
		end)))
	       


(define manual
  (list
   (section 1 "LispBM Display Reference Manual"
            (list create_image1
		  arcs
                  circles
                  circle-sectors
                  circle-segments
		  lines)
            )
   )
  )

(defun render-manual ()
  (let ((h (fopen "displayref.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    {
    (var t0 (systime))
    (render r manual)
    (print "Display reference manual was generated in " (secs-since t0) " seconds")
    }
    )
  )
