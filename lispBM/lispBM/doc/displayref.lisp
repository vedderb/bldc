

(define font-file (fopen "font_15_18.bin" "r"))
(define font (load-file font-file))


(define llama-file (fopen "images/lama2.bin" "r"))
(define llama-bin (load-file llama-file))


(display-to-image)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-image render-target)
(disp-clear)

(define img-rgb888 (img-buffer 'rgb888 320 200))

;(define small (img-buffer 'rgb888 10 10)) 

(define img-100-100 (img-buffer 'indexed2 100 100))

(img-blit img-100-100 llama-bin 0 0 -1 '(scale 0.3))

(defun code-disp-str (xs) (code-disp (map (lambda (x) (list 'read-eval x)) xs)))
(defun code-png-str (img c xs) (code-png img c (map (lambda (x) (list 'read-eval x)) xs)))

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

(define image-from-bin
  (ref-entry "img-buffer?"
             (list
              (para (list "Checks if the argument is likely to be an image buffer."
                          ))
              (code '((img-buffer? llama-bin)
		      (img-buffer? 'apa)
                      ))
              end)))

(define arcs
    (ref-entry "img-arc"
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
  (ref-entry "img-circle"
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
  (ref-entry "img-circle-sector"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-sector my-img 220 40 40 90 200 1)
                          (img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3))
                          ))
              end)))

(define circle-segments
  (ref-entry "img-circle-segment"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-segment my-img 100 100 80 0 100 1)
                          (img-circle-segment my-img 100 100 80 0 100 1 '(filled))
                          ))
              end)))

(define lines
  (ref-entry "img-line"
	     (list
	      (code-png 'my-img '(0x00 0xffffff)
			'((img-line my-img 0 0 320 200 1)
                          (img-line my-img 0 200 320 0 1 '(thickness 5))
                          (img-line my-img 0 0 320 200 1 '(dotted 4 20))
			  ))
	      end)))

(define rectangles
  (ref-entry "img-rectangle"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-rectangle my-img 10 10 120 180 1)
                          (img-rectangle my-img 10 10 120 180 1 '(filled))
                          (img-rectangle my-img 10 10 120 180 1 '(rounded 45))
                          ))
              end)))

(define texts
  (ref-entry "img-text"
             (list
              (code-png-str 'my-img '(0x00 0xffffff)
                            '("(img-text my-img 40 40 1 0 font \"LispBM\")"
                              "(img-text my-img 40 120 1 0 font \"LispBM\" 'up)"
                              "(img-text my-img 40 40 1 0 font \"LispBM\" 'down)"
                              ))
              end)))

(define setpixel
  (ref-entry "img-setpix"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-setpix my-img 10 10 1)
                          ))
              end)))

(define triangles
  (ref-entry "img-triangle"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-triangle my-img 30 60 160 120 10 180 1)
                          (img-triangle my-img 30 60 160 120 10 180 1 '(filled))
                          (img-triangle my-img 30 60 160 120 10 180 1 '(dotted 14 14))
                          ))
              end)))


(define blitting
  (ref-entry "img-blit"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-blit my-img llama-bin 10 10 -1)
                          (img-blit my-img llama-bin 10 10 -1 '(rotate 128 128 45))
                          (img-blit my-img llama-bin 10 10 -1 '(scale 0.5))
                          ))
              end)))

(define sierpinsky
  (ref-entry "Example: Sierpinsky triangle"
             (list
              (program-disp '(((define w 320)
                               (define h 200)
                               (define corners (list (cons 10 (- h 10))
                                                     (cons (- w 10) (- h 10))
                                                     (cons (/ w 2) 10)))
                               (define s-img (img-buffer 'indexed2 w h))
                               (defun point (p) (img-setpix s-img (car p) (cdr p) 1))
                               (defun mid-point (p1 p2)
                                 { (let ((x (/ (+ (car p1) (car p2)) 2))
                                         (y (/ (+ (cdr p1) (cdr p2)) 2)))
                                     (cons x y))
                                 })
                               (defun sierp (n corners p)
                                 (if (= n 0) ()
                                   (let ((i (mod (rand) 3))
                                         (target (ix corners i))
                                         (mid    (mid-point p target)))
                                     { (point mid)
                                       (sierp (- n 1) corners mid)
                                       })))
                               (sierp 25000 corners (car corners))
                               (disp-render s-img 0 0 '(0x000000 0xFFFFFF))
                               ))
                            )
             end)))



(define manual
  (list
   (section 1 "LispBM Display Library"
            (list 
	     (para (list "The display extensions contains a graphics library designed"
			 "for platforms for with very limited memory resources."
			 "The drawing routines in the library operate on rectangular images (arrays) of pixels"
			 ", called an image buffer."
			 ))
	     (para (list "The values stored in image buffers represents colors via an encoding"
			 "determined by the image buffer pixel format. A pixel buffer has one of"
			 "the following formats:"
			 ))
	     (bullet '("indexed2 : 2 colors (1 bit per pixel)"
		       "indexed4 : 4 colors (2 bits per pixel)"
		       "indexed16 : 16 colors (4 bits per pixel)"
		       "rgb332 : 8Bit color"
		       "rgb565 : 16bit color"
		       "rgb888 : 24bit color"
		       ))
	     (para (list "Note that the RAM requirenment of a 100x100 image is;"
			 ))
	     (bullet '("at indexed2: 1250 Bytes"
		       "at indexed4: 2500 Bytes"
		       "at indexed16: 5000 Bytes"
		       "at rgb332: 10000 Bytes"
		       "at rgb565: 20000 Bytes"
		       "at rgb888; 30000 Bytes"
		       ))
	     (para (list "So on an embedded platform you most likely not be able to be"
			 "working with rgb565, rgb888 other than in very limited areas."
			 ))
	     (para (list "At the low-level end of things you will want to display graphics"
			 "onto an display. The interface towards the low-level end needs to"
			 "be implemented for the particular hardware platform and display."
			 "For examples of this see [vesc_express](https://github.com/vedderb/vesc_express/tree/main/main/display)."
			 "The LBM linux REPL has SDL and png backends for the display library."
			 ))
	     (para (list "the display library is specifically designed to allow for using many"
			 "colors simultaneously on screen, without needing to use full screen high-color"
			 "buffers."
			 "This is done by delaying the choice of collor mapping in the `indexed2`, `indexed4` and `indexed16`"
			 "images until they are presented on screen."
			 ))
	     (para (list "images are rendered onto a display using the function `disp-render`."
			 "`disp-render` takes an image, a position (x,y) where to draw the image, and a colormapping"
			 "that can be expressed as a list of colors."
			 "for example:"
			 ))
	     (code-disp-str '("(disp-render llama-bin 10 10 '(0x000000 0xFFFFFF))"
			      "(disp-render llama-bin 20 20 '(0x000000 0xFF0000))"
			      "(disp-render llama-bin 30 30 '(0x000000 0x00FF00))"
			      "(disp-render llama-bin 30 30 '(0x000000 0x0000FF))"
			      "(disp-clear)"
			      ))

	     (code-disp-str '("(disp-render img-100-100 0 0 '(0x000000 0xFFFFFF))"
			      "(disp-render img-100-100 0 100 '(0x000000 0xFF0000))"
			      "(disp-render img-100-100 100 0 '(0x000000 0x00FF00))"
			      "(disp-render img-100-100 100 100 '(0x000000 0x0000FF))"
			      "(disp-render img-100-100 200 0 '(0x000000 0x00FFFF))"
			      "(disp-render img-100-100 200 100 '(0x000000 0xFF00FF))"
			      ))
	
	     
	     
	    end ))
   (section 1 "Reference"
            (list create_image1
                  image-from-bin
                  blitting
		  arcs
                  circles
                  circle-sectors
                  circle-segments
		  lines
                  rectangles
                  setpixel
                  texts
                  triangles
                  )
            )
   (section 1 "Examples"
            (list sierpinsky))
   info
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
