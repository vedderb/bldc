
(set-pic-prefix "disp")

(define font-file (fopen "font_15_18.bin" "r"))
(define font (load-file font-file))


(define llama-file (fopen "images/lama2.bin" "r"))
(define llama-bin (load-file llama-file))


(display-to-img)
(define render-target (img-buffer 'rgb888 320 200))
(set-active-img render-target)
(disp-clear)

(define img-rgb888 (img-buffer 'rgb888 320 200))

;(define small (img-buffer 'rgb888 10 10)) 

(define img-100-100 (img-buffer 'indexed2 100 100))

(img-blit img-100-100 llama-bin 0 0 -1 '(scale 0.3))

(defun code-disp-str (xs) (code-disp (map (lambda (x) (list 'read-eval x)) xs)))
(defun code-png-str (img c xs) (code-png img c (map (lambda (x) (list 'read-eval x)) xs)))


;; VESC style import emulator
(define import (macro (file sym)
                      `(define ,(eval sym) (load-file (fopen ,file "r")))))


(define create_image1
  (ref-entry "img-buffer"
             (list
              (para (list "Allocate an image buffer from lbm memory or from a compactable region."
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


(define entry-img-dims
  (ref-entry "img-dims"
             (list
              (para (list "`img-dims returns the width and height of an image."
                          "The form of an `img-dims` expression is `(img-dims image)`."
                          ))
              (code-png 'my-img '(0x00 0xffffff)
			'((img-dims my-img)
			  ))
              end))
  )

(define arcs
    (ref-entry "img-arc"
	       (list
                (para (list "Draw an arc into an image."
                            "The form of an `img-arc` expression is `(img-arc image cx cy r ang-s ang-e color ..option)`."
                            ))
                (para (list "|Arg || \n"
                            "|----|----|\n"
                            "`image` | An image buffer for example created using img-buffer.\n"
                            "`cx cy` | Center point x,y.\n"
                            "`r`     | Radius.\n"
                            "`ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).\n"
                            "`color` | Color value, range determined by image buffer color depth.\n"
                            ))
                (para (list "<br>"))
                (para (list "|Option      || \n"
                            "|----|----|\n"
                            "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                            "`filled`     | Filled, no arguments.\n"
                            "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                            "`rounded`    | Rounded edges, no argument.\n"
                            "`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                            ))
		(code-png 'my-img '(0x00 0xffffff)
			  '((img-arc my-img 100 100 50 160 100 1)
			    (img-arc my-img 100 100 50 160 100 1 '(dotted 15 15))
			    (img-arc my-img 100 100 50 160 100 1 '(filled))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10))
			    (img-arc my-img 100 100 50 160 100 1 '(rounded))
			    (img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
			    (img-arc my-img 100 100 50 160 100 1 '(thickness 10) '(rounded))
			  ))
		end)))


(define circles
  (ref-entry "img-circle"
             (list
              (para (list "Draw a circle into an image."
                          "The form of an `img-circle` expression is `(img-circle image cx cy r color ..option)`."
                          ))
               (para (list "|Arg || \n"
                            "|----|----|\n"
                            "`image` | An image buffer for example created using img-buffer.\n"
                            "`cx cy` | Center point x,y.\n"
                            "`r`     | Radius.\n"
                            "`color` | Color value, range determined by image buffer color depth.\n"
                            ))
               (para (list "<br>"))
               (para (list "|Option      || \n"
                           "|----|----|\n"
                           "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                           "`filled`     | Filled, no arguments.\n"
                           "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                           "`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                           ))
               (code-png 'my-img '(0x00 0xffffff)
                         '((img-circle my-img 100 100 80 1)
                           (img-circle my-img 100 100 80 1 '(thickness 5))
                           (img-circle my-img 100 100 80 1 '(dotted 14 14))
                           (img-circle my-img 100 100 80 1 '(filled))
                           (img-circle my-img 100 100 80 1 '(dotted 14 14) '(resolution 6))
                          ))
               end)))

(define circle-sectors
  (ref-entry "img-circle-sector"
             (list
              (para (list "Draw a circle sector into an image."
                          "The form of an `img-circle-sector` expression is `(img-circle-sector image cx cy r ang-s ang-e color ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`cx cy` | Center point x,y.\n"
                          "`r`     | Radius.\n"
                          "`ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).\n"
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option      || \n"
                          "|----|----|\n"
                          "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                          "`filled`     | Filled, no arguments.\n"
                          "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                          ;;"`rounded`    | Rounded edges, no arguments.\n"
                          ;;"`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-sector my-img 220 40 40 90 200 1)
                          (img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3))
                          ;;(img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3) '(rounded))
                          (img-circle-sector my-img 220 40 40 90 200 1 '(dotted 1 4))
                          (img-circle-sector my-img 220 40 40 90 200 1 '(filled))
                          ;;(img-circle-sector my-img 220 40 40 90 200 1 '(resolution 2))
                          ))
              end)))

(define circle-segments
  (ref-entry "img-circle-segment"
             (list
              (para (list "Draw a circle segment into an image."
                          "The form of an `img-circle-segment` expression is `(img-circle-segment image cx cy r ang-s ang-e color ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`cx cy` | Center point x,y.\n"
                          "`r`     | Radius.\n"
                          "`ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).\n"
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option      || \n"
                          "|----|----|\n"
                          "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                          "`filled`     | Filled, no arguments.\n"
                          "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                          ;;"`rounded`    | Rounded edges, no arguments.\n"
                          ;;"`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-circle-segment my-img 100 100 80 0 100 1)
                          (img-circle-segment my-img 100 100 80 0 100 1 '(filled))
                          (img-circle-segment my-img 100 100 80 0 100 1 '(thickness 5))
                          (img-circle-segment my-img 100 100 80 0 100 1 '(dotted 1 4))
                          ))
              end)))

(define clear-image
  (ref-entry "img-clear"
             (list
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-clear my-img)
                          (img-clear my-img 1)
                          (img-clear my-img 0)
                          ))
              end)))

(define color1 0xFF0000)
(define color2 0x0000FF)

(define create-color
  (ref-entry "img-color"
             (list
              (para (list "img-color is used to create more complex color objects for"
                          "use together with disp-render.")
                    )            
              (bullet (list "**gradient_x**: vertical gradients from color1 to color2."
                            "**gradient_y**: horizontal gradients from color1 to color2."
                            "**gradient_x_pre**: precomputes gradient."
                            "**gradient_y_pre**: precomputes gradient."))
  
              (code '((read-eval "(img-color 'regular 0xAABB11)")
                      (read-eval "(img-color 'gradient_x color1 color2 10 0 'repeat)")
                      (read-eval "(img-color 'gradient_x_pre color1 color2)")
                      ))
              (program-disp '((
                               (define fptr (fopen "images/lama2.bin" "r"))
                               (define pic (load-file fptr))
                               (fclose fptr)
                               (define c (img-color 'gradient_x color1 color2 100 0 'repeat))
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 100 0 (list (img-color 'regular 0) c))
                               )))
              (program-disp '((
                               (define fptr (fopen "images/lama2.bin" "r"))
                               (define pic (load-file fptr))
                               (fclose fptr)
                               (define c (img-color 'gradient_y color1 color2 100 0 'mirrored))
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 100 0 (list (img-color 'regular 0) c))
                               )))
              end)))

;;                  entry-color-set
;;                  entry-color-get
;;                  entry-color-setpre
;;                  entry-color-getpre

(define my-color (img-color 'gradient_y color1 color2 10 0 'repeat))

(define entry-color-set
  (ref-entry "img-color-set"
             (list
              (para (list "With `img-color-set`you can set properties of a color."
                          "The form of a img-color-set expression is `(img-color-set color prop value)`"
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`color` | Color value creted with img-color.\n"
                          "`property` | Symbol denoting property to change.\n"
                          "`value`    | New value to set property to.\n"
                          ))
              (para (list "|`img-color-set` | regular | gradient | pre |\n"
                          "|----|----|----|----|\n"
                          "`color0`      | ✓ | ✓ | ✗ |\n"
                          "`color1`      | ✗ | ✓ | ✗ |\n"
                          "`width`       | ✗ | ✓ | ✗ |\n"
                          "`offset`      | ✗ | ✓ | ✓ |\n"
                          "`repeat-type` | ✗ | ✓ | ✓ |\n"
                          ))
              (code '((img-color-set my-color 'repeat-type 'mirrored)
                      (img-color-set my-color 'color-0 0xFF00FF)
                      (img-color-set my-color 'color-1 0x00FF00)
                      (img-color-set my-color 'width 10)
                      (img-color-set my-color 'offset 1)
                      ))
              )))

(define entry-color-get
  (ref-entry "img-color-get"
             (list
              (para (list "With `img-color-get` you can access properties of a color."
                          "The form of an img-color-get expression is `(img-color-get color prop)`"
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`color` | Color value creted with img-color.\n"
                          "`property` | Symbol denoting property to access.\n"
                          ))
              (para (list "|`img-color-get` | regular | gradient | pre |\n"                 
                          "|----|----|----|----|\n"
                          "`color0`      | ✓ | ✓ | ✓ |\n"                                   
                          "`color1`      | ✗ | ✓ | ✓ |\n"
                          "`width`       | ✗ | ✓ | ✓ |\n"
                          "`offset`      | ✗ | ✓ | ✓ |\n"
                          "`repeat-type` | ✗ | ✓ | ✓ |\n"
                          ))
              (code '((img-color-get my-color 'repeat-type)
                      (img-color-get my-color 'color-0)
                      (img-color-get my-color 'color-1)
                      (img-color-get my-color 'width)
                      (img-color-get my-color 'offset)
                      ))
             )))

(define my-color-pre (img-color 'gradient_x_pre color1 color2 100 0 'mirrored))

(define entry-color-setpre
  (ref-entry "img-color-setpre"
             (list
              (para  (list "Update a value in a precalculated gradient color."
                           "The form of an `img-color-setpre` expression is `(img-color-setpre color pos color-val)`."
                           ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`color` | Color value creted with img-color.\n"
                          "`pos`   | Position in the precomputed colormap to update.\n"
                          "`color-val` | Color value to write into the position.\n"
                          ))   
             (code '((img-color-setpre my-color-pre 10 0xFFFFFF)
                     (img-color-setpre my-color-pre 11 0x000000)
                     ))
                           (program-disp '((
                               (define fptr (fopen "images/lama2.bin" "r"))
                               (define pic (load-file fptr))
                               (fclose fptr)
                               (define c (img-color 'gradient_x_pre color1 color2 100 0 'repeat))
                               (loopfor i 0 (< i 512) (+ i 2) {
                                        (img-color-setpre c i 0xFFFFFF)
                                        (img-color-setpre c (+ i 1) 0x00000)
                                        })
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 100 0 (list (img-color 'regular 0) c))
                               )))
              (program-disp '((
                               (define fptr (fopen "images/lama2.bin" "r"))
                               (define pic (load-file fptr))
                               (fclose fptr)
                               (define c (img-color 'gradient_y_pre color1 color2 200 0 'repeat))
                               (loopfor i 0 (< i 200) (+ i 10) {
                                        (var band-color (if (= (mod (/ i 10) 2) 0) color1 color2))
                                        (loopfor j 0 (< j 10) (+ j 1) {
                                                 (img-color-setpre c (+ i j) band-color)
                                                 })
                                        })
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 100 0 (list (img-color 'regular 0) c))
                               )))
             )))

(define entry-color-getpre
  (ref-entry "img-color-getpre"
             (list
              (para  (list "Get a value from a precalculated gradient color."
                           "The form of an `img-color-getpre` expression is `(img-color-getpre color pos)`."
                           ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`color` | Color value creted with img-color.\n"
                          "`pos`   | Position in the precomputed colormap to update.\n"
                          ))
             (code '((img-color-getpre my-color-pre 10)
                     (img-color-getpre my-color-pre 11)
                     (img-color-getpre my-color-pre 12)
                     ))
             )))

(define lines
  (ref-entry "img-line"
	     (list
              (para (list "Draw a line into an image."
                          "The form of an `img-line` expression is `(img-line image x1 y1 x2 y2 color ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x1 y1` | Start point  x,y.\n"
                          "`x2 y2` | End point  x,y.\n"
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option      || \n"
                          "|----|----|\n"
                          "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                          "`filled`     | Filled, no arguments.\n"
                          "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                          ;;"`rounded`    | Rounded edges, no arguments.\n"
                          ;;"`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                          ))
	      (code-png 'my-img '(0x00 0xffffff)
			'((img-line my-img 0 0 320 200 1)
                          (img-line my-img 0 200 320 0 1 '(thickness 5))
                          (img-line my-img 0 0 320 200 1 '(dotted 4 20))
			  ))
	      end)))

(define rectangles
  (ref-entry "img-rectangle"
             (list
              (para (list "Draw a rectangle into an image."
                          "The form of an `img-rectangle` expression is `(img-rectangle image x1 y1 w h color ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x1 y1` | Top left corner  x,y.\n"
                          "`w h`   | Width and height.\n"
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option      || \n"
                          "|----|----|\n"
                          "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                          "`filled`     | Filled, no arguments.\n"
                          "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                          "`rounded`    | Rounded edges, one argument rounded corner angle.\n"
                          ;;"`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-rectangle my-img 10 10 120 180 1)
                          (img-rectangle my-img 10 10 120 180 1 '(filled))
                          (img-rectangle my-img 10 10 120 180 1 '(rounded 45))
                          ))
              end)))

(define texts
  (ref-entry "img-text"
             (list
              (para (list "Draw text into an image."
                          "The form of an `img-text` expression is `(img-text image x1 y1 fg bg font)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x1 y1` | Position  x,y.\n"
                          "`fg bg` | Foreground and Background color.\n"
                          "`font` | font to use. This should be a bin type font as created by for example VESC Tool."
                          "The font file can be `imported` or loaded depending on platform\n"
                          ))
              (code-png-str 'my-img '(0x00 0xffffff)
                            '("(img-text my-img 40 40 1 0 font \"LispBM\")"
                              "(img-text my-img 40 120 1 0 font \"LispBM\" 'up)"
                              "(img-text my-img 40 40 1 0 font \"LispBM\" 'down)"
                              ))
              end)))

(define setpixel
  (ref-entry "img-setpix"
             (list
              (para (list "Draw a pixel into an image."
                          "The form of an `img-setpix` expression is `(img-setpix image x y color)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x y`   | Position  x,y.\n"
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-setpix my-img 10 10 1)
                          ))
              end)))

(define getpixel
  (ref-entry "img-getpix"
             (list
              (para (list "Get a pixel value from an image."
                          "The form of an `img-getpix` expression is `(img-getpix image x y)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x y`   | Position  x,y.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-getpix my-img 10 10)
                          ))
              end)))


(define triangles
  (ref-entry "img-triangle"
             (list
               (para (list "Draw a triangle into an image."
                          "The form of an `img-triangle` expression is `(img-triangle image x1 y1 x2 y2 x3 y3 color ..option)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image` | An image buffer for example created using img-buffer.\n"
                          "`x1 y1` | Position first point  x,y.\n"
                          "`x2 y2` | Position second point  x,y.\n"
                          "`x3 y3` | Position third point  x,y.\n"                         
                          "`color` | Color value, range determined by image buffer color depth.\n"
                          ))
              (para (list "<br>"))
              (para (list "|Option      || \n"
                          "|----|----|\n"
                          "`dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.\n"
                          "`filled`     | Filled, no arguments.\n"
                          "`thickness`  | Thickness of line, one argument specifying thickness in pixels.\n"
                          ;;"`rounded`    | Rounded edges, one argument rounded corner angle.\n"
                          ;;"`resolution` | One argument, Number of points that are connected into an arc using line segments.\n"
                          ))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-triangle my-img 30 60 160 120 10 180 1)
                          (img-triangle my-img 30 60 160 120 10 180 1 '(thickness 5))
                          (img-triangle my-img 30 60 160 120 10 180 1 '(filled))
                          (img-triangle my-img 30 60 160 120 10 180 1 '(dotted 14 14))
                          ))
              end)))


(define blitting
  (ref-entry "img-blit"
             (list
              (para (list "```clj\n (img-blit dest src x y transparent ..option)\n```"))
              (para (list "Copy pixels from `src` to `dest`. " 
                          " `x` and `y` are coordinates in `dest`."
                          "Pixels colored `transparent` in `src` will be skipped"
                          "`transparent` can be set to `-1` to indicate no transparency"))
              (para (list "|Options||\n"
                          "|----|----|\n"
                          "`'(rotate x y deg)` | Rotate `deg` degrees around `x` `y`\n"
                          "`'(scale s)` | Scale by `s`\n"
                          "`'(tile)` | Tile to fill `dest`\n"
                          "`'(clip x y w h)`  | Clip output in destination coords"))
              (code-png 'my-img '(0x00 0xffffff)
                        '((img-blit my-img llama-bin 10 10 -1)
                          (img-blit my-img llama-bin 10 10 -1 
                            '(rotate 128 128 45))
                          (img-blit my-img llama-bin 10 10 -1 
                            '(scale 0.5))
                          (img-blit my-img llama-bin 10 10 -1 
                            '(tile) 
                            '(scale 0.2))
                          (read-eval "(img-blit my-img llama-bin 10 10 -1\n    '(tile)\n    '(scale 0.2)\n    '(rotate 10 10 45))")
                          (read-eval "(img-blit my-img llama-bin 10 10 -1\n    '(tile)\n    '(scale 0.2)\n    '(rotate 10 10 45)\n    '(clip 50 50 250 150))")
                          
                        ))
              end)))

(define sierpinski
  (ref-entry "Example: Sierpinski triangle"
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

(define rotated-llama
  (ref-entry "Example: rotated llama"
             (list
              (program-disp '((
                               (import "images/lama2.bin" 'pic)
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 0 0 '(0x000000 0xFF0000))
                               ))
                            )
              (para (list "Note that `import` is a feature of the VESC integration of LispBM"
                          "and not really a part of core LispBM."
                          "The LispBM REPL does not have an import feature currently."
                          ))
              (para (list "In the \"Desktop\" LispBM REPL the rotated llama examples looks"
                          "as follows."
                          ))
              (program-disp '((
                               (define pic (load-file (fopen  "images/lama2.bin" "r")))
                               (define img (img-buffer 'indexed2 320 200))
                               (img-blit img pic 10 10 -1 '(rotate 128 128 45))
                               (disp-render img 100 0 '(0x000000 0xFF0000))
                               )
                              (
                               (disp-clear)
                               (define pic (load-file (fopen  "images/lama2.bin" "r")))
                               (define img128x128 (img-buffer 'indexed2 128 128))
                               (img-blit img128x128 pic 0 0 -1 '(scale 0.5) '(rotate 128 128 45))
                               (disp-render img128x128 10 10 '(0x000000 0xFF0000))
                               (img-clear img128x128)
                               (img-blit img128x128 pic 0 0 -1 '(scale 0.5) '(rotate 128 128 -45))
                               (disp-render img128x128 148 10 '(0x000000 0x00FF00))
                               ))
                            )
              (program-gif '( (100 ((define pic (load-file (fopen  "images/lama2.bin" "r")))
                                    (define img (img-buffer 'indexed2 128 128))
                                    (define m (/ 360.0 100.0))
                                    (disp-clear)
                                    (loopfor i 0 (< i 100) (+ i 1) {
                                             (var rot (list 'rotate 128 128 (* i m)))
                                             (img-blit img pic 0 0 -1 '(scale 0.5) rot)
                                             (disp-render-mac img 10 10 '(0x000000 0xFF0000))
                                             })
                                    )
                                   )
                              ))
              end)))

(let ((fptr (fopen "lispbm.jpeg" "r")))
  {
  (define my-jpg (load-file fptr))
  (fclose fptr)
  })
                  


(define entry-disp-render
  (ref-entry "disp-render"
             (list
              (para (list "An image is drawn onto a display using `disp-render`."
                          "The form of a `disp-render` expression is `(disp-render image x y color-list)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`image`      | An image buffer for example created using img-buffer.\n"
                          "`x y`        | position of top left corner x,y.\n"
                          "`color-list` | List of Color value, hex or color values.\n"
                          ))
	      (code-disp-str '("(disp-render llama-bin 10 10 '(0x000000 0xFFFFFF))"
			       "(disp-render llama-bin 20 20 '(0x000000 0xFF0000))"
			       "(disp-render llama-bin 30 30 '(0x000000 0x00FF00))"
			       "(disp-render llama-bin 30 30 '(0x000000 0x0000FF))"
			       ))

	      (code-disp-str '("(disp-render img-100-100 0 0 '(0x000000 0xFFFFFF))"
			       "(disp-render img-100-100 0 100 '(0x000000 0xFF0000))"
			       "(disp-render img-100-100 100 0 '(0x000000 0x00FF00))"
			       "(disp-render img-100-100 100 100 '(0x000000 0x0000FF))"
			       "(disp-render img-100-100 200 0 '(0x000000 0x00FFFF))"
			       "(disp-render img-100-100 200 100 '(0x000000 0xFF00FF))"
			       ))
              end))
  )

(define entry-disp-render-jpg
  (ref-entry "disp-render-jpg"
             (list
              (para (list "There is a renderer specifically for JPG images."
                          "If one is considering to use JPG for images, the images are most"
                          "likely larger than what is easy to handle with image buffers."
                          "The `disp-render-jpg` decompresses a JPG in small chunks and outputs"
                          "directly to the display."
                          "The form of a `disp-render-jpg` expression is `(disp-render-jpg jpg-data x y)`."
                          ))
              (para (list "|Arg || \n"
                          "|----|----|\n"
                          "`jpg-data`   | Imported or otherwise loaded jpg data.\n"
                          "`x y`        | position of top left corner x,y.\n"
                          ))
              (code-disp-str '("(disp-render-jpg my-jpg 0 0)"))
              end))
  )

(define entry-disp-clear
  (ref-entry "disp-clear"
             (list
              (para (list "Use `disp-clear` to clear the display\n."
                          "The form of a `disp-clear` expression is `(disp-clear opt-color)`."
                          "`opt-color` is an optional color value."
                          ))
              (code-disp-str '("(disp-clear)"
                               "(disp-clear 0xFF8822)"
                               "(disp-clear 0x000000)"
                               ))
	      end ))
  )


(define entry-disp-reset
  (ref-entry "disp-reset"
             (list
              (para (list "Use `disp-reset` to reset the display\n."
                          "What it means to reset a display is display-backend dependend"
                          "on an display connected over SPI to an MCU, it may mean powercycling and"
                          "running the initialization sequence of commands."
                          "The form of a `disp-reset` expression is `(disp-reset)`."
                          ))
              (code-disp-str '("(disp-reset)"
                               ))
	      end ))
  )


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
	     (para (list "Note that the RAM requirement of a 100x100 image is;"
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
			 "This is done by delaying the choice of color mapping in the `indexed2`, `indexed4` and `indexed16`"
			 "images until they are presented on screen."
			 ))
	     (para (list "images are rendered onto a display using the function `disp-render`."
			 "`disp-render` takes an image, a position (x,y) where to draw the image, and a colormapping"
			 "that can be expressed as a list of colors."
			 "for example:"
			 ))
             end))
   (section 1 "Reference"
            (list entry-disp-render
                  entry-disp-render-jpg
                  entry-disp-clear
                  entry-disp-reset
                  create_image1
                  image-from-bin
                  blitting
                  entry-img-dims
                  arcs
                  circles
                  circle-sectors
                  circle-segments
                  clear-image
                  create-color
                  entry-color-set
                  entry-color-get
                  entry-color-setpre
                  entry-color-getpre
		  lines
                  rectangles
                  setpixel
                  getpixel
                  texts
                  triangles
                  )
            )
   (section 1 "Examples"
            (list
             (para (list "These examples are leaving out the details on how to setup and initialize"
                         "any particular display you may have connected to your embedded system."
                         "For information on how to initialize a display on a VESC EXPRESS platform"
                         "see [vesc_express display documentation](https://github.com/vedderb/vesc_express/tree/main/main/display)."
                         ))
             
             sierpinski
             rotated-llama))
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
