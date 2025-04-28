
(define pic-prefix "")

(define png-count 0)
(define gif-count 0)

(defun set-pic-prefix (s)
  (setq pic-prefix s))

(defun leading-zeroes (n)
  (if (< n 10) (str-merge "000" (to-str n))
    (if (< n 100) (str-merge "00" (to-str n))
      (if < n 1000) (str-merge "0" (to-str n))
      ( to-str n))))


(defun gif-frame (n) 
       (str-merge "./images/frame_" (leading-zeroes n) ".png"))
        

(defun png-file () {
       (var n png-count)
       (setq png-count (+ png-count 1))
       (str-merge "./images/" pic-prefix "-img" (to-str png-count) ".png")
       })

(defun gif-file () {
       (var n gif-count)
       (setq gif-count (+ gif-count 1))
       (str-merge "./images/" pic-prefix "-anm" (to-str gif-count) ".gif")
       })

(defun is-read-eval-txt (x)
  (match x
         ( (read-eval . _) true)
         (_ false)))

(defun pretty (toplevel ind c)
  (if (and toplevel (list? c))
      (pretty-toplevel-list c)
    (pretty-ind ind c)
    ))

(defun ind-spaces (n)
  (str-replicate n 32b))

(defun pretty-ind (n c)
  (match c
         ( (loopfor (? v) (? init) (? cnd) (? upd) (? body) )
           (str-merge "(loopfor " (pretty nil (+ n 9) v) " " (pretty nil (+ n 9) init) " " (pretty nil (+ n 9) cnd) " " (pretty nil (+ n 9) upd) (pretty-aligned-on-top (+ n 9) (list body)) ")"))
         ( (loopwhile (? cnd) (? body) )
           (str-merge "(loopwhile " (pretty nil (+ n 11) cnd) (pretty-aligned-on-top (+ n 11) (list body)) ")"))
         ( (disp-render-mac (? i) (? x) (? y) (? color))
           (str-merge "(disp-render " (pretty nil (+ n 13) i) " " (pretty nil (+ n 13) x) " " (pretty nil (+ n 13) y) " " (pretty nil (+ n 13) color) ")"))
         ( (fopen (? f) (? m))
           (str-merge "(fopen \"" f "\" \"" m "\")"))
         ( (import (? txt) (? sym))
           (str-merge "(import \"" txt "\" '" (to-str (eval sym)) ")"))
         ( (loop (? e) . (? es))
           (str-merge "(loop " (pretty nil (+ n 6) e) (pretty-aligned-on-top (+ n 6) es) ")" ))
         ( (atomic (? e) . (? es))
           (str-merge "(atomic " (pretty nil (+ n 8) e) (pretty-aligned-on-top (+ n 8) es) ")" ))
         ( (recv  (? e) . (? es))
           (str-merge "(recv " (pretty nil (+ n 6) e) (pretty-aligned-on-top (+ n 6) es) ")" ))
         ( (recv-to  (? e) . (? es))
           (str-merge "(recv-to " (pretty nil (+ n 9) e) (pretty-aligned-on-top (+ n 9) es) ")" ))
         ( (match (? e) . (? es))
           (str-merge "(match " (pretty nil (+ n 7) e) (pretty-aligned-on-top (+ n 7) es) ")" ))
         ( (progn (? e ) . (? es))
           (str-merge "(progn " (pretty-aligned-on-top (+ n 4) (cons e es)) ")" ))
         ( (quote (? e)) (str-merge "'" (pretty nil (+ n 1) e)))
         ( (let ((? b0) . (? brest)) (? body)) ;; pattern
           (str-merge "(let ("

                      (pretty nil (+ n 6) b0)
                      (pretty-aligned-on-top (+ n 6) brest)
                      ")\n"

                      (ind-spaces (+ n 5)) (pretty-ind (+ n 5) body)
                      ")"
                      ))
         ( (cond (? x) . (? xs) )
           (let ( (conds (pretty-aligned-on-top (+ n 6) xs))
                  (cond0 (pretty nil (+ n 6) x)))
             (str-merge "(cond " cond0 conds ")")
             )
           )
         ( (defun (? name) (? args) (? body))
           (str-merge "(defun " (pretty nil (+ n 7) name) " " (pretty nil (+ n 7) args)
                      (pretty-aligned-on-top (+ n 2) (list body)) ")"))
         ( (lambda (? args) (? body))
           (str-merge "(lambda " (pretty nil (+ n 8) args)
                      (pretty-aligned-on-top (+ n 2) (list body)) ")"))
         ( (closure (? args) . (? body-and-env))
           (str-merge "(closure " (pretty nil (+ n 9) args) " " (pretty-aligned-on-top (+ n 2) body-and-env) ")"))
        ;  ( (foldl . (? xs))
        ;    (str-merge (ind-spaces n) ("(foldl " (pretty nil args))))
         ( ((? x) . (? xs)) (str-merge "(" (pretty-list (+ n 1) (cons x xs)) ")" ))
         ( (? x) (string? x) (str-merge "\"" x "\""))
         (_ (to-str c)))
  )

(defun pretty-list (n c)
  (match c
         ( nil "" )
         ( ((? x) . nil) (str-merge (pretty nil n x) ))
         ( ((? x) . (? y))
           (let ((first-str (pretty nil n x)))
             (if (eq (type-of y) type-list)
                 (str-merge first-str " " (pretty-list (+ n 1 (str-len first-str)) y))
               (str-merge first-str " . " (pretty nil (+ n 1 (str-len first-str)) y))))
           )
         ( (? x) (str-merge " . " (pretty nil n x)))))

(defun pretty-toplevel-list (c)
  (match c
         ( nil "" )
         ( ((? x) . nil) (str-merge (pretty nil 0 x) ))
         ( ((? x) . (? y))
           (if (eq (type-of y) type-list)
               (str-merge (pretty nil 0 x) "\n" (pretty-toplevel-list y))
             (str-merge (pretty nil 0 x) " . " (pretty nil 0 y)))
           )
         ( (? x) (str-merge " . " (pretty nil 0 x)))))

(defun pretty-aligned-on-top (n cs)
  (match cs
         (nil "")
         ( ( (? x ) . (? xs))
           (str-merge "\n" (ind-spaces n) (pretty-ind n x) (pretty-aligned-on-top n xs))))
  )

(defun render-code-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                          (pretty nil 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read (ix x 1))
                           x))
                 (res (eval nil x-code))
                 (res-str (pretty nil 0 res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend res-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-code-res-pairs rend xs)
             }))))

(defun render-code-table (rend c)
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-code-res-pairs rend c)
    (rend "</table>\n\n")
    })


(defun render-code-res-raw-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                          (pretty nil 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read (ix x 1))
                           x))
                 (res (eval nil x-code))
                 (res-str (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend res-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-code-res-raw-pairs rend xs)
             }))))

(defun render-code-raw-table (rend c)
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-code-res-raw-pairs rend c)
    (rend "</table>\n\n")
    })

(defun render-code-png-pairs (rend img colors cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                            (pretty nil 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read (ix x 1))
                             x))
		 (png (png-file))
		 )
	     {
	     (img-clear img 0)
	     (var res (eval nil x-code))
	     (var res-str (to-str res))
	     (disp-render img 0 0 colors) 
	     (save-active-img png)
             (disp-clear)
	     (rend "<tr>\n")
	     (rend "<td>\n\n")
	     (rend "```clj\n")
	     (rend x-str)
	     (rend "\n```\n")
	     (rend "\n\n</td>\n")
	     ;; image
	     (rend "<td>\n\n")
	     (rend (str-merge "<img src=" png " >"))
	     (rend "\n\n</td>\n")	     
	     (rend "<td>\n\n")
	     (rend "```clj\n")
	     (rend res-str)
	     (rend "\n```\n")
	     (rend "\n\n</td>\n")
	     (rend "</tr>\n")
	     (render-code-png-pairs rend img colors xs)
	     }))))
  
(defun render-code-png-table (rend img colors c)
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Image </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-code-png-pairs rend img colors c)
    (rend "</table>\n\n")
    })

(defun render-code-disp-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                            (pretty nil 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read (ix x 1))
                             x))
		 (png (png-file))
		 )
	     {
	     (var res (eval nil x-code))
	     (var res-str (to-str res))
	     (save-active-img png)
	     (rend "<tr>\n")
	     (rend "<td>\n\n")
	     (rend "```clj\n")
	     (rend x-str)
	     (rend "\n```\n")
	     (rend "\n\n</td>\n")
	     ;; image
	     (rend "<td>\n\n")
	     (rend (str-merge "<img src=" png " >"))
	     (rend "\n\n</td>\n")	     
	     (rend "<td>\n\n")
	     (rend "```clj\n")
	     (rend res-str)
	     (rend "\n```\n")
	     (rend "\n\n</td>\n")
	     (rend "</tr>\n")
	     (render-code-disp-pairs rend  xs)
	     }))))
  
(defun render-code-disp-table (rend c)
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Image </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-code-disp-pairs rend c)
    (rend "</table>\n\n")
    })


(defun intersperse (str strings)
  (match strings
         ( ((? s) . nil) s)
         ( ((? s) . (? ss))
           (str-merge s str (intersperse str ss)))))
    

(defun tableize (strings)
  (str-merge "|" (intersperse "|" strings) "|\n" )
  )

(defun render-table (rend h d)
  {
  (rend "\n")
  (rend (tableize h))
  (rend (tableize (map (lambda (x) ":----:") h)))
  (map (lambda (s) (rend (tableize s))) d)
  (rend "\n")
  }
  )

(defun render-program-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                          (pretty 't 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read-program (ix x 1))
                           x))
                 (res (eval-program nil x-code))
                 (res-str (to-str res)))
           ;(let ((c-strings (map (lambda (c) (str-merge (pretty c) "\n"))  x))
           ;      (res (eval-program nil x))
           ;      (res-str (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
                                        ;(map rend c-strings)
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
             (rend res-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-program-res-pairs rend xs)
             }))))

(defun render-program-table (rend c)
  {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-program-res-pairs rend c)
    (rend "</table>\n\n")
    })

(defun render-program-disp-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (is-read-eval-txt x)
                            (ix x 1)
                          (pretty 't 0 x)))
                 (x-code (if (is-read-eval-txt x)
                             (read-program (ix x 1))
                           x))
                 (res (eval-program nil x-code))
                 (res-str (to-str res))
                 (png (png-file)))
             {
             (save-active-img png)
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
                                        ;(map rend c-strings)
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
              ;; image
	     (rend "<td>\n\n")
	     (rend (str-merge "<img src=" png " >"))
             (rend "\n\n</td>\n")	     
             (rend "<td>\n\n")
             (rend "\n```clj\n")
             (rend res-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-program-disp-res-pairs rend xs)
             }))))

(defun render-program-disp-table (rend c)
  {
  (rend "<table>\n")
  (rend "<tr>\n")
  (rend "<td> Example </td> <td> Image </td> <td> Result </td>\n")
  (rend "</tr>\n")
  (render-program-disp-res-pairs rend c)
  (rend "</table>\n\n")
  })


;; assumes existence of frame-i value
;; assumes existence of frame-max value
;; assumes existence of yeet continuation 
(define disp-render-mac (macro (img x y color)
  `(if (= frame-i frame-max)
       (glob-yeet nil)
     (progn (var png (gif-frame frame-i))
            (setq frame-i (+ frame-i 1))
            (disp-render ,img ,x ,y ,color)
            (save-active-img png)))))

(define frame-i 0)
(define frame-max 0)
(define glob-yeet nil)

(defun eval-animation (gif c frames)
  {
  (setq frame-i 0)
  (setq frame-max frames)
  (call-cc (lambda (yeet)
             {
             (setq glob-yeet yeet)
             (eval-program c)
             }))
  ;; done, convert to gif.
  ;; convert -delay 10 -loop 0 frame*.png animation.gif
  (var cmd (str-merge "convert -delay 10 -loop 0 images/frame*.png " gif))
  (unsafe-call-system cmd)
  (unsafe-call-system "rm images/frame*")
  }
  )

     
(defun render-program-disp-gif-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (pretty 't 0 (car (cdr x))))
                 (x-code (car (cdr x)))
                 (gif (gif-file))
                 (res (eval-animation gif x-code (car x)))
                 (res-str (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
                                        ;(map rend c-strings)
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
              ;; image
	     (rend "<td>\n\n")
	     (rend (str-merge "<img src=" gif " >"))
             (rend "\n\n</td>\n")	     
             ;;(rend "<td>\n\n")
             ;;(rend "\n```clj\n")
             ;;(rend res-str)
             ;;(rend "\n```\n")
             ;;(rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-program-disp-gif-pairs rend xs)
             }))))

(defun render-program-disp-gif-table (rend c)
  {
  (rend "<table>\n")
  (rend "<tr>\n")
  (rend "<td> Example </td> <td> Animation </td>\n")
  (rend "</tr>\n")
  (render-program-disp-gif-pairs rend c)
  (rend "</table>\n\n")
  })



(defun str-merge-list (strings)
  (match strings
         ( nil "" )
         ( ((? s) . (? ss)) (str-merge s (str-merge-list ss)))))

(defun render-it (rend ss)
  (match ss
         ( nil (rend "\n") )
         ( (section (? i) (? x) (? xs))
           {
           (match i
                  (1 (rend (str-merge "# " x "\n\n")))
                  (2 (rend (str-merge "## " x "\n\n")))
                  (3 (rend (str-merge "### " x "\n\n")))
                  (4 (rend (str-merge "#### " x "\n\n"))))
           (render rend xs)
           }
           )
         ( (tagged-section (? i) (? name) (? tags) (? strings))
           {
             (match i
               (1 (rend (str-merge "# " name "\n\n")))
               (2 (rend (str-merge "## " name "\n\n")))
               (3 (rend (str-merge "### " name "\n\n")))
               (4 (rend (str-merge "#### " name "\n\n"))))
             (rend (str-merge "> "
                              (str-join (map (fn (x) (str-merge "***" x "***"))
                                             tags)
                                        ",")
                              ; "</sup>"
                              "\n\n"))
             (render rend strings)
           })
         ( (para (? x)) { (map (lambda (s) (rend (str-merge s " "))) x) (rend "\n\n") } )
         ( (verb (? x)) { (map (lambda (s) (rend s)) x) (rend "\n") } )
         ( hline (rend "\n---\n\n"))
         ( newline (rend "\n"))
         ( (bold (? s))
           (rend (str-merge "**" s "**")))
         ( (program (? c)) (render-program-table rend c))
         ( (code (? c)) (render-code-table rend c))
         ( (code-raw (? c)) (render-code-raw-table rend c))
	 ( (code-png (? img) (? colors) (? c))
	   (render-code-png-table rend (eval img) colors c))
	 ( (code-disp  (? c))
	   (render-code-disp-table rend  c))
         ( (program-disp (? c))
           (render-program-disp-table rend c))
         ( (program-gif (? c))
           (render-program-disp-gif-table rend c))
         ( (image (? alt) (? url))
           (rend (str-merge "![" alt "](" url " \"" alt "\")\n\n")))
         ( (image-pair (? cap0) (? txt0) (? fig0) (? cap1) (? txt1) (? fig1))
           (rend (str-merge cap0 " | " cap1 "\n"
                            "|:---:|:---:|\n"
                            "![" txt0 "](" fig0 ") | ![" txt1 "](" fig1 ")\n\n")))
         ( (s-exp-graph (? img-name) (? code) (? ra) )
           {
           (render-dot img-name code)
           (if ra
               (rend (str-merge "![" ra  "](./images/" img-name ".png)\n\n"))
             (rend (str-merge "![Graph representation of s-expression](./images/" img-name ".png)\n\n"))
             )
             
           })
         ( (semantic-step (? c1) (? c2) (? p)
                          ))
         ( (table (? h) (? d))
           (render-table rend h d))
         ( _ (render rend ss))
         ))


(defun render (rend ss)
  (match ss
         (nil t)
         ( ((? x) . (? xs))
           {
           (render-it rend x)
           (render rend xs)
           })
         ))

(define end nil)

(defun s+ (s ss)
  (cons s ss))

(defun section (i str strings)
  (list 'section i str strings))

(defun tagged-section (i str tags strings)
  (list 'tagged-section i str tags strings))

; signature: (ref-entry str [tags] strings)
(defun ref-entry (str strings-or-tags)
  (let ((tags (and (rest-args 0)
                   strings-or-tags))
        (strings (or (rest-args 0)
                     strings-or-tags)))
    (list
     'newline
     (if tags
       (tagged-section 3 str tags strings)
       (section 3 str strings))
     'newline
     'hline
     )))

(defun hline ()
  'hline)

(defun para (str)
  (list 'para str))

(defun verb (str)
  (list 'verb str))

(defun code (c)
  (list 'code c))

(defun code-raw (c)
  (list 'code-raw c))

(defun code-png (img colors c)
  (list 'code-png img colors c))

(defun code-disp (c)
  (list 'code-disp c))

(defun code-examples (c)
  (list 'code-examples c))

(defun code-entry-ref (code)
  (str-merge "[`" code "`](#" (or (rest-args 0) code) ")"))

(defun program (c)
  (list 'program c))

(defun program-disp (c)
  (list 'program-disp c))

(defun program-gif (c)
  (list 'program-gif c))

(defun newline ()
  'newline)

(defun bold (str)
  (list 'bold str))

(defun bullet (ss)
  (verb (map (lambda (x) (str-merge "   - " x "\n")) ss)))

(defun image (alt url)
  (list 'image alt url))

(defun image-pair (cap0 txt0 fig0 cap1 txt1 fig1)
  (list 'image-pair cap0 txt0 fig0 cap1 txt1 fig1))

(defun s-exp-graph (img-name code)
  (list 's-exp-graph img-name code (rest-args 0)))

(defun semantic-step (c1 c2 prop)
  (list 'semantic-step c1 c2 prop))

(defun table (header data)
  (list 'table header data))

;; Dot generation

(defun dot-it ( i x)
  (match x
         ( ((? x) . (? xs))
           (let ( (node (str-merge "cons" (to-str i)))
                  ((c1 str1) (dot-it (shl i 1) x))
                  ((c2 str2) (dot-it (+ 1 (shl i 1)) xs))
                  )
             (list node (str-merge "   " node " [label=\"cons\"]\n"
                                   str1 "\n"
                                   str2 "\n"
                                   "   " node " -> " c1 ";\n"
                                   "   " node " -> " c2 ";\n"
                                   )
                   )
             )
           )
         ( (? x) (eq (type-of x) type-lisparray)
           (let ( (node (str-merge "cons" (to-str i)))
                  (atom1 (str-merge "atom" (to-str (+ i 1))))
                  (atom2 (str-merge "atom" (to-str (+ i 2))))
                  (str1 (str-merge "   " atom1 " [label=\"" (to-str x) "\"]\n"))
                  (str2 (str-merge "   " atom2 " [label=\"ARRAY TAG\"]\n")))
             
             (list node (str-merge "   "  node " [label=\"heap-cell\"]\n"
                                   str1 str2
                                   "   " node " -> " atom1 ";\n"
                                   "   " node " -> " atom2 ";\n"))))
                                   
         ( (? x)
           (let ( (node (str-merge "atom" (to-str i))) )
             (list node (str-merge "   " node " [label=\"" (to-str x)  "\"]"))
             )
          )
         )
  )

(defun to-dot (x)
  (str-merge "digraph SExpression {\n"
             "   node [shape=ellipse, fontsize=12];\n"
             "   edge [fontsize=10];\n"
             (car (cdr (dot-it 1u64 x))) "\n}"))


(defun render-dot (filename code)
  (let ( (dot-str (to-dot code))
         (name-dot (str-merge "./images/" filename ".dot"))
         (name-png (str-merge "./images/" filename ".png"))
         (fp-dot (fopen name-dot "w"))
         (fp-png (fopen name-png "w"))
         )
    {
    (fwrite fp-dot dot-str)
    (unsafe-call-system (str-merge "dot " name-dot " -Tpng > " name-png))
    }
    ))


(define info
  (let (((major minor patch) (lbm-version))
        (version-str (str-merge (to-str major) "." (to-str minor) "." (to-str patch))))
        (para (list (str-merge "This document was generated by LispBM version " version-str))
        )))
