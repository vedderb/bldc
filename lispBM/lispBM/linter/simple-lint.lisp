(hide-trapped-error)

(define find-top-level-expressions
    (lambda (string)
      {
      (var result '())
      (var pos 0)
      (var len (str-len string))
      
      (define skip-whitespace
          (lambda (start)
            {
            (var i start)
            (loopwhile (and (< i len)
                            (or (eq (str-cmp (str-part string i 1) " ") 0)
                                (eq (str-cmp (str-part string i 1) "\t") 0)
                                (eq (str-cmp (str-part string i 1) "\n") 0)
                                (eq (str-cmp (str-part string i 1) "\r") 0)))
             (setq i (+ i 1)))
            i
            }))
      
      (define skip-comment
          (lambda (start)
            {
            (var i start)
            (if (and (< i len) (eq (str-cmp (str-part string i 1) ";") 0))
                {
                (loopwhile (and (< i len) (not (eq (str-cmp (str-part string i 1) "\n") 0)))
                 (setq i (+ i 1)))
                (if (< i len) (+ i 1) i)
                }
                start)
            }))
      
      (define find-matching-paren
          (lambda (start)
            {
            (var i (+ start 1))
            (var paren-count 1)
            (var in-string nil)
            (var escape-next nil)
            
            (loopwhile (and (< i len) (> paren-count 0))
             {
             (var c (str-part string i 1))
             (if escape-next
                 (setq escape-next nil)
                 {
                 (if in-string
                     {
                     (if (eq (str-cmp c "\\") 0)
                         (setq escape-next t)
                         (if (eq (str-cmp c "\"") 0)
                             (setq in-string nil)))
                     }
                     {
                     (if (eq (str-cmp c "\"") 0)
                         (setq in-string t)
                         {
                         (if (eq (str-cmp c "(") 0)
                             (setq paren-count (+ paren-count 1)))
                         (if (eq (str-cmp c ")") 0)
                             (setq paren-count (- paren-count 1)))
                         })
                     })
                 })
             (setq i (+ i 1))
             })
            (if (= paren-count 0) i -1)
            }))
      
      (define find-symbol-end
          (lambda (start)
            {
            (var i start)
            (loopwhile (and (< i len)
                           (not (or (eq (str-cmp (str-part string i 1) " ") 0)
                                   (eq (str-cmp (str-part string i 1) "\t") 0)
                                   (eq (str-cmp (str-part string i 1) "\n") 0)
                                   (eq (str-cmp (str-part string i 1) "\r") 0)
                                   (eq (str-cmp (str-part string i 1) "(") 0)
                                   (eq (str-cmp (str-part string i 1) ")") 0)
                                   (eq (str-cmp (str-part string i 1) ";") 0))))
             (setq i (+ i 1)))
            i
            }))
      
      (loopwhile (< pos len)
       {
       (setq pos (skip-whitespace pos))
       (if (< pos len)
           {
           (setq pos (skip-comment pos))
           (if (< pos len)
               {
               (var start-pos pos)
               (var c (str-part string pos 1))
               (if (eq (str-cmp c "(") 0)
                   {
                   (var end-pos (find-matching-paren pos))
                   (if (= end-pos -1)
                       {
                       (setq result (cons (list 'unclosed start-pos) result))
                       (setq pos len)
                       }
                       {
                       (if (> end-pos start-pos)
                           {
                           (setq result (cons (list start-pos end-pos) result))
                           (setq pos end-pos)
                           }
                           (setq pos (+ pos 1)))
                       })
                   }
                   {
                   (if (eq (str-cmp c ")") 0)
                       {
                       (setq result (cons (list 'close-paren-mismatch pos) result))
                       (setq pos (+ pos 1))
                       }
                       {
                       (var end-pos (find-symbol-end pos))
                       (if (> end-pos start-pos)
                           {
                           (setq result (cons (list start-pos end-pos) result))
                           (setq pos end-pos)
                           }
                           (setq pos (+ pos 1)))
                       })
                   })
               })
           })
       })
      
      (reverse result)
      }))

(define pos-to-line-col
    (lambda (string pos)
      {
      (var line 1)
      (var col 1)
      (var i 0)
      (var len (str-len string))
      
      (if (or (< pos 0) (> pos len))
          (list -1 -1)
          {
          (loopwhile (< i pos)
           {
           (var c (str-part string i 1))
           (if (eq (str-cmp c "\n") 0)
               {
               (setq line (+ line 1))
               (setq col 1)
               }
               (setq col (+ col 1)))
           (setq i (+ i 1))
           })
          (list line col)
          })
      }))

(define extract-and-read
    (lambda (string pos-pair)
      {
      (var start-pos (car pos-pair))
      (var end-pos (car (cdr pos-pair)))
      (var len (str-len string))
      (if (or (< start-pos 0) (< end-pos start-pos) (> end-pos len))
          nil
          {
          (var substring (str-part string start-pos (- end-pos start-pos)))
          (read substring)
          })
      }))

(define item-start-pos
    (lambda (x)
      (match x
             ( (unclosed (? p)) p)
             ( (close-paren-mismatch (? p)) p )
             ( ((? p) (? _)) p))))

(define item-end-pos
    (lambda (x)
      (match x
             ( (unclosed (? p)) nil)
             ( (close-paren-mismatch (? p)) nil)
             ( ((? _) (? e)) e))))


(define print-unclosed
    (lambda (pre p post)
      (print pre " " p " " post)
      ))

(define analyze-top-level
    (lambda (str pos-pairs)
      (looprange i 0 (length pos-pairs)
            (match (ix pos-pairs i)
                   ( (unclosed (? p)) {
                     (var line-col (pos-to-line-col str p))
                     (var line (car line-col))
                     (var col  (car (cdr line-col)))
                     (print "Expression at line " line " column " col " is not closed:")
                     (print ">" (str-part str p) "<")
                     })
                   ( (close-paren-mismatch (? p)) {
                     (var pre "")
                     (if (> i 0) {
                         (var pre-start (item-start-pos (ix pos-pairs (- i 1))))
                         (setq pre (str-part str pre-start (- p pre-start)))
                         }
                         )
                     (var line-col (pos-to-line-col str p))
                     (var line (car line-col))
                     (var col  (car (cdr line-col)))
                     (print "Mismatched parenthesis at line " line " column " col ":")
                     (print pre ">" (str-part str p 1) "<\n\n")
                     })
                   ( ((? p) (? e)) {
                     (var r (trap (read (str-part str p (- e p)))))
                     (match r
                            ( (exit-error (? err)) {
                              (var line-col (pos-to-line-col str p))
                              (var line (car line-col))
                              (var col  (car (cdr line-col)))
                              (print "syntax error at line " line " column " col ":")
                              (print ">" (str-part str p (- p e)) "<")
                              })
                            ( (exit-ok    (? exp)) {
                              ;; TODO Analyze the parsed expression
                              }
                              )
                            )
                     
                   })
                   ))))




;; load file filename and process it
(define run-lint
    (lambda (filename) {
      (var fh (fopen filename "r"))
      (var data (load-file fh))
      
      (define p-list (find-top-level-expressions data))
      (analyze-top-level data p-list)
      }))

             
            



