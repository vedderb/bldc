;; gsub-fn + expand-template (see lua2lbm.lisp's "runtime helpers" section,
;; the canonical copy of this code -- duplicated here, self-contained, to
;; avoid this test depending on loading the whole (larger, more heap-
;; hungry) transpiler file just to exercise a small utility function): %N/
;; %0 backreferences via expand-template, %% escape, no-match passthrough.
;;
;; Replaces the old str-lua-gsub tests (that C-level function was removed
;; from pattern_extensions.c -- see the plan/memory notes for why: its
;; capability was a narrow strict subset of str-replace + gsub-fn combined,
;; not worth a second C-level implementation to maintain).
;;
;; Note: gsub-fn has no max-count parameter, unlike the old str-lua-gsub --
;; a real, minor capability gap from the removal, not silently dropped here.

(defun char-code (s i) (bufget-u8 s i))

(defun safe-part (text start len)
  (if (< start (str-len text)) (str-part text start len) ""))

(defunret gsub-fn (text pattern f)
  (progn
    (var n (str-len text))
    (var out "")
    (var pos 0)
    (var lastmatch -1)
    (loopwhile t
      (progn
        (if (> pos n) (return out) nil)
        (var r (str-lua-find text pattern pos))
        (if (eq r nil)
            (return (str-merge out (safe-part text pos (- n pos))))
            (progn
              (var s (ix r 0))
              (var e (ix r 1))
              (var matched (ix r 2))
              (if (= e lastmatch)
                  (progn
                    (setq out (str-merge out (safe-part text pos 1)))
                    (setq pos (+ pos 1)))
                  (progn
                    (var gap (safe-part text pos (- s pos)))
                    (var repl (f matched))
                    (var repl-text (if (eq repl nil) (safe-part text s (- e s)) (to-str repl)))
                    (setq out (str-merge out gap repl-text))
                    (setq pos e)
                    (setq lastmatch e)))))))))

(defun expand-template (tmpl)
  (lambda (matched)
    (progn
      (var has-caps (eq (type-of matched) 'type-list))
      (var caps (if has-caps matched (list matched)))
      (var tlen (str-len tmpl))
      (var out "")
      (var i 0)
      (loopwhile (< i tlen)
        (progn
          (var c (char-code tmpl i))
          (if (and (= c 37) (< (+ i 1) tlen))
              (progn
                (var d (char-code tmpl (+ i 1)))
                (cond
                  ((= d 37) (progn (setq out (str-merge out "%")) (setq i (+ i 2))))
                  ((and (>= d 48) (<= d 57))
                   (progn
                     (var idx (- d 48))
                     (var piece
                          (if (= idx 0)
                              (if has-caps (to-str matched) matched)
                              (to-str (ix caps (- idx 1)))))
                     (setq out (str-merge out piece))
                     (setq i (+ i 2))))
                  (t (progn (setq out (str-merge out (str-part tmpl i 1))) (setq i (+ i 1))))))
              (progn (setq out (str-merge out (str-part tmpl i 1))) (setq i (+ i 1))))))
      out)))

(defun check (got expected i)
  (if (eq got expected)
      t
    (progn (print "FAIL " i ": got " got " expected " expected) nil)))

(define t1 (check (gsub-fn "hello world" "o" (expand-template "0")) "hell0 w0rld" 1))
(define t2 (check (gsub-fn "2026-07-05" "(%d+)-(%d+)-(%d+)" (expand-template "%3/%2/%1")) "05/07/2026" 2))
(define t3 (check (gsub-fn "50% off" "%%" (expand-template "pct")) "50pct off" 3))
(define t4 (check (gsub-fn "hello" "xyz" (expand-template "!")) "hello" 4))
(define t5 (check (gsub-fn "Hello World" "%l+" str-to-upper) "HELLO WORLD" 5))

(if (and t1 t2 t3 t4 t5)
    (print "SUCCESS")
    (print "FAILURE"))
