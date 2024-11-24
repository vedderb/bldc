

(defun b (xs)
  {
  (var rxs (reverse xs))
  (var num 0u32)
  (looprange i 0 (length rxs)
             (setq num (+ num (if (= (ix rxs i) 1)
                                  (shl 1 i)
                                0))))
  num
  }
  )


(b '(1 0 1 0 0 1 0 1))
