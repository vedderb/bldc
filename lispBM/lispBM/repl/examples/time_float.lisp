(define num_adds 10000000)

(defun res-str (str t0)
  (let ( (secs (secs-since t0)))
    (str-merge str ", " (to-str secs))))

;; Addition of u32:

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1u32 1u32)
      (setq i (+ i 1))
      }
      )

(print (res-str "u32" t0))

;; Addition of f32:

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1.0f32 1.0f32)
      (setq i (+ i 1))
      }
      )

(print (res-str "f32" t0))

;; Addition of f64:

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1.0f64 1.0f64)
      (setq i (+ i 1))
      }
      )

(print (res-str "f64" t0))
