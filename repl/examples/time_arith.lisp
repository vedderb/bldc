(define num_adds 10000000)

(defun res-str (str t0)
    (let ( (secs (secs-since t0)))
        (str-merge str ", " (to-str secs))))

;; Addition of bytes:

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1b 1b)
      (setq i (+ i 1))
      }
      )

(print (res-str "byte" t0))

;; Addition of i

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1 1)
      (setq i (+ i 1))
      }
      )

(print (res-str "i" t0))

;; Addition of u

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1u 1u)
      (setq i (+ i 1))
      }
      )

(print (res-str "u" t0))

;; Addition of i32

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1i32 1i32)
      (setq i (+ i 1))
      }
      )

(print (res-str "i32" t0))

;; Addition of u32

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1u32 1u32)
      (setq i (+ i 1))
      }
      )

(print (res-str "u32" t0))

;; Addition of i64

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1i64 1i64)
      (setq i (+ i 1))
      }
      )

(print (res-str "i64" t0))

;; Addition of u64

(define t0 (systime))

(loop ((i 0))
      (< i num_adds)
      {
      (+ 1u64 1u64)
      (setq i (+ i 1))
      }
      )

(print (res-str "u64" t0))

